import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import FIFOF::*;
import FIFO::*;
import Vector::*;
import RegFile::*;

import hasim_cpu_parameters::*;
import hasim_cpu_types::*;
import hasim_cpu_rob::*;
import hasim_cpu_branchPred::*;
import hasim_cpu_targetBuffer::*;

typedef enum {Commit, CommitDone}       CommitState    deriving (Bits, Eq);
typedef enum {RobUpdate, RobUpdateDone} RobUpdateState deriving (Bits, Eq);
typedef enum {Read, Write}              RobOpState     deriving (Bits, Eq);
typedef enum {Fetch, FetchDone}         FetchState     deriving (Bits, Eq);
typedef enum {Decoding, DecodeDone}     DecodeState    deriving (Bits, Eq);

module [HASim_Module] mkDecode();
    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, fromInteger(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, fromInteger(i)), 1);

    Connection_Receive#(Tuple2#(Token, PackedInst))                   fpFetchResp <- mkConnection_Receive("fp_fet_resp");
    Connection_Send#(Tuple2#(Token, void))                            fpDecodeReq <- mkConnection_Send("fp_dec_req");
    Connection_Receive#(Tuple2#(Token, DepInfo))                     fpDecodeResp <- mkConnection_Receive("fp_dec_resp");
    Connection_Receive#(Tuple2#(Token, void))                        fpMemoryResp <- mkConnection_Receive("fp_mem_resp");

    Connection_Send#(Token)                                             fpTokKill <- mkConnection_Send("fp_tok_kill");
    Connection_Send#(Token)                                           fpFetchKill <- mkConnection_Send("fp_fet_kill");
    Connection_Send#(Token)                                          fpDecodeKill <- mkConnection_Send("fp_dec_kill");
    Connection_Send#(Token)                                             fpExeKill <- mkConnection_Send("fp_exe_kill");
    Connection_Send#(Token)                                             fpMemKill <- mkConnection_Send("fp_mem_kill");
    Connection_Send#(Token)                                        fpMemStateKill <- mkConnection_Send("fp_memstate_kill");
    Connection_Send#(Token)                                     fpLocalCommitKill <- mkConnection_Send("fp_lco_kill");
    Connection_Send#(Token)                                    fpGlobalCommitKill <- mkConnection_Send("fp_gco_kill");
    Connection_Send#(Token)                                       fpRewindToToken <- mkConnection_Send("fp_rewindToToken");

    Connection_Server#(Command, Response)                            finishServer <- mkConnection_Server("controller_to_tp");

    Vector#(FetchWidth, Port_Receive#(Tuple2#(Token, Addr)))        tokenAddrPort <- genWithM(receiveFunctionM("fetchToDecode"));

    Port_Send#(FetchCount)                                          decodeNumPort <- mkPort_Send("decodeToFetchDecodeNum");
    Port_Send#(Addr)                                           predictedTakenPort <- mkPort_Send("decodeToFetchPredictedTaken");
    Port_Send#(Addr)                                               mispredictPort <- mkPort_Send("decodeToFetchMispredict");

    Port_Receive#(IntQCountType)                                    intQCountPort <- mkPort_Receive("issueToDecodeIntQ", 1);
    Port_Receive#(MemQCountType)                                    memQCountPort <- mkPort_Receive("issueToDecodeMemQ", 1);

    Vector#(FetchWidth, Port_Send#(IssueEntry))                         issuePort <- genWithM(sendFunctionM("decodeToIssue"));

    Vector#(NumFuncUnits, Port_Receive#(Tuple2#(ExecEntry, InstResult)))
                                                                   execResultPort <- genWithM(receiveFunctionM("execToDecodeResult"));

    Vector#(CommitWidth, Port_Send#(Token))                       commitTokenPort <- genWithM(sendFunctionM("decodeToCommit"));

    FIFOF#(Tuple2#(Token, Addr))                                  tokenAddrBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(FetchWidth)));
    FIFOF#(PackedInst)                                                 instBuffer <- mkFIFOF();
    FIFOF#(DepInfo)                                                  decodeBuffer <- mkFIFOF();

    Reg#(CommitState)                                                 commitState <- mkReg(CommitDone);
    Reg#(RobUpdateState)                                           robUpdateState <- mkReg(RobUpdateDone);
    Reg#(RobOpState)                                                   robOpState <- mkReg(?);
    Reg#(FetchState)                                                   fetchState <- mkReg(FetchDone);
    Reg#(DecodeState)                                                 decodeState <- mkReg(DecodeDone);
    Reg#(Bool)                                                     killInstBuffer <- mkReg(False);
    Reg#(Bool)                                                 nextKillInstBuffer <- mkReg(?);
    Reg#(Bool)                                                     realDecodeDone <- mkReg(?);

    Reg#(Maybe#(Tuple2#(ExecEntry, InstResult)))                        execEntry <- mkReg(?);

    Reg#(Bit#(TLog#(TAdd#(FreeListCount,1))))                   freeListFreeCount <- mkReg(fromInteger(valueOf(FreeListCount)));
    Reg#(Bit#(TLog#(TAdd#(IntQCount,1))))                           intQFreeCount <- mkReg(?);
    Reg#(Bit#(TLog#(TAdd#(MemQCount,1))))                         memQFreeCount <- mkReg(?);
    Reg#(Bit#(TLog#(TAdd#(BranchCount,1))))                           branchCount <- mkReg(fromInteger(valueOf(BranchCount)));

    Reg#(FetchCount)                                                   fetchCount <- mkReg(?);
    Reg#(FetchCount)                                                    decodeNum <- mkReg(fromInteger(valueOf(FetchWidth)));
    Reg#(InstCount)                                                       instNum <- mkReg(fromInteger(valueOf(TMul#(2,FetchWidth))));
    Reg#(FuncUnitPos)                                              robUpdateCount <- mkReg(?);
    Reg#(CommitCount)                                                 commitCount <- mkReg(?);
    Reg#(Maybe#(Addr))                                                predictedPC <- mkReg(tagged Invalid);
    Reg#(Maybe#(Addr))                                               mispredictPC <- mkReg(tagged Invalid);

    Rob                                                                       rob <- mkRob();
    Reg#(Vector#(PRNum, Bool))                                           pRegFile <- mkReg(replicate(False));
    BranchPred                                                         branchPred <- mkBranchPred();
    FIFO#(Addr)                                                      targetBuffer <- mkTargetBuffer(pcStart);

    Reg#(Bool)                                                              birth <- mkReg(True);

    Reg#(Bool)                                                      syncFromIssue <- mkReg(False);
    Reg#(Bool)                                                        syncToFetch <- mkReg(False);

    Reg#(ClockCounter)                                               clockCounter <- mkReg(0);
    Reg#(ClockCounter)                                               modelCounter <- mkReg(0);


    function Action restoreToToken(Token token);
    action
        fpRewindToToken.send(token);
    endaction
    endfunction

    function Action killRobStage(Token token);
    action
        fpTokKill.send(token);
        fpLocalCommitKill.send(token);
        fpMemStateKill.send(token);
    endaction
    endfunction
    
    function Action killDecodeStage(Token token);
    action
        fpTokKill.send(token);
        fpExeKill.send(token);
    endaction
    endfunction

    rule clockCount(True);
        clockCounter <= clockCounter + 1;
    endrule

    rule birthOfModel(birth);
        let req <- finishServer.getReq();
        birth   <= False;
    endrule

    rule getDecodeResp(True);
        let decodeTuple <- fpDecodeResp.receive();
        decodeBuffer.enq(tpl_2(decodeTuple));
    endrule

    //Currently the state transition diagram is:
    /*
              AllDone
                /   \
               /     \
           Fetch     RobUpdate
             |           |
         FetchDone   RobUpdateDone
              \        /      \
               \      /        \
                Decode        Commit
                   |             \
              DecodeDone       CommitDone
    */

    rule synchronize(fetchState == FetchDone && decodeState == DecodeDone && commitState == CommitDone);
        modelCounter       <= modelCounter + 1;

        let newInstNum  = instNum + zeroExtend(decodeNum);
        let sendInstNum = (newInstNum >= 4)? 4: newInstNum;
        instNum <= newInstNum;
        decodeNumPort.send(tagged Valid truncate(newInstNum));
        predictedTakenPort.send(predictedPC);
        mispredictPort.send(mispredictPC);

        let intQFreeCountLocal <- intQCountPort.receive();
        let memQFreeCountLocal <- memQCountPort.receive();

        if(isValid(intQFreeCountLocal))
        begin
            $display("Initial Free Count: %0d", validValue(intQFreeCountLocal));
            intQFreeCount  <= validValue(intQFreeCountLocal);
        end
        else
        begin
            $display("Initial Free Count Invalid");
            intQFreeCount  <= fromInteger(valueOf(IntQCount));
        end
        //intQFreeCount      <= fromMaybe(fromInteger(valueOf(IntQCount)), intQFreeCountLocal);
        memQFreeCount      <= fromMaybe(fromInteger(valueOf(MemQCount)), memQFreeCountLocal);

        fetchState         <= Fetch;
        fetchCount         <= 0;

        robUpdateState     <= RobUpdate;
        let execReceive    <- execResultPort[0].receive();
        execEntry          <= execReceive;
        if(isValid(execReceive))
            rob.readAnyReq((tpl_1(validValue(execReceive))).robTag);
        robOpState         <= isValid(execReceive)? Write: Read;
        robUpdateCount     <= isValid(execReceive)? 0: 1;

        nextKillInstBuffer <= False;
    endrule

    rule fetch(fetchState == Fetch);
        let tokenAddr <- tokenAddrPort[fetchCount].receive();
        if(isValid(tokenAddr))
        begin
            let tokenAddrVal = validValue(tokenAddr);
            tokenAddrBuffer.enq(tokenAddrVal);
            instNum         <= instNum - 1;
            match {.token, .inst} <- fpFetchResp.receive();
            instBuffer.enq(inst);
            fpDecodeReq.send(tuple2(tpl_1(tokenAddrVal), ?));
        end
        fetchCount       <= fetchCount + 1;
        if(fetchCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
            fetchState   <= FetchDone;
    endrule

    function finishRob();
    action
        robUpdateState <= RobUpdateDone;

        decodeState    <= Decoding;
        decodeNum      <= killInstBuffer? fromInteger(valueOf(FetchWidth)): 0;
        realDecodeDone <= killInstBuffer? True: False;

        commitState    <= Commit;
        commitCount    <= 0;
        rob.readHeadReq();

        predictedPC    <= tagged Invalid;
    endaction
    endfunction 

    rule robUpdateRead(robUpdateState == RobUpdate && robOpState == Read);
        let execReceive    <- execResultPort[robUpdateCount].receive();
        execEntry          <= execReceive;
        if(isValid(execReceive))
        begin
            rob.readAnyReq((tpl_1(validValue(execReceive))).robTag);
            robOpState     <= Write;
        end
        else
        begin
            robUpdateCount <= robUpdateCount + 1;
            if(robUpdateCount == fromInteger(valueOf(TSub#(NumFuncUnits,1))))
            begin
                finishRob();
            end
        end
    endrule

    rule robUpdateWrite(robUpdateState == RobUpdate && robOpState == Write);
        let robEntry       <- rob.readAnyResp();
        let memAck         <- fpMemoryResp.receive();
        match {.exec, .res} = validValue(execEntry);
        if(rob.isRobTagValid(exec.robTag) && robEntry.token == exec.token)
        begin
            let taken = case (res) matches
                            tagged RBranchTaken .addr: True;
                            default: False;
                        endcase;

            let newAddr = case (res) matches
                              tagged RBranchTaken .addr: addr;
                          endcase;

            let finished = case (res) matches
                               tagged RTerminate .execResult: True;
                               default: False;
                           endcase;

            let correct = case (res) matches
                              tagged RTerminate .execResult: execResult;
                          endcase;

            rob.writeAny(exec.robTag, RobEntry{token: robEntry.token, addr: robEntry.addr, done: True, finished: finished,
                                               isBranch: robEntry.isBranch, prediction: robEntry.prediction, taken: taken,
                                               isJR: robEntry.isJR, predAddr: robEntry.predAddr});

            pRegFile[exec.pRName] <= False;
            freeListFreeCount <= freeListFreeCount + 1;

            if(robEntry.isBranch || robEntry.isJR)
                branchCount <= branchCount + 1;

            if(robEntry.isBranch && robEntry.prediction != taken || robEntry.isJR && robEntry.predAddr != newAddr)
            begin
                restoreToToken(exec.token);
                rob.updateTail(exec.robTag);
                killInstBuffer     <= True;
                nextKillInstBuffer <= True;
                mispredictPC       <= tagged Valid newAddr;
            end

            if(finished)
                finishServer.makeResp(tagged RESP_DoneRunning correct);
        end
        else
            killRobStage(exec.token);

        robUpdateCount     <= robUpdateCount + 1;
        robOpState         <= Read;
        if(robUpdateCount == fromInteger(valueOf(TSub#(NumFuncUnits,1))))
        begin
            finishRob();
        end
    endrule

    rule commit(commitState == Commit && commitCount != fromInteger(valueOf(CommitWidth)));
        let robEntryMaybe <- rob.readHeadResp();
        let robEntry = validValue(robEntryMaybe);
        commitCount <= commitCount + 1;
        if(isValid(robEntryMaybe) && robEntry.done)
        begin
            rob.incrementHead();
            commitTokenPort[commitCount].send(tagged Valid robEntry.token);
            if(robEntry.isBranch)
                branchPred.upd(robEntry.addr, robEntry.prediction, robEntry.taken);
            rob.readHeadReq();
        end
        else
        begin
            for(Integer i = 0; i < valueOf(CommitWidth); i=i+1)
            begin
                if(fromInteger(i) >= commitCount)
                begin
                    commitTokenPort[i].send(tagged Invalid);
                end
            end
            commitState <= CommitDone;
        end
    endrule

    let currInst                  = instBuffer.first();
    let currDepInfo               = decodeBuffer.first();
    match {.currToken, .currAddr} = tokenAddrBuffer.first();

    let src1Valid = isValid(currDepInfo.dep_src1);
    let src1      = tpl_2(validValue(currDepInfo.dep_src1));
    let src2Valid = isValid(currDepInfo.dep_src2);
    let src2      = tpl_2(validValue(currDepInfo.dep_src2));
    let dest      = tpl_2(validValue(currDepInfo.dep_dest));

    let src1Ready = src1Valid? !pRegFile[src1]: True;
    let src2Ready = src2Valid? !pRegFile[src2]: True;

    rule decodeInst(decodeState == Decoding && !killInstBuffer && !realDecodeDone && tokenAddrBuffer.notEmpty() && decodeNum != fromInteger(valueOf(FetchWidth)));
        let branchPredAddr = branchPred.getPredAddr(currAddr);
        let jumpPredAddr   = targetBuffer.first();
        RobEntry res = RobEntry{token: currToken, addr: currAddr, done: False, finished: False,
                                isBranch: False, prediction: isValid(branchPredAddr), taken: False,
                                isJR: False, predAddr: jumpPredAddr};
        IssueEntry issue = IssueEntry{issueType: Normal,
                                      token: currToken, robTag: rob.getTail(),
                                      src1Ready: src1Ready, src1: src1,
                                      src2Ready: src2Ready, src2: src2,
                                      dest: dest};
        Bool doCommonDecode = ?;
        $display("QFreeCount: %0d %0d", intQFreeCount, memQFreeCount);
        if(!rob.notFull())
        begin
            realDecodeDone <= True;
            doCommonDecode  = False;
        end
        else if(isALU(currInst))
        begin
            if(freeListFreeCount != 0 && intQFreeCount != 0)
            begin
                if(isShift(currInst))
                    issue.issueType = Shift;
                freeListFreeCount <= freeListFreeCount + 1;
                intQFreeCount     <= intQFreeCount - 1;
                pRegFile[dest]    <= True;
                doCommonDecode     = True;
            end
            else
            begin
                realDecodeDone <= True;
                doCommonDecode  = False;
            end
        end
        else if(isLoad(currInst))
        begin
            if(freeListFreeCount != 0 && memQFreeCount != 0)
            begin
                issue.issueType    = Load;
                freeListFreeCount <= freeListFreeCount + 1;
                memQFreeCount     <= memQFreeCount - 1;
                pRegFile[dest]    <= True;
                doCommonDecode     = True;
            end
            else
            begin
                realDecodeDone <= True;
                doCommonDecode  = False;
            end
        end
        else if(isStore(currInst))
        begin
            if(memQFreeCount != 0)
            begin
                issue.issueType    = Store;
                memQFreeCount     <= memQFreeCount - 1;
                doCommonDecode     = True;
            end
            else
            begin
                realDecodeDone <= True;
                doCommonDecode  = False;
            end
        end
        else if(isBranch(currInst))
        begin
            if(intQFreeCount != 0)
            begin
                issue.issueType = Branch;
                res.isBranch    = True;
                intQFreeCount  <= intQFreeCount - 1;
                branchCount    <= branchCount - 1;
                predictedPC    <= branchPredAddr;
                if(isValid(branchPredAddr))
                begin
                    realDecodeDone     <= True;
                    nextKillInstBuffer <= True;
                    killInstBuffer     <= True;
                end
                doCommonDecode  = True;
            end
            else
            begin
                realDecodeDone <= True;
                doCommonDecode  = False;
            end
        end
        else if(isJ(currInst))
        begin
            issue.issueType     = J;
            predictedPC        <= tagged Valid getJAddr(currInst, currAddr);
            realDecodeDone     <= True;
            nextKillInstBuffer <= True;
            killInstBuffer     <= True;
            doCommonDecode      = True;
        end
        else if(isJAL(currInst))
        begin
            if(freeListFreeCount != 0)
            begin
                issue.issueType     = JAL;
                freeListFreeCount  <= freeListFreeCount - 1;
                let newPC           = getJALAddr(currInst, currAddr);
                predictedPC        <= tagged Valid newPC;
                targetBuffer.enq(currAddr+4);
                realDecodeDone     <= True;
                nextKillInstBuffer <= True;
                killInstBuffer     <= True;
                doCommonDecode      = True;
            end
            else
            begin
                realDecodeDone <= True;
                doCommonDecode  = False;
            end
        end
        else if(isJR(currInst))
        begin
            if(intQFreeCount != 0)
            begin
                issue.issueType     = JR;
                res.isJR            = True;
                intQFreeCount       <= intQFreeCount - 1;
                predictedPC <= tagged Valid jumpPredAddr;
                targetBuffer.deq();
                realDecodeDone     <= True;
                nextKillInstBuffer <= True;
                killInstBuffer     <= True;
                doCommonDecode      = True;
            end
            else
            begin
                realDecodeDone <= True;
                doCommonDecode  = False;
            end
        end
        else if(isJALR(currInst))
        begin
            if(freeListFreeCount != 0 && intQFreeCount != 0)
            begin
                issue.issueType     = JALR;
                res.isJR            = True;
                freeListFreeCount  <= freeListFreeCount - 1;
                intQFreeCount       <= intQFreeCount - 1;
                predictedPC        <= tagged Valid jumpPredAddr;
                targetBuffer.deq();
                targetBuffer.enq(currAddr+4);
                realDecodeDone     <= True;
                nextKillInstBuffer <= True;
                killInstBuffer     <= True;
                doCommonDecode      = True;
            end
            else
            begin
                realDecodeDone <= True;
                doCommonDecode  = False;
            end
        end

        if(doCommonDecode)
        begin
            decodeNum <= decodeNum + 1;

            rob.writeTail(res);
            issuePort[decodeNum].send(tagged Valid issue);

            tokenAddrBuffer.deq();
            instBuffer.deq();
            decodeBuffer.deq();
            $display("Decoded Instruction: Token: %0d @ Model: %0d", currToken.index, modelCounter-1);
        end
    endrule

    rule noInstInInstBuffer(decodeState == Decoding && !killInstBuffer && !realDecodeDone && !tokenAddrBuffer.notEmpty() && fetchState == FetchDone);
        realDecodeDone <= True;
    endrule

    rule decodeDone(decodeState == Decoding && !killInstBuffer && (realDecodeDone || decodeNum == fromInteger(valueOf(FetchWidth))));
        decodeState <= DecodeDone;
    endrule

    rule fillIssueQueues(decodeState == Decoding && realDecodeDone);
        for(FetchCount i = 0; i != fromInteger(valueOf(FetchWidth)); i=i+1)
        begin
            if(i >= decodeNum)
                issuePort[i].send(tagged Invalid);
        end
    endrule

    rule decodeKillInstBuffer(decodeState == Decoding && killInstBuffer && tokenAddrBuffer.notEmpty());
        if(tokenAddrBuffer.notEmpty())
        begin
            tokenAddrBuffer.deq();
            instBuffer.deq();
            decodeBuffer.deq();
            killDecodeStage(tpl_1(tokenAddrBuffer.first()));
        end
        else
        begin
            killInstBuffer <= nextKillInstBuffer;
            decodeState    <= DecodeDone;
        end
    endrule
endmodule
