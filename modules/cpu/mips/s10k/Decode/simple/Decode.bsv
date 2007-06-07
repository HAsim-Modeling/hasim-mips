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

typedef enum {Commit, Update, Decode, RobDone} RobState   deriving (Bits, Eq);
typedef enum {Fetch, FetchDone}                FetchState deriving (Bits, Eq);

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

    Port_Send#(FetchCount)                                    instBufferCountPort <- mkPort_Send("decodeToFetchInstBuffer");
    Port_Send#(Addr)                                           predictedTakenPort <- mkPort_Send("decodeToFetchPredictedTaken");
    Port_Send#(Addr)                                               mispredictPort <- mkPort_Send("decodeToFetchMispredict");

    Port_Receive#(IntQCountType)                                    intQCountPort <- mkPort_Receive("issueToDecodeIntQ", 1);
    Port_Receive#(MemQCountType)                                    memQCountPort <- mkPort_Receive("issueToDecodeMemQ", 1);

    Vector#(FetchWidth, Port_Send#(IssueEntry))                         issuePort <- genWithM(sendFunctionM("decodeToIssue"));

    Vector#(NumFuncUnits, Port_Receive#(Tuple2#(ExecEntry, InstResult)))
                                                                   execResultPort <- genWithM(receiveFunctionM("execToDecodeResult"));

    Vector#(CommitWidth, Port_Send#(Token))                       commitTokenPort <- genWithM(sendFunctionM("decodeToCommit"));

    FIFOF#(Tuple2#(Token, Addr))                                  tokenAddrBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(FetchWidth)));
    FIFOF#(PackedInst)                                                 instBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(FetchWidth)));
    FIFOF#(DepInfo)                                                  decodeBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(FetchWidth)));

    Reg#(RobState)                                                       robState <- mkReg(RobDone);
    Reg#(FetchState)                                                   fetchState <- mkReg(FetchDone);
    Reg#(Bool)                                                     killInstBuffer <- mkReg(False);
    Reg#(Bool)                                                 nextKillInstBuffer <- mkReg(?);
    Reg#(Bool)                                                     realDecodeDone <- mkReg(?);

    Reg#(Maybe#(Tuple2#(ExecEntry, InstResult)))                        execEntry <- mkReg(?);

    Reg#(Bit#(TLog#(TAdd#(FreeListCount,1))))                   freeListFreeCount <- mkReg(fromInteger(valueOf(FreeListCount)));
    Reg#(Bit#(TLog#(TAdd#(IntQCount,1))))                           intQFreeCount <- mkReg(?);
    Reg#(Bit#(TLog#(TAdd#(MemQCount,1))))                           memQFreeCount <- mkReg(?);
    Reg#(Bit#(TLog#(TAdd#(BranchCount,1))))                           branchCount <- mkReg(fromInteger(valueOf(BranchCount)));

    Reg#(FetchCount)                                                   fetchCount <- mkReg(?);
    Reg#(FetchCount)                                              instBufferCount <- mkReg(0);
    Reg#(Bit#(32))                                                       robCount <- mkReg(?);

    Reg#(Maybe#(Addr))                                                predictedPC <- mkReg(tagged Invalid);
    Reg#(Maybe#(Addr))                                               mispredictPC <- mkReg(tagged Invalid);

    Rob                                                                       rob <- mkRob();
    Reg#(Vector#(PRNum, Bool))                                           pRegFile <- mkReg(replicate(False));
    BranchPred                                                         branchPred <- mkBranchPred();
    BranchStack                                                       branchStack <- mkBranchStack();
    FIFO#(Addr)                                                      targetBuffer <- mkTargetBuffer(pcStart);

    Reg#(Bool)                                                              birth <- mkReg(True);

    Reg#(ClockCounter)                                               clockCounter <- mkReg(0);
    Reg#(ClockCounter)                                               modelCounter <- mkReg(0);

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

    rule synchronize(fetchState == FetchDone && decodeState == DecodeDone && commitState == CommitDone);
        modelCounter  <= modelCounter + 1;

        instBufferCountPort.send(tagged Valid truncate(robCount));
        predictedTakenPort.send(predictedPC);
        mispredictPort.send(mispredictPC);

        let sendSize = (instBufferCount < fromInteger(valueOf(FetchWidth)))? fromInteger(valueOf(FetchWidth))-instBufferCount: 0;
        $display("Decode synchronize: instBufferCount %0d, sendSize: %0d @ Model: %0d", instBufferCount, sendSize, modelCounter);

        let intQFreeCountLocal <- intQCountPort.receive();
        let memQFreeCountLocal <- memQCountPort.receive();
        intQFreeCount <= fromMaybe(fromInteger(valueOf(IntQCount)), intQFreeCountLocal);
        memQFreeCount <= fromMaybe(fromInteger(valueOf(MemQCount)), memQFreeCountLocal);

        fetchState    <= Fetch;
        fetchCount    <= 0;

        robState      <= Commit;
        robCount      <= 0;

        nextKillInstBuffer <= False;
    endrule

    rule fetch(fetchState == Fetch);
        let tokenAddrMaybe <- tokenAddrPort[fetchCount].receive();
        if(isValid(tokenAddrMaybe))
        begin
            let tokenAddr = validValue(tokenAddrMaybe);
            tokenAddrBuffer.enq(tokenAddr);
            match {.token, .inst} <- fpFetchResp.receive();
            instBufferCount <= instBufferCount + 1;
            instBuffer.enq(inst);
            fpDecodeReq.send(token, ?));
        end
        fetchCount     <= fetchCount + 1;
        if(fetchCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
            fetchState <= FetchDone;
    endrule

    rule commit(robState == Commit);
        let robEntryMaybe = rob.readHead();
        let robEntry      = validValue(robEntryMaybe);
        if(isValid(robEntryMaybe) && robEntry.done)
        begin
            commitTokenPort[commitCount].send(tagged Valid robEntry.token);
            $display("Commit: Token: %0d @ Model: %0d", robEntry.token.index, modelCounter-1);
            if(robEntry.isBranch)
                branchPred.upd(robEntry.addr, robEntry.prediction, robEntry.taken);
            if(robEntry.finished)
                finishServer.makeResp(tagged RESP_DoneRunning correct);
            if(robCount == fromInteger(valueOf(TSub#(CommitWidth,1))))
            begin
                robState <= Update;
                robCount <= 0;
            end
            else
                robCount <= robCount + 1;
        end
        else
        begin
            for(Integer i = 0; i < valueOf(CommitWidth); i=i+1)
            begin
                if(fromInteger(i) >= robCount)
                    commitTokenPort[i].send(tagged Invalid);
            end
            robState <= Update;
            robCount <= 0;
        end
    endrule

    rule update(robState == Update);
        let execReceive <- execResultPort[robCount].receive();
        let memAck      <- fpMemoryResp.receive();
        robCount == robCount + 1;
        if(isValid(execReceive))
        begin
            match {.execResult, .instResult} = validValue(execReceive);
            let robEntryMaybe = rob.read(execResult);
            if(isValid(robEntryMaybe)) //robtag is valid and tokens match
            begin
                let robEntry = validValue(robEntryMaybe);
                match {.branchTaken, .branchAddr, .finished, .result} = case (instResult) matches
                                                                            tagged RBranchTaken .addr: tuple4(True, addr, False, False);
                                                                            tagged RTerminate .result: tuple4(False, 0, True, result);
                                                                        endcase;
                let newRobEntry = robEntry;
                newRobEntry.taken = branchTaken;
                newRobEntry.finished = finished;
                rob.write(newRobEntry);

                pRegFile[exec.pRName] <= False;
                freeListFreeCount <= freeListFreeCount + 1;

                if(robEntry.isBranch || robEntry.isJR)
                    branchStack.remove(robEntry.branchIndex);

                if(robEntry.isBranch && robEntry.prediction != taken || robEntry.isJR && robEntry.predAddr != newAddr)
                begin
                    fpRewindToToken.send(token);
                    rob.updateTail(exec.robTag);
                    killInstBuffer     <= True;
                    nextKillInstBuffer <= True;
                    mispredictPC       <= tagged Valid newAddr;
                    $display("Branch mispredicted @ Model: %0d. Correct address: %x", modelCounter-1, newAddr);
                end
            end
            else
                killRobStage(exec.token);
        end
        if(robCount == fromInteger(valueOf(TSub#(NumFuncUnits,1))))
        begin
            robState <= Decode;
            robCount <= 0;
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

    rule decodeInst(decodeState == Decoding && !killInstBuffer && !realDecodeDone && tokenAddrBuffer.notEmpty() && decodeCount != fromInteger(valueOf(FetchWidth)));
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
            if(intQFreeCount != 0)// && branchCount != 0)
            begin
                $display("branchCount @ branch %0d", branchCount-1);
                issue.issueType = Branch;
                res.isBranch    = True;
                intQFreeCount  <= intQFreeCount - 1;
                branchCount    <= branchCount - 1;
                predictedPC    <= branchPredAddr;
                realDecodeDone     <= True;
                if(isValid(branchPredAddr))
                begin
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
            if(intQFreeCount != 0 && branchCount != 0)
            begin
                issue.issueType     = JR;
                res.isJR            = True;
                intQFreeCount      <= intQFreeCount - 1;
                branchCount        <= branchCount - 1; 
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
        else if(isJALR(currInst) && branchCount != 0)
        begin
            if(freeListFreeCount != 0 && intQFreeCount != 0)
            begin
                issue.issueType     = JALR;
                res.isJR            = True;
                freeListFreeCount  <= freeListFreeCount - 1;
                intQFreeCount      <= intQFreeCount - 1;
                branchCount        <= branchCount - 1;
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
            decodeCount     <= decodeCount + 1;
            instBufferCount <= instBufferCount-1;
            $display("Decode Decode: instBufferCount: %0d", instBufferCount+1);

            rob.writeTail(res);
            issuePort[decodeCount].send(tagged Valid issue);

            tokenAddrBuffer.deq();
            instBuffer.deq();
            decodeBuffer.deq();
            $display("Decode : Token: %0d, addr: %x, type: %0d @ Model: %0d, res.isBranch: %0d", currToken.index, currAddr, issue.issueType, modelCounter-1, res.isBranch);
        end
    endrule

    rule noInstInInstBuffer(decodeState == Decoding && !killInstBuffer && !realDecodeDone && !tokenAddrBuffer.notEmpty() && fetchState == FetchDone);
        realDecodeDone <= True;
    endrule

    rule decodeDone(decodeState == Decoding && !killInstBuffer && (realDecodeDone || decodeCount == fromInteger(valueOf(FetchWidth))));
        decodeState <= DecodeDone;
    endrule

    rule fillIssueQueues(decodeState == Decoding && realDecodeDone);
        for(FetchCount i = 0; i != fromInteger(valueOf(FetchWidth)); i=i+1)
        begin
            if(i >= decodeCount)
                issuePort[i].send(tagged Invalid);
        end
    endrule

    rule decodeKillInstBuffer(decodeState == Decoding && killInstBuffer);
        if(tokenAddrBuffer.notEmpty())
        begin
            tokenAddrBuffer.deq();
            instBuffer.deq();
            instBufferCount <= instBufferCount-1;
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
