import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import s10k_rob::*;
import hasim_parameters::*;
import hasim_types::*;
import BranchPred::*;
import TargetBuffer::*;

import FIFOF::*;
import FIFO::*;
import Vector::*;
import RegFile::*;

typedef enum {Commit, CommitDone}       CommitState    deriving (Bits, Eq);
typedef enum {ROBUpdate, ROBUpdateDone} ROBUpdateState deriving (Bits, Eq);
typedef enum {Read, Write}              ROBOpState     deriving (Bits, Eq);
typedef enum {Fetch, FetchDone}         FetchState     deriving (Bits, Eq);
typedef enum {Decoding, DecodeDone}     DecodeState    deriving (Bits, Eq);

function Bool isShift(PackedInst inst);
    let func = inst[31:26];
    let op   = inst[5:0];
    return (func == opFUNC) && (op == fcSLL || op == fcSRL || op == fcSRA || op == fcSLLV || op == fcSRLV || op == fcSRAV);
endfunction

function Bool isBranch(PackedInst inst);
    let func = inst[31:26];
    let rt   = inst[20:16];
    return (func == opBEQ || func == opBNE || func == opBLEZ || func == opBGTZ) || (func == opRT && (rt == rtBLTZ || rt == rtBGEZ));
endfunction

function Bool isJump(PackedInst inst);
    let func = inst[31:26];
    return func == opJ;
endfunction

function Bool isJAL(PackedInst inst);
    let func = inst[31:26];
    return func == opJAL;
endfunction

function Bool isJR(PackedInst inst);
    let func = inst[31:26];
    let op = inst[5:0];
    return func == opFUNC && op == fcJR;
endfunction

function Bool isJALR(PackedInst inst);
    let func = inst[31:26];
    let op = inst[5:0];
    return func == opFUNC && op == fcJALR;
endfunction

function Bool isLoad(PackedInst inst);
    let func = inst[31:26];
    return func == opLW;  
endfunction

function Bool isStore(PackedInst inst);
    let func = inst[31:26];
    return func == opSW;  
endfunction

function Bool isALU(PackedInst inst);
    return !(isBranch(inst) || isJump(inst) || isJAL(inst) || isJR(inst) || isJALR(inst) || isLoad(inst) || isStore(inst));
endfunction

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
    Port_Receive#(AddrQCountType)                                  addrQCountPort <- mkPort_Receive("issueToDecodeAddrQ", 1);

    Vector#(FetchWidth, Port_Send#(IssueEntry))                         issuePort <- genWithM(sendFunctionM("decodeToIssue"));

    Vector#(NumFuncUnits, Port_Receive#(Tuple2#(ExecEntry, InstResult)))
                                                                   execResultPort <- genWithM(receiveFunctionM("execToDecodeResult"));

    Vector#(CommitWidth, Port_Send#(Token))                       commitTokenPort <- genWithM(sendFunctionM("decodeToCommit"));

    FIFOF#(Tuple2#(Token, Addr))                                  tokenAddrBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(FetchWidth)));
    FIFOF#(PackedInst)                                                 instBuffer <- mkFIFOF();
    FIFOF#(DepInfo)                                                  decodeBuffer <- mkFIFOF();

    Reg#(CommitState)                                                 commitState <- mkReg(CommitDone);
    Reg#(ROBUpdateState)                                           robUpdateState <- mkReg(ROBUpdateDone);
    Reg#(ROBOpState)                                                   robOpState <- mkReg(?);
    Reg#(FetchState)                                                   fetchState <- mkReg(FetchDone);
    Reg#(DecodeState)                                                 decodeState <- mkReg(DecodeDone);
    Reg#(Bool)                                                     killInstBuffer <- mkReg(False);
    Reg#(Bool)                                                 nextKillInstBuffer <- mkReg(?);
    Reg#(Bool)                                                     realDecodeDone <- mkReg(?);
    Reg#(Bool)                                                     realCommitDone <- mkReg(?);

    Reg#(Maybe#(Tuple2#(ExecEntry, InstResult)))                        execEntry <- mkReg(?);

    Reg#(Bit#(TLog#(TAdd#(FreeListCount,1))))                   freeListFreeCount <- mkReg(fromInteger(valueOf(FreeListCount)));
    Reg#(Bit#(TLog#(TAdd#(IntQCount,1))))                           intQFreeCount <- mkReg(?);
    Reg#(Bit#(TLog#(TAdd#(AddrQCount,1))))                         addrQFreeCount <- mkReg(?);
    Reg#(Bit#(TLog#(TAdd#(BranchCount,1))))                           branchCount <- mkReg(fromInteger(valueOf(BranchCount)));

    Reg#(FetchCount)                                                   fetchCount <- mkReg(?);
    Reg#(FetchCount)                                                    decodeNum <- mkReg(fromInteger(valueOf(FetchWidth)));
    Reg#(FuncUnitPos)                                              robUpdateCount <- mkReg(?);
    Reg#(CommitCount)                                                 commitCount <- mkReg(?);
    Reg#(Maybe#(Addr))                                                predictedPC <- mkReg(tagged Invalid);
    Reg#(Maybe#(Addr))                                               mispredictPC <- mkReg(tagged Invalid);

    ROB                                                                       rob <- mkROB();
    Reg#(Vector#(PRNum, Bool))                                           pRegFile <- mkReg(replicate(False));
    BranchPred                                                         branchPred <- mkBranchPred();
    FIFO#(Addr)                                                      targetBuffer <- mkTargetBuffer(pcStart);

    Reg#(Bool)                                                              birth <- mkReg(True);

    Reg#(Bool)                                                      syncFromIssue <- mkReg(False);
    Reg#(Bool)                                                        syncToFetch <- mkReg(False);

    Reg#(Bit#(32))                                                   clockCounter <- mkReg(0);

    function Action restoreToToken(Token token);
    action
        fpRewindToToken.send(token);
        $display("&    fpRewindToToken.send(%x)", token);
    endaction
    endfunction

    function Action killROBStage(Token token);
    action
        fpTokKill.send(token);
        fpLocalCommitKill.send(token);
        fpMemStateKill.send(token);
        $display("&    fpTokKill.send(%x)", token);
        $display("&    fpLocalCommitKill.send(%x)", token);
        $display("&    fpMemStateKill.send(%x)", token);
    endaction
    endfunction
    
    function Action killDecodeStage(Token token);
    action
        fpTokKill.send(token);
        fpExeKill.send(token);
        $display("&    fpTokKill.send(%x)", token);
        $display("&    fpExeKill.send(%x)", token);
    endaction
    endfunction

    rule clockCount(True);
        clockCounter <= clockCounter + 1;
    endrule

    rule birthOfModel(birth);
        $display("&decode_birthOfModel: %d", clockCounter);
        let req <- finishServer.getReq();
        $display("&    req <- finishServer.getReq()");
        birth <= False;
    endrule

    rule getDecodeResp(True);
        $display("&decode_getDecodeResp: %d", clockCounter);
        let decodeTuple <- fpDecodeResp.receive();
        $display("&    decodeTuple <- fpDecodeResp.receive()");
        decodeBuffer.enq(tpl_2(decodeTuple));
    endrule

    //Currently the state transition diagram is:
    /*
              AllDone
                /   \
               /     ROBUpdate
           Fetch         |
             |       ROBUpdateDone
         FetchDone       /    \
      ----------------------   \
                        /       \
                     Decode    Commit
                      /           \
                 DecodeDone    CommitDone
    */

    //Synchronization point
    rule syncOver(syncToFetch && syncFromIssue);
        $display("&decode_syncOver: %d", clockCounter);
        syncToFetch   <= False;
        syncFromIssue <= False;

        fetchState         <= Fetch;
        fetchCount         <= 0;

        robUpdateState     <= ROBUpdate;
        let execReceive    <- execResultPort[0].receive();
        $display("&    Maybe#(%b, ...) <- execResultPort[0].receive()", isValid(execReceive));
        execEntry          <= execReceive;
        if(isValid(execReceive))
            rob.readAnyReq((tpl_1(validValue(execReceive))).robTag);

        robOpState         <= isValid(execReceive)? Write: Read;
        robUpdateCount     <= isValid(execReceive)? 0: 1;
        nextKillInstBuffer <= False;
    endrule

    rule synchronizeToFetch(fetchState == FetchDone && decodeState == DecodeDone && robUpdateState == ROBUpdateDone && commitState == CommitDone);
        $display("&decode_synchronizeToFetch: %d", clockCounter);
        decodeNumPort.send(tagged Valid decodeNum);
        predictedTakenPort.send(predictedPC);
        mispredictPort.send(mispredictPC);
        $display("&    decodeNumPort.send(tagged Valid %d)", decodeNum);
        $display("&    predictedTakenPort.send(Maybe#(%b, %x))", isValid(predictedPC), validValue(predictedPC));
        $display("&    mispredictPort.send(Maybe#(%b, %x))", isValid(mispredictPC), validValue(mispredictPC));
        syncToFetch <= True;
    endrule

    rule synchronizeFromIssue(fetchState == FetchDone && decodeState == DecodeDone && robUpdateState == ROBUpdateDone && commitState == CommitDone);
        let intQFreeCountLocal  <- intQCountPort.receive();
        let addrQFreeCountLocal <- addrQCountPort.receive();
        $display("&    Maybe#(%b, %d) <- intQCountPort.receive()", isValid(intQFreeCountLocal), validValue(intQFreeCountLocal));
        $display("&    Maybe#(%b, %d) <- addrQCountPort.receive()", isValid(addrQFreeCountLocal), validValue(addrQFreeCountLocal));

        intQFreeCount      <= fromMaybe(fromInteger(valueOf(IntQCount)), intQFreeCountLocal);
        addrQFreeCount     <= fromMaybe(fromInteger(valueOf(AddrQCount)), addrQFreeCountLocal);
        syncFromIssue <= True;
    endrule

    rule robUpdateRead(robUpdateState == ROBUpdate && robOpState == Read && robUpdateCount != fromInteger(valueOf(NumFuncUnits)));
        $display("&decode_robUpdateRead: %d", clockCounter);
        let execReceive    <- execResultPort[robUpdateCount].receive();
        $display("&    Maybe#(%b, ...) <- execResultPort[%d].receive()", isValid(execReceive), robUpdateCount);
        execEntry          <= execReceive;
        if(isValid(execReceive))
        begin
            rob.readAnyReq((tpl_1(validValue(execReceive))).robTag);
            robOpState     <= Write;
        end
        else
            robUpdateCount <= robUpdateCount + 1;
    endrule

    rule robUpdateWrite(robUpdateState == ROBUpdate && robOpState == Write);
        $display("&decode_robUpdateWrite: %d", clockCounter);
        let robEntry       <- rob.readAnyResp();
        let memAck         <- fpMemoryResp.receive();
        $display("&    ... <- fpMemoryResp.receive()");
        match {.exec, .res} = validValue(execEntry);
        if(rob.isROBTagValid(exec.robTag) && robEntry.token == exec.token)
        begin
            let taken = case (res) matches
                            tagged RBranchTaken .addr: True;
                            default: False;
                        endcase;

            let newAddr = case (res) matches
                              tagged RBranchTaken .addr: addr;
                          endcase;

            let finished = case (res) matches
                               tagged RTerminate .arbit: True;
                           endcase;

            rob.writeAny(exec.robTag, ROBEntry{token: robEntry.token, addr: robEntry.addr, done: True, finished: finished,
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
                finishServer.makeResp(tagged RESP_DoneRunning True);
        end
        else
            killROBStage(exec.token);

        robUpdateCount <= robUpdateCount + 1;
        robOpState     <= Read;
    endrule

    rule robUpdateDone(robUpdateState == ROBUpdate && robOpState == Read && robUpdateCount == fromInteger(valueOf(NumFuncUnits)));
        $display("&decode_robUpdateDone %d", clockCounter);
        robUpdateState  <= ROBUpdateDone;

        decodeState     <= Decoding;
        decodeNum       <= killInstBuffer? fromInteger(valueOf(FetchWidth)): 0;
        realDecodeDone  <= killInstBuffer? True: False;

        commitState     <= Commit;
        commitCount     <= 0;
        realCommitDone  <= False;
        rob.readHeadReq();

        predictedPC     <= tagged Invalid;
    endrule

    rule fetch(fetchState == Fetch && fetchCount != fromInteger(valueOf(FetchWidth)));
        $display("&decode_fetch %d", clockCounter);
        let tokenAddr <- tokenAddrPort[fetchCount].receive();
        $display("&    Maybe#(%b) <- tokenAddrPort[fetchCount].receive()", isValid(tokenAddr));
        if(isValid(tokenAddr))
        begin
            let tokenAddrVal = validValue(tokenAddr);
            tokenAddrBuffer.enq(tokenAddrVal);
            match {.token, .inst} <- fpFetchResp.receive();
            $display("&    {.%x, .%x} <- fpFetchResp.receive()", token, inst);
            instBuffer.enq(inst);
            fpDecodeReq.send(tuple2(tpl_1(tokenAddrVal), ?));
            $display("&    fpDecodeReq.send(...);");
        end
        fetchCount <= fetchCount + 1;
    endrule

    rule fetchDone(fetchState == Fetch && fetchCount == fromInteger(valueOf(FetchWidth)));
        $display("&decode_fetchDone %d", clockCounter);
        fetchState <= FetchDone;
    endrule

    rule commit(commitState == Commit && !realCommitDone && commitCount != fromInteger(valueOf(CommitWidth)));
        $display("&decode_commit %d", clockCounter);
        let robEntryMaybe <- rob.readHeadResp();
        let robEntry = validValue(robEntryMaybe);
        if(isValid(robEntryMaybe) && robEntry.done)
        begin
            rob.incrementHead();
            commitTokenPort[commitCount].send(tagged Valid robEntry.token);
            $display("&    commitTokenPort[%d].send(tagged Valid %x)", commitCount, robEntry.token);
            if(robEntry.isBranch)
                branchPred.upd(robEntry.addr, robEntry.prediction, robEntry.taken);
            rob.readHeadReq();
            commitCount <= commitCount + 1;
        end
        else
            realCommitDone <= True;
    endrule

    rule commitDone(commitState == Commit && (realCommitDone || commitCount == fromInteger(valueOf(CommitWidth))));
        $display("&decode_commitDone %d", clockCounter);
        for(CommitCount i = 0; i < fromInteger(valueOf(FetchWidth)); i=i+1)
        begin
            if(i >= commitCount)
            begin
                commitTokenPort[i].send(tagged Invalid);
                $display("&    commitTokenPort[%d].send(tagged Invalid)", i);
            end
        end
        commitState <= CommitDone;
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

    function Addr getJumpAddr(PackedInst inst);
        let addr = inst[25:0];
        let pc_4 = currAddr + 4;
        return {pc_4[31:28], {addr, 2'b0}};
    endfunction

    function Addr getJALAddr(PackedInst inst);
        return getJumpAddr(inst);
    endfunction

    rule decodeInst(decodeState == Decoding && !killInstBuffer && !realDecodeDone && tokenAddrBuffer.notEmpty() && decodeNum != fromInteger(valueOf(FetchWidth)));
        $display("decode_decodeInst %d", clockCounter);
        let branchPredAddr = branchPred.getPredAddr(currAddr);
        let jumpPredAddr   = targetBuffer.first();
        ROBEntry res = ROBEntry{token: currToken, addr: currAddr, done: False, finished: False,
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
            if(freeListFreeCount != 0 && addrQFreeCount != 0)
            begin
                issue.issueType    = Load;
                freeListFreeCount <= freeListFreeCount + 1;
                addrQFreeCount    <= addrQFreeCount - 1;
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
            if(addrQFreeCount != 0)
            begin
                issue.issueType    = Store;
                addrQFreeCount    <= addrQFreeCount - 1;
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
        else if(isJump(currInst))
        begin
            issue.issueType     = J;
            predictedPC        <= tagged Valid getJumpAddr(currInst);
            realDecodeDone     <= True;
            nextKillInstBuffer <= True;
            killInstBuffer     <= True;
            doCommonDecode      = True;
        end
        else if(isJAL(currInst))
        begin
            if(freeListFreeCount != 0)
            begin
                issue.issueType     = J;
                freeListFreeCount  <= freeListFreeCount - 1;
                let newPC           = getJALAddr(currInst);
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
            issue.issueType     = JR;
            res.isJR            = True;
            predictedPC <= tagged Valid jumpPredAddr;
            targetBuffer.deq();
            realDecodeDone     <= True;
            nextKillInstBuffer <= True;
            killInstBuffer     <= True;
            doCommonDecode      = True;
        end
        else if(isJALR(currInst))
        begin
            if(freeListFreeCount != 0)
            begin
                issue.issueType     = JR;
                res.isJR            = True;
                freeListFreeCount  <= freeListFreeCount - 1;
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
            $display("&    issuePort[%d].send(tagged Valid getNewIssueEntry())", decodeNum);

            tokenAddrBuffer.deq();
            instBuffer.deq();
            decodeBuffer.deq();
        end
    endrule

    rule noInst(decodeState == Decoding && !killInstBuffer && !realDecodeDone && !tokenAddrBuffer.notEmpty() && fetchState == FetchDone);
        $display("&decode_noInst %d", clockCounter);
        realDecodeDone <= True;
    endrule

    rule decodeDone(decodeState == Decoding && !killInstBuffer && (realDecodeDone || decodeNum == fromInteger(valueOf(FetchWidth))));
        $display("&decode_decodeDone %d", clockCounter);
        decodeState <= DecodeDone;
    endrule

    rule fillIssueQueues(decodeState == Decoding && realDecodeDone);
        $display("&decode_fillIssueQueues %d", clockCounter);
        for(FetchCount i = 0; i != fromInteger(valueOf(FetchWidth)); i=i+1)
        begin
            if(i > decodeNum)
            begin
                issuePort[i].send(tagged Invalid);
                $display("&    issuePort[%d].send(tagged Invalid)", i);
            end
        end
    endrule

    rule decodeKillInstBuffer(decodeState == Decoding && killInstBuffer && tokenAddrBuffer.notEmpty());
        $display("&decode_decodeKillInstBuffer %d", clockCounter);
        tokenAddrBuffer.deq();
        instBuffer.deq();
        decodeBuffer.deq();

        killDecodeStage(tpl_1(tokenAddrBuffer.first()));
    endrule

    rule decodeKillInstBufferDone(decodeState == Decoding && killInstBuffer && !tokenAddrBuffer.notEmpty());
        $display("&decode_decodeKillInstBufferDone %d", clockCounter);
        killInstBuffer <= nextKillInstBuffer;
        decodeState    <= DecodeDone;
    endrule
endmodule
