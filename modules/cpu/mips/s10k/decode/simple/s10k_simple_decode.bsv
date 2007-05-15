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

    Vector#(FetchWidth, Port_Receive#(Tuple2#(Token, Addr)))        tokenAddrPort <- genWithM(receiveFunctionM("fetchToDecode_TokenAddr"));

    Port_Send#(FetchCount)                                instBufferFreeCountPort <- mkPort_Send("decodeToFetch_DecodeNum");
    Port_Send#(Addr)                                           predictedTakenPort <- mkPort_Send("decodeToFetch_PredictedTaken");
    Port_Send#(Addr)                                               mispredictPort <- mkPort_Send("decodeToFetch_Mispredict");

    Port_Receive#(IntQCountType)                                    intQCountPort <- mkPort_Receive("issueToDecodeIntQ", 1);
    Port_Receive#(AddrQCountType)                                  addrQCountPort <- mkPort_Receive("issueToDecodeAddrQ", 1);

    Vector#(FetchWidth, Port_Send#(IssueEntry))                         issuePort <- genWithM(sendFunctionM("decodeToIssue"));

    Vector#(NumFuncUnits, Port_Receive#(Tuple2#(ExecEntry, InstResult))) execPort <- genWithM(receiveFunctionM("execToDecode"));

    Vector#(CommitWidth, Port_Send#(Token))                            commitPort <- genWithM(sendFunctionM("decodeToCommit"));

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

    Reg#(Addr)                                                                 pc <- mkReg(pcStart);
    ROB                                                                       rob <- mkROB();
    Reg#(Vector#(PRNum, Bool))                                           pRegFile <- mkReg(replicate(False));
    BranchPred                                                         branchPred <- mkBranchPred();
    FIFO#(Addr)                                                      targetBuffer <- mkTargetBuffer(pcStart);

    function Addr getJumpAddr(PackedInst inst);
        let addr = inst[25:0];
        let pc_4 = pc + 4;
        return {pc_4[31:28], {addr, 2'b0}};
    endfunction

    function Addr getJALAddr(PackedInst inst);
        return getJumpAddr(inst);
    endfunction

    function Action restoreToToken(Token token);
    action
        fpRewindToToken.send(token);
    endaction
    endfunction

    function Action killROBStage(Token token);
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

    //Prefetch from fetch FP
    //Token matches exactly as we get from Fetch TP
    rule getFetchResp(True);
        let instTuple <- fpFetchResp.receive();
        instBuffer.enq(tpl_2(instTuple));
    endrule

    //Prefetch from decode FP
    //Token matches exactly as we get from Fetch TP
    rule getDecodeResp(True);
        let decodeTuple <- fpDecodeResp.receive();
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
                        /      \
                     Decode    Commit
                      /          \
                 DecodeDone    CommitDone
    */

    //Synchronization point
    rule synchronize(fetchState == FetchDone && decodeState == DecodeDone && robUpdateState == ROBUpdateDone && commitState == CommitDone);
        instBufferFreeCountPort.send(tagged Valid decodeNum);
        predictedTakenPort.send(predictedPC);
        mispredictPort.send(mispredictPC);

        let intQFreeCountLocal  <- intQCountPort.receive();
        let addrQFreeCountLocal <- addrQCountPort.receive();

        intQFreeCount      <= fromMaybe(fromInteger(valueOf(IntQCount)), intQFreeCountLocal);
        addrQFreeCount     <= fromMaybe(fromInteger(valueOf(AddrQCount)), addrQFreeCountLocal);

        fetchState         <= Fetch;
        fetchCount         <= 0;

        robUpdateState     <= ROBUpdate;
        let execReceive    <- execPort[0].receive();
        execEntry          <= execReceive;
        if(isValid(execReceive))
            rob.readAnyReq((tpl_1(validValue(execReceive))).robTag);

        robOpState         <= isValid(execReceive)? Write: Read;
        robUpdateCount     <= isValid(execReceive)? 0: 1;
        nextKillInstBuffer <= False;
    endrule

    rule robUpdateRead(robUpdateState == ROBUpdate && robOpState == Read && robUpdateCount != fromInteger(valueOf(NumFuncUnits)));
        let execReceive    <- execPort[robUpdateCount].receive();
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
        let robEntry       <- rob.readAnyResp();
        let memAck         <- fpMemoryResp.receive();
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
                mispredictPort.send(tagged Valid newAddr);
                pc                 <= newAddr;
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
        let tokenAddr <- tokenAddrPort[fetchCount].receive();
        if(isValid(tokenAddr))
        begin
            let tokenAddrVal = validValue(tokenAddr);
            tokenAddrBuffer.enq(tokenAddrVal);
            fpDecodeReq.send(tuple2(tpl_1(tokenAddrVal), ?));
        end
        fetchCount <= fetchCount + 1;
    endrule

    rule fetchDone(fetchState == Fetch && fetchCount == fromInteger(valueOf(FetchWidth)));
        fetchState <= FetchDone;
    endrule

    rule commit(commitState == Commit && !realCommitDone && commitCount != fromInteger(valueOf(CommitWidth)));
        let robEntry <- rob.readHeadResp();
        if(robEntry.done)
        begin
            rob.incrementHead();
            commitPort[commitCount].send(tagged Valid robEntry.token);
            if(robEntry.isBranch)
                branchPred.upd(robEntry.addr, robEntry.prediction, robEntry.taken);
            rob.readHeadReq();
            commitCount <= commitCount + 1;
        end
        else
            realCommitDone <= True;
    endrule

    rule commitDone(commitState == Commit && (realCommitDone || commitCount == fromInteger(valueOf(CommitWidth))));
        for(CommitCount i = 0; i < fromInteger(valueOf(FetchWidth)); i=i+1)
            if(i >= commitCount)
                commitPort[i].send(tagged Invalid);
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

    let branchPredAddr = branchPred.getPredAddr(currAddr);

    function ROBEntry getNewROBEntry();
        ROBEntry res = ROBEntry{token: currToken, addr: currAddr, done: False, finished: False,
                                isBranch: False, prediction: isValid(branchPredAddr), taken: False,
                                isJR: False, predAddr: targetBuffer.first()};
        res.done = isJump(currInst) || isJAL(currInst);
        res.isBranch = isBranch(currInst);
        res.prediction =isValid(branchPredAddr);
        res.isJR = isJR(currInst) || isJALR(currInst);
        res.predAddr = validValue(branchPredAddr);
        return res;
    endfunction

    function getNewIssueEntry();
        IssueEntry res = IssueEntry{issueType: Normal,
                                    token: currToken, robTag: rob.getTail(),
                                    src1Ready: src1Ready, src1: src1,
                                    src2Ready: src2Ready, src2: src2,
                                    dest: dest};

        if(isJump(currInst) || isJAL(currInst))
            res.issueType = J;
        else if(isJALR(currInst) || isJR(currInst))
            res.issueType = JR;
        else if(isBranch(currInst))
            res.issueType = Branch;
        else if(isShift(currInst))
            res.issueType = Shift;
        else if(isLoad(currInst))
            res.issueType = Load;
        else if(isStore(currInst))
            res.issueType = Store;
        return res;
    endfunction

    rule decodeALU(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                   isALU(currInst) && rob.notFull() && freeListFreeCount != 0 && intQFreeCount != 0);
        decodeNum         <= decodeNum + 1;
        freeListFreeCount <= freeListFreeCount - 1;
        intQFreeCount     <= intQFreeCount - 1;

        pRegFile[dest]    <= True;

        pc <= pc+4;

        rob.writeTail(getNewROBEntry());
        issuePort[decodeNum].send(tagged Valid getNewIssueEntry());
    endrule

    rule decodeALUNoSpace(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                          isALU(currInst) && !(rob.notFull() && freeListFreeCount != 0 && intQFreeCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeBranch(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                      isBranch(currInst) && rob.notFull() && intQFreeCount != 0 && branchCount != 0);
        decodeNum      <= decodeNum + 1;
        intQFreeCount  <= intQFreeCount - 1;
        branchCount    <= branchCount - 1;

        predictedPC    <= branchPredAddr;

        pc             <= isValid(branchPredAddr)? validValue(branchPredAddr): pc+4;

        rob.writeTail(getNewROBEntry());
        issuePort[decodeNum].send(tagged Valid getNewIssueEntry());

        if(isValid(branchPredAddr))
        begin
            realDecodeDone     <= True;
            nextKillInstBuffer <= True;
            killInstBuffer     <= True;
        end
    endrule

    rule decodeBranchNoSpace(decodeState == Decoding && !killInstBuffer && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                             isBranch(currInst) && !(rob.notFull() && intQFreeCount != 0 && branchCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeJump(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                    isJump(currInst) && rob.notFull() && branchCount != 0);
        decodeNum    <= decodeNum + 1;
        branchCount  <= branchCount - 1;

        let newPC     = getJumpAddr(currInst);
        predictedPC  <= tagged Valid newPC;

        pc           <= newPC;

        rob.writeTail(getNewROBEntry());
        issuePort[decodeNum].send(tagged Valid getNewIssueEntry());

        realDecodeDone     <= True;
        nextKillInstBuffer <= True;
        killInstBuffer     <= True;
    endrule

    rule decodeJumpNoSpace(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                           isJump(currInst) && !(rob.notFull() && branchCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeJAL(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                   isJAL(currInst) && rob.notFull() && freeListFreeCount != 0 && branchCount != 0);
        decodeNum         <= decodeNum + 1;
        freeListFreeCount <= freeListFreeCount - 1;
        branchCount       <= branchCount - 1;

        let newPC          = getJALAddr(currInst);
        predictedPC       <= tagged Valid newPC;

        pc                <= getJALAddr(currInst);

        rob.writeTail(getNewROBEntry());
        issuePort[decodeNum].send(tagged Valid getNewIssueEntry());

        targetBuffer.enq(newPC);

        realDecodeDone     <= True;
        nextKillInstBuffer <= True;
        killInstBuffer     <= True;
    endrule

    rule decodeJALNoSpace(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                          isJAL(currInst) && !(rob.notFull() && freeListFreeCount != 0 && branchCount != 0));
        decodeState <= DecodeDone;
    endrule

    rule decodeJR(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                  isJR(currInst) && rob.notFull() && branchCount != 0);
        decodeNum    <= decodeNum + 1;
        branchCount  <= branchCount - 1;

        let newPC     = targetBuffer.first();
        predictedPC  <= tagged Valid newPC;

        pc           <= newPC;

        rob.writeTail(getNewROBEntry());
        issuePort[decodeNum].send(tagged Valid getNewIssueEntry());

        targetBuffer.deq();

        realDecodeDone     <= True;
        nextKillInstBuffer <= True;
        killInstBuffer     <= True;
    endrule

    rule decodeJRNoSpace(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                  isJR(currInst) && !(rob.notFull() && branchCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeJALR(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                    isJALR(currInst) && rob.notFull() && freeListFreeCount != 0 && branchCount != 0);
        decodeNum         <= decodeNum + 1;
        freeListFreeCount <= freeListFreeCount - 1;
        branchCount       <= branchCount - 1;

        let newPC          = targetBuffer.first();
        predictedPC       <= tagged Valid newPC;

        pc                <= newPC;

        rob.writeTail(getNewROBEntry);
        issuePort[decodeNum].send(tagged Valid getNewIssueEntry());

        targetBuffer.deq();
        targetBuffer.enq(newPC);

        realDecodeDone     <= True;
        nextKillInstBuffer <= True;
        killInstBuffer     <= True;
    endrule

    rule decodeJALRNoSpace(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                           isJALR(currInst) && !(rob.notFull() && freeListFreeCount != 0 && branchCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeLoad(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                    isLoad(currInst) && rob.notFull() && freeListFreeCount != 0 && addrQFreeCount != 0);
        decodeNum         <= decodeNum + 1;
        freeListFreeCount <= freeListFreeCount - 1;
        addrQFreeCount    <= addrQFreeCount - 1;

        pRegFile[dest]    <= True;

        pc <= pc+4;

        rob.writeTail(getNewROBEntry());
        issuePort[decodeNum].send(tagged Valid getNewIssueEntry());
    endrule

    rule decodeLoadNoSpace(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                           isLoad(currInst) && !(rob.notFull() && freeListFreeCount != 0 && addrQFreeCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeStore(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                     isStore(currInst) && rob.notFull() && addrQFreeCount != 0);
        decodeNum         <= decodeNum + 1;
        addrQFreeCount    <= addrQFreeCount - 1;

        pc <= pc+4;

        rob.writeTail(getNewROBEntry());
        issuePort[decodeNum].send(tagged Valid getNewIssueEntry());
    endrule

    rule decodeStoreNoSpace(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                            isStore(currInst) && !(rob.notFull() && addrQFreeCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeDone(decodeState == Decoding && !killInstBuffer && decodeNum == fromInteger(valueOf(FetchWidth)));
        decodeState <= DecodeDone;
    endrule

    rule fillIssueQueues(decodeState == Decoding && realDecodeDone);
        for(FetchCount i = 0; i != fromInteger(valueOf(FetchWidth)); i=i+1)
            if(i > decodeNum)
                issuePort[i].send(tagged Invalid);
    endrule

    rule decodeKillInstBuffer(decodeState == Decoding && killInstBuffer && tokenAddrBuffer.notEmpty());
        tokenAddrBuffer.deq();
        instBuffer.deq();
        decodeBuffer.deq();

        killDecodeStage(tpl_1(tokenAddrBuffer.first()));
    endrule

    rule decodeKillInstBufferDone(decodeState == Decoding && killInstBuffer && !tokenAddrBuffer.notEmpty());
        killInstBuffer <= nextKillInstBuffer;
        decodeState    <= DecodeDone;
    endrule
endmodule
