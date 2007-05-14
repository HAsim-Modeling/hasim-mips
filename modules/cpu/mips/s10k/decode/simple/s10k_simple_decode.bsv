import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import s10k_simple_common::*;
import s10k_rob::*;
import BranchPred::*;
import TargetBuffer::*;

import FIFOF::*;
import FIFO::*;
import Vector::*;
import RegFile::*;

interface Decode;
    method Bool done();
endinterface

typedef enum {CheckMispredict, CheckMispredictDone} CheckMispredictState deriving (Bits, Eq);
typedef enum {Commit, CommitDone}                   CommitState          deriving (Bits, Eq);
typedef enum {Exec1, Exec2, Exec3, ROBUpdateDone}   ROBUpdateState       deriving (Bits, Eq);
typedef enum {Read, Write}                          ROBOpState           deriving (Bits, Eq);
typedef enum {Fetch, FetchDone}                     FetchState           deriving (Bits, Eq);
typedef enum {Decoding, DecodeDone}                 DecodeState          deriving (Bits, Eq);

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

module [HASim_Module] mkDecode(Decode);
    Connection_Receive#(Tuple2#(Token, PackedInst))            fpFetchResp <- mkConnection_Receive("fp_fet_resp");
    Connection_Send#(Tuple2#(Token, void))                     fpDecodeReq <- mkConnection_Send("fp_dec_req");
    Connection_Receive#(Tuple2#(Token, DepInfo))              fpDecodeResp <- mkConnection_Receive("fp_dec_resp");

    Connection_Send#(Token)                                      fpTokKill <- mkConnection_Send("fp_tok_kill");
    Connection_Send#(Token)                                    fpFetchKill <- mkConnection_Send("fp_fet_kill");
    Connection_Send#(Token)                                   fpDecodeKill <- mkConnection_Send("fp_dec_kill");
    Connection_Send#(Token)                                      fpExeKill <- mkConnection_Send("fp_exe_kill");
    Connection_Send#(Token)                                      fpMemKill <- mkConnection_Send("fp_mem_kill");
    Connection_Send#(Token)                              fpLocalCommitKill <- mkConnection_Send("fp_lco_kill");
    Connection_Send#(Token)                             fpGlobalCommitKill <- mkConnection_Send("fp_gco_kill");
    Connection_Send#(Token)                                fpRewindToToken <- mkConnection_Send("fp_rewindToToken");
  
    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, fromInteger(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, fromInteger(i)), 1);

    Port_Receive#(IntQCountType)                             intQCountPort <- mkPort_Receive("issueToDecodeIntQ", 1);
    Port_Receive#(AddrQCountType)                           addrQCountPort <- mkPort_Receive("issueToDecodeAddrQ", 1);

    Port_Receive#(ExecEntry)                                     exec1Port <- mkPort_Receive("execToDecode1", 1);
    Port_Receive#(ExecEntry)                                     exec2Port <- mkPort_Receive("execToDecode2", 1);
    Port_Receive#(ExecEntry)                                     exec3Port <- mkPort_Receive("execToDecode3", 1);
    Port_Receive#(Addr)                                    branchTakenPort <- mkPort_Receive("execToDecodeBranch", 1);

    Vector#(FetchWidth, Port_Receive#(Tuple2#(Token, Addr))) tokenAddrPort <- genWithM(receiveFunctionM("fetchToDecode_TokenAddr"));

    Port_Send#(FetchCount)                         instBufferFreeCountPort <- mkPort_Send("decodeToFetch_DecodeNum");
    Port_Send#(Addr)                                    predictedTakenPort <- mkPort_Send("decodeToFetch_PredictedTaken");
    Port_Send#(Addr)                                        mispredictPort <- mkPort_Send("decodeToFetch_MispredictTaken");

    Vector#(CommitWidth, Port_Send#(Token))                     commitPort <- genWithM(sendFunctionM("decodeToCommit"));
    Vector#(FetchWidth, Port_Send#(IssueEntry))                  issuePort <- genWithM(sendFunctionM("decodeToIssue"));

    Port_Send#(Token)                                          execJALPort <- mkPort_Send("DecodeToIssueJAL");

    FIFOF#(Tuple2#(Token, Addr))                           tokenAddrBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(FetchWidth)));
    FIFOF#(PackedInst)                                          instBuffer <- mkFIFOF();
    FIFOF#(DepInfo)                                           decodeBuffer <- mkFIFOF();

    Reg#(CheckMispredictState)                        checkMispredictState <- mkReg(CheckMispredictDone);
    Reg#(CommitState)                                          commitState <- mkReg(CommitDone);
    Reg#(ROBUpdateState)                                    robUpdateState <- mkReg(ROBUpdateDone);
    Reg#(ROBOpState)                                            robOpState <- mkReg(?);
    Reg#(FetchState)                                            fetchState <- mkReg(FetchDone);
    Reg#(DecodeState)                                          decodeState <- mkReg(DecodeDone);
    Reg#(Bool)                                              killInstBuffer <- mkReg(False);
    Reg#(Bool)                                          nextKillInstBuffer <- mkReg(?);
    Reg#(Bool)                                              realDecodeDone <- mkReg(?);

    Reg#(Maybe#(ExecEntry))                                      execEntry <- mkReg(?);
    Reg#(Maybe#(Addr))                                         branchTaken <- mkReg(?);

    Reg#(Bit#(TLog#(TAdd#(FreeListCount,1))))            freeListFreeCount <- mkReg(fromInteger(valueOf(FreeListCount)));
    Reg#(Bit#(TLog#(TAdd#(IntQCount,1))))                    intQFreeCount <- mkReg(?);
    Reg#(Bit#(TLog#(TAdd#(AddrQCount,1))))                  addrQFreeCount <- mkReg(?);

    Reg#(FetchCount)                                            fetchCount <- mkReg(?);
    Reg#(CommitCount)                                          commitCount <- mkReg(?);
    Reg#(FetchCount)                                             decodeNum <- mkReg(fromInteger(valueOf(FetchWidth)));
    Reg#(Maybe#(Addr))                                         predictedPC <- mkReg(tagged Invalid);

    ROB                                                                rob <- mkROB();
    RegFile#(PRName, Bool)                                        pRegFile <- mkRegFileFull();
    BranchPred                                                  branchPred <- mkBranchPred();
    FIFO#(Addr)                                               targetBuffer <- mkTargetBuffer(pcStart);

    Reg#(Addr)                                                          pc <- mkReg(pcStart);

    function Addr getJumpAddr(PackedInst inst);
        let addr = inst[25:0];
        return pc + zeroExtend(addr<<2);
    endfunction

    function Addr getJALAddr(PackedInst inst);
        return getJumpAddr(inst);
    endfunction

    function Action restoreToToken(Token token);
    action
        fpRewindToToken.send(token);
    endaction
    endfunction

    //check
    function Action killROBStage(Token token);
    action
        fpMemKill.send(token);
        fpExeKill.send(token);
    endaction
    endfunction
    
    function Action killDecodeStage(Token token);
    action
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
            CheckMispredict
                /\
               /  \
              /    \
           Fetch   CheckMispredictDone
             |             /\
             |            /  \
         FetchDone       /    \
                        /      \
                   ROBUpdate    Commit
                       |           \
                 ROBUpdataDone    CommitDone
                       |
                    Decode
                       |
                  DecodeDone

         The Decode states is interwined with killInstBuffer. So if killInstBuffer is true, it kills instead of decode.
         Also a branch instruction can individually set the killInstBuffer.

         Note that decode could not be done in parallel with ROBUpdate, because it requires the physical 
         register busy information, which is got only after ROBUpdate.

         And one more thing is Fetch, Decode and Commit have associated counts with it, to count till widths.
         Also ROBUpdate is a complex of many states, done sequentially.
    */

    //Synchronization point
    rule synchronize(checkMispredictState == CheckMispredictDone && commitState == CommitDone &&
                     robUpdateState == ROBUpdateDone && fetchState == FetchDone && decodeState == DecodeDone);
        instBufferFreeCountPort.send(tagged Valid decodeNum);
        predictedTakenPort.send(predictedPC);

        let intQFreeCountLocal  <- intQCountPort.receive();
        let addrQFreeCountLocal <- addrQCountPort.receive();

        intQFreeCount    <= fromMaybe(fromInteger(valueOf(IntQCount)), intQFreeCountLocal);
        addrQFreeCount   <= fromMaybe(fromInteger(valueOf(AddrQCount)), addrQFreeCountLocal);

        let exec         <- exec1Port.receive();
        execEntry        <= exec;
        let _branchTaken <- branchTakenPort.receive();
        branchTaken      <= _branchTaken;

        if(isValid(exec))
            rob.readAnyReq(validValue(exec).robTag);

        fetchState           <= Fetch;
        checkMispredictState <= CheckMispredict;
    endrule

    //Common code in updating ROB complete information from the execute unit results //Check
    function Action updateExecToROB(Bool validateBranch);
    action
        if(isValid(execEntry))
        begin
            let robEntry <- rob.readAnyResp();
            let exec = validValue(execEntry);
            if(rob.isROBTagValid(exec.robTag) && robEntry.token == exec.token)
            begin
                rob.writeAny(exec.robTag, ROBEntry{token: robEntry.token, addr: robEntry.addr, done: True,
                                                   isBranch: robEntry.isBranch,
                                                   prediction: robEntry.prediction, taken: isValid(branchTaken),
                                                   isJR: robEntry.isJR, predAddr: robEntry.predAddr});
                if(validateBranch &&
                    (robEntry.isBranch && robEntry.prediction != isValid(branchTaken) ||
                    robEntry.isJR && robEntry.predAddr != validValue(branchTaken)))
                begin
                    restoreToToken(exec.token);
                    rob.updateTail(exec.robTag);
                    killInstBuffer <= True;
                    nextKillInstBuffer <= True;
                    mispredictPort.send(branchTaken);
                end
                pRegFile.upd(exec.pRName, False);
                freeListFreeCount <= freeListFreeCount + 1;
            end
            else
                killROBStage(exec.token);
        end
    endaction
    endfunction

    rule checkMispredict(checkMispredictState == CheckMispredict);
        commitState          <= Commit;
        rob.readHeadReq();
        commitCount          <= 0;

        checkMispredictState <= CheckMispredictDone;
        updateExecToROB(True);
        let exec             <- exec2Port.receive();
        execEntry            <= exec;
        robUpdateState       <= Exec2;
        robOpState           <= Read;
    endrule

    rule exec2Read(robUpdateState == Exec2 && robOpState == Read);
        let exec = validValue(execEntry);
        if(isValid(execEntry))
            rob.readAnyReq(exec.robTag);
        robOpState <= Write;
    endrule

    rule exec2Write(robUpdateState == Exec2 && robOpState == Write);
        updateExecToROB(False);
        let exec       <- exec3Port.receive();
        execEntry      <= exec;
        robUpdateState <= Exec3;
        robOpState     <= Read;
    endrule

    rule exec3Read(robUpdateState == Exec3 && robOpState == Read);
        let exec = validValue(execEntry);
        if(isValid(execEntry))
            rob.readAnyReq(exec.robTag);
        robOpState <= Write;
    endrule

    //Note that decodeNum is set to 0 in case of normal decode, otherwise set to width
    rule exec3Write(robUpdateState == Exec3 && robOpState == Write);
        updateExecToROB(False);
        robUpdateState     <= ROBUpdateDone;
        decodeState        <= Decoding;
        decodeNum          <= killInstBuffer? fromInteger(valueOf(FetchWidth)): 0;
        realDecodeDone     <= killInstBuffer? True: False;
        predictedPC        <= tagged Invalid;
        nextKillInstBuffer <= False;
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
        fetchCount <= 0;
        fetchState <= FetchDone;
    endrule

    rule commit(commitState == Commit && commitCount != fromInteger(valueOf(CommitWidth)));
        let robEntry <- rob.readHeadResp();
        if(robEntry.done)
        begin
            rob.incrementHead();
            commitPort[commitCount].send(tagged Valid robEntry.token);
            branchPred.upd(robEntry.addr, robEntry.prediction, robEntry.taken);
        end
        rob.readHeadReq();
        commitCount <= commitCount + 1;
    endrule

    rule commitDone(commitState == Commit && commitCount == fromInteger(valueOf(CommitWidth)));
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

    let src1Ready = src1Valid? !pRegFile.sub(src1): True;
    let src2Ready = src2Valid? !pRegFile.sub(src2): True;

    let branchPredAddr = branchPred.getPredAddr(currAddr);

    function getNewROBEntry = ROBEntry{token: currToken, addr: currAddr, done: False,
                                       isBranch: False,
                                       prediction: isValid(branchPredAddr), taken: ?,
                                       isJR: False, predAddr: targetBuffer.first()};

    function getNewIssueEntry = IssueEntry{issueType: ALU,
                                           token: currToken, robTag: rob.getTail(),
                                           src1Ready: src1Ready, src1: src1,
                                           src2Ready: src2Ready, src2: src2,
                                           dest: dest,
                                           alu1: isShift(currInst) || isBranch(currInst)};

    rule decodeALUSpace(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                        isALU(currInst) && rob.notFull() && freeListFreeCount != 0 && intQFreeCount != 0);
        decodeNum         <= decodeNum + 1;
        freeListFreeCount <= freeListFreeCount - 1;
        intQFreeCount     <= intQFreeCount - 1;

        pRegFile.upd(dest, True);

        pc <= pc+4;

        let newROBEntry = getNewROBEntry();
        rob.writeTail(newROBEntry);

        let newIssueEntry = getNewIssueEntry();
        issuePort[decodeNum].send(tagged Valid newIssueEntry);
    endrule

    rule decodeALUNoSpace(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                          isALU(currInst) && !(rob.notFull() && freeListFreeCount != 0 && intQFreeCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeBranch(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                      isBranch(currInst) && rob.notFull() && intQFreeCount != 0);
        decodeNum      <= decodeNum + 1;
        intQFreeCount  <= intQFreeCount - 1;

        predictedPC    <= branchPredAddr;

        pc <= isValid(branchPredAddr)? validValue(branchPredAddr): pc+4;

        let newROBEntry = getNewROBEntry();
        newROBEntry.isBranch = True;
        rob.writeTail(newROBEntry);

        let newIssueEntry = getNewIssueEntry();
        issuePort[decodeNum].send(tagged Valid newIssueEntry);

        if(isValid(branchPredAddr))
        begin
            realDecodeDone     <= True;
            nextKillInstBuffer <= True;
            killInstBuffer     <= True;
        end
    endrule

    rule decodeBranchNoSpace(decodeState == Decoding && !killInstBuffer && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                             isBranch(currInst) && !(rob.notFull() && intQFreeCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeJump(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                    isJump(currInst) && rob.notFull());
        decodeNum    <= decodeNum + 1;

        predictedPC  <= tagged Valid getJumpAddr(currInst);

        pc <= getJumpAddr(currInst);

        let newROBEntry = getNewROBEntry();
        newROBEntry.done = True;
        rob.writeTail(newROBEntry);

        issuePort[decodeNum].send(tagged Invalid);

        realDecodeDone     <= True;
        nextKillInstBuffer <= True;
        killInstBuffer     <= True;
    endrule

    rule decodeJumpNoSpace(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                           isJump(currInst) && !(rob.notFull()));
        realDecodeDone <= True;
    endrule

    rule decodeJAL(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                   isJAL(currInst) && rob.notFull() && freeListFreeCount != 0);
        decodeNum         <= decodeNum + 1;
        freeListFreeCount <= freeListFreeCount - 1;

        predictedPC  <= tagged Valid getJALAddr(currInst);

        pc <= getJALAddr(currInst);

        execJALPort.send(tagged Valid currToken);

        let newROBEntry = getNewROBEntry();
        newROBEntry.done = True;
        rob.writeTail(newROBEntry);

        issuePort[decodeNum].send(tagged Invalid);

        targetBuffer.enq(getJALAddr(currInst));

        realDecodeDone     <= True;
        nextKillInstBuffer <= True;
        killInstBuffer     <= True;
    endrule

    rule decodeJALNoSpace(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                          isJAL(currInst) && !(rob.notFull() && freeListFreeCount != 0));
        decodeState <= DecodeDone;
    endrule

    rule decodeJR(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                  isJR(currInst) && rob.notFull());
        decodeNum         <= decodeNum + 1;

        predictedPC  <= tagged Valid targetBuffer.first();

        pc <= targetBuffer.first();

        let newROBEntry = getNewROBEntry();
        newROBEntry.done = True;
        newROBEntry.isJR = True;
        rob.writeTail(newROBEntry);

        issuePort[decodeNum].send(tagged Invalid);

        targetBuffer.deq();

        realDecodeDone     <= True;
        nextKillInstBuffer <= True;
        killInstBuffer     <= True;
    endrule

    rule decodeJRNoSpace(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                  isJR(currInst) && !(rob.notFull()));
        realDecodeDone <= True;
    endrule

    rule decodeJALR(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                    isJR(currInst) && rob.notFull() && freeListFreeCount != 0);
        decodeNum         <= decodeNum + 1;
        freeListFreeCount <= freeListFreeCount - 1;

        predictedPC  <= tagged Valid targetBuffer.first();

        pc <= targetBuffer.first();

        execJALPort.send(tagged Valid currToken);

        let newROBEntry = getNewROBEntry();
        newROBEntry.done = True;
        newROBEntry.isJR = True;
        rob.writeTail(newROBEntry);

        issuePort[decodeNum].send(tagged Invalid);

        targetBuffer.deq();
        targetBuffer.enq(pc);

        realDecodeDone     <= True;
        nextKillInstBuffer <= True;
        killInstBuffer     <= True;
    endrule

    rule decodeJALRNoSpace(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                           isJR(currInst) && !(rob.notFull() && freeListFreeCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeLoad(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                    isLoad(currInst) && rob.notFull() && freeListFreeCount != 0 && addrQFreeCount != 0);
        decodeNum         <= decodeNum + 1;
        freeListFreeCount <= freeListFreeCount - 1;
        addrQFreeCount    <= addrQFreeCount - 1;

        pRegFile.upd(dest, True);

        pc <= pc + 4;

        let newROBEntry = getNewROBEntry();
        rob.writeTail(newROBEntry);

        let newIssueEntry = getNewIssueEntry();
        newIssueEntry.issueType = Load;

        issuePort[decodeNum].send(tagged Valid newIssueEntry);
    endrule

    rule decodeLoadNoSpace(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                           isLoad(currInst) && !(rob.notFull() && freeListFreeCount != 0 && addrQFreeCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeStore(decodeState == Decoding && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                     isStore(currInst) && rob.notFull() && addrQFreeCount != 0);
        decodeNum         <= decodeNum + 1;
        addrQFreeCount    <= addrQFreeCount - 1;

        pc <= pc + 4;

        let newROBEntry = getNewROBEntry();
        rob.writeTail(newROBEntry);

        let newIssueEntry = getNewIssueEntry();
        newIssueEntry.issueType = Store;

        issuePort[decodeNum].send(tagged Valid newIssueEntry);
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
