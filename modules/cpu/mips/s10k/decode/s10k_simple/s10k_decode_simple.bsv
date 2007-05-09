import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import FIFOF::*;
import Vector::*;

interface Decode;
    method Bool done();
endinterface

typedef 4 FetchWidth;
typedef 4 CommitWidth
typedef Bit#(TLog#(TAdd#(FetchWidth,1))) FetchCount;

typedef 32 ROBCount;
typedef 16 IntQCount;
typedef 16 AddrQCount;
typedef 32 FreeListCount;
typedef Bit#(TLog#(ROBCount)) ROBTag;

typedef enum {CheckMispredict, CheckMispredictDone} CheckMispredictState deriving (Bits, Eq);
typedef enum {Commit, CommitDone}                   CommitState          deriving (Bits, Eq);
typedef enum {ALU1, ALU2, LoadStore, ROBUpdateDone} ROBUpdateState       deriving (Bits, Eq);
typedef enum {Read, Write}                          ROBOpState           deriving (Bits, Eq);
typedef enum {Fetch, FetchDone}                     FetchState           deriving (Bits, Eq);
typedef enum {Decode, DecodeDone}                   DecodeState          deriving (Bits, Eq);

typedef enum {ALU, LoadStore} IssueType deriving (Bits, Eq);

typedef struct {
    IssueType issueType;
    Token     token;
    ROBTag    robTag;
    Bool      src1Ready;
    PRName    src1;
    Bool      src2Ready;
    PRName    src2;
    PRName    dest;
    Bool      alu1;
} IssueEntry deriving (Bits, Eq);

/*
    TODO:
    1) rob design
    2) branchPred design
    3) targetBuffer design
*/


module [HASim_Module] mkDecode(Decode);
    Connection_Receive#(Tuple2#(Token, PackedInst))            fpFetchResp <- mkConnection_Receive("fp_fet_resp");
    Connection_Send#(Tuple2#(Token, void))                     fpDecodeReq <- mkConnection_Send("fp_dec_req");
    Connection_Receive#(Tuple2#(Token, DepInfo))              fpDecodeResp <- mkConnection_Receive("fp_dec_resp");

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, fromInteger(i)), 1);
    Vector#(FetchWidth, Port_Receive#(Tuple2#(Token, Addr))) tokenAddrPort <- genWithM(receiveFunctionM("fetchToDecode_TokenAddr"));

    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, fromInteger(i)));
    Vector#(CommitWidth, Port_Send#(Token))                     commitPort <- genWithM(sendFunctionM("decodeToCommit"));

    Vector#(FetchWidth, Port_Send#(IssueEntry))                  issuePort <- genWithM(sendFunctionM("decodeToIssue"));

    Port_Receive#(Tuple4#(Token, PRName, ROBTag, Maybe#(Addr)))   alu1Port <- mkPort_Receive("alu1ToDecode");
    Port_Receive#(Tuple3#(Token, PRName, ROBTag))                 alu2Port <- mkPort_Receive("alu2ToDecode");
    Port_Receive#(Tuple3#(Token, PRName, ROBTag))            loadStorePort <- mkPort_Receive("loadStoreToDecode");

    Port_Send#(FetchCount)                         instBufferFreeCountPort <- mkPort_Send("decodeToFetch_FreeInstBufferNum");
    Port_Send#(Addr)                                    predictedTakenPort <- mkPort_Send("decodeToFetch_PredictedTaken");

    FIFOF#(Tuple2#(Token, Addr))                           tokenAddrBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(FetchWidth)));
    FIFOF#(PackedInst)                                          instBuffer <- mkFIFOF();
    FIFOF#(DepInfo)                                           decodeBuffer <- mkFIFOF();

    Reg#(CheckMispredicState)                         checkMispredictState <- mkReg(CheckMispredictDone);
    Reg#(CommitState)                                          commitState <- mkReg(?);
    Reg#(ROBUpdateState)                                    robUpdateState <- mkReg(?);
    Reg#(ROBOpState)                                            robOpState <- mkReg(?);
    Reg#(FetchState)                                            fetchState <- mkReg(?);
    Reg#(DecodeState)                                          decodeState <- mkReg(?);
    Reg#(Bool)                                              killInstBuffer <- mkReg(False);
    Reg#(Bool)                                          nextKillInstBuffer <- mkReg(?);
    Reg#(Bool)                                              realDecodeDone <- mkReg(?);

    Reg#(Token)                                                  execToken <- mkReg(?);
    Reg#(PRName)                                                execPRName <- mkReg(?);
    Reg#(ROBTag)                                                execROBTag <- mkReg(?);
    Reg#(Maybe#(Addr))                                     execBranchTaken <- mkReg(?);

    Reg#(Bit#(TLog#(TAdd#(FreeListCount,1))))            freeListFreeCount <- mkReg(fromInteger(valueOf(FreeListCount)));
    Reg#(Bit#(TLog#(TAdd#(IntQCount,0))))                    intQFreeCount <- mkReg(?);
    Reg#(Bit#(TLog#(TAdd#(AddrQCount,1))))                  addrQFreeCount <- mkReg(?);

    Reg#(FetchCount)                                            fetchCount <- mkReg(?);
    Reg#(CommitCount)                                          commitCount <- mkReg(?);
    Reg#(FetchCount)                                           decodeCount <- mkReg(fromInteger(valueOf(FetchWidth)));
    Reg#(Maybe#(Addr))                                         predictedPC <- mkReg(tagged Invalid);

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
    rule sendReceivePorts(checkMispredictState == CheckMispredictDone && commitState == CommitDone &&
                          robUpdateState == ROBUpdateDone && fetchState == FetchDone && decodeState == DecodeDone);

        instBufferFreeCountPort.send(tagged Valid decodeCount);
        predictedTakenPort.send(predictedPC);

        let intQFreeCountLocal  <- intQCountPort.receive();
        let addrQFreeCountLocal <- addrQCountPort.receive();

        intQFreeCount  <= fromMaybe(valueOf(IntQCount), intQFreeCountLocal);
        addrQFreeCount <= fromMaybe(valueOf(AddrQCount), addrQFreeCountLocal);

        match {token, pRName, robTag, branchTaken} <- alu1Port.receive();
        execToken       <= token;
        execPRName      <= pRName;
        execROBTag      <= robTag;
        execBranchTaken <= branchTaken;
        rob.readAnyReq(robTag);

        fetchState           <= Fetch;
        checkMispredictState <= CheckMispredict;
    endrule

    rule fetch(fetchState == Fetch && fetchCount != fromInteger(valueOf(FetchWidth)));
        let tokenAddr <- tokenAddrPort.receive();
        if(isValid(tokenAddr))
        begin
            let tokenAddrVal = validValue(tokenAddrBuffer);
            tokenAddrBuffer.enq(tokenAddrVal);
            fpDecodeReq.send(tpl1(tokenAddrVal));
        end
        fetchCount <= fetchCount + 1;
    endrule

    rule fetchDone(fetchState == Fetch && fetchCount == fromInteger(valueOf(FetchWidth)));
        fetchCount <= 0;
        fetchState <= FetchDone;
    endrule

    //Common code in updating ROB complete information from the execute unit results
    function Action updateExecToROB(Bool validateBranch);
    action
        let robEntry <- rob.readAnyResp();
        if(isROBTagValid(execROBTag) && robEntry.token == execToken)
        begin
            rob.writeAny(execROBTag, ROBEntry{token: execToken, addr: robEntry.addr, done: True,
                                              isBranch: robEntry: isBranch,
                                              prediction: robEntry.prediction, taken: isValid(execBranchTaken),
                                              isJR: robEntry.isJR, predAddr: robEntry.predAddr,
                                              realAddr: validValue(execBranchTaken)});
            if(validateBranch &&
               (robEntry.isBranch && robEntry.prediction != isValid(execBranchTaken) ||
                robEntry.isJR && robEntry.predAddr != validValue(execBranchTaken)))
            begin
                
                restoreToToken(execToken);
                rob.updateTail(execROBTag);
                killInstBuffer <= True;
            end
            pRegFile.upd(execPRName, False);
            freeListFreeCount <= freeListFreeCount + 1;
        end
        else
            killROBStage(execToken);
    endaction
    endfunction

    rule checkMispredict(checkMispredictState == CheckMispredict);
        commitState          <= Commit;
        commitCount          <= 0;
        rob.readHeadReq();

        checkMispredictState <= CheckMispredictDone;
        updateExecToROB(True);
        match {token, pName, robTag} <- alu2Port.receive();
        execToken            <= token;
        execPRName           <= pName;
        execROBTag           <= robTag;
        robUpdateState       <= ALU2;
        robOpState           <= Read;
    endrule

    rule alu2Read(robUpdateState == ALU2 && robOpState == Read);
        rob.readAnyReq(execROBTag);
        robOpState <= Write;
    endrule

    rule alu2Write(robUpdateState == ALU2 && robOpState == Write);
        updateExecToROB(False);
        match {token, pName, robTag} <- loadStorePort.receive();
        execToken      <= token;
        execPRName     <= pName;
        execROBTag     <= robTag;
        robUpdateState <= LoadStore;
        robOpState     <= Read;
    endrule

    rule loadStoreRead(robUpdateState == LoadStore && robOpState == Read);
        rob.readAnyReq(execROBTag);
        robOpState <= Write;
    endrule

    //Note that decodeNum is set to 0 in case of normal decode, otherwise set to width
    rule loadStoreWrite(robUpdateState == LoadStore && robOpState == Write);
        updateExecToROB(False);
        robUpdateState <= ROBUpdateDone;
        decodeState    <= Decode;
        decodeNum      <= killInstBuffer? fromInteger(valueOf(FetchWidth)): 0;
        realDecodeDone <= killInstBuffer? True: False;
        predictedPC    <= tagged Invalid;
    endrule

    rule commit(commitState == Commit && commitCount != fromInteger(valueOf(CommitWidth)));
        let robEntry <- rob.readHeadResp();
        if(robEntry.done)
        begin
            rob.incrementHead();
            commitPort[commitCount].send(robEntry.token);
            branchPred.upd(robEntry.addr, robEntry.prediction, robEntry.taken);
        end
        rob.readHeadReq();
        commitCount <= commitCount + 1;
    endrule

    rule commitDone(commitState == Commit && commitCount == fromInteger(valueOf(CommitWidth)));
        commitState <= commitDone;
    endrule

    let currInst                  = instBuffer.first();
    let currDepInfo               = decodeBuffer.first();
    match {.currToken, .currAddr} = tokenAddrBuffer.first();

    let src1Valid = isValid(currDepInfo.dep_src1);
    let src1      = tpl2(validValue(currDepInfo.dep_src1));
    let src2Valid = isValid(currDepInfo.dep_src2);
    let src2      = tpl2(validValue(currDepInfo.dep_src2));
    let dest      = tpl2(validValue(currDepInfo.dep_dest));

    let src1Ready = src1Valid? !pRegFile.sub(src1): True;
    let src2Ready = src2Valid? !pRegFile.sub(src2): True;

    let branchPredAddr = branchPred.getPredAddr(currAddr);

    let newROBEntry = ROBEntry{token: currToken, addr: currAddr, done: False,
                               isBranch: False,
                               prediction: isValid(branchPredAddr), taken: ?,
                               isJR: False, predAddr: targetBuffer.first(),
                               realAddr: ?};

    let newIssueEntry = IssueEntry{issueType: ALU,
                                   token: currToken, robTag: rob.getTail(),
                                   src1Ready: src1Ready, src1: src1,
                                   src2Ready: src2Ready, src2: src2,
                                   dest: dest,
                                   alu1: isShift(currInst) || isBranch(currInst)};

    rule decodeALUSpace(decodeState == Decode && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                        isALU(currInst) && robFreeCount != 0 && freeListFreeCount != 0 && intQFreeCount != 0);
        decodeNum         <= decodeNum + 1;
        robFreeCount      <= robFreeCount - 1;
        freeListFreeCount <= freeListFreeCount - 1;
        intQFreeCount     <= intQFreeCount - 1;

        pRegFile.upd(dest, True);
        rob.writeTail(newROBEntry);
        issuePort[decodeNum].send(tagged Valid newIssueEntry);
    endrule

    rule decodeALUNoSpace(decodeState == Decode && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                          isALU(currInst) && !(robFreeCount != 0 && freeListFreeCount != 0 && instQFreeCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeBranch(decodeState == Decode && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                      isBranch(currInst) && robFreeCount != 0 && instQFreeCount != 0);
        decodeNum      <= decodeNum + 1;
        robFreeCount   <= robFreeCount - 1;
        instQFreeCount <= instQFreeCount - 1;

        predictedPC  <= branchPredAddr;
        newROBEntry.isBranch = True;
        rob.writeTail(newROBEntry);
        issuePort[decodeNum].send(tagged Valid newIssueEntry);
        if(isValid(branchPredAddr))
            realDecodeDone <= True;
    endrule

    rule decodeBranchNoSpace(decodeState == Decode && !killInstBuffer && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                             isBranch(currInst) && !(robFreeCount != 0 && instQFreeCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeJump(decodeState == Decode && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                    isJump(currInst) && robFreeCount != 0);
        decodeNum    <= decodeNum + 1;
        robFreeCount <= robFreeCount - 1;

        predictedPC  <= tagged Valid getJumpAddr(currInst);
        newROBEntry.done = True;
        rob.writeTail(newROBEntry);
        issuePort[decodeNum].send(tagged Invalid);
        realDecodeDone <= True;
    endrule

    rule decodeJumpNoSpace(decodeState == Decode && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                           isJump(currInst) && !(robFreeCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeJAL(decodeState == Decode && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                   isJAL(currInst) && robFreeCount != 0 && freeListFreeCount != 0);
        decodeNum         <= decodeNum + 1;
        robFreeCount      <= robFreeCount - 1;
        freeListFreeCount <= freeListFreeCount - 1;

        pRegFile.upd(dest, True);
        newROBEntry.done = True;
        rob.writeTail(newROBEntry);
        issuePort[decodeNum].send(tagged Invalid);
        targetBuffer.enq(getJALAddr(currInst));
        realDecodeDone <= True;
    endrule

    rule decodeJALNoSpace(decodeState == Decode && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                          isJAL(currInst) && !(robFreeCount != 0 && freeListFreeCount != 0));
        decodeState <= DecodeDone;
    endrule

    rule decodeJR(decodeState == Decode && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                  isJR(currInst) && robFreeCount != 0);
        decodeNum         <= decodeNum + 1;
        robFreeCount      <= robFreeCount - 1;

        newROBEntry.done = True;
        newROBEntry.isJR = True;
        rob.writeTail(newROBEntry);
        issuePort[decodeNum].send(tagged Invalid);
        targetBuffer.deq();
        realDecodeDone <= True;
    endrule

    rule decodeJRNoSpace(decodeState == Decode && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                  isJR(currInst) && !(robFreeCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeJALR(decodeState == Decode && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                    isJR(currInst) && robFreeCount != 0 && freeListFreeCount != 0);
        decodeNum         <= decodeNum + 1;
        robFreeCount      <= robFreeCount - 1;
        freeListFreeCount <= freeListFreeCount - 1;

        pRegFile.upd(dest, True);
        newROBEntry.done = True;
        newROBEntry.isJR = True;
        rob.writeTail(newROBEntry);
        issuePort[decodeNum].send(tagged Invalid);
        targetBuffer.deq();
        targetBuffer.enq(getJALRAddr(currInst));
        realDecodeDone <= True;
    endrule

    rule decodeJALRNoSpace(decodeState == Decode && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                           isJR(currInst) && !(robFreeCount != 0 && freeListFreeCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeLoad(decodeState == Decode && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                    isLoad(currInst) && robFreeCount != 0 && freeListFreeCount != 0 && addrQFreeCount != 0);
        decodeNum         <= decodeNum + 1;
        robFreeCount      <= robFreeCount - 1;
        freeListFreeCount <= freeListFreeCount - 1;
        addrQFreeCount    <= addrQFreeCount - 1;

        pRegFile.upd(dest, True);
        rob.writeTail(newROBEntry);
        newIssueEntry.issueType = LoadStore;
        issuePort[decodeNum].send(tagged Valid newIssueEntry);
    endrule

    rule decodeLoadNoSpace(decodeState == Decode && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                           isLoad(currInst) && !(robFreeCount != 0 && freeListFreeCount != 0 && addrQFreeCount != 0));
        realDecodeDone <= True;
    endrule

    rule decodeStore(decodeState == Decode && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                     isStore(currInst) && robFreeCount != 0 && && addrQFreeCount != 0);
        decodeNum         <= decodeNum + 1;
        robFreeCount      <= robFreeCount - 1;
        addrQFreeCount    <= addrQFreeCount - 1;

        rob.writeTail(newROBEntry);
        newIssueEntry.issueType = LoadStore;
        issuePort[decodeNum].send(tagged Valid newIssueEntry);
    endrule

    rule decodeStoreNoSpace(decodeState == Decode && !killInstBuffer && !realDecodeDone && decodeNum != fromInteger(valueOf(FetchWidth)) &&
                            isStore(currInst) && !(robFreeCount != 0 && && addrQFreeCount != 0));
        realDecodeDone <= True;
    endrule

    rule fillIssueQueues(decodeState == Decode && !killInstBuffer && realDecodeDone && decodeNum != fromInteger(valueOf(FetchInst)));
        decodeNum <= decodeNum + 1;
        issuePort[decodeNum].send(tagged Invalid);
    endrule

    rule decodeDone(decodeState == Decode && !killInstBuffer && decodeNum == fromInteger(valueOf(FetchWidth)));
        decodeState <= DecodeDone;
    endrule

    rule decodeKillInstBuffer(decodeState == Decode && killInstBuffer && tokenAddrBuffer.notEmpty());
        tokenAddrBuffer.deq();
        instBuffer.deq();
        decodeBuffer.deq();

        killDecodeStage(tpl1(tokenAddrBuffer.first()));
    endrule

    rule decodeKillInstBufferDone(decodeState == Decode && killInstBuffer && !tokenAddrBuffer.notEmpty());
        killInstBuffer <= nextKillInstBuffer;
        decodeState    <= DecodeDone;
    endrule
endmodule
