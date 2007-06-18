import hasim_common::*;
import hasim_isa::*;

import FIFOF::*;
import FIFO::*;
import Vector::*;
import RegFile::*;

import hasim_cpu_parameters::*;
import hasim_cpu_types::*;
import hasim_rob::*;
import hasim_branch_stack::*;
import hasim_branch_pred::*;
import hasim_branch_target_buffer::*;

typedef enum {Commit, CommitDone}       CommitState    deriving (Bits, Eq);
typedef enum {RobUpdate, RobUpdateDone} RobUpdateState deriving (Bits, Eq);
typedef enum {Read, Write}              RobOpState     deriving (Bits, Eq);
typedef enum {Fetch, FetchDone}         FetchState     deriving (Bits, Eq);
typedef enum {Decoding, DecodeDone}     DecodeState    deriving (Bits, Eq);

module [HASim_Module] mkPipe_Decode();
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

    Vector#(FetchWidth, Port_Receive#(Addr))                             addrPort <- genWithM(receiveFunctionM("fetchToDecode"));

    Port_Send#(FetchCount)                                          decodeNumPort <- mkPort_Send("decodeToFetchDecodeNum");
    Port_Send#(Addr)                                           predictedTakenPort <- mkPort_Send("decodeToFetchPredictedTaken");
    Port_Send#(Addr)                                               mispredictPort <- mkPort_Send("decodeToFetchMispredict");

    Port_Receive#(IntQCountType)                                    intQCountPort <- mkPort_Receive("issueToDecodeIntQ", 1);
    Port_Receive#(MemQCountType)                                    memQCountPort <- mkPort_Receive("issueToDecodeMemQ", 1);

    Vector#(FetchWidth, Port_Send#(IssueEntry))                         issuePort <- genWithM(sendFunctionM("decodeToIssue"));

    Vector#(NumFuncUnits, Port_Receive#(Tuple2#(ExecEntry, InstResult)))
                                                                   execResultPort <- genWithM(receiveFunctionM("execToDecodeResult"));

    Vector#(CommitWidth, Port_Send#(Token))                       commitTokenPort <- genWithM(sendFunctionM("decodeToCommit"));

    FIFOF#(InstInfo)                                                   instBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(FetchWidth)));
    FIFOF#(DepInfo)                                                  decodeBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(FetchWidth)));

    Reg#(CommitState)                                                 commitState <- mkReg(CommitDone);
    Reg#(RobUpdateState)                                           robUpdateState <- mkReg(RobUpdateDone);
    Reg#(FetchState)                                                   fetchState <- mkReg(FetchDone);
    Reg#(DecodeState)                                                 decodeState <- mkReg(DecodeDone);
    Reg#(Bit#(TLog#(TAdd#(KillCount,1))))                               killCount <- mkReg(0);

    Reg#(Bit#(TLog#(TAdd#(FreeListCount,1))))                   freeListFreeCount <- mkReg(fromInteger(valueOf(FreeListCount)));
    Reg#(Bit#(TLog#(TAdd#(IntQCount,1))))                           intQFreeCount <- mkReg(?);
    Reg#(Bit#(TLog#(TAdd#(MemQCount,1))))                           memQFreeCount <- mkReg(?);

    Reg#(FetchCount)                                                   fetchCount <- mkReg(?);
    Reg#(FetchCount)                                                  decodeCount <- mkReg(fromInteger(valueOf(FetchWidth)));
    Reg#(FetchCount)                                              instBufferCount <- mkReg(0);
    Reg#(FuncUnitPos)                                              robUpdateCount <- mkReg(?);
    Reg#(CommitCount)                                                 commitCount <- mkReg(?);
    Reg#(Maybe#(Addr))                                                predictedPC <- mkReg(tagged Invalid);
    Reg#(Maybe#(Addr))                                               mispredictPC <- mkReg(tagged Invalid);

    Rob                                                                       rob <- mkRob();
    Reg#(Vector#(PRNum, Bool))                                           pRegFile <- mkReg(replicate(False));
    BranchStack                                                       branchStack <- mkBranchStack();
    BranchPred                                                         branchPred <- mkBranchPred();
    FIFOF#(Addr)                                                     targetBuffer <- mkTargetBuffer(pcStart);

    Reg#(Bool)                                                              birth <- mkReg(True);

    Reg#(ClockCounter)                                               clockCounter <- mkReg(0);
    Reg#(ClockCounter)                                               modelCounter <- mkReg(0);

    rule clockCount(True);
        clockCounter <= clockCounter + 1;
    endrule

    rule birthOfModel(birth);
        Command req <- finishServer.getReq();
        birth       <= False;
    endrule

    rule getDecodeResp(True);
        match {.token, .dep} <- fpDecodeResp.receive();
        decodeBuffer.enq(dep);
    endrule

    rule synchronize(fetchState == FetchDone && robUpdateState == RobUpdateDone && decodeState == DecodeDone && commitState == CommitDone);
        modelCounter           <= modelCounter + 1;

        FetchCount sendSize = (instBufferCount < fromInteger(valueOf(FetchWidth)))? fromInteger(valueOf(FetchWidth)) - instBufferCount: 0;

        decodeNumPort.send(tagged Valid sendSize);
        predictedTakenPort.send(predictedPC);
        mispredictPort.send(mispredictPC);

        $display("Decode synchronize: instBufferCount %0d, sendSize: %0d, decodeCount: %0d @ Model: %0d", instBufferCount, sendSize, decodeCount, modelCounter);

        let intQFreeCountLocal <- intQCountPort.receive();
        let memQFreeCountLocal <- memQCountPort.receive();
        intQFreeCount          <= fromMaybe(fromInteger(valueOf(IntQCount)), intQFreeCountLocal);
        memQFreeCount          <= fromMaybe(fromInteger(valueOf(MemQCount)), memQFreeCountLocal);

        fetchState             <= Fetch;
        fetchCount             <= 0;

        robUpdateState         <= RobUpdate;
        robUpdateCount         <= 0;

        mispredictPC           <= tagged Invalid;
        predictedPC            <= tagged Invalid;
        decodeCount            <= 0;
    endrule

    rule fetch(fetchState == Fetch);
        Maybe#(Addr) addrMaybe  <- addrPort[fetchCount].receive();
        $display("Decode: FetchCount: %0d", fetchCount);
        case (addrMaybe) matches
            tagged Valid .addr:
            begin
                match {.token, .inst} <- fpFetchResp.receive();
                instBufferCount       <= instBufferCount + 1;
                instBuffer.enq(InstInfo{token: token, addr: addr, inst: inst});
                fpDecodeReq.send(tuple2(token, ?));
            end
        endcase
        fetchCount     <= fetchCount + 1;
        if(fetchCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
            fetchState <= FetchDone;
    endrule

    rule commit(commitState == Commit);
        Maybe#(RobEntry) robEntryMaybe  = rob.readHead();
        commitCount            <= commitCount + 1;
        if(robEntryMaybe matches tagged Valid .robEntry &&& robEntry.done)
        begin
            commitTokenPort[commitCount].send(tagged Valid robEntry.token);
            $display("Commit: Token: %0d @ Model: %0d", robEntry.token.index, modelCounter-1);
            if(robEntry.isBranch)
                branchPred.upd(robEntry.token, robEntry.addr, robEntry.prediction, robEntry.taken);
            if(robEntry.finished)
                finishServer.makeResp(tagged RESP_DoneRunning robEntry.status);
            if(commitCount == fromInteger(valueOf(TSub#(CommitWidth,1))))
                commitState <= CommitDone;
            rob.incrementHead();
        end
        else
        begin
            for(Integer i = 0; i < valueOf(CommitWidth); i=i+1)
            begin
                if(fromInteger(i) >= commitCount)
                    commitTokenPort[i].send(tagged Invalid);
            end
            commitState <= CommitDone;
        end
    endrule

    rule update(robUpdateState == RobUpdate);
        Maybe#(Tuple2#(ExecEntry, InstResult)) execResultMaybe  <- execResultPort[robUpdateCount].receive();

        case (execResultMaybe) matches
            tagged Valid .execResult:
            begin
                Tuple2#(Token, void) memAck   <- fpMemoryResp.receive();
                match {.exec, .res}            = execResult;
                Maybe#(RobEntry) robEntryMaybe = rob.read(exec.robTag);
                if(robEntryMaybe matches tagged Valid .robEntry &&& robEntry.token == exec.token)
                begin
                    $display("Done: Token: %0d, index: %0d @ Model: %0d", exec.token.index, exec.robTag, modelCounter-1);
                    RobEntry newRobEntry  = robEntry;
                    newRobEntry.done = True;
                    newRobEntry.taken = case (res) matches
                                            tagged RBranchTaken .addr: return True;
                                            default: return False;
                                        endcase;
                    newRobEntry.status = case (res) matches
                                             tagged RTerminate .execResult: return execResult;
                                             default: return False;
                                         endcase;
                    newRobEntry.finished = case (res) matches
                                               tagged RTerminate .execResult: return True;
                                               default: return False;
                                           endcase;
                    Addr newAddr = case (res) matches
                                       tagged RBranchTaken .addr: return addr;
                                   endcase;
                    pRegFile[exec.pRName] <= False;
                    freeListFreeCount     <= freeListFreeCount + 1;
                    rob.write(exec.robTag, newRobEntry);

                    if(robEntry.isBranch && robEntry.prediction != newRobEntry.taken || robEntry.isJR && robEntry.predAddr != newAddr)
                    begin
                        branchStack.resolveWrong(robEntry.branchIndex);
                        fpRewindToToken.send(exec.token);
                        rob.updateTail(exec.robTag);
                        killCount          <= fromInteger(2);
                        mispredictPC       <= tagged Valid newAddr;
                        decodeCount        <= fromInteger(valueOf(FetchWidth));
                        $display("Branch mispredicted @ Model: %0d. Correct address: %x, rob index: %0d", modelCounter-1, newAddr, exec.robTag);
                    end
                    else if(robEntry.isBranch || robEntry.isJR)
                        branchStack.resolveRight(robEntry.branchIndex);
                end
                else
                begin
                    fpTokKill.send(exec.token);
                    fpLocalCommitKill.send(exec.token);
                    fpMemStateKill.send(exec.token);
                    $display("ReceiveWrong: Token: %0d @ Model: %0d, pos: %0d, tail: %0d", exec.token.index, modelCounter-1, exec.robTag, rob.getTail());
                end
            end
        endcase

        robUpdateCount <= robUpdateCount + 1;
        if(robUpdateCount == fromInteger(valueOf(TSub#(NumFuncUnits,1))))
        begin
            robUpdateState <= RobUpdateDone;
            decodeState    <= Decoding;
            commitState    <= Commit;
            commitCount    <= 0;
        end
    endrule

    function Action finishDecode();
    action
        decodeState    <= DecodeDone;
        for(FetchCount i = 0; i != fromInteger(valueOf(FetchWidth)); i=i+1)
        begin
            if(i > decodeCount)
                issuePort[i].send(tagged Invalid);
        end
    endaction
    endfunction

    rule decodeInst(decodeState == Decoding && killCount == 0 && instBuffer.notEmpty());
        InstInfo currInstBuffer     = instBuffer.first();
        DepInfo currDepInfo         = decodeBuffer.first();
        Addr currAddr               = currInstBuffer.addr;
        Token currToken             = currInstBuffer.token;
        PackedInst currInst         = currInstBuffer.inst;

        Bool pred                  <- branchPred.getPred(currToken, currAddr);
        Maybe#(Addr) branchPredAddr = pred? tagged Valid (currAddr + 4 + (signExtend(currInst[15:0]) << 2)) : tagged Invalid;

        Bool src1Valid              = isValid(currDepInfo.dep_src1);
        PRName src1                 = tpl_2(validValue(currDepInfo.dep_src1));
        Bool src2Valid              = isValid(currDepInfo.dep_src2);
        PRName src2                 = tpl_2(validValue(currDepInfo.dep_src2));
        PRName dest                 = tpl_2(validValue(currDepInfo.dep_dest));

        Bool src1Ready              = src1Valid? !pRegFile[src1]: True;
        Bool src2Ready              = src2Valid? !pRegFile[src2]: True;

        Bool doCommonDecode = False;

        function Action commonDecode(IssueType issueType, Bool intQ, Bool memQ, Bool regFileUpd,
                                     Bool branch, Bool jr, Maybe#(Addr) predPC, Bool tbEnq, Bool tbDeq,
                                     Bool kill);
        action
            RobEntry res = RobEntry{token: currToken, addr: currAddr, done: False, finished: False, status: False,
                                    branchIndex: 0, isBranch: branch, prediction: isValid(branchPredAddr), taken: False,
                                    isJR: jr, predAddr: targetBuffer.first()};
            IssueEntry issue = IssueEntry{issueType: issueType,
                                          token: currToken, robTag: rob.getTail(),
                                          src1Ready: src1Ready, src1: src1,
                                          src2Ready: src2Ready, src2: src2,
                                          dest: dest};
            let doDecode = rob.notFull() &&
                          !(regFileUpd && freeListFreeCount == 0) &&
                          !(intQ && intQFreeCount == 0) &&
                          !(memQ && memQFreeCount == 0) &&
                          !((branch || jr) && !branchStack.notFull()) &&
                          !(tbEnq && !targetBuffer.notFull()) &&
                          !(tbDeq && !targetBuffer.notEmpty());
            if(doDecode)
            begin
                if(regFileUpd) freeListFreeCount <= freeListFreeCount - 1;
                if(intQ)           intQFreeCount <= intQFreeCount - 1;
                if(memQ)           memQFreeCount <= memQFreeCount - 1;
                if(tbEnq)          targetBuffer.enq(currAddr + 4);
                if(tbDeq)          targetBuffer.deq();
                if(regFileUpd)    pRegFile[dest] <= True;

                if(branch || jr)
                begin
                    predictedPC     <= predPC;
                    res.branchIndex <- branchStack.add();
                end

                decodeCount     <= decodeCount + 1;
                instBufferCount <= instBufferCount - 1;

                rob.writeTail(res);
                issuePort[decodeCount].send(tagged Valid issue);

                instBuffer.deq();
                decodeBuffer.deq();
                $display("Decode : Token: %0d, addr: %x, type: %0d @ Model: %0d, res.isBranch: %0d, gettail: %0d", currToken.index, currAddr, issue.issueType, modelCounter-1, res.isBranch, rob.getTail());

                if(kill)
                    killCount      <= 2;

                if(kill || decodeCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
                    finishDecode();
            end
            else
            begin
                issuePort[decodeCount].send(tagged Invalid);
                finishDecode();
            end
        endaction
        endfunction

        IssueType intIssue = (isShift(currInst))? Shift: Normal;

        //               IssueType, intQ,  memQ,  regF,  branch, jr,    predPC,                                      tbEnq, tbDeq, kill
        if(isALU(currInst))
            commonDecode(intIssue,  True,  False, True,  False,  False, tagged Invalid,                              False, False, False);
        else if(isLoad(currInst))
            commonDecode(Load,      False, True,  True,  False,  False, tagged Invalid,                              False, False, False);
        else if(isStore(currInst))
            commonDecode(Store,     False, True,  False, False,  False, tagged Invalid,                              False, False, False);
        else if(isBranch(currInst))
            commonDecode(Branch,    True,  False, False, True,   False, branchPredAddr,                              False, False, isValid(branchPredAddr));
        else if(isJ(currInst))
            commonDecode(J,         False, False, False, False,  False, tagged Valid getJAddr(currInst, currAddr),   False, False, True);
        else if(isJAL(currInst))
            commonDecode(JAL,       False, False, True,  False,  False, tagged Valid getJALAddr(currInst, currAddr), True,  False, True);
        else if(isJR(currInst))
            commonDecode(JR,        True,  False, False, False,  True,  tagged Valid targetBuffer.first(),           False, True,  True);
        else if(isJALR(currInst))
            commonDecode(JALR,      True,  False, True,  False,  True,  tagged Valid targetBuffer.first(),           True,  True,  True);
        else
        begin
            decodeCount     <= decodeCount + 1;
            instBufferCount <= instBufferCount - 1;

            issuePort[decodeCount].send(tagged Invalid);

            instBuffer.deq();
            decodeBuffer.deq();
            if(decodeCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
                decodeState <= DecodeDone;
        end
    endrule

    rule prematureFinishDecode(decodeState == Decoding && killCount == 0 && !instBuffer.notEmpty() && fetchState == FetchDone);
        issuePort[decodeCount].send(tagged Invalid);
        finishDecode();
    endrule

    rule decodeKillInstBuffer(decodeState == Decoding && killCount != 0);
        if(!instBuffer.notEmpty())
        begin
            if(fetchState == FetchDone)
            begin
                killCount   <= killCount - 1;
                decodeState <= DecodeDone;
            end
        end
        else
        begin
            instBuffer.deq();
            instBufferCount <= instBufferCount - 1;
            decodeBuffer.deq();
            fpTokKill.send((instBuffer.first()).token);
            fpExeKill.send((instBuffer.first()).token);
        end
    endrule
endmodule
