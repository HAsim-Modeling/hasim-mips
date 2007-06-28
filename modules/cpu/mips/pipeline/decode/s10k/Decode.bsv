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
    Connection_Receive#(Tuple2#(Token, void))                        fpMemoryResp <- mkConnection_Receive("fp_mem_resp");

    Connection_Send#(Token)                                             fpTokKill <- mkConnection_Send("fp_tok_kill");
    Connection_Send#(Token)                                           fpFetchKill <- mkConnection_Send("fp_fet_kill");
    Connection_Send#(Token)                                          fpDecodeKill <- mkConnection_Send("fp_dec_kill");
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

    Vector#(FetchWidth, Port_Send#(IssuePort))                          issuePort <- genWithM(sendFunctionM("decodeToIssue"));

    Vector#(NumFuncUnits, Port_Receive#(ExecResult))               execResultPort <- genWithM(receiveFunctionM("execToDecodeResult"));
    Port_Receive#(KillData)                                            killDecode <- mkPort_Receive("execToDecodeKill", 1);

    Vector#(CommitWidth, Port_Send#(Token))                       commitTokenPort <- genWithM(sendFunctionM("decodeToCommit"));

    FIFOF#(InstInfo)                                                   instBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(FetchWidth)));

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
    Reg#(Bit#(TLog#(TAdd#(TMul#(FetchWidth,2),1))))               instBufferCount <- mkReg(0);
    Reg#(FuncUnitPos)                                              robUpdateCount <- mkReg(?);
    Reg#(CommitCount)                                                 commitCount <- mkReg(?);
    Reg#(Maybe#(Addr))                                                predictedPC <- mkReg(tagged Invalid);
    Reg#(Maybe#(Addr))                                               mispredictPC <- mkReg(tagged Invalid);

    Rob                                                                       rob <- mkRob();
    BranchStack                                                       branchStack <- mkBranchStack();
    BranchPred                                                         branchPred <- mkBranchPred();
    FIFOF#(Addr)                                                     targetBuffer <- mkTargetBuffer(pcStart);

    Reg#(Bool)                                                    modelCycleBegin <- mkReg(True);

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

    rule synchronize(fetchState == FetchDone && robUpdateState == RobUpdateDone && decodeState == DecodeDone && commitState == CommitDone);
        modelCounter           <= modelCounter + 1;

        FetchCount sendSize = (instBufferCount < fromInteger(valueOf(FetchWidth)))? truncate(fromInteger(valueOf(FetchWidth)) - instBufferCount): 0;

        Maybe#(KillData) killTokenGet  <- killDecode.receive();
        case (killTokenGet) matches
            tagged Valid .killData:
            begin
                fpRewindToToken.send(killData.token);
                rob.updateTail(killData.robTag);
                killCount <= 2;
                mispredictPC <= tagged Valid killData.mispredictPC;
                branchStack.resolveWrong(killData.branchIndex);
                freeListFreeCount <= killData.freeCount;
            end
            tagged Invalid:
                mispredictPC <= tagged Invalid;
        endcase

        if(!modelCycleBegin)
        begin
            decodeNumPort.send(tagged Valid sendSize);
            predictedTakenPort.send(predictedPC);
            mispredictPort.send(mispredictPC);
        end

        modelCycleBegin <= False;

        let intQFreeCountLocal <- intQCountPort.receive();
        let memQFreeCountLocal <- memQCountPort.receive();
        intQFreeCount          <= fromMaybe(fromInteger(valueOf(IntQCount)), intQFreeCountLocal);
        memQFreeCount          <= fromMaybe(fromInteger(valueOf(MemQCount)), memQFreeCountLocal);

        $display("Decode synchronize: instBufferCount %0d, sendSize: %0d, decodeCount: %0d @ Model: %0d", instBufferCount, sendSize, decodeCount, modelCounter);
        $display("intQFreeCount", fromMaybe(fromInteger(valueOf(IntQCount)), intQFreeCountLocal));
        $display("memQFreeCount", fromMaybe(fromInteger(valueOf(MemQCount)), memQFreeCountLocal));
        $display("freeListFreeCount: %0d", freeListFreeCount);

        fetchState             <= Fetch;

        fetchCount             <= 0;

        robUpdateState         <= RobUpdate;
        robUpdateCount         <= 0;

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
            end
        endcase
        fetchCount     <= fetchCount + 1;
        if(fetchCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
            fetchState <= FetchDone;
    endrule

    rule commit(commitState == Commit);
        Maybe#(RobEntry) robEntryMaybe <- rob.readHead();
        commitCount                    <= commitCount + 1;
        if(robEntryMaybe matches tagged Valid .robEntry &&& robEntry.done)
        begin
            commitTokenPort[commitCount].send(tagged Valid robEntry.token);
            $display("Commit: Token: %0d @ Model: %0d", robEntry.token.index, modelCounter-1);
            if(robEntry.issueType == Branch)
                branchPred.upd(robEntry.token, robEntry.addr, robEntry.pred, robEntry.taken);
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
        Maybe#(ExecResult) execResultMaybe  <- execResultPort[robUpdateCount].receive();

        case (execResultMaybe) matches
            tagged Valid .execResult:
            begin
                Tuple2#(Token, void) memAck    <- fpMemoryResp.receive();
                ExecResult exec                 = execResult;
                Maybe#(RobEntry) robEntryMaybe <- rob.read(exec.robTag);
                if(robEntryMaybe matches tagged Valid .robEntry &&& robEntry.token == exec.token)
                begin
                    $display("Done: Token: %0d, index: %0d @ Model: %0d %0d", exec.token.index, exec.robTag, modelCounter-1, rob.getTail());
                    RobEntry newRobEntry  = robEntry;
                    newRobEntry.done = True;
                    newRobEntry.taken = exec.taken;
                    newRobEntry.status = exec.status;
                    newRobEntry.finished = exec.finished;
                    rob.write(exec.robTag, newRobEntry);

                    if(exec.issueType == Branch || exec.issueType == JR || exec.issueType == JALR)
                        branchStack.resolveRight(exec.branchIndex);
                    if(exec.pRName != 0)
                        freeListFreeCount     <= freeListFreeCount + 1;
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
        for(Integer i = 0; i < valueOf(FetchWidth); i=i+1)
        begin
            if(fromInteger(i) >= decodeCount)
                issuePort[i].send(tagged Invalid);
        end
        decodeState <= DecodeDone;
    endaction
    endfunction

    rule decodeInst(decodeState == Decoding && killCount == 0 && instBuffer.notEmpty());
        InstInfo currInstBuffer     = instBuffer.first();
        Addr currAddr               = currInstBuffer.addr;
        Token currToken             = currInstBuffer.token;
        PackedInst currInst         = currInstBuffer.inst;

        branchPred.getPredReq(currToken, currAddr);
        Bool pred                  <- branchPred.getPredResp();
        Maybe#(Addr) branchPredAddr = pred? tagged Valid (currAddr + 4 + (signExtend(currInst[15:0]) << 2)) : tagged Invalid;

        Bool doCommonDecode = False;

        function Action commonDecode(IssueType issueType, Bool intQ, Bool memQ, Bool regFileUpd,
                                     Bool branch, Bool jr, Maybe#(Addr) predPC, Bool tbEnq, Bool tbDeq,
                                     Bool kill);
        action
            RobEntry res = RobEntry{token: currToken, issueType: issueType, addr: currAddr, done: False, finished: False, status: False,
                                    pred: pred, taken: False};

            IssuePort issue = IssuePort{issueType: issueType, token: currToken, robTag: rob.getTail(), freeCount: freeListFreeCount, branchIndex: 0, pred: pred, predAddr: validValue(predPC)};

            let doDecode = rob.notFull() &&
                          !(regFileUpd && freeListFreeCount == 0) &&
                          !(intQ && intQFreeCount == 0) &&
                          !(memQ && memQFreeCount == 0) &&
                          !((branch || jr) && !branchStack.notFull()) &&
                          !(tbEnq && !targetBuffer.notFull()) &&
                          !(tbDeq && !targetBuffer.notEmpty());

            $display("branchStack: doDecode:%0d branch:%0d branchNotFull:%0d %0d %0d", doDecode, branch, branchStack.notFull(), targetBuffer.notFull(), targetBuffer.notEmpty());

            $display("decode: try: %0d", currToken.index);

            if(doDecode)
            begin

                $display("dodecode: %0d", currToken.index);

                if(regFileUpd) freeListFreeCount <= freeListFreeCount - 1;
                if(intQ)           intQFreeCount <= intQFreeCount - 1;
                if(memQ)           memQFreeCount <= memQFreeCount - 1;
                if(tbEnq)          targetBuffer.enq(currAddr + 4);
                if(tbDeq)          targetBuffer.deq();

                if(branch || jr)
                begin
                    predictedPC       <= predPC;
                    issue.branchIndex <- branchStack.add();
                end

                decodeCount     <= decodeCount + 1;
                instBufferCount <= instBufferCount - 1;

                rob.writeTail(res);

                $display("Decode: Token: %0d %0d", currToken.index, issue.token.index);
                fpDecodeReq.send(tuple2(currToken, ?));
                issuePort[decodeCount].send(tagged Valid issue);

                instBuffer.deq();
                if(kill)
                    killCount <= 2;
                else if(decodeCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
                    decodeState <= DecodeDone;
            end
            else
                finishDecode();
        endaction
        endfunction

        IssueType intIssue = (p_isShift(currInst))? Shift: Normal;

        //               IssueType, intQ,  memQ,  regF,  branch, jr,    predPC,                                      tbEnq, tbDeq, kill
        if(p_isALU(currInst))
            commonDecode(intIssue,  True,  False, True,  False,  False, tagged Invalid,                              False, False, False);
        else if(p_isLoad(currInst))
            commonDecode(Load,      False, True,  True,  False,  False, tagged Invalid,                              False, False, False);
        else if(p_isStore(currInst))
            commonDecode(Store,     False, True,  False, False,  False, tagged Invalid,                              False, False, False);
        else if(p_isBranch(currInst))
            commonDecode(Branch,    True,  False, False, True,   False, branchPredAddr,                              False, False, isValid(branchPredAddr));
        else if(p_isJ(currInst))
            commonDecode(J,         False, False, False, False,  False, tagged Valid p_getJAddr(currInst, currAddr),   False, False, True);
        else if(p_isJAL(currInst))
            commonDecode(JAL,       False, False, True,  False,  False, tagged Valid p_getJALAddr(currInst, currAddr), True,  False, True);
        else if(p_isJR(currInst))
            commonDecode(JR,        True,  False, False, False,  True,  tagged Valid targetBuffer.first(),           False, True,  True);
        else if(p_isJALR(currInst))
            commonDecode(JALR,      True,  False, True,  False,  True,  tagged Valid targetBuffer.first(),           True,  True,  True);
        else
        begin
            decodeCount     <= decodeCount + 1;
            instBufferCount <= instBufferCount - 1;

            issuePort[decodeCount].send(tagged Invalid);

            instBuffer.deq();
            if(decodeCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
                decodeState <= DecodeDone;
        end
    endrule

    rule prematureFinishDecode(decodeState == Decoding && killCount == 0 && !instBuffer.notEmpty() && fetchState == FetchDone);
        finishDecode();
    endrule

    (* descending_urgency = "fetch, prematureFinishDecode, decodeKillInstBuffer" *)
    rule decodeKillInstBuffer(decodeState == Decoding && killCount != 0);
        $display("killCount:%0d", killCount);
        if(!instBuffer.notEmpty())
        begin
            $display("instBuffer.empty");
            if(fetchState == FetchDone)
            begin
                $display("finishing Decode kill @ Model: %0d", modelCounter-1);
                killCount   <= killCount - 1;
                finishDecode();
                decodeCount <= fromInteger(valueOf(FetchWidth));
            end
        end
        else
        begin
            instBuffer.deq();
            instBufferCount <= instBufferCount - 1;
            fpTokKill.send((instBuffer.first()).token);
            fpDecodeKill.send((instBuffer.first()).token);
        end
    endrule
endmodule
