import hasim_common::*;
import hasim_isa::*;
import hasim_local_controller::*;

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
    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, integerToString(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, integerToString(i)), 1);
    
    //Connections
    
    Connection_Receive#(Tuple2#(Token, PackedInst))    fpFetchResp <- mkConnection_Receive("fp_fet_resp");
    Connection_Send#(Tuple2#(Token, void))             fpDecodeReq <- mkConnection_Send("fp_dec_req");
    Connection_Receive#(Tuple2#(Token, void))         fpMemoryResp <- mkConnection_Receive("fp_mem_resp");

    Connection_Send#(Token)                              fpTokKill <- mkConnection_Send("fp_tok_kill");
    Connection_Send#(Token)                            fpFetchKill <- mkConnection_Send("fp_fet_kill");
    Connection_Send#(Token)                           fpDecodeKill <- mkConnection_Send("fp_dec_kill");
    Connection_Send#(Token)                              fpMemKill <- mkConnection_Send("fp_mem_kill");
    Connection_Send#(Token)                         fpMemStateKill <- mkConnection_Send("fp_memstate_kill");
    Connection_Send#(Token)                      fpLocalCommitKill <- mkConnection_Send("fp_lco_kill");
    Connection_Send#(Token)                     fpGlobalCommitKill <- mkConnection_Send("fp_gco_kill");
    Connection_Send#(Token)                        fpRewindToToken <- mkConnection_Send("fp_rewindToToken");
    
    //Ports
    
    Vector#(FetchWidth, Port_Receive#(Addr))              addrPort <- genWithM(receiveFunctionM("fetchToDecode"));

    Port_Send#(FetchCount)                          fetchCountPort <- mkPort_Send("decodeToFetchDecodeNum");
    Port_Send#(Addr)                            predictedTakenPort <- mkPort_Send("decodeToFetchPredictedTaken");
    Port_Send#(Addr)                                mispredictPort <- mkPort_Send("decodeToFetchMispredict");

    Port_Receive#(IntQCount)                         intQCountPort <- mkPort_Receive("issueToDecodeIntQ", 1);
    Port_Receive#(MemQCount)                         memQCountPort <- mkPort_Receive("issueToDecodeMemQ", 1);
    Port_Receive#(FreeListCount)                   freeListAddPort <- mkPort_Receive("issueToDecodeFreeListAdd", 1);

    Vector#(FetchWidth, Port_Send#(IssuePort))           issuePort <- genWithM(sendFunctionM("decodeToIssue"));

    Vector#(FuncUnitNum, Port_Receive#(ExecResult)) execResultPort <- genWithM(receiveFunctionM("execToDecodeResult"));
    Port_Receive#(KillData)                         killDecodePort <- mkPort_Receive("execToDecodeKill", 1);

    Vector#(CommitWidth, Port_Send#(Token))        commitTokenPort <- genWithM(sendFunctionM("decodeToCommit"));

    //Local Controller
    
    //Someday these should be real
    Vector#(0, Port_Control) inports = newVector();
    Vector#(0, Port_Control) outports = newVector();
    
    LocalController local_ctrl <- mkLocalController(inports, outports);

    FIFOF#(InstInfo)                                    instBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(FetchWidth)));
    FIFOF#(Bool)                                      branchBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(FetchWidth)));

    Reg#(CommitState)                                  commitState <- mkReg(CommitDone);
    Reg#(RobUpdateState)                            robUpdateState <- mkReg(RobUpdateDone);
    Reg#(FetchState)                                    fetchState <- mkReg(FetchDone);
    Reg#(DecodeState)                                  decodeState <- mkReg(DecodeDone);
    Reg#(KillCount)                                      killCount <- mkReg(0);

    Reg#(FreeListCount)                              freeListCount <- mkReg(fromInteger(valueOf(FreeListNum)));
    Reg#(IntQCount)                                      intQCount <- mkReg(?);
    Reg#(MemQCount)                                      memQCount <- mkReg(?);

    Reg#(FetchCount)                                    fetchCount <- mkReg(?);
    Reg#(FetchCount)                                   decodeCount <- mkReg(?);
    Reg#(InstCount)                                instBufferCount <- mkReg(0);
    Reg#(FuncUnitCount)                             robUpdateCount <- mkReg(?);
    Reg#(CommitCount)                                  commitCount <- mkReg(?);
    Reg#(Maybe#(Addr))                                 predictedPC <- mkReg(?);
    Reg#(Maybe#(Addr))                                mispredictPC <- mkReg(?);

    Rob                                                        rob <- mkRob();
    BranchStack                                        branchStack <- mkBranchStack();
    BranchPred                                          branchPred <- mkBranchPred();
    FIFO#(Addr)                                       targetBuffer <- mkTargetBuffer(pcStart);

    Reg#(Bool)                                     modelCycleBegin <- mkReg(True);

    Stat                                              clockCounter <- mkStatCounter("FPGA cycles");
    Stat                                              modelCounter <- mkStatCounter("Model cycles");
    Stat                                                   rewinds <- mkStatCounter("Rewinds");
    Stat                                                   decodes <- mkStatCounter("Decodes");
    Stat                                                   fetches <- mkStatCounter("Fetches");
    Stat                                                   commits <- mkStatCounter("Commits");

    rule clockCount(True);
        clockCounter.incr();
    endrule

    rule branchBufferFill(True);
        Bool pred <- branchPred.getPredResp();
        branchBuffer.enq(pred);
    endrule

    rule synchronize(fetchState == FetchDone && robUpdateState == RobUpdateDone && decodeState == DecodeDone && commitState == CommitDone);
        modelCounter.incr();
        FetchCount fetchCountSend = (instBufferCount < fromInteger(valueOf(FetchWidth)))? truncate(fromInteger(valueOf(FetchWidth)) - instBufferCount): 0;

        Maybe#(KillData) killTokenGet  <- killDecodePort.receive();
        case (killTokenGet) matches
            tagged Valid .killData:
            begin
                fpRewindToToken.send(killData.token);
                rob.updateTail(killData.robTag);
                killCount <= 2;
                mispredictPC <= tagged Valid killData.mispredictPC;
                branchStack.resolveWrong(killData.branchIndex);
                rewinds.incr();
            end
            tagged Invalid:
                mispredictPC <= tagged Invalid;
        endcase

        if(!modelCycleBegin)
        begin
            fetchCountPort.send(tagged Valid fetchCountSend);
            predictedTakenPort.send(predictedPC);
            mispredictPort.send(mispredictPC);
        end

        modelCycleBegin <= False;

        let intQCountLocal     <- intQCountPort.receive();
        let memQCountLocal     <- memQCountPort.receive();
        let freeListCountLocal <- freeListAddPort.receive();
        intQCount              <= fromMaybe(fromInteger(valueOf(IntQNum)), intQCountLocal);
        memQCount              <= fromMaybe(fromInteger(valueOf(MemQNum)), memQCountLocal);
        freeListCount          <= freeListCount + fromMaybe(0, freeListCountLocal);

        fetchState             <= Fetch;
        fetchCount             <= 0;

        robUpdateState         <= RobUpdate;
        robUpdateCount         <= 0;

        predictedPC            <= tagged Invalid;
        decodeCount            <= 0;
    endrule

    rule fetch(fetchState == Fetch);
        Maybe#(Addr) addrMaybe  <- addrPort[fetchCount].receive();
        case (addrMaybe) matches
            tagged Valid .addr:
            begin
                match {.token, .inst} <- fpFetchResp.receive();
                instBufferCount       <= instBufferCount + 1;
                instBuffer.enq(InstInfo{token: token, addr: addr, inst: inst});
                branchPred.getPredReq(token, addr);
                fetches.incr();
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
            commits.incr();
            commitTokenPort[commitCount].send(tagged Valid robEntry.token);
            if(robEntry.issueType == Branch)
                branchPred.upd(robEntry.token, robEntry.addr, robEntry.pred, robEntry.taken);
            if(robEntry.finished)
            begin
                local_ctrl.endProgram(robEntry.status);
            end
            if(commitCount == fromInteger(valueOf(TSub#(CommitWidth,1))))
                commitState <= CommitDone;
            rob.incrementHead();
            if(!(robEntry.issueType == Branch || robEntry.issueType == J || robEntry.issueType == JR || robEntry.issueType == Store))
                freeListCount <= freeListCount + 1;
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
            tagged Valid .exec:
            begin
                Tuple2#(Token, void) memAck    <- fpMemoryResp.receive();
                Maybe#(RobEntry) robEntryMaybe <- rob.read(exec.robTag);
                if(robEntryMaybe matches tagged Valid .robEntry &&& robEntry.token == exec.token)
                begin
                    RobEntry newRobEntry  = robEntry;
                    newRobEntry.done = True;
                    newRobEntry.taken = exec.taken;
                    newRobEntry.status = exec.status;
                    newRobEntry.finished = exec.finished;
                    rob.write(exec.robTag, newRobEntry);

                    if(exec.issueType == Branch || exec.issueType == JR || exec.issueType == JALR)
                        branchStack.resolveRight(exec.branchIndex);
                end
                else
                begin
                    fpTokKill.send(exec.token);
                    fpLocalCommitKill.send(exec.token);
                    fpMemStateKill.send(exec.token);
                    if(!(exec.issueType == Branch || exec.issueType == J || exec.issueType == JR || exec.issueType == Store))
                        freeListCount <= freeListCount + 1;
                end
            end
        endcase

        robUpdateCount <= robUpdateCount + 1;
        if(robUpdateCount == fromInteger(valueOf(TSub#(FuncUnitNum,1))))
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

        function Action commonDecode(IssueType issueType,
                                     Bool intQ, Bool memQ,
                                     Bool regFileUpd,
                                     Bool branch, Bool jr,
                                     Maybe#(Addr) predPC,
                                     Bool kill);
        action
            Bool doDecode = rob.notFull() &&
                           !(regFileUpd && freeListCount == 0) &&
                           !(intQ && intQCount == 0) &&
                           !(memQ && memQCount == 0) &&
                           !((branch || jr) && !branchStack.notFull());

            if(doDecode)
            begin
                Bool pred = branchBuffer.first();
                branchBuffer.deq();
                Maybe#(Addr) branchPredAddr = pred? tagged Valid (currAddr + 4 + (signExtend(currInst[15:0]) << 2)) : tagged Invalid;

                RobEntry res = RobEntry{token: currToken,
                                        addr: currAddr,
                                        issueType: issueType,
                                        done: False,
                                        finished: False,
                                        status: False,
                                        pred: pred,
                                        taken: False};

                IssuePort issue = IssuePort{issueType: issueType,
                                            addr: currAddr,
                                            token: currToken,
                                            robTag: rob.getTail(),
                                            branchIndex: 0,
                                            pred: pred,
                                            predAddr: validValue(predPC)};

                if(regFileUpd)
                    freeListCount <= freeListCount - 1;
                if(intQ)           intQCount <= intQCount - 1;
                if(memQ)           memQCount <= memQCount - 1;

                if(branch)
                begin
                    predictedPC       <= branchPredAddr;
                    issue.branchIndex <- branchStack.add();
                end
                else if(isValid(predPC))
                begin
                    predictedPC       <= predPC;
                    if(jr)
                        issue.branchIndex <- branchStack.add();
                end

                decodeCount     <= decodeCount + 1;
                instBufferCount <= instBufferCount - 1;

                rob.writeTail(res);

                fpDecodeReq.send(tuple2(currToken, ?));
                issuePort[decodeCount].send(tagged Valid issue);

                instBuffer.deq();
                if(kill || branch && pred)
                    killCount <= 2;
                else if(decodeCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
                    decodeState <= DecodeDone;

            end
            else
                finishDecode();
        endaction
        endfunction

        IssueType intIssue = (p_isShift(currInst))? Shift: (p_isMtc0(currInst)? Finish: Normal);

        //               IssueType, intQ,  memQ,  regF,  branch, jr,    predPC,                                        kill
        if(p_isALU(currInst))
            commonDecode(intIssue,  True,  False, True,  False,  False, tagged Invalid,                                False);
        else if(p_isLoad(currInst))
            commonDecode(Load,      False, True,  True,  False,  False, tagged Invalid,                                False);
        else if(p_isStore(currInst))
            commonDecode(Store,     False, True,  False, False,  False, tagged Invalid,                                False);
        else if(p_isBranch(currInst))
            commonDecode(Branch,    True,  False, False, True,   False, tagged Invalid,                                False);
        else if(p_isJ(currInst))
            commonDecode(J,         False, False, False, False,  False, tagged Valid p_getJAddr(currInst, currAddr),   True);
        else if(p_isJAL(currInst))
            commonDecode(JAL,       False, False, True,  False,  False, tagged Valid p_getJALAddr(currInst, currAddr), True);
        else if(p_isJR(currInst))
            commonDecode(JR,        True,  False, False, False,  True,  tagged Valid targetBuffer.first(),             True);
        else if(p_isJALR(currInst))
            commonDecode(JALR,      True,  False, True,  False,  True,  tagged Valid targetBuffer.first(),             True);
        else
        begin
            decodeCount     <= decodeCount + 1;
            instBufferCount <= instBufferCount - 1;
            issuePort[decodeCount].send(tagged Invalid);
            instBuffer.deq();
            branchBuffer.deq();
            fpTokKill.send((instBuffer.first()).token);
            fpDecodeKill.send((instBuffer.first()).token);
            if(decodeCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
                decodeState <= DecodeDone;
        end
    endrule

    rule prematureFinishDecode(decodeState == Decoding && killCount == 0 && !instBuffer.notEmpty() && fetchState == FetchDone);
        finishDecode();
    endrule

    (* descending_urgency = "fetch, prematureFinishDecode, decodeKillInstBuffer" *)
    rule decodeKillInstBuffer(decodeState == Decoding && killCount != 0);
        if(!instBuffer.notEmpty())
        begin
            if(fetchState == FetchDone)
            begin
                killCount   <= killCount - 1;
                finishDecode();
                decodeCount <= fromInteger(valueOf(FetchWidth));
            end
        end
        else
        begin
            instBuffer.deq();
            branchBuffer.deq();
            instBufferCount <= instBufferCount - 1;
            fpTokKill.send((instBuffer.first()).token);
            fpDecodeKill.send((instBuffer.first()).token);
        end
    endrule
endmodule
