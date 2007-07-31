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
    
    Port_Receive#(Addr)                                   addrPort <- mkPort_Receive("fetchToDecode", valueOf(FetchWidth));

    Port_Send#(FetchCount)                          fetchCountPort <- mkPort_Send("decodeToFetchDecodeNum");
    Port_Send#(Addr)                            predictedTakenPort <- mkPort_Send("decodeToFetchPredictedTaken");
    Port_Send#(Addr)                                mispredictPort <- mkPort_Send("decodeToFetchMispredict");

    Port_Receive#(IntQCount)                         intQCountPort <- mkPort_Receive("issueToDecodeIntQ", 1);
    Port_Receive#(MemQCount)                         memQCountPort <- mkPort_Receive("issueToDecodeMemQ", 1);
    Port_Receive#(FreeListCount)                   freeListAddPort <- mkPort_Receive("issueToDecodeFreeListAdd", 1);

    Port_Send#(IssuePort)                                issuePort <- mkPort_Send("decodeToIssue");

    Port_Receive#(ExecResult)                       execResultPort <- mkPort_Receive("execToDecodeResult", valueOf(FuncUnitNum));
    Port_Receive#(KillData)                         killDecodePort <- mkPort_Receive("execToDecodeKill", 1);

    Port_Send#(Token)                              commitTokenPort <- mkPort_Send("decodeToCommit");

    Reg#(ClockCounter)                                    clockReg <- mkReg(0);
    Reg#(ClockCounter)                                    modelReg <- mkReg(0);

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

    Reg#(Bool)                                   fillDecodeInvalid <- mkReg(?);
    Reg#(Bool)                                   fillCommitInvalid <- mkReg(?);

    Rob                                                        rob <- mkRob();
    BranchStack                                        branchStack <- mkBranchStack();
    BranchPred                                          branchPred <- mkBranchPred();
    FIFO#(Addr)                                       targetBuffer <- mkTargetBuffer(pcStart);

    Reg#(Bool)                                     modelCycleBegin <- mkReg(True);

    Stat                                              clockCounter <- mkStatCounter("FPGA_cycles");
    Stat                                              modelCounter <- mkStatCounter("Model_cycles");
    Stat                                                   rewinds <- mkStatCounter("Rewinds");
    Stat                                                   decodes <- mkStatCounter("Decodes");
    Stat                                                   fetches <- mkStatCounter("Fetches");
    Stat                                                   commits <- mkStatCounter("Commits");

    rule clockCount(True);
        clockCounter.incr();
        clockReg <= clockReg + 1;
    endrule

    rule branchBufferFill(True);
        Bool pred <- branchPred.getPredResp();
        branchBuffer.enq(pred);
    endrule

    rule synchronize(fetchState == FetchDone && robUpdateState == RobUpdateDone && decodeState == DecodeDone && commitState == CommitDone);
        $display("1 1 %0d %0d", clockReg, modelReg);
        $display("3 1 %0d %0d", clockReg, modelReg);
        modelReg <= modelReg + 1;
        modelCounter.incr();
        FetchCount fetchCountSend = (instBufferCount < fromInteger(valueOf(FetchWidth)))? truncate(fromInteger(valueOf(FetchWidth)) - instBufferCount): 0;

        Maybe#(KillData) killTokenGet  <- killDecodePort.receive();
        case (killTokenGet) matches
            tagged Valid .killData:
            begin
                $display("killed %0d %0d", killData.token.index, modelReg);
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

        fillCommitInvalid      <= False;

        robUpdateState         <= RobUpdate;
        robUpdateCount         <= 0;

        predictedPC            <= tagged Invalid;
        decodeCount            <= 0;
    endrule

    rule fetch(fetchState == Fetch);
        Maybe#(Addr) addrMaybe  <- addrPort.receive();
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
        begin
            $display("1 0 %0d %0d", clockReg, modelReg-1);
            fetchState <= FetchDone;
        end
    endrule

    rule commit(commitState == Commit);
        Maybe#(RobEntry) robEntryMaybe <- rob.readHead();
        commitCount                    <= commitCount + 1;
        if(commitCount == fromInteger(valueOf(TSub#(CommitWidth,1))))
            commitState <= CommitDone;
        if(!fillCommitInvalid)
        begin
            if(robEntryMaybe matches tagged Valid .robEntry &&& robEntry.done)
            begin
                commits.incr();
                commitTokenPort.send(tagged Valid robEntry.token);
                if(robEntry.issueType == Branch)
                    branchPred.upd(robEntry.token, robEntry.addr, robEntry.pred, robEntry.taken);
                if(robEntry.finished)
                    local_ctrl.endProgram(robEntry.status);
                rob.incrementHead();
                if(!(robEntry.issueType == Branch || robEntry.issueType == J || robEntry.issueType == JR || robEntry.issueType == Store))
                    freeListCount <= freeListCount + 1;
            end
            else
            begin
                fillCommitInvalid <= True;
                commitTokenPort.send(tagged Invalid);
            end
        end
        else
        begin
            commitTokenPort.send(tagged Invalid);
        end
    endrule

    rule update(robUpdateState == RobUpdate);
        Maybe#(ExecResult) execResultMaybe  <- execResultPort.receive();

        case (execResultMaybe) matches
            tagged Valid .exec:
            begin
                Tuple2#(Token, void) memAck    <- fpMemoryResp.receive();
                if(rob.isValidEntry(exec.robTag))
                begin
                    RobEntry newRobEntry = RobEntry {
                                               token: exec.token,
                                               addr: exec.addr,
                                               issueType: exec.issueType,
                                               done: True,
                                               pred: exec.pred,
                                               taken: exec.taken,
                                               finished: exec.finished,
                                               status: exec.status
                                           };
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
            $display("3 0 %0d %0d", clockReg, modelReg-1);
            $display("2 1 %0d %0d", clockReg, modelReg-1);
            $display("4 1 %0d %0d", clockReg, modelReg-1);
            robUpdateState <= RobUpdateDone;
            decodeState    <= Decoding;
            commitState    <= Commit;
            commitCount    <= 0;
            fillDecodeInvalid <= killCount != 0;
        end
    endrule

    rule decodeInst(decodeState == Decoding && killCount == 0 && instBuffer.notEmpty() && !fillDecodeInvalid);
        decodeCount     <= decodeCount + 1;
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

                RobEntry res = RobEntry{done: False};
                IssuePort issue = IssuePort{issueType: issueType,
                                            addr: currAddr,
                                            token: currToken,
                                            robTag: rob.getTail(),
                                            branchIndex: 0,
                                            pred: pred,
                                            predAddr: validValue(predPC)};

                if(regFileUpd) freeListCount <= freeListCount - 1;
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

                instBufferCount <= instBufferCount - 1;

                rob.writeTail(res);

                fpDecodeReq.send(tuple2(currToken, ?));
                $display("decodeSend: %0d %0d %0d", currToken.index, decodeCount, modelReg-1);
                issuePort.send(tagged Valid issue);

                instBuffer.deq();
                if(kill || branch && pred)
                begin
                    killCount <= 2;
                    if(decodeCount != fromInteger(valueOf(TSub#(FetchWidth,1))))
                        fillDecodeInvalid <= True;
                end
                else if(decodeCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
                begin
                    $display("4 0 %0d %0d", clockReg, modelReg-1);
                    decodeState <= DecodeDone;
                end
            end
            else
            begin
                $display("decodeSend Invalid finish %0d", modelReg-1);
                issuePort.send(tagged Invalid);
                decodeState <= DecodeDone;
                if(decodeCount != fromInteger(valueOf(TSub#(FetchWidth,1))))
                    fillDecodeInvalid <= True;
            end
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
            instBufferCount <= instBufferCount - 1;
            $display("decodeSend Invalid nop %0d", modelReg-1);
            issuePort.send(tagged Invalid);
            instBuffer.deq();
            branchBuffer.deq();
            fpTokKill.send((instBuffer.first()).token);
            fpDecodeKill.send((instBuffer.first()).token);
            if(decodeCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
            begin
                $display("4 0 %0d %0d", clockReg, modelReg-1);
                decodeState <= DecodeDone;
            end
        end
    endrule

    rule fillDecodeInvalidRule(fillDecodeInvalid);
        decodeCount <= decodeCount + 1;
        $display("decodeSend Invalid fill invalid %0d", modelReg-1);
        issuePort.send(tagged Invalid);
        if(decodeCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
        begin
            fillDecodeInvalid <= False;
        end
    endrule

    rule prematureFinishDecode(decodeState == Decoding && killCount == 0 && !instBuffer.notEmpty() && fetchState == FetchDone);
        decodeCount <= decodeCount + 1;
        $display("decodeSend Invalid empty %0d", modelReg-1);
        issuePort.send(tagged Invalid);
        if(decodeCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
            decodeState <= DecodeDone;
    endrule

    (* descending_urgency = "fetch, prematureFinishDecode, decodeKillInstBuffer" *)
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
            branchBuffer.deq();
            instBufferCount <= instBufferCount - 1;
            fpTokKill.send((instBuffer.first()).token);
            fpDecodeKill.send((instBuffer.first()).token);
        end
    endrule
endmodule
