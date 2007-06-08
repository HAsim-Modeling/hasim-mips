import fpga_components::*;
import hasim_common::*;
import hasim_isa::*;

import FIFOF::*;
import FIFO::*;
import Vector::*;
import RegFile::*;

import hasim_cpu_parameters::*;
import hasim_cpu_types::*;
import hasim_rob::*;
import hasim_branch_pred::*;
import hasim_branch_target_buffer::*;
import hasim_branch_stack::*;

typedef enum {Commit, Update, Decode, RobDone} RobState   deriving (Bits, Eq);
typedef enum {Fetch, FetchDone}                FetchState deriving (Bits, Eq);

function IssueType getIssueType(Inst inst);
    return case ( inst ) matches
               // -- Memory Ops ------------------------------------------------      
               tagged LW .it : return Load;
               tagged SW .it : return Store;

               // -- Simple Ops ------------------------------------------------      
               tagged ADDIU .it : return Normal;
               tagged SLTI  .it : return Normal;
               tagged SLTIU .it : return Normal;
               tagged ANDI  .it : return Normal;
               tagged ORI   .it : return Normal;
               tagged XORI  .it : return Normal;
               tagged LUI   .it : return Normal;

               tagged SLL   .it : return Shift;
               tagged SRL   .it : return Shift;
               tagged SRA   .it : return Shift;
               tagged SLLV  .it : return Shift;
               tagged SRLV  .it : return Shift;
               tagged SRAV  .it : return Shift;

               tagged ADDU  .it : return Normal;
               tagged SUBU  .it : return Normal;
               tagged AND   .it : return Normal;
               tagged OR    .it : return Normal;
               tagged XOR   .it : return Normal;
               tagged NOR   .it : return Normal;
               tagged SLT   .it : return Normal;
               tagged SLTU  .it : return Normal;

               tagged MTC0  .it : return Normal;
               tagged MFC0  .it : return Normal;

               // -- Branches --------------------------------------------------
               tagged BLEZ  .it : return Branch;
               tagged BGTZ  .it : return Branch;
               tagged BLTZ  .it : return Branch;
               tagged BGEZ  .it : return Branch;
               tagged BEQ   .it : return Branch;
               tagged BNE   .it : return Branch;
    
               // -- Jumps -----------------------------------------------------
               tagged J     .it : return J;
               tagged JR    .it : return JR;
               tagged JAL   .it : return JAL;
               tagged JALR  .it : return JALR;
               default          : return Normal;
    endcase;
endfunction
  
module [HASim_Module] mkPipe_Decode
    //interface:
                ();
		
    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, fromInteger(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, fromInteger(i)), 1);

    Connection_Receive#(Tuple2#(Token, PackedInst))     fpFetchResp <- mkConnection_Receive("fp_fet_resp");
    Connection_Send#(Tuple2#(Token, void))              fpDecodeReq <- mkConnection_Send("fp_dec_req");
    Connection_Receive#(Tuple2#(Token, DepInfo))       fpDecodeResp <- mkConnection_Receive("fp_dec_resp");
    Connection_Receive#(Tuple2#(Token, void))          fpMemoryResp <- mkConnection_Receive("fp_mem_resp");

    Connection_Send#(Token)                               fpTokKill <- mkConnection_Send("fp_tok_kill");
    Connection_Send#(Token)                             fpFetchKill <- mkConnection_Send("fp_fet_kill");
    Connection_Send#(Token)                            fpDecodeKill <- mkConnection_Send("fp_dec_kill");
    Connection_Send#(Token)                               fpExeKill <- mkConnection_Send("fp_exe_kill");
    Connection_Send#(Token)                               fpMemKill <- mkConnection_Send("fp_mem_kill");
    Connection_Send#(Token)                          fpMemStateKill <- mkConnection_Send("fp_memstate_kill");
    Connection_Send#(Token)                       fpLocalCommitKill <- mkConnection_Send("fp_lco_kill");
    Connection_Send#(Token)                      fpGlobalCommitKill <- mkConnection_Send("fp_gco_kill");
    Connection_Send#(Token)                         fpRewindToToken <- mkConnection_Send("fp_rewindToToken");

    Connection_Server#(Command, Response)                controller <- mkConnection_Server("controller_to_tp");

    Vector#(FetchWidth, Port_Receive#(Addr))               addrPort <- genWithM(receiveFunctionM("fetchToDecode"));

    Port_Send#(FetchCount)                      instBufferCountPort <- mkPort_Send("decodeToFetchInstBuffer");
    Port_Send#(Addr)                             predictedTakenPort <- mkPort_Send("decodeToFetchPredictedTaken");
    Port_Send#(Addr)                                 mispredictPort <- mkPort_Send("decodeToFetchMispredict");

    Port_Receive#(IntQCountType)                      intQCountPort <- mkPort_Receive("issueToDecodeIntQ", 1);
    Port_Receive#(MemQCountType)                      memQCountPort <- mkPort_Receive("issueToDecodeMemQ", 1);

    Vector#(FetchWidth, Port_Send#(IssueEntry))           issuePort <- genWithM(sendFunctionM("decodeToIssue"));

    Vector#(NumFuncUnits, Port_Receive#(ExecResult)) execResultPort <- genWithM(receiveFunctionM("execToDecode"));

    Vector#(CommitWidth, Port_Send#(Token))              commitPort <- genWithM(sendFunctionM("decodeToCommit"));

    FIFOF#(InstInfo)                                     instBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(FetchWidth))); 
    FIFOF#(DepInfo)                                    decodeBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(FetchWidth)));

    Reg#(RobState)                                         robState <- mkReg(RobDone);
    Reg#(FetchState)                                     fetchState <- mkReg(FetchDone);
    Reg#(Bool)                                       killInstBuffer <- mkReg(False);
    Reg#(Bool)                                   nextKillInstBuffer <- mkReg(?);
    Reg#(Bool)                                       realDecodeDone <- mkReg(?);

    Reg#(Bit#(TLog#(TAdd#(FreeListCount,1))))     freeListFreeCount <- mkReg(fromInteger(valueOf(FreeListCount)));
    Reg#(Bit#(TLog#(TAdd#(IntQCount,1))))             intQFreeCount <- mkReg(?);
    Reg#(Bit#(TLog#(TAdd#(MemQCount,1))))             memQFreeCount <- mkReg(?);

    Reg#(FetchCount)                                     fetchCount <- mkReg(?);
    Reg#(FetchCount)                                instBufferCount <- mkReg(0);
    Reg#(Bit#(32))                                            count <- mkReg(?);

    Reg#(Maybe#(Addr))                                  predictedPC <- mkReg(tagged Invalid);
    Reg#(Maybe#(Addr))                                 mispredictPC <- mkReg(tagged Invalid);

    Rob                                                         rob <- mkROB();
    Reg#(Vector#(PRNum, Bool))                             pRegFile <- mkReg(replicate(False));
    BranchPred                                           branchPred <- mkBranchPred();
    BranchStack                                         branchStack <- mkBranchStack();
    FIFOF#(Addr)                                       targetBuffer <- mkTargetBuffer(pcStart);

    Reg#(Bool)                                                birth <- mkReg(True);

    Reg#(ClockCounter)                                 clockCounter <- mkReg(0);
    Reg#(ClockCounter)                                 modelCounter <- mkReg(0);

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
        let req <- controller.getReq();
        birth   <= False;
    endrule

    rule getDecodeResp(True);
        match {.token, .dep} <- fpDecodeResp.receive();
        decodeBuffer.enq(dep);
    endrule

    rule synchronize(fetchState == FetchDone && robState == RobDone);
        modelCounter  <= modelCounter + 1;

        let sendSize = (instBufferCount < fromInteger(valueOf(FetchWidth)))? fromInteger(valueOf(FetchWidth))-instBufferCount: 0;

        instBufferCountPort.send(tagged Valid sendSize);
        predictedTakenPort.send(predictedPC);
        mispredictPort.send(mispredictPC);

        $display("Decode synchronize: instBufferCount %0d, sendSize: %0d @ Model: %0d", instBufferCount, sendSize, modelCounter);

        let intQFreeCountLocal <- intQCountPort.receive();
        let memQFreeCountLocal <- memQCountPort.receive();
        intQFreeCount <= fromMaybe(fromInteger(valueOf(IntQCount)), intQFreeCountLocal);
        memQFreeCount <= fromMaybe(fromInteger(valueOf(MemQCount)), memQFreeCountLocal);

        fetchState    <= Fetch;
        fetchCount    <= 0;

        robState      <= Commit;
        count         <= 0;

        nextKillInstBuffer <= False;
    endrule

    rule fetch(fetchState == Fetch);
        let addrMaybe <- addrPort[fetchCount].receive();
        if(isValid(addrMaybe))
        begin
            let addr = validValue(addrMaybe);
            match {.token, .inst} <- fpFetchResp.receive();
            instBufferCount <= instBufferCount + 1;
            instBuffer.enq(InstInfo{token: token, addr: addr, inst: inst});
            fpDecodeReq.send(tuple2(token, ?));
        end
        fetchCount     <= fetchCount + 1;
        if(fetchCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
            fetchState <= FetchDone;
    endrule

    rule commit(robState == Commit);
        let robEntryMaybe <- rob.readHead();
        let robEntry       = validValue(robEntryMaybe);
        if(isValid(robEntryMaybe) && robEntry.done)
        begin
            commitPort[count].send(tagged Valid robEntry.token);
            $display("Commit: Token: %0d @ Model: %0d", robEntry.token.index, modelCounter-1);
            if(robEntry.isBranch)
                branchPred.upd(robEntry.addr, robEntry.prediction, robEntry.taken);
            if(robEntry.finished)
                controller.makeResp(tagged RESP_DoneRunning robEntry.result);
            if(count == fromInteger(valueOf(TSub#(CommitWidth,1))))
            begin
                robState <= Update;
                count    <= 0;
            end
            else
                count    <= count + 1;
        end
        else
        begin
            for(Integer i = 0; i < valueOf(CommitWidth); i=i+1)
            begin
                if(fromInteger(i) >= count)
                    commitPort[i].send(tagged Invalid);
            end
            robState <= Update;
            count    <= 0;
        end
    endrule

    rule update(robState == Update);
        let execResultMaybe  <- execResultPort[count].receive();
        let memAck           <- fpMemoryResp.receive();
        if(isValid(execResultMaybe))
        begin
            let execResult    = validValue(execResultMaybe);
            let robEntryMaybe = rob.read(execResult.robTag);
            let robEntry      = validValue(robEntryMaybe);
            if(isValid(robEntryMaybe) && robEntry.token == execResult.token)
            begin
                match {.branchTaken, .branchAddr, .finished, .result} = case (execResult.instResult) matches
                                                                            tagged RBranchTaken .addr: tuple4(True, addr, False, False);
                                                                            tagged RTerminate .result: tuple4(False, 0, True, result);
                                                                        endcase;
                let newRobEntry              = robEntry;
                newRobEntry.taken            = branchTaken;
                newRobEntry.finished         = finished;
                newRobEntry.result           = result;
                rob.write(execResult.robTag, newRobEntry);

                pRegFile[execResult.pRName] <= False;
                freeListFreeCount           <= freeListFreeCount + 1;

                if(robEntry.isBranch && robEntry.prediction != branchTaken || robEntry.isJR && robEntry.predAddr != branchAddr)
                begin
                    fpRewindToToken.send(execResult.token);
                    rob.updateTail(execResult.robTag);
                    branchStack.resolveWrong(robEntry.token);
                    killInstBuffer          <= True;
                    nextKillInstBuffer      <= True;
                    mispredictPC            <= tagged Valid branchAddr;
                    $display("Branch mispredicted @ Model: %0d. Correct address: %x", modelCounter-1, branchAddr);
                end
                else if(robEntry.isBranch || robEntry.isJR)
                    branchStack.resolveRight(robEntry.token);
            end
            else
                killRobStage(execResult.token);
            count <= count + 1;
        end
        if(count == fromInteger(valueOf(TSub#(NumFuncUnits,1))))
        begin
            robState <= Decode;
            count <= 0;
        end
    endrule

    rule decode(robState == Decode && !killInstBuffer);
        if(!instBuffer.notEmpty())
        begin
            if(fetchState == FetchDone)
                robState <= RobDone;
        end
        else
        begin
            let currBuffer  = instBuffer.first();
            let currDepInfo = decodeBuffer.first();
            let currInst    = currBuffer.inst;
            let currToken   = currBuffer.token;
            let currAddr    = currBuffer.addr;

            let src1Valid   = isValid(currDepInfo.dep_src1);
            let src1        = tpl_2(validValue(currDepInfo.dep_src1));
            let src2Valid   = isValid(currDepInfo.dep_src2);
            let src2        = tpl_2(validValue(currDepInfo.dep_src2));
            let dest        = tpl_2(validValue(currDepInfo.dep_dest));

            let src1Ready   = src1Valid? !pRegFile[src1]: True;
            let src2Ready   = src2Valid? !pRegFile[src2]: True;

            let branchPredAddr = branchPred.getPredAddr(currAddr);
            let jumpPredAddr   = targetBuffer.first();
            RobEntry res = RobEntry{token: currToken, addr: currAddr, done: False, finished: False, result: False,
                                    isBranch: False, prediction: isValid(branchPredAddr), taken: False,
                                    isJR: False, predAddr: jumpPredAddr};
            IssueEntry issue = IssueEntry{issueType: Normal,
                                          token: currToken, robTag: rob.getTail(),
                                          src1Ready: src1Ready, src1: src1,
                                          src2Ready: src2Ready, src2: src2,
                                          dest: dest};

            IssueType issueType = getIssueType(bitsToInst(currInst));

            let freeList     = isALU(currInst) || isLoad(currInst) || isJAL(currInst) || isJALR(currInst);
            let intQ         = isALU(currInst) || isBranch(currInst) || isJR(currInst) || isJALR(currInst);
            let memQ         = isLoad(currInst) || isStore(currInst);
            let isB          = isBranch(currInst);
            let isJwithR     = isJR(currInst) || isJALR(currInst);
            let branchStk    = isB || isJwithR;
            let targBuffEnq  = isJAL(currInst) || isJALR(currInst);
            let targBuffDeq  = isJwithR;

            Maybe#(Addr) newPredictedPC = case (issueType)
                                              Branch: return branchPredAddr;
                                              J     : return tagged Valid getJAddr(currInst, currAddr);
                                              JAL   : return tagged Valid getJALAddr(currInst, currAddr);
                                              JR    : return tagged Valid jumpPredAddr;
                                              JALR  : return tagged Valid jumpPredAddr;
                                              default: return tagged Invalid;
                                          endcase;
            
            let doDecode = !rob.notFull() ||
                            freeList && freeListFreeCount == 0 ||
                            intQ && intQFreeCount == 0 ||
                            memQ && memQFreeCount == 0 ||
                            branchStk && !branchStack.notFull() ||
                            targBuffEnq && !targetBuffer.notFull();

            let killInst = isB && isValid(branchPredAddr) || isJ(currInst) || isJAL(currInst) || isJR(currInst) || isJALR(currInst);

            res.isBranch = isB;
            res.isJR     = isJwithR;

            issue.issueType = issueType;

            if(doDecode)
            begin
                count <= count + 1;
                instBufferCount <= instBufferCount - 1;
                if(freeList) freeListFreeCount <= freeListFreeCount - 1;
                if(intQ)     intQFreeCount     <= intQFreeCount - 1;
                if(memQ)     memQFreeCount     <= memQFreeCount - 1;
                if(branchStk) branchStack.add(currToken);
                if(targBuffEnq) targetBuffer.enq(currAddr+4);
                if(targBuffDeq) targetBuffer.deq();

                if(killInst)
                begin
                    killInstBuffer     <= True;
                    nextKillInstBuffer <= True;
                end

                rob.writeTail(res);
                issuePort[count].send(tagged Valid issue);

                instBuffer.deq();
                decodeBuffer.deq();

                predictedPC <= newPredictedPC;

                $display("Decode : Token: %0d, addr: %x, type: %0d @ Model: %0d, res.isBranch: %0d", currToken.index, currAddr, issue.issueType, modelCounter-1, res.isBranch);
            end

            if(!doDecode || killInst)
            begin
                for(Integer i = 0; i != valueOf(FetchWidth); i=i+1)
                begin
                    if(fromInteger(i) >= count)
                        issuePort[i].send(tagged Invalid);
                end
            end

            if(!doDecode || count == fromInteger(valueOf(TSub#(FetchWidth,1))))
                robState <= RobDone;
        end
    endrule

    rule decodeKillInstBuffer(robState == Decode && killInstBuffer);
        if(!instBuffer.notEmpty())
        begin
            if(fetchState == FetchDone)
            begin
                killInstBuffer <= nextKillInstBuffer;
                robState       <= RobDone;
            end
        end
        else
        begin
            instBuffer.deq();
            decodeBuffer.deq();
            instBufferCount <= instBufferCount-1;
            killDecodeStage((instBuffer.first()).token);
        end
    endrule
endmodule
