import hasim_common::*;
import hasim_isa::*;

import Vector::*;
import GetPut::*;

import hasim_cpu_parameters::*;
import hasim_cpu_types::*;
import hasim_issue_alg::*;

typedef enum {Kill, KillContinue, KillDone} KillState deriving (Bits, Eq);
typedef enum {Issue, IssueDone} IssueState deriving (Bits, Eq);
typedef enum {Dispatch, DispatchDone} DispatchState deriving (Bits, Eq);

module [HASim_Module] mkPipe_Issue();
    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, fromInteger(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, fromInteger(i)), 1);

    Connection_Receive#(Tuple2#(Token, DepInfo)) fpDecodeResp <- mkConnection_Receive("fp_dec_resp");
    Connection_Send#(Tuple2#(Token, Bit#(0)))       fpExePort <- mkConnection_Send("fp_exe_req");

    Connection_Send#(Token)                         fpExeKill <- mkConnection_Send("fp_exe_kill");

    Port_Send#(IntQCountType)                   intQCountPort <- mkPort_Send("issueToDecodeIntQ");
    Port_Send#(MemQCountType)                   memQCountPort <- mkPort_Send("issueToDecodeMemQ");

    Vector#(FetchWidth, Port_Receive#(IssuePort))   issuePort <- genWithM(receiveFunctionM("decodeToIssue"));

    Vector#(NumFuncUnits, Port_Send#(ExecEntry))     execPort <- genWithM(sendFunctionM("issueToExec"));

    Port_Receive#(Token)                            killIssue <- mkPort_Receive("execToIssueKill", 1);

    Reg#(KillState)                                 killState <- mkReg(KillDone);
    Reg#(IssueState)                               issueState <- mkReg(IssueDone);
    Reg#(DispatchState)                         dispatchState <- mkReg(DispatchDone);

    Reg#(FetchCount)                            dispatchCount <- mkReg(?);
    Reg#(FuncUnitPos)                             funcUnitPos <- mkReg(?);

    IssueAlg                                         issueAlg <- mkIssueAlg();

    Reg#(Bool)                                modelCycleBegin <- mkReg(True);

    Reg#(ClockCounter)                           modelCounter <- mkReg(0);

    rule synchronize(killState == KillDone && issueState == IssueDone && dispatchState == DispatchDone);
        modelCounter  <= modelCounter + 1;
        $display("Issue synchronize @ Model: %0d", modelCounter);

        let pseudoIntIssueFreeCount = fromInteger(valueOf(TSub#(IntQCount, FetchWidth)));
        let pseudoMemIssueFreeCount = fromInteger(valueOf(TSub#(MemQCount, FetchWidth)));
        let freeIntQ = pseudoIntIssueFreeCount > issueAlg.getIntQCount()? pseudoIntIssueFreeCount - issueAlg.getIntQCount() : 0;
        let freeMemQ = pseudoMemIssueFreeCount > issueAlg.getMemQCount()? pseudoMemIssueFreeCount - issueAlg.getMemQCount() : 0;

        if(!modelCycleBegin)
        begin
            intQCountPort.send(tagged Valid freeIntQ);
            memQCountPort.send(tagged Valid freeMemQ);
        end

        modelCycleBegin <= False;

        Maybe#(Token) newKillToken <- killIssue.receive();

        if(isValid(newKillToken))
        begin
            issueAlg.killInitialize(validValue(newKillToken));
            killState <= Kill;
        end
        else
        begin
            issueState    <= Issue;
            dispatchState <= Dispatch;
            funcUnitPos   <= 0;
            dispatchCount <= 0;
            issueAlg.reqIssueVals();
        end
    endrule

    rule kill(killState == Kill);
        if(!issueAlg.doneKill())
        begin
            Maybe#(Token) tokenMaybe <- issueAlg.killNext();
            case (tokenMaybe) matches
                tagged Valid .token: fpExeKill.send(token);
            endcase
        end
        else
        begin
            killState     <= KillContinue;
            issueState    <= Issue;
            dispatchState <= Dispatch;
            funcUnitPos   <= 0;
            dispatchCount <= 0;
            issueAlg.reqIssueVals();
        end
    endrule

    rule issue(issueState == Issue && issueAlg.canIssue());
        funcUnitPos    <= funcUnitPos + 1;
        if(funcUnitPos == fromInteger(valueOf(TSub#(NumFuncUnits,1))))
            issueState <= IssueDone;

        let recv <- (issueAlg.respIssueVals[funcUnitPos]).get();
        execPort[funcUnitPos].send(recv);
        if(isValid(recv))
        begin
            fpExePort.send(tuple2((validValue(recv)).token, ?));
            $display("Issue: Token: %0d, index: %0d @ Model: %0d", (validValue(recv)).token.index, funcUnitPos, modelCounter-1);
        end
    endrule

    rule dispatch(dispatchState == Dispatch && issueAlg.canIssue());
        dispatchCount <= dispatchCount + 1;
        if(dispatchCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
        begin
            dispatchState <= DispatchDone;
            killState <= KillDone;
        end

        Maybe#(IssuePort) recvMaybe <- issuePort[dispatchCount].receive();

        case (recvMaybe) matches
            tagged Valid .recv:
            begin
                match{.token, .dep} <- fpDecodeResp.receive();
                Bool src1Ready = !isValid(dep.dep_src1);
                Bool src2Ready = !isValid(dep.dep_src2);
                PRName src1    = tpl_2(validValue(dep.dep_src1));
                PRName src2    = tpl_2(validValue(dep.dep_src2));
                PRName dest    = tpl_2(fromMaybe(unpack(0),dep.dep_dest));
                $display("token: %0d %0d, %0d %0d %0d %0d : %0d", recv.token.index, token.index, src1, src1Ready, src2, src2Ready, dest);
                IssueEntry issueEntry = IssueEntry{token: recv.token, issueType: recv.issueType, robTag: recv.robTag, freeCount: recv.freeCount,
                                                   src1Ready: src1Ready, src1: src1, src2Ready: src2Ready, src2: src2, dest: dest,
                                                   branchIndex: recv.branchIndex, pred: recv.pred, predAddr: recv.predAddr};

                if(killState == KillContinue)
                    fpExeKill.send(token);
                else
                    issueAlg.dispatch(issueEntry);
            end
        endcase
    endrule
endmodule
