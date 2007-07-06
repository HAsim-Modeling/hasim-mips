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
    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, integerToString(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, integerToString(i)), 1);

    Connection_Receive#(Tuple2#(Token, DepInfo)) fpDecodeResp <- mkConnection_Receive("fp_dec_resp");
    Connection_Send#(Tuple2#(Token, Bit#(0)))       fpExePort <- mkConnection_Send("fp_exe_req");

    Connection_Send#(Token)                         fpExeKill <- mkConnection_Send("fp_exe_kill");

    Port_Send#(IntQCount)                       intQCountPort <- mkPort_Send("issueToDecodeIntQ");
    Port_Send#(MemQCount)                       memQCountPort <- mkPort_Send("issueToDecodeMemQ");
    Port_Send#(FreeListCount)                 freeListAddPort <- mkPort_Send("issueToDecodeFreeListAdd");

    Vector#(FetchWidth, Port_Receive#(IssuePort))   issuePort <- genWithM(receiveFunctionM("decodeToIssue"));

    Vector#(FuncUnitNum, Port_Send#(ExecEntry))      execPort <- genWithM(sendFunctionM("issueToExec"));
    Port_Receive#(Token)                        killIssuePort <- mkPort_Receive("execToIssueKill", 1);

    Reg#(KillState)                                 killState <- mkReg(KillDone);
    Reg#(IssueState)                               issueState <- mkReg(IssueDone);
    Reg#(DispatchState)                         dispatchState <- mkReg(DispatchDone);

    Reg#(FetchCount)                            dispatchCount <- mkReg(?);
    Reg#(FuncUnitCount)                           funcUnitPos <- mkReg(?);

    Reg#(FreeListCount)                         freeListCount <- mkReg(?);

    IssueAlg                                         issueAlg <- mkIssueAlg();

    Reg#(Bool)                                modelCycleBegin <- mkReg(True);

    rule synchronize(killState == KillDone && issueState == IssueDone && dispatchState == DispatchDone);
        let pseudoIntIssueCount = fromInteger(valueOf(TSub#(IntQNum, FetchWidth)));
        let pseudoMemIssueCount = fromInteger(valueOf(TSub#(MemQNum, FetchWidth)));
        let freeIntQ = pseudoIntIssueCount > issueAlg.getIntQCount()? pseudoIntIssueCount - issueAlg.getIntQCount() : 0;
        let freeMemQ = pseudoMemIssueCount > issueAlg.getMemQCount()? pseudoMemIssueCount - issueAlg.getMemQCount() : 0;

        if(!modelCycleBegin)
        begin
            intQCountPort.send(tagged Valid freeIntQ);
            memQCountPort.send(tagged Valid freeMemQ);
            freeListAddPort.send(tagged Valid freeListCount);
        end

        modelCycleBegin <= False;

        Maybe#(Token) newKillToken <- killIssuePort.receive();

        if(isValid(newKillToken))
        begin
            issueAlg.killInitialize(validValue(newKillToken));
            killState <= Kill;
        end
        else
        begin
            freeListCount <= 0;
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
            freeListCount <= issueAlg.getFreeListAdd();
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
        if(funcUnitPos == fromInteger(valueOf(TSub#(FuncUnitNum,1))))
            issueState <= IssueDone;

        let recv <- (issueAlg.respIssueVals[funcUnitPos]).get();
        execPort[funcUnitPos].send(recv);
        if(isValid(recv))
            fpExePort.send(tuple2((validValue(recv)).token, ?));
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
                IssueEntry issueEntry = IssueEntry{token: recv.token,
                                                   issueType: recv.issueType,
                                                   robTag: recv.robTag,
                                                   freeCount: recv.freeCount,
                                                   src1Ready: !isValid(dep.dep_src1),
                                                   src1: tpl_2(validValue(dep.dep_src1)),
                                                   src2Ready: !isValid(dep.dep_src2),
                                                   src2: tpl_2(validValue(dep.dep_src2)),
                                                   dest: tpl_2(fromMaybe(unpack(0),dep.dep_dest)),
                                                   branchIndex: recv.branchIndex,
                                                   pred: recv.pred,
                                                   predAddr: recv.predAddr};

                if(killState == KillContinue)
                begin
                    fpExeKill.send(token);
                    case (dep.dep_dest) matches
                        tagged Valid {.regDest, .dest}:
                            if(dest != 0)
                                freeListCount <= freeListCount + 1;
                    endcase
                end
                else
                    issueAlg.dispatch(issueEntry);
            end
        endcase
    endrule
endmodule
