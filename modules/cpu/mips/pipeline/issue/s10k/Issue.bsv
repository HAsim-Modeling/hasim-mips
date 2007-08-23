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

    Port_Receive#(IssuePort)                        issuePort <- mkPort_Receive("decodeToIssue", valueOf(FetchWidth));

    Port_Send#(ExecEntry)                            execPort <- mkPort_Send("exec");
    Port_Send#(ExecEntry)                             memPort <- mkPort_Send("mem");
    Port_Receive#(Token)                        killIssuePort <- mkPort_Receive("killIssue", 1);

    Reg#(KillState)                                 killState <- mkReg(KillDone);
    Reg#(IssueState)                               issueState <- mkReg(IssueDone);
    Reg#(DispatchState)                         dispatchState <- mkReg(DispatchDone);

    Reg#(FetchCount)                            dispatchCount <- mkReg(?);
    Reg#(FuncUnitCount)                           funcUnitPos <- mkReg(?);

    Reg#(FreeListCount)                         freeListCount <- mkReg(?);

    IssueAlg                                         issueAlg <- mkIssueAlg();

    Reg#(Bool)                                modelCycleBegin <- mkReg(True);

    EventRecorder                                    eventFet <- mkEventRecorder("Issue");

    rule synchronize(killState == KillDone && issueState == IssueDone && dispatchState == DispatchDone);
        if(!modelCycleBegin)
        begin
            let pseudoIntIssueCount = fromInteger(valueOf(TSub#(IntQNum, FetchWidth)));
            let pseudoMemIssueCount = fromInteger(valueOf(TSub#(MemQNum, FetchWidth)));
            let freeIntQ = pseudoIntIssueCount > issueAlg.getIntQCount()? pseudoIntIssueCount - issueAlg.getIntQCount() : 0;
            let freeMemQ = pseudoMemIssueCount > issueAlg.getMemQCount()? pseudoMemIssueCount - issueAlg.getMemQCount() : 0;

            intQCountPort.send(tagged Valid freeIntQ);
            memQCountPort.send(tagged Valid freeMemQ);
            freeListAddPort.send(tagged Valid freeListCount);
            eventFet.recordEvent(tagged Valid zeroExtend(freeListCount));
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

        let recvMaybe <- (issueAlg.respIssueVals[funcUnitPos]).get();
        if(funcUnitPos == fromInteger(valueOf(TSub#(FuncUnitNum,1))))
            memPort.send(recvMaybe);
        else
            execPort.send(recvMaybe);
        case (recvMaybe) matches
            tagged Valid .recv:
                fpExePort.send(tuple2(recv.token, ?));
        endcase
    endrule

    rule dispatch(dispatchState == Dispatch && issueAlg.canIssue());
        dispatchCount <= dispatchCount + 1;
        if(dispatchCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
        begin
            dispatchState <= DispatchDone;
            killState <= KillDone;
        end

        Maybe#(IssuePort) recvMaybe <- issuePort.receive();
        case (recvMaybe) matches
            tagged Valid .recv:
            begin
                match{.token, .dep} = fpDecodeResp.receive();
                fpDecodeResp.deq();
                IssueEntry issueEntry = IssueEntry{token: recv.token,
                                                   addr: recv.addr,
                                                   issueType: recv.issueType,
                                                   robTag: recv.robTag,
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
                begin
                    issueAlg.dispatch(issueEntry);
                end
            end
        endcase
    endrule
endmodule
