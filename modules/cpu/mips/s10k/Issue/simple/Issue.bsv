import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import Vector::*;
import GetPut::*;

import hasim_cpu_parameters::*;
import hasim_cpu_types::*;
import hasim_cpu_issueAlg::*;

typedef enum {Issue, IssueDone} IssueState deriving (Bits, Eq);
typedef enum {Dispatch, DispatchDone} DispatchState deriving (Bits, Eq);

module [HASim_Module] mkIssue();
    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, fromInteger(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, fromInteger(i)), 1);

    Connection_Send#(Tuple2#(Token, Bit#(0)))      fpExePort <- mkConnection_Send("fp_exe_req");

    Port_Send#(IntQCountType)                  intQCountPort <- mkPort_Send("issueToDecodeIntQ");
    Port_Send#(MemQCountType)                  memQCountPort <- mkPort_Send("issueToDecodeMemQ");

    Vector#(FetchWidth, Port_Receive#(IssueEntry)) issuePort <- genWithM(receiveFunctionM("decodeToIssue"));

    Vector#(NumFuncUnits, Port_Send#(ExecEntry))    execPort <- genWithM(sendFunctionM("issueToExec"));

    Reg#(IssueState)                              issueState <- mkReg(IssueDone);
    Reg#(DispatchState)                        dispatchState <- mkReg(DispatchDone);

    Reg#(FetchCount)                           dispatchCount <- mkReg(?);
    Reg#(FuncUnitPos)                            funcUnitPos <- mkReg(?);

    IssueAlg                                        issueAlg <- mkIssueAlg();

    rule synchronize(issueState == IssueDone && dispatchState == DispatchDone);
        let freeIntQ = fromInteger(valueOf(IntQCount)) - issueAlg.getIntQCount();
        let freeMemQ = fromInteger(valueOf(MemQCount)) - issueAlg.getMemQCount();
        intQCountPort.send(tagged Valid freeIntQ);
        memQCountPort.send(tagged Valid freeMemQ);

        issueState    <= Issue;
        dispatchState <= Dispatch;
        funcUnitPos   <= 0;
        dispatchCount <= 0;

        issueAlg.reqIssueVals();
    endrule

    rule issue(issueAlg.canIssue());
        funcUnitPos <= funcUnitPos + 1;
        if(funcUnitPos == fromInteger(valueOf(TSub#(NumFuncUnits,1))))
            issueState <= IssueDone;

        let recv <- (issueAlg.respIssueVals[funcUnitPos]).get();
        execPort[funcUnitPos].send(recv);
        if(isValid(recv))
            fpExePort.send(tuple2((validValue(recv)).token, ?));
    endrule

    rule dispatch(issueAlg.canIssue());
        dispatchCount <= dispatchCount + 1;
        if(dispatchCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
            dispatchState <= DispatchDone;

        let recv <- issuePort[dispatchCount].receive();
        if(isValid(recv))
            issueAlg.dispatch(validValue(recv));
    endrule
endmodule
