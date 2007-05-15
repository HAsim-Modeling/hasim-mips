import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import RegFile::*;
import Vector::*;

import hasim_parameters::*;
import hasim_types::*;

typedef Bit#(TLog#(TAdd#(NumFuncUnits,1))) FuncUnitPos;

interface IssueQ;
    method IntQCountType getHead();
    method IntQCountType getCount();
    method Maybe#(IssueEntry) read(IntQCountType idx);
    method Action write(IntQCountType idx, IssueEntry issue);
    method Action remove(IntQCountType idx);
    method Action add(IssueEntry issue);
endinterface

//The user must decide how to use it
module mkIssueQ(IssueQ);
    Reg#(IntQCountType)  head <- mkReg(0);
    Reg#(IntQCountType) count <- mkReg(0);

    RegFile#(Bit#(TLog#(IntQCount)), Maybe#(IssueEntry)) regFile <- mkRegFileFull();

    method IntQCountType getHead();
        return head;
    endmethod

    method IntQCountType getCount();
        return count;
    endmethod

    method Maybe#(IssueEntry) read(IntQCountType idx);
        return regFile.sub(truncate((head+idx)%fromInteger(valueOf(IntQCount))));
    endmethod

    method Action write(IntQCountType idx, IssueEntry issue);
        regFile.upd(truncate((head+idx)%fromInteger(valueOf(IntQCount))), tagged Valid issue);
    endmethod

    method Action remove(IntQCountType idx);
        regFile.upd(truncate((head+idx)%fromInteger(valueOf(IntQCount))), tagged Invalid);
        if(idx == head)
        begin
            head  <= (head + 1)%fromInteger(valueOf(IntQCount));
            count <= count - 1;
        end
    endmethod

    method Action add(IssueEntry issue);
        count <= count + 1;
        regFile.upd(truncate(head+count), tagged Valid issue);
    endmethod
endmodule

typedef enum {Issue, IssueFinal, IssueDone} IssueState    deriving (Bits, Eq);
typedef enum {Dispatch, DispatchDone}       DispatchState deriving (Bits, Eq);

module [HASim_Module] mkIssue();
    Connection_Send#(Tuple2#(Token, void))                            fpExeReq <- mkConnection_Send("fp_exe_req");

    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, fromInteger(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, fromInteger(i)), 1);

    Vector#(TSub#(NumFuncUnits,1), Port_Send#(ExecEntry))             execPort <- genWithM(sendFunctionM("issueToExec"));
    Port_Send#(ExecEntry)                                              memPort <- mkPort_Send("issueToExecMem");

    Vector#(FetchWidth, Port_Receive#(IssueEntry))                   issuePort <- genWithM(receiveFunctionM("decodeToIssue"));
    Port_Send#(IntQCountType)                                    intQCountPort <- mkPort_Send("issueToDecodeIntQ");
    Port_Send#(AddrQCountType)                                  addrQCountPort <- mkPort_Send("issueToDecodeAddrQ");

    Reg#(IntQCountType)                                              intQCount <- mkReg(fromInteger(valueOf(IntQCount)));
    Reg#(AddrQCountType)                                            addrQCount <- mkReg(fromInteger(valueOf(AddrQCount)));

    Reg#(Maybe#(PRName))                                               oldAlu1 <- mkReg(?);
    Reg#(Maybe#(PRName))                                               oldAlu2 <- mkReg(?);
    Reg#(Maybe#(PRName))                                               oldMem1 <- mkReg(tagged Invalid);
    Reg#(Maybe#(PRName))                                               oldMem2 <- mkReg(?);

    Reg#(Maybe#(PRName))                                               newAlu1 <- mkReg(tagged Invalid);
    Reg#(Maybe#(PRName))                                               newAlu2 <- mkReg(tagged Invalid);
    Reg#(Maybe#(PRName))                                               newMem1 <- mkReg(tagged Invalid);

    Reg#(IssueState)                                                issueState <- mkReg(IssueDone);
    Reg#(DispatchState)                                          dispatchState <- mkReg(DispatchDone);

    Reg#(FuncUnitPos)                                               issueCount <- mkReg(?);
    Reg#(FetchCount)                                             dispatchCount <- mkReg(?);

    Reg#(IntQCountType)                                               intCount <- mkReg(0);
    Reg#(IntQCountType)                                                 intPtr <- mkReg(0);
    Reg#(AddrQCountType)                                              memCount <- mkReg(0);
    Reg#(AddrQCountType)                                                memPtr <- mkReg(0);

    IssueQ                                                                intQ <- mkIssueQ();
    IssueQ                                                               addrQ <- mkIssueQ();

    Vector#(NumFuncUnits, Reg#(Maybe#(ExecEntry)))                   issueVals <- replicateM(mkReg(tagged Invalid));

    rule synchronize(issueState == IssueDone && dispatchState == DispatchDone);
        issueState    <= Issue;

        intPtr   <= 0;
        intCount <= intQ.getCount();

        memPtr   <= 0;
        memCount <= intQ.getCount();

        intQCountPort.send(tagged Valid intQCount);
        addrQCountPort.send(tagged Valid addrQCount);

        oldAlu1  <= newAlu1;
        oldAlu2  <= newAlu2;
        oldMem1  <= newMem1;
        oldMem2  <= oldMem1;

        for(Integer i = 0; i < valueOf(NumFuncUnits) - 1; i=i+1)
        begin
            execPort[i].send(issueVals[i]);
            issueVals[i] <= tagged Invalid;
        end
        memPort.send(issueVals[valueOf(NumFuncUnits)-1]);
        issueVals[valueOf(NumFuncUnits)-1] <= tagged Invalid;
    endrule

    function isReady(PRName pRName) =  isValid(oldAlu1) && pRName == validValue(oldAlu1) || 
                                       isValid(oldAlu2) && pRName == validValue(oldAlu2) || 
                                       isValid(oldMem2) && pRName == validValue(oldMem2);
    function isAllReady(IssueEntry issue) = issue.src1Ready && issue.src2Ready;

    let intIssueEntry = intQ.read(intPtr);
    let intExecEntry  = ExecEntry{token: (validValue(intIssueEntry)).token, robTag: (validValue(intIssueEntry)).robTag, pRName: (validValue(intIssueEntry)).dest};
    let newIntIssueEntry = IssueEntry{issueType: (validValue(intIssueEntry)).issueType, token: (validValue(intIssueEntry)).token, robTag: (validValue(intIssueEntry)).robTag,
                                   src1Ready: (validValue(intIssueEntry)).src1Ready || isReady((validValue(intIssueEntry)).src1), src1: (validValue(intIssueEntry)).src1,
                                   src2Ready: (validValue(intIssueEntry)).src2Ready || isReady((validValue(intIssueEntry)).src2), src2: (validValue(intIssueEntry)).src2,
                                   dest: (validValue(intIssueEntry)).dest};

    let addrIssueEntry = addrQ.read(memPtr);
    let addrExecEntry  = ExecEntry{token: (validValue(addrIssueEntry)).token, robTag: (validValue(addrIssueEntry)).robTag, pRName: (validValue(addrIssueEntry)).dest};
    let newAddrIssueEntry = IssueEntry{issueType: (validValue(addrIssueEntry)).issueType, token: (validValue(addrIssueEntry)).token, robTag: (validValue(addrIssueEntry)).robTag,
                                   src1Ready: (validValue(addrIssueEntry)).src1Ready || isReady((validValue(addrIssueEntry)).src1), src1: (validValue(addrIssueEntry)).src1,
                                   src2Ready: (validValue(addrIssueEntry)).src2Ready || isReady((validValue(addrIssueEntry)).src2), src2: (validValue(addrIssueEntry)).src2,
                                   dest: (validValue(addrIssueEntry)).dest};

    rule noIntIssue(issueState == Issue && intPtr != intCount && !(isValid(intIssueEntry) && isAllReady(newIntIssueEntry)));
        intQ.write(intPtr, newIntIssueEntry);
        intPtr <= intPtr + 1;
    endrule

    rule intIssueJR(issueState == Issue && intPtr != intCount && isValid(intIssueEntry) && isAllReady(newIntIssueEntry) && (validValue(intIssueEntry)).issueType == JR );
        if(!isValid(issueVals[0]))
        begin
            issueVals[0] <= tagged Valid intExecEntry;
            intQ.remove(intPtr);
        end
        intPtr <= intPtr + 1;
    endrule

    rule intIssueBranchShift(issueState == Issue && intPtr != intCount && isValid(intIssueEntry) && ((validValue(intIssueEntry)).issueType == Branch || (validValue(intIssueEntry)).issueType == Shift) && isAllReady(newIntIssueEntry));
        if(!isValid(issueVals[1]))
        begin
            issueVals[1] <= tagged Valid intExecEntry;
            intQ.remove(intPtr);
        end
        intPtr <= intPtr + 1;
    endrule

    rule intIssueNormal(issueState == Issue && intPtr != intCount && isValid(intIssueEntry) && (validValue(intIssueEntry)).issueType == Normal && isAllReady(newIntIssueEntry));
        if(!isValid(issueVals[2]))
        begin
            issueVals[2] <= tagged Valid intExecEntry;
            intQ.remove(intPtr);
            newAlu2 <= tagged Valid ((validValue(intIssueEntry)).dest);
        end
        else if(!isValid(issueVals[1]))
        begin
            issueVals[1] <= tagged Valid intExecEntry;
            intQ.remove(intPtr);
            newAlu1 <= tagged Valid ((validValue(intIssueEntry)).dest);
        end
        intPtr <= intPtr + 1;
    endrule

    rule intIssueJ(issueState == Issue && intPtr != intCount && isValid(intIssueEntry) && (validValue(intIssueEntry)).issueType == J && isAllReady(newIntIssueEntry));
        if(!isValid(issueVals[3]))
        begin
            issueVals[3] <= tagged Valid intExecEntry;
            intQ.remove(intPtr);
        end
        intPtr <= intPtr + 1;
    endrule

    rule memIssue(issueState == Issue && memPtr != memCount && isValid(addrIssueEntry) && isAllReady(newAddrIssueEntry));
        issueVals[4] <= tagged Valid addrExecEntry;
    endrule

    rule issueFinish(issueState == Issue && intPtr == intCount && (memPtr == memCount || !isValid(addrIssueEntry) || !isAllReady(newAddrIssueEntry)));
        issueState <= IssueFinal;
        dispatchState <= Dispatch;
        issueCount <= 0;
        dispatchCount <= 0;
    endrule

    rule issueFinishProceeding(issueState == IssueFinal && issueCount != fromInteger(valueOf(NumFuncUnits))-1);
        execPort[issueCount].send(issueVals[issueCount]);
        if(isValid(issueVals[issueCount]))
            fpExeReq.send(tuple2((validValue(issueVals[issueCount])).token, ?));
    endrule

    rule issueFinishProeceedingReally(issueState == IssueFinal && issueCount == fromInteger(valueOf(NumFuncUnits))-1);
        memPort.send(issueVals[issueCount]);
        if(isValid(issueVals[issueCount]))
            fpExeReq.send(tuple2((validValue(issueVals[issueCount])).token, ?));
        issueState <= IssueDone;
    endrule

    rule dispatch(dispatchState == Dispatch && dispatchCount != fromInteger(valueOf(FetchWidth)));
        let issue <- issuePort[dispatchCount].receive();
        if(isValid(issue))
        begin
            if(validValue(issue).issueType != Load && validValue(issue).issueType != Store)
                intQ.add(validValue(issue));
            else
                addrQ.add(validValue(issue));
        end
    endrule

    rule dispatchDone(dispatchState == Dispatch && dispatchCount == fromInteger(valueOf(FetchWidth)));
        dispatchState <= DispatchDone;
    endrule
endmodule
