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
    Connection_Send#(Tuple2#(Token, void))                      fpExeReq <- mkConnection_Send("fp_exe_req");

    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, fromInteger(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, fromInteger(i)), 1);

    Vector#(TSub#(NumFuncUnits,1), Port_Send#(ExecEntry))       execPort <- genWithM(sendFunctionM("issueToExecNormal"));
    Port_Send#(ExecEntry)                                        memPort <- mkPort_Send("issueToExecMem");

    Vector#(FetchWidth, Port_Receive#(IssueEntry))             issuePort <- genWithM(receiveFunctionM("decodeToIssue"));
    Port_Send#(IntQCountType)                              intQCountPort <- mkPort_Send("issueToDecodeIntQ");
    Port_Send#(AddrQCountType)                            addrQCountPort <- mkPort_Send("issueToDecodeAddrQ");

    Reg#(IntQCountType)                                        intQCount <- mkReg(fromInteger(valueOf(IntQCount)));
    Reg#(AddrQCountType)                                      addrQCount <- mkReg(fromInteger(valueOf(AddrQCount)));

    Reg#(Maybe#(PRName))                                         oldAlu1 <- mkReg(?);
    Reg#(Maybe#(PRName))                                         oldAlu2 <- mkReg(?);
    Reg#(Maybe#(PRName))                                         oldMem1 <- mkReg(tagged Invalid);
    Reg#(Maybe#(PRName))                                         oldMem2 <- mkReg(?);

    Reg#(Maybe#(PRName))                                         newAlu1 <- mkReg(tagged Invalid);
    Reg#(Maybe#(PRName))                                         newAlu2 <- mkReg(tagged Invalid);
    Reg#(Maybe#(PRName))                                         newMem1 <- mkReg(tagged Invalid);

    Reg#(IssueState)                                          issueState <- mkReg(IssueDone);
    Reg#(DispatchState)                                    dispatchState <- mkReg(DispatchDone);
    Reg#(FuncUnitPos)                                         issueCount <- mkReg(?);
    Reg#(FetchCount)                                       dispatchCount <- mkReg(?);

    Reg#(IntQCountType)                                         intCount <- mkReg(0);
    Reg#(IntQCountType)                                           intPtr <- mkReg(0);
    Reg#(AddrQCountType)                                        memCount <- mkReg(0);
    Reg#(AddrQCountType)                                          memPtr <- mkReg(0);

    IssueQ                                                          intQ <- mkIssueQ();
    IssueQ                                                         addrQ <- mkIssueQ();

    Reg#(Bool)                                             syncToExecute <- mkReg(False);
    Reg#(Bool)                                              syncToDecode <- mkReg(False);

    Vector#(NumFuncUnits, Reg#(Maybe#(ExecEntry)))             issueVals <- replicateM(mkReg(tagged Invalid));

    Reg#(Bit#(32))                                          clockCounter <- mkReg(0);

    rule clockCount(True);
        clockCounter <= clockCounter + 1;
    endrule

    rule synchronizeToDecode(issueState == IssueDone && dispatchState == DispatchDone && !syncToDecode);
        intQCountPort.send(tagged Valid intQCount);
        addrQCountPort.send(tagged Valid addrQCount);
        syncToDecode <= True;
    endrule

    rule synchronizeToExecute(issueState == IssueDone && dispatchState == DispatchDone && !syncToExecute);
        for(Integer i = 0; i < valueOf(NumFuncUnits) - 1; i=i+1)
        begin
            execPort[i].send(issueVals[i]);
            issueVals[i] <= tagged Invalid;
        end
        memPort.send(issueVals[valueOf(NumFuncUnits)-1]);
        issueVals[valueOf(NumFuncUnits)-1] <= tagged Invalid;
        syncToExecute <= True;
    endrule

    rule synchronize(issueState == IssueDone && dispatchState == DispatchDone && syncToExecute && syncToDecode);
        $display("issue_syncOver %d", clockCounter);
        issueState <= Issue;

        intPtr     <= 0;
        intCount   <= intQ.getCount();

        memPtr   <= 0;
        let addrQCountLocal = addrQ.getCount();
        memCount <= addrQCountLocal != 0 ? 1: 0;

        oldAlu1  <= newAlu1;
        oldAlu2  <= newAlu2;
        oldMem1  <= newMem1;
        oldMem2  <= oldMem1;
        syncToDecode  <= False;
        syncToExecute <= False;
    endrule

    function isReady(PRName pRName) =  isValid(oldAlu1) && pRName == validValue(oldAlu1) || 
                                       isValid(oldAlu2) && pRName == validValue(oldAlu2) || 
                                       isValid(oldMem2) && pRName == validValue(oldMem2);
    function isAllReady(IssueEntry issue) = issue.src1Ready && issue.src2Ready;

    rule intIssue(issueState == Issue && intPtr != intCount);
        let intIssueEntry = intQ.read(intPtr);
        if(isValid(intIssueEntry))
        begin
            Bool removed = ?;
            let validEntry = validValue(intIssueEntry);
            let intExecEntry  = ExecEntry{token: validEntry.token, robTag: validEntry.robTag, pRName: validEntry.dest};
            let newIntIssueEntry = IssueEntry{issueType: validEntry.issueType, token: validEntry.token, robTag: validEntry.robTag,
                                              src1Ready: validEntry.src1Ready || isReady(validEntry.src1), src1: validEntry.src1,
                                              src2Ready: validEntry.src2Ready || isReady(validEntry.src2), src2: validEntry.src2,
                                              dest: validEntry.dest};
            if(isAllReady(newIntIssueEntry))
            begin
                let issueType = (validValue(intIssueEntry)).issueType;
                if(issueType == JR)
                begin
                    if(!isValid(issueVals[0]))
                    begin
                        issueVals[0] <= tagged Valid intExecEntry;
                        removed = True;
                    end
                    else
                        removed = False;
                end
                else if(issueType == Branch || issueType == Shift)
                begin
                    if(!isValid(issueVals[1]))
                    begin
                        issueVals[1] <= tagged Valid intExecEntry;
                        removed = True;
                    end
                    else
                        removed = False;
                end
                else if(issueType == Shift)
                begin
                    if(!isValid(issueVals[1]))
                    begin
                        issueVals[1] <= tagged Valid intExecEntry;
                        removed = True;
                        newAlu2 <= tagged Valid validEntry.dest;
                    end
                    else
                        removed = False;
                end
                else if(issueType == Normal)
                begin
                    if(!isValid(issueVals[2]))
                    begin
                        issueVals[2] <= tagged Valid intExecEntry;
                        newAlu2 <= tagged Valid validEntry.dest;
                        removed = True;
                    end
                    else if(!isValid(issueVals[1]))
                    begin
                        issueVals[1] <= tagged Valid intExecEntry;
                        newAlu1 <= tagged Valid validEntry.dest;
                        removed = True;
                    end
                    else
                        removed = False;
                end
                else
                begin
                    if(!isValid(issueVals[3]))
                    begin
                        issueVals[3] <= tagged Valid intExecEntry;
                        removed = True;
                    end
                    else
                        removed = False;
                end
            end
            if(!removed)
                intQ.write(intPtr, newIntIssueEntry);
            else
                intQ.remove(intPtr);
        end
        intPtr <= intPtr + 1;
    endrule

    rule memIssue(issueState == Issue && memPtr != memCount);
        let addrIssueEntry = addrQ.read(memPtr);
        if(isValid(addrIssueEntry))
        begin
            let validEntry = validValue(addrIssueEntry);
            let addrExecEntry  = ExecEntry{token: validEntry.token, robTag: validEntry.robTag, pRName: validEntry.dest};
            let newAddrIssueEntry = IssueEntry{issueType: validEntry.issueType, token: validEntry.token, robTag: validEntry.robTag,
                                               src1Ready: validEntry.src1Ready || isReady(validEntry.src1), src1: validEntry.src1,
                                               src2Ready: validEntry.src2Ready || isReady(validEntry.src2), src2: validEntry.src2,
                                               dest: validEntry.dest};
            if(isAllReady(newAddrIssueEntry))
                issueVals[4] <= tagged Valid addrExecEntry;
            else
                addrQ.write(memPtr, newAddrIssueEntry);
        end
        memPtr <= memPtr + 1;
    endrule

    rule issueFinish(issueState == Issue && intPtr == intCount && memPtr == memCount);
        issueState <= IssueFinal;
        dispatchState <= Dispatch;
        issueCount <= 0;
        dispatchCount <= 0;
    endrule

    rule issueFinishProceeding(issueState == IssueFinal);
        if(issueCount == fromInteger(valueOf(TSub#(NumFuncUnits,1))))
        begin
            memPort.send(issueVals[issueCount]);
            issueState <= IssueDone;
        end
        else
            execPort[issueCount].send(issueVals[issueCount]);
        if(isValid(issueVals[issueCount]))
        begin
            fpExeReq.send(tuple2((validValue(issueVals[issueCount])).token, ?));
        end
        issueCount <= issueCount + 1;
    endrule

    rule dispatch(dispatchState == Dispatch);
        let issue <- issuePort[dispatchCount].receive();
        if(isValid(issue))
        begin
            if(validValue(issue).issueType != Load && validValue(issue).issueType != Store)
                intQ.add(validValue(issue));
            else
                addrQ.add(validValue(issue));
        end
        dispatchCount <= dispatchCount + 1;
        if(dispatchCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
            dispatchState <= DispatchDone;
    endrule
endmodule
