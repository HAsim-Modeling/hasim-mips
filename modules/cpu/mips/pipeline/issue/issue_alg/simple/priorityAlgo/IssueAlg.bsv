import hasim_common::*;
import hasim_isa::*;

import GetPut::*;
import Vector::*;

import hasim_cpu_parameters::*;
import hasim_cpu_types::*;
import hasim_issueQ::*;

interface IssueAlg;
    method Bool doneKill();
    method Action killInitialize(Token token);
    method ActionValue#(Maybe#(Token)) killNext();
    method Action dispatch(IssueEntry issue);
    method IntQCountType getIntQCount();
    method MemQCountType getMemQCount();
    method Action reqIssueVals();
    method Bool canIssue();
    interface Vector#(NumFuncUnits, Get#(Maybe#(ExecEntry))) respIssueVals;
endinterface

typedef enum {Kill, KillDone} KillState deriving (Bits, Eq);
typedef enum {IntIssue, IntIssueDone} IntIssueState deriving (Bits, Eq);
typedef enum {MemIssue, MemIssueDone} MemIssueState deriving (Bits, Eq);

typedef enum {Dispatched, Issued, Free} BusyState deriving (Bits, Eq);

module mkIssueAlg(IssueAlg);
    Vector#(NumFuncUnits, Reg#(Maybe#(ExecEntry))) issueVals <- replicateM(mkReg(tagged Invalid));
    IssueQ#(IntQCount)                                  intQ <- mkIssueQ();
    IssueQ#(MemQCount)                                  memQ <- mkIssueQ();

    Reg#(IntIssueState)                        intIssueState <- mkReg(IntIssueDone);
    Reg#(MemIssueState)                        memIssueState <- mkReg(MemIssueDone);

    Reg#(Vector#(PRNum, BusyState))                busyState <- mkReg(replicate(Free));
    Reg#(Vector#(PRNum, Bit#(32)))               regWaitTime <- mkReg(replicate(0));

    Reg#(Token)                                    killToken <- mkReg(?);
    Reg#(KillState)                                killState <- mkReg(KillDone);

    Vector#(NumFuncUnits, Get#(Maybe#(ExecEntry))) respIssueValsLocal = newVector();

    for(Integer i = 0; i < valueOf(NumFuncUnits); i=i+1)
    begin
        respIssueValsLocal[i] = (interface Get#(Maybe#(ExecEntry));
                                      method ActionValue#(Maybe#(ExecEntry)) get();
                                          issueVals[i] <= tagged Invalid;
                                          return issueVals[i];
                                      endmethod
                                  endinterface);   
    end

    function isReady(PRName pRName) = busyState[pRName] == Free;

    function isAllReady(IssueEntry issue) = issue.src1Ready && issue.src2Ready;

    function ExecEntry getExecEntry(IssueEntry issue);
        return ExecEntry{token: issue.token, robTag: issue.robTag, freeCount: issue.freeCount,
                         pRName: issue.dest, issueType: issue.issueType, branchIndex: issue.branchIndex, pred: issue.pred, predAddr: issue.predAddr};
    endfunction

    function IssueEntry getNewIssueEntry(IssueEntry issue);
        let newEntry = issue;
        newEntry.src1Ready = issue.src1Ready || isReady(issue.src1);
        newEntry.src2Ready = issue.src2Ready || isReady(issue.src2);
        return newEntry;
    endfunction

    rule intIssueCollect(intIssueState == IntIssue);
        if(intQ.isLast())
            intIssueState <= IntIssueDone;
        else
        begin
            let issueEntry   <- intQ.readResp();
            let validEntry    = validValue(issueEntry);
            let execEntry     = getExecEntry(validEntry);
            let newIssueEntry = getNewIssueEntry(validEntry);
            let newWriteEntry = ?;

            if(isValid(issueEntry))
            begin
                if(isAllReady(newIssueEntry))
                begin
                    Bit#(TLog#(NumFuncUnits)) index = case (validEntry.issueType) matches
                                                          J      : 3;
                                                          JAL    : 3;
                                                          JR     : 0;
                                                          JALR   : 0;
                                                          Branch : 1;
                                                          Shift  : 1;
                                                          Normal : ((isValid(issueVals[2]))? 1: 2);
                                                      endcase;
                    let aluOp = validEntry.issueType == Shift || validEntry.issueType == Normal;
                    $display("IssueCrap: %0d %0d %0d", validEntry.token.index, index, isValid(issueVals[index]));
                    if(!isValid(issueVals[index]))
                    begin
                        issueVals[index] <= tagged Valid execEntry;
                        newWriteEntry     = tagged Invalid;

                        if(validEntry.issueType == JAL || validEntry.issueType == JALR || validEntry.issueType == Normal || validEntry.issueType == Shift)
                        begin
                            if(validEntry.dest != 0)
                                busyState[validEntry.dest] <= Issued;
                            regWaitTime[validEntry.dest] <= 0;
                        end
                    end
                    else
                        newWriteEntry = tagged Valid newIssueEntry;
                end
                else
                begin
                    newWriteEntry = tagged Valid newIssueEntry;
                    $display("NotIssueCrap: %0d", validEntry.token.index);
                end
            end
            else
                newWriteEntry = tagged Invalid;
            intQ.write(newWriteEntry);
        end
    endrule

    rule memIssueCollect(memIssueState == MemIssue);
        memIssueState <= MemIssueDone;
        if(!memQ.isLast())
        begin
            let issueEntry   <- memQ.readResp();
            let validEntry    = validValue(issueEntry);
            let execEntry     = getExecEntry(validEntry);
            let newIssueEntry = getNewIssueEntry(validEntry);
            let newWriteEntry = ?;
            if(isValid(issueEntry))
            begin
                if(isAllReady(newIssueEntry))
                begin
                    issueVals[4] <= tagged Valid execEntry;
                    newWriteEntry = tagged Invalid;
                    if(validEntry.issueType == Load)
                    begin
                        if(validEntry.dest != 0)
                            busyState[validEntry.dest] <= Issued;
                        regWaitTime[validEntry.dest] <= 1;
                    end
                end
                else
                    newWriteEntry = tagged Valid newIssueEntry;
            end
            else
                newWriteEntry = tagged Invalid;
            memQ.write(newWriteEntry);
        end
    endrule

    method Action killInitialize(Token token);
        killState <= Kill;
        killToken <= token;
        $display("killtoken in issue :%0d", token.index);
        intQ.start();
        memQ.start();
    endmethod

    method ActionValue#(Maybe#(Token)) killNext() if(killState == Kill);
        Maybe#(Token) returnVal = tagged Invalid;
        if(!intQ.isLast())
        begin
            let issueEntry   <- intQ.readResp();
            case (issueEntry) matches
                tagged Valid .validEntry:
                begin
                    TokIndex diff = validEntry.token.index - killToken.index;
                    if(diff[7] == 0)
                    begin
                        $display("killed token: %0d", validEntry.token.index);
                        busyState[validEntry.dest] <= Free;
                        regWaitTime[validEntry.dest] <= 0;
                        intQ.write(tagged Invalid);
                        returnVal = tagged Valid validEntry.token;
                    end
                    else
                    begin
                        $display("not killed token: %0d", validEntry.token.index);
                        intQ.write(issueEntry);
                        returnVal = tagged Invalid;
                    end
                end
                tagged Invalid:
                begin
                    intQ.write(tagged Invalid);
                    returnVal = tagged Invalid;
                end
            endcase
        end
        else if(!memQ.isLast())
        begin
            let issueEntry   <- memQ.readResp();
            case (issueEntry) matches
                tagged Valid .validEntry:
                begin
                    TokIndex diff = validEntry.token.index - killToken.index;
                    if(diff[7] == 0)
                    begin
                        $display("killed token: %0d", validEntry.token.index);
                        busyState[validEntry.dest] <= Free;
                        regWaitTime[validEntry.dest] <= 0;
                        memQ.write(tagged Invalid);
                        returnVal = tagged Valid validEntry.token;
                    end
                    else
                    begin
                        $display("not killed token: %0d", validEntry.token.index);
                        memQ.write(issueEntry);
                        returnVal = tagged Invalid;
                    end
                end
                tagged Invalid:
                begin
                    memQ.write(tagged Invalid);
                    returnVal = tagged Invalid;
                end
            endcase
        end
        else
            killState <= KillDone;
        return returnVal;
    endmethod

    method Bool doneKill();
        return killState == KillDone;
    endmethod

    method Action dispatch(IssueEntry issue);
        if(issue.issueType == J || issue.issueType == JAL)
            issueVals[3] <= tagged Valid getExecEntry(issue);
        else
        begin
            if(issue.dest != 0)
                busyState[issue.dest] <= Dispatched;
            if(issue.issueType != Load && issue.issueType != Store)
                intQ.add(issue);
            else
            begin
                $display("token: %0d type: %0d, src1: %0d, src1Ready: %0d, src2: %0d, src2Ready: %0d", issue.token.index, issue.issueType, issue.src1, issue.src1Ready, issue.src2, issue.src2Ready);
                memQ.add(issue);
            end
        end
    endmethod

    method IntQCountType getIntQCount();
        return intQ.getCount();
    endmethod

    method MemQCountType getMemQCount();
        return memQ.getCount();
    endmethod

    method Bool canIssue();
        return intIssueState == IntIssueDone && memIssueState == MemIssueDone;
    endmethod

    method Action reqIssueVals();
        intIssueState <= IntIssue;
        memIssueState <= MemIssue;

        intQ.start();
        memQ.start();

        Vector#(PRNum, BusyState) newBusyState = newVector();
        Vector#(PRNum, Bit#(32)) newRegWaitTime = newVector();

        for(Integer i = 0; i < valueOf(PRNum); i=i+1)
        begin
            if(busyState[i] == Issued)
            begin
                if(regWaitTime[i] == 0)
                begin
                    newBusyState[i] = Free;
                    newRegWaitTime[i] = 0;
                end
                else
                begin
                    newBusyState[i] = Issued;
                    newRegWaitTime[i] = regWaitTime[i]-1;
                end
            end
            else if(busyState[i] == Dispatched)
            begin
                newBusyState[i] = Dispatched;
                newRegWaitTime[i] = 0;
            end
            else
            begin
                newBusyState[i] = Free;
                newRegWaitTime[i] = 0;
            end
        end

        busyState   <= newBusyState;
        regWaitTime <= newRegWaitTime;
    endmethod

    interface respIssueVals = respIssueValsLocal;
endmodule
