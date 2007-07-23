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
    method IntQCount getIntQCount();
    method MemQCount getMemQCount();
    method Action reqIssueVals();
    method Bool canIssue();
    method FreeListCount getFreeListAdd();
    interface Vector#(FuncUnitNum, Get#(Maybe#(ExecEntry))) respIssueVals;
endinterface

typedef enum {Kill, KillDone} KillState deriving (Bits, Eq);
typedef enum {IntIssue, IntIssueDone} IntIssueState deriving (Bits, Eq);
typedef enum {MemIssue, MemIssueDone} MemIssueState deriving (Bits, Eq);
typedef enum {Dispatched, Issued, Free} BusyState deriving (Bits, Eq);

/* Description of module:
 * It has two issue queues: intQ and memQ
 * When killInitialize is called, it changes its state to a Kill state, in which whenever killNext is called,
 * it issues the next instruction to be killed every cycle in the queues (or invalid of the instruction scanned at that cycle is not to be killed)
 * It first scans intQ and then memQ
 * After it ends scnaning, doneKill() returns True
 * reqIssueVals is the triggering step for collecting instructions to be issued to the EXE unit (in timing model)
 * Basically it changes state of intIssueState and memIssueState to IntIssue and MemIssue respectively.
 * Now in these states, the rules intIssueCollect and memIssueCollect fire and fill up the issueBuffer, after which it 
 * returns canIssue() as True
 */
module mkIssueAlg(IssueAlg);
    Vector#(FuncUnitNum, Reg#(Maybe#(ExecEntry))) issueVals <- replicateM(mkReg(tagged Invalid));
    IssueQ#(IntQNum)                                   intQ <- mkIssueQ();
    IssueQ#(MemQNum)                                   memQ <- mkIssueQ();

    Reg#(IntIssueState)                       intIssueState <- mkReg(IntIssueDone);
    Reg#(MemIssueState)                       memIssueState <- mkReg(MemIssueDone);

    Reg#(Vector#(PRNum, BusyState))               busyState <- mkReg(replicate(Free));
    Reg#(Vector#(PRNum, Bit#(32)))              regWaitTime <- mkReg(replicate(0));

    Reg#(FreeListCount)                       freeListCount <- mkReg(0);

    Reg#(Token)                                   killToken <- mkReg(?);
    Reg#(KillState)                               killState <- mkReg(KillDone);

    Vector#(FuncUnitNum, Get#(Maybe#(ExecEntry))) respIssueValsLocal = newVector();

    for(Integer i = 0; i < valueOf(FuncUnitNum); i=i+1)
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
        return ExecEntry{token: issue.token, addr: issue.addr, robTag: issue.robTag,
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
                    Bit#(TLog#(FuncUnitNum)) index = case (validEntry.issueType) matches
                                                          J      : 3;
                                                          JAL    : 3;
                                                          JR     : 0;
                                                          JALR   : 0;
                                                          Branch : 1;
                                                          Shift  : 1;
                                                          Finish : ((isValid(issueVals[2]))? 1: 2); 
                                                          Normal : ((isValid(issueVals[2]))? 1: 2);
                                                      endcase;
                    let aluOp = validEntry.issueType == Shift || validEntry.issueType == Normal || validEntry.issueType == Finish;
                    if(!isValid(issueVals[index]))
                    begin
                        issueVals[index] <= tagged Valid execEntry;
                        newWriteEntry     = tagged Invalid;

                        if(validEntry.issueType == JAL || validEntry.issueType == JALR || validEntry.issueType == Normal || validEntry.issueType == Shift || validEntry.issueType == Finish)
                        begin
                            if(validEntry.dest != 0)
                            begin
                                busyState[validEntry.dest] <= Issued;
                            end
                            regWaitTime[validEntry.dest] <= 0;
                        end
                    end
                    else
                        newWriteEntry = tagged Valid newIssueEntry;
                end
                else
                    newWriteEntry = tagged Valid newIssueEntry;
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
        freeListCount <= 0;
        intQ.start();
        memQ.start();
    endmethod

    method ActionValue#(Maybe#(Token)) killNext() if(killState == Kill);
        function ActionValue#(Maybe#(Token)) killQ(IssueQ#(qCount) issueQ)
            provisos(Add#(positive, TLog#(qCount), TLog#(TAdd#(qCount,1))));
        actionvalue
            let issueEntry   <- issueQ.readResp();
            case (issueEntry) matches
                tagged Valid .validEntry:
                begin
                    TokIndex diff = validEntry.token.index - killToken.index;
                    if(diff[7] == 0)
                    begin
                        busyState[validEntry.dest] <= Free;
                        regWaitTime[validEntry.dest] <= 0;
                        issueQ.write(tagged Invalid);
                        freeListCount <= freeListCount + 1;
                        return tagged Valid validEntry.token;
                    end
                    else
                    begin
                        issueQ.write(issueEntry);
                        return tagged Invalid;
                    end
                end
                tagged Invalid:
                begin
                    issueQ.write(tagged Invalid);
                    return tagged Invalid;
                end
            endcase
        endactionvalue
        endfunction

        if(!intQ.isLast())
        begin
            Maybe#(Token) retVal <- killQ(intQ);
            return retVal;
        end
        else if(!memQ.isLast())
        begin
            Maybe#(Token) retVal <- killQ(memQ);
            return retVal;
        end
        else
        begin
            killState <= KillDone;
            return tagged Invalid;
        end
    endmethod

    method FreeListCount getFreeListAdd();
        return freeListCount;
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
                memQ.add(issue);
        end
    endmethod

    method IntQCount getIntQCount();
        return intQ.getCount();
    endmethod

    method MemQCount getMemQCount();
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
