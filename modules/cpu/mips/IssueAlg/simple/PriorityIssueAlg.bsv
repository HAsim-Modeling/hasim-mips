import hasim_isa::*;
import hasim_base::*;

import GetPut::*;
import Vector::*;

import hasim_cpu_parameters::*;
import hasim_cpu_types::*;
import hasim_cpu_unguardedIssueQ::*;

interface IssueAlg;
    method Action dispatchInt(IssueEntry issue);
    method Action dispatchMem(IssueEntry issue);
    method IntQCountType getIntQCount();
    method MemQCountType getMemQCount();
    method Action reqIssueVals();
    method Bool canIssue();
    interface Vector#(NumFuncUnits, Get#(Maybe#(ExecEntry))) respIssueVals;
endinterface

typedef enum {IntIssue, IntIssueDone} IntIssueState deriving (Bits, Eq);
typedef enum {MemIssue, MemIssueDone} MemIssueState deriving (Bits, Eq);

module mkIssueAlg(IssueAlg);
    Reg#(Vector#(NumFuncUnits, Maybe#(ExecEntry))) issueVals <- mkReg(replicate(tagged Invalid));
    IssueQ#(IntQCount)                                  intQ <- mkIssueQ();
    IssueQ#(MemQCount)                                  memQ <- mkIssueQ();

    Reg#(IntIssueState)                        intIssueState <- mkReg(IntIssueDone);
    Reg#(MemIssueState)                        memIssueState <- mkReg(MemIssueDone);

    Reg#(IntQCountType)                              intTail <- mkReg(?);
    Reg#(IntQCountType)                              intPtr  <- mkReg(?);

    Reg#(MemQCountType)                              memTail <- mkReg(?);
    Reg#(MemQCountType)                              memPtr  <- mkReg(?);

    Reg#(Maybe#(PRName))                             oldAlu1 <- mkReg(tagged Invalid);
    Reg#(Maybe#(PRName))                             oldAlu2 <- mkReg(tagged Invalid);
    Reg#(Maybe#(PRName))                             oldMem1 <- mkReg(tagged Invalid);
    Reg#(Maybe#(PRName))                             oldMem2 <- mkReg(tagged Invalid);

    Reg#(Maybe#(PRName))                             newAlu1 <- mkReg(?);
    Reg#(Maybe#(PRName))                             newAlu2 <- mkReg(?);
    Reg#(Maybe#(PRName))                             newMem1 <- mkReg(?);

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

    function isReady(PRName pRName) =  isValid(oldAlu1) && pRName == validValue(oldAlu1) || 
                                       isValid(oldAlu2) && pRName == validValue(oldAlu2) || 
                                       isValid(oldMem2) && pRName == validValue(oldMem2);
    function isAllReady(IssueEntry issue) = issue.src1Ready && issue.src2Ready;

    rule intIssueCollect(intIssueState == IntIssue);
        if(intPtr == intTail)
            intIssueState <= IntIssueDone;
        else
        begin
            let issueEntry = intQ.read(intPtr);
            $display("Int Issue index: %0d", intPtr);
            if(isValid(issueEntry))
            begin
                Bool removed = ?;
                let validEntry = validValue(issueEntry);
                let execEntry  = ExecEntry{token: validEntry.token, robTag: validEntry.robTag, pRName: validEntry.dest};
                let newIssueEntry = validEntry;
                newIssueEntry.src1Ready = validEntry.src1Ready || isReady(validEntry.src1);
                newIssueEntry.src2Ready = validEntry.src2Ready || isReady(validEntry.src2);
                $display("Check Int Issue: token: %0d, dest: %0d, src1: %b(%0d), src2: %b(%0d)", newIssueEntry.token.index, newIssueEntry.dest, newIssueEntry.src1Ready, newIssueEntry.src1, newIssueEntry.src2Ready, newIssueEntry.src2);
                if(isAllReady(newIssueEntry))
                begin
                    $display("Can issue int: token: %0d", validEntry.token.index);
                    Bit#(TLog#(NumFuncUnits)) index = case (validEntry.issueType) matches
                                                          JR     : 0;
                                                          Branch : 1;
                                                          Shift  : 1;
                                                          Normal : ((isValid(issueVals[2]))? 1: 2);
                                                          J      : 3;
                                                      endcase;
                    let aluOp = validEntry.issueType == Shift || validEntry.issueType == Normal;
                    if(!isValid(issueVals[index]))
                    begin
                        issueVals[index] <= tagged Valid execEntry;
                        intQ.remove(intPtr);
                        if(aluOp)
                        begin
                            if(index == 1)
                                newAlu1 <= tagged Valid validEntry.dest;
                            else if(index == 2)
                                newAlu2 <= tagged Valid validEntry.dest;
                        end
                    end
                    else
                        intQ.write(intPtr, newIssueEntry);
                end
            end
            else
                intQ.remove(intPtr);
            intPtr <= (intPtr + 1)%fromInteger(valueOf(IntQCount));
        end
    endrule

    rule memIssueCollect(memIssueState == MemIssue);
        memIssueState <= MemIssueDone;
        if(memPtr != memTail)
        begin
            let issueEntry = memQ.read(memPtr);
            $display("Mem Issue index: %0d", memPtr);
            if(isValid(issueEntry))
            begin
                Bool removed = ?;
                let validEntry = validValue(issueEntry);
                let execEntry  = ExecEntry{token: validEntry.token, robTag: validEntry.robTag, pRName: validEntry.dest};
                let newIssueEntry = validEntry;
                newIssueEntry.src1Ready = validEntry.src1Ready || isReady(validEntry.src1);
                newIssueEntry.src2Ready = validEntry.src2Ready || isReady(validEntry.src2);
                $display("Check Mem Issue: token: %0d, dest: %0d, src1: %b(%0d), src2: %b(%0d)", newIssueEntry.token.index, newIssueEntry.dest, newIssueEntry.src1Ready, newIssueEntry.src1, newIssueEntry.src2Ready, newIssueEntry.src2);
                if(isAllReady(newIssueEntry))
                begin
                    $display("Can issue mem: token: %0d", validEntry.token.index);
                    issueVals[4] <= tagged Valid execEntry;
                    memQ.remove(memPtr);
                    if(validEntry.issueType == Load)
                        newMem1 <= tagged Valid validEntry.dest;
                end
                else
                    memQ.write(memPtr, newIssueEntry);
            end
        end
    endrule

    method Action dispatch(IssueEntry issue);
        if(issue.issueType != Load && issue.issueType != Store)
            intQ.add(issue);
        else
            memQ.add(issue);
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

        intPtr        <= intQ.getHead();
        intTail       <= intQ.getTail();

        memPtr        <= memQ.getHead();
        memTail       <= memQ.getTail();

        oldAlu1       <= newAlu1;
        oldAlu2       <= newAlu2;
        oldMem1       <= newMem1;
        oldMem2       <= oldMem1;
    endmethod

    interface respIssueVals = respIssueValsLocal;
endmodule
