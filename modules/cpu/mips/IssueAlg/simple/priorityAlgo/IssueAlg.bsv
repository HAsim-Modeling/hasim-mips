import hasim_isa::*;
import hasim_base::*;

import GetPut::*;
import Vector::*;

import hasim_cpu_parameters::*;
import hasim_cpu_types::*;
import hasim_cpu_issueQ::*;

interface IssueAlg;
    method Action dispatch(IssueEntry issue);
    method IntQCountType getIntQCount();
    method MemQCountType getMemQCount();
    method Action reqIssueVals();
    method Bool canIssue();
    interface Vector#(NumFuncUnits, Get#(Maybe#(ExecEntry))) respIssueVals;
endinterface

typedef enum {IntIssue, IntIssueDone} IntIssueState deriving (Bits, Eq);
typedef enum {MemIssue, MemIssueDone} MemIssueState deriving (Bits, Eq);

module mkIssueAlg(IssueAlg);
    Vector#(NumFuncUnits, Reg#(Maybe#(ExecEntry))) issueVals <- replicateM(mkReg(tagged Invalid));
    IssueQ#(IntQCount)                                  intQ <- mkIssueQ();
    IssueQ#(MemQCount)                                  memQ <- mkIssueQ();

    Reg#(IntIssueState)                        intIssueState <- mkReg(IntIssueDone);
    Reg#(MemIssueState)                        memIssueState <- mkReg(MemIssueDone);

    Reg#(Maybe#(PRName))                             oldAlu1 <- mkReg(tagged Invalid);
    Reg#(Maybe#(PRName))                             oldAlu2 <- mkReg(tagged Invalid);
    Reg#(Maybe#(PRName))                             oldJAL  <- mkReg(tagged Invalid);
    Reg#(Maybe#(PRName))                             oldJALR <- mkReg(tagged Invalid);
    Reg#(Maybe#(PRName))                             oldMem1 <- mkReg(tagged Invalid);
    Reg#(Maybe#(PRName))                             oldMem2 <- mkReg(tagged Invalid);

    Reg#(Maybe#(PRName))                             newAlu1 <- mkReg(?);
    Reg#(Maybe#(PRName))                             newAlu2 <- mkReg(?);
    Reg#(Maybe#(PRName))                             newMem1 <- mkReg(?);
    Reg#(Maybe#(PRName))                             newJAL  <- mkReg(?);
    Reg#(Maybe#(PRName))                             newJALR <- mkReg(?);

    Reg#(ClockCounter)                          modelCounter <- mkReg(0);

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
                                       isValid(oldJAL)  && pRName == validValue(oldJAL)  ||
                                       isValid(oldJALR) && pRName == validValue(oldJALR) ||
                                       isValid(oldMem2) && pRName == validValue(oldMem2);
    function isAllReady(IssueEntry issue) = issue.src1Ready && issue.src2Ready;

    function ExecEntry getExecEntry(IssueEntry issue);
        return ExecEntry{token: issue.token, robTag: issue.robTag, pRName: issue.dest};
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
                $display("Issue: present Token: %0d @ Model: %0d", validEntry.token.index, modelCounter-1);
                if(isAllReady(newIssueEntry))
                begin
                    $display("Issue: Source Ready Token: %0d", validEntry.token.index);
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
                    if(!isValid(issueVals[index]))
                    begin
                        $display("Issue: Issued Token: %0d", validEntry.token.index);
                        issueVals[index] <= tagged Valid execEntry;
                        newWriteEntry     = tagged Invalid;
                        if(aluOp)
                        begin
                            if(index == 0)
                                newJALR <= tagged Valid validEntry.dest;
                            else if(index == 1)
                                newAlu1 <= tagged Valid validEntry.dest;
                            else if(index == 2)
                                newAlu2 <= tagged Valid validEntry.dest;
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
                        newMem1 <= tagged Valid validEntry.dest;
                end
                else
                    newWriteEntry = tagged Valid newIssueEntry;
            end
            else
                newWriteEntry = tagged Invalid;
        end
    endrule

    method Action dispatch(IssueEntry issue);
        if(issue.issueType == J || issue.issueType == JAL)
        begin
            issueVals[3] <= tagged Valid getExecEntry(issue);
            if(issue.issueType == JALR)
                newJAL   <= tagged Valid issue.dest;
        end
        else if(issue.issueType != Load && issue.issueType != Store)
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

        intQ.start();
        memQ.start();

        oldAlu1       <= newAlu1;
        oldAlu2       <= newAlu2;
        oldJAL        <= newJAL;
        oldJALR       <= newJALR;
        oldMem1       <= newMem1;
        oldMem2       <= oldMem1;

        newAlu1       <= tagged Invalid;
        newAlu2       <= tagged Invalid;
        newJAL        <= tagged Invalid;
        newJALR       <= tagged Invalid;
        newMem1       <= tagged Invalid;

        modelCounter  <= modelCounter + 1;
    endmethod

    interface respIssueVals = respIssueValsLocal;
endmodule
