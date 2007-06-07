import hasim_common::*;

import RegFile::*;

import hasim_cpu_types::*;

typedef Bit#(TLog#(TAdd#(qCount,1))) QCountType#(numeric type qCount);

interface IssueQ#(numeric type qCount);
    method Action start();
    method Maybe#(IssueEntry) read();
    method Action write(Maybe#(IssueEntry) issue);
    method Bool isLast();
    method Action add(IssueEntry issue);
    method QCountType#(qCount) getCount();
endinterface

module mkIssueQ(IssueQ#(qCount))
    provisos(Add#(positive, TLog#(qCount), TLog#(TAdd#(qCount,1))));

    Reg#(QCountType#(qCount))  head <- mkReg(0);
    Reg#(QCountType#(qCount)) count <- mkReg(0);
    Reg#(QCountType#(qCount))   ptr <- mkReg(?);

    RegFile#(Bit#(TLog#(qCount)), Maybe#(IssueEntry)) regFile <- mkRegFileFull();

    method Action start();
         ptr <= 0;
    endmethod

    method Maybe#(IssueEntry) read();
        if(ptr >= count)
            return tagged Invalid;
        else
            return regFile.sub(truncate((head + ptr)%fromInteger(valueOf(qCount))));
    endmethod

    method Action write(Maybe#(IssueEntry) issue);
        regFile.upd(truncate((head + ptr)%fromInteger(valueOf(qCount))), issue);
        if(!isValid(issue) && ptr == 0)
        begin
            head     <= (head + 1)%fromInteger(valueOf(qCount));
            count    <= count - 1;
        end
        else
            ptr <= ptr + 1;
    endmethod

    method Bool isLast();
        return ptr == count;
    endmethod

    method Action add(IssueEntry issue);
        count <= count + 1;
        regFile.upd(truncate((head + count)%fromInteger(valueOf(qCount))), tagged Valid issue);
    endmethod

    method QCountType#(qCount) getCount();
        return count;
    endmethod
endmodule
