import RegFile::*;

import hasim_cpu_types::*;

typedef Bit#(TLog#(TAdd#(qCount,1))) QCountType#(numeric type qCount);

interface IssueQ#(numeric type qCount);
    method QCountType#(qCount) getHead();
    method QCountType#(qCount) getTail();
    method QCountType#(qCount) getCount();
    method Maybe#(IssueEntry) read(QCountType#(qCount) idx);
    method Action write(QCountType#(qCount) idx, IssueEntry issue);
    method Action remove(QCountType#(qCount) idx);
    method Action add(IssueEntry issue);
endinterface

module mkIssueQ(IssueQ#(qCount))
    provisos(Add#(xxx, TLog#(qCount), TLog#(TAdd#(qCount,1))));

    Reg#(QCountType#(qCount))  head <- mkReg(0);
    Reg#(QCountType#(qCount)) count <- mkReg(0);

    RegFile#(Bit#(TLog#(qCount)), Maybe#(IssueEntry)) regFile <- mkRegFileFull();

    method QCountType#(qCount) getHead();
        return head;
    endmethod

    method QCountType#(qCount) getTail();
        return (head+count)%fromInteger(valueOf(qCount));
    endmethod

    method QCountType#(qCount) getCount();
        return count;
    endmethod

    method Maybe#(IssueEntry) read(QCountType#(qCount) idx);
        return regFile.sub(truncate(idx));
    endmethod

    method Action write(QCountType#(qCount) idx, IssueEntry issue);
        regFile.upd(truncate(idx), tagged Valid issue);
    endmethod

    method Action remove(QCountType#(qCount) idx);
        regFile.upd(truncate(idx), tagged Invalid);
        if(idx == head)
        begin
            head  <= (head + 1)%fromInteger(valueOf(qCount));
            count <= count - 1;
        end
    endmethod

    method Action add(IssueEntry issue);
        count <= count + 1;
        let idx = (head+count)%fromInteger(valueOf(qCount));
        regFile.upd(truncate(idx), tagged Valid issue);
    endmethod
endmodule

