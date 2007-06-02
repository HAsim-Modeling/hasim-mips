import RegFile::*;

import hasim_cpu_types::*;

typedef Bit#(TLog#(TAdd#(qCount,1))) QCountType#(numeric type qCount);

interface IssueQ#(numeric type qCount);
    method Action start();
    method Action readReq();
    method ActionValue#(Maybe#(IssueEntry)) readResp();
    method Action write(Maybe#(IssueEntry) issue);
    method Bool isLast();
    method Action add(IssueEntry issue);
    method QCountType#(qCount) getCount();
endinterface

module mkIssueQ(IssueQ#(qCount))
    provisos(Add#(positive, TLog#(qCount), TLog#(TAdd#(qCount,1))));

    Reg#(QCountType#(qCount))     head <- mkReg(0);
    Reg#(QCountType#(qCount))    count <- mkReg(0);
    Reg#(QCountType#(qCount))  readPtr <- mkReg(?);
    Reg#(QCountType#(qCount)) writePtr <- mkReg(?);

    RegFile#(Bit#(TLog#(qCount)), Maybe#(IssueEntry)) regFile <- mkRegFileFull();

    method Action start();
         readPtr <= head;
        writePtr <= head;
    endmethod

    method Action readReq();
        noAction;
    endmethod

    method ActionValue#(Maybe#(IssueEntry)) readResp();
        readPtr <= readPtr + 1;
        if(readPtr >= count)
            return tagged Invalid;
        else
            return regFile.sub(truncate((head + readPtr)%fromInteger(valueOf(qCount))));
    endmethod

    method Action write(Maybe#(IssueEntry) issue);
        regFile.upd(truncate((head + writePtr)%fromInteger(valueOf(qCount))), issue);
        if(!isValid(issue) && writePtr == 0)
        begin
            head     <= (head + 1)%fromInteger(valueOf(qCount));
            count    <= count - 1;
            readPtr  <= readPtr - 1;
        end
        else
            writePtr <= writePtr + 1;
    endmethod

    method Bool isLast();
        return writePtr == count - 1;
    endmethod

    method Action add(IssueEntry issue);
        count <= count + 1;
        regFile.upd(truncate((head + writePtr)%fromInteger(valueOf(qCount))), tagged Valid issue);
    endmethod

    method QCountType#(qCount) getCount();
        return count;
    endmethod
endmodule
