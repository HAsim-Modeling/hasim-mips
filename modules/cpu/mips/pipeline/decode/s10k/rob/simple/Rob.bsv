import hasim_common::*;
import hasim_isa::*;

import RegFile::*;
import RWire::*;

import hasim_cpu_types::*;
import hasim_cpu_parameters::*;

interface Rob;
    method Bool isValidEntry(RobTag robTag);
    method Action write(RobTag robTag, RobEntry robEntry);
    method ActionValue#(Maybe#(RobEntry)) readHead();
    method Action incrementHead();
    method Action updateTail(RobTag robTab);
    method Action writeTail(RobEntry robEntry); //and increment
    method RobTag getTail();
    method Bool notFull();
endinterface

module mkRob(Rob);
    RegFile#(Bit#(TLog#(RobNum)), RobEntry) robFileDup <- mkRegFileFull();
    Reg#(RobTag) head <- mkReg(0);
    Reg#(RobTag) tail <- mkReg(0);

    Reg#(Bool)    inc <- mkReg(False);

    PulseWire incrementHeadEn <- mkPulseWire();
    PulseWire updateTailEn    <- mkPulseWire();

    let empty = head == tail && !inc;
    let full  = head == tail && inc;

    rule updateInc(incrementHeadEn || updateTailEn);
        inc <= False;
    endrule

    method Bool isValidEntry(RobTag robTag);
        return head < tail && robTag >= head && robTag < tail ||
               head > tail && (robTag >= head || robTag < tail) ||
               head == tail && !empty;
    endmethod

    method Action write(RobTag robTag, RobEntry robEntry);
        robFileDup.upd(truncate(robTag), robEntry);
    endmethod

    method Action incrementHead();
        head <= (head + 1)%fromInteger(valueOf(RobNum));
        incrementHeadEn.send();
    endmethod

    method ActionValue#(Maybe#(RobEntry)) readHead();
        if(!empty)
        begin
            RobEntry robEntryDup = robFileDup.sub(truncate(head));
            return tagged Valid robEntryDup;
        end
        else
            return tagged Invalid;
    endmethod

    method Action updateTail(RobTag robTab);
        tail <= (robTab + 1)%fromInteger(valueOf(RobNum));
        updateTailEn.send();
    endmethod

    method Action writeTail(RobEntry robEntry); //and increment
        robFileDup.upd(truncate(tail), robEntry);
        tail <= (tail + 1)%fromInteger(valueOf(RobNum));
        inc  <= True;
    endmethod

    method RobTag getTail();
        return tail;
    endmethod

    method Bool notFull();
        return !full;
    endmethod
endmodule
