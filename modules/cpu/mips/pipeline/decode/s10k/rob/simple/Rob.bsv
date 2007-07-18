import hasim_common::*;
import hasim_isa::*;

import RegFile::*;
import RWire::*;

import hasim_cpu_types::*;
import hasim_cpu_parameters::*;

import CircularBuffer::*;

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
    //CircularBuffer#(TLog#(RobNum), RobEntry) robFile <- mkCircularBuffer(fromInteger(valueOf(TSub#(RobNum,1))));
    RegFile#(Bit#(TLog#(RobNum)), RobEntry) robFileDup <- mkRegFileFull();
    Reg#(RobTag) head <- mkReg(0);
    Reg#(RobTag) tail <- mkReg(0);

    //RobTag head = robFile.getHead();

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
        //if(head != headDup)
            //$display("Screwed %0d %0d", head, headDup);
        //robFile.write(robTag, robEntry);
        robFileDup.upd(truncate(robTag), robEntry);
    endmethod

    method Action incrementHead();
        //if(head != headDup)
            //$display("Screwed %0d %0d", head, headDup);
        //robFile.incrementHead();
        head <= (head + 1)%fromInteger(valueOf(RobNum));
        incrementHeadEn.send();
    endmethod

    method ActionValue#(Maybe#(RobEntry)) readHead();
        //if(head != headDup)
            //$display("Screwed %0d %0d", head, headDup);
        if(!empty)
        begin
            RobEntry robEntryDup = robFileDup.sub(truncate(head));
            //RobEntry robEntry <- robFile.readHead();
            //if(robEntryDup != robEntry)
                //$display("ROB ENTRY SCREWED %0d %0d", head, headDup);
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
        //if(head != headDup)
            //$display("Screwed %0d %0d", head, headDup);
        //robFile.write(tail, robEntry);
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
