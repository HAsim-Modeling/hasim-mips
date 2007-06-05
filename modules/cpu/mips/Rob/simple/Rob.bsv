import hasim_isa::*;
import hasim_base::*;

import RegFile::*;
import RWire::*;

import hasim_cpu_types::*;
import hasim_cpu_parameters::*;

typedef struct {
    Token token;
    Addr addr;
    Bool finished;
    Bool done;
    Bool isBranch;
    Bool prediction;
    Bool taken;
    Bool isJR;
    Addr predAddr;
} RobEntry deriving (Bits, Eq);

interface Rob;
    method Action readAnyReq(RobTag robTag);
    method ActionValue#(RobEntry) readAnyResp();
    method Action writeAny(RobTag robTag, RobEntry robEntry);
    method Action readHeadReq();
    method ActionValue#(Maybe#(RobEntry)) readHeadResp();
    method Action updateTail(RobTag robTab);
    method Action writeTail(RobEntry robEntry); //and increment
    method Action incrementHead();
    method RobTag getTail();
    method Bool notFull();
    method Bool isRobTagValid(RobTag robTag);
endinterface

module mkRob(Rob);
    RegFile#(Bit#(TLog#(RobCount)), RobEntry) robFile <- mkRegFileFull();
    Reg#(RobTag) head <- mkReg(0);
    Reg#(RobTag) tail <- mkReg(0);

    Reg#(Bool) inc <- mkReg(False);

    PulseWire incrementHeadEn <- mkPulseWire();
    PulseWire updateTailEn    <- mkPulseWire();

    Reg#(RobTag) anyReg  <- mkReg(?);
    Reg#(RobTag) headReg <- mkReg(?);

    let empty = head == tail && !inc;
    let full  = head == tail && inc;

    rule updateInc(incrementHeadEn || updateTailEn);
        inc <= False;
    endrule

    method Action readAnyReq(RobTag robTag);
        anyReg <= robTag;    
    endmethod

    method ActionValue#(RobEntry) readAnyResp();
        return robFile.sub(truncate(anyReg));
    endmethod

    method Action writeAny(RobTag robTag, RobEntry robEntry);
        robFile.upd(truncate(robTag), robEntry);
    endmethod

    method Action readHeadReq();
        headReg <= head;
    endmethod

    method ActionValue#(Maybe#(RobEntry)) readHeadResp();
        $display("ROB read: head: %0d %0b", head, empty);
        if(!empty)
            return tagged Valid(robFile.sub(truncate(headReg)));
        else
            return tagged Invalid;
    endmethod

    method Action updateTail(RobTag robTab);
        tail <= robTab;
        updateTailEn.send();
    endmethod

    method Action writeTail(RobEntry robEntry); //and increment
        robFile.upd(truncate(tail), robEntry);
        tail <= (tail + 1)%fromInteger(valueOf(RobCount));
        inc  <= True;
    endmethod

    method Action incrementHead();
        if(!empty)
        begin
            incrementHeadEn.send();
            head <= (head + 1)%fromInteger(valueOf(RobCount));
        end
        $display("ROB inc: head: %0d %0b", head, empty);
    endmethod

    method RobTag getTail();
        return tail;
    endmethod

    method Bool notFull();
        return !full;
    endmethod

    method Bool isRobTagValid(RobTag robTag);
        if(head < tail)
        begin
            return robTag >= head && robTag < tail;
        end
        else
        begin
            return robTag >= head || robTag < tail;
        end
    endmethod
endmodule
