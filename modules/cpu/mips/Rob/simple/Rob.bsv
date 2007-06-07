import hasim_isa::*;
import hasim_common::*;

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
    method Action readHeadReq(Bool increment);
    method ActionValue#(Maybe#(RobEntry)) readHeadResp();
    method Action updateTail(RobTag robTab);
    method Action writeTail(RobEntry robEntry); //and increment
    method RobTag getTail();
    method Bool notFull();
    method ActionValue#(Bool) isRobTagValid(RobTag robTag);
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

    Reg#(Bool) emptyReg  <- mkReg(?);

    let empty = head == tail && !inc;
    let full  = head == tail && inc;

    let newHead = (head+1)%fromInteger(valueOf(RobCount));

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

    method Action readHeadReq(Bool increment);
        if(!empty && increment)
        begin
            incrementHeadEn.send();
            head <= newHead;
        end
        headReg  <= increment? newHead: head;
        emptyReg <= empty;
    endmethod

    method ActionValue#(Maybe#(RobEntry)) readHeadResp();
        $display("ROB read: head: %0d %0b", head, emptyReg);
        if(!emptyReg)
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

    method RobTag getTail();
        return tail;
    endmethod

    method Bool notFull();
        return !full;
    endmethod

    method ActionValue#(Bool) isRobTagValid(RobTag robTag);
        $display("TagValid: %0d %0d %0d", head, tail, robTag);
        if(head < tail)
            return robTag >= head && robTag < tail;
        else if(head > tail)
            return robTag >= head || robTag < tail;
        else if(empty)
            return False;
        else
            return True;
    endmethod
endmodule
