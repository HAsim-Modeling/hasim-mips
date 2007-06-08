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
    Bool result;
    Bool done;
    Bool isBranch;
    Bool prediction;
    Bool taken;
    Bool isJR;
    Addr predAddr;
} RobEntry deriving (Bits, Eq);

interface ROB;
    method ActionValue#(Maybe#(RobEntry)) readHead();
    method Maybe#(RobEntry) read(RobTag robTag);
    method Action write(RobTag robTag, RobEntry robEntry);
    method Action updateTail(RobTag robTab);
    method Action writeTail(RobEntry robEntry); //and increment
    method RobTag getTail();
    method Bool notFull();
endinterface

module mkROB
    //interface:
                (ROB);
		
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

    method Maybe#(RobEntry) read(RobTag robTag);
        let valid = head < tail && robTag >= head && robTag < tail ||
                    head > tail && (robTag >= head || robTag < tail) ||
                    full;
        if(valid)
            return tagged Valid robFile.sub(truncate(robTag));
        else
            return tagged Invalid;
    endmethod

    method Action write(RobTag robTag, RobEntry robEntry);
        robFile.upd(truncate(robTag), robEntry);
    endmethod

    method ActionValue#(Maybe#(RobEntry)) readHead();
        if(!empty)
        begin
            head <= (head + 1)%fromInteger(valueOf(RobCount));
            incrementHeadEn.send();
            return tagged Valid robFile.sub(truncate(head));
        end
        else
            return tagged Invalid;
    endmethod

    method Action updateTail(RobTag robTab);
        tail <= robTab;
        updateTailEn.send();
    endmethod

    method Action writeTail(RobEntry robEntry);
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
endmodule
