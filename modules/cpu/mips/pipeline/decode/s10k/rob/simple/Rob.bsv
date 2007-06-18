import hasim_common::*;
import hasim_isa::*;

import RegFile::*;
import RWire::*;

import hasim_cpu_types::*;
import hasim_cpu_parameters::*;

typedef struct {
    Token token;
    Addr addr;
    Bool finished;
    Bool status;
    Bool done;
    BranchStackIndex branchIndex;
    Bool isBranch;
    Bool prediction;
    Bool taken;
    Bool isJR;
    Addr predAddr;
} RobEntry deriving (Bits, Eq);

interface Rob;
    method Maybe#(RobEntry) read(RobTag robTag);
    method Action write(RobTag robTag, RobEntry robEntry);
    method Maybe#(RobEntry) readHead();
    method Action incrementHead();
    method Action updateTail(RobTag robTab);
    method Action writeTail(RobEntry robEntry); //and increment
    method RobTag getTail();
    method Bool notFull();
endinterface

module mkRob(Rob);
    RegFile#(Bit#(TLog#(RobCount)), RobEntry) robFile <- mkRegFileFull();
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

    method Maybe#(RobEntry) read(RobTag robTag);
        let valid = head < tail && robTag >= head && robTag < tail ||
                    head > tail && (robTag >= head || robTag < tail) ||
                    !empty;
        if(valid)
            return tagged Valid robFile.sub(truncate(robTag));
        else
            return tagged Invalid;
    endmethod

    method Action write(RobTag robTag, RobEntry robEntry);
        robFile.upd(truncate(robTag), robEntry);
    endmethod

    method Action incrementHead();
        head <= (head + 1)%fromInteger(valueOf(RobCount));
        incrementHeadEn.send();
    endmethod

    method Maybe#(RobEntry) readHead();
        if(!empty)
            return tagged Valid(robFile.sub(truncate(head)));
        else
            return tagged Invalid;
    endmethod

    method Action updateTail(RobTag robTab);
        tail <= (robTab + 1)%fromInteger(valueOf(RobCount));
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
endmodule
