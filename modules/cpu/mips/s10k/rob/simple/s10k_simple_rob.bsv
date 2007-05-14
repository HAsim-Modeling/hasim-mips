import hasim_isa::*;
import hasim_base::*;

import RegFile::*;

import s10k_simple_common::*;

typedef 32 ROBCount;

typedef struct {
    Token token;
    Addr addr;
    Bool done;
    Bool isBranch;
    Bool prediction;
    Bool taken;
    Bool isJR;
    Addr predAddr;
} ROBEntry deriving (Bits, Eq);

interface ROB;
    method Action readAnyReq(ROBTag robTag);
    method ActionValue#(ROBEntry) readAnyResp();
    method Action writeAny(ROBTag robTag, ROBEntry robEntry);
    method Action readHeadReq();
    method ActionValue#(ROBEntry) readHeadResp();
    method Action updateTail(ROBTag robTab);
    method Action writeTail(ROBEntry robEntry); //and increment
    method Action incrementHead();
    method ROBTag getTail();
    method Bool notFull();
    method Bool isROBTagValid(ROBTag robTag);
endinterface

module mkROB(ROB);
    RegFile#(ROBTag, ROBEntry) robFile <- mkRegFileFull();
    Reg#(ROBTag) head <- mkReg(0);
    Reg#(ROBTag) tail <- mkReg(0);

    Reg#(Bool) inc <- mkReg(False);

    Reg#(ROBTag) anyReg  <- mkReg(?);
    Reg#(ROBTag) headReg <- mkReg(?);

    let empty = head == tail && !inc;
    let full  = head == tail && inc;

    method Action readAnyReq(ROBTag robTag);
        anyReg <= robTag;    
    endmethod

    method ActionValue#(ROBEntry) readAnyResp();
        return robFile.sub(anyReg);
    endmethod

    method Action writeAny(ROBTag robTag, ROBEntry robEntry);
        robFile.upd(robTag, robEntry);
    endmethod

    method Action readHeadReq();
        headReg <= head;
    endmethod

    method ActionValue#(ROBEntry) readHeadResp();
        return robFile.sub(headReg);
    endmethod

    method Action updateTail(ROBTag robTab);
        tail <= robTab;
        inc  <= False;
    endmethod

    method Action writeTail(ROBEntry robEntry); //and increment
        robFile.upd(tail, robEntry);
        tail <= (tail + 1)%fromInteger(valueOf(ROBCount));
        inc  <= True;
    endmethod

    method Action incrementHead();
        head <= (head + 1)%fromInteger(valueOf(ROBCount));
        inc  <= False;
    endmethod

    method ROBTag getTail();
        return tail;
    endmethod

    method Bool notFull();
        return !full;
    endmethod

    method Bool isROBTagValid(ROBTag robTag);
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
