import hasim_isa::*;

import FIFOF::*;

module mkTargetBuffer#(Addr startAddr)
    //interface:
                (FIFOF#(Addr));

    Reg#(Addr) addr <- mkReg(startAddr);

    method Action enq(Addr _addr);
        addr <= _addr;
    endmethod

    method Action deq();
        noAction;
    endmethod

    method Addr first();
        return addr;
    endmethod

    method Action clear();
        noAction;
    endmethod

    method Bool notFull();
        return True;
    endmethod

    method Bool notEmpty();
        return True;
    endmethod
endmodule

