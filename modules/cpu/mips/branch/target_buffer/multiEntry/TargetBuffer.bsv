import hasim_isa::*;

import FIFO::*;
import FIFOF::*;

typedef 4 BufferSize;

module mkTargetBuffer#(Addr startAddr)(FIFOF#(Addr));
    FIFOF#(Addr) addrFifo <- mkSizedFIFOF(valueOf(BufferSize));

    //Really dumb buffer which drops the value if the fifo is full
    method Action enq(Addr _addr);
        if(addrFifo.notFull())
            addrFifo.enq(_addr);
    endmethod

    method Action deq();
        if(addrFifo.notEmpty())
            addrFifo.deq();
    endmethod

    method Addr first();
        if(addrFifo.notEmpty)
            return addrFifo.first();
        else
            return startAddr;
    endmethod

    method Action clear();
        addrFifo.clear();
    endmethod

    method Bool notEmpty();
        return addrFifo.notEmpty();
    endmethod

    method Bool notFull();
        return addrFifo.notFull();
    endmethod
endmodule
