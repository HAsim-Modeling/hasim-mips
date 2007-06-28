import FIFO::*;

import hasim_common::*;
import hasim_isa::*;

interface BranchPred;
    method Action upd(Token token, Addr addr, Bool pred, Bool actual);
    method Action getPredReq(Token token, Addr addr);
    method ActionValue#(Bool) getPredResp();
    method Action abort(Token token);
endinterface

module mkBranchPred(BranchPred);
    
    FIFO#(Bit#(0)) q <- mkFIFO();

    method Action upd(Token token, Addr addr, Bool pred, Bool actual);
        noAction;
    endmethod

    method Action  getPredReq(Token token, Addr addr);
        q.enq(?);
    endmethod

    method ActionValue#(Bool) getPredResp();
        q.deq();
        return False;
    endmethod
    
    method Action abort(Token token);
        noAction;
    endmethod
endmodule
