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
    method Action upd(Token token, Addr addr, Bool pred, Bool actual);
        noAction;
    endmethod

    method Action  getPredReq(Token token, Addr addr);
        noAction;
    endmethod

    method ActionValue#(Bool) getPredResp();
        return False;
    endmethod
    
    method Action abort(Token token);
        noAction;
    endmethod
endmodule
