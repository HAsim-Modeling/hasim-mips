import hasim_isa::*;

interface BranchPred;
    method Action upd(Token token, Addr addr, Bool pred, Bool actual);
    method ActionValue#(Bool) getPred(Token token, Addr addr);
    method Action abort(Token token);
endinterface

module mkBranchPred(BranchPred);
    method Action upd(Token token, Addr addr, Bool pred, Bool actual);
        noAction;
    endmethod

    method ActionValue#(Bool) getPred(Token token, Addr addr);
        return False;
    endmethod

    method Action abort(Token token);
        noAction;
    endmethod
endmodule
