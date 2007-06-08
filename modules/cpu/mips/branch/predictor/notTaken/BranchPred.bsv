import hasim_isa::*;

interface BranchPred;
    method Action upd(Addr addr, Bool pred, Bool actual);
    method Bool getPred(Addr addr);
endinterface

module mkBranchPred(BranchPred);
    method Action upd(Addr addr, Bool pred, Bool actual);
        noAction;
    endmethod

    method Bool getPred(Addr addr);
        return False;
    endmethod
endmodule
