import hasim_isa::*;

interface BranchPred;
    method Action upd(Addr addr, Bool pred, Bool actual);
    method Bool getPredAddr(Addr addr);
endinterface

module mkBranchPred
    //interface:
                (BranchPred);

    method Action upd(Addr addr, Bool pred, Bool actual);
        noAction;
    endmethod

    method Bool getPredAddr(Addr addr);
        return False;
    endmethod
endmodule
