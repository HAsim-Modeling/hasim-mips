import hasim_isa::*;

interface BranchPred;
    method Action upd(Addr addr, Bool pred, Bool actual);
    method Maybe#(Addr) getPredAddr(Addr addr);
endinterface

module mkBranchPred
    //interface:
                (BranchPred);

    method Action upd(Addr addr, Bool pred, Bool actual);
        noAction;
    endmethod

    method Maybe#(Addr) getPredAddr(Addr addr);
        return Invalid;
    endmethod
endmodule
