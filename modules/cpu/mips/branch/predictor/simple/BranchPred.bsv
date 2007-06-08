import hasim_isa::*;

import RegFile::*;

typedef Bit#(5) BranchIndex;

interface BranchPred;
    method Action upd(Addr addr, Bool pred, Bool actual);
    method Bool getPredAddr(Addr addr);
endinterface

module mkBranchPred(BranchPred);
    RegFile#(BranchIndex, Bool) branchRegFile <- mkRegFileFull();

    method Action upd(Addr addr, Bool pred, Bool actual);
        branchRegFile.upd(truncate(addr), actual);
    endmethod

    method Bool getPredAddr(Addr addr);
        return branchRegFile.sub(truncate(addr));
    endmethod
endmodule
