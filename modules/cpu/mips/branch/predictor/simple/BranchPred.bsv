import hasim_common::*;
import hasim_isa::*;

import RegFile::*;

typedef Bit#(`BRANCH_TABLE_SIZE) BranchIndex;

interface BranchPred;
    method Action upd(Token token, Addr addr, Bool pred, Bool actual);
    method ActionValue#(Bool) getPred(Token token, Addr addr);
    method Action abort(Token token);
endinterface

module mkBranchPred(BranchPred);
    RegFile#(BranchIndex, Bool) branchRegFile <- mkRegFileFull();

    method Action upd(Token token, Addr addr, Bool pred, Bool actual);
        branchRegFile.upd(truncate(addr), actual);
    endmethod

    method ActionValue#(Bool) getPred(Token token, Addr addr);
        return branchRegFile.sub(truncate(addr));
    endmethod

    method Action abort(Token token);
        noAction;
    endmethod
endmodule
