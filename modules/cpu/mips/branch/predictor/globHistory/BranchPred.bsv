import hasim_common::*;
import hasim_isa::*;

import RegFile::*;

typedef Bit#(`GLOBAL_HIST_SIZE) GlobalHist;
typedef Bit#(TAdd#(`GLOBAL_HIST_SIZE,`BRANCH_TABLE_SIZE)) BranchTableIndex;

interface BranchPred;
    method Action upd(Token token, Addr addr, Bool pred, Bool actual);
    method ActionValue#(Bool) getPred(Token token, Addr addr);
    method Action abort(Token token);
endinterface

module mkBranchPred(BranchPred);
    RegFile#(GlobalHist) globalHist <- mkReg(0);
    RegFile#(BranchTableIndex, Bool) branchTable <- mkRegFileFull();
    RegFile#(Token, globalHist) screenShot <- mkRegFileFull();

    method Action upd(Token token, Addr addr, Bool pred, Bool actual);
        branchTable.upd(truncate({addr, globalHist}), actual);
        screenShot.upd(token, globalHist);
        globalHist <= truncate({globalhist, pack(actual)});
    endmethod

    method ActionValue#(Bool) getPred(Token token, Addr addr);
        let val = branchRegFile.sub(truncate({addr, globalHist}));
        return val;
    endmethod

    method Action abort(Token token);
        globalHist = screenShot.sub(token);
    endmethod
endmodule
