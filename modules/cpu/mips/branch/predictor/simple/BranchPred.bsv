import hasim_common::*;
import hasim_isa::*;

import RegFile::*;

typedef Bit#(`BRANCH_TABLE_SIZE) BranchIndex;

interface BranchPred;
    method Action upd(Token token, Addr addr, Bool pred, Bool actual);
    method Action getPredReq(Token token, Addr addr);
    method ActionValue#(Bool) getPredResp();
    method Action abort(Token token);
endinterface

module mkBranchPred(BranchPred);

    RegFile#(BranchIndex, Bool) branchRegFile <- mkRegFileFull();
    FIFO#(Bool) respQ <- mkFIFO();

    method Action upd(Token token, Addr addr, Bool pred, Bool actual);
        branchRegFile.upd(truncate(addr), actual);
    endmethod

    method Action getPredReq(Token token, Addr addr);
        respQ.enq(branchRegFile.sub(truncate(addr)));
    endmethod

    method ActionValue#(Bool) getPred();
        respQ.deq();
        return respQ.first();
    endmethod

    method Action abort(Token token);
        noAction;
    endmethod
    
endmodule
