import hasim_common::*;
import hasim_isa::*;

import RegFile::*;
import FIFO::*;

typedef Bit#(`BRANCH_TABLE_SIZE) BranchIndex;

interface BranchPred;
    method Action upd(Token token, Addr addr, Bool pred, Bool actual);
    method Action getPredReq(Token token, Addr addr);
    method ActionValue#(Bool) getPredResp();
    method Action abort(Token token);
endinterface


module mkBranchPred(BranchPred);

    RegFile#(BranchIndex, Bit#(2)) branchRegFile <- mkRegFileFull();
    FIFO#(Bool) respQ <- mkFIFO();

    method Action upd(Token token, Addr addr, Bool pred, Bool actual);
        let counter = branchRegFile.sub(truncate(addr));
        let newCounter = 0;
        if(actual)
            newCounter = (counter == 3)? 3: counter + 1;
        else
            newCounter = (counter == 0)? 0: counter - 1;
        branchRegFile.upd(truncate(addr), newCounter);
    endmethod

    method Action getPredReq(Token token, Addr addr);
        let counter = branchRegFile.sub(truncate(addr));
        respQ.enq(counter > 1);
    endmethod
    
    method ActionValue#(Bool) getPredResp();
        respQ.deq();
        return respQ.first();
    endmethod

    method Action abort(Token token);
        noAction;
    endmethod
endmodule
