import hasim_cpu_types::*;

interface BranchStack;
    method ActionValue#(BranchStackIndex) add();
    method Action resolveRight(BranchStackIndex index);
    method Action resolveWrong(BranchStackIndex index);
    method Bool notFull();
endinterface

module mkBranchStack(BranchStack);
    Reg#(Bool) allotted <- mkReg(False);

    method ActionValue#(BranchStackIndex) add();
        allotted <= True;
        return 0;
    endmethod

    method Action resolveRight(BranchStackIndex index);
        allotted <= False;
    endmethod

    method Action resolveWrong(BranchStackIndex index);
        allotted <= False;
    endmethod

    method Bool notFull();
        return !allotted;
    endmethod
endmodule
