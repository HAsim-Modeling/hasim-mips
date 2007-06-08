import hasim_common::*;

interface BranchStack;
    method Action resolveWrong(Token token);
    method Action resolveRight(Token token);
    method Action add(Token token);
    method Bool notFull();
endinterface

module mkBranchStack
    //interface:
                (BranchStack);

    method Action resolveWrong(Token token);
        noAction;
    endmethod

    method Action resolveRight(Token token);
        noAction;
    endmethod

    method Action add(Token token);
        noAction;
    endmethod

    method Bool notFull();
        return True;
    endmethod
endmodule


