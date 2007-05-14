import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import FIFOF::*;

import s10k_simple_common::*;

module [HASim_Module] mkExecute();
    Connection_Receive#(Tuple2#(Token, InstResult))  fpExeResponse <- mkConnection_Receive("fp_exe_resp");
    Connection_Receive#(Tuple2#(Token, void))        fpMemResponse <- mkConnection_Receive("fp_mem_resp");

    Port_Receive#(ExecEntry)                         alu1IssuePort <- mkPort_Receive("ALU1IssuePort", 1);
    Port_Receive#(ExecEntry)                         alu2IssuePort <- mkPort_Receive("ALU2IssuePort", 1);
    Port_Receive#(ExecEntry)                          memIssuePort <- mkPort_Receive("MemIssuePort", 1);

    Port_Send#(ExecEntry)                                exec1Port <- mkPort_Send("execToDecode1");
    Port_Send#(ExecEntry)                                exec2Port <- mkPort_Send("execToDecode2");
    Port_Send#(ExecEntry)                                exec3Port <- mkPort_Send("execToDecode3");
    Port_Send#(Addr)                               branchTakenPort <- mkPort_Send("execToDecodeBranch");

    FIFOF#(Maybe#(ExecEntry))                            exec1FIFO <- mkFIFOF();
    FIFOF#(Maybe#(ExecEntry))                            exec2FIFO <- mkFIFOF();
    FIFOF#(Maybe#(ExecEntry))                            exec3FIFO <- mkFIFOF();

    rule exec1(True);
        let exec1 <- alu1IssuePort.receive();
        exec1FIFO.enq(exec1);
    endrule

    rule exec2(True);
        let exec2 <- alu2IssuePort.receive();
        exec2FIFO.enq(exec2);
    endrule

    rule exec3(True);
        let exec3 <- memIssuePort.receive();
        exec3FIFO.enq(exec3);
    endrule

    rule fillAlu1(isValid(exec1FIFO.first()));
        exec1Port.send(exec1FIFO.first());
        exec1FIFO.deq();
        match {.token, .result}  <- fpExeResponse.receive();
        case (result) matches
            tagged RBranchTaken .addr:
                branchTakenPort.send(tagged Valid addr);
            default:
                branchTakenPort.send(tagged Invalid);
        endcase
    endrule

    rule fillAlu1Invalid(exec1FIFO.notEmpty() && !isValid(exec1FIFO.first()));
        exec1Port.send(tagged Invalid);
        exec1FIFO.deq();
        branchTakenPort.send(tagged Invalid);
    endrule

    rule fillAlu2(isValid(exec2FIFO.first()));
        exec2Port.send(exec2FIFO.first());
        exec2FIFO.deq();
        let resp <- fpExeResponse.receive();
    endrule

    rule fillAlu2Invalid(exec2FIFO.notEmpty() && !isValid(exec2FIFO.first()));
        exec2Port.send(tagged Invalid);
        exec2FIFO.deq();
    endrule

    rule fillMem(isValid(exec3FIFO.first()));
        exec3Port.send(exec3FIFO.first());
        exec3FIFO.deq();
        let resp <- fpMemResponse.receive();
    endrule

    rule fillMemInvalid(exec3FIFO.notEmpty() && !isValid(exec3FIFO.first()));
        exec3Port.send(tagged Invalid);
        exec3FIFO.deq();
    endrule
endmodule
