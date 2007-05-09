import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

module [HASim_Module] mkExecute();
    Connection_Receive#(Tuple2#(Token, InstResult))  fpExeResponse <- mkConnection_Receive("fp_exe_resp");
    Connection_Receive#(Tuple2#(Token, void))        fpMemResponse <- mkConnection_Receive("fp_mem_resp");

    Port_Receive#(Tuple2#(Token, Addr))              alu1IssuePort <- mkPort_Receive("ALU1IssuePort");
    Port_Receive#(Tuple2#(Token, Addr))              alu2IssuePort <- mkPort_Receive("ALU2IssuePort");
    Port_Receive#(Token)                              memIssuePort <- mkPort_Receive("MemIssuePort");
endmodule
