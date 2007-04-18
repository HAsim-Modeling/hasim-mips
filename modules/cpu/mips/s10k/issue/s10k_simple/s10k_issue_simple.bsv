
import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_isa::*;

module [HASim_Module] mkIssue
    //interface:
                (Empty);

  Connection_Send#(Tuple2#(Token, void)) fp_exe_req  <- mkConnection_Send("fp_exe_req");
  Connection_Send#(Tuple2#(Token, void)) fp_mem_req  <- mkConnection_Send("fp_mem_req");
endmodule
