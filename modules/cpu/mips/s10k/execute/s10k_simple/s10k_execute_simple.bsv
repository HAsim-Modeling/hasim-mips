
import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_isa::*;

module [HASim_Module] mkExecute
    //interface:
                (Empty);
  Connection_Receive#(Tuple2#(Token, InstResult))  fp_exe_resp <- mkConnection_Receive("fp_exe_resp");
  Connection_Receive#(Tuple2#(Token, void))  fp_mem_resp <- mkConnection_Receive("fp_mem_resp");
endmodule
