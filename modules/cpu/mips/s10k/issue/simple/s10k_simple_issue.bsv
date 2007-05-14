import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import RegFile::*;

module [HASim_Module] mkIssue();
    Connection_Send#(Tuple2#(Token, void)) fpExeReq  <- mkConnection_Send("fp_exe_req");
    Connection_Send#(Tuple2#(Token, void)) fpMemReq  <- mkConnection_Send("fp_mem_req");
endmodule
