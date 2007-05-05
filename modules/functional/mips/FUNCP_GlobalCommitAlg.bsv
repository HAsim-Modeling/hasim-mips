import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_funcp_base::*;
import hasim_isa::*;


`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Functional"


//-------------------------------------------------------------------------//
// Global Commit Unit                                                      //
//-------------------------------------------------------------------------//

//mkGlobalCommit :: Memory -> FP_Unit

`define MODULE_NAME "mkGlobalCommit"
module [HASim_Module] mkFUNCP_GlobalCommitAlg ();

  Connection_Send#(Token) link_mem_commit <- mkConnection_Send("mem_commit");
  
  Connection_Server#(Tuple3#(Token, InstWBInfo, void),
                     Tuple3#(Token, void, void)) 
  //...
  link_gco <- mkConnection_Server("fp_gco_stage");
  
  rule handleGCO (True);
  
    debug_rule("handleGCO");
    
    match {.tok, .inf, .*} <- link_gco.getReq();
    
    case (inf)
      WStore:
        link_mem_commit.send(tok);
      default:
        noAction;
    endcase
    
    link_gco.makeResp(tuple3(tok, ?, ?));
  
  endrule
  
  
endmodule
`undef MODULE_NAME
