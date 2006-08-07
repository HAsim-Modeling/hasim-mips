import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import FUNCP_Base::*;
import FUNCP_MemState::*;
import Debug::*;

import ISA::*;


`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Functional"


//-------------------------------------------------------------------------//
// Local Commit Unit                                                       //
//-------------------------------------------------------------------------//

//mkLocalCommit :: BypassUnit -> FP_Unit

`define MODULE_NAME "mkLocalCommit"
module [HASim_Module] mkFUNCP_LocalCommitAlg ();
  
   
  Connection_Server#(Tuple3#(Token, InstWBInfo, void),
                     Tuple3#(Token, void, InstWBInfo)) 
  //...
  link_lco <- mkConnection_Server("fp_lco_stage");
  
  Connection_Send#(Token) 
  //...
        link_freePReg <- mkConnection_Send("lco_to_bypass_free");

  rule handleLCO (True);
  
    match {.t, .wbinf, .*} <- link_lco.getReq();
    
    link_freePReg.send(t);

    link_lco.makeResp(tuple3(t, ?, wbinf));

  endrule
  
endmodule
`undef MODULE_NAME  

