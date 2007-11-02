import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import fpga_components::*;
import hasim_common::*;

import hasim_funcp_base::*;
import hasim_isa::*;


`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Functional"
 

//-------------------------------------------------------------------------//
// Fetch Algorithm                                                         //
//-------------------------------------------------------------------------//

`define MODULE_NAME "mkFUNCP_FetchAlg"
module [HASim_Module] mkFUNCP_FetchAlg#(File debug_log, Tick curCC) ();

  FIFO#(Tuple2#(Token, Addr)) waitingQ <- mkFIFO();

  //Ports
  
  Connection_Server#(Tuple3#(Token, void, Addr), 
                     Tuple3#(Token, PackedInst, Tuple2#(Addr, PackedInst)))
  //... 
  link_fet <- mkConnection_Server("fp_fet_stage");
 
  Connection_Client#(Addr, PackedInst) 
  //...
  link_to_imem <- mkConnection_Client("mem_imem");
  
  //handleReq
  
  //Just pass the request on to the IMem

  rule handleFetch (True);
  
    debug_rule("handleFetch");
    
    Tuple3#(Token, void, Addr) tup = link_fet.getReq();
    link_fet.deq();
    
    match {.t, .*, .a} = tup;
    
    link_to_imem.makeReq(a);
    waitingQ.enq(tuple2(t, a));
    
  endrule

  //getMemResp
  
  //Just pass the response back from the IMem

  rule getMemResp (True);
  
    debug_rule("getMemResp");
    
    PackedInst resp = link_to_imem.getResp();
    link_to_imem.deq();
    
    match {.tok, .addr} = waitingQ.first();
    waitingQ.deq();
    
    link_fet.makeResp(tuple3(tok, resp, tuple2(addr, resp)));
  endrule

endmodule
`undef MODULE_NAME
