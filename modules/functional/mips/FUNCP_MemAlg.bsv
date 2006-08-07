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
// Memory Unit                                                             //
//-------------------------------------------------------------------------//


`define MODULE_NAME "mkMem"
module [HASim_Module] mkFUNCP_MemAlg ();

  
  //Links
  Connection_Server#(Tuple3#(Token, ExecedInst, void),
                     Tuple3#(Token, void, InstWBInfo)) 
  //...
  link_mem <- mkConnection_Server("fp_mem_stage");
	  
  Connection_Client#(MemReq, MemResp) 
  //...
  link_to_dmem <- mkConnection_Client("mem_dmem");

  Connection_Send#(Tuple2#(PRName, Value)) 
  //...
        link_write2 <- mkConnection_Send("mem_to_bypass_write2");

  FIFO#(Tuple2#(Token, ExecedInst)) 
  //...
  waitingQ <- mkFIFO();
  //doLookup

  //doReq
  
  rule doReq (True);
    
    debug_rule("doReq");
    
    match {.t, .i, .*} <- link_mem.getReq();
    
    case (i) matches
      tagged ELoad {addr: .a, pdest: .prd}:
      begin
      
        debug_case("i", "ELoad");
       
        link_to_dmem.makeReq(Ld {addr: a, token: t});
	  
        debug(2, $display("MEM: [%d] Load Request: 0x%h", t.index, a));
	
        waitingQ.enq(tuple2(t, i));
      end
      tagged EStore{val: .v, addr: .a}:
      begin
      
        debug_case("i", "EStore");
	
        link_to_dmem.makeReq(St {val: v, addr: a, token: t});
	  
        debug(2, $display("MEM: [%d] Store Request: 0x%h := %d", t.index, a, v));
	
	
        waitingQ.enq(tuple2(t, i));
      end
      tagged EWB {pdest: .pd}:
      begin

	debug_case_default("i");

        link_mem.makeResp(tuple3(t, ?, WWB));

        debug(2, $display("MEM: [%d] Passing through Nop", t.index));
	
      end
      tagged ENop:
      begin

	debug_case_default("i");

        link_mem.makeResp(tuple3(t, ?, WNop));

        debug(2, $display("MEM: [%d] Passing through Nop", t.index));
	
      end
    endcase
    
  endrule

  //getResp

  rule getResp(True);
       
    debug_rule("getResp");
  
    match {.tok, .i} = waitingQ.first();
    
    case (i) matches
      tagged ELoad {pdest: .prd, addr: .*}:
        begin
	
	  debug_case("i", "ELoad");
	  
          let resp <- link_to_dmem.getResp();
	  
          Value v = case (resp) matches
                      tagged LdResp .val: return val;
                      tagged StResp .*  : return 0; // impossible
                    endcase;
		    
          waitingQ.deq();
          link_mem.makeResp(tuple3(tok,?, WWB));
          link_write2.send(tuple2(prd, v));
	  
          debug(2, $display("MEM: [%d] LdResp: PR%d <= %0h", tok, prd, v));
	  
        end
      tagged EStore {val: .*, addr: .*}:
        begin
	
	  debug_case("i", "EStore");
	  
          let resp <- link_to_dmem.getResp();
          waitingQ.deq();
          link_mem.makeResp(tuple3(tok, ?, WStore));
	  
          debug(2, $display("MEM: [%d] StResp", tok));
        end
      default:
        begin
	
	  debug_case_default("i");
	  
          waitingQ.deq();
          link_mem.makeResp(tuple3(tok, ?, WNop));
	  
          $display("MEM: [%d] ERROR NON-MEMORY OP IN QUEUE", tok);
        end
    endcase
  endrule
  
endmodule
`undef MODULE_NAME  
