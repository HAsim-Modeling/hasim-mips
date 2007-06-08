import LFSR::*;

import hasim_common::*;
import hasim_isa::*;

import hasim_command_center::*;


`define MEM_Hit_Chance 64
`define MEM_Miss_Penalty 10


typedef enum 
{
  MEM_Ready,
  MEM_Finish
}
  MEM_State
    deriving (Eq, Bits);

module [HASim_Module] mk5stage_MEM#(CommandCenter cc)
    //interface:
                ();
  
  //Local State
  Reg#(Token)     stall_tok   <- mkRegU;
  Reg#(Bit#(16))  stall_count <- mkReg(0);
  Reg#(Bool)      stalling    <- mkReg(False);
  Reg#(MEM_State) state       <- mkReg(MEM_Ready);
  
  //Pseudo-randomness
  LFSR#(Bit#(7)) lfsr <- mkFeedLFSR(7'b0011011);

  //Connections to FP
  Connection_Send#(Tuple2#(Token, void))     fp_mem_req  <- mkConnection_Send("fp_mem_req");
  Connection_Receive#(Tuple2#(Token, void))  fp_mem_resp <- mkConnection_Receive("fp_mem_resp");

  Connection_Send#(Token)     fp_mem_kill <- mkConnection_Send("fp_mem_kill");

  //Events
  //EventRecorder event_mem <- mkEventRecorder("MemOps");
  
  //Incoming Ports
  Port_Receive#(Token) port_from_exe <- mkPort_Receive("exe_to_mem", 1);

  //Outgoing Ports
  Port_Send#(Token) port_to_wb <- mkPort_Send("mem_to_wb");

  rule beginMem (cc.running && state == MEM_Ready);
  
    let mtok <- port_from_exe.receive();

    if (stalling)
      begin
	if (stall_count == 0)
	  begin
            port_to_wb.send(tagged Valid stall_tok);
	    stalling <= False;
	  end
	else
	  begin
	    stall_count <= stall_count - 1;
	    port_to_wb.send(tagged Invalid);
	  end
      end
    else
      begin

	case (mtok) matches
	  tagged Invalid:
	  begin
            port_to_wb.send(tagged Invalid);
            //event_mem.recordEvent(tagged Invalid);
	  end
	  tagged Valid .tok:
	  begin
            $display("REQ:MEM:%d", tok.index);
            fp_mem_req.send(tuple2(tok, ?));
	    state <= MEM_Finish;
	  end
	endcase

      end
  
  endrule

  rule finishMem (cc.running && state == MEM_Finish);
  
    match {.tok, .*} <- fp_mem_resp.receive();
    
    let isHit = (lfsr.value < `MEM_Hit_Chance);
    lfsr.next();
    
    if (isHit)
      begin

	port_to_wb.send(tagged Valid tok);
	//event_mem.recordEvent(tagged Valid zeroExtend(tok.index));

      end
    else
      begin
	port_to_wb.send(tagged Invalid);
	//event_mem.recordEvent(tagged Invalid);
	stall_count <= `MEM_Miss_Penalty;
	stall_tok   <= tok;
	stalling    <= True;
      end

    state <= MEM_Ready;
    
  endrule

endmodule
