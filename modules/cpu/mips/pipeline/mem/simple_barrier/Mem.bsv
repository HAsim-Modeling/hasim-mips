import LFSR::*;
import Vector::*;

import hasim_common::*;
import hasim_isa::*;

import hasim_local_controller::*;

//AWB Parameters          default:
//MEM_DCACHE_HIT_CHANCE     50
//MEM_DCACHE_MISS_PENALTY   10


typedef enum 
{
  MEM_Ready,
  MEM_Finish
}
  MEM_State
    deriving (Eq, Bits);

Integer mem_hit_chance = (`MEM_DCACHE_HIT_CHANCE * 127) / 100;

interface Mem;
  method Action start();
endinterface

module [HASim_Module] mkPipe_Mem#(File debug_file, Tick curTick)
    //interface:
                (Mem);
  
  //Local State
  Reg#(Token)     stall_tok   <- mkRegU;
  Reg#(Bit#(16))  stall_count <- mkReg(0);
  Reg#(Bool)      stalling    <- mkReg(False);
  Reg#(MEM_State) state       <- mkReg(MEM_Ready);
  
  //Pseudo-randomness
  LFSR#(Bit#(7)) lfsr <- mkFeedLFSR(7'b1001110);

  //Connections to FP
  Connection_Send#(Tuple2#(Token, void))     fp_mem_req  <- mkConnection_Send("fp_mem_req");
  Connection_Receive#(Tuple2#(Token, void))  fp_mem_resp <- mkConnection_Receive("fp_mem_resp");

  Connection_Send#(Token)     fp_mem_kill <- mkConnection_Send("fp_mem_kill");

  //Events
  EventRecorder event_mem <- mkEventRecorder("4       MEM");
  
  //Stats
  Stat stat_dmisses <- mkStatCounter("DCache Misses");
  
  //Incoming Ports
  Port_Receive#(Token) port_from_exe <- mkPort_Receive("exe_to_mem", 1);

  //Outgoing Ports
  Port_Send#(Token) port_to_wb <- mkPort_Send("mem_to_wb");

  //Local Controller
  Vector#(1, Port_Control) inports  = newVector();
  Vector#(1, Port_Control) outports = newVector();
  inports[0]  = port_from_exe.ctrl;
  outports[0] = port_to_wb.ctrl;
  LocalController local_ctrl <- mkLocalController(inports, outports);

  rule finishMem (state == MEM_Finish);
  
    match {.tok, .*} = fp_mem_resp.receive();
    fp_mem_resp.deq();
    
    $fdisplay(debug_file, "[%d]:RSP:MEM: %0d", curTick, tok.index);
    
    let isHit = True; //(lfsr.value < fromInteger(mem_hit_chance)); //Currently bugged
    lfsr.next();
    
    if (isHit)
      begin

	port_to_wb.send(tagged Valid tok);
	event_mem.recordEvent(tagged Valid zeroExtend(tok.index));

      end
    else
      begin
	port_to_wb.send(tagged Invalid);
	event_mem.recordEvent(tagged Invalid);
	stat_dmisses.incr();
	stall_count <= `MEM_DCACHE_MISS_PENALTY;
	stall_tok   <= tok;
	stalling    <= True;
      end

    state <= MEM_Ready;
    
  endrule


  method Action start() if (state == MEM_Ready);
    
    local_ctrl.startModelCC();
     
    let mtok <- port_from_exe.receive();

    if (stalling)
      begin
	if (stall_count == 0)
	  begin
            port_to_wb.send(tagged Valid stall_tok);
            event_mem.recordEvent(tagged Valid zeroExtend(stall_tok.index));
	    stalling <= False;
	  end
	else
	  begin
	    stall_count <= stall_count - 1;
	    port_to_wb.send(tagged Invalid);
            event_mem.recordEvent(tagged Invalid);
	  end
      end
    else
      begin

	case (mtok) matches
	  tagged Invalid:
	  begin
            port_to_wb.send(tagged Invalid);
            event_mem.recordEvent(tagged Invalid);
	  end
	  tagged Valid .tok:
	  begin
            $fdisplay(debug_file, "[%d]:REQ:MEM: %0d", curTick, tok.index);
            fp_mem_req.send(tuple2(tok, ?));
	    state <= MEM_Finish;
	  end
	endcase

      end
  
  endmethod
endmodule
