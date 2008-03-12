import LFSR::*;
import Vector::*;

import hasim_common::*;
import soft_connections::*;
import hasim_modellib::*;
import hasim_isa::*;

import hasim_local_controller::*;
`include "asim/dict/STREAMS_EVENTS_MEMORY.bsh"

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

module [HASim_Module] mkPipe_Mem#(File debug_file, Tick curTick)
    //interface:
                ();
  
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
  EventRecorder event_mem <- mkEventRecorder(`STREAMS_EVENTS_MEMORY_INSTRUCTION_MEM);
    
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


  rule beginMem (state == MEM_Ready);
    
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
  
  endrule

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
        stall_count <= `MEM_DCACHE_MISS_PENALTY;
        stall_tok   <= tok;
        stalling    <= True;
      end

    state <= MEM_Ready;
    
  endrule

endmodule
