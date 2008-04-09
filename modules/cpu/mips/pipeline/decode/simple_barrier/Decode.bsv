import FIFO::*;
import Vector::*;

import hasim_common::*;
import soft_connections::*;
import hasim_modellib::*;
import hasim_isa::*;

import module_local_controller::*;
`include "asim/dict/STREAMS.bsh"

//AWB Parameters           default:
//DEC_PIPELINE_IS_BYPASSED   True

interface Decode;
  method Action start();
endinterface

module [HASim_Module] mkPipe_Decode#(File debug_file, Tick curTick)
    //interface:
                (Decode);


  //Local State
  Reg#(Bit#(2))   stall_count <- mkReg(0);
  Reg#(DepInfo)   stall_deps  <- mkRegU();
  Reg#(Token)     stall_tok   <- mkRegU();
  Reg#(Bool)      in_flight   <- mkReg(False);
  FIFO#(Maybe#(Addr)) addrQ   <- mkFIFO();
  
  //Scoreboard
  Reg#(Maybe#(DepInfo)) exe_stall_info <- mkReg(tagged Invalid);
  Reg#(Maybe#(DepInfo)) mem_stall_info <- mkReg(tagged Invalid);
  Reg#(Maybe#(DepInfo))  wb_stall_info <- mkReg(tagged Invalid);

  //Connections to FP
  Connection_Send#(Tuple2#(Token, void))        fp_dec_req  <- mkConnection_Send("fp_dec_req");
  Connection_Receive#(Tuple2#(Token, DepInfo))  fp_dec_resp <- mkConnection_Receive("fp_dec_resp");
  
  //Events
  EventRecorder event_dec <- mkEventRecorder(`STREAMS_EVENT_DECODE);
  
  //Incoming Ports
  Port_Receive#(Tuple2#(Token, Maybe#(Addr))) port_from_fet <- mkPort_Receive("fet_to_dec", 1);

  //Outgoing Ports
  Port_Send#(Tuple2#(Token, Maybe#(Addr)))      port_to_exe <- mkPort_Send("dec_to_exe");

  //Local Controller
  Vector#(1, Port_Control) inports  = newVector();
  Vector#(1, Port_Control) outports = newVector();
  inports[0]  = port_from_fet.ctrl;
  outports[0] = port_to_exe.ctrl;
  LocalController local_ctrl <- mkLocalController(inports, outports);


  //Stall functions

  function Action shiftStalls(Maybe#(DepInfo) mdeps);
  action
  
    exe_stall_info <= mdeps;
    mem_stall_info <= exe_stall_info;
    wb_stall_info <= mem_stall_info;
  
  endaction
  endfunction

  function Bit#(n) max(Bit#(n) x, Bit#(n) y);
  
    return (x > y) ? x : y;
  
  endfunction

  function Bool isCalculating(PRName pr, Maybe#(DepInfo) mdeps);
    
    case (mdeps) matches
      tagged Invalid:
        return False;
      tagged Valid .deps:
        case (deps.dep_dest) matches
          tagged Invalid:
            return False;
          tagged Valid {.rnm, .prnm}:
            return prnm == pr;
        endcase
    endcase
     
  endfunction
  
  function Bit#(2) stallsFor(PRName pr);
  
    Bit#(2) st_exe = isCalculating(pr, exe_stall_info) ? 3 : 0;
    Bit#(2) st_mem = isCalculating(pr, mem_stall_info) ? 2 : 0;
    Bit#(2) st_wb  = isCalculating(pr, wb_stall_info)  ? 1 : 0;
  
    return max(max(st_exe, st_mem), st_wb);
  
  endfunction

  function Bit#(2) stallLength(DepInfo deps);
  
    if (`DEC_PIPELINE_IS_BYPASSED) 
      return 0;
    else
    begin
      Bit#(2) stall1 = 
        case (deps.dep_src1) matches
          tagged Invalid:
            return 0;
          tagged Valid {.rnm, .prnm}:
            return stallsFor(prnm);
        endcase;
    
      Bit#(2) stall2 = 
        case (deps.dep_src2) matches
          tagged Invalid:
            return 0;
          tagged Valid {.rnm, .prnm}:
            return stallsFor(prnm);
        endcase;
      
      return max(stall1, stall2);
      
    end
  
  endfunction

  
  //Rules

  rule decodeResp (stall_count == 0 && in_flight);
  
    match {.tok, .deps} = fp_dec_resp.receive();
    fp_dec_resp.deq();
    
    $fdisplay(debug_file, "[%d]:DEC:RSP: %0d", curTick, tok.index);

    Bit#(2) new_stall = stallLength(deps);

    if (new_stall != 0) //We're stalling
    begin
      port_to_exe.send(tagged Invalid);
      event_dec.recordEvent(tagged Invalid);
      shiftStalls(tagged Invalid);
      stall_tok <= tok;
      stall_deps <= deps;
    end
    else
    begin
      port_to_exe.send(tagged Valid tuple2(tok, addrQ.first()));
      addrQ.deq();
      event_dec.recordEvent(tagged Valid zeroExtend(tok.index));
      shiftStalls(tagged Valid deps);
      in_flight <= False;
    end
    
    stall_count <= new_stall;
    
  endrule

  rule decode_stall (stall_count > 0 && in_flight);
  
    stall_count <= stall_count - 1;
    port_to_exe.send(tagged Invalid);
    event_dec.recordEvent(tagged Invalid);
    shiftStalls(tagged Invalid);

  endrule


  method Action start() if (!in_flight);
  
    local_ctrl.startModelCC();
  
    let mtup <- port_from_fet.receive();
    
    case (mtup) matches
      tagged Invalid: //Pass-through
      begin
        port_to_exe.send(tagged Invalid);
        event_dec.recordEvent(tagged Invalid);
        shiftStalls(tagged Invalid);
      end
      tagged Valid {.tok, .maddr}:
      begin
        $fdisplay(debug_file, "[%d]:DEC:REQ: %0d", curTick, tok.index);
        fp_dec_req.send(tuple2(tok, ?));
        addrQ.enq(maddr);
        in_flight <= True;
      end
    endcase
    
  endmethod
endmodule
