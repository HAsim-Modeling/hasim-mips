
import Vector::*;

import hasim_common::*;
import hasim_isa::*;

import hasim_local_controller::*;

interface Writeback;
  method Action start();
endinterface

module [HASim_Module] mkPipe_Writeback#(File debug_file, Tick curTick)
    //interface:
                (Writeback);

  //Local State
  Reg#(Bool) in_flight <- mkReg(False);

  //Connections to FP
  Connection_Send#(Tuple2#(Token, void))    fp_lco_req  <- mkConnection_Send("fp_lco_req");
  Connection_Receive#(Tuple2#(Token, void)) fp_lco_resp <- mkConnection_Receive("fp_lco_resp");
  
  Connection_Send#(Token)     fp_lco_kill <- mkConnection_Send("fp_lco_kill");
  
  Connection_Send#(Tuple2#(Token, void))    fp_gco_req  <- mkConnection_Send("fp_gco_req");
  Connection_Receive#(Tuple2#(Token, void)) fp_gco_resp <- mkConnection_Receive("fp_gco_resp");
    
  Connection_Send#(Token)        fp_gco_kill <- mkConnection_Send("fp_gco_kill");
  
  Connection_Send#(Token) link_memstate_kill <- mkConnection_Send("fp_memstate_kill");

  //Events
  EventRecorder event_wb <- mkEventRecorder("5          WB");
  
  //Stats
  Stat stat_wb <- mkStatCounter("Instructions Committed");

  //Incoming Ports
  Port_Receive#(Token) port_from_mem <- mkPort_Receive("mem_to_wb", 1);

  //Local Controller
  Vector#(1, Port_Control) inports = newVector();
  Vector#(0, Port_Control) outports = newVector();
  inports[0] = port_from_mem.ctrl;
  LocalController local_ctrl <- mkLocalController(inports, outports);

  rule gcoReq (in_flight);
  
    match {.tok, .*} = fp_lco_resp.receive();
    fp_lco_resp.deq();
    
    $fdisplay(debug_file, "[%d]:LCO:RSP: %0d", curTick, tok.index);
    $fdisplay(debug_file, "[%d]:GCO:REQ: %0d", curTick, tok.index);
    fp_gco_req.send(tuple2(tok, ?));
  endrule
  
  rule gcoResp (in_flight);
  
    match {.tok, .*}  = fp_gco_resp.receive();
    fp_gco_resp.deq();
    
    $fdisplay(debug_file, "[%d]:GCO:RSP: %0d", curTick, tok.index);
    
    event_wb.recordEvent(tagged Valid zeroExtend(tok.index));
    stat_wb.incr();
    
    in_flight <= False;

    if (tok.timep_info.scratchpad[1] == 1)
       local_ctrl.endProgram(unpack(tok.timep_info.scratchpad[2]));
    
  endrule

  method Action start() if (!in_flight);
  
    let mtok <- port_from_mem.receive();
    
    case (mtok) matches
      tagged Invalid:
      begin
        noAction;
        event_wb.recordEvent(tagged Invalid);
      end
      tagged Valid .tok:
      begin
        $fdisplay(debug_file, "[%d]:LCO:REQ: %0d", curTick, tok.index);
        fp_lco_req.send(tuple2(tok, ?));
        in_flight <= True;
      end
    endcase
  
  endmethod
   
endmodule 
