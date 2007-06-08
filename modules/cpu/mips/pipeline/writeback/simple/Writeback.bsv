
import hasim_common::*;
import hasim_isa::*;

import hasim_command_center::*;

module [HASim_Module] mkPipe_Writeback#(CommandCenter cc)
    //interface:
                ();

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
  //EventRecorder event_wb <- mkEventRecorder("Writeback");
  
  //Incoming Ports
  Port_Receive#(Token) port_from_mem <- mkPort_Receive("mem_to_wb", 1);
  
  rule lcoReq (cc.running && !in_flight);
  
    let mtok <- port_from_mem.receive();
    
    case (mtok) matches
      tagged Invalid:
      begin
        noAction;
	//event_wb.recordEvent(tagged Invalid);
      end
      tagged Valid .tok:
      begin
        $display("REQ:LCO:%d", tok.index);
        fp_lco_req.send(tuple2(tok, ?));
	in_flight <= True;
      end
    endcase
  
  endrule
   
  rule gcoReq (cc.running && in_flight);
  
    match {.tok, .*} <- fp_lco_resp.receive();
    $display("REQ:GCO:%d", tok.index);
    fp_gco_req.send(tuple2(tok, ?));
  endrule
  
  rule gcoResp (cc.running && in_flight);
  
    match {.tok, .*}  <- fp_gco_resp.receive();
    //event_wb.recordEvent(Valid zeroExtend(tok.index));
    in_flight <= False;
    
    case (cc.getStopToken) matches
      tagged Valid .t:
        if (t == tok)
	  cc.stop();
      default:
        noAction;
    endcase
    
  endrule

endmodule 
