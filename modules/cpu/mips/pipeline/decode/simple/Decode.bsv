
import hasim_common::*;
import hasim_isa::*;

import hasim_command_center::*;

`define DEC_Is_Bypassed True

module [HASim_Module] mkPipe_Decode#(CommandCenter cc)
    //interface:
                ();


  //Local State
  Reg#(Bit#(2))   stall_count <- mkReg(0);
  Reg#(DepInfo)   stall_deps  <- mkRegU();
  Reg#(Token)     stall_tok   <- mkRegU();
  Reg#(Bool)      in_flight   <- mkReg(False);
  
  //Scoreboard
  Reg#(Maybe#(DepInfo)) exe_stall_info <- mkReg(tagged Invalid);
  Reg#(Maybe#(DepInfo)) mem_stall_info <- mkReg(tagged Invalid);
  Reg#(Maybe#(DepInfo))  wb_stall_info <- mkReg(tagged Invalid);

  //Connections to FP
  Connection_Send#(Tuple2#(Token, void))        fp_dec_req  <- mkConnection_Send("fp_dec_req");
  Connection_Receive#(Tuple2#(Token, DepInfo))  fp_dec_resp <- mkConnection_Receive("fp_dec_resp");
  
  //Events
  //EventRecorder event_dec <- mkEventRecorder("Decode");
  
  //Incoming Ports
  Port_Receive#(Token) port_from_fet <- mkPort_Receive("fet_to_dec", 1);

  //Outgoing Ports
  Port_Send#(Token)      port_to_exe <- mkPort_Send("dec_to_exe");

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
  
    if (`DEC_Is_Bypassed) 
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

  rule decodeReq (cc.running && !in_flight);
  
    let mtok <- port_from_fet.receive();
    
    case (mtok) matches
      tagged Invalid: //Pass-through
      begin
        port_to_exe.send(tagged Invalid);
	//event_dec.recordEvent(tagged Invalid);
	shiftStalls(tagged Invalid);
      end
      tagged Valid .tok:
      begin
        $display("REQ:DEC:%d", tok.index);
        fp_dec_req.send(tuple2(tok, ?));
	in_flight <= True;
      end
    endcase
    
  endrule

  rule decodeResp (cc.running && stall_count == 0 && in_flight);
  
    match {.tok, .deps} <- fp_dec_resp.receive();

    Bit#(2) new_stall = stallLength(deps);

    if (new_stall != 0) //We're stalling
    begin
      port_to_exe.send(tagged Invalid);
      //event_dec.recordEvent(tagged Invalid);
      shiftStalls(tagged Invalid);
      stall_tok <= tok;
      stall_deps <= deps;
    end
    else
    begin
      port_to_exe.send(tagged Valid tok);
      //event_dec.recordEvent(tagged Valid zeroExtend(tok.index));
      shiftStalls(tagged Valid deps);
      in_flight <= False;
    end
    
    stall_count <= new_stall;
    
  endrule

  rule decode_stall (cc.running && stall_count > 0 && in_flight);
  
    stall_count <= stall_count - 1;
    port_to_exe.send(tagged Invalid);
    //event_dec.recordEvent(tagged Invalid);
    shiftStalls(tagged Invalid);

  endrule

endmodule
