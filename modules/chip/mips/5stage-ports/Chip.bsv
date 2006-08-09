import ConfigReg::*;
import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;
import LFSR::*;

import HASim::*;
import Events::*;
import ISA::*;
import Ports::*;
import CommandCenter::*;


//XXX chances are out of 127
`define FET_Miss_Penalty 1
`define FET_Hit_Chance 64
`define DEC_Is_Bypassed False
`define MEM_Hit_Chance 64
`define MEM_Miss_Penalty 1

typedef Bit#(1) Epoch;

module [HASim_Module] mk5stage_FET#(CommandCenter cc)
    //interface:
                ();

  //Local State

  Reg#(Addr)              pc <- mkReg(0);
  Reg#(Epoch)          epoch <- mkReg(0);
  Reg#(Bool)         killing <- mkReg(False);
  Reg#(Token)       kill_tok <- mkRegU();
  Reg#(Token)      stall_tok <- mkRegU;
  Reg#(Bit#(16)) stall_count <- mkReg(0);
  Reg#(Bit#(16)) numtokens <- mkReg(0);
  
  //Pseudo-randomness
  LFSR#(Bit#(7)) lfsr <- mkFeedLFSR(7'b1001110);

  //Connections to FP
  Connection_Send#(Bit#(8))   fp_tok_req  <- mkConnection_Send("fp_tok_req");
  Connection_Receive#(Token)  fp_tok_resp <- mkConnection_Receive("fp_tok_resp");
  Connection_Send#(Token)     fp_tok_kill <- mkConnection_Send("fp_tok_kill");
  
  Connection_Send#(Tuple2#(Token, Addr))     fp_fet_req  <- mkConnection_Send("fp_fet_req");
  Connection_Receive#(Tuple2#(Token, Inst))  fp_fet_resp <- mkConnection_Receive("fp_fet_resp");

  Connection_Send#(Token)     fp_fet_kill <- mkConnection_Send("fp_fet_kill");
  Connection_Send#(Token)     fp_dec_kill <- mkConnection_Send("fp_dec_kill");
  
  Connection_Send#(Token)     fp_rewindToToken <- mkConnection_Send("fp_rewindToToken");
    
  //Events
  //EventRecorder event_fet <- mkEventRecorder("Fetch");
  
  //Incoming Ports
  Port_Receive#(Tuple2#(Token, Addr)) port_from_ic <- mkPort_Receive("fet_setPC", 1);

  //Outgoing Ports
  Port_Send#(Token) port_to_dec <- mkPort_Send("fet_to_dec");

  rule tokenReq (cc.running && numtokens < 5);
    
    numtokens <= numtokens + 1;
    
    fp_tok_req.send(17); //17 is arbitrarily-chosen bug workaround
    
  endrule


  rule fetchReq (cc.running);
  
    let tok <- fp_tok_resp.receive();
    
    let inf = TokInfo {epoch: epoch, ctxt: ?};
    let tok2 = Token {index: tok.index, info: inf};
        
    pc <= pc + 1;
  
    $display("REQ:FET");
    fp_fet_req.send(tuple2(tok2, pc));
  
  endrule


  rule fetchResp (cc.running && stall_count == 0);
  
    match {.tok, .inst} <- fp_fet_resp.receive();
        
    if (tok.info.epoch != epoch) //kill it and don't fetch it
    begin
      fp_dec_kill.send(tok);
    end
    else //continue to fetch it
    begin
    
      if (killing) //stop killing
      begin
	killing <= False;
	fp_rewindToToken.send(kill_tok);
	fp_tok_kill.send(kill_tok);
      end

      let isHit = lfsr.value < `FET_Hit_Chance;
      lfsr.next();

      if (isHit)
      begin
	port_to_dec.send(Valid tok);
	numtokens <= numtokens - 1;
	//event_fet.recordEvent(Valid zeroExtend(tok.index));
      end
      else
      begin
        port_to_dec.send(Invalid);
	//event_fet.recordEvent(Invalid);
	
	stall_count <= `FET_Miss_Penalty;
	stall_tok <= tok;
      end

    end

  endrule

  rule fetch_stalling (cc.running && stall_count > 0);
   
    stall_count <= stall_count - 1;
    
    if (stall_count == 1)
    begin
      port_to_dec.send(Valid stall_tok);
      numtokens <= numtokens - 1;
      //event_fet.recordEvent(Valid zeroExtend(stall_tok.index));
    end
    else
    begin
      port_to_dec.send(Invalid);
      //event_fet.recordEvent(Invalid);
    end
    
  endrule
  
  rule fetch_kill (cc.running);
 
    let mtup <- port_from_ic.receive();
    
    case (mtup) matches
      tagged Invalid:
	noAction;
      tagged Valid {.ktok, .new_pc}:
      begin
	killing <= True;
	kill_tok <= ktok;
	epoch <= epoch + 1;
	pc <= new_pc;
      end
    endcase
  
  endrule

endmodule

module [HASim_Module] mk5stage_DEC#(CommandCenter cc)
    //interface:
                ();


  //Local State
  Reg#(Bit#(2))   stall_count <- mkReg(0);
  Reg#(DepInfo)   stall_deps  <- mkRegU();
  Reg#(Token)     stall_tok   <- mkRegU();
  
  //Scoreboard
  Reg#(Maybe#(DepInfo)) exe_stall_info <- mkReg(Invalid);
  Reg#(Maybe#(DepInfo)) mem_stall_info <- mkReg(Invalid);
  Reg#(Maybe#(DepInfo))  wb_stall_info <- mkReg(Invalid);

  //Connections to FP
  Connection_Send#(Tuple2#(Token, void))        fp_dec_req  <- mkConnection_Send("fp_dec_req");
  Connection_Receive#(Tuple2#(Token, DepInfo))  fp_dec_resp <- mkConnection_Receive("fp_dec_resp");

  Connection_Send#(Token)     fp_exe_kill <- mkConnection_Send("fp_exe_kill");
  
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

  rule decodeReq (cc.running && stall_count == 0);
  
    let mtok <- port_from_fet.receive();
    
    case (mtok) matches
      tagged Invalid: //Pass-through
      begin
        port_to_exe.send(Invalid);
	//event_dec.recordEvent(Invalid);
	shiftStalls(Invalid);
      end
      tagged Valid .tok:
      begin
        $display("REQ:DEC");
        fp_dec_req.send(tuple2(tok, ?));
      end
    endcase
    
  endrule

  rule decodeResp (cc.running && stall_count == 0);
  
    match {.tok, .deps} <- fp_dec_resp.receive();

    Bit#(2) new_stall = stallLength(deps);

    if (new_stall != 0) //We're stalling
    begin
      port_to_exe.send(Invalid);
      //event_dec.recordEvent(Invalid);
      shiftStalls(Invalid);
      stall_tok <= tok;
      stall_deps <= deps;
    end
    else
    begin
      port_to_exe.send(Valid tok);
      //event_dec.recordEvent(Valid zeroExtend(tok.index));
      shiftStalls(Valid deps);
    end
    
    stall_count <= new_stall;
    
  endrule

  rule decode_stall (cc.running && stall_count > 0);
  
    stall_count <= stall_count - 1;
    
    if (stall_count == 1)
    begin
      port_to_exe.send(Valid stall_tok);
      //event_dec.recordEvent(Valid zeroExtend(stall_tok.index));
      shiftStalls(Valid stall_deps);
    end
    else
    begin
      port_to_exe.send(Invalid);
      //event_dec.recordEvent(Invalid);
      shiftStalls(Invalid);
    end
    
  endrule

endmodule

module [HASim_Module] mk5stage_EXE#(CommandCenter cc)
    //interface:
                ();
  
  //Local State
  Reg#(Bool) killing <- mkReg(False);
  Reg#(Epoch)  epoch <- mkReg(0);

  //Connections to FP
  
  Connection_Send#(Tuple2#(Token, void))           fp_exe_req  <- mkConnection_Send("fp_exe_req");
  Connection_Receive#(Tuple2#(Token, InstResult))  fp_exe_resp <- mkConnection_Receive("fp_exe_resp");

  Connection_Send#(Token)     fp_mem_kill <- mkConnection_Send("fp_mem_kill");
  
  //Events
  //EventRecorder event_exe <- mkEventRecorder("Execute");
  
  //Incoming Ports
  Port_Receive#(Token) port_from_dec <- mkPort_Receive("dec_to_exe", 1);

  //Outgoing Ports
  Port_Send#(Token)               port_to_mem <- mkPort_Send("exe_to_mem");
  Port_Send#(Tuple2#(Token, Addr)) port_to_ic <- mkPort_Send("fet_setPC");

  rule executeReq (cc.running);
  
    let mtok <- port_from_dec.receive();
    
    case (mtok) matches
      tagged Invalid:
      begin
        port_to_mem.send(Invalid);
	//event_exe.recordEvent(Invalid);
      end
      tagged Valid .tok:
      begin
        $display("REQ:EXE");
        fp_exe_req.send(tuple2(tok, ?));
      end
    endcase
  
  endrule

  rule executeResp (cc.running);
  
    match {.tok, .res} <- fp_exe_resp.receive();
   
    if (tok.info.epoch != epoch) //kill it
    begin
      fp_mem_kill.send(tok);
    end
    else //continue to execute it
    begin
    
      case (res) matches
	tagged RBranchTaken .addr:
	  begin
	    port_to_ic.send(Valid tuple2(tok, addr));
	  end
	tagged RBranchNotTaken:
          noAction;
	tagged RNop:
          noAction;
	tagged RTerminate:
          case (cc.getStopToken) matches
	    Invalid:
  	      cc.setStopToken(tok);
	    default:
	      noAction;
	  endcase
      endcase

      port_to_mem.send(Valid tok);
      //event_exe.recordEvent(Valid zeroExtend(tok.index));
      
    end
    
  endrule


endmodule

module [HASim_Module] mk5stage_MEM#(CommandCenter cc)
    //interface:
                ();
  
  //Local State
  Reg#(Token)    stall_tok   <- mkRegU;
  Reg#(Bit#(16)) stall_count <- mkReg(0);
  
  //Pseudo-randomness
  LFSR#(Bit#(7)) lfsr <- mkFeedLFSR(7'b0011011);

  //Connections to FP
  Connection_Send#(Tuple2#(Token, void))     fp_mem_req  <- mkConnection_Send("fp_mem_req");
  Connection_Receive#(Tuple2#(Token, void))  fp_mem_resp <- mkConnection_Receive("fp_mem_resp");

  //Events
  //EventRecorder event_mem <- mkEventRecorder("MemOps");
  
  //Incoming Ports
  Port_Receive#(Token) port_from_exe <- mkPort_Receive("exe_to_mem", 1);

  //Outgoing Ports
  Port_Send#(Token) port_to_wb <- mkPort_Send("mem_to_wb");

  rule memReq (cc.running && stall_count == 0);
  
    let mtok <- port_from_exe.receive();
    
    case (mtok) matches
      tagged Invalid:
      begin
        port_to_wb.send(Invalid);
	//event_mem.recordEvent(Invalid);
      end
      tagged Valid .tok:
      begin
        $display("REQ:MEM");
        fp_mem_req.send(tuple2(tok, ?));
      end
    endcase
  
  endrule

  rule memResp (cc.running && stall_count == 0);
  
    match {.tok, .*} <- fp_mem_resp.receive();
    
    let isHit = (lfsr.value < `MEM_Hit_Chance);
    lfsr.next();
    
    if (isHit)
    begin
      port_to_wb.send(Valid tok);
      //event_mem.recordEvent(Valid zeroExtend(tok.index));
    end
    else
    begin
      port_to_wb.send(Invalid);
      //event_mem.recordEvent(Invalid);
      stall_count <= `MEM_Miss_Penalty;
      stall_tok <= tok;
    end
    
  endrule

  rule memStall (cc.running && stall_count > 0);
  
    stall_count <= stall_count - 1;
    
    if (stall_count == 1)
    begin
      port_to_wb.send(Valid stall_tok);
      //event_mem.recordEvent(Valid zeroExtend(stall_tok.index));
    end
    else
    begin
      port_to_wb.send(Invalid);
      //event_mem.recordEvent(Invalid);
    end
  
  endrule


endmodule

module [HASim_Module] mk5stage_WB#(CommandCenter cc)
    //interface:
                ();

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
  
  rule lcoReq (cc.running);
  
    let mtok <- port_from_mem.receive();
    
    case (mtok) matches
      tagged Invalid:
      begin
	//event_wb.recordEvent(Invalid);
      end
      tagged Valid .tok:
      begin
        $display("REQ:LCO");
        fp_lco_req.send(tuple2(tok, ?));
      end
    endcase
  
  endrule
   
  rule gcoReq (cc.running);
  
    match {.tok, .*} <- fp_lco_resp.receive();
    $display("REQ:GCO");
    fp_gco_req.send(tuple2(tok, ?));
  endrule
  
  rule gcoResp (cc.running);
  
    match {.tok, .*}  <- fp_gco_resp.receive();
    //event_wb.recordEvent(Valid zeroExtend(tok.index));
  
    case (cc.getStopToken) matches
      tagged Valid .t:
        if (t == tok)
	  cc.stop();
      default:
        noAction;
    endcase
    
  endrule

endmodule 



module [HASim_Module] mkChip 
    //interface:
                (TModule#(Command, Response));


  CommandCenter cc <- mkCommandCenter();
  
  let fet <- mk5stage_FET(cc);
  let dec <- mk5stage_DEC(cc);
  let exe <- mk5stage_EXE(cc);
  let mem <- mk5stage_MEM(cc);
  let wb  <- mk5stage_WB(cc);

  Reg#(Bool) ran <- mkReg(False);
 
  method Action exec(Command c);

    cc.start();
    ran <= True;
    
  endmethod

  method ActionValue#(Response) response() if (ran && !cc.running);
  
    ran <= False;
    return RESP_DoneRunning;

  endmethod


endmodule
