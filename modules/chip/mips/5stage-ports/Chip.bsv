import ConfigReg::*;
import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;
import LFSR::*;

import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_isa::*;

import CommandCenter::*;


//XXX chances are out of 127
`define FET_Miss_Penalty 10
`define FET_Hit_Chance 64
`define DEC_Is_Bypassed True
`define MEM_Hit_Chance 64
`define MEM_Miss_Penalty 10

typedef TokEpoch Epoch;

typedef enum 
{
  FET_Ready,
  FET_GetInst,
  FET_Finish
}
  FET_State
    deriving (Eq, Bits);

module [HASim_Module] mk5stage_FET#(CommandCenter cc)
    //interface:
                ();

  //Local State

  Reg#(Addr)              pc <- mkReg(32'h00001000);
  Reg#(Epoch)          epoch <- mkReg(0);
  Reg#(Token)      stall_tok <- mkRegU;
  Reg#(Bit#(16)) stall_count <- mkReg(0);
  Reg#(Bool)        stalling <- mkReg(False);
  Reg#(FET_State)      state <- mkReg(FET_Ready);
  
  //Pseudo-randomness
  LFSR#(Bit#(7)) lfsr <- mkFeedLFSR(7'b1001110);

  //Connections to FP
  Connection_Send#(Bit#(8))   fp_tok_req  <- mkConnection_Send("fp_tok_req");
  Connection_Receive#(Token)  fp_tok_resp <- mkConnection_Receive("fp_tok_resp");
  Connection_Send#(Token)     fp_tok_kill <- mkConnection_Send("fp_tok_kill");
  
  Connection_Send#(Tuple2#(Token, Addr))           fp_fet_req  <- mkConnection_Send("fp_fet_req");
  Connection_Receive#(Tuple2#(Token, PackedInst))  fp_fet_resp <- mkConnection_Receive("fp_fet_resp");

  Connection_Send#(Token)     fp_fet_kill <- mkConnection_Send("fp_fet_kill");
  Connection_Send#(Token)     fp_dec_kill <- mkConnection_Send("fp_dec_kill");
      
  //Events
  //EventRecorder event_fet <- mkEventRecorder("Fetch");
  
  //Stats
  //Stat stat_fet <- mkStatCounter("Fetch");
    
  //Incoming Ports
  Port_Receive#(Tuple2#(Token, Addr)) port_from_ic <- mkPort_Receive("fet_setPC", 1);

  //Outgoing Ports
  Port_Send#(Token) port_to_dec <- mkPort_Send("fet_to_dec");


  rule beginFetch (cc.running && state == FET_Ready);
    
    let mtup <- port_from_ic.receive();
    
    case (mtup) matches
      tagged Invalid: //We're on the right path
        noAction;
      tagged Valid {.ktok, .new_pc}: //Re-steer
      begin
	epoch <= epoch + 1;
	pc <= new_pc;
        fp_tok_kill.send(ktok);
      end
    endcase
    
    if (!stalling)
      begin
        $display("REQ:TOK");
	fp_tok_req.send(17); //17 is arbitrarily-chosen bug workaround
	state <= FET_GetInst;

      end
    else
      begin
      
        if (stall_count == 0)
	  begin
            port_to_dec.send(Valid stall_tok);
            //event_fet.recordEvent(Valid zeroExtend(stall_tok.index));
            //stat_fet.incr();
	    stalling <= False;
	  end
	else
	  begin
            port_to_dec.send(Invalid);
            //event_fet.recordEvent(Invalid);
            stall_count <= stall_count - 1;
	  end
      end

   endrule
   
   rule fetchInst (state == FET_GetInst);

     let tok <- fp_tok_resp.receive();

     let inf = TokInfo {epoch: epoch, ctxt: ?};
     let tok2 = Token {index: tok.index, info: inf};
      
     $display("REQ:FET:%d:0x%h", tok.index, pc);
     fp_fet_req.send(tuple2(tok2, pc));
      
     pc <= pc + 4;
     
     state <= FET_Finish;
     
   endrule


   rule finishFetch (state == FET_Finish);
   
     match {.tok, .inst} <- fp_fet_resp.receive();

     let isHit = lfsr.value < `FET_Hit_Chance;
     lfsr.next();

     if (isHit)
     begin
     
       port_to_dec.send(Valid tok);
       //event_fet.recordEvent(Valid zeroExtend(tok.index));
       //stat_fet.incr();

     end
     else
     begin
       port_to_dec.send(Invalid);
       //event_fet.recordEvent(Invalid);
       stall_count <= `FET_Miss_Penalty;
       stall_tok   <= tok;
       stalling    <= True;
     end
     
     state       <= FET_Ready;
     
   endrule

endmodule

module [HASim_Module] mk5stage_DEC#(CommandCenter cc)
    //interface:
                ();


  //Local State
  Reg#(Bit#(2))   stall_count <- mkReg(0);
  Reg#(DepInfo)   stall_deps  <- mkRegU();
  Reg#(Token)     stall_tok   <- mkRegU();
  Reg#(Bool)      in_flight   <- mkReg(False);
  
  //Scoreboard
  Reg#(Maybe#(DepInfo)) exe_stall_info <- mkReg(Invalid);
  Reg#(Maybe#(DepInfo)) mem_stall_info <- mkReg(Invalid);
  Reg#(Maybe#(DepInfo))  wb_stall_info <- mkReg(Invalid);

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
        port_to_exe.send(Invalid);
	//event_dec.recordEvent(Invalid);
	shiftStalls(Invalid);
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
      in_flight <= False;
    end
    
    stall_count <= new_stall;
    
  endrule

  rule decode_stall (cc.running && stall_count > 0 && in_flight);
  
    stall_count <= stall_count - 1;
    port_to_exe.send(Invalid);
    //event_dec.recordEvent(Invalid);
    shiftStalls(Invalid);

  endrule

endmodule

module [HASim_Module] mk5stage_EXE#(CommandCenter cc)
    //interface:
                ();
  
  //Local State
  Reg#(Epoch)  epoch     <- mkReg(0);
  Reg#(Bool)   in_flight <- mkReg(False);

  //Connections to FP
  
  Connection_Send#(Tuple2#(Token, void))           fp_exe_req  <- mkConnection_Send("fp_exe_req");
  Connection_Receive#(Tuple2#(Token, InstResult))  fp_exe_resp <- mkConnection_Receive("fp_exe_resp");

  Connection_Send#(Token)     fp_exe_kill <- mkConnection_Send("fp_exe_kill");
  
  Connection_Send#(Token)     fp_rewindToToken <- mkConnection_Send("fp_rewindToToken");

  //Events
  //EventRecorder event_exe <- mkEventRecorder("Execute");
  
  //Incoming Ports
  Port_Receive#(Token) port_from_dec <- mkPort_Receive("dec_to_exe", 1);

  //Outgoing Ports
  Port_Send#(Token)               port_to_mem <- mkPort_Send("exe_to_mem");
  Port_Send#(Tuple2#(Token, Addr)) port_to_ic <- mkPort_Send("fet_setPC");

  rule executeReq (cc.running && !in_flight);
  
    let mtok <- port_from_dec.receive();
    
    case (mtok) matches
      tagged Invalid:
      begin
        port_to_mem.send(Invalid);
	//event_exe.recordEvent(Invalid);
	port_to_ic.send(Invalid);
      end
      tagged Valid .tok:
      begin
	if (tok.info.epoch != epoch) //kill it
	begin
	  fp_exe_kill.send(tok);
          //event_exe.recordEvent(Invalid);
          port_to_mem.send(Invalid);
	  port_to_ic.send(Invalid);
	end
	else //continue to execute it
	begin
          $display("REQ:EXE:%d", tok.index);
          fp_exe_req.send(tuple2(tok, ?));
	  in_flight <= True;
        end
      end
    endcase
  
  endrule

  rule executeResp (cc.running && in_flight);
  
    match {.tok, .res} <- fp_exe_resp.receive();

    case (res) matches
      tagged RBranchTaken .addr:
	begin
	  $display("Branch taken!");
	  epoch <= epoch + 1;
	  fp_rewindToToken.send(tok);
	  port_to_ic.send(Valid tuple2(tok, addr));
	end
      tagged RBranchNotTaken:
	begin
	  port_to_ic.send(Invalid);
	  $display("Branch not taken!");
	end
      tagged RNop:
	port_to_ic.send(Invalid);
      tagged RTerminate .pf:
      begin
	port_to_ic.send(Invalid);
	$display("Setting Termination!");
	cc.setPassFail(pf);
        case (cc.getStopToken) matches
	  Invalid:
  	    cc.setStopToken(tok);
	  default:
	    noAction;
	endcase
      end
    endcase

    port_to_mem.send(Valid tok);
    //event_exe.recordEvent(Valid zeroExtend(tok.index));
    in_flight <= False;
    
  endrule


endmodule

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
  FIFO#(Token)    buff        <- mkSizedFIFO(`MEM_Miss_Penalty);
  
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
            port_to_wb.send(Valid stall_tok);
	    stalling <= False;
	  end
	else
	  begin
	    stall_count <= stall_count - 1;
	    port_to_wb.send(Invalid);
	  end
      end
    else
      begin

	case (mtok) matches
	  tagged Invalid:
	  begin
            port_to_wb.send(Invalid);
            //event_mem.recordEvent(Invalid);
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

	port_to_wb.send(Valid tok);
	//event_mem.recordEvent(Valid zeroExtend(tok.index));

      end
    else
      begin
	port_to_wb.send(Invalid);
	//event_mem.recordEvent(Invalid);
	stall_count <= `MEM_Miss_Penalty;
	stall_tok   <= tok;
	stalling    <= True;
      end

    state <= MEM_Ready;
    
  endrule

endmodule

module [HASim_Module] mk5stage_WB#(CommandCenter cc)
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
	//event_wb.recordEvent(Invalid);
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



module [HASim_Module] mkChip 
    //interface:
                ();


  CommandCenter cc <- mkCommandCenter();
  Connection_Server#(Command, Response)  link_controller <- mkConnection_Server("controller_to_tp");
  Reg#(Bool) ran <- mkReg(False);

  let fet <- mk5stage_FET(cc);
  let dec <- mk5stage_DEC(cc);
  let exe <- mk5stage_EXE(cc);
  let mem <- mk5stage_MEM(cc);
  let wb  <- mk5stage_WB(cc);

  rule startup (True);
  
    let cmd <- link_controller.getReq();
    
    case (cmd) matches
      tagged COM_RunProgram:
      begin
        cc.start();
	ran <= True;
      end
      default:
        noAction;
    endcase
  
  endrule

  rule finishup (ran && !cc.running);
  
    link_controller.makeResp(RESP_DoneRunning cc.getPassFail());
    ran <= False;
    
  endrule

endmodule
