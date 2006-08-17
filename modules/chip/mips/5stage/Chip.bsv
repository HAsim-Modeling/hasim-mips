import ConfigReg::*;
import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;
import LFSR::*;

import HASim::*;
import Debug::*;
import Events::*;
import ISA::*;

//XXX chances are < 127
`define TOK_Latency 0
`define FET_Hit_Latency 1
`define FET_Miss_Latency 10
`define FET_Hit_Chance 13
`define DEC_Latency 1
`define EXE_Latency 1
`define MEM_Hit_Latency 1
`define MEM_Miss_Latency 10
`define MEM_Hit_Chance 64
`define LCO_Latency 0
`define GCO_Latency 1

typedef Bit#(1) Epoch;

module [HASim_Module] mkChip 
    //interface:
                (TModule#(Command, Response));

  Reg#(Bit#(32)) hostCC <- mkReg(0);
  Reg#(Tick) baseTick <- mkReg(0);
  Reg#(Tick) fetStall <- mkReg(0);

  Reg#(Maybe#(Token)) mstopToken <- mkConfigReg(Nothing);
  
  Reg#(Bool) running <- mkReg(False);
  Reg#(Bool) ran <- mkReg(False);
  Reg#(Addr) pc <- mkReg(0);
  
  //for killing
  Reg#(Epoch) epoch <- mkReg(0);
  Reg#(Bool) killing <- mkReg(False);
  Reg#(Token) kill_tok <- mkRegU();

  //Pseudo-randomness
  LFSR#(Bit#(7)) fet_rand <- mkFeedLFSR(7'b1001110);
  LFSR#(Bit#(7)) mem_rand <- mkFeedLFSR(7'b0010101);

  FIFO#(Tick)                  tokQ     <- mkFIFO();
  FIFO#(Tuple2#(Token, Tick))  tok2fetQ <- mkFIFO();
  FIFO#(Tuple2#(Token, Tick))  fet2decQ <- mkFIFO();
  FIFO#(Tuple2#(Token, Tick))  dec2exeQ <- mkFIFO();
  FIFO#(Tuple2#(Token, Tick))  exe2memQ <- mkFIFO();
  FIFO#(Tuple2#(Token, Tick))  mem2lcoQ <- mkFIFO();
  FIFO#(Tuple2#(Token, Tick))  lco2gcoQ <- mkFIFO();

  //********* Connections *********//
  
  Connection_Client#(Bit#(8), Token)
  //...
  link_to_tok <- mkConnection_Client("fp_tok");
  
  Connection_Client#(Tuple2#(Token, Addr),
                     Tuple2#(Token, Inst))
  //...
  link_to_fet <- mkConnection_Client("fp_fet");
  
  Connection_Client#(Tuple2#(Token, void),
                     Tuple2#(Token, DepInfo))
  //...
  link_to_dec <- mkConnection_Client("fp_dec");
  
  Connection_Client#(Tuple2#(Token, void),
                     Tuple2#(Token, InstResult))
  //...
  link_to_exe <- mkConnection_Client("fp_exe");
  
  Connection_Client#(Tuple2#(Token, void),
                     Tuple2#(Token, void))
  //...
  link_to_mem <- mkConnection_Client("fp_mem");
  
  Connection_Client#(Tuple2#(Token, void),
                     Tuple2#(Token, void))
  //...
  link_to_lco <- mkConnection_Client("fp_lco");
  
  Connection_Client#(Tuple2#(Token, void),
                     Tuple2#(Token, void))
  //...
  link_to_gco <- mkConnection_Client("fp_gco");

  //For killing
  
  Connection_Send#(Token) 
  //...
        link_rewindToToken <- mkConnection_Send("fp_rewindToToken");

  Connection_Send#(Token) 
  //...
        link_memstate_kill <- mkConnection_Send("fp_memstate_kill");

  Connection_Send#(Token) 
  //...
        link_tok_kill <- mkConnection_Send("fp_tok_kill");

  Connection_Send#(Token) 
  //...
        link_fet_kill <- mkConnection_Send("fp_fet_kill");
	
  Connection_Send#(Token) 
  //...
        link_dec_kill <- mkConnection_Send("fp_dec_kill");

  Connection_Send#(Token) 
  //...
        link_exe_kill <- mkConnection_Send("fp_exe_kill");

  Connection_Send#(Token) 
  //...
        link_mem_kill <- mkConnection_Send("fp_mem_kill");
	
  Connection_Send#(Token) 
  //...
        link_lco_kill <- mkConnection_Send("fp_lco_kill");

  Connection_Send#(Token) 
  //...
        link_gco_kill <- mkConnection_Send("fp_gco_kill");

  //Events
/*
  EventRecorder 
  //...
        event_fet <- mkEventRecorder("fet");

  EventRecorder 
  //...
        event_dec <- mkEventRecorder("dec");

  EventRecorder 
  //...
        event_exe <- mkEventRecorder("exe");

  EventRecorder 
  //...
        event_mem <- mkEventRecorder("mem");

  EventRecorder 
  //...
        event_com <- mkEventRecorder("com");
*/

  rule count (True);
    hostCC <= hostCC + 1;
  endrule

  rule tokenReq (running);
  
    debug(2, $display("[%d] TOKG Requesting a new token. Began on model CC %0d.", hostCC, baseTick));
  
  
    tokQ.enq(baseTick);
    baseTick <= baseTick + 1;
  
    link_to_tok.makeReq(17);
    
  endrule
  
  rule tokenGen (running);
  
    let tok <- link_to_tok.getResp();
    let inf = TokInfo {epoch: epoch, ctxt: ?};
    let tok2 = Token {index: tok.index, info: inf};
    
    let old_tick = tokQ.first();
    tokQ.deq();
  
    debug(2, $display("[%d] TOKR/FETG Fetching token %0d at address %h", hostCC, tok2.index, pc));
    
    pc <= pc + 1;
  
    let tick = old_tick + `TOK_Latency;
  
    tok2fetQ.enq(tuple2(tok2, tick));
  
    link_to_fet.makeReq(tuple2(tok2, pc));
  
  endrule

  rule fetch (running);
  
    match {.tok, .inst} <- link_to_fet.getResp();
    
    debug(2, $display("[%d] FETR/DECG Decoding token %0d", hostCC, tok.index));
    Bool new_killing = killing;
    
    match {.cur_tok, .old_tick} = tok2fetQ.first();
    tok2fetQ.deq();

    if ((killing) && (tok.info.epoch != epoch)) //kill it
    begin
      debug(2, $display("[%d] FETR Rolling back wrong-path token %0d", hostCC, tok.index));
      link_dec_kill.send(tok);
    end
    else //continue to execute it
    begin
    
      if (killing) //stop killing
      begin
	debug(2, $display("[%d] FETR Returning to right-path on token %0d", hostCC, tok.index));
	new_killing = False;
	link_rewindToToken.send(kill_tok);
	link_tok_kill.send(kill_tok);
      end
  
    let isHit = fet_rand.value < `FET_Hit_Chance;
    let fet_lat = isHit ? `FET_Hit_Latency : `FET_Miss_Latency;
    fet_rand.next();
    
    let tick = old_tick + fet_lat + fetStall;
    
    if (!isHit)
    begin
      debug(2, $display("[%h] ICACHE MISS on token %0d on model CC %d", hostCC, tok.index, old_tick));
      fetStall <= fetStall + `FET_Miss_Latency;
    end

    if (tok != cur_tok)
       $display ("[%h] FET ERROR: Mismatched token. Expected: %0d, Received: %0d", hostCC, cur_tok, tok);

    fet2decQ.enq(tuple2(tok, tick));

    link_to_dec.makeReq(tuple2(tok, ?));
    killing <= new_killing;
    //event_fet.recordEvent(tok.index);

    end

  endrule

  rule decode (running);
  
    match {.tok, .deps} <- link_to_dec.getResp();
    
    debug(2, $display("[%d] DECR/EXEG Decode Responded with token %0d.", hostCC, tok.index));
    
    match {.cur_tok, .old_tick} = fet2decQ.first();
    fet2decQ.deq();
    
    Bool new_killing = killing;

    if ((killing) && (tok.info.epoch != epoch)) //kill it
    begin
      debug(2, $display("[%d] DECR Rolling back wrong-path token %0d", hostCC, tok.index));
      link_exe_kill.send(tok);
    end
    else //continue to execute it
    begin
    
    let tick = old_tick + `DEC_Latency;
    
    if (tok != cur_tok)
       $display ("[%h] DEC ERROR: Mismatched token. Expected: %0d, Received: %0d", hostCC, cur_tok, tok);

    case (deps.dep_dest) matches
      tagged Valid {.rname, .prname}:
	debug(2, $display("\tDestination: (%d, %d)", rname, prname));
      tagged Invalid:
	debug(2, $display("\tNo destination."));
    endcase
    case (deps.dep_src1) matches
      tagged Valid {.rname, .prname}:
        debug(2, $display("\tSource 1: (%d, %d)", rname, prname));
      tagged Invalid:
        debug(2, $display("\tNo Source 1."));
    endcase
    case (deps.dep_src2) matches
      tagged Valid {.rname, .prname}:
        debug(2, $display("\tSource 2: (%d, %d)", rname, prname));
      tagged Invalid:
        debug(2, $display("\tNo Source 2."));
    endcase  
    
    killing <= new_killing;
    dec2exeQ.enq(tuple2(tok, tick));
    link_to_exe.makeReq(tuple2(tok, ?));
    //event_dec.recordEvent(zeroExtend(tok.index));
    end
  endrule

  rule execute (running);
  
    match {.tok, .res} <- link_to_exe.getResp();
    Bool new_killing = killing;
   
    debug(2, $display("[%d] Executing token %0d", hostCC, tok.index));

    match {.cur_tok, .old_tick} = dec2exeQ.first();
    dec2exeQ.deq();
    
    if (tok != cur_tok)
       $display ("[%h] EXER/MEMG ERROR: Mismatched token. Expected: %0d, Received: %0d", hostCC, cur_tok, tok);

    if ((killing) && (tok.info.epoch != epoch)) //kill it
    begin
      debug(2, $display("[%d] EXER Rolling back wrong-path token %0d", hostCC, tok.index));
      link_mem_kill.send(tok);
    end
    else //continue to execute it
    begin
    
      let tick = old_tick + `EXE_Latency;

      case (res) matches
	tagged RBranchTaken .addr:
	  begin
	    debug(2, $display("[%d] Branch taken to address %h on Model CC: %0d", hostCC, addr, tick));
	    pc <= addr;

            //kill wrongpath FP tokens
	    epoch <= epoch + 1;
	    new_killing = True;
	    kill_tok <= tok;

	  end
	tagged RBranchNotTaken:
          noAction;
	tagged RNop:
          noAction;
	tagged RTerminate:
          case (mstopToken) matches
	    Invalid:
  	      mstopToken <= Valid tok;
	    default:
	      noAction;
	  endcase
      endcase

      killing <= new_killing;
      exe2memQ.enq(tuple2(tok, tick));
      link_to_mem.makeReq(tuple2(tok, ?));
      //event_exe.recordEvent(zeroExtend(tok.index));
    end
    
  endrule

  rule memory (running);
  
    match {.tok, .*} <- link_to_mem.getResp();
    
    debug(2, $display("[%d] MEMR/LCOG Memory responded with token %0d.", hostCC, tok.index));
    
    match {.cur_tok, .old_tick} = exe2memQ.first();
    exe2memQ.deq();
    
    if (tok != cur_tok)
       $display ("[%h] MEM ERROR: Mismatched token. Expected: %0d, Received: %0d", hostCC, cur_tok, tok);

    let isHit = True; //mem_rand.value < `MEM_Hit_Chance;
    let mem_lat = isHit ? `MEM_Hit_Latency : `MEM_Miss_Latency;
    mem_rand.next();
    
    let tick = old_tick + mem_lat;

    mem2lcoQ.enq(tuple2(tok, tick));
    link_to_lco.makeReq(tuple2(tok, ?));
    //event_mem.recordEvent(zeroExtend(tok.index));
    
  endrule

  rule local_commit (running);
  
    match {.tok, .*} <- link_to_lco.getResp();
    
    debug(2, $display("[%d] LCOR/GCOG Local commit responded with token %0d.", hostCC, tok.index));
    
    match {.cur_tok, .old_tick} = mem2lcoQ.first();
    mem2lcoQ.deq();
    
    if (tok != cur_tok)
       $display ("[%h] LCO ERROR: Mismatched token. Expected: %0d, Received: %0d", hostCC, cur_tok, tok);

    let tick = old_tick + `LCO_Latency;
    
    lco2gcoQ.enq(tuple2(tok, tick));
    link_to_gco.makeReq(tuple2(tok, ?));
    
  endrule

  rule global_commit (running);
  
    match {.tok, .*} <- link_to_gco.getResp();
    
    debug(2, $display("[%d] GCOR Global commit responded with token %0d.", hostCC, tok.index));
    
    match {.cur_tok, .old_tick} = lco2gcoQ.first();
    lco2gcoQ.deq();

    if (tok != cur_tok)
       $display ("[%h] GCO ERROR: Mismatched token. Expected: %0d, Received: %0d", hostCC, cur_tok, tok);

    let tick = old_tick + `GCO_Latency;
        
    debug(1, $display("[%d] Finished token %0d at model cycle %0d", hostCC, tok.index, tick));
  
    case (mstopToken) matches
      tagged Valid .t:
        if (t == tok)
	  running <= False;
      default:
        noAction;
    endcase
    
    //event_com.recordEvent(zeroExtend(tok.index));

  endrule
    
  method Action exec(Command c);

    running <= True;
    ran <= True;
    
  endmethod

  method ActionValue#(Response) response() if (ran && !running);
    ran <= False;
    return RESP_DoneRunning;

  endmethod


endmodule
