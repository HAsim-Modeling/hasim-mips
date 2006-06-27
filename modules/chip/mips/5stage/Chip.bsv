import ConfigReg::*;
import GetPut::*;
import ClientServer::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import Debug::*;
import ISA::*;

`define TOK_Latency 0
`define FET_Latency 3
`define DEC_Latency 1
`define EXE_Latency 2
`define MEM_Latency 3
`define LCO_Latency 0
`define GCO_Latency 1

module [HASim_Module] mkChip 
    //interface:
                (TModule#(Command, Response));

  Reg#(Bit#(32)) hostCC <- mkReg(0);
  Reg#(Tick) baseTick <- mkReg(0);

  Reg#(Maybe#(Token)) mstopToken <- mkConfigReg(Nothing);
  
  Reg#(Bool) running <- mkReg(False);
  Reg#(Bool) ran <- mkReg(False);
  Reg#(Addr) pc <- mkReg(0);

  FIFO#(Tick)                  tokQ     <- mkFIFO();
  FIFO#(Tuple2#(Token, Tick))  tok2fetQ <- mkFIFO();
  FIFO#(Tuple2#(Token, Tick))  fet2decQ <- mkFIFO();
  FIFO#(Tuple2#(Token, Tick))  dec2exeQ <- mkFIFO();
  FIFO#(Tuple2#(Token, Tick))  exe2memQ <- mkFIFO();
  FIFO#(Tuple2#(Token, Tick))  mem2lcoQ <- mkFIFO();
  FIFO#(Tuple2#(Token, Tick))  lco2gcoQ <- mkFIFO();

  //********* Connections *********//
  
  Connection_Client#(Tuple2#(Bit#(8), Tick), Token)
  //...
  link_to_tok <- mkConnection_Client("fp_tok");
  
  Connection_Client#(Tuple3#(Token, Tick, Addr),
                     Tuple2#(Token, Inst))
  //...
  link_to_fet <- mkConnection_Client("fp_fet");
  
  Connection_Client#(Tuple3#(Token, Tick, void),
                     Tuple2#(Token, DepInfo))
  //...
  link_to_dec <- mkConnection_Client("fp_dec");
  
  Connection_Client#(Tuple3#(Token, Tick, void),
                     Tuple2#(Token, InstResult))
  //...
  link_to_exe <- mkConnection_Client("fp_exe");
  
  Connection_Client#(Tuple3#(Token, Tick, void),
                     Tuple2#(Token, void))
  //...
  link_to_mem <- mkConnection_Client("fp_mem");
  
  Connection_Client#(Tuple3#(Token, Tick, void),
                     Tuple2#(Token, void))
  //...
  link_to_lco <- mkConnection_Client("fp_lco");
  
  Connection_Client#(Tuple3#(Token, Tick, void),
                     Tuple2#(Token, void))
  //...
  link_to_gco <- mkConnection_Client("fp_gco");

  rule count (True);
    hostCC <= hostCC + 1;
  endrule

  rule tokenReq (running);
  
    debug(2, $display("[%h] Requesting a new token. Began on model CC %0d.", hostCC, baseTick));
  
  
    tokQ.enq(baseTick);
    baseTick <= baseTick + 1;
  
    link_to_tok.makeReq(tuple2(17, baseTick));
    
  endrule
  
  rule tokenGen (running);
  
    let tok <- link_to_tok.getResp();
    
    let old_tick = tokQ.first();
    tokQ.deq();
  
    debug(2, $display("[%h] Fetching token %0d at address %h", hostCC, old_tick, tok, pc));
    
    pc <= pc + 1;
  
    let tick = old_tick + `TOK_Latency;
  
    tok2fetQ.enq(tuple2(tok, tick));
  
    link_to_fet.makeReq(tuple3(tok, tick, pc));
  
  endrule

  rule fetch (running);
  
    match {.tok, .inst} <- link_to_fet.getResp();
    
    debug(2, $display("[%h] Decoding token %0d", hostCC, tok));
    
    match {.cur_tok, .old_tick} = tok2fetQ.first();
    tok2fetQ.deq();
    
    let tick = old_tick + `FET_Latency;
    
    if (tok != cur_tok)
       $display ("[%h] FET ERROR: Mismatched token. Expected: %0d, Received: %0d", hostCC, cur_tok, tok);

    fet2decQ.enq(tuple2(tok, tick));
       
    link_to_dec.makeReq(tuple3(tok, tick, ?));
    
  endrule

  rule decode (running);
  
    match {.tok, .deps} <- link_to_dec.getResp();
    
    debug(2, $display("[%h] DEC Responded with token %0d.", hostCC, tok));
    
    match {.cur_tok, .old_tick} = fet2decQ.first();
    fet2decQ.deq();
    
    let tick = old_tick + `DEC_Latency;
    
    if (tok != cur_tok)
       $display ("[%h] DEC ERROR: Mismatched token. Expected: %0d, Received: %0d", hostCC, cur_tok, tok);

    case (deps.dep_dest) matches
      tagged Valid {.rname, .prname}:
	debug(2, $display("Destination: (%d, %d)", rname, prname));
      tagged Invalid:
	debug(2, $display("No destination."));
    endcase
    case (deps.dep_src1) matches
      tagged Valid {.rname, .prname}:
        debug(2, $display("Source 1: (%d, %d)", rname, prname));
      tagged Invalid:
        debug(2, $display("No Source 1."));
    endcase
    case (deps.dep_src2) matches
      tagged Valid {.rname, .prname}:
        debug(2, $display("Source 2: (%d, %d)", rname, prname));
      tagged Invalid:
        debug(2, $display("No Source 2."));
    endcase  
    
    dec2exeQ.enq(tuple2(tok, tick));
    link_to_exe.makeReq(tuple3(tok, tick, ?));

  endrule

  rule execute (running);
  
    match {.tok, .res} <- link_to_exe.getResp();
   
    debug(2, $display("[%h] Executing token %0d", hostCC, tok));

    match {.cur_tok, .old_tick} = dec2exeQ.first();
    dec2exeQ.deq();
    
    if (tok != cur_tok)
       $display ("[%h] EXE ERROR: Mismatched token. Expected: %0d, Received: %0d", hostCC, cur_tok, tok);

    let tick = old_tick + `EXE_Latency;
    
    case (res) matches
      tagged RBranchTaken .addr:
	begin
	  debug(2, $display("[%h] Branch taken to address %h on Model CC: %0d", hostCC, addr, tick));
	  pc <= addr;
	  
          //XXX kill wrongpath FP tokens here
	  
	end
      tagged RBranchNotTaken:
        noAction;
      tagged RNop:
        noAction;
      tagged RTerminate:
	mstopToken <= Valid tok;
    endcase
    
    exe2memQ.enq(tuple2(tok, tick));
    link_to_mem.makeReq(tuple3(tok, tick, ?));
    
  endrule

  rule memory (running);
  
    match {.tok, .*} <- link_to_mem.getResp();
    
    debug(2, $display("[%h] MEM Responded with token %0d.", hostCC, tok));
    
    match {.cur_tok, .old_tick} = exe2memQ.first();
    exe2memQ.deq();
    
    if (tok != cur_tok)
       $display ("[%h] MEM ERROR: Mismatched token. Expected: %0d, Received: %0d", hostCC, cur_tok, tok);

    let tick = old_tick + `MEM_Latency;
    
    mem2lcoQ.enq(tuple2(tok, tick));
    link_to_lco.makeReq(tuple3(tok, tick, ?));
    
  endrule

  rule local_commit (running);
  
    match {.tok, .*} <- link_to_lco.getResp();
    
    debug(2, $display("[%h] LCO Responded with token %0d.", hostCC, tok));
    
    match {.cur_tok, .old_tick} = mem2lcoQ.first();
    mem2lcoQ.deq();
    
    if (tok != cur_tok)
       $display ("[%h] LCO ERROR: Mismatched token. Expected: %0d, Received: %0d", hostCC, cur_tok, tok);

    let tick = old_tick + `LCO_Latency;
    
    lco2gcoQ.enq(tuple2(tok, tick));
    link_to_gco.makeReq(tuple3(tok, tick, ?));
    
  endrule

  rule global_commit (running);
  
    match {.tok, .*} <- link_to_gco.getResp();
    
    debug(2, $display("[%h] GCO Responded with token %0d.", hostCC, tok));
    
    match {.cur_tok, .old_tick} = lco2gcoQ.first();
    lco2gcoQ.deq();

    if (tok != cur_tok)
       $display ("[%h] GCO ERROR: Mismatched token. Expected: %0d, Received: %0d", hostCC, cur_tok, tok);

    let tick = old_tick + `GCO_Latency;
        
    debug(1, $display("[%h]: finished token %0d at model cycle %0d", hostCC, tok, old_tick));
  
    if ((Valid tok) == mstopToken)
      running <= False;

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
