import Vector::*;

//HASim library imports
import hasim_common::*;
import soft_connections::*;
import hasim_modellib::*;
import hasim_local_controller::*;

//Model-specific imports
import hasim_isa::*;

`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Timing"



//************************* Simple Timing Partition ***********************//
//                                                    `                    //
// This is about the simplest timing partition you can conceive of. It     //
// simply fetches one instruction at a time, executes it, then moves to    //
// the next instruction. This can serve as a good mechanism to verify      //
// the functional partition and can serve as a "golden model" for more     //
// complex timing partitions.                                              //
//                                                    `                    //
//*************************************************************************//



typedef enum 
{ 
  TOK, FET, DEC, EXE, MEM, LCO, GCO 
} 
  Stage deriving (Eq, Bits);

`define MODULE_NAME "mkCPU"
module [HASim_Module] mkCPU
     //interface:
                 ();

  
  //********* State Elements *********//
  
  //Have we made a req to FP and are waiting for a response?
  Reg#(Bool) madeReq <- mkReg(False);
  
  //The current stage
  Reg#(Stage) stage <- mkReg(TOK);
  
  //Current token (response from TOK stage)
  Reg#(Token) cur_tok <- mkRegU();
  
  //Current instruction (response from FET stage)
  Reg#(Inst)  cur_inst <- mkRegU();
  
  //The Program Counter
  Reg#(Addr) pc <- mkReg(32'h00001000);
  
  //The actual Clock Cycle, for debugging messages
  Reg#(Bit#(32)) hostCC <- mkReg(0);
  
  //The simulation Clock Cycle, or "tick"
  Reg#(Tick) baseTick <- mkReg(0);
  
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

  //For killing. UNUSED
  
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
  
  EventRecorder event_com <- mkEventRecorder("com");
  
  Vector#(0, Port_Control) inports = newVector();
  Vector#(0, Port_Control) outports = newVector();
  
  LocalController local_ctrl <- mkLocalController(inports, outports);
  
  //********* Rules *********//

  //count
  rule count (True);
  
    hostCC <= hostCC + 1;
  
  endrule
  
  //process
  
  rule process (local_ctrl.running());
    debug_rule("process");
    
    case (stage)
      TOK:
      begin
        debug_case("stage", "TOK");
	
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    
	    //Request a token
	    debug(2, $display("[%d] Requesting a new token on model cycle %0d.", hostCC, baseTick));
	    link_to_tok.makeReq(17);
	    
	    madeReq <= True;
	    
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
	    //Get the response
	    let tok = link_to_tok.getResp();
	    link_to_tok.deq();
	    
            tok.timep_info = TIMEP_TokInfo{epoch: 0, scratchpad: 0};

	    debug(2, $display("[%d] TOK Responded with token %0d.", hostCC, tok.index));
	    
	    cur_tok <= tok;
	    
	    stage <= FET;
	    madeReq <= False;
	  end
      end
      FET:
      begin
        debug_case("stage", "FET");
	
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    
	    //Fetch next instruction
	    debug(2, $display("[%d] Fetching token %0d at address 0x%h.", hostCC, cur_tok.index, pc));
            link_to_fet.makeReq(tuple2(cur_tok, pc));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
	    //Get the response
            match {.tok, .inst} = link_to_fet.getResp();
	    link_to_fet.deq();

	    debug(2, $display("[%d] FET Responded with token %0d.", hostCC, tok.index));
	    
	    if (tok.index != cur_tok.index) $display ("FET ERROR: Token Mismatch. Expected: %0d Received: %0d", cur_tok.index, tok.index);
	    
	    stage <= DEC;
	    madeReq <= False;
	  end
      end
      DEC:
      begin
        debug_case("stage", "DEC");
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    
	    //Decode current inst
	    debug(2, $display("[%d] Decoding token %0d.", hostCC, cur_tok.index));
            link_to_dec.makeReq(tuple2(cur_tok, ?));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
            match {.tok, .deps} = link_to_dec.getResp();
	    link_to_dec.deq();

	    debug(2, $display("[%d] DEC Responded with token %0d.", hostCC, tok.index));
	    
	    if (tok.index != cur_tok.index) $display ("DEC ERROR: Token Mismatch. Expected: %0d Received: %0d", cur_tok.index, tok.index);
	    
	    stage <= EXE;
	    madeReq <= False;
	  end
      end
      EXE:
      begin
        debug_case("stage", "EXE");
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    //Execute instruction
	    debug(2, $display("[%d] Executing token %0d", hostCC, cur_tok.index));
            link_to_exe.makeReq(tuple2(cur_tok, ?));
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
            match {.tok, .res} = link_to_exe.getResp();
	    link_to_exe.deq();

	    debug(2, $display("[%d] EXE Responded with token %0d.", hostCC, tok.index));
	    
	    if (tok.index != cur_tok.index) $display ("EXE ERROR: Token Mismatch. Expected: %0d Received: %0d", cur_tok.index, tok.index);
	   	
	    case (res) matches
	      tagged RBranchTaken .addr:
	      begin
	        debug(2, $display("Branch taken to address %h", addr));
	   	pc <= addr;
	      end
              tagged RBranchNotTaken .addr:
	      begin
	        debug(2, $display("Branch not taken"));
	   	pc <= pc + 4;
	      end
              tagged RNop:
	      begin
	        debug(2, $display("Nop"));
	   	pc <= pc + 4;
	      end
              tagged RTerminate .pf:
	      begin
	        debug(2, $display("Terminating Execution"));
                local_ctrl.endProgram(pf);
	      end
	    endcase
	    
	    stage <= MEM;
	    madeReq <= False;
	  end
      end
      MEM:
      begin
        debug_case("stage", "MEM");
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    
	    //Request memory ops
	    debug(2, $display("[%d] Memory ops for token %0d", hostCC, cur_tok.index));
            link_to_mem.makeReq(tuple2(cur_tok, ?));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
	    match {.tok, .*} = link_to_mem.getResp();
	    link_to_mem.deq();

	    debug(2, $display("[%d] MEM Responded with token %0d.", hostCC, tok.index));
	    
	    if (tok.index != cur_tok.index) $display ("MEM ERROR: Token Mismatch. Expected: %0d Received: %0d", cur_tok.index, tok.index);
	    
	    stage <= LCO;
	    madeReq <= False;
	  end
      end
      LCO:
      begin
        debug_case("stage", "LCO");
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    
	    //Request memory ops
	    debug(2, $display("[%d] Locally committing token %0d.", hostCC, cur_tok.index));
            link_to_lco.makeReq(tuple2(cur_tok, ?));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
  
            match {.tok, .*} = link_to_lco.getResp();
	    link_to_lco.deq();

	    debug(2, $display("[%d] LCO Responded with token %0d.", hostCC, tok.index));
	    
	    if (tok.index != cur_tok.index) $display ("LCO ERROR: Token Mismatch. Expected: %0d Received: %0d", cur_tok.index, tok.index);
	    
	    stage <= GCO;
	    madeReq <= False;
	  end
      end
      GCO:
      begin
        debug_case("stage", "GCO");
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    
	    //Request memory ops
	    debug(2, $display("[%d] Globally committing token %0d", hostCC, cur_tok.index));
            link_to_gco.makeReq(tuple2(cur_tok, ?));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
            match {.tok, .*} = link_to_gco.getResp();
	    link_to_gco.deq();

	    debug(2, $display("[%d] GCO Responded with token %0d.", hostCC, tok.index));
	    
	    if (tok.index != cur_tok.index) $display ("GCO ERROR: Token Mismatch. Expected: %0d Received: %0d", cur_tok.index, tok.index);
	    
	    debug(1, $display("Committed token %0d on model cycle %0d.", cur_tok.index, baseTick));
	    //event_com.recordEvent(tagged Valid zeroExtend(cur_tok.index));
	    
	    stage <= TOK;
	    madeReq <= False;
	    baseTick <= baseTick + 1;
	  end
      end
    endcase    
  endrule
  
endmodule
`undef MODULE_NAME
