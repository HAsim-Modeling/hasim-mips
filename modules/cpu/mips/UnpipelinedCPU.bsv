import Vector::*;

//HASim library imports
import hasim_common::*;
import soft_connections::*;
import hasim_modellib::*;
import hasim_local_controller::*;

//Model-specific imports
import hasim_isa::*;


//************************* Simple Timing Partition ***********************//
//                                                                         //
// This is about the simplest timing partition you can conceive of. It     //
// simply fetches one instruction at a time, executes it, then moves to    //
// the next instruction. This can serve as a good mechanism to verify      //
// the functional partition and can serve as a "golden model" for more     //
// complex timing partitions.                                              //
//                                                                         //
//*************************************************************************//



typedef enum 
{ 
  TOK, FET, DEC, EXE, LOA, STO, LCO, GCO 
} 
  Stage deriving (Eq, Bits);

module [HASim_Module] mkCPU
     //interface:
                 ();

  Reg#(File) debug_log <- mkReg(InvalidFile);
  
  //********* State Elements *********//
  
  //Have we made a req to FP and are waiting for a response?
  Reg#(Bool) madeReq <- mkReg(False);
  
  //The current stage
  Reg#(Stage) stage <- mkReg(TOK);
  
  //Current TOKEN (response from TOK stage)
  Reg#(TOKEN) cur_tok <- mkRegU();
  
  //Current instruction (response from FET stage)
  Reg#(ISA_INSTRUCTION)  cur_inst <- mkRegU();
  
  //The Program Counter
  Reg#(ISA_ADDRESS) pc <- mkReg(32'h00001000);
  
  //The actual Clock Cycle, for debugging messages
  Reg#(Bit#(32)) hostCC <- mkReg(0);
  
  //The simulation Clock Cycle, or "tick"
  Reg#(Bit#(32)) baseTick <- mkReg(0);
  
  //********* Connections *********//
  
  Connection_Client#(void, TOKEN)
  //...
  link_to_tok <- mkConnection_Client("funcp_newInFlight");
  
  Connection_Client#(Tuple2#(TOKEN, ISA_ADDRESS),
                     Tuple2#(TOKEN, ISA_INSTRUCTION))
  //...
  link_to_fet <- mkConnection_Client("funcp_getInstruction");
  
  Connection_Client#(TOKEN,
                     Tuple2#(TOKEN, ISA_DEPENDENCY_INFO))
  //...
  link_to_dec <- mkConnection_Client("funcp_getDependencies");
  
  Connection_Client#(TOKEN,
                     Tuple2#(TOKEN, ISA_EXECUTION_RESULT))
  //...
  link_to_exe <- mkConnection_Client("funcp_getResults");
  
  Connection_Client#(TOKEN,
                     TOKEN)
  //...
  link_to_load <- mkConnection_Client("funcp_doLoads");
  
  Connection_Client#(TOKEN,
                     TOKEN)
  //...
  link_to_store <- mkConnection_Client("funcp_doSpeculativeStores");

  Connection_Client#(TOKEN,
                     TOKEN)
  //...
  link_to_lco <- mkConnection_Client("funcp_commitResults");
  
  Connection_Client#(TOKEN,
                     TOKEN)
  //...
  link_to_gco <- mkConnection_Client("funcp_commitStores");

  //For killing. UNUSED
  
  Connection_Send#(TOKEN) 
  //...
        link_rewindToToken <- mkConnection_Send("funcp_rewindToToken");

 
  //Events
  
  EventRecorder event_com <- mkEventRecorder(0);
  
  Vector#(0, Port_Control) inports = newVector();
  Vector#(0, Port_Control) outports = newVector();
  
  LocalController local_ctrl <- mkLocalController(inports, outports);
  
  //********* Rules *********//

  //count
  rule count (True);
    
    if (hostCC == 0)
    begin
      let fd <- $fopen("hasim_cpu.out");
      if (fd == InvalidFile)
      begin
        $display("Error opening logfile!");
	$finish(1);
      end
      debug_log <= fd;
    end
    
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
	    
	    //Request a TOKEN
	    debug(2, $fdisplay(debug_log, "[%d] Requesting a new TOKEN on model cycle %0d.", hostCC, baseTick));
	    link_to_tok.makeReq(?);
	    
	    madeReq <= True;
	    
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
	    //Get the response
	    let tok = link_to_tok.getResp();
	    link_to_tok.deq();
	    
            tok.timep_info = TIMEP_TokInfo{epoch: 0, scratchpad: 0};

	    debug(2, $fdisplay(debug_log, "[%d] TOK Responded with TOKEN %0d.", hostCC, tok.index));
	    
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
	    debug(2, $fdisplay(debug_log, "[%d] Fetching TOKEN %0d at address 0x%h.", hostCC, cur_tok.index, pc));
            link_to_fet.makeReq(tuple2(cur_tok, pc));
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
	    //Get the response
            match {.tok, .inst} = link_to_fet.getResp();
	    link_to_fet.deq();

	    debug(2, $fdisplay(debug_log, "[%d] FET Responded with TOKEN %0d.", hostCC, tok.index));
	    
	    if (tok.index != cur_tok.index) $display ("FET ERROR: TOKEN Mismatch. Expected: %0d Received: %0d", cur_tok.index, tok.index);
	    
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
	    debug(2, $fdisplay(debug_log, "[%d] Decoding TOKEN %0d.", hostCC, cur_tok.index));
            link_to_dec.makeReq(cur_tok);
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
            match {.tok, .deps} = link_to_dec.getResp();
	    link_to_dec.deq();

	    debug(2, $fdisplay(debug_log, "[%d] DEC Responded with TOKEN %0d.", hostCC, tok.index));
	    
	    if (tok.index != cur_tok.index) $display ("DEC ERROR: TOKEN Mismatch. Expected: %0d Received: %0d", cur_tok.index, tok.index);
	    
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
	    debug(2, $fdisplay(debug_log, "[%d] Executing TOKEN %0d", hostCC, cur_tok.index));
            link_to_exe.makeReq(cur_tok);
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
            match {.tok, .res} = link_to_exe.getResp();
	    link_to_exe.deq();

	    debug(2, $fdisplay(debug_log, "[%d] EXE Responded with TOKEN %0d.", hostCC, tok.index));
	    
	    if (tok.index != cur_tok.index) $display ("EXE ERROR: TOKEN Mismatch. Expected: %0d Received: %0d", cur_tok.index, tok.index);
	   	
	    case (res) matches
	      tagged RBranchTaken .addr:
	      begin
	        debug(2, $fdisplay(debug_log, "Branch taken to address %h", addr));
	   	pc <= addr;
	      end
              tagged RBranchNotTaken .addr:
	      begin
	        debug(2, $fdisplay(debug_log, "Branch not taken"));
	   	pc <= pc + 4;
	      end
              tagged RNop:
	      begin
	        debug(2, $fdisplay(debug_log, "Nop"));
	   	pc <= pc + 4;
	      end
              tagged RTerminate .pf:
	      begin
	        debug(2, $fdisplay(debug_log, "Terminating Execution"));
                local_ctrl.endProgram(pf);
	      end
	    endcase
	    
            if (isaIsLoad(cur_inst))
  	      stage <= LOA;
            else if (isaIsStore(cur_inst))
              stage <= STO;
            else
              stage <= LCO;

	    madeReq <= False;
	  end
      end
      LOA:
      begin
        debug_case("stage", "LOA");
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    
	    //Request load
	    debug(2, $fdisplay(debug_log, "[%d] Load for TOKEN %0d", hostCC, cur_tok.index));
            link_to_load.makeReq(cur_tok);
            madeReq <= True;
          end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
	    let tok = link_to_load.getResp();
	    link_to_load.deq();

	    debug(2, $fdisplay(debug_log, "[%d] Load ops responded with TOKEN %0d.", hostCC, tok.index));
	    
	    if (tok.index != cur_tok.index) $display ("LOA ERROR: TOKEN Mismatch. Expected: %0d Received: %0d", cur_tok.index, tok.index);
	    
	    stage <= LCO;
	    madeReq <= False;
	  end
      end
      STO:
      begin
        debug_case("stage", "STO");
        if (!madeReq)
	  begin
	    debug_then("!madeReq");
	    
	    //Request store
	    debug(2, $fdisplay(debug_log, "[%d] Store for TOKEN %0d", hostCC, cur_tok.index));
            link_to_store.makeReq(cur_tok);
            madeReq <= True;

          end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
	    let tok = link_to_store.getResp();
	    link_to_store.deq();

	    debug(2, $fdisplay(debug_log, "[%d] Store ops responded with TOKEN %0d.", hostCC, tok.index));
	    
	    if (tok.index != cur_tok.index) $display ("STO ERROR: TOKEN Mismatch. Expected: %0d Received: %0d", cur_tok.index, tok.index);
	    
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
	    debug(2, $fdisplay(debug_log, "[%d] Locally committing TOKEN %0d.", hostCC, cur_tok.index));
            link_to_lco.makeReq(cur_tok);
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
  
            let tok = link_to_lco.getResp();
	    link_to_lco.deq();

	    debug(2, $fdisplay(debug_log, "[%d] LCO Responded with TOKEN %0d.", hostCC, tok.index));
	    
	    if (tok.index != cur_tok.index) $display ("LCO ERROR: TOKEN Mismatch. Expected: %0d Received: %0d", cur_tok.index, tok.index);
	    if (isaIsStore(cur_inst))
	      stage <= GCO;
            else
            begin
              stage <= TOK;
              baseTick <= baseTick + 1;
	      debug(1, $fdisplay(debug_log, "Committed TOKEN %0d on model cycle %0d.", cur_tok.index, baseTick));
	      event_com.recordEvent(tagged Valid zeroExtend(cur_tok.index));
            end
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
	    debug(2, $fdisplay(debug_log, "[%d] Globally committing TOKEN %0d", hostCC, cur_tok.index));
            link_to_gco.makeReq(cur_tok);
	    
	    madeReq <= True;
	  end
	else
	  begin
	    debug_else("!madeReq");
	    
 	    //Get the response
            let tok = link_to_gco.getResp();
	    link_to_gco.deq();

	    debug(2, $fdisplay(debug_log, "[%d] GCO Responded with TOKEN %0d.", hostCC, tok.index));
	    
	    if (tok.index != cur_tok.index) $display ("GCO ERROR: TOKEN Mismatch. Expected: %0d Received: %0d", cur_tok.index, tok.index);
	    
	    debug(1, $fdisplay(debug_log, "Committed TOKEN %0d on model cycle %0d.", cur_tok.index, baseTick));
	    event_com.recordEvent(tagged Valid zeroExtend(cur_tok.index));
	    
	    stage <= TOK;
	    madeReq <= False;
	    baseTick <= baseTick + 1;
	  end
      end
    endcase    
  endrule
  
endmodule
`undef MODULE_NAME
