import FIFO::*;
import Vector::*;

import hasim_common::*;
import hasim_isa::*;

import hasim_local_controller::*;

interface Execute;
  method Action start();
endinterface

module [HASim_Module] mkPipe_Execute#(File debug_file, Tick curTick)
    //interface:
                (Execute);
  
  //Local State
  Reg#(TIMEP_Epoch)  epoch     <- mkReg(0);
  Reg#(Bool)   in_flight <- mkReg(False);
  FIFO#(Maybe#(Addr)) addrQ   <- mkFIFO();

  //Connections to FP
  
  Connection_Send#(Tuple2#(Token, void))           fp_exe_req  <- mkConnection_Send("fp_exe_req");
  Connection_Receive#(Tuple2#(Token, InstResult))  fp_exe_resp <- mkConnection_Receive("fp_exe_resp");

  Connection_Send#(Token)     fp_exe_kill <- mkConnection_Send("fp_exe_kill");
  
  Connection_Send#(Token)     fp_rewindToToken <- mkConnection_Send("fp_rewindToToken");

  //Events
  EventRecorder event_exe <- mkEventRecorder("3     EXE");
  
  //Stats
  Stat stat_mpred <- mkStatCounter("Branch Mispredicts");
  
  //Incoming Ports
  Port_Receive#(Tuple2#(Token, Maybe#(Addr))) port_from_dec <- mkPort_Receive("dec_to_exe", 1);

  //Outgoing Ports
  Port_Send#(Token)                        port_to_mem <- mkPort_Send("exe_to_mem");
  Port_Send#(Tuple2#(Token, Maybe#(Addr))) port_to_fet <- mkPort_Send("fet_branchResolve");

  //Local Controller
  Vector#(1, Port_Control) inports  = newVector();
  Vector#(2, Port_Control) outports = newVector();
  inports[0]  = port_from_dec.ctrl;
  outports[0] = port_to_mem.ctrl;
  outports[1] = port_to_fet.ctrl;
  LocalController local_ctrl <- mkLocalController(inports, outports);

  rule executeResp (in_flight);
  
    match {.tok, .res} = fp_exe_resp.receive();
    fp_exe_resp.deq();
    
    $fdisplay(debug_file, "[%d]:EXE:RSP: %0d", curTick, tok.index);
    
    let pred_taken = tok.timep_info.scratchpad[0];

    case (res) matches
      tagged RBranchTaken .addr:
	begin
	  $fdisplay(debug_file, "[%d]:EXE: Branch taken", curTick);
	  Bool mispredict = case (addrQ.first()) matches
	                 tagged Valid .pred: (pred != addr);
			 tagged Invalid: True;
		       endcase;
          if (mispredict)
	  begin
	    $fdisplay(debug_file, "[%d]:EXE: Branch mispredicted!", curTick);
	    stat_mpred.incr();
            epoch <= epoch + 1;
            fp_rewindToToken.send(tok);
	    port_to_fet.send(tagged Valid tuple2(tok, tagged Valid addr));  
	  end
	  else
	    port_to_fet.send(tagged Valid tuple2(tok, tagged Invalid));
	  
	end
      tagged RBranchNotTaken .addr:
	begin
	  
	  $fdisplay(debug_file, "[%d]:EXE: Branch not taken", curTick);
	  if (pred_taken == 1)
	  begin
	    $fdisplay(debug_file, "[%d]:EXE: Branch mispredicted!", curTick);
	    stat_mpred.incr();
            epoch <= epoch + 1;
            fp_rewindToToken.send(tok);
	    port_to_fet.send(tagged Valid tuple2(tok, tagged Valid addr));
	  end
	  else
	    port_to_fet.send(tagged Valid tuple2(tok, tagged Invalid));
	    
	end
      tagged RNop:
	port_to_fet.send(tagged Invalid);
      tagged RTerminate .pf:
      begin
	port_to_fet.send(tagged Invalid);
	$fdisplay(debug_file, "[%d]:EXE: Setting Termination!", curTick);
	tok.timep_info.scratchpad[1] = 1; //[1] is termination
	tok.timep_info.scratchpad[2] = pack(pf); //[2] is passfail
      end
    endcase

    addrQ.deq();
    port_to_mem.send(tagged Valid tok);
    event_exe.recordEvent(tagged Valid zeroExtend(tok.index));
    in_flight <= False;
    
  endrule


  method Action start() if (!in_flight);
  
    local_ctrl.startModelCC();
    
    let mtup <- port_from_dec.receive();
    
    case (mtup) matches
      tagged Invalid:
      begin
        port_to_mem.send(tagged Invalid);
	event_exe.recordEvent(tagged Invalid);
	port_to_fet.send(tagged Invalid);
      end
      tagged Valid {.tok, .maddr}:
      begin
	if (tok.timep_info.epoch != epoch) //kill it
	begin
          $fdisplay(debug_file, "[%d]:EXE: Killing: %0d", curTick, tok.index);
	  fp_exe_kill.send(tok);
          event_exe.recordEvent(tagged Invalid);
          port_to_mem.send(tagged Invalid);
	  port_to_fet.send(tagged Invalid);
	end
	else //continue to execute it
	begin
          $fdisplay(debug_file, "[%d]:EXE:REQ: %0d", curTick, tok.index);
          fp_exe_req.send(tuple2(tok, ?));
	  addrQ.enq(maddr);
	  in_flight <= True;
        end
      end
    endcase
  
  endmethod
endmodule
