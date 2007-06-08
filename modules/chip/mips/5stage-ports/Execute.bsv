
import hasim_common::*;
import hasim_isa::*;

import hasim_command_center::*;


typedef TokEpoch Epoch;

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
        port_to_mem.send(tagged Invalid);
	//event_exe.recordEvent(tagged Invalid);
	port_to_ic.send(tagged Invalid);
      end
      tagged Valid .tok:
      begin
	if (tok.info.epoch != epoch) //kill it
	begin
	  fp_exe_kill.send(tok);
          //event_exe.recordEvent(tagged Invalid);
          port_to_mem.send(tagged Invalid);
	  port_to_ic.send(tagged Invalid);
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
	  port_to_ic.send(tagged Valid tuple2(tok, addr));
	end
      tagged RBranchNotTaken:
	begin
	  port_to_ic.send(tagged Invalid);
	  $display("Branch not taken!");
	end
      tagged RNop:
	port_to_ic.send(tagged Invalid);
      tagged RTerminate .pf:
      begin
	port_to_ic.send(tagged Invalid);
	$display("Setting Termination!");
	cc.setPassFail(pf);
        case (cc.getStopToken) matches
	  tagged Invalid:
  	    cc.setStopToken(tok);
	  default:
	    noAction;
	endcase
      end
    endcase

    port_to_mem.send(tagged Valid tok);
    //event_exe.recordEvent(tagged Valid zeroExtend(tok.index));
    in_flight <= False;
    
  endrule


endmodule
