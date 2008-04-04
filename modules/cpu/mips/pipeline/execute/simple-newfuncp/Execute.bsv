import FIFO::*;
import Vector::*;

import hasim_common::*;
import soft_connections::*;
import hasim_modellib::*;
import hasim_isa::*;

import hasim_local_controller::*;

`include "asim/dict/EVENTS_EXECUTE.bsh"
`include "asim/dict/STREAMS_STATS_EXECUTE.bsh"

module [HASIM_MODULE] mkPipe_Execute#(File debug_file, Bit#(32) curTick)
    //interface:
                ();
  
  //Local State
  Reg#(TOKEN_TIMEP_EPOCH)  epoch     <- mkReg(0);
  Reg#(Bool)   in_flight <- mkReg(False);
  FIFO#(Tuple3#(Maybe#(ISA_ADDRESS), Bool, Bool)) addrQ   <- mkFIFO();

  //Connections to FP
  
  Connection_Send#(Token)           fp_exe_req  <- mkConnection_Send("funcp_getResults_req");
  Connection_Receive#(Tuple2#(TOKEN, ISA_EXECUTION_RESULT))  fp_exe_resp <- mkConnection_Receive("funcp_getResults_resp");

  Connection_Send#(Token)     fp_rewindToToken <- mkConnection_Send("funcp_rewindToToken");

  //Events
  EventRecorder event_exe <- mkEventRecorder(`EVENTS_EXECUTE_INSTRUCTION_EXECUTE);
  
  //Stats
  Stat stat_mpred <- mkStatCounter(`STREAMS_STATS_EXECUTE_BPRED_MISPREDS);
  
  //Incoming Ports
  Port_Receive#(Tuple4#(TOKEN, Maybe#(ISA_ADDRESS), Bool, Bool)) port_from_dec <- mkPort_Receive("dec_to_exe", 1);

  //Outgoing Ports
  Port_Send#(Tuple3#(TOKEN, Bool, Bool))                        port_to_mem <- mkPort_Send("exe_to_mem");
  Port_Send#(Tuple2#(TOKEN, Maybe#(ISA_ADDRESS))) port_to_fet <- mkPort_Send("fet_branchResolve");

  //Local Controller
  Vector#(1, Port_Control) inports  = newVector();
  Vector#(2, Port_Control) outports = newVector();
  inports[0]  = port_from_dec.ctrl;
  outports[0] = port_to_mem.ctrl;
  outports[1] = port_to_fet.ctrl;
  LocalController local_ctrl <- mkLocalController(inports, outports);

  rule executeReq (!in_flight);
  
    local_ctrl.startModelCC();
    
    let mtup <- port_from_dec.receive();
    
    case (mtup) matches
      tagged Invalid:
      begin
        port_to_mem.send(tagged Invalid);
            event_exe.recordEvent(tagged Invalid);
            port_to_fet.send(tagged Invalid);
      end
      tagged Valid {.tok, .maddr, .isLoad, .isStore}:
      begin
            if (tok.timep_info.epoch != epoch) //kill it
            begin
              event_exe.recordEvent(tagged Invalid);
              port_to_mem.send(tagged Invalid);
              port_to_fet.send(tagged Invalid);
            end
            else //continue to execute it
            begin
              $fdisplay(debug_file, "[%d]:EXE:REQ: %0d", curTick, tok.index);
              fp_exe_req.send(tok);
              addrQ.enq(tuple3(maddr, isLoad, isStore));
              in_flight <= True;
        end
      end
    endcase
  
  endrule

  rule executeResp (in_flight);
  
    match {.tok, .res} = fp_exe_resp.receive();
    fp_exe_resp.deq();
    
    $fdisplay(debug_file, "[%d]:EXE:RSP: %0d", curTick, tok.index);
    
    let pred_taken = tok.timep_info.scratchpad[0];
    match {.predAddr, .isLoad, .isStore} = addrQ.first();
    
    case (res) matches
      tagged RBranchTaken .addr:
          begin
            $fdisplay(debug_file, "[%d]:EXE: Branch taken", curTick);
            Bool mispredict = case (predAddr) matches
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
      tagged REffectiveAddr .ea:
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
    port_to_mem.send(tagged Valid tuple3(tok, isLoad, isStore));
    event_exe.recordEvent(tagged Valid zeroExtend(tok.index));
    in_flight <= False;
    
  endrule


endmodule
