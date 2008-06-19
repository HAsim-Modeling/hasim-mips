import FIFO::*;
import Vector::*;

import hasim_common::*;
import soft_connections::*;
import hasim_modellib::*;
import hasim_isa::*;

import module_local_controller::*;

`include "asim/dict/EVENTS_EXECUTE.bsh"
`include "asim/dict/STATS_EXECUTE.bsh"

module [HASIM_MODULE] mkPipe_Execute#(File debug_file, Bit#(32) curTick)
    //interface:
                ();
  
  //Local State
  Reg#(TOKEN_TIMEP_EPOCH)  epoch     <- mkReg(0);
  Reg#(Bool)   in_flight <- mkReg(False);
  FIFO#(Tuple4#(ISA_ADDRESS, Bool, Bool, Bool)) addrQ   <- mkFIFO();

  //Connections to FP
  
  Connection_Send#(Token)           fp_exe_req  <- mkConnection_Send("funcp_getResults_req");
  Connection_Receive#(Tuple2#(TOKEN, ISA_EXECUTION_RESULT))  fp_exe_resp <- mkConnection_Receive("funcp_getResults_resp");

  //Events
  EventRecorder event_exe <- mkEventRecorder(`EVENTS_EXECUTE_INSTRUCTION_EXECUTE);
  
  //Stats
  Stat stat_mpred <- mkStatCounter(`STATS_EXECUTE_BPRED_MISPREDS);
  
  //Incoming Ports
  Port_Receive#(Tuple5#(TOKEN, ISA_ADDRESS, Bool, Bool, Bool)) port_from_dec <- mkPort_Receive("dec_to_exe", 1);

  //Outgoing Ports
  Port_Send#(Tuple3#(TOKEN, Bool, Bool))                        port_to_mem <- mkPort_Send("exe_to_mem");
  Port_Send#(Tuple2#(TOKEN, Maybe#(ISA_ADDRESS))) port_to_fet <- mkPort_Send("fet_branchResolve");

    Reg#(Bit#(64)) counter <- mkReg(0);

  //Local Controller
  Vector#(1, Port_Control) inports  = newVector();
  Vector#(2, Port_Control) outports = newVector();
  inports[0]  = port_from_dec.ctrl;
  outports[0] = port_to_mem.ctrl;
  outports[1] = port_to_fet.ctrl;
  LocalController local_ctrl <- mkLocalController(inports, outports);

  rule executeReq (!in_flight);
  
    local_ctrl.startModelCC();

    counter <= counter + 1;
    $fdisplay(debug_file, "[%d]:Exe Counter : %0d", curTick, counter);
    
    
    let mtup <- port_from_dec.receive();
    
    case (mtup) matches
      tagged Invalid:
      begin
            port_to_mem.send(tagged Invalid);
            event_exe.recordEvent(tagged Invalid);
            port_to_fet.send(tagged Invalid);
      end
      tagged Valid {.tok, .addr, .isLoad, .isStore, .drainAfter}:
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
              addrQ.enq(tuple4(addr, isLoad, isStore, drainAfter));
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

    match {.predAddr, .isLoad, .isStore, .drainAfter} = addrQ.first();
    addrQ.deq();
    
    $fdisplay(debug_file, "[%d]:Exe Counter when sent : %0d", curTick, counter);
    
    Bool mispredict = False;
    
    case (res) matches
      tagged RBranchTaken .addr:
          begin
            $fdisplay(debug_file, "[%d]:EXE: Branch taken", curTick);
        if (predAddr != addr)
            begin
              $fdisplay(debug_file, "[%d]:EXE: Branch mispredicted!", curTick);
              stat_mpred.incr();
              epoch <= epoch + 1;
              port_to_fet.send(tagged Valid tuple2(tok, tagged Valid addr));  
            end
            else if (drainAfter)
            begin
              $fdisplay(debug_file, "[%d]:EXE: Emulation resteer to 0x%h!", curTick, predAddr);
              port_to_fet.send(tagged Valid tuple2(tok, tagged Valid predAddr));
              epoch <= epoch + 1;
            end
            else
            begin
              port_to_fet.send(tagged Valid tuple2(tok, tagged Invalid));
            end
          end
      tagged RBranchNotTaken .addr:
          begin
          
            $fdisplay(debug_file, "[%d]:EXE: Branch not taken", curTick);
            if (pred_taken == 1)
            begin
              $fdisplay(debug_file, "[%d]:EXE: Branch mispredicted!", curTick);
              stat_mpred.incr();
              epoch <= epoch + 1;
              port_to_fet.send(tagged Valid tuple2(tok, tagged Valid addr));
            end
            else if (drainAfter)
            begin
              $fdisplay(debug_file, "[%d]:EXE: Emulation resteer to 0x%h!", curTick, predAddr);
              epoch <= epoch + 1;
              port_to_fet.send(tagged Valid tuple2(tok, tagged Valid predAddr));
            end
            else
            begin
              port_to_fet.send(tagged Valid tuple2(tok, tagged Invalid));
            end
          end
      tagged RNop:
      begin
            if (drainAfter)
            begin
                $fdisplay(debug_file, "[%d]:EXE: Emulation resteer to 0x%h!", curTick, predAddr);
                port_to_fet.send(tagged Valid tuple2(tok, tagged Valid predAddr));
                epoch <= epoch + 1;
            end
            else
            begin
                port_to_fet.send(tagged Invalid);
            end
      end
      tagged REffectiveAddr .ea:
      begin
            if (drainAfter)
            begin
                $fdisplay(debug_file, "[%d]:EXE: Emulation resteer to 0x%h!", curTick, predAddr);
                port_to_fet.send(tagged Valid tuple2(tok, tagged Valid predAddr));
            end
            else
            begin
                port_to_fet.send(tagged Invalid);
            end
      end
      tagged RTerminate .pf:
      begin
            if (drainAfter)
            begin
                $fdisplay(debug_file, "[%d]:EXE: Emulation resteer to 0x%h!", curTick, predAddr);
                port_to_fet.send(tagged Valid tuple2(tok, tagged Valid predAddr));
                epoch <= epoch + 1;
            end
            else
            begin
                port_to_fet.send(tagged Invalid);
            end
            $fdisplay(debug_file, "[%d]:EXE: Setting Termination!", curTick);
            tok.timep_info.scratchpad[1] = 1; //[1] is termination
            tok.timep_info.scratchpad[2] = pack(pf); //[2] is passfail
      end
    endcase
    port_to_mem.send(tagged Valid tuple3(tok, isLoad, isStore));
    event_exe.recordEvent(tagged Valid zeroExtend(tok.index));
    in_flight <= False;
    
  endrule


endmodule
