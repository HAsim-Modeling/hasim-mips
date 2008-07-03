
import Vector::*;

import hasim_common::*;
import soft_connections::*;
import hasim_modellib::*;
import hasim_isa::*;

import module_local_controller::*;
`include "asim/provides/hasim_controller.bsh"

`include "asim/dict/EVENTS_WRITEBACK.bsh"
`include "asim/dict/STATS_WRITEBACK.bsh"

typedef enum
{
  WB_Ready, WB_Finish, WB_CommitStore, WB_FinishStore
}
  WB_STATE deriving (Eq, Bits);

module [HASIM_MODULE] mkPipe_Writeback#(File debug_file, Bit#(32) curTick)
    //interface:
                ();

  //Local State
  Reg#(WB_STATE) state <- mkReg(WB_Ready);

  //Connections to FP
  Connection_Send#(TOKEN)    fp_lco_req  <- mkConnection_Send("funcp_commitResults_req");
  Connection_Receive#(TOKEN) fp_lco_resp <- mkConnection_Receive("funcp_commitResults_resp");
    
  Connection_Send#(TOKEN)    fp_gco_req  <- mkConnection_Send("funcp_commitStores_req");
  Connection_Receive#(TOKEN) fp_gco_resp <- mkConnection_Receive("funcp_commitStores_resp");
  //Events
  EventRecorder event_wb <- mkEventRecorder(`EVENTS_WRITEBACK_INSTRUCTION_WRITEBACK);
  
  //Stats
  Stat stat_wb <- mkStatCounter(`STATS_WRITEBACK_INSTS_COMMITTED);

  //Incoming Ports
  Port_Receive#(Tuple2#(TOKEN, Bool)) port_from_mem <- mkPort_Receive("mem_to_wb", 1);

  //Local Controller
  Vector#(1, Port_Control) inports = newVector();
  Vector#(0, Port_Control) outports = newVector();
  inports[0] = port_from_mem.ctrl;
  LocalController local_ctrl <- mkLocalController(inports, outports);

  // Number of commits (to go along with heartbeat)
  Connection_Send#(MODEL_NUM_COMMITS) linkModelCommit <- mkConnection_Send("model_commits");

  rule lcoReq (state == WB_Ready);
  
    let mtok <- port_from_mem.receive();
    
    case (mtok) matches
      tagged Invalid:
      begin
        noAction;
        event_wb.recordEvent(tagged Invalid);
      end
      tagged Valid {.tok, .isStore}:
      begin
        $fdisplay(debug_file, "[%d]:LCO:REQ: %0d", curTick, tok.index);
        fp_lco_req.send(tok);
        if (isStore)
          state <= WB_CommitStore;
        else
          state <= WB_Finish;
      end
    endcase
  
  endrule
   
  rule finish (state == WB_Finish);
  
    let tok = fp_lco_resp.receive();
    fp_lco_resp.deq();
    
    event_wb.recordEvent(tagged Valid zeroExtend(tok.index));
    stat_wb.incr();
    linkModelCommit.send(1);
    
    $fdisplay(debug_file, "[%d]:LCO:RSP: %0d", curTick, tok.index);

    if (tok.timep_info.scratchpad[1] == 1)
       local_ctrl.endProgram(unpack(tok.timep_info.scratchpad[2]));
    
    state <= WB_Ready;
    
  endrule
  
  rule gcoReq (state == WB_CommitStore);
  
    let tok = fp_lco_resp.receive();
    fp_lco_resp.deq();
    
    $fdisplay(debug_file, "[%d]:LCO:RSP: %0d", curTick, tok.index);
    $fdisplay(debug_file, "[%d]:GCO:REQ: %0d", curTick, tok.index);
    fp_gco_req.send(tok);
    
    state <= WB_FinishStore;
  endrule
  
  rule gcoResp (state == WB_FinishStore);
  
    let tok  = fp_gco_resp.receive();
    fp_gco_resp.deq();
    
    $fdisplay(debug_file, "[%d]:GCO:RSP: %0d", curTick, tok.index);
    
    event_wb.recordEvent(tagged Valid zeroExtend(tok.index));
    stat_wb.incr();
    linkModelCommit.send(1);
    
    state <= WB_Ready;

    if (tok.timep_info.scratchpad[1] == 1)
       local_ctrl.endProgram(unpack(tok.timep_info.scratchpad[2]));
    
  endrule

endmodule 
