
import LFSR::*;
import RegFile::*;
import Vector::*;
 
import hasim_common::*;
import soft_connections::*;
import hasim_modellib::*;
import hasim_isa::*;

import module_local_controller::*;
import hasim_branch_pred::*;

`include "asim/dict/EVENTS_FETCH.bsh"
`include "asim/dict/STATS_FETCH.bsh"

//AWB Parameters            default:
//FET_ICACHE_HIT_CHANCE       50
//FET_ICACHE_MISS_PENALTY     10
//FET_BTB_HASH_BITS           8

Integer fet_hit_chance = (`FET_ICACHE_HIT_CHANCE * 127)/ 100;

typedef enum 
{
  FET_Ready,
  FET_GetInst,
  FET_Finish
}
  FET_STATE
    deriving (Eq, Bits);

typedef Bit#(`FET_BTB_HASH_BITS) ISA_ADDRESSHash;

function ISA_ADDRESSHash btbHash(ISA_ADDRESS a);

  return truncate(a);

endfunction

module [HASIM_MODULE] mkPipe_Fetch#(File debug_file, Bit#(32) curTick)
    //interface:
                ();

  //Local State

  Reg#(ISA_ADDRESS)           pc <- mkReg('h00001000);
  Reg#(TOKEN_TIMEP_EPOCH)     epoch <- mkReg(0);
  Reg#(TOKEN)          stall_tok <- mkRegU;
  Reg#(Maybe#(ISA_ADDRESS))  stall_addr <- mkRegU;
  Reg#(ISA_INSTRUCTION)    stall_inst <- mkRegU;
  Reg#(Bit#(16))     stall_count <- mkReg(0);
  Reg#(Bool)                 stalling <- mkReg(False);
  Reg#(FET_STATE)               state <- mkReg(FET_Ready);
  
  //For branch prediction
  
  BranchPred branch_pred <- mkBranchPred();
  RegFile#(TokIndex, ISA_ADDRESS)         addrs <- mkRegFileFull();
  RegFile#(ISA_ADDRESSHash, Maybe#(ISA_ADDRESS))   btb <- mkRegFileFull();
  
  //Pseudo-randomness
  LFSR#(Bit#(7)) lfsr <- mkFeedLFSR(7'b1001110);

  //Connections to controller
  Connection_Send#(Bool) link_model_cycle <- mkConnection_Send("model_cycle");

  //Connections to FP
  Connection_Send#(void)   fp_tok_req  <- mkConnection_Send("funcp_newInFlight_req");
  Connection_Receive#(TOKEN)  fp_tok_resp <- mkConnection_Receive("funcp_newInFlight_resp");
  
  Connection_Send#(Tuple2#(TOKEN, ISA_ADDRESS))         fp_fet_req  <- mkConnection_Send("funcp_getInstruction_req");
  Connection_Receive#(Tuple2#(TOKEN, ISA_INSTRUCTION))  fp_fet_resp <- mkConnection_Receive("funcp_getInstruction_resp");
      
  //Events
  EventRecorder event_fet <- mkEventRecorder(`EVENTS_FETCH_INSTRUCTION_FET);
  
  //Stats
  Stat stat_cycles   <- mkStatCounter(`STATS_FETCH_TOTAL_CYCLES);
  Stat stat_fet      <- mkStatCounter(`STATS_FETCH_INSTS_FETCHED);
  Stat stat_imisses  <- mkStatCounter(`STATS_FETCH_ICACHE_MISSES);

    
  //Incoming Ports
  Port_Receive#(Tuple2#(TOKEN, Maybe#(ISA_ADDRESS))) port_from_exe <- mkPort_Receive("fet_branchResolve", 1);

  //Outgoing Ports
  Port_Send#(Tuple3#(TOKEN, Maybe#(ISA_ADDRESS), ISA_INSTRUCTION)) port_to_dec <- mkPort_Send("fet_to_dec");

  //Local Controller
  Vector#(1, Port_Control) inports  = newVector();
  Vector#(1, Port_Control) outports = newVector();
  inports[0]  = port_from_exe.ctrl;
  outports[0] = port_to_dec.ctrl;
  LocalController local_ctrl <- mkLocalController(inports, outports);

  rule beginFetch (state == FET_Ready);
  
    local_ctrl.startModelCC();
    
    let mtup <- port_from_exe.receive();
    stat_cycles.incr();
    
    //Note new model cycle
    link_model_cycle.send(?);

    //First let's take care of incoming resteers
    
    case (mtup) matches
      tagged Invalid: //No Re-steer
        noAction;
      tagged Valid {.ktok, .mpc}: //Re-steer
      begin
      
        //Look up this token
        Bool pred_taken = ktok.timep_info.scratchpad[0] == 1; //The prediction is stored in the scratchpad
        let iaddr = addrs.sub(ktok.index); //Get the address
        let hash = btbHash(iaddr);    //Hash the address
        let pred_pc = btb.sub(hash);  //Get the predpc 
        
        case (mpc) matches
          tagged Invalid:  //Branch predicted correctly
          begin 
            branch_pred.upd(ktok, iaddr, pred_taken, pred_taken);
          end
          tagged Valid .new_pc: //Branch mispredicted. Start the new epoch
          begin
            branch_pred.upd(ktok, iaddr, pred_taken, !pred_taken);
            btb.upd(hash, tagged Valid new_pc);
            epoch <= epoch + 1;
            pc <= new_pc;
          end
        endcase

      end
    endcase
    
    if (!stalling)
      begin
        $fdisplay(debug_file, "[%d]:TOK:REQ", curTick);
        fp_tok_req.send(?);
        state <= FET_GetInst;

      end
    else
      begin
      
        if (stall_count == 0)
          begin
            port_to_dec.send(tagged Valid tuple3(stall_tok, stall_addr, stall_inst));
            event_fet.recordEvent(tagged Valid zeroExtend(stall_tok.index));
            stat_fet.incr();
            stalling <= False;
          end
        else
          begin
            port_to_dec.send(tagged Invalid);
            event_fet.recordEvent(tagged Invalid);
            stall_count <= stall_count - 1;
          end
      end

   endrule
   
   rule fetchInst (state == FET_GetInst);

     let tok = fp_tok_resp.receive();
     fp_tok_resp.deq();

     $fdisplay(debug_file, "[%d]:TOK:RSP: %0d", curTick, tok.index);
     
     let inf = TIMEP_TokInfo {epoch: epoch, scratchpad: 0};
     tok.timep_info = inf;

     $fdisplay(debug_file, "[%d]:FET:REQ: %0d:0x%h", curTick, tok.index, pc);
     fp_fet_req.send(tuple2(tok, pc));
     branch_pred.getPredReq(tok, pc);
     addrs.upd(tok.index, pc);
     
     state <= FET_Finish;
     
   endrule


   rule finishFetch (state == FET_Finish);
   
     match {.tok, .inst} = fp_fet_resp.receive();
     fp_fet_resp.deq();
     
     $fdisplay(debug_file, "[%d]:FET:RSP: %0d:0x%h", curTick, tok.index, inst);
     
     let pred_taken <- branch_pred.getPredResp();
     let btb_resp = btb.sub(btbHash(pc));
     
     tok.timep_info.scratchpad[0] = pack(pred_taken);

     pc <= pred_taken && isValid(btb_resp) ? validValue(btb_resp) : pc + 4;
     let pred_addr = pred_taken && isValid(btb_resp) ? btb_resp : tagged Invalid;

     let isHit = lfsr.value < fromInteger(fet_hit_chance);
     lfsr.next();

     if (isHit)
     begin
     
       port_to_dec.send(tagged Valid tuple3(tok, pred_addr, inst));
       event_fet.recordEvent(tagged Valid zeroExtend(tok.index));
       stat_fet.incr();

     end
     else
     begin
       port_to_dec.send(tagged Invalid);
       event_fet.recordEvent(tagged Invalid);
       stat_imisses.incr();
       stall_count <= `FET_ICACHE_MISS_PENALTY;
       stall_tok   <= tok;
       stall_addr  <= pred_addr;
       stall_inst  <= inst;
       stalling    <= True;
     end
     
     state       <= FET_Ready;
     
   endrule

endmodule
