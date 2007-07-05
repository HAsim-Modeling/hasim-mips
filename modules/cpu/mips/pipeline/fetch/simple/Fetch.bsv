
import LFSR::*;
import RegFile::*;
import Vector::*;


import hasim_common::*;
import hasim_isa::*;

import hasim_local_controller::*;
import hasim_branch_pred::*;

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
  FET_State
    deriving (Eq, Bits);

typedef Bit#(`FET_BTB_HASH_BITS) AddrHash;

function AddrHash btbHash(Addr a);

  return truncate(a);

endfunction

module [HASim_Module] mkPipe_Fetch#(File debug_file, Tick curTick)
    //interface:
                ();

  //Local State

  Reg#(Addr)                  pc <- mkReg(32'h00001000);
  Reg#(TIMEP_Epoch)        epoch <- mkReg(0);
  Reg#(Token)          stall_tok <- mkRegU;
  Reg#(Maybe#(Addr))  stall_addr <- mkRegU;
  Reg#(Bit#(16))     stall_count <- mkReg(0);
  Reg#(Bool)         	stalling <- mkReg(False);
  Reg#(FET_State)    	   state <- mkReg(FET_Ready);
  
  //For branch prediction
  
  BranchPred branch_pred <- mkBranchPred();
  RegFile#(TokIndex, Addr)         addrs <- mkRegFileFull();
  RegFile#(AddrHash, Maybe#(Addr))   btb <- mkRegFileFull();
  
  //Pseudo-randomness
  LFSR#(Bit#(7)) lfsr <- mkFeedLFSR(7'b1001110);

  //Connections to FP
  Connection_Send#(Bit#(8))   fp_tok_req  <- mkConnection_Send("fp_tok_req");
  Connection_Receive#(Token)  fp_tok_resp <- mkConnection_Receive("fp_tok_resp");
  Connection_Send#(Token)     fp_tok_kill <- mkConnection_Send("fp_tok_kill");
  
  Connection_Send#(Tuple2#(Token, Addr))           fp_fet_req  <- mkConnection_Send("fp_fet_req");
  Connection_Receive#(Tuple2#(Token, PackedInst))  fp_fet_resp <- mkConnection_Receive("fp_fet_resp");

  Connection_Send#(Token)     fp_fet_kill <- mkConnection_Send("fp_fet_kill");
  Connection_Send#(Token)     fp_dec_kill <- mkConnection_Send("fp_dec_kill");
      
  //Events
  EventRecorder event_fet <- mkEventRecorder("1 FET");
  
  //Stats
  Stat stat_cycles <- mkStatCounter("Total Cycles");
  Stat stat_fet <- mkStatCounter("Instructions Fetched");
  Stat stat_imisses <- mkStatCounter("ICache Misses");

    
  //Incoming Ports
  Port_Receive#(Tuple2#(Token, Maybe#(Addr))) port_from_exe <- mkPort_Receive("fet_branchResolve", 1);

  //Outgoing Ports
  Port_Send#(Tuple2#(Token, Maybe#(Addr))) port_to_dec <- mkPort_Send("fet_to_dec");

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
	fp_tok_req.send(17); //17 is arbitrarily-chosen bug workaround
	state <= FET_GetInst;

      end
    else
      begin
      
        if (stall_count == 0)
	  begin
            port_to_dec.send(tagged Valid tuple2(stall_tok, stall_addr));
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

     let tok <- fp_tok_resp.receive();

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
   
     match {.tok, .inst} <- fp_fet_resp.receive();
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
     
       port_to_dec.send(tagged Valid tuple2(tok, pred_addr));
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
       stalling    <= True;
     end
     
     state       <= FET_Ready;
     
   endrule

endmodule
