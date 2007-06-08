
import LFSR::*;

import hasim_common::*;
import hasim_isa::*;

import hasim_command_center::*;

`define FET_Miss_Penalty 10
`define FET_Hit_Chance 64

typedef TokEpoch Epoch;

typedef enum 
{
  FET_Ready,
  FET_GetInst,
  FET_Finish
}
  FET_State
    deriving (Eq, Bits);

module [HASim_Module] mk5stage_FET#(CommandCenter cc)
    //interface:
                ();

  //Local State

  Reg#(Addr)              pc <- mkReg(32'h00001000);
  Reg#(Epoch)          epoch <- mkReg(0);
  Reg#(Token)      stall_tok <- mkRegU;
  Reg#(Bit#(16)) stall_count <- mkReg(0);
  Reg#(Bool)        stalling <- mkReg(False);
  Reg#(FET_State)      state <- mkReg(FET_Ready);
  
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
  //EventRecorder event_fet <- mkEventRecorder("Fetch");
  
  //Stats
  //Stat stat_fet <- mkStatCounter("Fetch");
    
  //Incoming Ports
  Port_Receive#(Tuple2#(Token, Addr)) port_from_ic <- mkPort_Receive("fet_setPC", 1);

  //Outgoing Ports
  Port_Send#(Token) port_to_dec <- mkPort_Send("fet_to_dec");


  rule beginFetch (cc.running && state == FET_Ready);
    
    let mtup <- port_from_ic.receive();
    
    case (mtup) matches
      tagged Invalid: //We're on the right path
        noAction;
      tagged Valid {.ktok, .new_pc}: //Re-steer
      begin
	epoch <= epoch + 1;
	pc <= new_pc;
        fp_tok_kill.send(ktok);
      end
    endcase
    
    if (!stalling)
      begin
        $display("REQ:TOK");
	fp_tok_req.send(17); //17 is arbitrarily-chosen bug workaround
	state <= FET_GetInst;

      end
    else
      begin
      
        if (stall_count == 0)
	  begin
            port_to_dec.send(tagged Valid stall_tok);
            //event_fet.recordEvent(tagged Valid zeroExtend(stall_tok.index));
            //stat_fet.incr();
	    stalling <= False;
	  end
	else
	  begin
            port_to_dec.send(tagged Invalid);
            //event_fet.recordEvent(tagged Invalid);
            stall_count <= stall_count - 1;
	  end
      end

   endrule
   
   rule fetchInst (state == FET_GetInst);

     let tok <- fp_tok_resp.receive();

     let inf = TokInfo {epoch: epoch, ctxt: ?};
     let tok2 = Token {index: tok.index, info: inf};
      
     $display("REQ:FET:%d:0x%h", tok.index, pc);
     fp_fet_req.send(tuple2(tok2, pc));
      
     pc <= pc + 4;
     
     state <= FET_Finish;
     
   endrule


   rule finishFetch (state == FET_Finish);
   
     match {.tok, .inst} <- fp_fet_resp.receive();

     let isHit = lfsr.value < `FET_Hit_Chance;
     lfsr.next();

     if (isHit)
     begin
     
       port_to_dec.send(tagged Valid tok);
       //event_fet.recordEvent(tagged Valid zeroExtend(tok.index));
       //stat_fet.incr();

     end
     else
     begin
       port_to_dec.send(tagged Invalid);
       //event_fet.recordEvent(tagged Invalid);
       stall_count <= `FET_Miss_Penalty;
       stall_tok   <= tok;
       stalling    <= True;
     end
     
     state       <= FET_Ready;
     
   endrule

endmodule
