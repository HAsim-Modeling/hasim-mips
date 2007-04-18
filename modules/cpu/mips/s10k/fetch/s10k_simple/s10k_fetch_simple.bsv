
import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_isa::*;

module [HASim_Module] mkFetch
    //interface:
                (Empty);

  Connection_Send#(Bit#(8))   fp_tok_req  <- mkConnection_Send("fp_tok_req");
  Connection_Receive#(Token)  fp_tok_resp <- mkConnection_Receive("fp_tok_resp");
  Connection_Send#(Token)     fp_tok_kill <- mkConnection_Send("fp_tok_kill");
  
  Connection_Send#(Tuple2#(Token, Addr))     fp_fet_req  <- mkConnection_Send("fp_fet_req");

  Port_Send#(Bit#(32)) port <- mkPort_Send("fetch_to_decode");

  Reg#(Bit#(32)) sendVal <- mkReg(22);

  rule incVal(True);
    sendVal <= sendVal + 1;
  endrule

  rule fillPort(True);
    port.send(tagged Valid sendVal);
  endrule

endmodule
