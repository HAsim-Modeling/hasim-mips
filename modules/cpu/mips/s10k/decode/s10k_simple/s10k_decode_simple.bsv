
import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_isa::*;

interface Decode;
    method Bool done();
endinterface

module [HASim_Module] mkDecode
    //interface:
                (Decode);

  Connection_Receive#(Tuple2#(Token, PackedInst)) fp_fet_resp <- mkConnection_Receive("fp_fet_resp");

  Connection_Send#(Tuple2#(Token, void))        fp_dec_req  <- mkConnection_Send("fp_dec_req");
  Connection_Receive#(Tuple2#(Token, DepInfo))  fp_dec_resp <- mkConnection_Receive("fp_dec_resp");

  Connection_Send#(Tuple2#(Token, void))    fp_lco_req  <- mkConnection_Send("fp_lco_req");
  Connection_Receive#(Tuple2#(Token, void)) fp_lco_resp <- mkConnection_Receive("fp_lco_resp");
  
  Connection_Send#(Tuple2#(Token, void))    fp_gco_req  <- mkConnection_Send("fp_gco_req");
  Connection_Receive#(Tuple2#(Token, void)) fp_gco_resp <- mkConnection_Receive("fp_gco_resp");
    
  Connection_Send#(Token) link_memstate_kill <- mkConnection_Send("fp_memstate_kill");

  Connection_Send#(Token)     fp_fet_kill <- mkConnection_Send("fp_fet_kill");
  Connection_Send#(Token)     fp_dec_kill <- mkConnection_Send("fp_dec_kill");
  Connection_Send#(Token)     fp_exe_kill <- mkConnection_Send("fp_exe_kill");
  Connection_Send#(Token)     fp_mem_kill <- mkConnection_Send("fp_mem_kill");
  Connection_Send#(Token)     fp_lco_kill <- mkConnection_Send("fp_lco_kill");
  Connection_Send#(Token)     fp_gco_kill <- mkConnection_Send("fp_gco_kill");

  Connection_Send#(Token)     fp_rewindToToken <- mkConnection_Send("fp_rewindToToken");

  Port_Receive#(Bit#(32))     port <- mkPort_Receive("fetch_to_decode", 1);

  Reg#(Bit#(32)) modelCC <- mkReg(0);
  Reg#(Bit#(32)) fpgaCC  <- mkReg(0);

    rule alway(True);
        fpgaCC <= fpgaCC + 1;
    endrule

    rule receive(True);
        let s <- port.receive();
        case (s) matches
            tagged Invalid : $display("%d %d :Decode: received nothing", fpgaCC, modelCC);
            tagged Valid .v: $display("%d %d :Decode: received %d", fpgaCC, modelCC, v);
        endcase
        modelCC <= modelCC + 1;
    endrule

    method Bool done();
        return False;
    endmethod

endmodule
