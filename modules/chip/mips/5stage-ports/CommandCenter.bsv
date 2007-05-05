
import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

interface CommandCenter;

  method Action start();
  method Action stop();
  method Bool running();
  
  method Action setStopToken(Token t);
  method Maybe#(Token) getStopToken();

endinterface

module mkCommandCenter (CommandCenter);

  Reg#(Bool) r <- mkReg(False);
  Reg#(Maybe#(Token)) mStopToken <- mkReg(Invalid);
  
  method Action start();
    
    r <= True;
  
  endmethod
  
  method Action stop();
    
    r <= False;
    
  endmethod
  
  method Bool running() = r;
  
  method Action setStopToken(Token t);
  
    mStopToken <= Valid t;
    
  endmethod
  
  method getStopToken() = mStopToken;

endmodule
