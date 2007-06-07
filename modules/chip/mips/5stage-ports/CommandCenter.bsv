
import hasim_common::*;

interface CommandCenter;

  method Action start();
  method Action stop();
  method Bool running();
  
  method Action setStopToken(Token t);
  method Maybe#(Token) getStopToken();

  method Action setPassFail(Bool pf);
  method Bool getPassFail();
endinterface

module mkCommandCenter (CommandCenter);

  Reg#(Bool) r <- mkReg(False);
  Reg#(Bool) passed <- mkReg(False);
  Reg#(Maybe#(Token)) mStopToken <- mkReg(tagged Invalid);
  
  method Action start();
    
    r <= True;
  
  endmethod
  
  method Action stop();
    
    r <= False;
    
  endmethod
  
  method Bool running() = r;
  
  method Action setStopToken(Token t);
  
    mStopToken <= tagged Valid t;
    
  endmethod
  
  
  method getStopToken() = mStopToken;

  method Action setPassFail(Bool pf);
  
    passed <= pf;
    
  endmethod
  
  method getPassFail() = passed;
  
endmodule
