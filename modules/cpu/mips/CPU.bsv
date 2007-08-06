
import hasim_common::*;
import hasim_isa::*;

import hasim_pipe_fetch::*;
import hasim_pipe_decode::*;
import hasim_pipe_execute::*;
import hasim_pipe_mem::*;
import hasim_pipe_writeback::*;



module [HASim_Module] mkCPU 
    //interface:
                ();
   
  let debug_file <- mkReg(InvalidFile);
  Reg#(Tick) curTick <- mkReg(0);

  Reg#(Bool) ran <- mkReg(False);

  let fet <- mkPipe_Fetch(debug_file, curTick);
  let dec <- mkPipe_Decode(debug_file, curTick);
  let exe <- mkPipe_Execute(debug_file, curTick);
  let mem <- mkPipe_Mem(debug_file, curTick);
  let wb  <- mkPipe_Writeback(debug_file, curTick);

  rule openFile (debug_file == InvalidFile);
  
    let fd <- $fopen("hasim_cpu.out", "w");
    
    if (fd == InvalidFile)
    begin
      $display("CPU: ERROR: Could not open file hasim_cpu.out");
      $finish(1);
    end
    
    debug_file <= fd;
  
  endrule

  rule countCC (True);
  
    curTick <= curTick + 1;
  
  endrule

endmodule
