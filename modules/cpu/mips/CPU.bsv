
import hasim_common::*;
import hasim_isa::*;

import hasim_command_center::*;
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

  CommandCenter cc <- mkCommandCenter();
  Connection_Server#(Command, Response)  link_controller <- mkConnection_Server("controller_to_tp");
  Reg#(Bool) ran <- mkReg(False);

  let fet <- mkPipe_Fetch(cc, debug_file, curTick);
  let dec <- mkPipe_Decode(cc, debug_file, curTick);
  let exe <- mkPipe_Execute(cc, debug_file, curTick);
  let mem <- mkPipe_Mem(cc, debug_file, curTick);
  let wb  <- mkPipe_Writeback(cc, debug_file, curTick);

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

  rule startup (True);
  
    let cmd <- link_controller.getReq();
    
    case (cmd) matches
      tagged COM_RunProgram:
      begin
        cc.start();
	ran <= True;
      end
      default:
        noAction;
    endcase
  
  endrule

  rule finishup (ran && !cc.running);
  
    link_controller.makeResp(tagged RESP_DoneRunning cc.getPassFail());
    ran <= False;
    
  endrule

endmodule
