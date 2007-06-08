
import hasim_common::*;

import hasim_command_center::*;
import hasim_pipe_fetch::*;
import hasim_pipe_decode::*;
import hasim_pipe_execute::*;
import hasim_pipe_mem::*;
import hasim_pipe_writeback::*;



module [HASim_Module] mkCPU 
    //interface:
                ();


  CommandCenter cc <- mkCommandCenter();
  Connection_Server#(Command, Response)  link_controller <- mkConnection_Server("controller_to_tp");
  Reg#(Bool) ran <- mkReg(False);

  let fet <- mkPipe_Fetch(cc);
  let dec <- mkPipe_Decode(cc);
  let exe <- mkPipe_Execute(cc);
  let mem <- mkPipe_Mem(cc);
  let wb  <- mkPipe_Writeback(cc);

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
