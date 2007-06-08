
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

  let fet <- mk5stage_FET(cc);
  let dec <- mk5stage_DEC(cc);
  let exe <- mk5stage_EXE(cc);
  let mem <- mk5stage_MEM(cc);
  let wb  <- mk5stage_WB(cc);

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
