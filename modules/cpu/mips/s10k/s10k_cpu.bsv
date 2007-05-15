import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_fetch::*;
import hasim_decode::*;
import hasim_issue::*;
import hasim_execute::*;
import hasim_commit::*;

module [HASim_Module] mkCpu();


  let fetch   <- mkFetch();
  let decode  <- mkDecode();
  let issue   <- mkIssue();
  let execute <- mkExecute();
  let commit  <- mkCommit();

  Reg#(Bool) ran <- mkReg(False);

   /*
  method Action exec(Command c);
    ran <= True;
  endmethod

  method ActionValue#(Response) response() if(ran && decode.done());
    return RESP_DoneRunning;
  endmethod
    */
endmodule
