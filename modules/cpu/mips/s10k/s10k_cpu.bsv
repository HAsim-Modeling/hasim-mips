
import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_fetch::*;
import hasim_decode::*;
import hasim_issue::*;
import hasim_execute::*;

module [HASim_Module] mkCpu
    //interface:
                (TModule#(Command, Response));


  let fetch   <- mkFetch();
  let decode  <- mkDecode();
  let issue   <- mkIssue();
  let execute <- mkExecute();

  Reg#(Bool) ran <- mkReg(False);

  method Action exec(Command c);
    ran <= True;
  endmethod

  method ActionValue#(Response) response() if(ran && decode.done());
    return RESP_DoneRunning;
  endmethod

endmodule