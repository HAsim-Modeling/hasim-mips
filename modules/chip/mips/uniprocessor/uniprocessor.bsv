
import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_cpu::*;
import hasim_memory::*;

module [HASim_Module] mkChip ();

  let cpu <- mkCpu();
  let mem <- mkMemory();

  /*
  method Action exec(Command c);

    cpu.exec(c);
  
  endmethod

  method ActionValue#(Response) response();
  
    let r <- cpu.response();
    return r;

  endmethod
  */

endmodule
