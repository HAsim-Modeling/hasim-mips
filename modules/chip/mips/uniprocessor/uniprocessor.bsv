
import hasim_common::*;

import hasim_cpu::*;
import hasim_memory::*;

module [HASim_Module] mkChip ();

  let cpu <- mkCpu();
  let mem <- mkMemory();

endmodule
