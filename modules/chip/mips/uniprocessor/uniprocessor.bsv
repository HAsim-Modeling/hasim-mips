
import hasim_common::*;

import hasim_cpu::*;
import hasim_memory::*;

module [HASIM_MODULE] mkChip ();

  let cpu <- mkCPU();
  let mem <- mkMemory();

endmodule
