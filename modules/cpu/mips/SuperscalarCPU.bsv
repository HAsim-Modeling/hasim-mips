import hasim_common::*;

import hasim_cpu_fetch::*;
import hasim_cpu_decode::*;
import hasim_cpu_issue::*;
import hasim_cpu_execute::*;
import hasim_cpu_commit::*;

module [HASim_Module] mkCPU();
    let fetch   <- mkFetch();
    let decode  <- mkDecode();
    let issue   <- mkIssue();
    let execute <- mkExecute();
    let commit  <- mkCommit();
endmodule
