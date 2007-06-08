import hasim_common::*;

import hasim_pipe_fetch::*;
import hasim_pipe_decode::*;
import hasim_pipe_issue::*;
import hasim_pipe_execute::*;
import hasim_pipe_commit::*;

module [HASim_Module] mkCPU();
    let fetch   <- mkFetch();
    let decode  <- mkDecode();
    let issue   <- mkIssue();
    let execute <- mkExecute();
    let commit  <- mkCommit();
endmodule
