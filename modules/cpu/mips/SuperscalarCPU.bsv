import hasim_common::*;

import hasim_pipe_fetch::*;
import hasim_pipe_decode::*;
import hasim_pipe_issue::*;
import hasim_pipe_execute::*;
import hasim_pipe_writeback::*;

module [HASim_Module] mkCPU();
    let fetch   <- mkPipe_Fetch();
    let decode  <- mkPipe_Decode();
    let issue   <- mkPipe_Issue();
    let execute <- mkPipe_Execute();
    let commit  <- mkPipe_Writeback();
endmodule
