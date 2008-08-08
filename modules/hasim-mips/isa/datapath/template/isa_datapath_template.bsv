
// isa_datapath_template

// This file contains a template ISA datapath.

// You should fill this in with with an ISA-specific ALU, 
// which may be pipelined if you so choose.


// ***** Imports *****

import Vector::*;

import hasim_common::*;
import soft_connections::*;

import hasim_isa::*;

// ***** Modules *****

// mkISA_Datapath

// The datapath module itself.

module [HASIM_MODULE] mkISA_Datapath 
  //interface:
              ();

    // ***** Soft Connections *****

    // Connection to the functional partition.
    
    Connection_Server#(Tuple3#(ISA_INSTRUCTION, ISA_ADDRESS, ISA_SOURCE_VALUES), 
                       Tuple3#(ISA_EXECUTION_RESULT, ISA_ADDRESS, ISA_RESULT_VALUES)) link_fp <- mkConnection_Server("isa_datapath");

    // ***** Debugging Log *****
    
    // This logfile is available for debugging during Bluesim simulation.
    // It has no affect on the FPGA.
    
    let debug_log <- mkReg(InvalidFile);


    // ***** Rules ******

    
    // openLog
    
    // Opens the logfile for writing. The filename is an AWB parameter.

    rule open_log (debug_log == InvalidFile);

        let fd <- $fopen(`HASIM_ISA_DP_LOGFILE, "w");

        if (fd == InvalidFile)
        begin
          $display("ERROR: ISA: Datapath: Could not create logfile %s", `HASIM_ISA_DP_LOGFILE);
          $finish(1);
        end

        debug_log <= fd;

    endrule


    // datapathExec

    // You should write this rule to execute your ISA-specific instructions.
    // Parameters: 
    //    * Instruction
    //    * Address (program counter)
    //    * List of source values from registers
    // Returns: 
    //    * Instruction result (given to timing partition)
    //    * Effective address (unused if not a load/store)
    //    * List of values to writeback

    // Note that you may choose to break this rule up into a pipeline for efficiency.

    rule datapathExec (True);

        // Get the request from the functional partition.
        match {.inst, .addr, .srcs} = link_fp.getReq();
        link_fp.deq();

        // Some convenient variables to return.

        // The result for the timing partition.
        ISA_EXECUTION_RESULT timep_result;
        
        // The effective address for Loads/Stores
        ISA_ADDRESS effective_addr = 0;
        
        // The writebacks which are sent to the register file.
        ISA_RESULT_VALUES writebacks = Vector::replicate(Invalid);

        case (inst) matches // You should write this.
            default: 
            begin
                timep_result = tagged RNop;
                $fdisplay(debug_log, "WARNING: EXECUTING UNDEFINED INSTRUCTION.");
            end
        endcase

        // Return the result to the functional partition.
        link_fp.makeResp(tuple3(timep_result, effective_addr, writebacks));

    endrule

endmodule
