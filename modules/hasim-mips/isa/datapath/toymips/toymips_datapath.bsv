
// mips datapath

// Datapath for executing mips instructions.


// ***** Imports *****

import Vector::*;

import hasim_common::*;
import soft_connections::*;

import hasim_isa::*;

// ***** Modules *****

// mkISA_Datapath

// The toymips datapath itself.

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
    
    // Executes MIPS instructions.
    
    // Parameters: 
    //    * Instruction
    //    * Address (program counter)
    //    * List of source values from registers
    // Returns: 
    //    * Instruction result (given to timing partition)
    //    * Effective address (unused if not a load/store)
    //    * List of values to writeback

    rule datapathExec (True);

        // Get the request from the functional partition.
        match {.inst, .addr, .vs} = link_fp.getReq();
        link_fp.deq();

        // Some convenient variables to return.

        ISA_EXECUTION_RESULT result;
        ISA_ADDRESS eaddr = 0;
        ISA_RESULT_VALUES writebacks = Vector::replicate(Invalid);
         
        MIPS_OPCODE op = inst[31:26];

        case (op)
            mipsLW: // Load word
            begin
                // Calculate the effective address.
                eaddr = vs[0] + vs[1];
                // Return the effective address to the timing partition.
                result = tagged REffectiveAddr eaddr;
            end
            mipsSW:
            begin
                // Calculate the effective address.
                eaddr = vs[0] + vs[1];
                // Return the effective address to the timing partition.
                result = tagged REffectiveAddr eaddr;
            end
            mipsSPECIAL:
            begin
                // Is it an add or a subtract?
                MIPS_SPECIAL_OPCODE special_op = inst[5:0];
                case (special_op)
                    mipsSpecialADDU:
                    begin

                        // Writeback the addition
                        writebacks[0] = tagged Valid (vs[0] + vs[1]);
                        // No information for the timing partition.
                        result = tagged RNop;

                    end
                    mipsSpecialSUBU:
                    begin

                        // Writeback the subtraction
                        writebacks[0] = tagged Valid (vs[0] - vs[1]);
                        // No information for the timing partition.
                        result = tagged RNop;
                    end
                    default:
                    begin
                      $fdisplay(debug_log, "WARNING: EXECUTING UNDEFINED SPECIAL INSTRUCTION.");
                      result = tagged RNop;
                    end
                endcase
            end
            mipsBGTZ:
            begin
                // First see if r > 0
                Bool taken = signedGT(vs[0], 0);
                // Add 4 to the current pc.
                ISA_ADDRESS addr_plus_4 = addr + 4;
                // Resolve the branch.
                if (taken)
                begin
                    // Get the offset from the instruction.
                    Bit#(16) offset = inst[15:0];
                    // Sign Extend the immediate
                    ISA_ADDRESS extimm = signExtend(offset) << 2;
                    // Calculate the destination
                    ISA_ADDRESS dest  = addr_plus_4 + extimm;
                    // The result is taken.
                    result = tagged RBranchTaken dest;
                end
                else
                begin
                    // The correct next pc is pc + 4;
                    result = tagged RBranchNotTaken addr_plus_4;
                end
            end
            mipsCOP0:
            begin

                // If src1 is equal to 1 we passed
                Bool pf = vs[0] == 1;
                // We treat any COP0 instruction as terminate.
                result = tagged RTerminate pf;

            end
            default: 
            begin
                result = tagged RNop;
                $fdisplay(debug_log, "WARNING: EXECUTING UNDEFINED INSTRUCTION.");
            end
        endcase

        // Return the result to the functional partition.
        link_fp.makeResp(tuple3(result, eaddr, writebacks));

    endrule

endmodule
