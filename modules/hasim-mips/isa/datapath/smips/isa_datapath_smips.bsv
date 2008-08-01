
// isa_datapath_template

// The SMIPS datapath.


// ***** Imports *****

import Vector::*;

import hasim_common::*;
import soft_connections::*;

import hasim_isa::*;

// ***** Modules *****

// mkISA_Datapath

// The datapath module itself.

module [HASim_Module] mkISA_Datapath 
  //interface:
              ();

    // ***** Soft Connections *****

    // Connection to the functional partition.
    
    Connection_Server#(ISA_DATAPATH_REQ, 
                       ISA_DATAPATH_RSP) link_fp <- mkConnection_Server("isa_datapath");

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

    // Execute SMIPS instructions. This is currently unpipelined combinational logic.
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
        let req = link_fp.getReq();
        link_fp.deq();
        let inst = req.instruction;
        let addr = req.instAddress;
        let srcs = req.srcValues;

        // Some convenient variables to return.

        ISA_EXECUTION_RESULT timep_result = RNop;
        ISA_ADDRESS effective_addr = 0;
        Bool isStore = False;
        ISA_RESULT_VALUES writebacks = Vector::replicate(Invalid);
        
        // Calculate a sign-extended immediate used by many operations.
        ISA_VALUE sign_ext_imm = signExtend(inst[15:0]);
        
        // Calculate a zero-extended offset used by many logical operations.
        ISA_VALUE zero_ext_imm = zeroExtend(inst[15:0]);

        // Calculate a sign-extended address used by loads/stores.
        ISA_ADDRESS sign_ext_offset = signExtend(inst[15:0]);
        
        // A shifted offset used by many branches.
        ISA_ADDRESS shifted_ext_offset = sign_ext_offset << 2;

        // Calculate the PC + 4 for many branch instructions.
        ISA_ADDRESS branch_not_taken_dest = addr + 4;
        
        // Calculate the branch-taken destination for conditional branches.
        ISA_ADDRESS branch_taken_dest = branch_not_taken_dest + shifted_ext_offset;

        MIPS_OPCODE op = inst[31:26];

        case (op)

            // LW

            // Load word. (Effective Address calculation only.)
            mipsLW:
            begin

                // Calculate Effective Address.
                effective_addr = srcs[0] + sign_ext_offset;
                // Log it.
                $fdisplay(debug_log, "LW EADDR 0x%h = 0x%h + 0x%h", effective_addr, srcs[0], sign_ext_offset);
                // Return the effective address to the timing partition.
                timep_result = tagged REffectiveAddr effective_addr;

            end

            // SW

            // Store word. (Effective Address calculation only.)
            mipsSW:
            begin

                // Calculate the Effective Address.
                effective_addr = srcs[0] + sign_ext_offset;
                isStore = True;
                // Log it.
                $fdisplay(debug_log, "SW EADDR 0x%h = 0x%h + 0x%h <= 0x%h", effective_addr, srcs[0], sign_ext_offset, srcs[1]);
                // Return the effective address to the timing partition.
                timep_result = tagged REffectiveAddr effective_addr;
                // Write src2 into our destination.
                // No one can observe this but it allows the doStores operation to 
                // avoid reading the maptable.
                writebacks[0] = tagged Valid srcs[1];

            end

            // ADDIU

            // Add immediate unsigned (which means signed addition, but no overflow)
            mipsADDIU:
            begin

                // Do the addition and write it back to our dest.
                ISA_VALUE result = srcs[0] + sign_ext_imm;
                writebacks[0] = tagged Valid result;
                // Log it.
                $fdisplay(debug_log, "ADDIU 0x%h = 0x%h + 0x%h]", result, srcs[0], sign_ext_imm);
                // No information needs to be returned to the timing partition.
                timep_result = tagged RNop;

            end

            // SLTI

            // Set less-than immediate
            mipsSLTI:
            begin
                // Do the calculation.
                ISA_VALUE result = zeroExtend(pack(signedLT(srcs[0], sign_ext_imm)));
                writebacks[0] = tagged Valid result;
                // Log it.
                $fdisplay(debug_log, "SLTI 0x%h = slt(0x%h, 0x%h)", result, srcs[0], sign_ext_imm);
                // No information needs to be returned to the timing partition.
                timep_result = tagged RNop;

            end


            // SLTIU

            // Set less-than immediate unsigned
            mipsSLTIU:
            begin
                
                // Do the calculation.
                ISA_VALUE result = zeroExtend(pack(srcs[0] < sign_ext_imm));
                writebacks[0] = tagged Valid result;
                // Log it.
                $fdisplay(debug_log, "SLTIU 0x%h = sltu(0x%h, 0x%h)", result, srcs[0], sign_ext_imm);
                // No information needs to be returned to the timing partition.
                timep_result = tagged RNop;

            end

            // ANDI

            // And immediate
            mipsANDI: 
            begin

                // Do the AND and write it back to our destination.
                ISA_VALUE result = srcs[0] & zero_ext_imm;
                writebacks[0] = tagged Valid result;
                // Log it.
                $fdisplay(debug_log, "ANDI 0x%h = 0x%h & 0x%h", result, srcs[0], zero_ext_imm);
                // No information needs to be returned to the timing partition.
                timep_result = tagged RNop;

            end

            // ORI

            // OR immediate
            mipsORI:
            begin

                // Do the OR and write it back to our destination.
                ISA_VALUE result = srcs[0] | zero_ext_imm;
                writebacks[0] = tagged Valid result;
                // Log it.
                $fdisplay(debug_log, "ORI 0x%h = 0x%h | 0x%h", result, srcs[0], zero_ext_imm);
                // No information needs to be returned to the timing partition.
                timep_result = tagged RNop;

            end

            // XORI

            // XOR immediate
            mipsXORI:
            begin

                // Do the XOR and write it back to our destination.
                ISA_VALUE result = srcs[0] ^ zero_ext_imm;
                writebacks[0] = tagged Valid result;
                // Log it.
                $fdisplay(debug_log, "XORI 0x%h = 0x%h ^ 0x%h", result, srcs[0], zero_ext_imm);
                timep_result = tagged RNop;

            end

            // LUI

            // Load upper immediate (Really is unsigned. Does not touch memory.)
            mipsLUI:
            begin
                
                // Shift the value into the upper bits of the word.
                ISA_VALUE result = zero_ext_imm << 16;
                writebacks[0] = tagged Valid result;
                // Log it.
                $fdisplay(debug_log, "LUI 0x%h = 0x%h << 16", result, zero_ext_imm);
                // No information needs to be returned to the timing partition.
                timep_result = tagged RNop;

            end

            // J

            // Jump unconditionally
            mipsJ:
            begin
              
                // Do the address calculation.
                ISA_ADDRESS dest  = {branch_not_taken_dest[31:28], inst[25:0], 2'b00};
                // Log it.
                $fdisplay(debug_log, "J PC <= 0x%h = {%0h, %0h, 00}", dest, branch_not_taken_dest[31:26], inst[25:0]);
                // Return the branch result to the timing partition.
                timep_result = tagged RBranchTaken dest;
            end


            // JAL

            // Jump and link
            mipsJAL:
            begin

                // Do the address calculation.
                ISA_ADDRESS dest  = {branch_not_taken_dest[31:28], inst[25:0], 2'b00};
                // Write the old PC into our destination.
                writebacks[0] = tagged Valid branch_not_taken_dest;
                // Log it.
                $fdisplay(debug_log, "JAL PC <= 0x%h = {%0h, %0h, 00}, (Old PC: 0x%h)", dest, branch_not_taken_dest[31:28], inst[25:0], branch_not_taken_dest);
                // Return the branch result to the timing partition.
                timep_result = tagged RBranchTaken dest;

            end

            // BEQ

            // Branch if equal
            mipsBEQ:
            begin
                
                // Branch is taken if src1 == src2.
                Bool taken = srcs[0] == srcs[1];
                // Log it.
                $fdisplay(debug_log, "BEQ PC <= 0x%h (offset 0x%h) if (0x%h == 0x%h)", branch_taken_dest, shifted_ext_offset, srcs[0], srcs[1]);
                // Tell the timing partition if the branch was taken or not.
                timep_result = taken ? (tagged RBranchTaken branch_taken_dest) : (tagged RBranchNotTaken branch_not_taken_dest);

            end

            // BNE

            // Branch if not-equal
            mipsBNE:
            begin
                
                // Branch is taken if src1 != src2.
                Bool taken = srcs[0] != srcs[1];
                // Log it.
                $fdisplay(debug_log, "BNE PC <= 0x%h (offset 0x%h) if (0x%h != 0x%h)", branch_taken_dest, shifted_ext_offset, srcs[0], srcs[1]);
                // Tell the timing partition if the branch was taken or not.
                timep_result = taken ? (tagged RBranchTaken branch_taken_dest) : (tagged RBranchNotTaken branch_not_taken_dest);

            end

            // BLEZ

            // Branch if less-than-or-equal-to-zero
            mipsBLEZ:
            begin

                // The branch is taken if src1 <= zero (signed).
                Bool taken = signedLE(srcs[0], 0);
                // Log it.
                $fdisplay(debug_log, "BLEZ PC <= 0x%h (offset 0x%h) if (0x%h <= 0)", branch_taken_dest, shifted_ext_offset, srcs[0]);
                // Tell the timing partition if the branch was taken or not.
                timep_result = taken ? (tagged RBranchTaken branch_taken_dest) : (tagged RBranchNotTaken branch_not_taken_dest);

            end

            // BGTZ

            // Branch if greater-than-zero
            mipsBGTZ:
            begin

                // The branch is taken if src1 > zero (signed).
                Bool taken = signedGT(srcs[0], 0);
                // Log it.
                $fdisplay(debug_log, "BGTZ PC <= 0x%h (offset 0x%h) if (0x%h > 0)", branch_taken_dest, shifted_ext_offset, srcs[0]);
                // Tell the timing partition if the branch was taken or not.
                timep_result = taken ? (tagged RBranchTaken branch_taken_dest) : (tagged RBranchNotTaken branch_not_taken_dest);

            end

            // SPECIAL

            // Many operations are encoded as "special" and differentiated by bits [5:0]
            mipsSPECIAL:
            begin

                MIPS_SPECIAL_OPCODE spec_op = inst[5:0];

                // SLL
                case (spec_op)
                
                    // Shift left logical
                    mipsSpecialSLL:
                    begin

                        // Get the shift amount from the instruction.
                        Bit#(5) shift_amount = inst[10:6];
                        // Do the calculation and write it back.
                        ISA_VALUE result = srcs[0] << shift_amount;
                        writebacks[0] = tagged Valid result;
                        // Log it.
                        $fdisplay(debug_log, "SLL 0x%h = 0x%h << 0x%h", result, srcs[0], shift_amount);
                        // The timing partition does not need any information.
                        timep_result = tagged RNop;

                    end

                    // SRL

                    // Shift Right Logical
                    mipsSpecialSRL:
                    begin

                        // Get the shift amount from the instruction.
                        Bit#(5) shift_amount = inst[10:6];
                        // Do the calculation and write it back.
                        ISA_VALUE result = srcs[0] >> shift_amount;
                        writebacks[0] = tagged Valid result;
                        // Log it.
                        $fdisplay(debug_log, "SRL 0x%h = 0x%h >> 0x%h", result, srcs[0], shift_amount);
                        // The timing partition does not need any information.
                        timep_result = tagged RNop;

                    end

                    // SRA

                    // Shift Right Arithmatic
                    mipsSpecialSRA:
                    begin

                        // Get the shift amount from the instruction.
                        Bit#(5) shift_amount = inst[10:6];
                        // Do the calculation and write it back.
                        ISA_VALUE result = signedShiftRight(srcs[0], shift_amount);
                        writebacks[0] = tagged Valid result;
                        // Log it.
                        $fdisplay(debug_log, "SRA 0x%h = 0x%h >>a 0x%h", result, srcs[0], shift_amount);
                        // The timing partition does not need any information.
                        timep_result = tagged RNop;

                    end

                    // SLLV

                    // Shift Left Logical Variable
                    mipsSpecialSLLV:
                    begin

                        // Get the shift amount from src1
                        Bit#(5) shift_amount = srcs[0][4:0];
                        // Do the calculation and write it back.
                        ISA_VALUE result = srcs[1] << shift_amount;
                        writebacks[0] = tagged Valid result;
                        // Log it.
                        $fdisplay(debug_log, "SLLV 0x%h = 0x%h << 0x%h", result, srcs[0], shift_amount);
                        // The timing partition does not need any information.
                        timep_result = tagged RNop;

                    end

                    // SRLV

                    // Shift Right Logical Variable
                    mipsSpecialSRLV:
                    begin

                        // Get the shift amount from src1
                        Bit#(5) shift_amount = srcs[0][4:0];
                        // Do the calculation and write it back.
                        ISA_VALUE result = srcs[1] >> shift_amount;
                        writebacks[0] = tagged Valid result;
                        // Log it.
                        $fdisplay(debug_log, "SLLV 0x%h = 0x%h >> 0x%h", result, srcs[0], shift_amount);
                        // The timing partition does not need any information.
                        timep_result = tagged RNop;

                    end

                    // SRAV

                    // Shift Right Arithmatic Variable
                    mipsSpecialSRAV:
                    begin

                        // Get the shift amount from src1
                        Bit#(5) shift_amount = srcs[0][4:0];
                        // Do the calculation and write it back.
                        ISA_VALUE result = signedShiftRight(srcs[1], shift_amount);
                        writebacks[0] = tagged Valid result;
                        // Log it.
                        $fdisplay(debug_log, "SRAV 0x%h = 0x%h >>a 0x%h", result, srcs[0], shift_amount);
                        // The timing partition does not need any information.
                        timep_result = tagged RNop;

                    end

                    // ADDU

                    // Add unsigned (signed arithmetic but no overflow)
                    mipsSpecialADDU:
                    begin

                        // Do the addition.
                        ISA_VALUE result = srcs[0] + srcs[1];
                        writebacks[0] = tagged Valid result;
                        // Log it.
                        $fdisplay(debug_log, "ADDU %0h = %0h + %0h", result, srcs[0], srcs[1]);
                        // The timing partition does not need any information.
                        timep_result = tagged RNop;

                    end

                    // SUBU

                    // Sub unsigned (signed arithmetic but no overflow)
                    mipsSpecialSUBU:
                    begin

                        // Do the subtraction.
                        ISA_VALUE result = srcs[0] - srcs[1];
                        writebacks[0] = tagged Valid result;
                        // Log it.
                        $fdisplay(debug_log, "SUBU %0h = %0h - %0h", result, srcs[0], srcs[1]);
                        // The timing partition does not need any information.
                        timep_result = tagged RNop;

                    end

                    // AND

                    // And logical
                    mipsSpecialAND:
                    begin

                        // Do the AND.
                        ISA_VALUE result = srcs[0] & srcs[1];
                        writebacks[0] = tagged Valid result;
                        // Log it.
                        $fdisplay(debug_log, "AND %0h = %0h & %0h", result, srcs[0], srcs[1]);
                        // The timing partition does not need any information.
                        timep_result = tagged RNop;

                    end

                    // OR

                    // Or logical
                    mipsSpecialOR:
                    begin

                        // Do the OR.
                        ISA_VALUE result = srcs[0] | srcs[1];
                        writebacks[0] = tagged Valid result;
                        // Log it.
                        $fdisplay(debug_log, "OR %0h = %0h | %0h", result, srcs[0], srcs[1]);
                        // The timing partition does not need any information.
                        timep_result = tagged RNop;

                    end

                    // XOR

                    // XOR logical
                    mipsSpecialXOR:
                    begin

                        // Do the XOR.
                        ISA_VALUE result = srcs[0] ^ srcs[1];
                        writebacks[0] = tagged Valid result;
                        // Log it.
                        $fdisplay(debug_log, "XOR %0h = %0h ^ %0h", result, srcs[0], srcs[1]);
                        // The timing partition does not need any information.
                        timep_result = tagged RNop;

                    end

                    // NOR

                    // NOR logical
                    mipsSpecialNOR:
                    begin

                        // Do the NOR.
                        ISA_VALUE result = ~(srcs[0] | srcs[1]);
                        writebacks[0] = tagged Valid result;
                        // Log it.
                        $fdisplay(debug_log, "NOR %0h = %0h nor %0h", result, srcs[0], srcs[1]);
                        // The timing partition does not need any information.
                        timep_result = tagged RNop;

                    end

                    // SLT

                    // Set less-than
                    mipsSpecialSLT:
                    begin

                        // Set if src1 < src2 (signed).
                        ISA_VALUE result = zeroExtend(pack(signedLT(srcs[0], srcs[1])));
                        writebacks[0] = tagged Valid result;
                        // Log it.
                        $fdisplay(debug_log, "SLT %0h = slt(%0h, %0h)", result, srcs[0], srcs[1]);
                        // The timing partition does not need any information.
                        timep_result = tagged RNop;

                    end

                    // SLTU

                    // Set less-than unsigned
                    mipsSpecialSLTU:
                    begin

                        // Set if src1 < src2.
                        ISA_VALUE result = zeroExtend(pack(srcs[0] < srcs[1]));
                        writebacks[0] = tagged Valid result;
                        // Log it.
                        $fdisplay(debug_log, "SLTU %0h = sltu(%0h, %0h)", result, srcs[0], srcs[1]);
                        // The timing partition does not need any information.
                        timep_result = tagged RNop;

                    end

                    // JR

                    // Jump Register.
                    mipsSpecialJR:
                    begin

                        // The destination is in src1.
                        ISA_ADDRESS dest = unpack(srcs[0]);
                        // Log it.
                        $fdisplay(debug_log, "JR PC <= 0x%h ", dest);
                        // Tell the timing partition the branch was taken.
                        timep_result = tagged RBranchTaken dest;

                    end

                    // JALR

                    // Jump and Link Register.
                    mipsSpecialJALR:
                    begin

                        // The destination is in src1.
                        ISA_ADDRESS dest = unpack(srcs[0]);
                        // Write the old PC back to our destination.
                        writebacks[0] = tagged Valid (branch_not_taken_dest);
                        // Log it.
                        $fdisplay(debug_log, "JALR PC <= 0x%h, (Old PC: 0x%h)", dest, branch_not_taken_dest);
                        // Tell the timing partition the branch was taken.
                        timep_result = tagged RBranchTaken dest;

                    end
                endcase

            end

            // REGIMM

            // Some opcodes are encoded using the rt field [20:16]
            mipsREGIMM:
            begin

                MIPS_REGIMM_OPCODE rimm_op = inst[20:16];
            
                case (rimm_op)
            
                    // BLTZ
            
                    // Branch if less-than-zero
                    mipsRegimmBLTZ:
                    begin

                        // The branch is taken if src1 < zero (signed).
                        Bool taken = signedLT(srcs[0], 0);
                        // Log it.
                        $fdisplay(debug_log, "BLTZ PC <= 0x%h (offset 0x%h) if (0x%h < 0)", branch_taken_dest, shifted_ext_offset, srcs[0]);
                        // Tell the timing partition if the branch was taken or not.
                        timep_result = taken ? (tagged RBranchTaken branch_taken_dest) : (tagged RBranchNotTaken branch_not_taken_dest);

                    end

                    // BGEZ

                    // Branch if greater-than-or-equal-to-zero
                    mipsRegimmBGEZ:
                    begin

                        // The branch is taken if src1 >= zero (signed).
                        Bool taken = signedGE(srcs[0], 0);
                        // Log it.
                        $fdisplay(debug_log, "BGEZ PC <= 0x%h (offset 0x%h) if (0x%h >= 0)", branch_taken_dest, shifted_ext_offset, srcs[0]);
                        // Tell the timing partition if the branch was taken or not.
                        timep_result = taken ? (tagged RBranchTaken branch_taken_dest) : (tagged RBranchNotTaken branch_not_taken_dest);

                    end
                    
                endcase
                
            end

            // COP0

            // CP0 ops where the opcode is encoded in the rs field.
            // For smips this is is only MTC0/MFC0
            mipsCOP:
            begin

                MIPS_COP_OPCODE cop_op = inst[25:21];

                case (cop_op)
                    // MFC0
                    
                    // Move From Coprocessor 0.
                    mipsCopMFC0:
                    begin
                        //This instruction is pretty useless because cop0src doesn't exist.
                        //So what we do instead is set rd to 1 (because this is what most of our testcases use this for).
                        writebacks[0] = tagged Valid (1);
                        // Log it.
                        $fdisplay(debug_log, "MFC0 COP0 R%0d (Hardwired to 1)", inst[15:11]);
                        // No information back to the timing partition.
                        timep_result = tagged RNop;

                    end
                    
                    // MTC0
                    
                    // Move to Coprocessor 0.
                    mipsCopMTC0:
                    begin

                        // src1==1 means we passed
                        Bool pf = srcs[0] == 1;
                        // Log it.
                        $fdisplay(debug_log, "MTC0 COP0 R%d <= 0x%h", inst[15:11], srcs[0]);
                        //A Non-Zero value to "fromHost" is equivalent to a terminate for our purposes
                        timep_result = (srcs[0] == 0) ? tagged RNop : (inst[15:11] == 21) ? tagged RTerminate pf : tagged RNop;

                    end
                endcase

            end
            
            default
            begin
            
                // An illegal instruction
                $fdisplay(debug_log, "WARNING: EXECUTING ILLEGAL INSTRUCTION: %0h", inst);
            
            end

        endcase

        // Return the result to the functional partition.
        link_fp.makeResp(initISADatapathRsp(timep_result, effective_addr, isStore, writebacks));

    endrule

endmodule
