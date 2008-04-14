
// isa_decode_functions

// This file contains functions which decode a mips instruction.

// First some helper functions


// 6 bits of an instruction are the opcode.
typedef Bit#(6) MIPS_OPCODE;

// LW

// Load word.
MIPS_OPCODE mipsLW = 6'b100011;

// SW

// Store word.
MIPS_OPCODE mipsSW = 6'b101011;

// ADDIU

// Add immediate unsigned (which means signed addition, but no overflow)
MIPS_OPCODE mipsADDIU = 6'b001001;

// SLTI

// Set less-than immediate
MIPS_OPCODE mipsSLTI  = 6'b001010;

// SLTIU

// Set less-than immediate unsigned
MIPS_OPCODE mipsSLTIU = 6'b001011;

// ANDI

// And immediate
MIPS_OPCODE mipsANDI  = 6'b001100;

// ORI

// OR immediate
MIPS_OPCODE mipsORI   = 6'b001101;

// XORI

// XOR immediate
MIPS_OPCODE mipsXORI  = 6'b001110;

// LUI

// Load upper immediate (does not touch memory)
MIPS_OPCODE mipsLUI   = 6'b001111;

// J

// Jump unconditionally
MIPS_OPCODE mipsJ     = 6'b000010;

// JAL

// Jump and link
MIPS_OPCODE mipsJAL   = 6'b000011;

// BEQ

// Branch if equal
MIPS_OPCODE mipsBEQ   = 6'b000100;

// BNE

// Branch if not-equal

MIPS_OPCODE mipsBNE   = 6'b000101;

// BLEZ

// Branch if less-than-or-equal-to-zero
MIPS_OPCODE mipsBLEZ  = 6'b000110;

// BGTZ

// Branch if greater-than-zero
MIPS_OPCODE mipsBGTZ  = 6'b000111;


// SPECIAL

// Many operations are encoded as "special" and differentiated by bits [5:0]
MIPS_OPCODE mipsSPECIAL = 6'b000000;

// REGIMM

// Some opcodes are encoded using the rt field [20:16]
MIPS_OPCODE mipsREGIMM = 6'b0000001;

// COP0

// CP0 ops where the opcode is encoded in the rs field.
// For smips this is is only MTC0/MFC0
MIPS_OPCODE mipsCOP = 6'b010000;


// SPECIAL Opcodes

// If the opcode is SPECIAL than a different field shows the actual
// operation.

typedef Bit#(6) MIPS_SPECIAL_OPCODE;

// SLL

// Shift left logical
MIPS_SPECIAL_OPCODE mipsSpecialSLL   = 6'b000000;

// SRL

// Shift Right Logical
MIPS_SPECIAL_OPCODE mipsSpecialSRL   = 6'b000010;

// SRA

// Shift Right Arithmatic
MIPS_SPECIAL_OPCODE mipsSpecialSRA   = 6'b000011;

// SLLV

// Shift Left Logical Variable
MIPS_SPECIAL_OPCODE mipsSpecialSLLV  = 6'b000100;

// SRLV

// Shift Right Logical Variable
MIPS_SPECIAL_OPCODE mipsSpecialSRLV  = 6'b000110;

// SRAV

// Shift Right Arithmatic Variable
MIPS_SPECIAL_OPCODE mipsSpecialSRAV  = 6'b000111;

// ADDU

// Add unisigned (signed arithmetic but no overflow)
MIPS_SPECIAL_OPCODE mipsSpecialADDU  = 6'b100001;

// SUBU

// Sub unsigned (signed arithmetic but no overflow)
MIPS_SPECIAL_OPCODE mipsSpecialSUBU  = 6'b100011;

// AND

// And logical
MIPS_SPECIAL_OPCODE mipsSpecialAND   = 6'b100100;

// OR

// Or logical
MIPS_SPECIAL_OPCODE mipsSpecialOR    = 6'b100101;

// XOR

// XOR logical
MIPS_SPECIAL_OPCODE mipsSpecialXOR   = 6'b100110;

// NOR

// NOR logical
MIPS_SPECIAL_OPCODE mipsSpecialNOR   = 6'b100111;

// SLT

// Set less-than
MIPS_SPECIAL_OPCODE mipsSpecialSLT   = 6'b101010;

// SLTU

// Set less-than unsigned
MIPS_SPECIAL_OPCODE mipsSpecialSLTU  = 6'b101011;

// JR

// Jump to register
MIPS_SPECIAL_OPCODE mipsSpecialJR    = 6'b001000;

// JALR

// Jump and link to register.
MIPS_SPECIAL_OPCODE mipsSpecialJALR  = 6'b001001;


// REGIMM Opcodes

typedef Bit#(5) MIPS_REGIMM_OPCODE;

// BLTZ

// Branch if less-than-zero
MIPS_REGIMM_OPCODE mipsRegimmBLTZ  = 5'b00000;

// BGEZ

// Branch if greater-than-or-equal-to-zero
MIPS_REGIMM_OPCODE mipsRegimmBGEZ  = 5'b00001;

// COP Opcodes

typedef Bit#(5) MIPS_COP_OPCODE;

MIPS_COP_OPCODE mipsCopMFC0  = 5'b00000;
MIPS_COP_OPCODE mipsCopMTC0  = 5'b00100;

// isaGetSrc

// Given a mips instruction, return the nth source register.
// Or return Invalid if there is no such source for this instruction.
// Implemented using the helper functions below.

function Maybe#(ISA_REG_INDEX) isaGetSrc(ISA_INSTRUCTION i, Integer n);

    case (n)
      0: return mipsGetSrc1(i);
      1: return mipsGetSrc2(i);
    endcase

endfunction

// mipsGetSrc1

// Given an instruction, return source 1 (rs)
// All smips instructions except LUI, J, JAL and MFC0 have at least one source.
// MTCO and non-variable shift operations keeps their src1 in rt.


function Maybe#(ISA_REG_INDEX) mipsGetSrc1(ISA_INSTRUCTION i);
    
    MIPS_OPCODE op = i[31:26];

    case (op)
        // LUI has no src1
        mipsLUI: return tagged Invalid;
        // J has no src1
        mipsJ:   return tagged Invalid;
        // JAL has no src1
        mipsJAL: return tagged Invalid;
        // SPECIAL operations which are shifts are different.
        mipsSPECIAL:
        begin
          MIPS_SPECIAL_OPCODE sp_op = i[5:0];
          
          case (sp_op)
            mipsSpecialSLL,
            mipsSpecialSRL,
            mipsSpecialSRA:  return tagged Valid i[20:16];
            default:         return tagged Valid i[25:21];
          endcase
        
        end
        // COP operations are different.
        mipsCOP:
        begin
          MIPS_COP_OPCODE cop_op = i[25:21];
          
          case (cop_op)
              // MFC0 has no src1
              mipsCopMFC0: return tagged Invalid;
              // MTC0 keeps its src1 in rt
              mipsCopMTC0: return tagged Valid i[20:16];
          endcase
        
        end
        // Every other SMIPS instruction keeps their src1 in rs
        default:     return tagged Valid i[25:21];
    endcase

endfunction

// mipsGetSrc2

// Given an instruction, return source 2 (rt)
// In SMIPS, only SW, BEQ, BNE, and most SPECIAL ops have src2.

function Maybe#(ISA_REG_INDEX) mipsGetSrc2(ISA_INSTRUCTION i);
    
    MIPS_OPCODE op = i[31:26];

    case (op)
        mipsSW,
        mipsBEQ,
        mipsBNE: return tagged Valid i[20:16];
        mipsSPECIAL:
        begin
        
            MIPS_SPECIAL_OPCODE sp_op = i[5:0];
        
            case (sp_op)
                mipsSpecialSLL,
                mipsSpecialSRL,
                mipsSpecialJALR,
                mipsSpecialJR:   return tagged Invalid;
                default:         return tagged Valid i[20:16];
            endcase
        
        end
        default: return tagged Invalid;
    endcase

endfunction


// isaGetDst

// Given an instruction, return the destination register (rd).
// No SMIPS instruction has more than one dest.


function Maybe#(ISA_REG_INDEX) isaGetDst(ISA_INSTRUCTION i, Integer n);

    case (n)
      0:       return mipsGetDst1(i);
      default: return tagged Invalid;
    endcase

endfunction

// mipsGetDst1

// Get the destination of an SMIPS instruction, if any.
// Non-linking Branches, Stores, and MTC0 have no destinations.
// Immediate-ops keep their dest in rt.
// Other instructions keep it in rd.
// JAL always writes architectural register 31.

function Maybe#(ISA_REG_INDEX) mipsGetDst1(ISA_INSTRUCTION i);

    MIPS_OPCODE op = i[31:26];

    case (op)
        // These have no destinations
        mipsSW,
        mipsJ,
        mipsBEQ,
        mipsBNE,
        mipsBLEZ,
        mipsBGTZ,
        mipsREGIMM: return tagged Invalid;
        // JAL always writes architectural reg 31
        mipsJAL:    return tagged Valid 5'd31;
        // Of the special ops, JR has no destination.
        mipsSPECIAL:
        begin
        
            MIPS_SPECIAL_OPCODE sp_op = i[5:0];
            
            case (sp_op)
                mipsSpecialJR:  return tagged Invalid;
                default: return tagged Valid i[15:11];
            endcase
        
        end
        // Of the COP operations, only MFC0 has a destination.
        mipsCOP:
        begin
        
            MIPS_COP_OPCODE cop_op = i[25:21];
            
            case (cop_op)
                mipsCopMTC0: return tagged Invalid; 
                mipsCopMFC0: return tagged Valid i[20:16];
            endcase
        
        end
        // The rest of SMIPS instructions keep their dest in rt.
        default: return tagged Valid i[20:16];
    endcase


endfunction


// isaGetNumDsts

// Given an instruction, return the number of destination registers (rd).
// Most have one dest. 
// Non-linking Branches, Stores, and MTC0 have no destinations.

function Integer isaGetNumDsts(ISA_INSTRUCTION i);

    MIPS_OPCODE op = i[31:26];

    case (op)
        // These have no destinations
        mipsSW,
        mipsJ,
        mipsBEQ,
        mipsBNE,
        mipsBLEZ,
        mipsBGTZ,
        mipsREGIMM: return 0;
        // Of the special ops, JR has no destination.
        mipsSPECIAL:
        begin
        
            MIPS_SPECIAL_OPCODE sp_op = i[5:0];
            
            case (sp_op)
                mipsSpecialJR:  return 0;
                default: return 1;
            endcase
        
        end
        // Of the COP operations, only MFC0 has a destination.
        mipsCOP:
        begin
        
            MIPS_COP_OPCODE cop_op = i[25:21];
            
            case (cop_op)
                mipsCopMTC0: return 0;
                mipsCopMFC0: return 1;
            endcase
        
        end
        // The rest of SMIPS instructions have one destination.
        default: return 1;

    endcase

endfunction

// isaIsLoad

// Only LW is a load.

function Bool isaIsLoad(ISA_INSTRUCTION i);

    MIPS_OPCODE op = i[31:26];

    return op == mipsLW;

endfunction


// isaIsStore

// Only SW is a store.

function Bool isaIsStore(ISA_INSTRUCTION i);

    MIPS_OPCODE op = i[31:26];

    return op == mipsSW;

endfunction


// isIsBranch

// All branches and jumps should be snapshotted.

function Bool isaIsBranch(ISA_INSTRUCTION i);

    MIPS_OPCODE op = i[31:26];

    case (op)
        // Conditional branches, J, JAL
        mipsJ,
        mipsJAL,
        mipsBEQ,
        mipsBNE,
        mipsBLEZ,
        mipsBGTZ,
        mipsREGIMM: return True;
        // Of the special ops, JR and JALR are jumps.
        mipsSPECIAL:
        begin
        
            MIPS_SPECIAL_OPCODE sp_op = i[5:0];
            
            case (sp_op)
                mipsSpecialJR:   return True;
                mipsSpecialJALR: return True;
                default:         return False;
            endcase
        
        end
        // The rest of SMIPS instructions are not branches.
        default: return False;

    endcase

endfunction


// isaDrainBefore

// No SMIPS instructions require draining.

function Bool isaDrainBefore(ISA_INSTRUCTION i);

    return False;

endfunction


// isaDrainAfter

// No SMIPS instructions require draining.

function Bool isaDrainAfter(ISA_INSTRUCTION i);

    return False;

endfunction


// isaLoadType

// Returns MEMOP_Word.

function ISA_MEMOP_TYPE isaLoadType(ISA_INSTRUCTION i);

    return MEMOP_Word;

endfunction


// isaStoreType

// Returns MEMOP_Word.

function ISA_MEMOP_TYPE isaStoreType(ISA_INSTRUCTION i);

    return MEMOP_Word;

endfunction

// isaEmulateInstruction

// Returns true if the major opcode is unrecognized.

function Bool isaEmulateInstruction(ISA_INSTRUCTION i);

    MIPS_OPCODE op = i[31:26];

    case (op)
        mipsLW, 
        mipsSW,
        mipsADDIU,
        mipsSLTI,
        mipsSLTIU,
        mipsANDI,
        mipsORI,
        mipsXORI,
        mipsLUI,
        mipsJ,
        mipsJAL,
        mipsBEQ,
        mipsBNE,
        mipsBLEZ,
        mipsBGTZ,
        mipsSPECIAL,
        mipsREGIMM,
        mipsCOP: return False;
        default: return True;
    endcase

endfunction
