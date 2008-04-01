
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

// SPECIAL

// ADDU and SUBU are both encoded as special and differentiated by [4:0]
MIPS_OPCODE mipsSPECIAL = 6'b000000;

// BGTZ

// Branch if greater than zero
MIPS_OPCODE mipsBGTZ = 6'b000111;

// COP0

// CP0 ops where the opcode is encoded in the rs field.
// For mips this is is only MTC0, which is terminate.
MIPS_OPCODE mipsCOP0 = 6'b010000;


// If the opcode is SPECIAL than a different field shows the actual
// operation.

typedef Bit#(6) MIPS_SPECIAL_OPCODE;

// ADDU
MIPS_SPECIAL_OPCODE mipsSpecialADDU = 6'b100001;

// SUBU
MIPS_SPECIAL_OPCODE mipsSpecialSUBU = 6'b100011;


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
// All mips instruction have at least one source.
// Only MTC0 keeps that source in a different place.

function Maybe#(ISA_REG_INDEX) mipsGetSrc1(ISA_INSTRUCTION i);
    
    MIPS_OPCODE op = i[31:26];

    case (op)
        mipsCOP0: return tagged Valid i[20:16];
        default: return tagged Valid i[25:21];
    endcase

endfunction

// mipsGetSrc2

// Given an instruction, return source 2 (rt)
// Only LW and MTC0 does not have a second source.

function Maybe#(ISA_REG_INDEX) mipsGetSrc2(ISA_INSTRUCTION i);
    
    MIPS_OPCODE op = i[31:26];

    case (op)
        mipsLW:   return tagged Invalid;
        mipsCOP0: return tagged Invalid;
        default: return tagged Valid i[20:16];
    endcase

endfunction


// isaGetDst

// Given an instruction, return the destination register (rd).
// SW, BGTZ and MTC0 have no destinations.
// LW has its destination in rt.
// Nothing has more than 1 destination.

function Maybe#(ISA_REG_INDEX) isaGetDst(ISA_INSTRUCTION i, Integer n);

    if (n == 0)
    begin

        MIPS_OPCODE op = i[31:26];

        case (op)
            mipsLW:   return tagged Valid i[20:16];
            mipsSW:   return tagged Invalid;
            mipsBGTZ: return tagged Invalid;
            mipsCOP0: return tagged Invalid;
            default:  return tagged Valid i[15:11];
        endcase

    end
    else // No mips instruction has more than 1 destination.
    begin
        return tagged Invalid;
    end

endfunction

// isaGetNumDsts

// Given an instruction, return the number of destination registers (rd).
// SW, BGTZ and MTC0 have no destinations.
// LW has its destination in rt.
// Nothing has more than 1 destination.

function Integer isaGetNumDsts(ISA_INSTRUCTION i);

    MIPS_OPCODE op = i[31:26];

    case (op)
        mipsLW:   return 1;
        mipsSW:   return 0;
        mipsBGTZ: return 0;
        mipsCOP0: return 0;
        default:  return 1;
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

// Only BTGZ is a branch.

function Bool isaIsBranch(ISA_INSTRUCTION i);

    MIPS_OPCODE op = i[31:26];

    return op == mipsBGTZ;

endfunction


// isaDrainBefore

// No instructions require draining.

function Bool isaDrainBefore(ISA_INSTRUCTION i);

    return False;

endfunction


// isaDrainAfter

// No instructions require draining.

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
