
// isa_decode_functions

// This file contains functions which decode an architectural instruction for both the
// functional and timing partition.

// TODO: Support decoding variable-width instructions.


// isaGetSrc

// Given an instruction, return the nth source register.
// Or return Invalid if there is no such source for this instruction.

function Maybe#(ISA_REG_INDEX) isaGetSrc(ISA_INSTRUCTION i, Integer n);

    return tagged Invalid; // You should write this.
    
endfunction


// isaGetDst

// Given an instruction, return the nth destination register.
// Or return Invalid if there is no such destination for this instruction.

function Maybe#(ISA_REG_INDEX) isaGetDst(ISA_INSTRUCTION i, Integer n);

  return tagged Invalid; // You should write this.
 
endfunction

// isaGetNumDsts

// Given an instruction, return how many destinations it has.

function Integer isaGetNumDsts(ISA_INSTRUCTION i);

  return tagged Invalid; // You should write this.
 
endfunction


// isaIsLoad

// Returns true if the given instruction is a load.

function Bool isaIsLoad(ISA_INSTRUCTION i);

    return False; // You should write this.

endfunction


// isaIsStore

// Returns true if the given instruction is a store.

function Bool isaIsStore(ISA_INSTRUCTION i);

    return False; // You should write this.

endfunction

// isaLoadType

// Returns the ISA_LOAD_TYPE (which you defined in isa_datatypes.bsv) of a given instruction.
// This will only be called on instructions where isaIsLoad() returns True.

function ISA_MEMOP_TYPE isaLoadType(ISA_INSTRUCTION i);

    return ?; // You should write this.

endfunction


// isaStoreType

// Returns the ISA_STORE_TYPE (which you defined in isa_datatypes.bsv) of a given instruction.
// This will only be called on instructions where isaIsStore() returns True.

function ISA_MEMOP_TYPE isaStoreType(ISA_INSTRUCTION i);

    return ?; // You should write this.

endfunction


// isaIsBranch

// Returns true if the given instruction is a branch.

function Bool isaIsBranch(ISA_INSTRUCTION i);

    return False; // You should write this.

endfunction


// isaDrainBefore

// Returns true if the timing model should drain the pipeline before executing this
// instuction.

// Note that both isaDrainBefore() and isaDrainAfter() may be true for a given instruction.

function Bool isaDrainBefore(ISA_INSTRUCTION i);

    return False; // You should write this.

endfunction


// isaDrainAfter

// Returns true if the timing model should drain the pipeline after executing this
// instruction.

// Note that both isaDrainBefore() and isaDrainAfter() may be true for a given instruction.

function Bool isaDrainAfter(ISA_INSTRUCTION i);

    return False; // You should write this.

endfunction


// isaEmulateInstruction

// Returns true if the given instruction should be emulated in software.

function Bool isaEmulateInstruction(ISA_INSTRUCTION i);

    return False; // You should write this.

endfunction

