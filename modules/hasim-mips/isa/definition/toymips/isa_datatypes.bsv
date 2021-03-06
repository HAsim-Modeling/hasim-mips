// isa_datatypes

// This file contains datatype definitions for the toymips ISA.


// ISA_ADDRESS

// A toymips address.

typedef Bit#(32) ISA_ADDRESS;


// ISA_VALUE

// The value stored in registers.

typedef Bit#(32) ISA_VALUE;


// ISA_INSTRUCTION

// A toymips instruction.

typedef Bit#(32) ISA_INSTRUCTION;


// ISA_MAX_SRCS

// The maximum number of source registers is 2.

typedef 2 ISA_MAX_SRCS;


// ISA_MAX_DSTS

// The maximum number of destination registers is 1.

typedef 1 ISA_MAX_DSTS;


// ISA_MEMOP_TYPE

// Toymips can only operate on words.

typedef enum
{
  MEMOP_Word
}
  ISA_MEMOP_TYPE
     deriving (Eq, Bits);


// ISA_REG_INDEX

// Toymips has no special registers, so we use a Bit#(5) here.

typedef Bit#(5) ISA_REG_INDEX;
