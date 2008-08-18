// isa_datatypes

// This file contains datatype definitions for the SMIPS ISA.


// ISA_ADDRESS

// A SMIPS address.

typedef Bit#(32) ISA_ADDRESS;


// ISA_VALUE

// The value stored in registers.

typedef Bit#(32) ISA_VALUE;


// ISA_INSTRUCTION

// An SMIPS instruction.

typedef Bit#(32) ISA_INSTRUCTION;


// ISA_MAX_SRCS

// The maximum number of source registers is 2.

typedef 2 ISA_MAX_SRCS;


// ISA_MAX_DSTS

// The maximum number of destination registers is 1.

typedef 1 ISA_MAX_DSTS;


// ISA_MEMOP_TYPE

// SMIPS can only operate on words.

typedef enum
{
  MEMOP_Word,
  MEMOP_Dummy
}
  ISA_MEMOP_TYPE
     deriving (Eq, Bits);


// ISA_REG_INDEX

// SMIPS has no special registers, so we use a Bit#(5) here.

typedef 5 ISA_REG_INDEX_SIZE;
typedef Bit#(ISA_REG_INDEX_SIZE) ISA_REG_INDEX;
typedef 32 ISA_NUM_REGS;

