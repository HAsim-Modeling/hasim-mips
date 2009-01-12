
import Vector::*;

`include "asim/provides/funcp_base_types.bsh"

// isa_derived_types

// NOTE: Unchanged from template.

// This file contains type definitions that are derived from types defined elsewhere.
// You probably don't need to change the contents of this file unless you know what
// you are doing.

// Note: Changes to this file generally imply changing the functional and/or timing
// partitions in order to reflect the new changes.


// FUNCP_PHYSICAL_REG_INDEX

// The number of functional partition physical registers available.
// This isn't really an ISA-specific thing, but still lives here for now since
// both the timing partition and functional partition need to know about it.

typedef Bit#(`FUNCP_PHYSICAL_REG_INDEX_BITS) FUNCP_PHYSICAL_REG_INDEX;
typedef TExp#(`FUNCP_PHYSICAL_REG_INDEX_BITS) FUNCP_NUM_PHYSICAL_REGS;


// FUNCP_PHYSICAL_REGS

// The total number of physical regs available.

typedef TExp#(`FUNCP_PHYSICAL_REG_INDEX_BITS) FUNCP_PHYSICAL_REGS;


// ISA_REG_MAPPING

// A mapping of architectural reg -> physical reg.

typedef Tuple2#(ISA_REG_INDEX, FUNCP_PHYSICAL_REG_INDEX) ISA_REG_MAPPING;


// ISA_SOURCE_VALUES

// A Vector of source values. Passed to the isa-datapath for execution.

typedef Vector#(ISA_MAX_SRCS, ISA_VALUE) ISA_SOURCE_VALUES;


// ISA_RESULT_VALUES

// A Vector of (possible) result values. Returned from the isa-datapath for writeback.

typedef Vector#(ISA_MAX_DSTS, Maybe#(ISA_VALUE)) ISA_RESULT_VALUES;


// ISA_INST_SRCS

// A Vector of source registers that the functional partiton reads for a given instruction.

typedef Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) ISA_INST_SRCS;


// ISA_INST_DSTS

// A Vector of destination registers that an instruction writes back after execution.

typedef Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) ISA_INST_DSTS;


// ISA_SRC_INDEX

// A reference to an instruction source. (IE 3 srcs => bit 2)

typedef Bit#(TLog#(ISA_MAX_SRCS)) ISA_SRC_INDEX;


// ISA_SRC_COUNTER

// A counter of sources.  (One bit larger than ISA_SRC_INDEX).

typedef Bit#(TLog#(TAdd#(1, ISA_MAX_SRCS))) ISA_SRC_COUNTER;


// ISA_DST_INDEX

// A reference to an instruction dest. (IE 5 dsts => bit 3)

typedef Bit#(TLog#(ISA_MAX_DSTS)) ISA_DST_INDEX;


// ISA_DST_COUNTER

// A counter of destinations.  (One bit larger than ISA_DST_INDEX).

typedef Bit#(TLog#(TAdd#(1, ISA_MAX_DSTS))) ISA_DST_COUNTER;


// ISA_SRC_MAPPING

// A Vector which says for each instruction source architectural register, which
// physical register it maps to.

typedef Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_MAPPING)) ISA_SRC_MAPPING;


// ISA_DST_MAPPING

// A Vector which says for each instruction destination architecural register, which
// physical register it maps to.

typedef Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_MAPPING)) ISA_DST_MAPPING;


// ISA_DEPENDENCY_INFO

// All the information about an instruction's source and dest mappings.
// Returned by getDependencies().

typedef Tuple2#(ISA_SRC_MAPPING, ISA_DST_MAPPING) ISA_DEPENDENCY_INFO;

