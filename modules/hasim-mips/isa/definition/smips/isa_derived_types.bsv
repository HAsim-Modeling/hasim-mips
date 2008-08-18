
import Vector::*;

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

typedef 7 FUNCP_PHYSICAL_REG_INDEX_SIZE;
typedef Bit#(FUNCP_PHYSICAL_REG_INDEX_SIZE) FUNCP_PHYSICAL_REG_INDEX;
typedef TExp#(7) FUNCP_NUM_PHYSICAL_REGS;


// FUNCP_PHYSICAL_REGS

// The total number of physical regs available.

typedef TExp#(7) FUNCP_PHYSICAL_REGS;


// ISA_REG_MAPPING

// A mapping of architectural reg -> physical reg.

typedef Tuple2#(ISA_REG_INDEX, FUNCP_PHYSICAL_REG_INDEX) ISA_REG_MAPPING;


// ISA_EXECUTION_RESULT

// A struct of possible execution results that the timing model should know about.
// Returned by the getResults() operation, which obtains it from the ISA datapath.

typedef union tagged
{
  ISA_ADDRESS RBranchTaken;    //Branch was taken to this Addr
  ISA_ADDRESS RBranchNotTaken; //Branch was not taken
  ISA_ADDRESS REffectiveAddr;  //Load/Store effective address for DCache
  void        RNop;            //ALU op with no interesting data
  Bool        RTerminate;      //End the run if this instruction commits. Bool is pass/fail.
}
  ISA_EXECUTION_RESULT 
    deriving 
            (Eq, Bits);


// ISA_DATAPATH_REQ

typedef struct
{
    ISA_INSTRUCTION   instruction;
    ISA_ADDRESS       instAddress;
    ISA_SOURCE_VALUES srcValues;
}
    ISA_DATAPATH_REQ
        deriving (Eq, Bits);

function ISA_DATAPATH_REQ initISADatapathReq(ISA_INSTRUCTION i, ISA_ADDRESS pc, ISA_SOURCE_VALUES sr);

    return ISA_DATAPATH_REQ
            {
                instruction: i,
                instAddress: pc,
                srcValues:   sr
            };

endfunction

// ISA_DATAPATH_RSP

typedef struct
{
    ISA_EXECUTION_RESULT  timepResult;  // Result to give to the timing partition.
    ISA_ADDRESS           memAddress;   // Address for Loads/Stores
    Bool                  isStore;      // If it's a store, writebacks[0] should be the store value.
    ISA_RESULT_VALUES     writebacks;   // Values to write back to the registers.
}
    ISA_DATAPATH_RSP
        deriving (Eq, Bits);

function ISA_DATAPATH_RSP initISADatapathRsp(ISA_EXECUTION_RESULT r, ISA_ADDRESS a, Bool isSt, ISA_RESULT_VALUES wr);

    return ISA_DATAPATH_RSP
            {
                timepResult: r,
                memAddress:  a,
                isStore:     isSt,
                writebacks:  wr
            };

endfunction

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


// ISA_DST_INDEX

// A reference to an instruction dest. (IE 5 dsts => bit 3)

typedef Bit#(TLog#(ISA_MAX_DSTS)) ISA_DST_INDEX;


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

