import Vector::*;

`include "memory.bsh"

typedef Addr ISA_ADDRESS;

typedef Value ISA_VALUE;

typedef PackedInst  ISA_INSTRUCTION;

typedef void ISA_LOAD_TYPE;

typedef void ISA_STORE_TYPE;

typedef RName ISA_REG_INDEX;

typedef PRName FUNCP_PHYSICAL_REG_INDEX;
typedef TExp#(7) FUNCP_PHYSICAL_REGS;

typedef 2 ISA_MAX_SRCS;

typedef 1 ISA_MAX_DSTS;

typedef Tuple2#(ISA_REG_INDEX, FUNCP_PHYSICAL_REG_INDEX) ISA_REG_MAPPING;


typedef InstResult ISA_INSTRUCTION_RESULT;
typedef Vector#(ISA_MAX_SRCS, ISA_VALUE) ISA_SOURCE_VALUES;
typedef Vector#(ISA_MAX_DSTS, Maybe#(ISA_VALUE)) ISA_RESULT_VALUES;
typedef Vector#(ISA_MAX_SRCS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) ISA_INST_SRCS;
typedef Vector#(ISA_MAX_DSTS, Maybe#(FUNCP_PHYSICAL_REG_INDEX)) ISA_INST_DSTS;
typedef Bit#(TLog#(ISA_MAX_SRCS)) ISA_SRC_INDEX;
typedef Bit#(TLog#(ISA_MAX_DSTS)) ISA_DST_INDEX;
typedef Vector#(ISA_MAX_SRCS, Maybe#(ISA_REG_MAPPING)) ISA_SRC_MAPPING;
typedef Vector#(ISA_MAX_DSTS, Maybe#(ISA_REG_MAPPING)) ISA_DST_MAPPING;
typedef Tuple2#(ISA_SRC_MAPPING, ISA_DST_MAPPING) ISA_DEPENDENCY_INFO;

function Maybe#(ISA_REG_INDEX) isaGetSrc(ISA_INSTRUCTION i, Integer n);

  case (n)
    0: return getSrc1(bitsToInst(i));
    1: return getSrc2(bitsToInst(i));
    default: return tagged Invalid;
  endcase
 
endfunction

function Maybe#(ISA_REG_INDEX) isaGetDst(ISA_INSTRUCTION i, Integer n);

  case (n)
    0: return getDest(bitsToInst(i));
    default: return tagged Invalid;
  endcase
 
endfunction

function Integer isaGetNumDsts(ISA_INSTRUCTION i);

  case (getDest(bitsToInst(i))) matches
    tagged Invalid:  return 0;
    tagged Valid .d: return 1;
  endcase

endfunction

function Bool isaIsLoad(ISA_INSTRUCTION i) = isLoad(bitsToInst(i));
function Bool isaIsStore(ISA_INSTRUCTION i) = isStore(bitsToInst(i));
function Bool isaIsBranch(ISA_INSTRUCTION i) = isBranch(bitsToInst(i));

function ISA_LOAD_TYPE isaLoadType(ISA_INSTRUCTION i) = ?;
function ISA_STORE_TYPE isaStoreType(ISA_INSTRUCTION i) = ?;

function MEM_ADDRESS isaAddressToMemAddress(ISA_ADDRESS a);

    return unpack(a);

endfunction

function ISA_ADDRESS isaAddressFromMemAddress(MEM_ADDRESS a);

    return unpack(a);

endfunction

function ISA_INSTRUCTION isaInstructionFromMemValue(MEM_VALUE v);

    return unpack(v);

endfunction

function MEM_VALUE isaInstructionToMemValue(ISA_INSTRUCTION i);

   return unpack(i);

endfunction

function ISA_VALUE isaValueFromMemValue(MEM_VALUE v);

    return unpack(v);

endfunction

function MEM_VALUE isaValueToMemValue(ISA_VALUE v);

    return unpack(v);
    
endfunction

