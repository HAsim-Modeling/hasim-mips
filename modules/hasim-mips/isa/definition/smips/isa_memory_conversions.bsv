
`include "funcp_memory.bsh"

// Note: Nothing changed in this file.


// isaAlignAddress

// This function takes an arbitrary address and aligns it to the standard
// memory reference size for the ISA.  The result will typically be passed
// to the TLB.

function Tuple2#(ISA_ADDRESS, MEM_OFFSET) isaAlignAddress(ISA_ADDRESS a);

    return tuple2({a[31:3], 3'b0}, a[2:0]);

endfunction


// isaInstructionFromMemValue

// This function takes a value from the memory virtual device and turns
// it into an isa-specific instruction.
// TODO: Support turning multiple memory values into an instruction.


function ISA_INSTRUCTION isaInstructionFromMemValue(MEM_VALUE v, MEM_OFFSET offset);
    return unpack(v);
endfunction


function ISA_INSTRUCTION isaInstructionFromSpanningMemValues(MEM_VALUE v1, MEM_VALUE v2, MEM_OFFSET offset);

    case (offset)
        3'b101:  return {v2[7:0],  v1[31:8]};
        3'b110:  return {v2[15:0], v1[31:16]};
        3'b111:  return {v2[23:0], v1[31:24]};
        default: return isaInstructionFromMemValue(v1, offset);
    endcase

endfunction


// isaStoreRequiresReadModifyWrite

// This function returns True if the MEMOP_TYPE (which you have defined) requires
// a read-modify-write to implement. An example of this would be updating a single
// byte in an existing word. The result of this function will determine which of
// the following two functions are called.

function Bool isaStoreRequiresReadModifyWrite(ISA_MEMOP_TYPE memtype);

    return False;

endfunction

// isaStoreValueToMemValue

// This function takes an ISA-specific value and turns it into a value
// that the memory state understands. You are given the memtype and the
// original address, so all byte selection and extension can be performed here.

// This function is called ONLY if the above function returns False.

function MEM_VALUE isaStoreValueToMemValue(ISA_VALUE v, ISA_MEMOP_TYPE memtype);

    return zeroExtend(v);

endfunction

// isaStoreValueToMemValueRMW

// This function takes an ISA-specific value and an existing memory value. 
// The function should update the existing memory value appropriately for writeback.

// This function is called ONLY if the above function returns True.

function MEM_VALUE isaStoreValueToMemValueRMW(MEM_VALUE e, ISA_VALUE v, MEM_OFFSET offset, ISA_MEMOP_TYPE memtype);

    return e; // Never called.

endfunction

function Tuple2#(MEM_VALUE, MEM_VALUE) isaStoreValueToSpanningMemValues(MEM_VALUE existing_val1, MEM_VALUE existing_val2, MEM_OFFSET offset, ISA_VALUE store_val, ISA_MEMOP_TYPE st_type);

    case (offset)
        3'b101:  return tuple2({store_val[31:8],  existing_val1[7:0]},  {existing_val2[31:8],  store_val[7:0]});
        3'b110:  return tuple2({store_val[31:16], existing_val1[15:0]}, {existing_val2[31:16], store_val[15:0]});
        3'b111:  return tuple2({store_val[31:24], existing_val1[23:0]}, {existing_val2[31:24], store_val[23:0]});
        default: return tuple2(isaStoreValueToMemValue(existing_val1, st_type), existing_val2);
    endcase

endfunction

function ISA_VALUE isaLoadValueFromMemValue(MEM_VALUE val, ISA_MEMOP_TYPE memtype, MEM_OFFSET offset);

    return zeroExtend(val);
endfunction


function ISA_VALUE isaLoadValueFromSpanningMemValues(MEM_VALUE v1, MEM_VALUE v2, MEM_OFFSET offset, ISA_MEMOP_TYPE memtype);
 
    case (offset)
        3'b101:  return {v2[7:0],  v1[31:8]};
        3'b110:  return {v2[15:0], v1[31:16]};
        3'b111:  return {v2[23:0], v1[31:24]};
        default: return isaLoadValueFromMemValue(v1, memtype, offset);
    endcase

endfunction

function Bool isaFetchSpansTwoMemValues(ISA_ADDRESS vaddr);

    return False;

endfunction
    
function Bool isaMemOpSpansTwoMemValues(ISA_ADDRESS vaddr, ISA_MEMOP_TYPE op_type);
    
    match {.addr, .offset} = isaAlignAddress(vaddr);
    
    case (offset)
        3'b101:  return True;     // Words 5-7 span.
        3'b110:  return True;
        3'b111:  return True;
        default: return False;
    endcase

endfunction

