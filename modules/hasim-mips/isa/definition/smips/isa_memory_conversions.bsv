
`include "funcp_memory.bsh"

// Note: Nothing changed in this file.


// isaAlignAddress

// This function takes an arbitrary address and aligns it to the standard
// memory reference size for the ISA.  The result will typically be passed
// to the TLB.

function ISA_ADDRESS isaAlignAddress(ISA_ADDRESS a);

    return unpack(a); // If you need more than this write it here.

endfunction


// isaAddressFromMemoryAddress

// This function takes an address from the memory virtual device and turns
// it into an isa-specific address.

function ISA_ADDRESS isaAddressFromMemAddress(MEM_ADDRESS a);

    return unpack(a); // If you need more than this write it here.

endfunction


// isaInstructionFromMemValue

// This function takes a value from the memory virtual device and turns
// it into an isa-specific instruction.
// TODO: Support turning multiple memory values into an instruction.

function ISA_INSTRUCTION isaInstructionFromMemValue(MEM_VALUE v, ISA_ADDRESS a);

    return unpack(v); // If you need more than this write it here.

endfunction


// isaInstructionToMemValue

// This function takes an isa-specific instruction and turns it into a memory value.
// TODO: Support turning an instruction into more than one memory value.

function MEM_VALUE isaInstructionToMemValue(ISA_INSTRUCTION i);

   return unpack(i); // If you need more than this write it here.

endfunction


// isaValueFromMemValue

// This function takes a value from the memory device and turns it into an
// isa-specific value. You are given the memtype and the
// original address, so all byte selection and extension can be performed here.


function ISA_VALUE isaValueFromMemValue(MEM_VALUE v, ISA_MEMOP_TYPE memtype, ISA_ADDRESS addr);

    return unpack(v); // If you need more than this write it here.

endfunction

// isaMemOpRequiresReadModifyWrite

// This function returns True if the MEMOP_TYPE (which you have defined) requires
// a read-modify-write to implement. An example of this would be updating a single
// byte in an existing word. The result of this function will determine which of
// the following two functions are called.

function Bool isaMemOpRequiresReadModifyWrite(ISA_MEMOP_TYPE memtype);

    return False; // No ToyMIPS instruction requires read-modify-write.

endfunction

// isaValueToMemValue

// This function takes an ISA-specific value and turns it into a value
// that the memory state understands. You are given the memtype and the
// original address, so all byte selection and extension can be performed here.

// This function is called ONLY if the above function returns False.

function MEM_VALUE isaValueToMemValue(ISA_VALUE v, ISA_MEMOP_TYPE memtype, ISA_ADDRESS addr);
 
    return unpack(v); // If you need more than this write it here.
    
endfunction

// isaValueToMemValueRMW

// This function takes an ISA-specific value and an existing memory value. 
// The function should update the existing memory value appropriately for writeback.

// This function is called ONLY if the above function returns True.

function MEM_VALUE isaValueToMemValueRMW(ISA_VALUE v, ISA_MEMOP_TYPE memtype, ISA_ADDRESS addr, MEM_VALUE existing_value);
 
    return existing_value; // If you need more than this write it here.
    
endfunction
