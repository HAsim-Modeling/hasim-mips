This directory contains a definition of the toymips ISA, a small ISA with 6 instructions: 

Load
Store
Add
Subtract
Branch if greater than zero
Terminate (Move to CP0)

Files:

isa_datatypes.bsv           The main datatype definition file.
isa_decode_functions.bsv    Functions for decoding instructions.
isa_memory_conversions.bsv  Functions for converting from isa-specific memory types to types of the memory Virtual Device.
isa_derived_types.bsv       Types which are derived from the other types. Unchanged from template.
