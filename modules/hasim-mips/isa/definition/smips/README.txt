This directory contains a definition of the SMISP ISA, a small instruction set
which is a subset of the MIPS ISA.


Files:

isa_datatypes.bsv           The main datatype definition file.
isa_decode_functions.bsv    Functions for decoding instructions.
isa_memory_conversions.bsv  Functions for converting from isa-specific memory types to types of the memory Virtual Device.
isa_derived_types.bsv       Types which are derived from the other types. Unchanged from template.
