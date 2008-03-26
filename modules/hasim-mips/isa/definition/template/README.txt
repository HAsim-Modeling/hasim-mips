This directory contains everything needed to support an ISA in the hasim
functional partition.

Copy the template files and fill in the datatype and function definitions.

You will also need to define a datapath capable of executing your instructions.
See the isa/datapath/template directory for a template.


Files:

isa_datatypes.bsv           The main datatype definition file.
isa_decode_functions.bsv    Functions for decoding instructions.
isa_memory_conversions.bsv  Functions for converting from isa-specific memory types to types of the memory Virtual Device.
isa_derived_types.bsv       Types which are derived from the other types. You probably don't have to change these.
