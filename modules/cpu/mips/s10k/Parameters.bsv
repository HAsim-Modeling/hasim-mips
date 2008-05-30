import hasim_isa::*;

`include "asim/provides/funcp_simulated_memory.bsh"

ISA_ADDRESS pcStart = `PROGRAM_START_ADDR;

typedef 8  TokenSize;
typedef 4  FetchWidth;
typedef 4  CommitWidth;
typedef 32 RobNum;
typedef 16 IntQNum;
typedef 16 MemQNum;
typedef 32 FreeListNum;
typedef 4  BranchNum;
typedef 5  FuncUnitNum;
typedef 2  KillNum;
typedef 128 PRNum;
