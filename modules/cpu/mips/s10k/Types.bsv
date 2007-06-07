import hasim_common::*;
import hasim_isa::*;

import hasim_cpu_parameters::*;

typedef Bit#(TLog#(TAdd#(FetchWidth,1))) FetchCount;
typedef Bit#(TLog#(TAdd#(1,TMul#(FetchWidth,2)))) InstCount;
typedef Bit#(TLog#(TAdd#(CommitWidth,1))) CommitCount;

typedef Bit#(TLog#(TAdd#(RobCount,1))) RobTag;

typedef Bit#(TLog#(TAdd#(IntQCount,1))) IntQCountType;
typedef Bit#(TLog#(TAdd#(MemQCount,1))) MemQCountType;

typedef Bit#(TLog#(TAdd#(NumFuncUnits,1))) FuncUnitPos;

typedef enum {J, JAL, JR, JALR, Branch, Shift, Normal, Load, Store} IssueType deriving (Bits, Eq);

typedef Bit#(64) ClockCounter;

typedef struct {
    IssueType issueType;
    Token     token;
    RobTag    robTag;
    Bool      src1Ready;
    PRName    src1;
    Bool      src2Ready;
    PRName    src2;
    PRName    dest;
} IssueEntry deriving (Bits, Eq);

typedef struct {
    Token token;
    PRName pRName;
    RobTag robTag;
} ExecEntry deriving (Bits, Eq);

