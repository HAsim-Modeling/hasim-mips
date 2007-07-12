import hasim_common::*;
import hasim_isa::*;

import hasim_cpu_parameters::*;

typedef Bit#(TLog#(TAdd#(FetchWidth,1))) FetchCount;
typedef Bit#(TLog#(TAdd#(CommitWidth,1))) CommitCount;
typedef Bit#(TLog#(TAdd#(RobNum,1))) RobCount;
typedef Bit#(TLog#(TAdd#(IntQNum,1))) IntQCount;
typedef Bit#(TLog#(TAdd#(MemQNum,1))) MemQCount;
typedef Bit#(TLog#(TAdd#(FreeListNum,1))) FreeListCount;
typedef Bit#(TLog#(TAdd#(BranchNum,1))) BranchCount;
typedef Bit#(TLog#(TAdd#(KillNum,1))) KillCount;

typedef Bit#(TLog#(TAdd#(1,TMul#(FetchWidth,2)))) InstCount;

typedef Bit#(TLog#(TAdd#(RobNum,1))) RobTag;

typedef Bit#(TLog#(TAdd#(FuncUnitNum,1))) FuncUnitCount;

typedef enum {J, JAL, JR, JALR, Branch, Shift, Normal, Load, Store} IssueType deriving (Bits, Eq);

typedef Bit#(64) ClockCounter;

typedef struct {
    Token token;
    IssueType issueType;
    Addr addr;
    Bool done;
    Bool pred;
    Bool taken;
    Bool finished;
    Bool status;
} RobEntry deriving (Bits, Eq);

typedef struct {
    Token token;
    IssueType issueType;
    RobTag robTag;
    BranchCount branchIndex;
    Bool pred;
    Addr predAddr;
} IssuePort deriving (Bits, Eq);

typedef struct {
    Token     token;
    IssueType issueType;
    RobTag    robTag;
    Bool      src1Ready;
    PRName    src1;
    Bool      src2Ready;
    PRName    src2;
    PRName    dest;
    BranchCount branchIndex;
    Bool      pred;
    Addr      predAddr;
} IssueEntry deriving (Bits, Eq);

typedef struct {
    Token token;
    IssueType issueType;
    RobTag robTag;
    PRName pRName;
    BranchCount branchIndex;
    Bool pred;
    Addr predAddr;
} ExecEntry deriving (Bits, Eq);

typedef struct {
    Token token;
    IssueType issueType;
    RobTag robTag;
    PRName pRName;
    BranchCount branchIndex;
    Bool pred;
    Addr predAddr;
    Bool taken;
    Addr takenAddr;
    Bool finished;
    Bool status;
} ExecResult deriving (Bits, Eq);

typedef struct {
    Token token;
    Addr addr;
    PackedInst inst;
} InstInfo deriving (Bits, Eq);

typedef struct {
    Token token;
    RobTag robTag;
    Addr mispredictPC;
    BranchCount branchIndex;
} KillData deriving (Bits, Eq);
