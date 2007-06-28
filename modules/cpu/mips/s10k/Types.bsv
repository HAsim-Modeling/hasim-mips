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

typedef Bit#(TLog#(TAdd#(BranchCount,1))) BranchStackIndex;

typedef enum {J, JAL, JR, JALR, Branch, Shift, Normal, Load, Store} IssueType deriving (Bits, Eq);

typedef Bit#(64) ClockCounter;

typedef Bit#(TLog#(TAdd#(FreeListCount,1))) FreeListFreeCount;

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
    FreeListFreeCount freeCount;
    BranchStackIndex branchIndex;
    Bool pred;
    Addr predAddr;
} IssuePort deriving (Bits, Eq);

typedef struct {
    Token     token;
    IssueType issueType;
    RobTag    robTag;
    FreeListFreeCount freeCount;
    Bool      src1Ready;
    PRName    src1;
    Bool      src2Ready;
    PRName    src2;
    PRName    dest;
    BranchStackIndex branchIndex;
    Bool      pred;
    Addr      predAddr;
} IssueEntry deriving (Bits, Eq);

typedef struct {
    Token token;
    IssueType issueType;
    RobTag robTag;
    FreeListFreeCount freeCount;
    PRName pRName;
    BranchStackIndex branchIndex;
    Bool pred;
    Addr predAddr;
} ExecEntry deriving (Bits, Eq);

typedef struct {
    Token token;
    IssueType issueType;
    RobTag robTag;
    FreeListFreeCount freeCount;
    PRName pRName;
    BranchStackIndex branchIndex;
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
    FreeListFreeCount freeCount;
    Addr mispredictPC;
    BranchStackIndex branchIndex;
} KillData deriving (Bits, Eq);
