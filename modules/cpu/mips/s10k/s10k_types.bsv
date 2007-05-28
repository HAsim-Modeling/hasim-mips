import hasim_common::*;
import hasim_base::*;
import hasim_isa::*;

import hasim_parameters::*;

typedef Bit#(TLog#(TAdd#(ROBCount,1))) ROBTag;

typedef Bit#(TLog#(TAdd#(FetchWidth,1))) FetchCount;
typedef Bit#(TLog#(TAdd#(1,TMul#(FetchWidth,2)))) InstCount;
typedef Bit#(TLog#(TAdd#(CommitWidth,1))) CommitCount;

typedef Bit#(TLog#(TAdd#(IntQCount,1))) IntQCountType;
typedef Bit#(TLog#(TAdd#(AddrQCount,1))) AddrQCountType;

typedef Bit#(TLog#(TAdd#(NumFuncUnits,1))) FuncUnitPos;

typedef enum {J, JR, Branch, Shift, Normal, Load, Store} IssueType deriving (Bits, Eq);

typedef Bit#(64) ClockCounter;

typedef struct {
    IssueType issueType;
    Token     token;
    ROBTag    robTag;
    Bool      src1Ready;
    PRName    src1;
    Bool      src2Ready;
    PRName    src2;
    PRName    dest;
} IssueEntry deriving (Bits, Eq);

typedef struct {
    Token token;
    PRName pRName;
    ROBTag robTag;
} ExecEntry deriving (Bits, Eq);

