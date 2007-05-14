import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

typedef 4 FetchWidth;
Addr pcStart = 32'h00001000;

typedef 32 ROBCount;
typedef Bit#(TLog#(TAdd#(ROBCount,1))) ROBTag;

typedef 4 CommitWidth;
typedef Bit#(TLog#(TAdd#(FetchWidth,1))) FetchCount;
typedef Bit#(TLog#(TAdd#(CommitWidth,1))) CommitCount;

typedef 16 IntQCount;
typedef 16 AddrQCount;
typedef 32 FreeListCount;
typedef Bit#(TLog#(TAdd#(IntQCount,1))) IntQCountType;
typedef Bit#(TLog#(TAdd#(AddrQCount,1))) AddrQCountType;

typedef enum {ALU, Load, Store} IssueType deriving (Bits, Eq);

typedef struct {
    IssueType issueType;
    Token     token;
    ROBTag    robTag;
    Bool      src1Ready;
    PRName    src1;
    Bool      src2Ready;
    PRName    src2;
    PRName    dest;
    Bool      alu1;
} IssueEntry deriving (Bits, Eq);

typedef struct {
    Token token;
    PRName pRName;
    ROBTag robTag;
} ExecEntry deriving (Bits, Eq);
