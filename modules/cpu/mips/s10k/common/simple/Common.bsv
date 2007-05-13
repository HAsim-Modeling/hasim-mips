import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

typedef 4 NumInst;
typedef Bit#(TLog#(TAdd#(NumInst,1))) Count;
Addr pcStart = 0;

typedef Bit#(TLog#(ROBCount)) ROBTag;
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


