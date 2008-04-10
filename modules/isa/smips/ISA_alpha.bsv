///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// ISA.bsv                                                                   //
//                                                                           //
// Top-level datatypes for the SALPHA example ISA.                            //
//                                                                           //
// This file will be included by most SALPHA-specific modules.                //
//                                                                           //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

import FIFO::*;

import hasim_common::*;


/************* Basic Datatypes *************/

typedef Bit#(32) Tick;
typedef Bit#(32) Addr;
typedef Bit#(32) PackedInst;
typedef Bit#(5)  RName;
typedef Bit#(7)  PRName;
typedef Bit#(32) Value;
typedef Bit#(4)  SnapshotPtr;
typedef Bit#(16) SImm;
typedef Bit#(16) ZImm;
typedef Bit#(6)  ShAmt;
typedef Bit#(26) Target;
typedef Bit#(5)  CP0Index;
typedef Bit#(12) SoftAddr;

//For convenience

RName r0 = 0;
RName r1 = 1;
RName r2 = 2; 
RName r3 = 3;
RName r4 = 4;
RName r5 = 5;
RName r6 = 6;
RName r7 = 7;
RName r8 = 8;
RName r9 = 9;



/************* Functional Partition Datatypes *************/


//Unpacked ISA representation

typedef Bit#(16) MemDisp;
typedef Bit#(8) Imm;
typedef Bit#(21) BranchDisp;
typedef union tagged                
{

  struct { RName rbase; RName rdest; MemDisp offset;} LW;
  struct { RName rbase; RName rsrc;  MemDisp offset;} SW; 

  struct { RName rsrc;  RName rdest;  Imm imm;      } ADDIU;
  struct { RName rsrc;  RName rdest;  Imm imm;      } SLTI;
  struct { RName rsrc;  RName rdest;  Imm imm;      } SLTIU;
  struct { RName rsrc;  RName rdest;  Imm imm;      } ANDI;
  struct { RName rsrc;  RName rdest;  Imm imm;      } ORI;
  struct { RName rsrc;  RName rdest;  Imm imm;      } XORI;
  struct {              RName rdest;  Imm imm;      } LUI;

  struct { RName rsrc;  RName rdest;  ShAmt shamt;  } SLL;
  struct { RName rsrc;  RName rdest;  ShAmt shamt;  } SRL;
  struct { RName rsrc;  RName rdest;  ShAmt shamt;  } SRA;
  struct { RName rsrc;  RName rdest;  RName rshamt; } SLLV;
  struct { RName rsrc;  RName rdest;  RName rshamt; } SRLV;
  struct { RName rsrc;  RName rdest;  RName rshamt; } SRAV;
  struct { RName rsrc1; RName rsrc2;  RName rdest;  } ADDU;
  struct { RName rsrc1; RName rsrc2;  RName rdest;  } SUBU;
  struct { RName rsrc1; RName rsrc2;  RName rdest;  } AND;
  struct { RName rsrc1; RName rsrc2;  RName rdest;  } OR;
  struct { RName rsrc1; RName rsrc2;  RName rdest;  } XOR;
  struct { RName rsrc1; RName rsrc2;  RName rdest;  } NOR;
  struct { RName rsrc1; RName rsrc2;  RName rdest;  } SLT;
  struct { RName rsrc1; RName rsrc2;  RName rdest;  } SLTU;

  struct { Target target;                           } J;
  struct { Target target;                           } JAL;
  struct { RName rsrc;                              } JR;
  struct { RName rsrc;  RName rdest;                } JALR;
  struct { RName rsrc1; RName rsrc2; SImm offset;   } BEQ;
  struct { RName rsrc1; RName rsrc2; SImm offset;   } BNE;
  struct { RName rsrc;  BranchDisp offset;          } BLEZ;
  struct { RName rsrc;  BranchDisp offset;          } BGTZ;
  struct { RName rsrc;  BranchDisp offset;          } BLTZ;
  struct { RName rsrc;  BranchDisp offset;          } BGEZ;

  struct { RName rdest; CP0Index cop0src;           } MFC0;
  struct { RName rsrc;  CP0Index cop0dest;          } MTC0;

  void                                                TERMINATE;
  void                                                ILLEGAL;

}
  Inst
    deriving (Eq, Bits);

Bit#(6) opFUNC  = 6'b000000;  Bit#(6) fcSLL   = 6'b000000;
Bit#(6) opRT    = 6'b000001;  Bit#(6) fcSRL   = 6'b000010;
Bit#(6) opRS    = 6'b010000;  Bit#(6) fcSRA   = 6'b000011;
                              Bit#(6) fcSLLV  = 6'b000100;
Bit#(6) opLW    = 6'b100011;  Bit#(6) fcSRLV  = 6'b000110;
Bit#(6) opSW    = 6'b101011;  Bit#(6) fcSRAV  = 6'b000111;
                              Bit#(6) fcADDU  = 6'b100001;
Bit#(6) opADDIU = 6'b001001;  Bit#(6) fcSUBU  = 6'b100011;
Bit#(6) opSLTI  = 6'b001010;  Bit#(6) fcAND   = 6'b100100;
Bit#(6) opSLTIU = 6'b001011;  Bit#(6) fcOR    = 6'b100101;
Bit#(6) opANDI  = 6'b001100;  Bit#(6) fcXOR   = 6'b100110;
Bit#(6) opORI   = 6'b001101;  Bit#(6) fcNOR   = 6'b100111;
Bit#(6) opXORI  = 6'b001110;  Bit#(6) fcSLT   = 6'b101010;
Bit#(6) opLUI   = 6'b001111;  Bit#(6) fcSLTU  = 6'b101011;

Bit#(6) opJ     = 6'b000010;
Bit#(6) opJAL   = 6'b000011;
Bit#(6) fcJR    = 6'b001000;
Bit#(6) fcJALR  = 6'b001001;
Bit#(6) opBEQ   = 6'b000100;
Bit#(6) opBNE   = 6'b000101;
Bit#(6) opBLEZ  = 6'b000110;
Bit#(6) opBGTZ  = 6'b000111;
Bit#(5) rtBLTZ  = 5'b00000;
Bit#(5) rtBGEZ  = 5'b00001;

Bit#(5) rsMFC0  = 5'b00000;
Bit#(5) rsMTC0  = 5'b00100;

Bit#(5) rsTERMINATE = 5'b11111;

  // Inst to Bit#(32) Function

function Bit#(32) instToBits(Inst instr);

    return 0;
endfunction

  // Bit#(32) to Inst Function

function Inst bitsToInst( Bit#(32) inst );

    let     opcode = inst[31:26];
    let       src1 = inst[25:21];
    let       src2 = inst[20:16];
    let        imm = inst[20:13];
    Bool    useImm = unpack(inst[12]);
    let      funct = inst[11:5];
    let       dest = inst[4:0];
    let branchDisp = inst[20:0];
    let    memDisp = inst[15:0];

    case (opcode)
        'h29: return tagged LW {rbase: src2, rdest: src1, offset: memDisp};
        'h2d: return tagged SW {rbase: src2, rsrc: src1, offset: memDisp};
        'h10:
            case (funct)
                'h20: return (useImm? tagged ADDIU{rsrc: src1, rdest: dest, imm: imm}: tagged ADDU {rsrc1: src1, rsrc2: src2, rdest: dest});
                'h29: return (!useImm? tagged ADDU {rsrc1: src1, rsrc2: src2, rdest: dest}: tagged ILLEGAL);
                'h4d: return (useImm? tagged SLTI{rsrc: src1, rdest: dest, imm: imm}: tagged SLT {rsrc1: src1, rsrc2: src2, rdest: dest});
                'h1d: return (useImm? tagged SLTIU{rsrc: src1, rdest: dest, imm: imm}: tagged SLTU {rsrc1: src1, rsrc2: src2, rdest: dest});
            endcase
        'h11:
            case (funct)
                'h00: return (useImm? tagged ANDI{rsrc: src1, rdest: dest, imm: imm}: tagged AND {rsrc1: src1, rsrc2: src2, rdest: dest});
                'h20: return (useImm? tagged ORI{rsrc: src1, rdest: dest, imm: imm}: tagged OR {rsrc1: src1, rsrc2: src2, rdest: dest});
                'h40: return (useImm? tagged XORI{rsrc: src1, rdest: dest, imm: imm}: tagged XOR {rsrc1: src1, rsrc2: src2, rdest: dest});
                'h28: return (!useImm? tagged NOR{rsrc1: src1, rsrc2: src2, rdest: dest}: tagged ILLEGAL);
            endcase
        'h12:
            case (funct)
                'h39: return useImm? tagged SLL{rsrc: src1, rdest: dest, shamt: truncate(imm)}: tagged SLLV {rsrc: src1, rshamt: src2, rdest: dest};
                'h34: return useImm? tagged SRL{rsrc: src1, rdest: dest, shamt: truncate(imm)}: tagged SRLV {rsrc: src1, rshamt: src2, rdest: dest};
                'h3c: return useImm? tagged SRA{rsrc: src1, rdest: dest, shamt: truncate(imm)}: tagged SRAV {rsrc: src1, rshamt: src2, rdest: dest};
            endcase
        'h3b: return tagged BLEZ {rsrc: src1, offset: branchDisp};
        'h3f: return tagged BGTZ {rsrc: src1, offset: branchDisp};
        'h3a: return tagged BLTZ {rsrc: src1, offset: branchDisp};
        'h3e: return tagged BGEZ {rsrc: src1, offset: branchDisp};
        'h1a: return tagged JALR {rsrc: src1, rdest: dest};
        default : return tagged ILLEGAL;
    endcase
endfunction


//Decoded Instruction
typedef union tagged                
{

  struct { PRName pbase; PRName pdest;  SImm offset;  } DLW;
  struct { PRName pbase; PRName psrc;  SImm offset;   } DSW; 

  struct { PRName psrc;  PRName pdest; SImm imm;      } DADDIU;
  struct { PRName psrc;  PRName pdest; SImm imm;      } DSLTI;
  struct { PRName psrc;  PRName pdest; SImm imm;      } DSLTIU;
  struct { PRName psrc;  PRName pdest; ZImm imm;      } DANDI;
  struct { PRName psrc;  PRName pdest; ZImm imm;      } DORI;
  struct { PRName psrc;  PRName pdest; ZImm imm;      } DXORI;
  struct {               PRName pdest; ZImm imm;      } DLUI;

  struct { PRName psrc;  PRName pdest; ShAmt shamt;   } DSLL;
  struct { PRName psrc;  PRName pdest; ShAmt shamt;   } DSRL;
  struct { PRName psrc;  PRName pdest; ShAmt shamt;   } DSRA;
  struct { PRName psrc;  PRName pdest; PRName pshamt; } DSLLV;
  struct { PRName psrc;  PRName pdest; PRName pshamt; } DSRLV;
  struct { PRName psrc;  PRName pdest; PRName pshamt; } DSRAV;
  struct { PRName psrc1; PRName psrc2; PRName pdest;  } DADDU;
  struct { PRName psrc1; PRName psrc2; PRName pdest;  } DSUBU;
  struct { PRName psrc1; PRName psrc2; PRName pdest;  } DAND;
  struct { PRName psrc1; PRName psrc2; PRName pdest;  } DOR;
  struct { PRName psrc1; PRName psrc2; PRName pdest;  } DXOR;
  struct { PRName psrc1; PRName psrc2; PRName pdest;  } DNOR;
  struct { PRName psrc1; PRName psrc2; PRName pdest;  } DSLT;
  struct { PRName psrc1; PRName psrc2; PRName pdest;  } DSLTU;

  struct { Target target;                             } DJ;
  struct { PRName pdest;  Target target;              } DJAL;
  struct { PRName psrc;                               } DJR;
  struct { PRName psrc;  PRName pdest;                } DJALR;
  struct { PRName psrc1; PRName psrc2; SImm offset;   } DBEQ;
  struct { PRName psrc1; PRName psrc2; SImm offset;   } DBNE;
  struct { PRName psrc;  SImm offset;                 } DBLEZ;
  struct { PRName psrc;  SImm offset;                 } DBGTZ;
  struct { PRName psrc;  SImm offset;                 } DBLTZ;
  struct { PRName psrc;  SImm offset;                 } DBGEZ;

  struct { PRName pdest;  CP0Index cop0src;           } DMFC0;
  struct { PRName psrc;   CP0Index cop0dest;          } DMTC0; 

  void                                                  DTERMINATE;
  void                                                  DILLEGAL;

}
  DecodedInst
    deriving 
            (Eq, Bits);

//Executed Instruction

//Possibly should include branch info if Functional Partition has branch predictor

//Exec-->Mem
typedef union tagged 
{
  struct {PRName pdest;                      } EWB;
  struct {Value addr; PRName pdest;          } ELoad;
  struct {Value addr; Value val;             } EStore;
  void                                         ENop;
}
  ExecedInst
     deriving 
             (Eq, Bits);

//Mem-->LCO-->GCO
typedef enum
{
  WWB,
  WStore,
  WNop
}
  InstWBInfo
     deriving 
             (Eq, Bits);


/************* Timing Partition Datatypes *************/


//Dependency info for Timing Partition

//FP Decode-->TP
typedef struct 
{
  Maybe#(Tuple2#(RName, PRName)) dep_dest;
  Maybe#(Tuple2#(RName, PRName)) dep_src1; 
  Maybe#(Tuple2#(RName, PRName)) dep_src2;
}
  DepInfo 
    deriving
            (Eq, Bits);

//FP Decode-->TP
typedef struct 
{
  Maybe#(TokIndex) src1; 
  Maybe#(TokIndex) src2;
}
  TokDep
    deriving
            (Eq, Bits);

//Result of executing an instruction

//FP Exec-->TP
typedef union tagged
{
  Addr     RBranchTaken;    //Branch was taken to this Addr
  Addr     RBranchNotTaken; //Branch was not taken
  Addr     REffectiveAddr;  //Load/Store effective address for DCache
  void     RNop;            //ALU op with no interesting data
  Bool     RTerminate;      //End the run if this instruction commits. Bool is pass/fail.
}
  InstResult 
    deriving 
            (Eq, Bits);

//Timing model helper functions
function Bool p_isShift(PackedInst inst);
    let op   = inst[31:26];
    let func = inst[5:0];
    return (inst != 32'b0) && (op == opFUNC) && (func == fcSLL || func == fcSRL || func == fcSRA || func == fcSLLV || func == fcSRLV || func == fcSRAV);
endfunction

function Bool p_isMtc0(PackedInst inst);
    let op   = inst[31:26];
    let rs   = inst[25:21];
    return op == opRS && rs == rsMTC0;
endfunction

function Bool p_isBranch(PackedInst inst);
    let op = inst[31:26];
    let rt = inst[20:16];
    return (op == opBEQ || op == opBNE || op == opBLEZ || op == opBGTZ) || (op == opRT && (rt == rtBLTZ || rt == rtBGEZ));
endfunction

function Bool p_isJ(PackedInst inst);
    let op = inst[31:26];
    return op == opJ;
endfunction

function Bool p_isJAL(PackedInst inst);
    let op = inst[31:26];
    return op == opJAL;
endfunction

function Bool p_isJR(PackedInst inst);
    let op   = inst[31:26];
    let func = inst[5:0];
    return op == opFUNC && func == fcJR;
endfunction

function Bool p_isJALR(PackedInst inst);
    let op   = inst[31:26];
    let func = inst[5:0];
    return op == opFUNC && func == fcJALR;
endfunction

function Bool p_isLoad(PackedInst inst);
    let op = inst[31:26];
    return op == opLW;  
endfunction

function Bool p_isStore(PackedInst inst);
    let op = inst[31:26];
    return op == opSW;  
endfunction

function Bool p_isALU(PackedInst inst);
    let op = inst[31:26];
    let rt = inst[20:16];
    let rd = inst[15:11];
    return !(inst == 32'b0) && !(p_isBranch(inst) || p_isJ(inst) || p_isJAL(inst) || p_isJR(inst) || p_isJALR(inst) || p_isLoad(inst) || p_isStore(inst))
         && !(op == 0 && rd == 0 || op != 0 && rt == 0);
endfunction

function Addr p_getJAddr(PackedInst inst, Addr pc);
    let addr = inst[25:0];
    let pcPlus4 = pc + 4;
    return {pcPlus4[31:28], {addr, 2'b0}};
endfunction

function Addr p_getJALAddr(PackedInst inst, Addr pc);
    return p_getJAddr(inst, pc);
endfunction

//Helper functions


function Maybe#(RName) getOp1(Inst i);

  return case ( i ) matches

    // -- Memory Ops ------------------------------------------------      

    tagged LW .it : return tagged Valid it.rbase;
    tagged SW .it : return tagged Valid it.rbase;

    // -- Simple Ops ------------------------------------------------      

    tagged ADDIU .it : return tagged Valid it.rsrc;
    tagged SLTI  .it : return tagged Valid it.rsrc;
    tagged SLTIU .it : return tagged Valid it.rsrc;
    tagged ANDI  .it : return tagged Valid it.rsrc;
    tagged ORI   .it : return tagged Valid it.rsrc;
    tagged XORI  .it : return tagged Valid it.rsrc;
    tagged LUI   .it : return tagged Invalid;

    tagged SLL   .it : return tagged Valid it.rsrc;
    tagged SRL   .it : return tagged Valid it.rsrc;
    tagged SRA   .it : return tagged Valid it.rsrc;
    tagged SLLV  .it : return tagged Valid it.rsrc;
    tagged SRLV  .it : return tagged Valid it.rsrc;
    tagged SRAV  .it : return tagged Valid it.rsrc;
    tagged ADDU  .it : return tagged Valid it.rsrc1;
    tagged SUBU  .it : return tagged Valid it.rsrc1;
    tagged AND   .it : return tagged Valid it.rsrc1;
    tagged OR    .it : return tagged Valid it.rsrc1;
    tagged XOR   .it : return tagged Valid it.rsrc1;
    tagged NOR   .it : return tagged Valid it.rsrc1;
    tagged SLT   .it : return tagged Valid it.rsrc1;
    tagged SLTU  .it : return tagged Valid it.rsrc1;

    tagged MTC0  .it : return tagged Valid it.rsrc;
    tagged MFC0  .it : return tagged Invalid;

    // -- Branches --------------------------------------------------

    tagged BLEZ  .it : return tagged Valid it.rsrc;
    tagged BGTZ  .it : return tagged Valid it.rsrc;
    tagged BLTZ  .it : return tagged Valid it.rsrc;
    tagged BGEZ  .it : return tagged Valid it.rsrc;
    tagged BEQ   .it : return tagged Valid it.rsrc1;
    tagged BNE   .it : return tagged Valid it.rsrc1;

    // -- Jumps -----------------------------------------------------

    tagged J     .it : return tagged Invalid;
    tagged JR    .it : return tagged Valid it.rsrc;
    tagged JAL   .it : return tagged Invalid;
    tagged JALR  .it : return tagged Valid it.rsrc;

    default:           return tagged Invalid;
  endcase;

endfunction
  
function Maybe#(RName) getSrc1(Inst i);

  case (getOp1(i)) matches
    tagged Invalid: return tagged Invalid;
    tagged Valid .r: return (r == 0) ? tagged Invalid : tagged Valid r;
  endcase 
  
endfunction

function Maybe#(RName) getOp2(Inst i);

  return case ( i ) matches

    tagged SW    .it : return tagged Valid it.rsrc;
    tagged ADDU  .it : return tagged Valid it.rsrc2;
    tagged SUBU  .it : return tagged Valid it.rsrc2;
    tagged AND   .it : return tagged Valid it.rsrc2;
    tagged OR    .it : return tagged Valid it.rsrc2;
    tagged XOR   .it : return tagged Valid it.rsrc2;
    tagged NOR   .it : return tagged Valid it.rsrc2;
    tagged SLT   .it : return tagged Valid it.rsrc2;
    tagged SLTU  .it : return tagged Valid it.rsrc2;
    tagged SLLV  .it : return tagged Valid it.rshamt;
    tagged SRLV  .it : return tagged Valid it.rshamt;
    tagged SRAV  .it : return tagged Valid it.rshamt;

    tagged BEQ   .it : return tagged Valid it.rsrc2;

    tagged BNE   .it : return tagged Valid it.rsrc2;
    default:           return tagged Invalid;
  endcase;

endfunction

function Maybe#(RName) getSrc2(Inst i);

  case (getOp2(i)) matches
    tagged Invalid:  return tagged Invalid;
    tagged Valid .r: return (r == 0) ? tagged Invalid : tagged Valid r;
  endcase
  
endfunction

function Bool src1IsR0(Inst i);

  case (getOp1(i)) matches
    tagged Invalid: return False;
    tagged Valid .r: return r == 0;
  endcase

endfunction

function Bool src2IsR0(Inst i);
  
  case (getOp2(i)) matches
    tagged Invalid: return False;
    tagged Valid .r: return r == 0;
  endcase
  
endfunction

function Maybe#(RName) getRDest(Inst i);
   return case ( i ) matches

    // -- Memory Ops ------------------------------------------------      

    tagged LW .it : return tagged Valid it.rdest;

    tagged SW .it : return tagged Invalid;

    // -- Simple Ops ------------------------------------------------      

    tagged ADDIU .it : return tagged Valid it.rdest;
    tagged SLTI  .it : return tagged Valid it.rdest;
    tagged SLTIU .it : return tagged Valid it.rdest;
    tagged ANDI  .it : return tagged Valid it.rdest;
    tagged ORI   .it : return tagged Valid it.rdest;
    tagged XORI  .it : return tagged Valid it.rdest;
    tagged LUI   .it : return tagged Valid it.rdest;

    tagged SLL   .it : return tagged Valid it.rdest;
    tagged SRL   .it : return tagged Valid it.rdest;
    tagged SRA   .it : return tagged Valid it.rdest;
    tagged SLLV  .it : return tagged Valid it.rdest;
    tagged SRLV  .it : return tagged Valid it.rdest;
    tagged SRAV  .it : return tagged Valid it.rdest;
    tagged ADDU  .it : return tagged Valid it.rdest;
    tagged SUBU  .it : return tagged Valid it.rdest;
    tagged AND   .it : return tagged Valid it.rdest;
    tagged OR    .it : return tagged Valid it.rdest;
    tagged XOR   .it : return tagged Valid it.rdest;
    tagged NOR   .it : return tagged Valid it.rdest;
    tagged SLT   .it : return tagged Valid it.rdest;
    tagged SLTU  .it : return tagged Valid it.rdest;

    tagged MTC0  .it : return tagged Invalid;
    tagged MFC0  .it : return tagged Valid it.rdest;

    // -- Branches --------------------------------------------------

    tagged BLEZ  .it : return tagged Invalid;

    tagged BGTZ  .it : return tagged Invalid;

    tagged BLTZ  .it : return tagged Invalid;

    tagged BGEZ  .it : return tagged Invalid;

    tagged BEQ   .it : return tagged Invalid;

    tagged BNE   .it : return tagged Invalid;

    // -- Jumps -----------------------------------------------------

    tagged J     .it : return tagged Invalid;

    tagged JR    .it : return tagged Invalid;

    tagged JAL   .it : return tagged Valid 5'd31;

    tagged JALR  .it : return tagged Valid it.rdest;
    default:           return tagged Invalid;
  endcase;
endfunction

function Maybe#(RName) getDest(Inst i);

   return case (getRDest(i)) matches
     tagged Invalid: return tagged Invalid;
     tagged Valid .d: return (d == 0) ? tagged Invalid : tagged Valid d;
    endcase;
    
endfunction


function Bool isBranch(Inst i);
   return case ( i ) matches

    tagged BLEZ  .it : return True;

    tagged BGTZ  .it : return True;

    tagged BLTZ  .it : return True;

    tagged BGEZ  .it : return True;

    tagged BEQ   .it : return True;

    tagged BNE   .it : return True;

    // -- Jumps -----------------------------------------------------

    tagged J     .it : return True;

    tagged JR    .it : return True;

    tagged JAL   .it : return True;

    tagged JALR  .it : return True;
    default:           return False;
  endcase;
endfunction

function Bool isLoad(Inst i);
  return case ( i ) matches

    tagged LW .it : return True;
    default: return False;
    
  endcase;
endfunction

function Bool isStore(Inst i);
  return case ( i ) matches

    tagged SW .it : return True;
    default: return False;
    
  endcase;
endfunction


function Bool isFence(Inst inst);

    return False;

endfunction

function Bool drainBefore(Inst i);

  return False;

endfunction

function Bool drainAfter(Inst i);

  return False;

endfunction
