///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// ISA.bsv                                                                   //
//                                                                           //
// Top-level datatypes for the SMIPS example ISA.                            //
//                                                                           //
// This file will be included by most SMIPs-specific modules.                //
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
typedef Bit#(5)  ShAmt;
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

typedef union tagged                
{

  struct { RName rbase; RName rdest;  SImm offset;  } LW;
  struct { RName rbase; RName rsrc;   SImm offset;  } SW; 

  struct { RName rsrc;  RName rdest;  SImm imm;     } ADDIU;
  struct { RName rsrc;  RName rdest;  SImm imm;     } SLTI;
  struct { RName rsrc;  RName rdest;  SImm imm;     } SLTIU;
  struct { RName rsrc;  RName rdest;  ZImm imm;     } ANDI;
  struct { RName rsrc;  RName rdest;  ZImm imm;     } ORI;
  struct { RName rsrc;  RName rdest;  ZImm imm;     } XORI;
  struct {              RName rdest;  ZImm imm;     } LUI;

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
  struct { RName rsrc;  SImm offset;                } BLEZ;
  struct { RName rsrc;  SImm offset;                } BGTZ;
  struct { RName rsrc;  SImm offset;                } BLTZ;
  struct { RName rsrc;  SImm offset;                } BGEZ;

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

  case (instr) matches

    tagged LW    .it : return { opLW,    it.rbase, it.rdest,  it.offset                   };
    tagged SW    .it : return { opSW,    it.rbase, it.rsrc,   it.offset                   };

    tagged ADDIU .it : return { opADDIU, it.rsrc,  it.rdest,  it.imm                      }; 
    tagged SLTI  .it : return { opSLTI,  it.rsrc,  it.rdest,  it.imm                      }; 
    tagged SLTIU .it : return { opSLTIU, it.rsrc,  it.rdest,  it.imm                      }; 
    tagged ANDI  .it : return { opANDI,  it.rsrc,  it.rdest,  it.imm                      }; 
    tagged ORI   .it : return { opORI,   it.rsrc,  it.rdest,  it.imm                      }; 
    tagged XORI  .it : return { opXORI,  it.rsrc,  it.rdest,  it.imm                      }; 
    tagged LUI   .it : return { opLUI,   5'b0,     it.rdest,  it.imm                      };

    tagged SLL   .it : return { opFUNC,  5'b0,     it.rsrc,  it.rdest,   it.shamt, fcSLL  }; 
    tagged SRL   .it : return { opFUNC,  5'b0,     it.rsrc,  it.rdest,   it.shamt, fcSRL  }; 
    tagged SRA   .it : return { opFUNC,  5'b0,     it.rsrc,  it.rdest,   it.shamt, fcSRA  }; 

    tagged SLLV  .it : return { opFUNC,  it.rshamt, it.rsrc, it.rdest,   5'b0,     fcSLLV }; 
    tagged SRLV  .it : return { opFUNC,  it.rshamt, it.rsrc, it.rdest,   5'b0,     fcSRLV }; 
    tagged SRAV  .it : return { opFUNC,  it.rshamt, it.rsrc, it.rdest,   5'b0,     fcSRAV }; 

    tagged ADDU  .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdest,   5'b0,     fcADDU }; 
    tagged SUBU  .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdest,   5'b0,     fcSUBU }; 
    tagged AND   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdest,   5'b0,     fcAND  }; 
    tagged OR    .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdest,   5'b0,     fcOR   }; 
    tagged XOR   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdest,   5'b0,     fcXOR  }; 
    tagged NOR   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdest,   5'b0,     fcNOR  }; 
    tagged SLT   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdest,   5'b0,     fcSLT  }; 
    tagged SLTU  .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdest,   5'b0,     fcSLTU }; 

    tagged J     .it : return { opJ,     it.target                                        }; 
    tagged JAL   .it : return { opJAL,   it.target                                        }; 
    tagged JR    .it : return { opFUNC,  it.rsrc,  5'b0,     5'b0,      5'b0,      fcJR   };
    tagged JALR  .it : return { opFUNC,  it.rsrc,  5'b0,     it.rdest,   5'b0,     fcJALR };
    tagged BEQ   .it : return { opBEQ,   it.rsrc1, it.rsrc2, it.offset                    };
    tagged BNE   .it : return { opBNE,   it.rsrc1, it.rsrc2, it.offset                    };
    tagged BLEZ  .it : return { opBLEZ,  it.rsrc,  5'b0,     it.offset                    };
    tagged BGTZ  .it : return { opBGTZ,  it.rsrc,  5'b0,     it.offset                    };
    tagged BLTZ  .it : return { opRT,    it.rsrc,  rtBLTZ,   it.offset                    };
    tagged BGEZ  .it : return { opRT,    it.rsrc,  rtBGEZ,   it.offset                    };

    tagged MFC0  .it : return { opRS,    rsMFC0,   it.rdest,  it.cop0src,  11'b0          };
    tagged MTC0  .it : return { opRS,    rsMTC0,   it.rsrc,   it.cop0dest, 11'b0          };
    tagged TERMINATE  .it : return { opRS, rsTERMINATE, 21'b0           };  

  endcase

endfunction

  // Bit#(32) to Inst Function

function Inst bitsToInst( Bit#(32) instrBits );

  let opcode = instrBits[ 31 : 26 ];
  let rs     = instrBits[ 25 : 21 ];
  let rt     = instrBits[ 20 : 16 ];
  let rd     = instrBits[ 15 : 11 ];
  let shamt  = instrBits[ 10 :  6 ];
  let funct  = instrBits[  5 :  0 ];
  let imm    = instrBits[ 15 :  0 ];
  let target = instrBits[ 25 :  0 ];

  case (opcode)

    opLW        : return LW    { rbase:rs, rdest:rt,  offset:imm  };
    opSW        : return SW    { rbase:rs, rsrc:rt,  offset:imm   };
    opADDIU     : return ADDIU { rsrc:rs,  rdest:rt,  imm:imm     };
    opSLTI      : return SLTI  { rsrc:rs,  rdest:rt,  imm:imm     };
    opSLTIU     : return SLTIU { rsrc:rs,  rdest:rt,  imm:imm     };
    opANDI      : return ANDI  { rsrc:rs,  rdest:rt,  imm:imm     };
    opORI       : return ORI   { rsrc:rs,  rdest:rt,  imm:imm     };
    opXORI      : return XORI  { rsrc:rs,  rdest:rt,  imm:imm     };
    opLUI       : return LUI   {           rdest:rt,  imm:imm     };
    opJ         : return J     { target:target                    };
    opJAL       : return JAL   { target:target                    };
    opBEQ       : return BEQ   { rsrc1:rs, rsrc2:rt, offset:imm   };
    opBNE       : return BNE   { rsrc1:rs, rsrc2:rt, offset:imm   };
    opBLEZ      : return BLEZ  { rsrc:rs,  offset:imm             };
    opBGTZ      : return BGTZ  { rsrc:rs,  offset:imm             };

    opFUNC  : 
      case (funct)
        fcSLL   : return SLL   { rsrc:rt,  rdest:rd,  shamt:shamt };
        fcSRL   : return SRL   { rsrc:rt,  rdest:rd,  shamt:shamt };
        fcSRA   : return SRA   { rsrc:rt,  rdest:rd,  shamt:shamt };
        fcSLLV  : return SLLV  { rsrc:rt,  rdest:rd,  rshamt:rs   };
        fcSRLV  : return SRLV  { rsrc:rt,  rdest:rd,  rshamt:rs   };
        fcSRAV  : return SRAV  { rsrc:rt,  rdest:rd,  rshamt:rs   };
        fcADDU  : return ADDU  { rsrc1:rs, rsrc2:rt, rdest:rd     };
        fcSUBU  : return SUBU  { rsrc1:rs, rsrc2:rt, rdest:rd     };
        fcAND   : return AND   { rsrc1:rs, rsrc2:rt, rdest:rd     };
        fcOR    : return OR    { rsrc1:rs, rsrc2:rt, rdest:rd     };
        fcXOR   : return XOR   { rsrc1:rs, rsrc2:rt, rdest:rd     };
        fcNOR   : return NOR   { rsrc1:rs, rsrc2:rt, rdest:rd     };
        fcSLT   : return SLT   { rsrc1:rs, rsrc2:rt, rdest:rd     }; 
        fcSLTU  : return SLTU  { rsrc1:rs, rsrc2:rt, rdest:rd     };
        fcJR    : return JR    { rsrc:rs                          };
        fcJALR  : return JALR  { rsrc:rs,  rdest:rd               };
        default : return ILLEGAL;
      endcase

    opRT : 
      case (rt)
        rtBLTZ  : return BLTZ  { rsrc:rs,  offset:imm             };
        rtBGEZ  : return BGEZ  { rsrc:rs,  offset:imm             };
        default : return ILLEGAL;
      endcase

    opRS : 
      case (rs)
        rsMFC0  : return MFC0  { rdest:rt,  cop0src:rd            };
        rsMTC0  : return MTC0  { rsrc:rt,   cop0dest:rd           };
        rsTERMINATE : return TERMINATE;
        default : return ILLEGAL;
      endcase

    default : return ILLEGAL;

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


function Bool isFence(MIPS_INST inst);

    return False;

endfunction

function Bool drainBefore(Inst i);

  return False;

endfunction

function Bool drainAfter(Inst i);

  return False;

endfunction
