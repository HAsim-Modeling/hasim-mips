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

import GetPut::*;
import ClientServer::*;
import RegFile::*;

/************* Basic Datatypes *************/

typedef Bit#(8)  Token;
typedef Bit#(32) Tick;
typedef Bit#(32) Addr;
typedef Bit#(5)  RName;
typedef Bit#(6)  PRName;
typedef Bit#(32) Value;
typedef Bit#(4)  SnapshotPtr;
typedef Bit#(16) SImm;
typedef Bit#(16) ZImm;
typedef Bit#(5)  ShAmt;
typedef Bit#(26) Target;
typedef Bit#(5)  CP0Index;

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

  struct { RName rbase; RName rdst;  SImm offset;  } LW;
  struct { RName rbase; RName rsrc;  SImm offset;  } SW; 

  struct { RName rsrc;  RName rdst;  SImm imm;     } ADDIU;
  struct { RName rsrc;  RName rdst;  SImm imm;     } SLTI;
  struct { RName rsrc;  RName rdst;  SImm imm;     } SLTIU;
  struct { RName rsrc;  RName rdst;  ZImm imm;     } ANDI;
  struct { RName rsrc;  RName rdst;  ZImm imm;     } ORI;
  struct { RName rsrc;  RName rdst;  ZImm imm;     } XORI;
  struct {              RName rdst;  ZImm imm;     } LUI;

  struct { RName rsrc;  RName rdst;  ShAmt shamt;  } SLL;
  struct { RName rsrc;  RName rdst;  ShAmt shamt;  } SRL;
  struct { RName rsrc;  RName rdst;  ShAmt shamt;  } SRA;
  struct { RName rsrc;  RName rdst;  RName rshamt; } SLLV;
  struct { RName rsrc;  RName rdst;  RName rshamt; } SRLV;
  struct { RName rsrc;  RName rdst;  RName rshamt; } SRAV;
  struct { RName rsrc1; RName rsrc2; RName rdst;   } ADDU;
  struct { RName rsrc1; RName rsrc2; RName rdst;   } SUBU;
  struct { RName rsrc1; RName rsrc2; RName rdst;   } AND;
  struct { RName rsrc1; RName rsrc2; RName rdst;   } OR;
  struct { RName rsrc1; RName rsrc2; RName rdst;   } XOR;
  struct { RName rsrc1; RName rsrc2; RName rdst;   } NOR;
  struct { RName rsrc1; RName rsrc2; RName rdst;   } SLT;
  struct { RName rsrc1; RName rsrc2; RName rdst;   } SLTU;

  struct { Target target;                          } J;
  struct { Target target;                          } JAL;
  struct { RName rsrc;                             } JR;
  struct { RName rsrc;  RName rdst;                } JALR;
  struct { RName rsrc1; RName rsrc2; SImm offset;  } BEQ;
  struct { RName rsrc1; RName rsrc2; SImm offset;  } BNE;
  struct { RName rsrc;  SImm offset;               } BLEZ;
  struct { RName rsrc;  SImm offset;               } BGTZ;
  struct { RName rsrc;  SImm offset;               } BLTZ;
  struct { RName rsrc;  SImm offset;               } BGEZ;

  //struct { RName rdst;  CP0Index cop0src;          } MFC0;
  //struct { RName rsrc;  CP0Index cop0dst;          } MTC0; 

  void                                               ILLEGAL;

}
  Inst
    deriving (Eq);

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

instance Bits#(Inst,32);

  // Pack Function

  function Bit#(32) pack( Inst instr );

    case ( instr ) matches

      tagged LW    .it : return { opLW,    it.rbase, it.rdst,  it.offset };
      tagged SW    .it : return { opSW,    it.rbase, it.rsrc,  it.offset };

      tagged ADDIU .it : return { opADDIU, it.rsrc,  it.rdst,  it.imm                      }; 
      tagged SLTI  .it : return { opSLTI,  it.rsrc,  it.rdst,  it.imm                      }; 
      tagged SLTIU .it : return { opSLTIU, it.rsrc,  it.rdst,  it.imm                      }; 
      tagged ANDI  .it : return { opANDI,  it.rsrc,  it.rdst,  it.imm                      }; 
      tagged ORI   .it : return { opORI,   it.rsrc,  it.rdst,  it.imm                      }; 
      tagged XORI  .it : return { opXORI,  it.rsrc,  it.rdst,  it.imm                      }; 
      tagged LUI   .it : return { opLUI,   5'b0,     it.rdst,  it.imm                      };

      tagged SLL   .it : return { opFUNC,  5'b0,     it.rsrc,  it.rdst,   it.shamt, fcSLL  }; 
      tagged SRL   .it : return { opFUNC,  5'b0,     it.rsrc,  it.rdst,   it.shamt, fcSRL  }; 
      tagged SRA   .it : return { opFUNC,  5'b0,     it.rsrc,  it.rdst,   it.shamt, fcSRA  }; 

      tagged SLLV  .it : return { opFUNC,  it.rshamt, it.rsrc, it.rdst,   5'b0,     fcSLLV }; 
      tagged SRLV  .it : return { opFUNC,  it.rshamt, it.rsrc, it.rdst,   5'b0,     fcSRLV }; 
      tagged SRAV  .it : return { opFUNC,  it.rshamt, it.rsrc, it.rdst,   5'b0,     fcSRAV }; 

      tagged ADDU  .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcADDU }; 
      tagged SUBU  .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcSUBU }; 
      tagged AND   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcAND  }; 
      tagged OR    .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcOR   }; 
      tagged XOR   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcXOR  }; 
      tagged NOR   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcNOR  }; 
      tagged SLT   .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcSLT  }; 
      tagged SLTU  .it : return { opFUNC,  it.rsrc1, it.rsrc2, it.rdst,   5'b0,     fcSLTU }; 

      tagged J     .it : return { opJ,     it.target                                       }; 
      tagged JAL   .it : return { opJAL,   it.target                                       }; 
      tagged JR    .it : return { opFUNC,  it.rsrc,  5'b0,     5'b0,      5'b0,     fcJR   };
      tagged JALR  .it : return { opFUNC,  it.rsrc,  5'b0,     it.rdst,   5'b0,     fcJALR };
      tagged BEQ   .it : return { opBEQ,   it.rsrc1, it.rsrc2, it.offset                   }; 
      tagged BNE   .it : return { opBNE,   it.rsrc1, it.rsrc2, it.offset                   }; 
      tagged BLEZ  .it : return { opBLEZ,  it.rsrc,  5'b0,     it.offset                   }; 
      tagged BGTZ  .it : return { opBGTZ,  it.rsrc,  5'b0,     it.offset                   }; 
      tagged BLTZ  .it : return { opRT,    it.rsrc,  rtBLTZ,   it.offset                   }; 
      tagged BGEZ  .it : return { opRT,    it.rsrc,  rtBGEZ,   it.offset                   }; 

      //tagged MFC0  .it : return { opRS,    rsMFC0,   it.rdst,  it.cop0src, 11'b0           }; 
      //tagged MTC0  .it : return { opRS,    rsMTC0,   it.rsrc,  it.cop0dst, 11'b0           };  

    endcase

  endfunction

  // Unpack Function

  function Inst unpack( Bit#(32) instrBits );

    let opcode = instrBits[ 31 : 26 ];
    let rs     = instrBits[ 25 : 21 ];
    let rt     = instrBits[ 20 : 16 ];
    let rd     = instrBits[ 15 : 11 ];
    let shamt  = instrBits[ 10 :  6 ];
    let funct  = instrBits[  5 :  0 ];
    let imm    = instrBits[ 15 :  0 ];
    let target = instrBits[ 25 :  0 ];

    case ( opcode )

      opLW        : return LW    { rbase:rs, rdst:rt,  offset:imm  };
      opSW        : return SW    { rbase:rs, rsrc:rt,  offset:imm  };
      opADDIU     : return ADDIU { rsrc:rs,  rdst:rt,  imm:imm     };
      opSLTI      : return SLTI  { rsrc:rs,  rdst:rt,  imm:imm     };
      opSLTIU     : return SLTIU { rsrc:rs,  rdst:rt,  imm:imm     };
      opANDI      : return ANDI  { rsrc:rs,  rdst:rt,  imm:imm     };
      opORI       : return ORI   { rsrc:rs,  rdst:rt,  imm:imm     };
      opXORI      : return XORI  { rsrc:rs,  rdst:rt,  imm:imm     };
      opLUI       : return LUI   {           rdst:rt,  imm:imm     };
      opJ         : return J     { target:target                   };
      opJAL       : return JAL   { target:target                   };
      opBEQ       : return BEQ   { rsrc1:rs, rsrc2:rt, offset:imm  };
      opBNE       : return BNE   { rsrc1:rs, rsrc2:rt, offset:imm  };
      opBLEZ      : return BLEZ  { rsrc:rs,  offset:imm            };
      opBGTZ      : return BGTZ  { rsrc:rs,  offset:imm            };

      opFUNC  : 
        case ( funct )
          fcSLL   : return SLL   { rsrc:rt,  rdst:rd,  shamt:shamt };
          fcSRL   : return SRL   { rsrc:rt,  rdst:rd,  shamt:shamt };
          fcSRA   : return SRA   { rsrc:rt,  rdst:rd,  shamt:shamt };
          fcSLLV  : return SLLV  { rsrc:rt,  rdst:rd,  rshamt:rs   };
          fcSRLV  : return SRLV  { rsrc:rt,  rdst:rd,  rshamt:rs   };
          fcSRAV  : return SRAV  { rsrc:rt,  rdst:rd,  rshamt:rs   };
          fcADDU  : return ADDU  { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcSUBU  : return SUBU  { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcAND   : return AND   { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcOR    : return OR    { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcXOR   : return XOR   { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcNOR   : return NOR   { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcSLT   : return SLT   { rsrc1:rs, rsrc2:rt, rdst:rd     }; 
          fcSLTU  : return SLTU  { rsrc1:rs, rsrc2:rt, rdst:rd     };
          fcJR    : return JR    { rsrc:rs                         };
          fcJALR  : return JALR  { rsrc:rs,  rdst:rd               };
          default : return ILLEGAL;
        endcase

      opRT : 
        case ( rt )
          rtBLTZ  : return BLTZ  { rsrc:rs,  offset:imm            };
          rtBGEZ  : return BGEZ  { rsrc:rs,  offset:imm            };
          default : return ILLEGAL;
        endcase

      opRS : 
        case ( rs )
          //rsMFC0  : return MFC0  { rdst:rt,  cop0src:rd            };
          //rsMTC0  : return MTC0  { rsrc:rt,  cop0dst:rd            };
          default : return ILLEGAL;
        endcase

      default : return ILLEGAL;
      
    endcase

  endfunction

endinstance


//Decoded Instruction
typedef union tagged                
{

  struct { PRName pbase; PRName pdst;  SImm offset;	PRName opdst; } DLW;
  struct { PRName pbase; PRName psrc;  SImm offset;	PRName opdst; } DSW; 

  struct { PRName psrc;  PRName pdst;  SImm imm;	PRName opdst; } DADDIU;
  struct { PRName psrc;  PRName pdst;  SImm imm;	PRName opdst; } DSLTI;
  struct { PRName psrc;  PRName pdst;  SImm imm;	PRName opdst; } DSLTIU;
  struct { PRName psrc;  PRName pdst;  ZImm imm;	PRName opdst; } DANDI;
  struct { PRName psrc;  PRName pdst;  ZImm imm;	PRName opdst; } DORI;
  struct { PRName psrc;  PRName pdst;  ZImm imm;	PRName opdst; } DXORI;
  struct { 		 PRName pdst;  ZImm imm;	PRName opdst; } DLUI;

  struct { PRName psrc;  PRName pdst;  ShAmt shamt;	PRName opdst; } DSLL;
  struct { PRName psrc;  PRName pdst;  ShAmt shamt;	PRName opdst; } DSRL;
  struct { PRName psrc;  PRName pdst;  ShAmt shamt;	PRName opdst; } DSRA;
  struct { PRName psrc;  PRName pdst;  PRName pshamt;   PRName opdst; } DSLLV;
  struct { PRName psrc;  PRName pdst;  PRName pshamt;   PRName opdst; } DSRLV;
  struct { PRName psrc;  PRName pdst;  PRName pshamt;   PRName opdst; } DSRAV;
  struct { PRName psrc1; PRName psrc2; PRName pdst;	PRName opdst; } DADDU;
  struct { PRName psrc1; PRName psrc2; PRName pdst;	PRName opdst; } DSUBU;
  struct { PRName psrc1; PRName psrc2; PRName pdst;	PRName opdst; } DAND;
  struct { PRName psrc1; PRName psrc2; PRName pdst;	PRName opdst; } DOR;
  struct { PRName psrc1; PRName psrc2; PRName pdst;	PRName opdst; } DXOR;
  struct { PRName psrc1; PRName psrc2; PRName pdst;	PRName opdst; } DNOR;
  struct { PRName psrc1; PRName psrc2; PRName pdst;	PRName opdst; } DSLT;
  struct { PRName psrc1; PRName psrc2; PRName pdst;	PRName opdst; } DSLTU;

  struct { Target target; 				PRName opdst; } DJ;
  struct { PRName pdst;  Target target;			PRName opdst; } DJAL;
  struct { PRName psrc;					PRName opdst; } DJR;
  struct { PRName psrc;  PRName pdst;			PRName opdst; } DJALR;
  struct { PRName psrc1; PRName psrc2; SImm offset;	PRName opdst; } DBEQ;
  struct { PRName psrc1; PRName psrc2; SImm offset;	PRName opdst; } DBNE;
  struct { PRName psrc;  SImm offset;			PRName opdst; } DBLEZ;
  struct { PRName psrc;  SImm offset;			PRName opdst; } DBGTZ;
  struct { PRName psrc;  SImm offset;			PRName opdst; } DBLTZ;
  struct { PRName psrc;  SImm offset;			PRName opdst; } DBGEZ;

  //struct { PRName pdst;  CP0Index cop0src;  		PRName opdst; } DMFC0;
  //struct { PRName rsrc;  CP0Index cop0dst;  		PRName opdst; } DMTC0; 

  void                                                                  DILLEGAL;

}
  DecodedInst
    deriving 
            (Eq, Bits);

//Executed Instruction

//Possibly should include branch info if Functional Partition has branch predictor

//Exec-->Mem-->LCom-->GCom
typedef union tagged 
{
  struct { Value val; PRName pdst; PRName opdst; } EWB;
  //struct { Value val; CP0Index cop0dst; PRName opdst; } ECoProc;
  struct { Addr addr; PRName pdst; PRName opdst; } ELoad;
  struct { Addr addr; Value  val;  PRName opdst; } EStore;
  struct {		    	   PRName opdst; } ENop;
}
  ExecedInst
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

//Result of executing an instruction

//FP Exec-->TP
typedef union tagged
{
  Addr     RBranchTaken;
  void     RBranchNotTaken; // Possibly should also include address. 
  void     RNop;
  void     RTerminate;
}
  InstResult 
    deriving 
            (Eq, Bits);


typedef union tagged
{
  void         COM_LoadState;
  void         COM_RunProgram;  //Perhaps pass in global tick here
  void         COM_CheckResult;
  ModelCommand COM_Other;
}
  Command 
                deriving 
		        (Eq, Bits);

		
typedef union tagged
{
  void                                            RESP_DoneLoading;
  void                                            RESP_DoneRunning; //Perhaps return local tick here	 
  struct {Addr addr; Value exp_v; Value found_v;} RESP_Failure;
  void                                            RESP_CheckPassed;
  void                                            RESP_CheckFailed;
  ModelResponse                                   RESP_Other;
}
  Response
                 deriving
		         (Eq, Bits);

//Commands specific to this model

typedef void ModelCommand;
typedef void ModelResponse;

