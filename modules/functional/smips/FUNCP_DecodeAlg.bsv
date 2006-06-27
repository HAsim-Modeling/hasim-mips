import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import HASim::*;
import FUNCP_Base::*;
import Debug::*;

import ISA::*;


`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Functional"

//-------------------------------------------------------------------------//
// Decode Stage                                                            //
//-------------------------------------------------------------------------//

// Also lookup physical register from BypassUnit

module [HASim_Module] mkFUNCP_DecodeAlg ();
  
  //Ports
  Connection_Server#(Tuple3#(Token, Tuple2#(Addr, Inst), void), 
                     Tuple3#(Token, DepInfo, Tuple2#(Addr, DecodedInst))) 
  //...
  link_dec <- mkConnection_Server("link_dec");

  Connection_Client#(Tuple3#(Maybe#(RName), Token, Bool), 
                     Tuple2#(PRName, PRName))
  //...
  link_mapping <- mkConnection_Client("dec_to_bypass_mapping");
  
  Connection_Client#(RName, PRName) 
  //...
        link_lookup1 <- mkConnection_Client("dec_to_bypass_lookup1");

  Connection_Client#(RName, PRName) 
  //...
        link_lookup2 <- mkConnection_Client("dec_to_bypass_lookup2");

  FIFO#(Tuple3#(Token, Addr, Inst)) 
  //...
  waitingQ <- mkFIFO();
  
  //Helper functions
  
  
  function RName getOp1(Inst i);
     return case ( i ) matches

      // -- Memory Ops ------------------------------------------------      

      tagged LW .it : return it.rbase;

      tagged SW .it : return it.rbase;

      // -- Simple Ops ------------------------------------------------      

      tagged ADDIU .it : return it.rsrc;
      tagged SLTI  .it : return it.rsrc;
      tagged SLTIU .it : return it.rsrc;
      tagged ANDI  .it : return it.rsrc;
      tagged ORI   .it : return it.rsrc;
      tagged XORI  .it : return it.rsrc;
      tagged LUI   .it : return ?;

      tagged SLL   .it : return it.rsrc;
      tagged SRL   .it : return it.rsrc;
      tagged SRA   .it : return it.rsrc;
      tagged SLLV  .it : return it.rsrc;
      tagged SRLV  .it : return it.rsrc;
      tagged SRAV  .it : return it.rsrc;
      tagged ADDU  .it : return it.rsrc1;
      tagged SUBU  .it : return it.rsrc1;
      tagged AND   .it : return it.rsrc1;
      tagged OR    .it : return it.rsrc1;
      tagged XOR   .it : return it.rsrc1;
      tagged NOR   .it : return it.rsrc1;
      tagged SLT   .it : return it.rsrc1;
      tagged SLTU  .it : return it.rsrc1;

      //tagged MTC0  .it : return it.rsrc;
      //tagged MFC0  .it : return ?;

      // -- Branches --------------------------------------------------

      tagged BLEZ  .it : return it.rsrc;

      tagged BGTZ  .it : return it.rsrc;

      tagged BLTZ  .it : return it.rsrc;

      tagged BGEZ  .it : return it.rsrc;

      tagged BEQ   .it : return it.rsrc1;

      tagged BNE   .it : return it.rsrc1;
      
      // -- Jumps -----------------------------------------------------
      
      tagged J     .it : return ?;
      
      tagged JR    .it : return ?;

      tagged JAL   .it : return ?;

      tagged JALR  .it : return  it.rsrc;
      default:           return ?;
    endcase;
  endfunction
  
  function RName getOp2(Inst i);
    return case ( i ) matches

      tagged SW    .it : return it.rsrc;
      tagged ADDU  .it : return it.rsrc2;
      tagged SUBU  .it : return it.rsrc2;
      tagged AND   .it : return it.rsrc2;
      tagged OR    .it : return it.rsrc2;
      tagged XOR   .it : return it.rsrc2;
      tagged NOR   .it : return it.rsrc2;
      tagged SLT   .it : return it.rsrc2;
      tagged SLTU  .it : return it.rsrc2;

      tagged BEQ   .it : return it.rsrc2;

      tagged BNE   .it : return it.rsrc2;
      default:           return ?;
    endcase;
  endfunction
 
  
  function Maybe#(RName) getDest(Inst i);
     return case ( i ) matches

      // -- Memory Ops ------------------------------------------------      

      tagged LW .it : return Valid it.rdest;

      tagged SW .it : return Invalid;

      // -- Simple Ops ------------------------------------------------      

      tagged ADDIU .it : return Valid it.rdest;
      tagged SLTI  .it : return Valid it.rdest;
      tagged SLTIU .it : return Valid it.rdest;
      tagged ANDI  .it : return Valid it.rdest;
      tagged ORI   .it : return Valid it.rdest;
      tagged XORI  .it : return Valid it.rdest;
      tagged LUI   .it : return Invalid;

      tagged SLL   .it : return Valid it.rdest;
      tagged SRL   .it : return Valid it.rdest;
      tagged SRA   .it : return Valid it.rdest;
      tagged SLLV  .it : return Valid it.rdest;
      tagged SRLV  .it : return Valid it.rdest;
      tagged SRAV  .it : return Valid it.rdest;
      tagged ADDU  .it : return Valid it.rdest;
      tagged SUBU  .it : return Valid it.rdest;
      tagged AND   .it : return Valid it.rdest;
      tagged OR    .it : return Valid it.rdest;
      tagged XOR   .it : return Valid it.rdest;
      tagged NOR   .it : return Valid it.rdest;
      tagged SLT   .it : return Valid it.rdest;
      tagged SLTU  .it : return Valid it.rdest;

      //tagged MTC0  .it : return Invalid;
      //tagged MFC0  .it : return Valid it.rdest;

      // -- Branches --------------------------------------------------

      tagged BLEZ  .it : return Invalid;

      tagged BGTZ  .it : return Invalid;

      tagged BLTZ  .it : return Invalid;

      tagged BGEZ  .it : return Invalid;

      tagged BEQ   .it : return Invalid;

      tagged BNE   .it : return Invalid;
      
      // -- Jumps -----------------------------------------------------
      
      tagged J     .it : return Invalid;
      
      tagged JR    .it : return Invalid;

      tagged JAL   .it : return Invalid;

      tagged JALR  .it : return Valid it.rdest;
      default:           return Invalid;
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
  
  rule handleDecode (True);
  
    debug_rule("handleDecode");
    
    Tuple3#(Token, Tuple2#(Addr, Inst), void) 
    //...
    tup <- link_dec.getReq();
    
    match {.t, {.a, .inst}, .*} = tup;
    
    //Get the architectural dest/sources
    RName ara = getOp1(inst);
    RName arb = getOp2(inst);
    Maybe#(RName) mrd = getDest(inst);
    Bool rewind = isBranch(inst);
    
    //Translate into physical registers
    link_lookup1.makeReq(ara);
    link_lookup2.makeReq(arb);
    
    link_mapping.makeReq(tuple3(mrd, t, rewind));
    
    waitingQ.enq(tuple3(t, a, inst));
        
  endrule
  //handleDecode
  
  //Handles the actual decoding and register allocation
  
  rule handleResponse (True);
      
    debug_rule("handleResponse");
    
    match {.t, .a, .inst} = waitingQ.first();
    waitingQ.deq();
    
    DepInfo depinfo = ?;
    DecodedInst decinst = ?;
    
    PRName pra <- link_lookup1.getResp();
    PRName prb <- link_lookup2.getResp();
    match {.prd, .oprd} <- link_mapping.getResp();
    
    //Actually do the decode
    case (inst) matches
      // -- Memory Ops ------------------------------------------------      
      
      //Load Word
      tagged LW {rbase: .rb, rdest:.rd, offset: .off}: 
	begin

          decinst = DLW 
	            {
		      opdest:  oprd, 
		      pbase:  pra, 
		      pdest:   prd, 
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rb, pra), 
		      dep_src2: Invalid
		    };

	end
	
      //Store Word
      tagged SW {rbase: .rb, rsrc: .rs, offset: .off}: 
        begin

          decinst = DSW
	            {
		      opdest:  oprd, 
		      pbase:  pra, 
		      psrc:   prb, 
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rb, pra), 
		      dep_src2: Valid tuple2(rs, prb)
		    };
        end

      // -- Simple Ops ------------------------------------------------      

      //Add Immediate Signed (Not a Typo)
      tagged ADDIU {rsrc: .rs, rdest: .rd, imm: .simm}:
        begin
	  	  
          decinst = DADDIU 
	            {
		      opdest: oprd, 
		      pdest:  prd, 
		      psrc:  pra, 
		      imm:   simm
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
        end
	
      //Set Less Than Immediate (Signed)
      tagged SLTI {rsrc: .rs, rdest: .rd, imm: .simm}:
        begin
	  
          decinst = DSLTI 
	            {
		      opdest: oprd, 
		      pdest:  prd, 
		      psrc:  pra, 
		      imm:   simm
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
	end
	
      //Set Less Than Immediate Unsigned 
      tagged SLTIU {rsrc: .rs, rdest: .rd, imm:.simm}:
        begin
	  
          decinst = DSLTIU
	            {
		      opdest: oprd, 
		      pdest:  prd, 
		      psrc:  pra, 
		      imm:   simm //Immediate is still sign extended
		                  //Not a typo: Exec handles it differently
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
	end
	
      //And Immediate
      tagged ANDI {rsrc: .rs, rdest: .rd, imm:.zimm}:
        begin
	  
          decinst = DANDI 
	            {
		      opdest: oprd, 
		      pdest:  prd, 
		      psrc:  pra, 
		      imm:   zimm
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
	end
	
      //Or Immediate
      tagged ORI {rsrc: .rs, rdest: .rd, imm:.zimm}:
        begin
	  
          decinst = DORI 
	            {
		      opdest: oprd, 
		      pdest:  prd, 
		      psrc:  pra, 
		      imm:   zimm
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
	end
	
      //XOR Immediate
      tagged XORI {rsrc: .rs, rdest: .rd, imm:.zimm}:
        begin
	  
          decinst = DXORI 
	            {
		      opdest: oprd, 
		      pdest:  prd, 
		      psrc:  pra, 
		      imm:   zimm
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
	end
	
      //Load Unsigned Immediate (Really is unsigned)
      tagged LUI {rdest: .rd, imm:.zimm}:
        begin
	  
          decinst = DLUI 
	            {
		      opdest: oprd, 
		      pdest:  prd, 
		      imm:   zimm
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Invalid,
		      dep_src2: Invalid
		    };
	end
	
      //Shift Left Logical (Immediate)
      tagged SLL {rsrc: .rs, rdest: .rd, shamt:.sha}: 
        begin
	  
          decinst = DSLL 
	            {
		      opdest: oprd, 
		      pdest:  prd, 
		      psrc:  pra, 
		      shamt: sha
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
	end
	
      //Shift Right Logical (Immediate)
      tagged SRL {rsrc: .rs, rdest: .rd, shamt:.sha}: 
        begin
	  
          decinst = DSRL
	            {
		      opdest: oprd, 
		      pdest:  prd, 
		      psrc:  pra, 
		      shamt: sha
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
	end
	
      //Shift Right Arithmatic (Immediate)
      tagged SRA {rsrc: .rs, rdest: .rd, shamt:.sha}: 
        begin
	  
          decinst = DSRA
	            {
		      opdest: oprd, 
		      pdest:  prd, 
		      psrc:  pra, 
		      shamt: sha
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
	end
	
      //Shift Left Logical Variable
      tagged SLLV {rsrc: .rs, rdest: .rd, rshamt:.rsha}: 
        begin
	  
          decinst = DSLLV 
	            {
		      opdest:  oprd, 
		      pdest:   prd, 
		      psrc:   pra, 
		      pshamt: prb
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Valid tuple2(rsha, prb)
		    };
	end
	
      //Shift Right Logical Variable
      tagged SRLV {rsrc: .rs, rdest: .rd, rshamt:.rsha}: 
        begin
	  
          decinst = DSRLV 
	            {
		      opdest:  oprd, 
		      pdest:   prd, 
		      psrc:   pra, 
		      pshamt: prb
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Valid tuple2(rsha, prb)
		    };
	end
	
      //Shift Right Arithmatic Variable
      tagged SRAV {rsrc: .rs, rdest: .rd, rshamt:.rsha}: 
        begin
	  
          decinst = DSRAV 
	            {
		      opdest:  oprd, 
		      pdest:   prd, 
		      psrc:   pra, 
		      pshamt: prb
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Valid tuple2(rsha, prb)
		    };
	end
	
      //Add Unsigned
      tagged ADDU {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}: 
        begin
	  
          decinst = DADDU
	            {
		      opdest:  oprd, 
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, pra), 
		      dep_src2: Valid tuple2(rs2, prb)
		    };
	end

      //Subtract Unsigned
      tagged SUBU {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}:
        begin
	  
          decinst = DSUBU 
	            {
		      opdest:  oprd, 
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, pra), 
		      dep_src2: Valid tuple2(rs2, prb)
		    };
	end
	
      //And
      tagged AND {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}:
        begin

          decinst = DAND
	            {
		      opdest:  oprd, 
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, pra), 
		      dep_src2: Valid tuple2(rs2, prb)
		    };
	end
      
      //OR
      tagged OR {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}:
        begin
	  
          decinst = DOR
	            {
		      opdest:  oprd, 
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, pra), 
		      dep_src2: Valid tuple2(rs2, prb)
		    };
	end
	
      //XOR
      tagged XOR {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}:
        begin
	  
          decinst = DXOR
	            {
		      opdest:  oprd, 
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, pra), 
		      dep_src2: Valid tuple2(rs2, prb)
		    };
	end

      //NOR
      tagged NOR {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}:
        begin
	  
          decinst = DNOR
	            {
		      opdest:  oprd, 
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, pra), 
		      dep_src2: Valid tuple2(rs2, prb)
		    };
	end

      //Set Less Than
      tagged SLT {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}: 
        begin
	  
          decinst = DSLT
	            {
		      opdest:  oprd, 
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, pra), 
		      dep_src2: Valid tuple2(rs2, prb)
		    };
	end
      
      //Set Less Than Unsigned
      tagged SLTU {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}: 
        begin
	  
          decinst = DSLTU
	            {
		      opdest:  oprd, 
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, pra), 
		      dep_src2: Valid tuple2(rs2, prb)
		    };
	end
/*
      //Move To Coprocessor 0
      tagged MTC0 .rs .op: 
        begin
	 
          decinst = DMTC0
	            {
		      opdest:   oprd, 
		      psrc:    pra,
		      cop0src: op
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
	end
      
      //Move From Coprocessor 0
      tagged MFC0 .rd .op:
        begin 
	
          decinst = DMFC0
	            {
		      opdest:   oprd, 
		      pdest:    prd,
		      cop0dest: op
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Invalid, 
		      dep_src2: Invalid
		    };
	end
*/

      // -- Branches --------------------------------------------------
      
      //Branch if Less-Than or Equal to Zero
      tagged BLEZ {rsrc: .rs, offset: .off}:
        begin
	
          decinst = DBLEZ
	            {
		      opdest:  oprd, 
		      psrc:   pra,
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
        end

      //Branch if Greater Than Zero
      tagged BGTZ {rsrc: .rs, offset: .off}: 
        begin
	
          decinst = DBGTZ
	            {
		      opdest:  oprd, 
		      psrc:   pra,
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
        end

      //Branch if Less Than Zero
      tagged BLTZ {rsrc: .rs, offset: .off}: 
        begin
	
          decinst = DBLTZ
	            {
		      opdest:  oprd, 
		      psrc:   pra,
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
        end

      //Branch if Greater than or Equal to Zero
      tagged BGEZ {rsrc: .rs, offset: .off}: 
        begin
	
          decinst = DBGEZ
	            {
		      opdest:  oprd, 
		      psrc:   pra,
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
        end

      //Branch if Equal
      tagged BEQ {rsrc1: .rs1, rsrc2: .rs2, offset: .off}: 
        begin
	
          decinst = DBEQ
	            {
		      opdest:  oprd, 
		      psrc1:  pra,
		      psrc2:  prb,
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs1, pra), 
		      dep_src2: Valid tuple2(rs2, prb)
		    };
        end

      //Branch if Not Equal
      tagged BNE {rsrc1: .rs1, rsrc2: .rs2, offset: .off}: 
        begin
	
          decinst = DBNE
	            {
		      opdest:  oprd, 
		      psrc1:  pra,
		      psrc2:  prb,
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs1, pra), 
		      dep_src2: Valid tuple2(rs2, prb)
		    };
        end
      
      // -- Jumps -----------------------------------------------------

      //Jump
      tagged J {target: .targ}: 
        begin
	
          decinst = DJ
	            {
		      opdest:  oprd, 
		      target: targ
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Invalid, 
		      dep_src2: Invalid
		    };
        end
      
      //Jump Register
      tagged JR {rsrc: .rs}:
        begin
	
          decinst = DJR
	            {
		      opdest:  oprd, 
		      psrc:   pra
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
        end
      //Jump and Link (into archictectural register 31)
      tagged JAL {target: .targ}: 
        begin
	
          decinst = DJAL
	            {
		      opdest:  oprd,
		      pdest:   prd, 
		      target: targ
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(5'd31, prd), 
		      dep_src1: Invalid, 
		      dep_src2: Invalid
		    };
        end

      //Jump and Link into Register
      tagged JALR {rsrc: .rs, rdest: .rd}: 
        begin
	  
          decinst = DJALR
	            {
		      opdest:  oprd,
		      pdest:   prd,
		      psrc:   pra
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, pra), 
		      dep_src2: Invalid
		    };
        end

       // -- Illegal ---------------------------------------------------
 
      tagged TERMINATE: 
        begin
          decinst = DTERMINATE;
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Invalid, 
		      dep_src2: Invalid
		    };
        end

      default: 
        begin
          decinst = DILLEGAL;
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Invalid, 
		      dep_src2: Invalid
		    };
        end

    endcase

    debug(2, $display("DEC: Physical Sources: (PR%d, PR%d)", pra, prb));
    
    link_dec.makeResp(tuple3(t, depinfo, tuple2(a, decinst)));
    
  endrule
  
endmodule

