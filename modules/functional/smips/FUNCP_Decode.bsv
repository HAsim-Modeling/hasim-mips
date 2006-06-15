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

module [Module] mkDecode ();
  
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
 
  
  function RName getDest(Inst i);
     return case ( i ) matches

      // -- Memory Ops ------------------------------------------------      

      tagged LW .it : return Valid it.rdst;

      tagged SW .it : return Invalid;

      // -- Simple Ops ------------------------------------------------      

      tagged ADDIU .it : return Valid it.rdst;
      tagged SLTI  .it : return Valid it.rdst;
      tagged SLTIU .it : return Valid it.rdst;
      tagged ANDI  .it : return Valid it.rdst;
      tagged ORI   .it : return Valid it.rdst;
      tagged XORI  .it : return Valid it.rdst;
      tagged LUI   .it : return Invalid;

      tagged SLL   .it : return Valid it.rdst;
      tagged SRL   .it : return Valid it.rdst;
      tagged SRA   .it : return Valid it.rdst;
      tagged SLLV  .it : return Valid it.rdst;
      tagged SRLV  .it : return Valid it.rdst;
      tagged SRAV  .it : return Valid it.rdst;
      tagged ADDU  .it : return Valid it.rdst;
      tagged SUBU  .it : return Valid it.rdst;
      tagged AND   .it : return Valid it.rdst;
      tagged OR    .it : return Valid it.rdst;
      tagged XOR   .it : return Valid it.rdst;
      tagged NOR   .it : return Valid it.rdst;
      tagged SLT   .it : return Valid it.rdst;
      tagged SLTU  .it : return Valid it.rdst;

      //tagged MTC0  .it : return Invalid;
      //tagged MFC0  .it : return Valid it.rdst;

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

      tagged JALR  .it : return Valid it.rdst;
      default:           return Invalid;
    endcase;
  endfunction
  
  
  function RName isBranch(Inst i);
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
    
    //Actually do the decode
    case (inst) matches
      // -- Memory Ops ------------------------------------------------      
      
      //Load Word
      tagged LW {rbase: .rb, rdst:.rd, offset: .off}: 
	begin
	  let prb  = bypass.lookup1(rb);

          let rtup <- bypass.makeMapping(Valid rd, t, False);
          match {.prd, .oprd} = rtup;

          decinst = DLW 
	            {
		      opdst:  oprd, 
		      pbase:  prb, 
		      pdst:   prd, 
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rb, prb), 
		      dep_src2: Invalid
		    };

	end
	
      //Store Word
      tagged SW {rbase: .rb, rsrc: .rs, offset: .off}: 
        begin
	  let prb = bypass.lookup1(rb);
	  let prs = bypass.lookup2(rs);
	  
          let rtup <- bypass.makeMapping(Invalid, t, False);
          match {.prd, .oprd} = rtup;
	  
          decinst = DSW
	            {
		      opdst:  oprd, 
		      pbase:  prb, 
		      psrc:   prs, 
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rb, prb), 
		      dep_src2: Valid tuple2(rs, prs)
		    };
        end

      // -- Simple Ops ------------------------------------------------      

      //Add Immediate Signed (Not a Typo)
      tagged ADDIU {rsrc: .rs, rdst: .rd, imm: .simm}:
        begin
	  
	  let prs  = bypass.lookup1(rs);
	  
          let rtup <- bypass.makeMapping(Valid rd, t, False);
          match {.prd, .oprd} = rtup;
	  
	  
          decinst = DADDIU 
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      imm:   simm
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
        end
	
      //Set Less Than Immediate (Signed)
      tagged SLTI {rsrc: .rs, rdst: .rd, imm: .simm}:
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSLTI 
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      imm:   simm
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //Set Less Than Immediate Unsigned 
      tagged SLTIU {rsrc: .rs, rdst: .rd, imm:.simm}:
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSLTIU
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      imm:   simm //Immediate is still sign extended
		                              //Not a typo: Exec handles it differently
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //And Immediate
      tagged ANDI {rsrc: .rs, rdst: .rd, imm:.zimm}:
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DANDI 
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      imm:   zimm
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //Or Immediate
      tagged ORI {rsrc: .rs, rdst: .rd, imm:.zimm}:
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DORI 
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      imm:   zimm
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //XOR Immediate
      tagged XORI {rsrc: .rs, rdst: .rd, imm:.zimm}:
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DXORI 
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      imm:   zimm
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //Load Unsigned Immediate (Really is unsigned)
      tagged LUI {rdst: .rd, imm:.zimm}:
        begin
	  	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DLUI 
	            {
		      opdst: oprd, 
		      pdst:  prd, 
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
      tagged SLL {rsrc: .rs, rdst: .rd, shamt:.sha}: 
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSLL 
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      shamt: sha
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //Shift Right Logical (Immediate)
      tagged SRL {rsrc: .rs, rdst: .rd, shamt:.sha}: 
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	 
          decinst = DSRL
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      shamt: sha
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //Shift Right Arithmatic (Immediate)
      tagged SRA {rsrc: .rs, rdst: .rd, shamt:.sha}: 
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSRA
	            {
		      opdst: oprd, 
		      pdst:  prd, 
		      psrc:  prs, 
		      shamt: sha
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
	
      //Shift Left Logical Variable
      tagged SLLV {rsrc: .rs, rdst: .rd, rshamt:.rsha}: 
        begin
	  
	  let prs   = bypass.lookup1(rs);
	  let prsha = bypass.lookup2(rsha);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSLLV 
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc:   prs, 
		      pshamt: prsha
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Valid tuple2(rsha, prsha)
		    };
	end
	
      //Shift Right Logical Variable
      tagged SRLV {rsrc: .rs, rdst: .rd, rshamt:.rsha}: 
        begin
	  
	  let prs   = bypass.lookup1(rs);
	  let prsha = bypass.lookup2(rsha);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSRLV 
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc:   prs, 
		      pshamt: prsha
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Valid tuple2(rsha, prsha)
		    };
	end
	
      //Shift Right Arithmatic Variable
      tagged SRAV {rsrc: .rs, rdst: .rd, rshamt:.rsha}: 
        begin
	  
	  let prs   = bypass.lookup1(rs);
	  let prsha = bypass.lookup2(rsha);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSRAV 
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc:   prs, 
		      pshamt: prsha
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Valid tuple2(rsha, prsha)
		    };
	end
	
      //Add Unsigned
      tagged ADDU {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}: 
        begin
	  
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DADDU
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end

      //Subtract Unsigned
      tagged SUBU {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}:
        begin
	  
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSUBU 
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end
	
      //And
      tagged AND {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}:
        begin
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DAND
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end
      
      //OR
      tagged OR {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}:
        begin
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DOR
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end
	
      //XOR
      tagged XOR {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}:
        begin
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DXOR
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end

      //NOR
      tagged NOR {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}:
        begin
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DNOR
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end

      //Set Less Than
      tagged SLT {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}: 
        begin
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSLT
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end
      
      //Set Less Than Unsigned
      tagged SLTU {rsrc1: .rs1, rsrc2: .rs2, rdst: .rd}: 
        begin
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup2(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DSLTU
	            {
		      opdst:  oprd, 
		      pdst:   prd, 
		      psrc1:  prs1,
		      psrc2:  prs2
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd,  prd), 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
	end
/*
      //Move To Coprocessor 0
      tagged MTC0 .rs .op: 
        begin
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DMTC0
	            {
		      opdst:   oprd, 
		      psrc:    prs,
		      cop0src: op
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
	end
      
      //Move From Coprocessor 0
      tagged MFC0 .rd .op:
        begin 
	
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DMFC0
	            {
		      opdst:   oprd, 
		      pdst:    prd,
		      cop0dst: op
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
	
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DBLEZ
	            {
		      opdst:  oprd, 
		      psrc:   prs,
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
        end

      //Branch if Greater Than Zero
      tagged BGTZ {rsrc: .rs, offset: .off}: 
        begin
	
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DBGTZ
	            {
		      opdst:  oprd, 
		      psrc:   prs,
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
        end

      //Branch if Less Than Zero
      tagged BLTZ {rsrc: .rs, offset: .off}: 
        begin
	
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DBLTZ
	            {
		      opdst:  oprd, 
		      psrc:   prs,
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
        end

      //Branch if Greater than or Equal to Zero
      tagged BGEZ {rsrc: .rs, offset: .off}: 
        begin
	
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DBGEZ
	            {
		      opdst:  oprd, 
		      psrc:   prs,
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
        end

      //Branch if Equal
      tagged BEQ {rsrc1: .rs1, rsrc2: .rs2, offset: .off}: 
        begin
	
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup1(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DBEQ
	            {
		      opdst:  oprd, 
		      psrc1:  prs1,
		      psrc2:  prs2,
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
        end

      //Branch if Not Equal
      tagged BNE {rsrc1: .rs1, rsrc2: .rs2, offset: .off}: 
        begin
	
	  let prs1 = bypass.lookup1(rs1);
	  let prs2 = bypass.lookup1(rs2);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DBNE
	            {
		      opdst:  oprd, 
		      psrc1:  prs1,
		      psrc2:  prs2,
		      offset: off
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs1, prs1), 
		      dep_src2: Valid tuple2(rs2, prs2)
		    };
        end
      
      // -- Jumps -----------------------------------------------------

      //Jump
      tagged J {target: .targ}: 
        begin
		  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DJ
	            {
		      opdst:  oprd, 
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
	
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Invalid, t, False);
	  
          decinst = DJR
	            {
		      opdst:  oprd, 
		      psrc:   prs
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Invalid, 
		      dep_src1: Valid tuple2(rs, prs), 
		      dep_src2: Invalid
		    };
        end
      //Jump and Link (into archictectural register 31)
      tagged JAL {target: .targ}: 
        begin
		 
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid 5'd31, t, False);
	  
          decinst = DJAL
	            {
		      opdst:  oprd,
		      pdst:   prd, 
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
      tagged JALR {rsrc: .rs, rdst: .rd}: 
        begin
	  
	  let prs = bypass.lookup1(rs);
	  
          match {.prd, .oprd} <- bypass.makeMapping(Valid rd, t, False);
	  
          decinst = DJALR
	            {
		      opdst:  oprd,
		      pdst:   prd,
		      psrc:   prs
		    };
		    
          depinfo = DepInfo 
	            {
		      dep_dest: Valid tuple2(rd, prd), 
		      dep_src1: Valid tuple2(rs, prs), 
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

