import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import fpga_components::*;
import hasim_common::*;

import hasim_funcp_base::*;
import hasim_isa::*;


`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Functional"

//-------------------------------------------------------------------------//
// Decode Stage                                                            //
//-------------------------------------------------------------------------//

// Also lookup physical register from BypassUnit

module [HASim_Module] mkFUNCP_DecodeAlg#(File debug_log, Tick curCC) ();
  
  //Ports
  Connection_Server#(Tuple3#(Token, Tuple2#(Addr, PackedInst), void), 
                     Tuple3#(Token, DepInfo, Tuple2#(Addr, DecodedInst))) 
  //...
  link_dec <- mkConnection_Server("fp_dec_stage");

  Connection_Client#(Tuple5#(Maybe#(RName), Token, Bool, RName, RName), 
                     Tuple3#(PRName, PRName, PRName))
  //...
  link_mapping <- mkConnection_Client("dec_to_bypass_mapping");
  
  FIFO#(Tuple3#(Token, Addr, PackedInst)) 
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

      tagged MTC0  .it : return it.rsrc;
      tagged MFC0  .it : return ?;

      // -- Branches --------------------------------------------------

      tagged BLEZ  .it : return it.rsrc;

      tagged BGTZ  .it : return it.rsrc;

      tagged BLTZ  .it : return it.rsrc;

      tagged BGEZ  .it : return it.rsrc;

      tagged BEQ   .it : return it.rsrc1;

      tagged BNE   .it : return it.rsrc1;
      
      // -- Jumps -----------------------------------------------------
      
      tagged J     .it : return ?;
      
      tagged JR    .it : return it.rsrc;

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
      tagged SLLV  .it : return it.rshamt;
      tagged SRLV  .it : return it.rshamt;
      tagged SRAV  .it : return it.rshamt;

      tagged BEQ   .it : return it.rsrc2;

      tagged BNE   .it : return it.rsrc2;
      default:           return ?;
    endcase;
  endfunction
 
  
  function Maybe#(RName) getDest(Inst i);
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

      tagged JAL   .it : return False; //Don't snapshot JAL instructions

      tagged JALR  .it : return False;
      default:           return False;
    endcase;
  endfunction
  
  rule handleDecode (True);
  
    debug_rule("handleDecode");
    
    Tuple3#(Token, Tuple2#(Addr, PackedInst), void) 
    //...
    tup = link_dec.getReq();
    link_dec.deq();
    
    match {.t, {.a, .pinst}, .*} = tup;
    
    Inst inst = bitsToInst(pinst);
    //Get the architectural dest/sources
    RName ara = getOp1(inst);
    RName arb = getOp2(inst);
    Maybe#(RName) mrd = getDest(inst);
    Bool rewind = isBranch(inst);
    
    //Translate into physical registers
    link_mapping.makeReq(tuple5(mrd, t, rewind, ara, arb));
    
    waitingQ.enq(tuple3(t, a, pinst));
        
  endrule
  //handleDecode
  
  //Handles the actual decoding and register allocation
  
  rule handleResponse (True);
      
    debug_rule("handleResponse");
    
    match {.tok, .a, .pinst} = waitingQ.first();
    Inst inst = bitsToInst(pinst);
    waitingQ.deq();
    
    DepInfo depinfo = ?;
    DecodedInst decinst = ?;
    Action dbg = noAction;
    
    match {.prd, .pra, .prb} = link_mapping.getResp();
    link_mapping.deq();
    
    //Actually do the decode
    case (inst) matches
      // -- Memory Ops ------------------------------------------------      
      
      //Load Word
      tagged LW {rbase: .rb, rdest:.rd, offset: .off}: 
	begin

          decinst = DLW 
	            {
		      pbase:  pra, 
		      pdest:   prd, 
		      offset: off
		    };
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] LW dest:(R%0d/PR%0d) base: (R%0d/PR%0d) offset: 0x%h", curCC,  tok.index, rd, prd, rb, pra, off);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Valid tuple2(rb, pra), 
		      dep_src2: tagged Invalid
		    };

	end
	
      //Store Word
      tagged SW {rbase: .rb, rsrc: .rs, offset: .off}: 
        begin

          decinst = DSW
	            {
		      pbase:  pra, 
		      psrc:   prb, 
		      offset: off
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] SW src:(R%0d/PR%0d) base: (R%0d/PR%0d) offset: 0x%h", curCC,  tok.index, rs, prb, rb, pra, off);
	  
          depinfo = DepInfo 
	            {
		      dep_dest: tagged Invalid, 
		      dep_src1: tagged Valid tuple2(rb, pra), 
		      dep_src2: tagged Valid tuple2(rs, prb)
		    };
        end

      // -- Simple Ops ------------------------------------------------      

      //Add Immediate Signed (Not a Typo)
      tagged ADDIU {rsrc: .rs, rdest: .rd, imm: .simm}:
        begin
	  	  
          decinst = DADDIU 
	            {
		      pdest:  prd, 
		      psrc:  pra, 
		      imm:   simm
		    };

          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] ADDIU dest:(R%0d/PR%0d) src1: (R%0d/PR%0d) imm: 0x%h", curCC,  tok.index, rd, prd, rs, pra, simm);
		    
          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
        end
	
      //Set Less Than Immediate (Signed)
      tagged SLTI {rsrc: .rs, rdest: .rd, imm: .simm}:
        begin
	  
          decinst = DSLTI 
	            {
		      pdest:  prd, 
		      psrc:  pra, 
		      imm:   simm
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] SLTI dest:(R%0d/PR%0d) src: (R%0d/PR%0d) imm: 0x%h", curCC,  tok.index, rd, prd, rs, pra, simm);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
	end
	
      //Set Less Than Immediate Unsigned 
      tagged SLTIU {rsrc: .rs, rdest: .rd, imm:.simm}:
        begin
	  
          decinst = DSLTIU
	            {
		      pdest:  prd, 
		      psrc:  pra, 
		      imm:   simm //Immediate is still sign extended
		                  //Not a typo: Exec handles it differently
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] SLTIU dest:(R%0d/PR%0d) src: (R%0d/PR%0d) imm: 0x%h", curCC,  tok.index, rd, prd, rs, pra, simm);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
	end
	
      //And Immediate
      tagged ANDI {rsrc: .rs, rdest: .rd, imm:.zimm}:
        begin
	  
          decinst = DANDI 
	            {
		      pdest:  prd, 
		      psrc:  pra, 
		      imm:   zimm
		    };
		
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] ANDI dest:(R%0d/PR%0d) src: (R%0d/PR%0d) imm: 0x%h", curCC,  tok.index, rd, prd, rs, pra, zimm);
    
          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
	end
	
      //Or Immediate
      tagged ORI {rsrc: .rs, rdest: .rd, imm:.zimm}:
        begin
	  
          decinst = DORI 
	            {
		      pdest:  prd, 
		      psrc:  pra, 
		      imm:   zimm
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] ORI dest:(R%0d/PR%0d) src: (R%0d/PR%0d) imm: 0x%h", curCC,  tok.index, rd, prd, rs, pra, zimm);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
	end
	
      //XOR Immediate
      tagged XORI {rsrc: .rs, rdest: .rd, imm:.zimm}:
        begin
	  
          decinst = DXORI 
	            {
		      pdest:  prd, 
		      psrc:  pra, 
		      imm:   zimm
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] XORI dest:(R%0d/PR%0d) src: (R%0d/PR%0d) imm: 0x%h", curCC,  tok.index, rd, prd, rs, pra, zimm);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
	end
	
      //Load Unsigned Immediate (Really is unsigned)
      tagged LUI {rdest: .rd, imm:.zimm}:
        begin
	  
          decinst = DLUI 
	            {
		      pdest:  prd, 
		      imm:   zimm
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] LUI dest:(R%0d/PR%0d) imm: 0x%h", curCC,  tok.index, rd, prd, zimm);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Invalid,
		      dep_src2: tagged Invalid
		    };
	end
	
      //Shift Left Logical (Immediate)
      tagged SLL {rsrc: .rs, rdest: .rd, shamt:.sha}: 
        begin
	  
          decinst = DSLL 
	            {
		      pdest:  prd, 
		      psrc:  pra, 
		      shamt: sha
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] SLL dest:(R%0d/PR%0d) src: (R%0d/PR%0d) sha: 0x%h", curCC,  tok.index, rd, prd, rs, pra, sha);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
	end
	
      //Shift Right Logical (Immediate)
      tagged SRL {rsrc: .rs, rdest: .rd, shamt:.sha}: 
        begin
	  
          decinst = DSRL
	            {
		      pdest:  prd, 
		      psrc:  pra, 
		      shamt: sha
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] SRL dest:(R%0d/PR%0d) src: (R%0d/PR%0d) sha: 0x%h", curCC,  tok.index, rd, prd, rs, pra, sha);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
	end
	
      //Shift Right Arithmatic (Immediate)
      tagged SRA {rsrc: .rs, rdest: .rd, shamt:.sha}: 
        begin
	  
          decinst = DSRA
	            {
		      pdest:  prd, 
		      psrc:  pra, 
		      shamt: sha
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] SRA dest:(R%0d/PR%0d) src: (R%0d/PR%0d) sha: 0x%h", curCC,  tok.index, rd, prd, rs, pra, sha);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
	end
	
      //Shift Left Logical Variable
      tagged SLLV {rsrc: .rs, rdest: .rd, rshamt:.rsha}: 
        begin
	  
          decinst = DSLLV 
	            {
		      pdest:   prd, 
		      psrc:   pra, 
		      pshamt: prb
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] SLLV dest:(R%0d/PR%0d) src: (R%0d/PR%0d) rsha: (R%0d/PR%0d)", curCC,  tok.index, rd, prd, rs, pra, rsha, prb);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Valid tuple2(rsha, prb)
		    };
	end
	
      //Shift Right Logical Variable
      tagged SRLV {rsrc: .rs, rdest: .rd, rshamt:.rsha}: 
        begin
	  
          decinst = DSRLV 
	            {
		      pdest:   prd, 
		      psrc:   pra, 
		      pshamt: prb
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] SRLV dest:(R%0d/PR%0d) src: (R%0d/PR%0d) rsha: (R%0d/PR%0d)", curCC,  tok.index, rd, prd, rs, pra, rsha, prb);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Valid tuple2(rsha, prb)
		    };
	end
	
      //Shift Right Arithmatic Variable
      tagged SRAV {rsrc: .rs, rdest: .rd, rshamt:.rsha}: 
        begin
	  
          decinst = DSRAV 
	            {
		      pdest:   prd, 
		      psrc:   pra, 
		      pshamt: prb
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] SRAV dest:(R%0d/PR%0d) src: (R%0d/PR%0d) rsha: (R%0d/PR%0d)", curCC,  tok.index, rd, prd, rs, pra, rsha, prb);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Valid tuple2(rsha, prb)
		    };
	end
	
      //Add Unsigned
      tagged ADDU {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}: 
        begin
	  
          decinst = DADDU
	            {
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] ADDU dest:(R%0d/PR%0d) src1: (R%0d/PR%0d) src2: (R%0d/PR%0d)", curCC,  tok.index, rd, prd, rs1, pra, rs2, prb);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd,  prd), 
		      dep_src1: tagged Valid tuple2(rs1, pra), 
		      dep_src2: tagged Valid tuple2(rs2, prb)
		    };
	end

      //Subtract Unsigned
      tagged SUBU {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}:
        begin
	  
          decinst = DSUBU 
	            {
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] SUBU dest:(R%0d/PR%0d) src1: (R%0d/PR%0d) src2: (R%0d/PR%0d)", curCC,  tok.index, rd, prd, rs1, pra, rs2, prb);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd,  prd), 
		      dep_src1: tagged Valid tuple2(rs1, pra), 
		      dep_src2: tagged Valid tuple2(rs2, prb)
		    };
	end
	
      //And
      tagged AND {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}:
        begin

          decinst = DAND
	            {
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] AND dest:(R%0d/PR%0d) src1: (R%0d/PR%0d) src2: (R%0d/PR%0d)", curCC,  tok.index, rd, prd, rs1, pra, rs2, prb);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd,  prd), 
		      dep_src1: tagged Valid tuple2(rs1, pra), 
		      dep_src2: tagged Valid tuple2(rs2, prb)
		    };
	end
      
      //OR
      tagged OR {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}:
        begin
	  
          decinst = DOR
	            {
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] OR dest:(R%0d/PR%0d) src1: (R%0d/PR%0d) src2: (R%0d/PR%0d)", curCC,  tok.index, rd, prd, rs1, pra, rs2, prb);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd,  prd), 
		      dep_src1: tagged Valid tuple2(rs1, pra), 
		      dep_src2: tagged Valid tuple2(rs2, prb)
		    };
	end
	
      //XOR
      tagged XOR {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}:
        begin
	  
          decinst = DXOR
	            {
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] XOR dest:(R%0d/PR%0d) src1: (R%0d/PR%0d) src2: (R%0d/PR%0d)", curCC,  tok.index, rd, prd, rs1, pra, rs2, prb);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd,  prd), 
		      dep_src1: tagged Valid tuple2(rs1, pra), 
		      dep_src2: tagged Valid tuple2(rs2, prb)
		    };
	end

      //NOR
      tagged NOR {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}:
        begin
	  
          decinst = DNOR
	            {
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] NOR dest:(R%0d/PR%0d) src1: (R%0d/PR%0d) src2: (R%0d/PR%0d)", curCC,  tok.index, rd, prd, rs1, pra, rs2, prb);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd,  prd), 
		      dep_src1: tagged Valid tuple2(rs1, pra), 
		      dep_src2: tagged Valid tuple2(rs2, prb)
		    };
	end

      //Set Less Than
      tagged SLT {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}: 
        begin
	  
          decinst = DSLT
	            {
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] SLT dest:(R%0d/PR%0d) src1: (R%0d/PR%0d) src2: (R%0d/PR%0d)", curCC,  tok.index, rd, prd, rs1, pra, rs2, prb);
          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd,  prd), 
		      dep_src1: tagged Valid tuple2(rs1, pra), 
		      dep_src2: tagged Valid tuple2(rs2, prb)
		    };
	end
      
      //Set Less Than Unsigned
      tagged SLTU {rsrc1: .rs1, rsrc2: .rs2, rdest: .rd}: 
        begin
	  
          decinst = DSLTU
	            {
		      pdest:   prd, 
		      psrc1:  pra,
		      psrc2:  prb
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] SLTU dest:(R%0d/PR%0d) src1: (R%0d/PR%0d) src2: (R%0d/PR%0d)", curCC,  tok.index, rd, prd, rs1, pra, rs2, prb);
          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd,  prd), 
		      dep_src1: tagged Valid tuple2(rs1, pra), 
		      dep_src2: tagged Valid tuple2(rs2, prb)
		    };
	end

      //Move To Coprocessor 0
      tagged MTC0 {rsrc: .rs, cop0dest: .op}:
        begin
	 
          decinst = DMTC0
	            {
		      psrc:    pra,
		      cop0dest: op
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] MTC0 src: (R%0d/PR%0d) cop0dest: %0d", curCC,  tok.index, rs, pra, op);
          depinfo = DepInfo 
	            {
		      dep_dest: tagged Invalid, 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
	end
      
      //Move From Coprocessor 0
      tagged MFC0 {rdest:.rd, cop0src:.op}:
        begin 
	
          decinst = DMFC0
	            {
		      pdest:    prd,
		      cop0src: op
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] MFC0 dest: (R%0d/PR%0d) cop0src: %0d", curCC,  tok.index, rd, prd, op);
          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Invalid, 
		      dep_src2: tagged Invalid
		    };
	end


      // -- Branches --------------------------------------------------
      
      //Branch if Less-Than or Equal to Zero
      tagged BLEZ {rsrc: .rs, offset: .off}:
        begin
	
          decinst = DBLEZ
	            {
		      psrc:   pra,
		      offset: off
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] BLEZ src: (R%0d/PR%0d) off: 0x%h", curCC,  tok.index, rs, pra, off);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Invalid, 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
        end

      //Branch if Greater Than Zero
      tagged BGTZ {rsrc: .rs, offset: .off}: 
        begin
	
          decinst = DBGTZ
	            {
		      psrc:   pra,
		      offset: off
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] BGTZ src: (R%0d/PR%0d) off: 0x%h", curCC,  tok.index, rs, pra, off);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Invalid, 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
        end

      //Branch if Less Than Zero
      tagged BLTZ {rsrc: .rs, offset: .off}: 
        begin
	
          decinst = DBLTZ
	            {
		      psrc:   pra,
		      offset: off
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] BLTZ src: (R%0d/PR%0d) off: 0x%h", curCC,  tok.index, rs, pra, off);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Invalid, 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
        end

      //Branch if Greater than or Equal to Zero
      tagged BGEZ {rsrc: .rs, offset: .off}: 
        begin
	
          decinst = DBGEZ
	            {
		      psrc:   pra,
		      offset: off
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] BGEZ src: (R%0d/PR%0d) off: 0x%h", curCC,  tok.index, rs, pra, off);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Invalid, 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
        end

      //Branch if Equal
      tagged BEQ {rsrc1: .rs1, rsrc2: .rs2, offset: .off}: 
        begin
	
          decinst = DBEQ
	            {
		      psrc1:  pra,
		      psrc2:  prb,
		      offset: off
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] BEQ src1: (R%0d/PR%0d) src2: (R%0d/PR%0d) off: 0x%h", curCC,  tok.index, rs1, pra, rs2, prb, off);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Invalid, 
		      dep_src1: tagged Valid tuple2(rs1, pra), 
		      dep_src2: tagged Valid tuple2(rs2, prb)
		    };
        end

      //Branch if Not Equal
      tagged BNE {rsrc1: .rs1, rsrc2: .rs2, offset: .off}: 
        begin
	
          decinst = DBNE
	            {
		      psrc1:  pra,
		      psrc2:  prb,
		      offset: off
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] BNE src1: (R%0d/PR%0d) src2: (R%0d/PR%0d) off: 0x%h", curCC,  tok.index, rs1, pra, rs2, prb, off);
		    
          depinfo = DepInfo 
	            {
		      dep_dest: tagged Invalid, 
		      dep_src1: tagged Valid tuple2(rs1, pra), 
		      dep_src2: tagged Valid tuple2(rs2, prb)
		    };
        end
      
      // -- Jumps -----------------------------------------------------

      //Jump
      tagged J {target: .targ}: 
        begin
	
          decinst = DJ
	            {
		      target: targ
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] J target: 0x%h", curCC,  tok.index, targ);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Invalid, 
		      dep_src1: tagged Invalid, 
		      dep_src2: tagged Invalid
		    };
        end
      
      //Jump Register
      tagged JR {rsrc: .rs}:
        begin
	
          decinst = DJR
	            {
		      psrc:   pra
		    };
		    		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] JR src: (R%0d/PR%0d) ", curCC,  tok.index, rs, pra);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Invalid, 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
        end
      //Jump and Link (into archictectural register 31)
      tagged JAL {target: .targ}: 
        begin
	
          decinst = DJAL
	            {
		      pdest:   prd, 
		      target: targ
		    };
		 		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] JAL dest: (R31/PR%0d) target: 0x%h", curCC,  tok.index, prd, targ);
    
          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(5'd31, prd), 
		      dep_src1: tagged Invalid, 
		      dep_src2: tagged Invalid
		    };
        end

      //Jump and Link into Register
      tagged JALR {rsrc: .rs, rdest: .rd}: 
        begin
	  
          decinst = DJALR
	            {
		      pdest:   prd,
		      psrc:   pra
		    };
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] JALR dest: (R%0d/PR%0d) src: (R%0d/PR%0d)", curCC,  tok.index, rd, prd, rs, pra);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Valid tuple2(rd, prd), 
		      dep_src1: tagged Valid tuple2(rs, pra), 
		      dep_src2: tagged Invalid
		    };
        end

       // -- Illegal ---------------------------------------------------
 
      tagged TERMINATE: 
        begin
          decinst = DTERMINATE;
   
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] TERMINATE", curCC,  tok.index);

          depinfo = DepInfo 
	            {
		      dep_dest: tagged Invalid, 
		      dep_src1: tagged Invalid, 
		      dep_src2: tagged Invalid
		    };
        end

      default: 
        begin
          decinst = DILLEGAL;
		    
          dbg = $fdisplay(debug_log, "[%d]: DEC: [%d] ILLEGAL", curCC,  tok.index);
	  
          depinfo = DepInfo 
	            {
		      dep_dest: tagged Invalid, 
		      dep_src1: tagged Invalid, 
		      dep_src2: tagged Invalid
		    };
        end

    endcase
    
    debug(2, dbg);
    if (prd == 0)
      $fdisplay(debug_log, "[%d]: DEC: WARNING [%d]: Got assigned PR0 as destination!", curCC, tok.index);
      
    link_dec.makeResp(tuple3(tok, depinfo, tuple2(a, decinst)));
    
  endrule
  
endmodule

