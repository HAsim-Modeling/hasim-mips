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
// Execute Unit                                                            //
//-------------------------------------------------------------------------//
// 

module [Module] mkSMIPS_Execute#(BypassUnit#(SMIPS_RName, SMIPS_PRName, SMIPS_Value, SMIPS_Token, SMIPS_SnapshotPtr) bypass) ();

  //Ports
  Connection_Server#(Tuple3#(Token, Tuple2#(Addr, DecodedInst), void),
                     Tuple3#(Token, InstResult, ExecedInst)) 
  //...
  link_exe <- mkConnection_Server("link_exe");
  
  Connection_Client#(PRName, Maybe#(Value)) 
  //...
        link_read1 <- mkConnection_Client("exe_to_bypass_read1");

  Connection_Client#(PRName, Maybe#(Value)) 
  //...
        link_read2 <- mkConnection_Client("exe_to_bypass_read2");

  Connection_Send#(Tuple2#(PRName, Value)) 
  //...
        link_write1 <- mkConnection_Send("exe_to_bypass_write1");

  //State elements
  FIFO#(Tuple3#(Token, Tuple2#(Addr, DecodedInst), void)) 
  //...
  waitingQ <- mkFIFO();

  
  function PRName getSrc1(DecInst i);
     return case ( i ) matches

      // -- Memory Ops ------------------------------------------------      

      tagged DLW .it : return it.pbase;

      tagged DSW .it : return it.pbase;

      // -- Simple Ops ------------------------------------------------      

      tagged ADDIU .it : return it.psrc;
      tagged SLTI  .it : return it.psrc;
      tagged SLTIU .it : return it.psrc;
      tagged ANDI  .it : return it.psrc;
      tagged ORI   .it : return it.psrc;
      tagged XORI  .it : return it.psrc;
      tagged LUI   .it : return ?;

      tagged SLL   .it : return it.psrc;
      tagged SRL   .it : return it.psrc;
      tagged SRA   .it : return it.psrc;
      tagged SLLV  .it : return it.psrc;
      tagged SRLV  .it : return it.psrc;
      tagged SRAV  .it : return it.psrc;
      tagged ADDU  .it : return it.psrc1;
      tagged SUBU  .it : return it.psrc1;
      tagged AND   .it : return it.psrc1;
      tagged OR    .it : return it.psrc1;
      tagged XOR   .it : return it.psrc1;
      tagged NOR   .it : return it.psrc1;
      tagged SLT   .it : return it.psrc1;
      tagged SLTU  .it : return it.psrc1;

      //tagged MTC0  .it : return it.psrc;
      //tagged MFC0  .it : return ?;

      // -- Branches --------------------------------------------------

      tagged BLEZ  .it : return it.psrc;

      tagged BGTZ  .it : return it.psrc;

      tagged BLTZ  .it : return it.psrc;

      tagged BGEZ  .it : return it.psrc;

      tagged BEQ   .it : return it.psrc1;

      tagged BNE   .it : return it.psrc1;
      
      // -- Jumps -----------------------------------------------------
      
      tagged J     .it : return ?;
      
      tagged JR    .it : return ?;

      tagged JAL   .it : return ?;

      tagged JALR  .it : return  it.psrc;
      default:           return ?;
    endcase;
  endfunction

  function PRName getSrc2(DecInst i);
    return case ( i ) matches

      tagged SW    .it : return it.psrc;
      tagged ADDU  .it : return it.psrc2;
      tagged SUBU  .it : return it.psrc2;
      tagged AND   .it : return it.psrc2;
      tagged OR    .it : return it.psrc2;
      tagged XOR   .it : return it.psrc2;
      tagged NOR   .it : return it.psrc2;
      tagged SLT   .it : return it.psrc2;
      tagged SLTU  .it : return it.psrc2;

      tagged BEQ   .it : return it.psrc2;

      tagged BNE   .it : return it.psrc2;
      default:           return ?;
    endcase;
  endfunction
  //handleExec
  
  //We can't always exec right away, since our operands may not be available.
   
  rule handleExec (True);
  
    debug_rule("handleExec");

    let tup <- link_exe.getReq();
    match {.t, {.addr, .dec}, .*} = tup;

    PRName va = getSrc1(dec);
    PRName vb = getSrc2(dec);

    //Try to get the values from the Bypass unit
    link_read1.makeReq(va);
    link_read2.makeReq(vb);

    waitingQ.enq(tup);

  endrule
  
  //execute

  rule execute (True);
  
    debug_rule("execute");

    match {.t, {.addr, .dec}, .*} = waitingQ.first();

    //Try to get the values from the Bypass unit
    Maybe#(Value) mva <- link_read1.getResp();
    Maybe#(Value) mvb <- link_read2.getResp();

    debug_rule("execute");

    match {.t, {.addr, .dec}, .*} = waitingQ.first();

    SMIPS_InstResult res = ?;
    SMIPS_ExecedInst einst = ?;
    Bool done = False;
    Maybe#(SMIPS_Addr) branchResult = Invalid;

    //Actually do the execute
    case (dec) matches
      // -- Memory Ops ------------------------------------------------      
      
      //Load Word
      tagged DLW {pbase: .rb, pdst: .rd, offset: .off, opdst: .opd}: 
	begin
	
          done  = isJust(mva);
	  res   = RNop;
	  einst = ELoad 
	          {
		    addr:   unJust(mva) + signExtend(off), 
		    pdst:   rd, 
		    opdst:  opd
		  };
	end
	
      //Store Word
      tagged DSW {pbase: .rb, psrc: .rs, offset: .off, opdst: .opd}: 
	begin
	
          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  einst = EStore
	          {
		    addr:   unJust(mva) + signExtend(off), 
		    val:    unJust(mva), 
		    opdst:  opd
		  };
	end

      // -- Simple Ops ------------------------------------------------      

      //Add Immediate Unsigned 
      //Actually the numbers are sign extended, it just can't overflow
      tagged DADDIU {psrc: .rs, pdst: .rd, imm:.simm, opdst: .opd}: 
	begin
	
          done  = isJust(mva);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mva) + signExtend(simm),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Set Less Than Immediate (Signed)
      tagged DSLTI {psrc: .rs, pdst: .rd, imm:.simm, opdst: .opd}: 
	begin
	
          done  = isJust(mva);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   zeroExtend(pack(signedLT(unJust(mva), signExtend(simm)))),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Set Less Than Immediate Unsigned 
      tagged DSLTIU {psrc: .rs, pdst: .rd, imm:.simm, opdst: .opd}: 
	begin
	
          done  = isJust(mva);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   zeroExtend(pack(unJust(mva) < signExtend(simm))),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //And Immediate
      tagged DANDI {psrc: .rs, pdst: .rd, imm:.zimm, opdst: .opd}: 
	begin
	
          done  = isJust(mva);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mva) & zeroExtend(zimm),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Or Immediate
      tagged DORI {psrc: .rs, pdst: .rd, imm:.zimm, opdst: .opd}: 
	begin
	
          done  = isJust(mva);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mva) | zeroExtend(zimm),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //XOR Immediate
      tagged DXORI {psrc: .rs, pdst: .rd, imm:.zimm, opdst: .opd}: 
	begin
	
          done  = isJust(mva);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mva) ^ zeroExtend(zimm),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
	
      //Load Unsigned Immediate (Really is unsigned)
      tagged DLUI {pdst: .rd, imm:.zimm, opdst: .opd}: 
	begin

          done  = True;
	  res   = RNop;
	  einst = EWB
	          {
		    val:   zeroExtend(zimm) << 16,
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
	
      //Shift Left Logical (Immediate)
      tagged DSLL {psrc: .rs, pdst: .rd, shamt:.sha, opdst: .opd}: 
	begin

          done  = isJust(mva);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mva) << sha,
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Shift Right Logical (Immediate)
      tagged DSRL {psrc: .rs, pdst: .rd, shamt:.sha, opdst: .opd}: 
	begin

          done  = isJust(mva);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mva) >> sha,
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Shift Right Arithmatic (Immediate)
      tagged DSRA {psrc: .rs, pdst: .rd, shamt:.sha, opdst: .opd}: 
	begin

          done  = isJust(mva);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   signedShiftRight(unJust(mva), sha),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Shift Left Logical Variable
      tagged DSLLV {psrc: .rs, pdst: .rd, pshamt:.rsha, opdst: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mva) << unJust(mvb)[4:0],
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Shift Right Logical Variable
      tagged DSRLV {psrc: .rs, pdst: .rd, pshamt:.rsha, opdst: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mva) >> unJust(mvb)[4:0],
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Shift Right Arithmatic Variable
      tagged DSRAV {psrc: .rs, pdst: .rd, pshamt:.rsha, opdst: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   signedShiftRight(unJust(mva), unJust(mvb)[4:0]),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //Add Unsigned
      tagged DADDU {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mva) + unJust(mvb),
		    pdst:  rd,
		    opdst: opd
		  };
	end

      //Subtract Unsigned
      tagged DSUBU {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mva) - unJust(mvb),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //And
      tagged DAND {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mva) & unJust(mvb),
		    pdst:  rd,
		    opdst: opd
		  };
	end
      
      //OR
      tagged DOR {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mva) | unJust(mvb),
		    pdst:  rd,
		    opdst: opd
		  };
	end
	
      //XOR
      tagged DXOR {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   unJust(mva) ^ unJust(mvb),
		    pdst:  rd,
		    opdst: opd
		  };
	end

      //NOR
      tagged DNOR {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   ~(unJust(mva) | unJust(mvb)),
		    pdst:  rd,
		    opdst: opd
		  };
	end

      //Set Less Than
      tagged DSLT {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   zeroExtend(pack(signedLT(unJust(mva), unJust(mvb)))),
		    pdst:  rd,
		    opdst: opd
		  };
	end
      
      //Set Less Than Unsigned
      tagged DSLTU {psrc1: .rs1, psrc2: .rs2, pdst: .rd, opdst: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  einst = EWB
	          {
		    val:   zeroExtend(pack(unJust(mva) < unJust(mvb))),
		    pdst:  rd,
		    opdst: opd
		  };
	end


      // -- Branches --------------------------------------------------
      
      //Branch if Less-Than or Equal to Zero
      tagged DBLEZ {psrc: .rs, offset: .off, opdst: .opd}: 
	begin

          Bool taken = signedLE(unJust(mva), 0);
	  SMIPS_Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mva);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end

      //Branch if Greater Than Zero
      tagged DBGTZ {psrc: .rs, offset: .off, opdst: .opd}: 
	begin
	
          Bool taken = signedGT(unJust(mva), 0);
	  SMIPS_Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mva);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end

      //Branch if Less Than Zero
      tagged DBLTZ {psrc: .rs, offset: .off, opdst: .opd}: 
	begin
	
          Bool taken = signedLT(unJust(mva), 0);
	  SMIPS_Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mva);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end

      //Branch if Greater than or Equal to Zero
      tagged DBGEZ {psrc: .rs, offset: .off, opdst: .opd}: 
	begin

          Bool taken = signedGE(unJust(mva), 0);
	  SMIPS_Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mva);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end

      //Branch if Equal
      tagged DBEQ {psrc1: .rs1, psrc2: .rs2, offset: .off, opdst: .opd}: 
	begin

          Bool taken = unJust(mva) == unJust(mvb);
	  SMIPS_Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mva) && isJust(mvb);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end

      //Branch if Not Equal
      tagged DBNE {psrc1: .rs1, psrc2: .rs2, offset: .off, opdst: .opd}: 
	begin

          Bool taken = unJust(mva) != unJust(mvb);
	  SMIPS_Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mva) && isJust(mvb);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end
      
      // -- Jumps -----------------------------------------------------

      //Jump
      tagged DJ {target: .targ, opdst: .opd}: 
	begin

	  SMIPS_Addr dest  = {addr[31:28], targ, 2'b0};

          done  = True;
	  res   = RBranchTaken dest;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end
      
      //Jump Register
      tagged DJR {psrc: .rs, opdst: .opd}: 
	begin

          SMIPS_Addr dest = unJust(mva);

          done  = isJust(mva);
	  res   = RBranchTaken dest;
	  einst = ENop 
	          {
		    opdst: opd
		  };
	end

      //Jump and Link (into archictectural register 31)
      tagged DJAL {target: .targ, pdst: .rd, opdst: .opd}: 
	begin

	  SMIPS_Addr dest  = {addr[31:28], targ, 2'b0};
	  
          done  = True;
	  res   = RBranchTaken dest;
	  einst = EWB
	          {
		    val:   addr,
		    pdst:  rd,
		    opdst: opd
		  };
	end


      //Jump and Link into Register
      tagged DJALR {psrc: .rs, pdst: .rd, opdst: .opd}: 
	begin
	  
	  SMIPS_Addr dest  = unJust(mva);
	  
          done  = isJust(mva);
	  res   = RBranchTaken dest;
	  einst = EWB
	          {
		    val:   addr,
		    pdst:  rd,
		    opdst: opd
		  };
	end

       // -- Illegal ---------------------------------------------------
 
      tagged TERMINATE: 
        begin
	
	  done = True;
	  res = RTerminate;
	  einst = ENop {opdst: ?};
	  	  
        end
	
      default: 
        begin
	
	  done = True;
	  res = RNop;
	  einst = ENop {opdst: ?};
	  
	  $display("ERROR: EXECUTING ILLEGAL INSTRUCTION");
	  
        end
    endcase
      
    if (done)
      begin
	link_exe.makeResp(tuple3(t, res, einst));
	waitingQ.deq();
      end

  endrule

  
endmodule 

