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

module [HASim_Module] mkFUNCP_ExecuteAlg ();

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

  
  function PRName getSrc1(DecodedInst i);
     return case ( i ) matches

      // -- Memory Ops ------------------------------------------------      

      tagged DLW .it : return it.pbase;

      tagged DSW .it : return it.pbase;

      // -- Simple Ops ------------------------------------------------      

      tagged DADDIU .it : return it.psrc;
      tagged DSLTI  .it : return it.psrc;
      tagged DSLTIU .it : return it.psrc;
      tagged DANDI  .it : return it.psrc;
      tagged DORI   .it : return it.psrc;
      tagged DXORI  .it : return it.psrc;
      tagged DLUI   .it : return ?;

      tagged DSLL   .it : return it.psrc;
      tagged DSRL   .it : return it.psrc;
      tagged DSRA   .it : return it.psrc;
      tagged DSLLV  .it : return it.psrc;
      tagged DSRLV  .it : return it.psrc;
      tagged DSRAV  .it : return it.psrc;
      tagged DADDU  .it : return it.psrc1;
      tagged DSUBU  .it : return it.psrc1;
      tagged DAND   .it : return it.psrc1;
      tagged DOR    .it : return it.psrc1;
      tagged DXOR   .it : return it.psrc1;
      tagged DNOR   .it : return it.psrc1;
      tagged DSLT   .it : return it.psrc1;
      tagged DSLTU  .it : return it.psrc1;

      //tagged DMTC0  .it : return it.psrc;
      //tagged DMFC0  .it : return ?;

      // -- Branches --------------------------------------------------

      tagged DBLEZ  .it : return it.psrc;

      tagged DBGTZ  .it : return it.psrc;

      tagged DBLTZ  .it : return it.psrc;

      tagged DBGEZ  .it : return it.psrc;

      tagged DBEQ   .it : return it.psrc1;

      tagged DBNE   .it : return it.psrc1;
      
      // -- Jumps -----------------------------------------------------
      
      tagged DJ     .it : return ?;
      
      tagged DJR    .it : return ?;

      tagged DJAL   .it : return ?;

      tagged DJALR  .it : return  it.psrc;
      default:           return ?;
    endcase;
  endfunction

  function PRName getSrc2(DecodedInst i);
    return case ( i ) matches

      tagged DSW    .it : return it.psrc;
      tagged DADDU  .it : return it.psrc2;
      tagged DSUBU  .it : return it.psrc2;
      tagged DAND   .it : return it.psrc2;
      tagged DOR    .it : return it.psrc2;
      tagged DXOR   .it : return it.psrc2;
      tagged DNOR   .it : return it.psrc2;
      tagged DSLT   .it : return it.psrc2;
      tagged DSLTU  .it : return it.psrc2;

      tagged DBEQ   .it : return it.psrc2;

      tagged DBNE   .it : return it.psrc2;
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

    InstResult res = ?;
    ExecedInst einst = ?;
    Value wbval = ?;
    Bool done = False;
    Maybe#(Addr) branchResult = Invalid;

    //Actually do the execute
    case (dec) matches
      // -- Memory Ops ------------------------------------------------      
      
      //Load Word
      tagged DLW {pbase: .rb, pdest: .rd, offset: .off, opdest: .opd}: 
	begin
	
          done  = True;
	  res   = RNop;
	  einst = ELoad 
	          {
		    idx:     rb,
		    offset:  off,
		    pdest:   rd, 
		    opdest:   opd
		  };
	end
	
      //Store Word
      tagged DSW {pbase: .rb, psrc: .rs, offset: .off, opdest: .opd}: 
	begin
	
          done  = True;
	  res   = RNop;
	  einst = EStore
	          {
		    idx:    rb,
		    val:    rs,
		    offset: off,
		    opdest:  opd
		  };
	end

      // -- Simple Ops ------------------------------------------------      

      //Add Immediate Unsigned 
      //Actually the numbers are sign extended, it just can't overflow
      tagged DADDIU {psrc: .rs, pdest: .rd, imm:.simm, opdest: .opd}: 
	begin
	
          done  = isJust(mva);
	  res   = RNop;
	  wbval = unJust(mva) + signExtend(simm);
	  einst = EWB
	          {
		    pdest:  rd,
		    opdest: opd
		  };
	end
	
      //Set Less Than Immediate (Signed)
      tagged DSLTI {psrc: .rs, pdest: .rd, imm:.simm, opdest: .opd}: 
	begin
	
          done  = isJust(mva);
	  res   = RNop;
	  wbval = zeroExtend(pack(signedLT(unJust(mva), signExtend(simm))));
	  einst = EWB
	          {
		    pdest:  rd,
		    opdest: opd
		  };
	end
	
      //Set Less Than Immediate Unsigned 
      tagged DSLTIU {psrc: .rs, pdest: .rd, imm:.simm, opdest: .opd}: 
	begin
	
          done  = isJust(mva);
	  res   = RNop;
	  wbval = zeroExtend(pack(unJust(mva) < signExtend(simm)));
	  einst = EWB
	          {
		    pdest:  rd,
		    opdest: opd
		  };
	end
	
      //And Immediate
      tagged DANDI {psrc: .rs, pdest: .rd, imm:.zimm, opdest: .opd}: 
	begin
	
          done  = isJust(mva);
	  res   = RNop;
	  wbval = unJust(mva) & zeroExtend(zimm);
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end
	
      //Or Immediate
      tagged DORI {psrc: .rs, pdest: .rd, imm:.zimm, opdest: .opd}: 
	begin
	
          done  = isJust(mva);
	  res   = RNop;
	  wbval = unJust(mva) | zeroExtend(zimm);
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end
	
      //XOR Immediate
      tagged DXORI {psrc: .rs, pdest: .rd, imm:.zimm, opdest: .opd}: 
	begin
	
          done  = isJust(mva);
	  res   = RNop;
	  wbval = unJust(mva) ^ zeroExtend(zimm);
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end
	
	
      //Load Unsigned Immediate (Really is unsigned)
      tagged DLUI {pdest: .rd, imm:.zimm, opdest: .opd}: 
	begin

          done  = True;
	  res   = RNop;
	  wbval = zeroExtend(zimm) << 16;
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end
	
	
      //Shift Left Logical (Immediate)
      tagged DSLL {psrc: .rs, pdest: .rd, shamt:.sha, opdest: .opd}: 
	begin

          done  = isJust(mva);
	  res   = RNop;
	  wbval = unJust(mva) << sha;
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end
	
      //Shift Right Logical (Immediate)
      tagged DSRL {psrc: .rs, pdest: .rd, shamt:.sha, opdest: .opd}: 
	begin

          done  = isJust(mva);
	  res   = RNop;
	  wbval = unJust(mva) >> sha;
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end
	
      //Shift Right Arithmatic (Immediate)
      tagged DSRA {psrc: .rs, pdest: .rd, shamt:.sha, opdest: .opd}: 
	begin

          done  = isJust(mva);
	  res   = RNop;
	  wbval = signedShiftRight(unJust(mva), sha);
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end
	
      //Shift Left Logical Variable
      tagged DSLLV {psrc: .rs, pdest: .rd, pshamt:.rsha, opdest: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  wbval = unJust(mva) << unJust(mvb)[4:0];
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end
	
      //Shift Right Logical Variable
      tagged DSRLV {psrc: .rs, pdest: .rd, pshamt:.rsha, opdest: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  wbval = unJust(mva) >> unJust(mvb)[4:0];
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end
	
      //Shift Right Arithmatic Variable
      tagged DSRAV {psrc: .rs, pdest: .rd, pshamt:.rsha, opdest: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  wbval = signedShiftRight(unJust(mva), unJust(mvb)[4:0]);
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end
	
      //Add Unsigned
      tagged DADDU {psrc1: .rs1, psrc2: .rs2, pdest: .rd, opdest: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  wbval = unJust(mva) + unJust(mvb);
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end

      //Subtract Unsigned
      tagged DSUBU {psrc1: .rs1, psrc2: .rs2, pdest: .rd, opdest: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  wbval = unJust(mva) - unJust(mvb);
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end
	
      //And
      tagged DAND {psrc1: .rs1, psrc2: .rs2, pdest: .rd, opdest: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  wbval = unJust(mva) & unJust(mvb);
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end
      
      //OR
      tagged DOR {psrc1: .rs1, psrc2: .rs2, pdest: .rd, opdest: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  wbval = unJust(mva) | unJust(mvb);
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end
	
      //XOR
      tagged DXOR {psrc1: .rs1, psrc2: .rs2, pdest: .rd, opdest: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  wbval = unJust(mva) ^ unJust(mvb);
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end

      //NOR
      tagged DNOR {psrc1: .rs1, psrc2: .rs2, pdest: .rd, opdest: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  wbval = ~(unJust(mva) | unJust(mvb));
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end

      //Set Less Than
      tagged DSLT {psrc1: .rs1, psrc2: .rs2, pdest: .rd, opdest: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  wbval = zeroExtend(pack(signedLT(unJust(mva), unJust(mvb))));
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end
      
      //Set Less Than Unsigned
      tagged DSLTU {psrc1: .rs1, psrc2: .rs2, pdest: .rd, opdest: .opd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = RNop;
	  wbval = zeroExtend(pack(unJust(mva) < unJust(mvb)));
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end


      // -- Branches --------------------------------------------------
      
      //Branch if Less-Than or Equal to Zero
      tagged DBLEZ {psrc: .rs, offset: .off, opdest: .opd}: 
	begin

          Bool taken = signedLE(unJust(mva), 0);
	  Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mva);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdest: opd
		  };
	end

      //Branch if Greater Than Zero
      tagged DBGTZ {psrc: .rs, offset: .off, opdest: .opd}: 
	begin
	
          Bool taken = signedGT(unJust(mva), 0);
	  Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mva);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdest: opd
		  };
	end

      //Branch if Less Than Zero
      tagged DBLTZ {psrc: .rs, offset: .off, opdest: .opd}: 
	begin
	
          Bool taken = signedLT(unJust(mva), 0);
	  Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mva);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdest: opd
		  };
	end

      //Branch if Greater than or Equal to Zero
      tagged DBGEZ {psrc: .rs, offset: .off, opdest: .opd}: 
	begin

          Bool taken = signedGE(unJust(mva), 0);
	  Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mva);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdest: opd
		  };
	end

      //Branch if Equal
      tagged DBEQ {psrc1: .rs1, psrc2: .rs2, offset: .off, opdest: .opd}: 
	begin

          Bool taken = unJust(mva) == unJust(mvb);
	  Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mva) && isJust(mvb);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdest: opd
		  };
	end

      //Branch if Not Equal
      tagged DBNE {psrc1: .rs1, psrc2: .rs2, offset: .off, opdest: .opd}: 
	begin

          Bool taken = unJust(mva) != unJust(mvb);
	  Addr dest  = addr + (signExtend(off) << 2);

          done  = isJust(mva) && isJust(mvb);
	  res   = taken ? (RBranchTaken dest) : RBranchNotTaken;
	  einst = ENop 
	          {
		    opdest: opd
		  };
	end
      
      // -- Jumps -----------------------------------------------------

      //Jump
      tagged DJ {target: .targ, opdest: .opd}: 
	begin

	  Addr dest  = {addr[31:28], targ, 2'b0};

          done  = True;
	  res   = RBranchTaken dest;
	  einst = ENop 
	          {
		    opdest: opd
		  };
	end
      
      //Jump Register
      tagged DJR {psrc: .rs, opdest: .opd}: 
	begin

          Addr dest = unJust(mva);

          done  = isJust(mva);
	  res   = RBranchTaken dest;
	  einst = ENop 
	          {
		    opdest: opd
		  };
	end

      //Jump and Link (into archictectural register 31)
      tagged DJAL {target: .targ, pdest: .rd, opdest: .opd}: 
	begin

	  Addr dest  = {addr[31:28], targ, 2'b0};
	  
          done  = True;
	  res   = RBranchTaken dest;
	  wbval = addr;
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end


      //Jump and Link into Register
      tagged DJALR {psrc: .rs, pdest: .rd, opdest: .opd}: 
	begin
	  
	  Addr dest  = unJust(mva);
	  
          done  = isJust(mva);
	  res   = RBranchTaken dest;
	  wbval = addr;
          einst = EWB
                  {
		    pdest:  rd,
		    opdest: opd
		  };
	end

       // -- Illegal ---------------------------------------------------
 
      tagged DTERMINATE: 
        begin
	
	  done = True;
	  res = RTerminate;
	  einst = ENop {opdest: ?};
	  	  
        end
	
      default: 
        begin
	
	  done = True;
	  res = RNop;
	  einst = ENop {opdest: ?};
	  
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

