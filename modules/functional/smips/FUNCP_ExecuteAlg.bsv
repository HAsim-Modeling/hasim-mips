import GetPut::*;
import ClientServer::*;
import Connectable::*;
import RegFile::*;
import FIFO::*;
import Vector::*;

import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import hasim_funcp_base::*;
import hasim_isa::*;


`ifdef PARTITION_NAME
`undef PARTITION_NAME
`endif

`define PARTITION_NAME "Functional"

//-------------------------------------------------------------------------//
// Execute Unit                                                            //
//-------------------------------------------------------------------------//
// 

module [HASim_Module] mkFUNCP_ExecuteAlg ();

  Reg#(Bool) waiting <- mkReg(False);

  //Ports
  Connection_Server#(Tuple3#(Token, Tuple2#(Addr, DecodedInst), void),
                     Tuple3#(Token, InstResult, ExecedInst)) 
  //...
  link_exe <- mkConnection_Server("fp_exe_stage");
  
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

      tagged DMTC0  .it : return it.psrc;
      tagged DMFC0  .it : return ?;

      // -- Branches --------------------------------------------------

      tagged DBLEZ  .it : return it.psrc;

      tagged DBGTZ  .it : return it.psrc;

      tagged DBLTZ  .it : return it.psrc;

      tagged DBGEZ  .it : return it.psrc;

      tagged DBEQ   .it : return it.psrc1;

      tagged DBNE   .it : return it.psrc1;
      
      // -- Jumps -----------------------------------------------------
      
      tagged DJ     .it : return ?;
      
      tagged DJR    .it : return it.psrc;

      tagged DJAL   .it : return ?;

      tagged DJALR  .it : return it.psrc;
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
      tagged DSLLV  .it : return it.pshamt;
      tagged DSRLV  .it : return it.pshamt;
      tagged DSRAV  .it : return it.pshamt;

      tagged DBEQ   .it : return it.psrc2;

      tagged DBNE   .it : return it.psrc2;
      default:           return ?;
    endcase;
  endfunction
  //handleExec
  
  //We can't always exec right away, since our operands may not be available.
   
  rule handleExec (True);
  
    debug_rule("handleExec");

    match {.tok, {.addr, .dec}, .*} <- link_exe.getReq();
    
    //Might as well add 4 to PC now.
    Addr   addr_plus_4 = addr + 4;

    waitingQ.enq(tuple3(tok, tuple2(addr_plus_4, dec), ?));
    
  endrule
  
  rule makeReq (!waiting);
  
    match {.t, {.addr, .dec}, .*} = waitingQ.first();

    PRName va = getSrc1(dec);
    PRName vb = getSrc2(dec);

    //Try to get the values from the Bypass unit
    link_read1.makeReq(va);
    link_read2.makeReq(vb);

    waiting <= True;

  endrule
  
  //execute

  rule execute (waiting);
  
    debug_rule("execute");

    match {.tok, {.addr_plus_4, .dec}, .*} = waitingQ.first();

    //Try to get the values from the Bypass unit
    Maybe#(Value) mva <- link_read1.getResp();
    Maybe#(Value) mvb <- link_read2.getResp();

    InstResult res = ?;
    ExecedInst einst = ?;
    Value wbval = ?;
    Bool done = False;
    Action dbg = noAction;

    //Actually do the execute
    case (dec) matches
      // -- Memory Ops ------------------------------------------------      
      
      //Load Word
      tagged DLW {pbase: .rb, pdest: .rd, offset: .off}: 
	begin
	
          done  = isJust(mva);
	  Addr extimm = signExtend(off);
	  Addr ea = unJust(mva) + extimm;
	  res   = tagged RNop;
	  dbg = $display("EXE: [%d] LW PR%d <= MEM[0x%h = 0x%h + 0x%h]", tok.index, rd, ea, unJust(mva), extimm);
	  einst = ELoad 
	          {
		    addr:  ea,
		    pdest: rd 
		  };
	end
	
      //Store Word
      tagged DSW {pbase: .rb, psrc: .rs, offset: .off}: 
	begin
	
          done  = isJust(mva) && isJust(mvb);
	  Addr extimm = signExtend(off);
	  Value ea = unJust(mva) + extimm;
	  res   = tagged RNop;
	  dbg = $display("EXE: [%d] SW MEM[0x%h = 0x%h + 0x%h] <= 0x%h", tok.index, ea, unJust(mva), extimm, unJust(mvb));
	  einst = EStore
	          {
		    addr: ea,
		    val:  unJust(mvb)
		  };
	end

      // -- Simple Ops ------------------------------------------------      

      //Add Immediate Unsigned 
      //Actually the numbers are sign extended, it just can't overflow
      tagged DADDIU {psrc: .rs, pdest: .rd, imm:.simm}: 
	begin
	
          done  = isJust(mva);
	  res   = tagged RNop;
	  Value extimm = signExtend(simm);
	  wbval = unJust(mva) + extimm;
	  dbg = $display("EXE: [%d] DADDIU PR%d <= 0x%h = 0x%h + 0x%h]", tok.index, rd, wbval, unJust(mva), extimm);
	  einst = EWB
	          {
		    pdest:  rd
		  };
	end
	
      //Set Less Than Immediate (Signed)
      tagged DSLTI {psrc: .rs, pdest: .rd, imm:.simm}: 
	begin
	
          done  = isJust(mva);
	  res   = tagged RNop;
	  Value extimm = signExtend(simm);
	  wbval = zeroExtend(pack(signedLT(unJust(mva), extimm)));
	  dbg = $display("EXE: [%d] DSLTI PR%d <= 0x%h = slt(0x%h, 0x%h)", tok.index, rd, wbval, unJust(mva), extimm);
	  einst = EWB
	          {
		    pdest:  rd
		  };
	end
	
      //Set Less Than Immediate Unsigned 
      tagged DSLTIU {psrc: .rs, pdest: .rd, imm:.simm}: 
	begin
	
          done  = isJust(mva);
	  res   = tagged RNop;
	  Value extimm = signExtend(simm);
	  wbval = zeroExtend(pack(unJust(mva) < extimm));
	  dbg = $display("EXE: [%d] DSLTIU PR%d <= 0x%h = sltu(0x%h, 0x%h)", tok.index, rd, wbval, unJust(mva), extimm);
	  einst = EWB
	          {
		    pdest:  rd
		  };
	end
	
      //And Immediate
      tagged DANDI {psrc: .rs, pdest: .rd, imm:.zimm}: 
	begin
	
          done  = isJust(mva);
	  res   = tagged RNop;
	  wbval = unJust(mva) & zeroExtend(zimm);
	  dbg = $display("EXE: [%d] DANDI PR%d <= 0x%h = 0x%h & 0x%h", tok.index, rd, wbval, unJust(mva), zimm);
          einst = EWB
                  {
		    pdest:  rd
		  };
	end
	
      //Or Immediate
      tagged DORI {psrc: .rs, pdest: .rd, imm:.zimm}: 
	begin
	
          done  = isJust(mva);
	  res   = tagged RNop;
	  wbval = unJust(mva) | zeroExtend(zimm);
	  dbg = $display("EXE: [%d] DORI PR%d <= 0x%h = 0x%h | 0x%h", tok.index, rd, wbval, unJust(mva), zimm);
          einst = EWB
                  {
		    pdest:  rd
		  };
	end
	
      //XOR Immediate
      tagged DXORI {psrc: .rs, pdest: .rd, imm:.zimm}: 
	begin
	
          done  = isJust(mva);
	  res   = tagged RNop;
	  wbval = unJust(mva) ^ zeroExtend(zimm);
	  dbg = $display("EXE: [%d] DXORI PR%d <= 0x%h = 0x%h ^ 0x%h", tok.index, rd, wbval, unJust(mva), zimm);
          einst = EWB
                  {
		    pdest:  rd
		  };
	end
	
	
      //Load Unsigned Immediate (Really is unsigned)
      tagged DLUI {pdest: .rd, imm:.zimm}: 
	begin

          done  = True;
	  res   = tagged RNop;
	  wbval = zeroExtend(zimm) << 16;
	  dbg = $display("EXE: [%d] DLUI PR%d <= 0x%h = 0x%h << 16", tok.index, rd, wbval, zimm);
          einst = EWB
                  {
		    pdest:  rd
		  };
	end
	
	
      //Shift Left Logical (Immediate)
      tagged DSLL {psrc: .rs, pdest: .rd, shamt:.sha}: 
	begin

          done  = isJust(mva);
	  res   = tagged RNop;
	  wbval = unJust(mva) << sha;
	  dbg = $display("EXE: [%d] DSLL PR%d <= 0x%h = 0x%h << 0x%h", tok.index, rd, wbval, mva, sha);
          einst = EWB
                  {
		    pdest:  rd
		  };
	end
	
      //Shift Right Logical (Immediate)
      tagged DSRL {psrc: .rs, pdest: .rd, shamt:.sha}: 
	begin

          done  = isJust(mva);
	  res   = tagged RNop;
	  wbval = unJust(mva) >> sha;
	  dbg = $display("EXE: [%d] DSRL PR%d <= 0x%h = 0x%h >> 0x%h", tok.index, rd, wbval, mva, sha);
          einst = EWB
                  {
		    pdest:  rd
		  };
	end
	
      //Shift Right Arithmatic (Immediate)
      tagged DSRA {psrc: .rs, pdest: .rd, shamt:.sha}: 
	begin

          done  = isJust(mva);
	  res   = tagged RNop;
	  wbval = signedShiftRight(unJust(mva), sha);
	  dbg = $display("EXE: [%d] DSRA PR%d <= 0x%h = 0x%h <<a 0x%h", tok.index, rd, wbval, mva, sha);
          einst = EWB
                  {
		    pdest:  rd
		  };
	end
	
      //Shift Left Logical Variable
      tagged DSLLV {psrc: .rs, pdest: .rd, pshamt:.rsha}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = tagged RNop;
	  wbval = unJust(mva) << unJust(mvb)[4:0];
	  dbg = $display("EXE: [%d] DSLLV PR%d <= 0x%h = 0x%h << 0x%h", tok.index, rd, wbval, unJust(mva), unJust(mvb)[4:0]);
          einst = EWB
                  {
		    pdest:  rd
		  };
	end
	
      //Shift Right Logical Variable
      tagged DSRLV {psrc: .rs, pdest: .rd, pshamt:.rsha}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = tagged RNop;
	  wbval = unJust(mva) >> unJust(mvb)[4:0];
	  dbg = $display("EXE: [%d] DSRLV PR%d <= 0x%h = 0x%h >> 0x%h", tok.index, rd, wbval, unJust(mva), unJust(mvb)[4:0]);
	  einst = EWB
                  {
		    pdest:  rd
		  };
	end
	
      //Shift Right Arithmatic Variable
      tagged DSRAV {psrc: .rs, pdest: .rd, pshamt:.rsha}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = tagged RNop;
	  wbval = signedShiftRight(unJust(mva), unJust(mvb)[4:0]);
	  dbg = $display("EXE: [%d] DSRAV PR%d <= 0x%h = 0x%h >>a 0x%h", tok.index, rd, wbval, unJust(mva), unJust(mvb)[4:0]);
	  einst = EWB
                  {
		    pdest:  rd
		  };
	end
	
      //Add Unsigned
      tagged DADDU {psrc1: .rs1, psrc2: .rs2, pdest: .rd}: 
	begin
	  
          done  = isJust(mva) && isJust(mvb);
	  res   = tagged RNop;
	  wbval = unJust(mva) + unJust(mvb);
	  dbg = $display("EXE: [%d] DADDU PR%d <= %0h = %0h + %0h", tok.index, rd, wbval, unJust(mva), unJust(mvb));
          einst = EWB
                  {
		    pdest:  rd
		  };
		  
	    
	end

      //Subtract Unsigned
      tagged DSUBU {psrc1: .rs1, psrc2: .rs2, pdest: .rd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = tagged RNop;
	  wbval = unJust(mva) - unJust(mvb);
	  dbg = $display("EXE: [%d] DSUBU PR%d <= %0h = %0h - %0h", tok.index, rd, wbval, unJust(mva), unJust(mvb));
          einst = EWB
                  {
		    pdest:  rd
		  };
	end
	
      //And
      tagged DAND {psrc1: .rs1, psrc2: .rs2, pdest: .rd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = tagged RNop;
	  wbval = unJust(mva) & unJust(mvb);
	  dbg = $display("EXE: [%d] DAND PR%d <= %0h = %0h & %0h", tok.index, rd, wbval, unJust(mva), unJust(mvb));
          einst = EWB
                  {
		    pdest:  rd
		  };
	end
      
      //OR
      tagged DOR {psrc1: .rs1, psrc2: .rs2, pdest: .rd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = tagged RNop;
	  wbval = unJust(mva) | unJust(mvb);
	  dbg = $display("EXE: [%d] DOR PR%d <= %0h = %0h | %0h", tok.index, rd, wbval, unJust(mva), unJust(mvb));
          einst = EWB
                  {
		    pdest:  rd
		  };
	end
	
      //XOR
      tagged DXOR {psrc1: .rs1, psrc2: .rs2, pdest: .rd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = tagged RNop;
	  wbval = unJust(mva) ^ unJust(mvb);
	  dbg = $display("EXE: [%d] DXOR PR%d <= %0h = %0h ^ %0h", tok.index, rd, wbval, unJust(mva), unJust(mvb));
          einst = EWB
                  {
		    pdest:  rd
		  };
	end

      //NOR
      tagged DNOR {psrc1: .rs1, psrc2: .rs2, pdest: .rd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = tagged RNop;
	  wbval = ~(unJust(mva) | unJust(mvb));
	  dbg = $display("EXE: [%d] DNOR PR%d <= %0h = %0h nor %0h", tok.index, rd, wbval, unJust(mva), unJust(mvb));
          einst = EWB
                  {
		    pdest:  rd
		  };
	end

      //Set Less Than
      tagged DSLT {psrc1: .rs1, psrc2: .rs2, pdest: .rd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = tagged RNop;
	  wbval = zeroExtend(pack(signedLT(unJust(mva), unJust(mvb))));
	  dbg = $display("EXE: [%d] DSLT PR%d <= %0h = slt(%0h, %0h)", tok.index, rd, wbval, unJust(mva), unJust(mvb));
          einst = EWB
                  {
		    pdest:  rd
		  };
	end
      
      //Set Less Than Unsigned
      tagged DSLTU {psrc1: .rs1, psrc2: .rs2, pdest: .rd}: 
	begin

          done  = isJust(mva) && isJust(mvb);
	  res   = tagged RNop;
	  wbval = zeroExtend(pack(unJust(mva) < unJust(mvb)));
	  dbg = $display("EXE: [%d] DSLTU PR%d <= %0h = sltu(%0h, %0h)", tok.index, rd, wbval, unJust(mva), unJust(mvb));
          einst = EWB
                  {
		    pdest:  rd
		  };
	end


      // -- Branches --------------------------------------------------
      
      //Branch if Less-Than or Equal to Zero
      tagged DBLEZ {psrc: .rs, offset: .off}: 
	begin

          Bool taken = signedLE(unJust(mva), 0);
	  Addr extimm = signExtend(off) << 2;
	  Addr dest  = addr_plus_4 + extimm;

          done  = isJust(mva);
	  res   = taken ? (tagged RBranchTaken dest) : tagged RBranchNotTaken;
	  dbg = $display("EXE: [%d] DBLEZ PC <= 0x%h (offset 0x%h) if (0x%h <= 0)", tok.index, dest, extimm, unJust(mva));
	  einst = ENop;

	end

      //Branch if Greater Than Zero
      tagged DBGTZ {psrc: .rs, offset: .off}: 
	begin
	
          Bool taken = signedGT(unJust(mva), 0);
	  Addr extimm = signExtend(off) << 2;
	  Addr dest  = addr_plus_4 + extimm;

          done  = isJust(mva);
	  res   = taken ? (tagged RBranchTaken dest) : tagged RBranchNotTaken;
	  dbg = $display("EXE: [%d] DBGTZ PC <= 0x%h (offset 0x%h) if (0x%h > 0)", tok.index, dest, extimm, unJust(mva));
	  einst = ENop;

	end

      //Branch if Less Than Zero
      tagged DBLTZ {psrc: .rs, offset: .off}: 
	begin
	
          Bool taken = signedLT(unJust(mva), 0);
	  Addr extimm = signExtend(off) << 2;
	  Addr dest  = addr_plus_4 + extimm;

          done  = isJust(mva);
	  res   = taken ? (tagged RBranchTaken dest) : tagged RBranchNotTaken;
	  dbg = $display("EXE: [%d] DBLTZ PC <= 0x%h (offset 0x%h) if (0x%h < 0)", tok.index, dest, extimm, unJust(mva));
	  einst = ENop;

	end

      //Branch if Greater than or Equal to Zero
      tagged DBGEZ {psrc: .rs, offset: .off}: 
	begin

          Bool taken = signedGE(unJust(mva), 0);
	  Addr extimm = signExtend(off) << 2;
	  Addr dest  = addr_plus_4 + extimm;

          done  = isJust(mva);
	  res   = taken ? (tagged RBranchTaken dest) : tagged RBranchNotTaken;
	  dbg = $display("EXE: [%d] DBGEZ PC <= 0x%h (offset 0x%h) if (0x%h > 0)", tok.index, dest, extimm, unJust(mva));
	  einst = ENop;

	end

      //Branch if Equal
      tagged DBEQ {psrc1: .rs1, psrc2: .rs2, offset: .off}: 
	begin

          Bool taken = unJust(mva) == unJust(mvb);
	  Addr extimm = signExtend(off) << 2;
	  Addr dest  = addr_plus_4 + extimm;

          done  = isJust(mva) && isJust(mvb);
	  res   = taken ? (tagged RBranchTaken dest) : tagged RBranchNotTaken;
	  dbg = $display("EXE: [%d] DBEQ PC <= 0x%h (offset 0x%h) if (0x%h == 0x%h)", tok.index, dest, extimm, unJust(mva), unJust(mvb));
	  einst = ENop;

	end

      //Branch if Not Equal
      tagged DBNE {psrc1: .rs1, psrc2: .rs2, offset: .off}: 
	begin

          Bool taken = unJust(mva) != unJust(mvb);
	  Addr extimm = signExtend(off) << 2;
	  Addr dest  = addr_plus_4 + extimm;

          done  = isJust(mva) && isJust(mvb);
	  res   = taken ? (tagged RBranchTaken dest) : tagged RBranchNotTaken;
	  dbg = $display("EXE: [%d] DBNE PC <= 0x%h (offset 0x%h) if (0x%h != 0x%h)", tok.index, dest, extimm, unJust(mva), unJust(mvb));
	  einst = ENop;

	end
      
      // -- Jumps -----------------------------------------------------

      //Jump
      tagged DJ {target: .targ}: 
	begin

	  Addr dest  = {addr_plus_4[31:28], targ, 2'b00};

          done  = True;
	  res   = tagged RBranchTaken dest;
	  dbg = $display("EXE: [%d] DJ PC <= 0x%h = {%0h, %0h, 00}", tok.index, dest, addr_plus_4[31:26], targ);
	  einst = ENop;

	end
      
      //Jump Register
      tagged DJR {psrc: .rs}: 
	begin

          Addr dest = unJust(mva);

          done  = isJust(mva);
	  res   = tagged RBranchTaken dest;
	  dbg = $display("EXE: [%d] DJR PC <= 0x%h ", tok.index, dest);
	  einst = ENop;

	end

      //Jump and Link (into archictectural register 31)
      tagged DJAL {target: .targ, pdest: .rd}:
	begin

	  Addr dest  = {addr_plus_4[31:28], targ, 2'b0};
	  
          done  = True;
	  res   = tagged RBranchTaken dest;
	  wbval = addr_plus_4;
	  dbg = $display("EXE: [%d] DJAL PC <= 0x%h, PR%d <= 0x%h", tok.index, dest, rd, addr_plus_4);
          einst = EWB
                  {
		    pdest:  rd
		  };
	end


      //Jump and Link into Register
      tagged DJALR {psrc: .rs, pdest: .rd}: 
	begin
	  
	  Addr dest  = unJust(mva);
	  
          done  = isJust(mva);
	  res   = tagged RBranchTaken dest;
	  wbval = addr_plus_4;
	  dbg = $display("EXE: [%d] DJALR PC <= 0x%h, PR%d <= 0x%h", tok.index, dest, rd, addr_plus_4);
          einst = EWB
                  {
		    pdest:  rd
		  };
	end
       // -- Co-Proc ---------------------------------------------------

      //Move To Co-Processor 0
      tagged DMTC0 {psrc: .rs, cop0dest: .cd}: 
	begin
	  
          done  = isJust(mva);
	  Bool pf = unJust(mva) == 1; //Equal to 1 means we passed
	  //A Non-Zero value to "fromHost" is equivalent to a terminate for our purposes
	  res   = (unJust(mva) == 0) ? tagged RNop : (cd == 21) ? tagged RTerminate pf : tagged RNop;
	  dbg = $display("EXE: [%d] DMTC0 COP0 R%d <= 0x%h", tok.index, cd, rs);
          einst = ENop;
	end

      //Move From Co-Processor 0
      tagged DMFC0 {pdest: .rd, cop0src: .cs}: 
	begin
	  //This instruction is pretty useless because cop0src doesn't exist.
	  //So what we do instead is set rd to 1 (because this is what most of our testcases use this for).
          done  = True;
	  res   = tagged RNop;
	  wbval = 1;
	  dbg = $display("EXE: [%d] DMFC0 PR%d <= COP0 R%0d (Hardwired to 1)", tok.index, rd, cs);
          einst = EWB
                  {
		    pdest:  rd
		  };
	end

       // -- Illegal ---------------------------------------------------
 
      tagged DTERMINATE: 
        begin
	
	  done = True;
	  res = tagged RTerminate True;
	  dbg = $display("EXE: [%d] DTERMINATE", tok.index);
	  einst = ENop;
	  	  
        end
	
      default: 
        begin
	
	  done = True;
	  res = tagged RNop;
	  einst = ENop;
	  
	  $display("ERROR: EXECUTING ILLEGAL INSTRUCTION");
	  
        end
    endcase
      
    if (done)
      begin
	link_exe.makeResp(tuple3(tok, res, einst));
	
	case (einst) matches
	  tagged EWB {pdest: .d}:
	    link_write1.send(tuple2(d, wbval));
	  default:
	    noAction;
	endcase
	
	debug(2, dbg);
	waitingQ.deq();
      end
      else
	  debug(2, $display("EXE stall"));
      
    waiting <= False;

  endrule

  
endmodule 

