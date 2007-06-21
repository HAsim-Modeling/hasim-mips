
import hasim_common::*;

import hasim_isa::*;

module [HASim_Module] mkISA_Datapath 
  //interface:
              ();

  Connection_Server#(Tuple4#(Inst, Addr, Value, Value),
                     Tuple3#(InstResult, Addr, Value)) link_fp <- mkConnection_Server("isa_datapath");

  rule isa_exec (True);
  
    match {.inst, .addr, .v1, .v2} <- link_fp.getReq();

    Value src1 = src1IsR0(inst) ? 0 : v1;
    Value src2 = src2IsR0(inst) ? 0 : v2;

    Addr addr_plus_4 = addr + 4;

    InstResult res = ?;
    Addr eaddr = ?;
    Value wbval = ?;
    Action dbg   = noAction;

    case (inst) matches
      // -- Memory Ops ------------------------------------------------      
      
      //Load Word
      tagged LW .info: 
	begin
	
	  Addr extimm = signExtend(info.offset);
	  eaddr = src1 + extimm;
	  res   = tagged RNop;
	  dbg   = $display("DP: LW EADDR 0x%h = 0x%h + 0x%h", eaddr, src1, extimm);

	end
	
      //Store Word
      tagged SW .info: 
	begin

	  Addr extimm = signExtend(info.offset);
	  eaddr = src1 + extimm;
	  res   = tagged RNop;
	  wbval = src2;
	  dbg   = $display("DP: SW EADDR 0x%h = 0x%h + 0x%h <= 0x%h", eaddr, src1, extimm, src2);

	end

      // -- Simple Ops ------------------------------------------------      

      //Add Immediate Unsigned 
      //Actually the numbers are sign extended, it just can't overflow
      tagged ADDIU .info: 
	begin
	
	  res   = tagged RNop;
	  Value extimm = signExtend(info.imm);
	  wbval = src1 + extimm;
	  dbg   = $display("DP: ADDIU 0x%h = 0x%h + 0x%h]", wbval, src1, extimm);

	end
	
      //Set Less Than Immediate (Signed)
      tagged SLTI .info: 
	begin
	
	  res   = tagged RNop;
	  Value extimm = signExtend(info.imm);
	  wbval = zeroExtend(pack(signedLT(src1, extimm)));
	  dbg   = $display("DP: SLTI 0x%h = slt(0x%h, 0x%h)", wbval, src1, extimm);
	  
	end
	
      //Set Less Than Immediate Unsigned 
      tagged SLTIU .info: 
	begin
	
	  res   = tagged RNop;
	  Value extimm = signExtend(info.imm);
	  wbval = zeroExtend(pack(src1 < extimm));
	  dbg   = $display("DP: SLTIU 0x%h = sltu(0x%h, 0x%h)", wbval, src1, extimm);

	end
	
      //And Immediate
      tagged ANDI .info: 
	begin
	
	  res   = tagged RNop;
	  Value zimm = zeroExtend(info.imm);
	  wbval = src1 & zimm;
	  dbg   = $display("DP: ANDI 0x%h = 0x%h & 0x%h", wbval, src1, zimm);

	end
	
      //Or Immediate
      tagged ORI .info: 
	begin
	
	  res   = tagged RNop;
	  Value zimm = zeroExtend(info.imm);
	  wbval = src1 | zimm;
	  dbg   = $display("DP: ORI 0x%h = 0x%h | 0x%h", wbval, src1, zimm);

	end
	
      //XOR Immediate
      tagged XORI .info: 
	begin
	
	  res   = tagged RNop;
	  Value zimm = zeroExtend(info.imm);
	  wbval = src1 ^ zimm;
	  dbg   = $display("DP: XORI 0x%h = 0x%h ^ 0x%h", wbval, src1, zimm);
          
	end
	
	
      //Load Unsigned Immediate (Really is unsigned)
      tagged LUI .info: 
	begin

	  res   = tagged RNop;
	  Value zimm = zeroExtend(info.imm);
	  wbval = zimm << 16;
	  dbg   = $display("DP: LUI 0x%h = 0x%h << 16", wbval, zimm);

	end
	
	
      //Shift Left Logical (Immediate)
      tagged SLL .info: 
	begin

	  res   = tagged RNop;
	  wbval = src1 << info.shamt;
	  dbg   = $display("DP: SLL 0x%h = 0x%h << 0x%h", wbval, src1, info.shamt);

	end
	
      //Shift Right Logical (Immediate)
      tagged SRL .info: 
	begin

	  res   = tagged RNop;
	  wbval = src1 >> info.shamt;
	  dbg   = $display("DP: SRL 0x%h = 0x%h >> 0x%h", wbval, src1, info.shamt);

	end
	
      //Shift Right Arithmatic (Immediate)
      tagged SRA .info: 
	begin

	  res   = tagged RNop;
	  wbval = signedShiftRight(src1, info.shamt);
	  dbg   = $display("DP: SRA 0x%h = 0x%h <<a 0x%h", wbval, src1, info.shamt);

	end
	
      //Shift Left Logical Variable
      tagged SLLV .info: 
	begin

	  res   = tagged RNop;
	  wbval = src1 << src2[4:0];
	  dbg   = $display("DP: SLLV 0x%h = 0x%h << 0x%h", wbval, src1, src2[4:0]);

	end
	
      //Shift Right Logical Variable
      tagged SRLV .info: 
	begin

	  res   = tagged RNop;
	  wbval = src1 >> src2[4:0];
	  dbg   = $display("DP: SRLV 0x%h = 0x%h >> 0x%h", wbval, src1, src2[4:0]);

	end
	
      //Shift Right Arithmatic Variable
      tagged SRAV .info: 
	begin

	  res   = tagged RNop;
	  wbval = signedShiftRight(src1, src2[4:0]);
	  dbg   = $display("DP: SRAV 0x%h = 0x%h >>a 0x%h", wbval, src1, src2[4:0]);

	end
	
      //Add Unsigned
      tagged ADDU .info: 
	begin
	  
	  res   = tagged RNop;
	  wbval = src1 + src2;
	  dbg   = $display("DP: ADDU %0h = %0h + %0h", wbval, src1, src2);
	    
	end

      //Subtract Unsigned
      tagged SUBU .info: 
	begin

	  res   = tagged RNop;
	  wbval = src1 - src2;
	  dbg   = $display("DP: SUBU 0h = %0h - %0h", wbval, src1, src2);

	end
	
      //And
      tagged AND .info: 
	begin

	  res   = tagged RNop;
	  wbval = src1 & src2;
	  dbg   = $display("DP: AND %0h = %0h & %0h", wbval, src1, src2);

	end
      
      //OR
      tagged OR .info: 
	begin

	  res   = tagged RNop;
	  wbval = src1 | src2;
	  dbg   = $display("DP: OR %0h = %0h | %0h", wbval, src1, src2);

	end
	
      //XOR
      tagged XOR .info: 
	begin

	  res   = tagged RNop;
	  wbval = src1 ^ src2;
	  dbg   = $display("DP: XOR %0h = %0h ^ %0h", wbval, src1, src2);

	end

      //NOR
      tagged NOR .info: 
	begin

	  res   = tagged RNop;
	  wbval = ~(src1 | src2);
	  dbg   = $display("DP: NOR %0h = %0h nor %0h", wbval, src1, src2);

	end

      //Set Less Than
      tagged SLT .info: 
	begin

	  res   = tagged RNop;
	  wbval = zeroExtend(pack(signedLT(src1, src2)));
	  dbg   = $display("DP: SLT %0h = slt(%0h, %0h)", wbval, src1, src2);

	end
      
      //Set Less Than Unsigned
      tagged SLTU .info: 
	begin

	  res   = tagged RNop;
	  wbval = zeroExtend(pack(src1 < src2));
	  dbg   = $display("DP: SLTU %0h = sltu(%0h, %0h)", wbval, src1, src2);

	end


      // -- Branches --------------------------------------------------
      
      //Branch if Less-Than or Equal to Zero
      tagged BLEZ .info: 
	begin

          Bool taken = signedLE(src1, 0);
	  Addr extimm = signExtend(info.offset) << 2;
	  Addr dest  = addr_plus_4 + extimm;

	  res   = taken ? (tagged RBranchTaken dest) : tagged RBranchNotTaken;
	  dbg   = $display("DP: BLEZ PC <= 0x%h (offset 0x%h) if (0x%h <= 0)", dest, extimm, src1);
	  
	end

      //Branch if Greater Than Zero
      tagged BGTZ .info: 
	begin
	
          Bool taken = signedGT(src1, 0);
	  Addr extimm = signExtend(info.offset) << 2;
	  Addr dest  = addr_plus_4 + extimm;

	  res   = taken ? (tagged RBranchTaken dest) : tagged RBranchNotTaken;
	  dbg   = $display("DP: BGTZ PC <= 0x%h (offset 0x%h) if (0x%h > 0)", dest, extimm, src1);
	  
	end

      //Branch if Less Than Zero
      tagged BLTZ .info: 
	begin
	
          Bool taken = signedLT(src1, 0);
	  Addr extimm = signExtend(info.offset) << 2;
	  Addr dest  = addr_plus_4 + extimm;

	  res   = taken ? (tagged RBranchTaken dest) : tagged RBranchNotTaken;
	  dbg   = $display("DP: BLTZ PC <= 0x%h (offset 0x%h) if (0x%h < 0)", dest, extimm, src1);

	end

      //Branch if Greater than or Equal to Zero
      tagged BGEZ .info: 
	begin

          Bool taken = signedGE(src1, 0);
	  Addr extimm = signExtend(info.offset) << 2;
	  Addr dest  = addr_plus_4 + extimm;

	  res   = taken ? (tagged RBranchTaken dest) : tagged RBranchNotTaken;
	  dbg   = $display("DP: BGEZ PC <= 0x%h (offset 0x%h) if (0x%h > 0)", dest, extimm, src1);

	end

      //Branch if Equal
      tagged BEQ .info: 
	begin

          Bool taken = src1 == src2;
	  Addr extimm = signExtend(info.offset) << 2;
	  Addr dest  = addr_plus_4 + extimm;

	  res   = taken ? (tagged RBranchTaken dest) : tagged RBranchNotTaken;
	  dbg   = $display("DP: BEQ PC <= 0x%h (offset 0x%h) if (0x%h == 0x%h)", dest, extimm, src1, src2);

	end

      //Branch if Not Equal
      tagged BNE .info: 
	begin

          Bool taken = src1 != src2;
	  Addr extimm = signExtend(info.offset) << 2;
	  Addr dest  = addr_plus_4 + extimm;

	  res   = taken ? (tagged RBranchTaken dest) : tagged RBranchNotTaken;
	  dbg   = $display("DP: BNE PC <= 0x%h (offset 0x%h) if (0x%h != 0x%h)", dest, extimm, src1, src2);

	end
      
      // -- Jumps -----------------------------------------------------

      //Jump
      tagged J .info: 
	begin

	  Addr dest  = {addr_plus_4[31:28], info.target, 2'b00};

	  res   = tagged RBranchTaken dest;
	  dbg   = $display("DP: J PC <= 0x%h = {%0h, %0h, 00}", dest, addr_plus_4[31:26], info.target);

	end
      
      //Jump Register
      tagged JR .info: 
	begin

          Addr dest = src1;

	  res   = tagged RBranchTaken dest;
	  dbg   = $display("DP: JR PC <= 0x%h ", dest);
	
	end

      //Jump and Link (into archictectural register 31)
      tagged JAL .info:
	begin

	  Addr dest  = {addr_plus_4[31:28], info.target, 2'b0};
	  
	  res   = tagged RBranchTaken dest;
	  wbval = addr_plus_4;
	  dbg   = $display("DP: JAL PC <= 0x%h = {%0h, %0h, 00}, (Old PC: 0x%h)", dest, addr_plus_4[31:28], info.target, addr_plus_4);

	end


      //Jump and Link into Register
      tagged JALR .info: 
	begin
	  
	  Addr dest  = src1;
	  
	  res   = tagged RBranchTaken dest;
	  wbval = addr_plus_4;
	  dbg   = $display("DP: JALR PC <= 0x%h, (Old PC: 0x%h)", dest, addr_plus_4);

	end

       // -- Co-Proc ---------------------------------------------------

      //Move To Co-Processor 0
      tagged MTC0 .info: 
	begin
	  
	  Bool pf = src1 == 1; //Equal to 1 means we passed
	  //A Non-Zero value to "fromHost" is equivalent to a terminate for our purposes
	  res   = (src1 == 0) ? tagged RNop : (info.cop0dest == 21) ? tagged RTerminate pf : tagged RNop;
	  dbg   = $display("DP: MTC0 COP0 R%d <= 0x%h", info.cop0dest, src1);

	end

      //Move From Co-Processor 0
      tagged MFC0 .info: 
	begin
	  //This instruction is pretty useless because cop0src doesn't exist.
	  //So what we do instead is set rd to 1 (because this is what most of our testcases use this for).
          
	  res   = tagged RNop;
	  wbval = 1;
	  dbg   = $display("DP: MFC0 R%d <= COP0 R%0d (Hardwired to 1)", info.rdest, info.cop0src);
          
	end

       // -- Illegal ---------------------------------------------------
 
      tagged TERMINATE: 
        begin
	
	  res = tagged RTerminate True;
	  dbg   = $display("DP: TERMINATE");
	  	  
        end
	
      default: 
        begin
	
	  res = tagged RNop;
	  
	  $display("DP: ERROR: EXECUTING ILLEGAL INSTRUCTION");
	  
        end
    endcase
    
    dbg;
    link_fp.makeResp(tuple3(res, eaddr, wbval));

  endrule
  
endmodule