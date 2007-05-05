//HASim library imports
import HASim::*;
import TestCase_Base::*;

//Model-specific imports
import ISA::*;

//This file provides the following HASim-required names:
//     Name:             Type:
//     -----             -----
//     test_case         TestCase

// A more complex, branching test
// Tests x * y without using Mul instruction

function TestCase testBranch (Integer x, Integer y);

  Inst prog[12] = 
    { 
      ILoadImm {dest: r1, imm: fromInteger(x)}, //      Set x
      ILoadImm {dest: r2, imm: fromInteger(y)}, //      Set y
      ILoadImm {dest: r7, imm: 10},             //      Set End
      ILoadImm {dest: r8, imm: 6},              //      Set Loop
      ILoadImm {dest: r9, imm: 1},              //      Set 1	    
      ILoadImm {dest: r3, imm: 0},              //      Set res = 0 
      IAdd     {dest: r3, src1: r3, src2: r1},  //Loop: res := res + x
      ISub     {dest: r2, src1: r2, src2: r9},  //      y := y - 1
      IBz      {cond: r2, addr: r7},            //	if y = 0 GOTO End
      IBz      {cond: r0, addr: r8},            //	GOTO Loop
      IStore   {src: r3, idx: r0, offset: 0},   // End: Store res
      ITerminate                                //      finish
    };

  Value dmem_i[1] = {0};
  Value dmem_e[1] = {fromInteger(x * y)};
  
  return TestCase
         {
           imem_init: prog, 
	   dmem_init: dmem_i,
	   dmem_exp:  dmem_e
	 };
	 
endfunction

//Eventually these should be parameters
TestCase test_case = testBranch(17, 12);
