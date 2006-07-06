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

  Inst prog[10] = 
    { 
      LUI {rdest: r1, imm: fromInteger(x)},     //	Set x
      LUI {rdest: r2, imm: fromInteger(y)},     //	Set y
      LUI {rdest: r9, imm: 1},                  //	Set 1	    
      LUI {rdest: r3, imm: 0},                  //	Set res = 0 
      ADDU {rdest: r3, rsrc1: r3, rsrc2: r1},   //Loop: res := res + x
      SUBU {rdest: r2, rsrc1: r2, rsrc2: r9},   //      y := y - 1
      BEQ  {rsrc1: r2, rsrc2: r0, offset: 2},   //	if y = 0 GOTO End
      J    {target: 4},                         //	GOTO Loop
      SW   {rsrc: r3, rbase: r0, offset: 0},    // End: Store res
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
