//HASim library imports

import hasim_common::*;

import hasim_isa::*;

import hasim_testcase_base::*;

//This file provides the following HASim-required names:
//     Name:             Type:
//     -----             -----
//     test_case         TestCase

// A more complex, branching test
// Tests x * y without using Mul instruction

function TestCase testBranch (Integer x, Integer y);

  Inst prog[34] = 
    { 
      ADDIU {rdest: r1, rsrc: r0, imm: fromInteger(x)},     //	Set x
      ADDIU {rdest: r2, rsrc: r0, imm: fromInteger(y)},     //	Set y
      ADDIU {rdest: r9, rsrc: r0, imm: 1},                  //	Set 1	    
      ADDIU {rdest: r3, rsrc: r0, imm: 0},                  //	Set res = 0 
      ADDU {rdest: r3, rsrc1: r3, rsrc2: r1},   //Loop: res := res + x
      SUBU {rdest: r2, rsrc1: r2, rsrc2: r9},   //      y := y - 1
      BEQ  {rsrc1: r2, rsrc2: r0, offset: 2},   //	if y = 0 GOTO End
      J    {target: 4},                         //	GOTO Loop
      SW   {rsrc: r3, rbase: r0, offset: 0},    // End: Store res
      TERMINATE,                                 //      finish
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE,
      TERMINATE
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

(* synthesize *)
module printTestCase ();

  Reg#(int) r <- mkReg(0);
  let max = 34;
  
  rule printNext (r < max);
  
    
    $display("%h", instToBits(test_case.imem_init[r]));
    r <= r + 1;
  
  endrule

endmodule
