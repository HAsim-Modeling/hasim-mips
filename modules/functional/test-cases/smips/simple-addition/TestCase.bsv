//HASim library imports
import HASim::*;
import TestCase_Base::*;

//Model-specific imports
import ISA::*;

//This file provides the following HASim-required names:
//     Name:             Type:
//     -----             -----
//     test_case         TestCase 

// A simple test that of x + y

function TestCase testAddition (Integer x, Integer y);
  
  Inst prog[5] = 
    {
      LW   {rdest: r1, rbase:  r0, offset: 0  },
      LW   {rdest: r2, rbase:  r0, offset: 1  },
      ADDU {rdest: r4, rsrc1: r1, rsrc2:   r2 },
      SW   {rsrc:  r4, rbase:  r0, offset: 2  },
      TERMINATE
    };

  Value dmem_i[2] = {fromInteger(x), fromInteger(y)};
  Value dmem_e[3] = {fromInteger(x), fromInteger(y), fromInteger(x + y)};
  
  return TestCase
         {
           imem_init: prog, 
	   dmem_init: dmem_i, 
	   dmem_exp:  dmem_e
	 };
	 
endfunction


//Eventually these should be parameters
TestCase test_case = testAddition(11, 5);
