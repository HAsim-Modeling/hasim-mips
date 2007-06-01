import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import Vector::*;

import hasim_cpu_parameters::*;
import hasim_cpu_types::*;

typedef enum {Exec, Done} State deriving (Bits, Eq);

module [HASim_Module] mkExecute();
    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, fromInteger(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, fromInteger(i)), 1);

    Connection_Receive#(Tuple2#(Token, InstResult))           fpExeResponse <- mkConnection_Receive("fp_exe_resp");
    Connection_Send#(Tuple2#(Token, void))                         fpMemReq <- mkConnection_Send("fp_mem_req");

    Vector#(NumFuncUnits, Port_Receive#(ExecEntry))                execPort  = newVector();
    for(Integer i = 0; i < valueOf(TSub#(NumFuncUnits,1)); i=i+1)
        execPort[i] <- mkPort_Receive(strConcat("issueToExec", fromInteger(i)), 1);
    execPort[valueOf(TSub#(NumFuncUnits,1))] <- mkPort_Receive(strConcat("issueToExec", fromInteger(valueOf(TSub#(NumFuncUnits,1)))), 2);

    Vector#(NumFuncUnits, Port_Send#(Tuple2#(ExecEntry, InstResult)))
                                                             execResultPort <- genWithM(sendFunctionM("execToDecodeResult"));

    Reg#(FuncUnitPos)                                           funcUnitPos <- mkReg(0);

    Reg#(ClockCounter)                                         clockCounter <- mkReg(0);

    rule clockCount(True);
        clockCounter <= clockCounter + 1;
    endrule

    rule execute(True);
        funcUnitPos <= (funcUnitPos + 1)%fromInteger(valueOf(NumFuncUnits));
        let recv <- execPort[funcUnitPos].receive();

        Maybe#(Tuple2#(ExecEntry, InstResult)) execResult = ?;
        if(isValid(recv))
        begin
            match {.token, .res} <- fpExeResponse.receive();
            fpMemReq.send(tuple2(token, ?));
            execResult = tagged Valid tuple2(validValue(recv), res);
        end
        else
            execResult = tagged Invalid;
        execResultPort[funcUnitPos].send(execResult);
    endrule
endmodule
