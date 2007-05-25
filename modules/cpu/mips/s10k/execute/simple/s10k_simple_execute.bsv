import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import FIFOF::*;
import Vector::*;

import hasim_parameters::*;
import hasim_types::*;

typedef enum {Exec, Done} State deriving (Bits, Eq);

module [HASim_Module] mkExecute();
    Connection_Receive#(Tuple2#(Token, InstResult))           fpExeResponse <- mkConnection_Receive("fp_exe_resp");
    Connection_Send#(Tuple2#(Token, void))                         fpMemReq <- mkConnection_Send("fp_mem_req");

    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, fromInteger(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, fromInteger(i)), 1);

    Vector#(TSub#(NumFuncUnits,1), Port_Receive#(ExecEntry))       execPort <- genWithM(receiveFunctionM("issueToExecNormal"));
    Port_Receive#(ExecEntry)                                        memPort <- mkPort_Receive("issueToExecMem", 2);

    Vector#(NumFuncUnits, Port_Send#(Tuple2#(ExecEntry, InstResult))) 
                                                             execResultPort <- genWithM(sendFunctionM("execToDecodeResult"));

    Reg#(FuncUnitPos)                                           funcUnitPos <- mkReg(0);

    Reg#(ClockCounter)                                         clockCounter <- mkReg(0);

    rule clockCount(True);
        clockCounter <= clockCounter + 1;
    endrule

    rule execute(True);
        funcUnitPos <= (funcUnitPos == fromInteger(valueOf(TSub#(NumFuncUnits,1))))? 0: funcUnitPos + 1;
        Maybe#(ExecEntry) recv = ?;
        if(funcUnitPos == fromInteger(valueOf(TSub#(NumFuncUnits,1))))
        begin
            recv <- memPort.receive();
        end
        else
        begin
            recv <- execPort[funcUnitPos].receive();
        end
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
