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

    Reg#(FuncUnitPos)                                           funcUnitPos <- mkReg(fromInteger(valueOf(NumFuncUnits)));

    Reg#(Bit#(32))                                             clockCounter <- mkReg(0);

    rule clockCount(True);
        clockCounter <= clockCounter + 1;
    endrule

    rule start(funcUnitPos == fromInteger(valueOf(NumFuncUnits)));
        $display("&execute_start %d", clockCounter);
        funcUnitPos <= 1;
        let recv <- execPort[0].receive();
        $display("&    Maybe#(%b, ...) <- execPort[0]", isValid(recv));
        Maybe#(Tuple2#(ExecEntry, InstResult)) execResult = ?;
        if(isValid(recv))
        begin
            match {.token, .res} <- fpExeResponse.receive();
            $display("&    ... <- fpExeResponse.receive()");
            fpMemReq.send(tuple2(token,?));
            $display("&    fpMemReq.send()");
            execResult = tagged Valid tuple2(validValue(recv), res);
        end
        else
            execResult = tagged Invalid;
        execResultPort[0].send(execResult);
        $display("&    execResultPort[0].send(Maybe#(%b, ...))", isValid(execResult));
    endrule

    rule cont(funcUnitPos < fromInteger(valueOf(NumFuncUnits))-1);
        $display("&execute_cont %d", clockCounter);
        funcUnitPos <= funcUnitPos + 1;
        let recv <- execPort[funcUnitPos].receive();
        $display("&    Maybe#(%b, ...) <- execPort[%d]", isValid(recv), funcUnitPos);
        Maybe#(Tuple2#(ExecEntry, InstResult)) execResult = ?;
        if(isValid(recv))
        begin
            match {.token, .res} <- fpExeResponse.receive();
            $display("&    ... <- fpExeResponse.receive()");
            fpMemReq.send(tuple2(token, ?));
            $display("&    fpMemReq.send()");
            execResult = tagged Valid tuple2(validValue(recv), res);
        end
        else
            execResult = tagged Invalid;
        execResultPort[funcUnitPos].send(execResult);
        $display("&    execResultPort[%d].send(Maybe#(%b, ...))", funcUnitPos, isValid(execResult));
    endrule

    rule mem(funcUnitPos == fromInteger(valueOf(NumFuncUnits))-1);
        $display("&execute_mem %d", clockCounter);
        funcUnitPos <= funcUnitPos + 1;
        let recv <- memPort.receive();
        $display("&    Maybe#(%b, ...) <- execPort[%d]", isValid(recv), funcUnitPos);
        Maybe#(Tuple2#(ExecEntry, InstResult)) execResult = ?;
        if(isValid(recv))
        begin
            match {.token, .res} <- fpExeResponse.receive();
            $display("&    ... <- fpExeResponse.receive()");
            fpMemReq.send(tuple2(token, ?));
            $display("&    fpMemReq.send()");
            execResult = tagged Valid tuple2(validValue(recv), res);
        end
            execResult = tagged Invalid;
        execResultPort[funcUnitPos].send(execResult);
        $display("&    execResultPort[%d].send(Maybe#(%b, ...))", funcUnitPos, isValid(execResult));
    endrule
endmodule
