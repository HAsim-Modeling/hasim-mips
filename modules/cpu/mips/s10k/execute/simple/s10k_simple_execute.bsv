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

    Vector#(TSub#(NumFuncUnits,1), Port_Receive#(ExecEntry))issueToExecPort <- genWithM(receiveFunctionM("issueToExec"));
    Port_Receive#(ExecEntry)                                        memPort <- mkPort_Receive("issueToExecMem", 2);

    Vector#(NumFuncUnits, Port_Send#(ExecEntry))                   execPort <- genWithM(sendFunctionM("execToDecode"));

    Reg#(FuncUnitPos)                                           funcUnitPos <- mkReg(fromInteger(valueOf(NumFuncUnits)));

    rule start(funcUnitPos == fromInteger(valueOf(NumFuncUnits)));
        funcUnitPos <= 1;
        let recv <- issueToExecPort[0].receive();
        execPort[0].send(recv);
        if(isValid(recv))
        begin
            let ack <- fpExeResponse.receive();
            fpMemReq.send(tuple2(tpl_1(ack),?));
        end
    endrule

    rule cont(funcUnitPos < fromInteger(valueOf(NumFuncUnits))-1);
        funcUnitPos <= funcUnitPos + 1;
        let recv <- issueToExecPort[funcUnitPos].receive();
        execPort[funcUnitPos].send(recv);
        if(isValid(recv))
        begin
            let ack <- fpExeResponse.receive();
            fpMemReq.send(tuple2(tpl_1(ack),?));
        end
    endrule

    rule mem(funcUnitPos == fromInteger(valueOf(NumFuncUnits))-1);
        funcUnitPos <= funcUnitPos + 1;
        let recv <- memPort.receive();
        execPort[funcUnitPos].send(recv);
        if(isValid(recv))
        begin
            let ack <- fpExeResponse.receive();
            fpMemReq.send(tuple2(tpl_1(ack),?));
        end
    endrule
    
endmodule
