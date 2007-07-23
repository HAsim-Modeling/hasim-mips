import hasim_common::*;

import Vector::*;

import hasim_cpu_parameters::*;
import hasim_cpu_types::*;

/*
 * Description of module:
 * rule localCommit happens first. It reads the 4 commitPorts and sends 4 requests to local commit of FuncP
 * rule globalCommit happens next. It reads the 4 responses from local commit of FuncP and sends 4 requests
 * to global commit of FuncP
 * rule globalCommitAck happens next. It reads the 4 responses from the global commit of FuncP
 */
module [HASim_Module] mkPipe_Writeback();
    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, integerToString(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, integerToString(i)), 1);

    Vector#(CommitWidth, Port_Receive#(Token))        commitPort <- genWithM(receiveFunctionM("decodeToCommit"));

    Connection_Send#(Tuple2#(Token, void))      fpLocalCommitReq <- mkConnection_Send("fp_lco_req");
    Connection_Receive#(Tuple2#(Token, void))  fpLocalCommitResp <- mkConnection_Receive("fp_lco_resp");

    Connection_Send#(Tuple2#(Token, void))     fpGlobalCommitReq <- mkConnection_Send("fp_gco_req");
    Connection_Receive#(Tuple2#(Token, void)) fpGlobalCommitResp <- mkConnection_Receive("fp_gco_resp");

    Reg#(CommitCount)                             localCommitPos <- mkReg(0);

    Reg#(ClockCounter)                                  clockReg <- mkReg(0);
    Reg#(ClockCounter)                                  modelReg <- mkReg(0);

    rule clockCount(True);
        clockReg <= clockReg + 1;
    endrule

    rule localCommit(True);
        Maybe#(Token) tokenMaybe <- commitPort[localCommitPos].receive();
        case (tokenMaybe) matches
            tagged Valid .token:
                fpLocalCommitReq.send(tuple2(token, ?));
        endcase
        localCommitPos <= (localCommitPos == fromInteger(valueOf(TSub#(CommitWidth,1))))? 0: localCommitPos + 1;
        if(localCommitPos == fromInteger(valueOf(TSub#(CommitWidth,1))))
        begin
            modelReg <= modelReg + 1;
            $display("6 0 %0d %0d", clockReg, modelReg);
        end
        if(localCommitPos == 0)
            $display("6 1 %0d %0d", clockReg, modelReg);
    endrule

    rule globalCommit(True);
        let token <- fpLocalCommitResp.receive();
        fpGlobalCommitReq.send(token);
    endrule

    rule globalCommitAck(True);
        let ack <- fpGlobalCommitResp.receive();
    endrule
endmodule
