import hasim_common::*;

import Vector::*;

import hasim_cpu_parameters::*;
import hasim_cpu_types::*;

module [HASim_Module] mkPipe_Writeback();
    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, integerToString(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, integerToString(i)), 1);

    Vector#(CommitWidth, Port_Receive#(Token))        commitPort <- genWithM(receiveFunctionM("decodeToCommit"));

    Connection_Send#(Tuple2#(Token, void))      fpLocalCommitReq <- mkConnection_Send("fp_lco_req");
    Connection_Receive#(Tuple2#(Token, void))  fpLocalCommitResp <- mkConnection_Receive("fp_lco_resp");

    Connection_Send#(Tuple2#(Token, void))     fpGlobalCommitReq <- mkConnection_Send("fp_gco_req");
    Connection_Receive#(Tuple2#(Token, void)) fpGlobalCommitResp <- mkConnection_Receive("fp_gco_resp");

    Reg#(CommitCount)                             localCommitPos <- mkReg(0);

    rule localCommit(True);
        Maybe#(Token) tokenMaybe <- commitPort[localCommitPos].receive();
        case (tokenMaybe) matches
            tagged Valid .token:
                fpLocalCommitReq.send(tuple2(token, ?));
        endcase
        localCommitPos <= (localCommitPos == fromInteger(valueOf(TSub#(CommitWidth,1))))? 0: localCommitPos + 1;
    endrule

    rule globalCommit(True);
        let token <- fpLocalCommitResp.receive();
        fpGlobalCommitReq.send(token);
    endrule

    rule globalCommitAck(True);
        let ack <- fpGlobalCommitResp.receive();
    endrule
endmodule
