import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import FIFO::*;
import Vector::*;

import s10k_simple_common::*;

module [HASim_Module] mkCommit();
    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, fromInteger(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, fromInteger(i)), 1);

    Vector#(CommitWidth, Port_Receive#(Token))        commitPort <- genWithM(receiveFunctionM("decodeToCommit"));

    Connection_Send#(Tuple2#(Token, void))      fpLocalCommitReq <- mkConnection_Send("fp_lco_req");
    Connection_Receive#(Tuple2#(Token, void))  fpLocalCommitResp <- mkConnection_Receive("fp_lco_resp");

    Connection_Send#(Tuple2#(Token, void))     fpGlobalCommitReq <- mkConnection_Send("fp_gco_req");
    Connection_Receive#(Tuple2#(Token, void)) fpGlobalCommitResp <- mkConnection_Receive("fp_gco_resp");

    Reg#(Bit#(TLog#(CommitWidth)))                localCommitPos <- mkReg(0);

    FIFO#(Token)                                globalCommitFIFO <- mkFIFO();

    rule localCommitAck(True);
        let ack <- fpLocalCommitResp.receive();
    endrule

    rule globalCommitAck(True);
        let ack <- fpGlobalCommitResp.receive();
    endrule

    rule localCommit(True);
        localCommitPos <= localCommitPos + 1;//Assuming Power of 2
        let tokenMaybe <- commitPort[localCommitPos].receive();
        if(isValid(tokenMaybe))
        begin
            let token = validValue(tokenMaybe);
            globalCommitFIFO.enq(token);
            fpLocalCommitReq.send(tuple2(token, ?));
        end
    endrule

    rule globalCommit(True);
        let token = globalCommitFIFO.first();
        globalCommitFIFO.deq();
        fpGlobalCommitReq.send(tuple2(token, ?));
    endrule
endmodule
