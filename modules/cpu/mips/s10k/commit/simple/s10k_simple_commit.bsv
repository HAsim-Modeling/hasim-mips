import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;

import FIFO::*;
import Vector::*;

import hasim_parameters::*;

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

    Reg#(Bit#(32))                                  clockCounter <- mkReg(0);

    rule clockCount(True);
        clockCounter <= clockCounter + 1;
    endrule

    rule globalCommitAck(True);
        $display("&commit_globalCommitAck %d", clockCounter);
        let ack <- fpGlobalCommitResp.receive();
        $display("&    ... <- fpGlobalCommitResp.receive()");
    endrule

    rule localCommit(True);
        $display("&commit_localCommit %d", clockCounter);
        localCommitPos <= localCommitPos + 1;//Assuming Power of 2
        let tokenMaybe <- commitPort[localCommitPos].receive();
        $display("&    Maybe#(%b, ...) <- commitPort[%d].receive()", isValid(tokenMaybe), localCommitPos);
        if(isValid(tokenMaybe))
        begin
            let token = validValue(tokenMaybe);
            globalCommitFIFO.enq(token);
            fpLocalCommitReq.send(tuple2(token, ?));
            $display("&    fpLocalCommitReq.send(...)");
        end
    endrule

    rule globalCommit(True);
        $display("&commit_globalCommit %d", clockCounter);
        let token <- fpLocalCommitResp.receive();
        $display("&    ... <- fpLocalCommitResp.receive()");
        globalCommitFIFO.deq();
        fpGlobalCommitReq.send(token);
        $display("&    fpGlobalCommitReq.send(...)");
    endrule
endmodule
