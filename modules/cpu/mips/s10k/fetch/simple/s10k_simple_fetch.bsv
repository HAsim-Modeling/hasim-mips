import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import FIFOF::*;
import Vector::*;

import s10k_simple_common::*;

//No branch predictor in the fetch stage. No pipelining
module [HASim_Module] mkFetch();

    Connection_Send#(Bit#(8))              fpTokenReq  <- mkConnection_Send("fp_tok_req");
    Connection_Receive#(Token)             fpTokenResp <- mkConnection_Receive("fp_tok_resp");
    Connection_Send#(Tuple2#(Token, Addr)) fpFetchReq  <- mkConnection_Send("fp_fet_req");

    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, fromInteger(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, fromInteger(i)), 1);

    Port_Receive#(FetchCount)            decodeNumPort <- mkPort_Receive("decodeToFetch_DecodeNum", 1);
    Port_Receive#(Addr)             predictedTakenPort <- mkPort_Receive("decodeToFetch_PredictedTaken", 1);
    Port_Receive#(Addr)                 mispredictPort <- mkPort_Receive("decodeToFetch_Mispredict", 1);

    Vector#(FetchWidth, Port_Send#(Tuple2#(Token, Addr))) tokenAddrPort <- genWithM(sendFunctionM("fetchToDecode_TokenAddr"));

    Reg#(Addr)                                      pc <- mkReg(pcStart);
    Reg#(FetchCount)                        fetchCount <- mkReg(0);
    Reg#(Bit#(32))                             latency <- mkReg(?); //Actual model latency * 4
    Reg#(FetchCount)                      instPosition <- mkReg(fromInteger(valueOf(FetchWidth))); //This triggers only start rule

    //Returns model latency * 4
    function Bit#(32) getHostLatency();
        return 0;
    endfunction

    function FetchCount getCacheBoundary(Addr pcPassed, Maybe#(FetchCount) decodeNum);
        return isValid(decodeNum)? validValue(decodeNum): fromInteger(valueOf(FetchWidth));
    endfunction

    rule start(fetchCount == 0 && instPosition == fromInteger(valueOf(FetchWidth)));
        let predictedTaken <- predictedTakenPort.receive();
        let     mispredict <- mispredictPort.receive();
        let      decodeNum <- decodeNumPort.receive();

        Addr newPC = ?;
        Bit#(32) newLatency = ?;
        if(isValid(mispredict))
        begin
            newLatency = 0;
            newPC = validValue(mispredict);
        end
        else if(isValid(predictedTaken))
        begin
            newLatency = getHostLatency();
            newPC = validValue(predictedTaken);
        end
        else
        begin
            newLatency = getHostLatency();
            newPC = pc;
        end

        pc           <= newPC;
        latency      <= newLatency;

        let newCount  = getCacheBoundary(newPC, decodeNum);
        fetchCount   <= newCount;
        instPosition <= 0;

        if(newCount != 0)
            fpTokenReq.send(17);
        $display("Fetch begins for %d instructions", fetchCount);
    endrule

    rule missWait(latency != 0 && fetchCount != 0);
        latency <= latency - 1;
        for(Integer i = 0; i < valueOf(FetchWidth); i=i+1)
            tokenAddrPort[i].send(tagged Invalid);
    endrule

    function Action doFetch();
    action
        let token <- fpTokenResp.receive();
        fpFetchReq.send(tuple2(token, pc));
        tokenAddrPort[instPosition].send(tagged Valid tuple2(token, pc));
        instPosition <= instPosition + 1;
        fetchCount <= fetchCount - 1;
        pc <= pc + 4;
    endaction
    endfunction

    rule middleInsts(latency == 0 && fetchCount != 1);  //if fetchCount = 0, then we wont receive anything from the token, so this rule wont fire
        doFetch();
        fpTokenReq.send(17);
    endrule

    rule finishInsts(latency == 0 && fetchCount == 1);
        doFetch();
    endrule

    rule finishOverall(latency == 0 && fetchCount == 0 && instPosition != fromInteger(valueOf(FetchWidth)));
        instPosition <= instPosition + 1;
        tokenAddrPort[instPosition].send(tagged Invalid);
    endrule

    rule displayDone(instPosition == fromInteger(valueOf(FetchWidth)) - 1);
        $display("Fetch done");
    endrule

endmodule
