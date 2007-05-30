import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import FIFOF::*;
import Vector::*;

import s10k_types::*;
import s10k_parameters::*;

typedef enum {Fetch, FetchDone} FetchState deriving (Bits, Eq);

module [HASim_Module] mkFetch();
    Connection_Send#(Bit#(8))               fpTokenReq <- mkConnection_Send("fp_tok_req");
    Connection_Receive#(Token)             fpTokenResp <- mkConnection_Receive("fp_tok_resp");
    Connection_Send#(Tuple2#(Token, Addr))  fpFetchReq <- mkConnection_Send("fp_fet_req");

    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, fromInteger(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, fromInteger(i)), 1);

    Port_Receive#(FetchCount)            decodeNumPort <- mkPort_Receive("decodeToFetchDecodeNum", 1);
    Port_Receive#(Addr)             predictedTakenPort <- mkPort_Receive("decodeToFetchPredictedTaken", 1);
    Port_Receive#(Addr)                 mispredictPort <- mkPort_Receive("decodeToFetchMispredict", 1);

    Vector#(FetchWidth, Port_Send#(Tuple2#(Token, Addr)))
                                         tokenAddrPort <- genWithM(sendFunctionM("fetchToDecode"));

    Reg#(FetchState)                        fetchState <- mkReg(FetchDone);
    Reg#(Addr)                                      pc <- mkReg(pcStart);
    Reg#(FetchCount)                        totalCount <- mkReg(?);
    Reg#(Bit#(32))                             latency <- mkReg(?); //Actual model latency * 4
    Reg#(FetchCount)                          fetchPos <- mkReg(?); //This triggers only start rule

    Reg#(ClockCounter)                    clockCounter <- mkReg(0);
    Reg#(ClockCounter)                    modelCounter <- mkReg(0);

    //Returns model latency * 4
    function Bit#(32) getHostLatency();
        return 0;
    endfunction

    function FetchCount getCacheBoundary(Addr pcPassed, Maybe#(FetchCount) decodeNum);
        return isValid(decodeNum)? validValue(decodeNum): fromInteger(valueOf(FetchWidth));
    endfunction

    rule clockCount(True);
        clockCounter <= clockCounter + 1;
    endrule

    function fillTokenAddrPort(FetchCount fetchedCount);
    action
        for(Integer i = 0; i < valueof(FetchWidth); i=i+1)
        begin
            if(fromInteger(i) >= fetchedCount)
            begin
                tokenAddrPort[i].send(tagged Invalid);
            end
        end
    endaction
    endfunction

    rule synchronize(fetchState == FetchDone);
        modelCounter <= modelCounter + 1;
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
        totalCount   <= newCount;
        fetchPos     <= 0;

        if(newCount == 0)
        begin
            fillTokenAddrPort(0);
        end
        else
        begin
            fpTokenReq.send(17);
            fetchState <= Fetch;
        end
    endrule

    rule missWait(fetchState == Fetch && latency != 0);
        latency <= latency - 1;
        for(Integer i = 0; i < valueOf(FetchWidth); i=i+1)
        begin
            tokenAddrPort[i].send(tagged Invalid);
        end
    endrule

    rule fetchInsts(fetchState == Fetch && latency == 0);
        let token <- fpTokenResp.receive();
        fpFetchReq.send(tuple2(token, pc));
        tokenAddrPort[fetchPos].send(tagged Valid tuple2(token, pc));
        let newFetchPos = fetchPos + 1;
        fetchPos  <= newFetchPos;
        pc        <= pc + 4;
        if(newFetchPos != totalCount)
        begin
            fpTokenReq.send(17);
        end
        else
        begin
            fillTokenAddrPort(totalCount);
            fetchState <= FetchDone;
        end
    endrule
endmodule
