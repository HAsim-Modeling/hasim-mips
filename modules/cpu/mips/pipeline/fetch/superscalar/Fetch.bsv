import hasim_common::*;
import hasim_isa::*;

import Vector::*;

import hasim_cpu_types::*;
import hasim_cpu_parameters::*;

typedef enum {Fetch, FetchDone} FetchState deriving (Bits, Eq);

module [HASim_Module] mkPipe_Fetch();
    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, integerToString(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, integerToString(i)), 1);

    Connection_Send#(Bit#(8))              fpTokenReq <- mkConnection_Send("fp_tok_req");
    Connection_Receive#(Token)            fpTokenResp <- mkConnection_Receive("fp_tok_resp");
    Connection_Send#(Tuple2#(Token, Addr)) fpFetchReq <- mkConnection_Send("fp_fet_req");

    Port_Receive#(FetchCount)          fetchCountPort <- mkPort_Receive("decodeToFetchDecodeNum", 1);
    Port_Receive#(Addr)            predictedTakenPort <- mkPort_Receive("decodeToFetchPredictedTaken", 1);
    Port_Receive#(Addr)                mispredictPort <- mkPort_Receive("decodeToFetchMispredict", 1);

    Vector#(FetchWidth, Port_Send#(Addr))    addrPort <- genWithM(sendFunctionM("fetchToDecode"));

    Reg#(FetchState)                       fetchState <- mkReg(FetchDone);
    Reg#(Addr)                                     pc <- mkReg(pcStart);
    Reg#(FetchCount)                       totalCount <- mkReg(?);
    Reg#(FetchCount)                         fetchPos <- mkReg(?);

    Reg#(TIMEP_Epoch)                           epoch <- mkReg(0);

    EventRecorder                            eventRec <- mkEventRecorder("Fetch");

    function fillAddrPort(FetchCount fetchedCount);
    action
        for(Integer i = 0; i < valueof(FetchWidth); i=i+1)
        begin
            if(fromInteger(i) >= fetchedCount)
                addrPort[i].send(tagged Invalid);
        end
    endaction
    endfunction

    rule synchronize(fetchState == FetchDone);
        Maybe#(Addr)   predictedTaken <- predictedTakenPort.receive();
        Maybe#(Addr)       mispredict <- mispredictPort.receive();
        Maybe#(FetchCount) fetchCount <- fetchCountPort.receive();

        if(isValid(mispredict))
        begin
            pc    <= validValue(mispredict);
            epoch <= epoch + 1;
            eventRec.recordEvent(mispredict);
        end
        else if(isValid(predictedTaken))
        begin
            pc <= validValue(predictedTaken);
            eventRec.recordEvent(predictedTaken);
        end
        else
        begin
            pc <= pc;
            eventRec.recordEvent(tagged Valid pc);
        end

        FetchCount newCount = fromMaybe(fromInteger(valueOf(FetchWidth)), fetchCount);
        totalCount   <= newCount;
        fetchPos     <= 0;

        if(newCount == 0)
            fillAddrPort(0);
        else
        begin
            fpTokenReq.send(17);
            fetchState <= Fetch;
        end
    endrule

    rule fetch(fetchState == Fetch);
        Token token <- fpTokenResp.receive();
        token.timep_info = TIMEP_TokInfo{epoch: epoch,
                                         scratchpad: 0};

        fpFetchReq.send(tuple2(token, pc));
        addrPort[fetchPos].send(tagged Valid pc);

        fetchPos  <= fetchPos + 1;
        pc        <= pc + 4;
        if(fetchPos + 1 != totalCount)
            fpTokenReq.send(17);
        else
        begin
            fillAddrPort(totalCount);
            fetchState <= FetchDone;
        end
    endrule
endmodule
