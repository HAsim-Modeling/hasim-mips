import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import Vector::*;

import hasim_cpu_types::*;
import hasim_cpu_parameters::*;

typedef enum {Fetch, FetchDone} FetchState deriving (Bits, Eq);

module [HASim_Module] mkFetch();
    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, fromInteger(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, fromInteger(i)), 1);

    Connection_Send#(Bit#(8))               fpTokenReq <- mkConnection_Send("fp_tok_req");
    Connection_Receive#(Token)             fpTokenResp <- mkConnection_Receive("fp_tok_resp");
    Connection_Send#(Tuple2#(Token, Addr))  fpFetchReq <- mkConnection_Send("fp_fet_req");

    Port_Receive#(FetchCount)            decodeNumPort <- mkPort_Receive("decodeToFetchDecodeNum", 1);
    Port_Receive#(Addr)             predictedTakenPort <- mkPort_Receive("decodeToFetchPredictedTaken", 1);
    Port_Receive#(Addr)                 mispredictPort <- mkPort_Receive("decodeToFetchMispredict", 1);

    Vector#(FetchWidth, Port_Send#(Tuple2#(Token, Addr)))
                                         tokenAddrPort <- genWithM(sendFunctionM("fetchToDecode"));

    Reg#(FetchState)                        fetchState <- mkReg(FetchDone);
    Reg#(Addr)                                      pc <- mkReg(pcStart);
    Reg#(FetchCount)                        totalCount <- mkReg(?);
    Reg#(FetchCount)                          fetchPos <- mkReg(?);

    Reg#(ClockCounter)                    modelCounter <- mkReg(0);

    function fillTokenAddrPort(FetchCount fetchedCount);
    action
        for(Integer i = 0; i < valueof(FetchWidth); i=i+1)
        begin
            if(fromInteger(i) >= fetchedCount)
                tokenAddrPort[i].send(tagged Invalid);
        end
    endaction
    endfunction

    rule synchronize(fetchState == FetchDone);
        modelCounter <= modelCounter + 1;

        let predictedTaken <- predictedTakenPort.receive();
        let     mispredict <- mispredictPort.receive();
        let      decodeNum <- decodeNumPort.receive();

        if(isValid(mispredict))
            pc <= validValue(mispredict);
        else if(isValid(predictedTaken))
            pc <= validValue(predictedTaken);
        else
            pc <= pc;

        let newCount  = isValid(decodeNum)? validValue(decodeNum): fromInteger(valueOf(FetchWidth));
        totalCount   <= newCount;
        fetchPos     <= 0;
        $display("Fetch synchronize @ Model: %0d newCount: %0d", modelCounter, newCount);

        if(newCount == 0)
            fillTokenAddrPort(0);
        else
        begin
            fpTokenReq.send(17);
            fetchState <= Fetch;
        end
    endrule

    rule fetch(fetchState == Fetch);
        let token <- fpTokenResp.receive();
        fpFetchReq.send(tuple2(token, pc));
        tokenAddrPort[fetchPos].send(tagged Valid tuple2(token, pc));
        $display("Fetch: Token: %0d @ Model: %0d", token.index, modelCounter-1);
        let newFetchPos = fetchPos + 1;
        fetchPos  <= newFetchPos;
        pc        <= pc + 4;
        if(newFetchPos != totalCount)
            fpTokenReq.send(17);
        else
        begin
            fillTokenAddrPort(totalCount);
            fetchState <= FetchDone;
        end
    endrule
endmodule