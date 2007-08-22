import hasim_common::*;
import hasim_isa::*;
import hasim_cpu_parameters::*;
import hasim_cpu_types::*;

typedef enum {Fetch, FetchDone} FetchState deriving (Bits, Eq);

module [HASim_Module] mkPipe_Fetch();
    Connection_Send#(Bit#(TokenSize))      fpTokReq <- mkConnection_Send("fp_tok_req");
    Connection_Receive#(Token)            fpTokResp <- mkConnection_Receive("fp_tok_resp");
    Connection_Send#(Tuple2#(Token, Addr)) fpFetReq <- mkConnection_Send("fp_fet_req");

    Port_Receive#(FetchCount)          fetchMaxPort <- mkPort_Receive("fetchMax", 1);
    Port_Receive#(Addr)           predictedAddrPort <- mkPort_Receive("predictedAddr", 1);
    Port_Receive#(Addr)          mispredictAddrPort <- mkPort_Receive("mispredictAddr", 1);

    Port_Send#(Addr)                   instAddrPort <- mkPort_Send("instAddr");

    Reg#(FetchState)                     fetchState <- mkReg(FetchDone);
    Reg#(Addr)                                   pc <- mkReg(pcStart);
    Reg#(FetchCount)                     totalCount <- mkReg(?);
    Reg#(FetchCount)                     fetchCount <- mkReg(?);
    Reg#(Bool)                     fillFetchInvalid <- mkReg(False);

    Reg#(TIMEP_Epoch)                         epoch <- mkReg(0);

    EventRecorder                          eventFet <- mkEventRecorder("Fetch");

    rule synchronize(fetchState == FetchDone);
        eventFet.recordEvent(tagged Invalid);
        Maybe#(Addr)  predictedAddr <- predictedAddrPort.receive();
        Maybe#(Addr) mispredictAddr <- mispredictAddrPort.receive();
        Maybe#(FetchCount) fetchMax <- fetchMaxPort.receive();

        if(isValid(mispredictAddr))
        begin
            pc    <= validValue(mispredictAddr);
            epoch <= epoch + 1;
        end
        else if(isValid(predictedAddr))
            pc <= validValue(predictedAddr);

        FetchCount newCount = fromMaybe(fromInteger(valueOf(FetchWidth)), fetchMax);
        totalCount <= newCount;
        fetchState <= Fetch;
        fetchCount <= 0;

        if(newCount == 0)
            fillFetchInvalid <= True;
        else
        begin
            fpTokReq.send(17);
            fillFetchInvalid <= False;
        end
    endrule

    rule fetch(fetchState == Fetch);
        fetchCount <= fetchCount + 1;
        if(fetchCount == fromInteger(valueOf(TSub#(FetchWidth,1))))
            fetchState <= FetchDone;
        if(!fillFetchInvalid)
        begin
            Token token = fpTokResp.receive();
            fpTokResp.deq();
            token.timep_info = TIMEP_TokInfo{epoch: epoch,
                                             scratchpad: 0};

            fpFetReq.send(tuple2(token, pc));
            instAddrPort.send(tagged Valid pc);

            pc <= pc + 4;
            if(fetchCount + 1 != totalCount)
                fpTokReq.send(17);
            else
                fillFetchInvalid <= True;
        end
        else
            instAddrPort.send(tagged Invalid);
    endrule
endmodule
