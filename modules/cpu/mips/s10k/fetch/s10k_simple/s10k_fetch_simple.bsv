import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import FIFOF::*;
import Vector::*;

typedef 4 NumInst;
typedef Bit#(TLog#(TAdd#(NumInst,1))) Count;
Addr pcStart = 0;

//No branch predictor in the fetch stage. No pipelining
module [HASim_Module] mkFetch();

    Connection_Send#(Bit#(8))              fpTokenReq  <- mkConnection_Send("fp_tok_req");
    Connection_Receive#(Token)             fpTokenResp <- mkConnection_Receive("fp_tok_resp");

    Connection_Send#(Tuple2#(Token, Addr)) fpFetchReq  <- mkConnection_Send("fp_fet_req");

    Port_Receive#(Count)                 decodeNumPort <- mkPort_Receive("decodeToFetch_DecodeNum", 1);
    Port_Receive#(Addr)             predictedTakenPort <- mkPort_Receive("decodeToFetch_PredictedTaken", 1);
    Port_Receive#(Addr)                 mispredictPort <- mkPort_Receive("executeToFetch_Mispredict", 1);

    function sendFunctionM(Integer i) = mkPort_Send(strConcat("fetchToDecode_TokenAddr", fromInteger(i)));
    Vector#(NumInst, Port_Send#(Tuple2#(Token, Addr))) tokenAddrPort <- genWithM(sendFunctionM);

    Reg#(Addr)                                      pc <- mkReg(pcStart);
    Reg#(Count)                                  count <- mkReg(0);
    Reg#(Bit#(32))                             latency <- mkReg(?); //Actual model latency * 4
    Reg#(Count)                           instPosition <- mkReg(fromInteger(valueOf(NumInst))); //This triggers only start rule

    //Returns model latency * 4
    function Bit#(32) getHostLatency();
        return 0;
    endfunction

    function Count getCacheBoundary(Addr pcPassed, Maybe#(Count) decodeNum);
        case (decodeNum) matches
            tagged Invalid   : return fromInteger(valueOf(NumInst));
            tagged Valid .num: return num; 
        endcase
    endfunction

    rule start(count == 0 && instPosition == fromInteger(valueOf(NumInst)));
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
        count        <= newCount;
        instPosition <= 0;


        if(newCount != 0)
            fpTokenReq.send(17);
        $display("Fetch begins for %d instructions", count);
    endrule

    rule missWait(latency != 0 && count != 0);
        latency <= latency - 1;
        for(Integer i = 0; i < valueOf(NumInst); i=i+1)
            tokenAddrPort[i].send(tagged Invalid);
    endrule

    function Action doFetch();
    action
        let token <- fpTokenResp.receive();
        fpFetchReq.send(tuple2(token, pc));
        tokenAddrPort[instPosition].send(tagged Valid tuple2(token, pc));
        instPosition <= instPosition + 1;
        count <= count - 1;
        pc <= pc + 4;
    endaction
    endfunction

    rule middleInsts(latency == 0 && count != 1);  //if count = 0, then we wont receive anything from the token, so this rule wont fire
        doFetch();
        fpTokenReq.send(17);
    endrule

    rule finishInsts(latency == 0 && count == 1);
        doFetch();
    endrule

    rule finishOverall(latency == 0 && count == 0 && instPosition != fromInteger(valueOf(NumInst)));
        instPosition <= instPosition + 1;
        tokenAddrPort[instPosition].send(tagged Invalid);
    endrule

    rule displayDone(instPosition == fromInteger(valueOf(NumInst)) - 1);
        $display("Fetch done");
    endrule

endmodule
