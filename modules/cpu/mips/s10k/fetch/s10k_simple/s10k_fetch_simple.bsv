import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import FIFOF::*;

typedef 4 NumInst;
typedef Bit#(TLog#(TAdd#(NumInst,1))) Count;
Addr pcStart = 0;

//No branch predictor in the fetch stage. No pipelining
module [HASim_Module] mkFetch();

    Connection_Send#(Bit#(8))              fpTokenReq  <- mkConnection_Send("fp_tok_req");
    Connection_Receive#(Token)             fpTokenResp <- mkConnection_Receive("fp_tok_resp");

    Connection_Send#(Tuple2#(Token, Addr)) fpFetchReq  <- mkConnection_Send("fp_fet_req");

    Port_Send#(Tuple2#(Token, Addr))     tokenAddrPort <- mkPort_Send("fetchToDecode_TokenAddr");

    Port_Receive#(Count)                 decodeNumPort <- mkPort_Receive("decodeToFetch_DecodeNum", 1);
    Port_Receive#(Addr)             predictedTakenPort <- mkPort_Receive("decodeToFetch_PredictedTaken", 1);
    Port_Receive#(Addr)                 mispredictPort <- mkPort_Receive("executeToFetch_Mispredict", 1);

    Reg#(Addr)                                      pc <- mkReg(pcStart);
    Reg#(Count)                                  count <- mkReg(0);
    Reg#(Bit#(32))                             latency <- mkReg(?); //Actual model latency * 4
    Reg#(Count)                              remaining <- mkReg(0);

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

    rule start(count == 0 && remaining == 0);
        let predictedTaken <- predictedTakenPort.receive();
        let     mispredict <- mispredictPort.receive();
        let      decodeNum <- decodeNumPort.receive();

        Addr newPC = ?;
        if(isValid(mispredict))
            newPC = validValue(mispredict);
        else if(isValid(predictedTaken))
            newPC = validValue(predictedTaken);
        else
            newPC = pc;

        pc           <= newPC;

        let newCount = getCacheBoundary(newPC, decodeNum);
        count        <= newCount;
        remaining    <= fromInteger(valueOf(NumInst)) - newCount;

        latency      <= getHostLatency();

        if(newCount != 0)
            fpTokenReq.send(17);
        $display("Fetch begins for %d instructions", count);
    endrule

    rule missWait(latency != 0 && count != 0);
        latency <= latency - 1;
    endrule

    function Action doFetch();
    action
        let token <- fpTokenResp.receive();
        fpFetchReq.send(tuple2(token, pc));
        tokenAddrPort.send(tagged Valid tuple2(token, pc));
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
        if(remaining == 0)
            $display("Fetch done");
    endrule

    rule finishOverall(latency == 0 && count == 0 && remaining != 0);
        tokenAddrPort.send(tagged Invalid);
        remaining <= remaining - 1;
        if(remaining == 1)
            $display("Fetch done");
    endrule

endmodule
