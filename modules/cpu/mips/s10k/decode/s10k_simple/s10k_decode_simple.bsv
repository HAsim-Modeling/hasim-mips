import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import FIFOF::*;
import Vector::*;
import RWire::*;

interface Decode;
    method Bool done();
endinterface

typedef 4 NumInst;
typedef Bit#(TLog#(TAdd#(NumInst,1))) Count;
Addr pcStart = 0;

typedef enum {FetchOrMispredict, FetchInst, Mispredict, SendPorts} State deriving (Bits, Eq);

typedef 32 ActiveListCount;
typedef 16 IntQCount;
typedef 16 AddrQCount;
typedef 32 FreeListCount;

typedef struct {
    Token token;
    PRName src1Name;
    Bool src1Ready;
    PRName src2Name;
    Bool src2Ready;
    PRName destName;
    Bool issued;
    Bit#(32) count;
} ROBEntry deriving (Bits, Eq);

interface ShiftFIFOF#(numeric type n, type data_t);
    method Action enq(data_t data);
    method Action deq();
    method data_t first();
    method data_t getPosition(Bit#(TLog#(n)) position);
    method Bit#(TLog#(n)) size();
    method Action clear();
    method Bool notEmpty();
    method Bool notFull();
endinterface

module mkShiftFIFOF(ShiftFIFOF#(n, data_t))
    provisos(Bits#(data_t, data_sz));

    Reg#(Vector#(n, data_t)) circularBuffer <- mkReg(?);
    Reg#(Bit#(TLog#(n)))  firstPtr <- mkReg(0);
    Reg#(Bit#(TLog#(n)))   lastPtr <- mkReg(0);

    PulseWire deqEn <- mkPulseWire();
    PulseWire enqEn <- mkPulseWire();

    rule updateFirstPtr(deqEn);
        firstPtr <= firstPtr + 1;
    endrule

    rule updateLastPtr(enqEn);
        lastPtr  <= lastPtr + 1;
    endrule

    method Action enq(data_t data) if (lastPtr + 1 != firstPtr);
        circularBuffer[lastPtr] <= data;
        enqEn.send();
    endmethod

    method Action deq() if (firstPtr != lastPtr);
        deqEn.send();
    endmethod

    method data_t first() if (firstPtr != lastPtr);
        return circularBuffer[firstPtr];
    endmethod

    method data_t getPosition(Bit#(TLog#(n)) position);
        return circularBuffer[position + firstPtr];
    endmethod

    method Bit#(TLog#(n)) size();
        return lastPtr - firstPtr;
    endmethod

    method Action clear();
        lastPtr  <= 0;
        firstPtr <= 0;
    endmethod

    method Bool notEmpty();
        return (firstPtr != lastPtr);
    endmethod

    method Bool notFull();
        return (lastPtr + 1 != firstPtr);
    endmethod
endmodule

module [HASim_Module] mkDecode(Decode);
    Connection_Receive#(Tuple2#(Token, PackedInst)) fpFetchResp <- mkConnection_Receive("fp_fet_resp");

    Connection_Send#(Tuple2#(Token, void))          fpDecodeReq <- mkConnection_Send("fp_dec_req");
    Connection_Receive#(Tuple2#(Token, DepInfo))   fpDecodeResp <- mkConnection_Receive("fp_dec_resp");

    Port_Receive#(Tuple2#(Token, void))          mispredictPort <- mkPort_Receive("execToDecode_Mispredicted", 1);
    Port_Send#(Count)                             decodeNumPort <- mkPort_Send("decodeToFetch_DecodeNum");
    Port_Send#(Addr)                         predictedTakenPort <- mkPort_Send("decodeToFetch_PredictedTaken");

    function receiveFunctionM(Integer i) = mkPort_Receive(strConcat("fetchToDecode_TokenAddr", fromInteger(i)), 1);
    Vector#(NumInst, Port_Receive#(Tuple2#(Token, Addr))) tokenAddrPort <- genWithM(receiveFunctionM);

    Reg#(State)                                           state <- mkReg(Finish);

    Reg#(Count)                                      fetchCount <- mkReg(?);
    Reg#(Count)                                     decodeCount <- mkReg(fromInteger(valueOf(NumInst)));
    Reg#(Maybe#(Addr))                              predictedPC <- mkReg(tagged Invalid);

    Reg#(TLog#(TAdd#(ActiveListCount,1)))       activeListCount <- mkReg(fromInteger(valueOf(ActiveListCount)));
    Reg#(TLog#(TAdd#(FreeListCount,1)))           freeListCount <- mkReg(fromInteger(valueOf(FreeListCount)));
    Reg#(TLog#(TAdd#(IntQCount,1)))                   intQCount <- mkReg(fromInteger(valueOf(IntQCount)));
    Reg#(TLog#(TAdd#(AddrQCount,1)))                 addrQCount <- mkReg(fromInteger(valueOf(AddrQCount)));

    PulseWire activeListCountInc             activeListCountInc <- mkPulseWire();
    PulseWire activeListCountDec             activeListCountDec <- mkPulseWire();

    PulseWire freeListCountInc                 freeListCountInc <- mkPulseWire();
    PulseWire freeListCountDec                 freeListCountDec <- mkPulseWire();

    PulseWire intQCountInc                         intQCountInc <- mkPulseWire();
    PulseWire intQCountDec                         intQCountDec <- mkPulseWire();

    PulseWire addrQCountInc                       addrQCountInc <- mkPulseWire();
    PulseWire addrQCountDec                       addrQCountDec <- mkPulseWire();

    FIFOF#(Tuple2#(Token, Addr))                tokenAddrBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(NumInst)));
    Reg#(Bool)                                   killInstBuffer <- mkReg(False);
    Reg#(Bool)                               nextKillInstBuffer <- mkReg(?);

    FIFOF#(PackedInst)                               instBuffer <- mkFIFOF();
    FIFOF#(DepInfo)                                decodeBuffer <- mkFIFOF();

    ShiftFIFOF#(ActiveListCount, Reg#(ROBEntry))            rob <- mkShiftFIFOF();

    rule getFetchResp(True);
        let instTuple <- fpFetchResp.receive();
        instBuffer.enq(tpl_2(instTuple));
    endrule

    rule getDecodeResp(True);
        let decodeTuple <- fpDecodeResp.receive();
        decodeBuffer.enq(tpl_2(decodeTuple));
    endrule

    rule updateActiveListCount(True);
        if(!activeListCountInc && activeListCountDec)
            activeListCount <= activeListCount - 1;
        else if(activeListCountInc && !activeListCountDec)
            activeListCount <= activeListCount + 1;
    endrule

    rule updateFreeListCount(True);
        if(!freeListCountInc && freeListCountDec)
            freeListCount <= freeListCount - 1;
        else if(freeListCountInc && !freeListCountDec)
            freeListCount <= freeListCount + 1;
    endrule

    rule updateIntQCount(True);
        if(!intQCountInc && intQCountDec)
            intQCount <= intQCount - 1;
        else if(intQCountInc && !intQCountDec)
            intQCount <= intQCount + 1;
    endrule

    rule updateAddrQCount(True);
        if(!addrQCountInc && addrQCountDec)
            addrQCount <= addrQCount - 1;
        else if(addrQCountInc && !addrQCountDec)
            addrQCount <= addrQCount + 1;
    endrule

    rule sendPorts(state == SendPorts);
        decodeNumPort.send(tagged Valid decodeCount);
        predictedTakenPort.send(predictedPC);
        state <= FetchOrMispredict;
    endrule

    function Action killFetchDecode(Token tok);
    action
        noAction;
    endaction
    endfunction

    rule instKill(state == FetchInst && killInstBuffer && instBuffer.notEmpty());
        instBuffer.deq();
        decodeBuffer.deq();
        tokenAddrBuffer.deq();
        match {.token, .addr} = tokenAddrBuffer.first();
        killFetchDecode(token);
    endrule

    rule instKillFinish(state == FetchInst && killInstBuffer && !instBuffer.notEmpty());
        killInstBuffer <= nextKillInstBuffer;
        state <= SendPorts;
    endrule

    rule fetchOrMispredict(state == FetchOrMispredict);
        let tokenAddr  <- tokenAddrPort[0].receive();
        let mispredict <- mispredictPort.receive();
        if(isValid(mispredict))
        begin
            state <= Mispredict;
        end
        else if(isValid(tokenAddr))
        begin
            fpDecodeReq.send(tuple2(tpl_1(validValue(tokenAddr)), ?));
            tokenAddrBuffer.enq(validValue(tokenAddr));
            state       <= FetchInst;

            fetchCount  <= 1;

            //Set it in later stages if this is not valid
            decodeCount <= 0;
            predictedPC <= tagged Invalid;
            nextKillInstBuffer <= False;
        end
    endrule

    rule fetchInst(state == FetchInst && fetchCount != fromInteger(valueOf(NumInst)));
        fetchCount    <= fetchCount + 1;
        let tokenAddr <- tokenAddrPort[fetchCount].receive();
        if(isValid(tokenAddr))
        begin
            fpDecodeReq.send(tuple2(tpl_1(validValue(tokenAddr)), ?));
            tokenAddrBuffer.enq(validValue(tokenAddr));
        end
    endrule

    rule dispatchALU(decodeCount != fromInteger(valueOf(NumInst)) &&
                     freeListCount != 0 && activeListCount != 0 && intQCount != 0);
        tokenAddrBuffer.deq();
        instBuffer.deq();
        decodeBuffer.deq();

        match {.token, .addr} = tokenAddrBuffer.first();
        let inst              = instBuffer.first();
        let dep               = dep.first();

        decodeCount <= decodeCount + 1;

        let robEntry = ROBEntry {
                           token    : token,
                           src1Name : tpl_2(validValue(dep.dep_src1)),
                           src1Ready: !isValid(dep.dep_src1),
                           src2Name : tpl_2(validValue(dep.dep_src2)),
                           src2Ready: !isValid(dep.dep_src2),
                           destName : tpl_2(validValue(dep.dep_dest)),
                           issued   : False,
                           count    : ?
                       };

        rob.enq(robEntry);

        activeListCountDec.send();
        freeListCountDec.send();
        intQCountDec.send();
    endrule

endmodule

/*
module [HASim_Module] mkDecode
    //interface:
                (Decode);

  Connection_Receive#(Tuple2#(Token, PackedInst)) fp_fet_resp <- mkConnection_Receive("fp_fet_resp");

  Connection_Send#(Tuple2#(Token, void))        fp_dec_req  <- mkConnection_Send("fp_dec_req");
  Connection_Receive#(Tuple2#(Token, DepInfo))  fp_dec_resp <- mkConnection_Receive("fp_dec_resp");

  Connection_Send#(Tuple2#(Token, void))    fp_lco_req  <- mkConnection_Send("fp_lco_req");
  Connection_Receive#(Tuple2#(Token, void)) fp_lco_resp <- mkConnection_Receive("fp_lco_resp");
  
  Connection_Send#(Tuple2#(Token, void))    fp_gco_req  <- mkConnection_Send("fp_gco_req");
  Connection_Receive#(Tuple2#(Token, void)) fp_gco_resp <- mkConnection_Receive("fp_gco_resp");
    
  Connection_Send#(Token) link_memstate_kill <- mkConnection_Send("fp_memstate_kill");

  Connection_Send#(Token)     fpTokenKill <- mkConnection_Send("fp_tok_kill");
  Connection_Send#(Token)     fp_fet_kill <- mkConnection_Send("fp_fet_kill");
  Connection_Send#(Token)     fp_dec_kill <- mkConnection_Send("fp_dec_kill");
  Connection_Send#(Token)     fp_exe_kill <- mkConnection_Send("fp_exe_kill");
  Connection_Send#(Token)     fp_mem_kill <- mkConnection_Send("fp_mem_kill");
  Connection_Send#(Token)     fp_lco_kill <- mkConnection_Send("fp_lco_kill");
  Connection_Send#(Token)     fp_gco_kill <- mkConnection_Send("fp_gco_kill");

  Connection_Send#(Token)     fp_rewindToToken <- mkConnection_Send("fp_rewindToToken");

  Port_Receive#(Bit#(32))     port <- mkPort_Receive("fetch_to_decode", 1);

  Reg#(Bit#(32)) modelCC <- mkReg(0);
  Reg#(Bit#(32)) fpgaCC  <- mkReg(0);

    rule alway(True);
        fpgaCC <= fpgaCC + 1;
    endrule

    rule receive(True);
        let s <- port.receive();
        case (s) matches
            tagged Invalid : $display("%d %d :Decode: received nothing", fpgaCC, modelCC);
            tagged Valid .v: $display("%d %d :Decode: received %d", fpgaCC, modelCC, v);
        endcase
        modelCC <= modelCC + 1;
    endrule

    method Bool done();
        return False;
    endmethod

endmodule
*/
