import hasim_base::*;
import hasim_fpgalib::*;
import hasim_common::*;
import hasim_isa::*;

import FIFOF::*;
import Vector::*;

interface Decode;
    method Bool done();
endinterface

typedef 4 NumInst;
typedef Bit#(TLog#(TAdd#(NumInst,1))) Count;
Addr pcStart = 0;

typedef Bit#(TLog#(TAdd#(TMul#(2,NumInst),1))) InstBufferCount;

typedef enum {FetchInst, DecodeInst, Finish} State deriving (Bits, Eq);

module [HASim_Module] mkDecode(Decode);
    Connection_Receive#(Tuple2#(Token, PackedInst)) fpFetchResp <- mkConnection_Receive("fp_fet_resp");

    Connection_Send#(Tuple2#(Token, void))          fpDecodeReq <- mkConnection_Send("fp_dec_req");
    Connection_Receive#(Tuple2#(Token, DepInfo))   fpDecodeResp <- mkConnection_Receive("fp_dec_resp");

    Port_Receive#(Tuple2#(Token, void))        mispredictedPort <- mkPort_Receive("execToDecode_Mispredicted", 1);
    Port_Send#(Count)                             decodeNumPort <- mkPort_Send("decodeToFetch_DecodeNum");
    Port_Send#(Addr)                         predictedTakenPort <- mkPort_Send("decodeToFetch_PredictedTaken");

    function receiveFunctionM(Integer i) = mkPort_Receive(strConcat("fetchToDecode_TokenAddr", fromInteger(i)), 1);
    Vector#(NumInst, Port_Receive#(Tuple2#(Token, Addr))) tokenAddrPort <- genWithM(receiveFunctionM);

    Reg#(State)                                           state <- mkReg(Finish);

    Reg#(Count)                                      fetchCount <- mkReg(?);
    Reg#(Count)                                     decodeCount <- mkReg(fromInteger(valueOf(NumInst)));
    Reg#(Maybe#(Addr))                              predictedPC <- mkReg(tagged Invalid);
    //Reg#(LogFreeListSize)                         freeListCount <- mkReg(valueOf(FreeListSize));
    //Reg#(LogActiveListSize)                     activeListCount <- mkReg(valueOf(ActiveListSize));
    //Reg#(LogIntQSize)                                 intQCount <- mkReg(valueOf(IntQSize));
    //Reg#(LogAddrQSize)                               addrQCount <- mkReg(valueOf(AddrQSize));

    FIFOF#(Tuple2#(Token, Addr))                     instBuffer <- mkSizedFIFOF(2*fromInteger(valueOf(NumInst)));
    Reg#(Bool)                                   killInstBuffer <- mkReg(False);
    Reg#(Bool)                               nextKillInstBuffer <- mkReg(?);

    function Action killFetchDecode(Token tok);
    action
        noAction;
    endaction
    endfunction

    function isJump(Inst inst) = True;
    function isBranch(Inst inst) = True;
    function isPredictedTaken(Addr addr) = True;
    function getPredictedPC(Inst inst, Addr addr) = tagged Invalid;

    rule replyToFetch(state == Finish);
        decodeNumPort.send(tagged Valid decodeCount);
        predictedTakenPort.send(predictedPC);
        fetchCount <= 0;
        state      <= FetchInst;
    endrule

    rule fetchInst(state == FetchInst && fetchCount < fromInteger(valueOf(NumInst)));
        fetchCount       <= fetchCount + 1;
        let tokenAddr    <- tokenAddrPort[fetchCount].receive();
        let mispredicted <- mispredictedPort.receive();
        if(isValid(tokenAddr))
        begin
            fpDecodeReq.send(tuple2(tpl_1(validValue(tokenAddr)), ?));
            instBuffer.enq(validValue(tokenAddr));
        end
        if(fetchCount == fromInteger(valueOf(NumInst)) - 1)
            state <= DecodeInst;

        //Set it in later stages if this is not valid
        decodeCount <= fromInteger(valueOf(NumInst));
        predictedPC <= tagged Invalid;
        nextKillInstBuffer <= False;
    endrule

    rule decodeInstKillInst(state == DecodeInst && killInstBuffer && instBuffer.notEmpty());
        match {.token, .inst}  <- fpFetchResp.receive();
        let decodeResp <- fpDecodeResp.receive();
        instBuffer.deq();
        killFetchDecode(token);
    endrule

    rule decodeInstKillInstFinish(state == DecodeInst && killInstBuffer && !instBuffer.notEmpty());
        killInstBuffer <= nextKillInstBuffer;
        state <= Finish;
    endrule

    rule decodeInstNormal(state == DecodeInst && !killInstBuffer);
        match{.token, .inst}      <- fpFetchResp.receive();
        match{.tokenInst, .addr}   = instBuffer.first();
        match{.tokenDecode, .dep} <- fpDecodeResp.receive();
        instBuffer.deq();

        decodeCount <= decodeCount + 1;

        if(isJump(inst) || (isBranch(inst) && isPredictedTaken(addr)))
        begin
            nextKillInstBuffer <= True;
            killInstBuffer     <= True;
            predictedPC        <= getPredictedPC(inst, addr);
        end

        if(decodeCount == fromInteger(valueOf(NumInst))-1)
            state <= Finish;
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
