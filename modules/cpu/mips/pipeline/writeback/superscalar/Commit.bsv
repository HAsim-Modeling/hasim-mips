import hasim_common::*;
import soft_connections::*;
import hasim_modellib::*;
import hasim_cpu_parameters::*;
`include "asim/provides/hasim_controller.bsh"

module [HASim_Module] mkPipe_Writeback();
    Port_Receive#(Token)                     commitPort <- mkPort_Receive("decodeToCommit", valueOf(CommitWidth));

    Connection_Receive#(Tuple2#(Token, void)) fpLcoResp <- mkConnection_Receive("fp_lco_resp");

    Connection_Send#(Tuple2#(Token, void))     fpGcoReq <- mkConnection_Send("fp_gco_req");
    Connection_Receive#(Tuple2#(Token, void)) fpGcoResp <- mkConnection_Receive("fp_gco_resp");

    // Number of commits (to go along with heartbeat)
    Connection_Send#(MODEL_NUM_COMMITS) linkModelCommit <- mkConnection_Send("model_commits");

    rule globalCommit(True);
        Maybe#(Token) tokenMaybe <- commitPort.receive();
        case (tokenMaybe) matches
            tagged Valid .token:
            begin
                let tok = fpLcoResp.receive();
                fpLcoResp.deq();
                fpGcoReq.send(tok);
            end
        endcase
    endrule

    rule globalCommitAck(True);
        let ack = fpGcoResp.receive();
        fpGcoResp.deq();
        linkModelCommit.send(1);
    endrule
endmodule
