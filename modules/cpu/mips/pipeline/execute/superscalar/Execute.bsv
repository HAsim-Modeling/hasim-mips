import hasim_common::*;
import hasim_isa::*;

import Vector::*;

import hasim_cpu_parameters::*;
import hasim_cpu_types::*;

typedef enum {Exec, Done} State deriving (Bits, Eq);

module [HASim_Module] mkPipe_Execute();
    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, fromInteger(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, fromInteger(i)), 1);

    Connection_Receive#(Tuple2#(Token, InstResult))           fpExeResponse <- mkConnection_Receive("fp_exe_resp");
    Connection_Send#(Tuple2#(Token, void))                         fpMemReq <- mkConnection_Send("fp_mem_req");

    Vector#(NumFuncUnits, Port_Receive#(ExecEntry))                execPort  = newVector();
    for(Integer i = 0; i < valueOf(TSub#(NumFuncUnits,1)); i=i+1)
        execPort[i] <- mkPort_Receive(strConcat("issueToExec", fromInteger(i)), 1);
    execPort[valueOf(TSub#(NumFuncUnits,1))] <- mkPort_Receive(strConcat("issueToExec", fromInteger(valueOf(TSub#(NumFuncUnits,1)))), 2);

    Vector#(NumFuncUnits, Port_Send#(ExecResult))            execResultPort <- genWithM(sendFunctionM("execToDecodeResult"));

    Port_Send#(KillData)                                         killDecode <- mkPort_Send("execToDecodeKill");
    Port_Send#(Token)                                             killIssue <- mkPort_Send("execToIssueKill");

    Reg#(FuncUnitPos)                                           funcUnitPos <- mkReg(0);

    Reg#(Maybe#(KillData))                                        killToken <- mkReg(tagged Invalid);

    rule execute(True);
        Maybe#(KillData) newKillToken = tagged Invalid;

        let recvMaybe <- execPort[funcUnitPos].receive();

        case (recvMaybe) matches
            tagged Valid .recv:
            begin
                match {.token, .res} <- fpExeResponse.receive();
                fpMemReq.send(tuple2(token, ?));
                Bool finished = case (res) matches
                                    tagged RTerminate .status: return True;
                                    default: return False;
                                endcase;
                Bool status   = case (res) matches
                                    tagged RTerminate .status: return status;
                                endcase;
                Bool taken    = case (res) matches
                                    tagged RBranchTaken .addr: True;
                                    default: False;
                                endcase;
                Addr takeAddr = case (res) matches
                                    tagged RBranchTaken .addr: addr;
                                    default: 0;
                                endcase;
                ExecResult execResult = ExecResult{token: recv.token, pRName: recv.pRName, robTag: recv.robTag, freeCount: recv.freeCount, issueType: recv.issueType, branchIndex: recv.branchIndex, pred: recv.pred, predAddr: recv.predAddr, taken: taken, takenAddr: takeAddr, finished: finished, status: status};
                KillData killDataVal = KillData{token: recv.token, robTag: recv.robTag, freeCount: recv.freeCount, mispredictPC: takeAddr, branchIndex: recv.branchIndex};

                if(recv.issueType == Branch && taken != recv.pred || (recv.issueType == JR || recv.issueType == JALR) && recv.predAddr != takeAddr)
                begin
                    case (killToken) matches
                        tagged Valid .killData:
                        begin
                            TokIndex diff = killData.token.index - recv.token.index;
                            if(diff[7] == 1)
                                newKillToken = tagged Valid killDataVal;
                            else
                                newKillToken = killToken;
                        end
                        tagged Invalid:
                            newKillToken = tagged Valid killDataVal;
                    endcase
                end
                else
                    newKillToken = killToken;

                execResultPort[funcUnitPos].send(tagged Valid execResult);
                $display("exec: %0d", token.index);
            end
            tagged Invalid:
            begin
                execResultPort[funcUnitPos].send(tagged Invalid);
                newKillToken = killToken;
            end
        endcase

        if(funcUnitPos == fromInteger(valueOf(TSub#(NumFuncUnits, 1))))
        begin
            funcUnitPos <= 0;
            killToken <= tagged Invalid;
            killDecode.send(newKillToken);
            case (newKillToken) matches
                tagged Valid .data: killIssue.send(tagged Valid data.token);
                tagged Invalid: killIssue.send(tagged Invalid);
            endcase
        end
        else
        begin
            funcUnitPos <= funcUnitPos + 1;
            killToken <= newKillToken;
        end
    endrule
endmodule
