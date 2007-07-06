import hasim_common::*;
import hasim_isa::*;

import Vector::*;

import hasim_cpu_parameters::*;
import hasim_cpu_types::*;

typedef enum {Exec, Done} State deriving (Bits, Eq);

module [HASim_Module] mkPipe_Execute();
    function sendFunctionM(String str, Integer i) = mkPort_Send(strConcat(str, integerToString(i)));

    function receiveFunctionM(String str, Integer i) = mkPort_Receive(strConcat(str, integerToString(i)), 1);

    Connection_Receive#(Tuple2#(Token, InstResult)) fpExeResponse <- mkConnection_Receive("fp_exe_resp");
    Connection_Send#(Tuple2#(Token, void))               fpMemReq <- mkConnection_Send("fp_mem_req");

    Vector#(FuncUnitNum, Port_Receive#(ExecEntry))      execPort  = newVector();
    for(Integer i = 0; i < valueOf(TSub#(FuncUnitNum,1)); i=i+1)
        execPort[i] <- mkPort_Receive(strConcat("issueToExec", integerToString(i)), 1);
    execPort[valueOf(TSub#(FuncUnitNum,1))] <- mkPort_Receive(strConcat("issueToExec", integerToString(valueOf(TSub#(FuncUnitNum,1)))), 2);
    Port_Send#(Token)                               killIssuePort <- mkPort_Send("execToIssueKill");

    Vector#(FuncUnitNum, Port_Send#(ExecResult))   execResultPort <- genWithM(sendFunctionM("execToDecodeResult"));
    Port_Send#(KillData)                           killDecodePort <- mkPort_Send("execToDecodeKill");

    Reg#(FuncUnitCount)                             funcUnitCount <- mkReg(0);

    Reg#(Maybe#(KillData))                            killDataReg <- mkReg(tagged Invalid);

    rule execute(True);
        Maybe#(KillData) newKillData = tagged Invalid;
        let recvMaybe <- execPort[funcUnitCount].receive();

        case (recvMaybe) matches
            tagged Valid .recv:
            begin
                match {.token, .res} <- fpExeResponse.receive();
                $display("Mem Req: %0d", token.index);
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
                Addr takenAddr = case (res) matches
                                     tagged RBranchTaken .addr: addr;
                                     default: 0;
                                 endcase;

                ExecResult execResult = ExecResult{token: recv.token,
                                                   pRName: recv.pRName,
                                                   robTag: recv.robTag,
                                                   freeCount: recv.freeCount,
                                                   issueType: recv.issueType,
                                                   branchIndex: recv.branchIndex,
                                                   pred: recv.pred,
                                                   predAddr: recv.predAddr,
                                                   taken: taken,
                                                   takenAddr: takenAddr,
                                                   finished: finished,
                                                   status: status};

                KillData killDataVal = KillData{token: recv.token,
                                                robTag: recv.robTag,
                                                freeCount: recv.freeCount,
                                                mispredictPC: takenAddr,
                                                branchIndex: recv.branchIndex};

                if(recv.issueType == Branch && taken != recv.pred || (recv.issueType == JR || recv.issueType == JALR) && recv.predAddr != takenAddr)
                begin
                    case (killDataReg) matches
                        tagged Valid .killData:
                        begin
                            TokIndex diff = killData.token.index - recv.token.index;
                            if(diff[7] == 1)
                                newKillData = tagged Valid killDataVal;
                            else
                                newKillData = killDataReg;
                        end
                        tagged Invalid:
                            newKillData = tagged Valid killDataVal;
                    endcase
                end
                else
                    newKillData = killDataReg;

                execResultPort[funcUnitCount].send(tagged Valid execResult);
            end
            tagged Invalid:
            begin
                execResultPort[funcUnitCount].send(tagged Invalid);
                newKillData = killDataReg;
            end
        endcase

        if(funcUnitCount == fromInteger(valueOf(TSub#(FuncUnitNum, 1))))
        begin
            funcUnitCount <= 0;
            killDataReg   <= tagged Invalid;
            killDecodePort.send(newKillData);
            case (newKillData) matches
                tagged Valid .data: killIssuePort.send(tagged Valid data.token);
                tagged Invalid: killIssuePort.send(tagged Invalid);
            endcase
        end
        else
        begin
            funcUnitCount <= funcUnitCount + 1;
            killDataReg   <= newKillData;
        end
    endrule
endmodule
