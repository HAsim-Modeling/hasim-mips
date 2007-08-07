import RegFile::*;
import Vector::*;
import FIFO::*;

import hasim_common::*;
import hasim_isa::*;

import hasim_cpu_parameters::*;
import hasim_cpu_types::*;

typedef enum {Exec, Done} State deriving (Bits, Eq);

module [HASim_Module] mkPipe_Execute();
    Connection_Receive#(Tuple2#(Token, InstResult)) fpExeResp <- mkConnection_Receive("fp_exe_resp");
    Connection_Send#(Tuple2#(Token, void))           fpMemReq <- mkConnection_Send("fp_mem_req");

    Port_Receive#(ExecEntry)                         execPort <- mkPort_Receive("issueToExec", valueOf(TSub#(FuncUnitNum,1)));
    Port_Receive#(ExecEntry)                          memPort <- mkPort_Receive("issueToExecMem", 2);
    Port_Send#(Token)                           killIssuePort <- mkPort_Send("execToIssueKill");

    Port_Send#(ExecResult)                     execResultPort <- mkPort_Send("execToDecodeResult");
    Port_Send#(KillData)                       killDecodePort <- mkPort_Send("execToDecodeKill");

    Reg#(FuncUnitCount)                         funcUnitCount <- mkReg(0);

    Reg#(Maybe#(KillData))                        killDataReg <- mkReg(tagged Invalid);

    Reg#(Vector#(RobNum, Bool))                     execValid <- mkReg(replicate(False));
    RegFile#(Bit#(TLog#(RobNum)), InstResult)      instResult <- mkRegFileFull();

    FIFO#(Maybe#(ExecEntry))                        execEntry <- mkFIFO();

    rule fillInstResult(True);
        match {.token, .res} = fpExeResp.receive();
	fpExeResp.deq();
        execValid[token.timep_info.scratchpad] <= True;
        instResult.upd(token.timep_info.scratchpad, res);
        $display("got something: %0d %0d", token.index, token.timep_info.scratchpad);
    endrule

    rule fillExecEntry(True);
        if(funcUnitCount == fromInteger(valueOf(TSub#(FuncUnitNum,1))))
        begin
            Maybe#(ExecEntry) recvMaybe <- memPort.receive();
            execEntry.enq(recvMaybe);
        end
        else
        begin
            Maybe#(ExecEntry) recvMaybe <- execPort.receive();
            execEntry.enq(recvMaybe);
        end
        $display("fill something");
    endrule

    rule execute(!isValid(execEntry.first()) || execValid[(validValue(execEntry.first())).robTag]);
        Maybe#(KillData) newKillData = tagged Invalid;
        Maybe#(ExecEntry) recvMaybe = execEntry.first();
        execEntry.deq();

        case (recvMaybe) matches
            tagged Valid .recv:
            begin
                execValid[recv.robTag] <= False;
                InstResult res = instResult.sub(truncate(recv.robTag));
                // match {.token, .res} <- fpExeResp.receive();
                $display("adding %0d %0d %0d %0d", recv.robTag, recv.token.index, funcUnitCount);
                //token.timep_info.scratchpad = truncate(recv.robTag);
                fpMemReq.send(tuple2(recv.token, ?));

                Bool finished = case (res) matches
                                    tagged RTerminate .status: return True;
                                    default: return False;
                                endcase;
                Bool status   = case (res) matches
                                    tagged RTerminate .status: return status;
                                endcase;
                Bool taken    = case (res) matches
                                    tagged RBranchTaken .addr: return True;
                                    default: return False;
                                endcase;
                Addr takenAddr = case (res) matches
                                     tagged RBranchTaken .addr: return addr;
                                     default: return recv.addr + 4;
                                 endcase;

                ExecResult execResult = ExecResult{token: recv.token,
                                                   addr: recv.addr,
                                                   pRName: recv.pRName,
                                                   robTag: recv.robTag,
                                                   issueType: recv.issueType,
                                                   branchIndex: recv.branchIndex,
                                                   pred: recv.pred,
                                                   predAddr: recv.predAddr,
                                                   taken: taken,
                                                   takenAddr: takenAddr,
                                                   finished: finished && recv.issueType == Finish,
                                                   status: status};

                KillData killDataVal = KillData{token: recv.token,
                                                robTag: recv.robTag,
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

                execResultPort.send(tagged Valid execResult);
            end
            tagged Invalid:
            begin
                
                execResultPort.send(tagged Invalid);
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
            if(funcUnitCount == 0)
                $display("3 %0d", $time);
            funcUnitCount <= funcUnitCount + 1;
            killDataReg   <= newKillData;
        end
    endrule
endmodule
