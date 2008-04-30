import RegFile::*;
import FIFO::*;

import hasim_common::*;
import soft_connections::*;
import hasim_modellib::*;
import hasim_isa::*;

import hasim_cpu_parameters::*;
import hasim_cpu_types::*;

typedef enum {Exec, Done} State deriving (Bits, Eq);

module [HASim_Module] mkPipe_Execute();
    Connection_Receive#(Tuple2#(Token, InstResult)) fpExeResp <- mkConnection_Receive("fp_exe_resp");
    Connection_Send#(Tuple2#(Token, void))           fpMemReq <- mkConnection_Send("fp_mem_req");

    Port_Receive#(ExecEntry)                         execPort <- mkPort_Receive("exec", valueOf(TSub#(FuncUnitNum,1)));
    Port_Receive#(ExecEntry)                          memPort <- mkPort_Receive("mem", 2);
    Port_Send#(Token)                           killIssuePort <- mkPort_Send("killIssue");

    Port_Send#(ExecResult)                     execResultPort <- mkPort_Send("execResult");
    Port_Send#(KillData)                       killDecodePort <- mkPort_Send("killDecode");

    Reg#(FuncUnitCount)                         funcUnitCount <- mkReg(0);
    Reg#(FuncUnitCount)                        execEntryCount <- mkReg(0);

    Reg#(Maybe#(KillData))                        killDataReg <- mkReg(tagged Invalid);

    RegFile#(Bit#(TLog#(RobNum)), Bool)             execValid <- mkRegFileFull();
    RegFile#(Bit#(TLog#(RobNum)), InstResult)      instResult <- mkRegFileFull();

    FIFO#(Maybe#(ExecEntry))                        execEntry <- mkFIFO();

    Reg#(Bool)                                   initializing <- mkReg(False);
    Reg#(RobTag)                            initializingCount <- mkReg(0);

    rule initialize(initializing);
        execValid.upd(truncate(initializingCount), False);
        initializingCount <= initializingCount + 1;
        if(initializingCount == fromInteger(valueOf(TSub#(RobNum,1))))
            initializing <= False;
    endrule

    rule fillInstResult(!initializing);
        match {.token, .res} = fpExeResp.receive();
        fpExeResp.deq();
        execValid.upd(token.timep_info.scratchpad, True);
        instResult.upd(token.timep_info.scratchpad, res);
    endrule

    rule fillExecEntry(True);
        if(execEntryCount == fromInteger(valueOf(TSub#(FuncUnitNum,1))))
        begin
            execEntryCount <= 0;
            Maybe#(ExecEntry) recvMaybe <- memPort.receive();
            execEntry.enq(recvMaybe);
        end
        else
        begin
            execEntryCount <= execEntryCount + 1;
            Maybe#(ExecEntry) recvMaybe <- execPort.receive();
            execEntry.enq(recvMaybe);
        end
    endrule

    rule execute(!initializing && (!isValid(execEntry.first()) || execValid.sub(truncate(validValue(execEntry.first()).robTag))));
        Maybe#(KillData) newKillData = tagged Invalid;
        Maybe#(ExecEntry) recvMaybe = execEntry.first();
        execEntry.deq();

        case (recvMaybe) matches
            tagged Valid .recv:
            begin
                execValid.upd(truncate(recv.robTag), False);
                let res = instResult.sub(truncate(recv.robTag));
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
                let takenAddr = case (res) matches
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
            funcUnitCount <= funcUnitCount + 1;
            killDataReg   <= newKillData;
        end
    endrule
endmodule
