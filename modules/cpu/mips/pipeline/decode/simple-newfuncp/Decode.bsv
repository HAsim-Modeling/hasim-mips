import FIFO::*;
import Vector::*;

import hasim_common::*;
import soft_connections::*;
import hasim_modellib::*;
import hasim_isa::*;

import hasim_local_controller::*;

`include "asim/dict/STREAMS_EVENTS_DECODE.bsh"

//AWB Parameters           default:
//DEC_PIPELINE_IS_BYPASSED   True

module [HASim_Module] mkPipe_Decode#(File debug_file, Tick curTick)
    //interface:
                ();


  //Local State
  Reg#(Bool)      in_flight   <- mkReg(False);
  FIFO#(Tuple3#(Maybe#(Addr), Bool, Bool)) infoQ   <- mkFIFO();
  
  //Scoreboard
  Reg#(Maybe#(ISA_DEPENDENCY_INFO)) exe_stall_info <- mkReg(tagged Invalid);
  Reg#(Maybe#(ISA_DEPENDENCY_INFO)) mem_stall_info <- mkReg(tagged Invalid);
  Reg#(Maybe#(ISA_DEPENDENCY_INFO))  wb_stall_info <- mkReg(tagged Invalid);
  Reg#(Bool) exe_is_load <- mkReg(False);
  Vector#(3, Reg#(Maybe#(Tuple5#(Token, ISA_DEPENDENCY_INFO, Maybe#(Addr), Bool, Bool)))) stall_toks = newVector();
  stall_toks[0] <- mkReg(Invalid);
  stall_toks[1] <- mkReg(Invalid);
  stall_toks[2] <- mkReg(Invalid);

  //Connections to FP
  Connection_Send#(Token)        fp_dec_req  <- mkConnection_Send("funcp_getDependencies_req");
  Connection_Receive#(Tuple2#(Token, ISA_DEPENDENCY_INFO))  fp_dec_resp <- mkConnection_Receive("funcp_getDependencies_resp");
  
  //Events
  EventRecorder event_dec <- mkEventRecorder(`STREAMS_EVENTS_DECODE_INSTRUCTION_DECODE);
  
  //Incoming Ports
  Port_Receive#(Tuple3#(Token, Maybe#(Addr), Inst)) port_from_fet <- mkPort_Receive("fet_to_dec", 1);

  //Outgoing Ports
  Port_Send#(Tuple4#(Token, Maybe#(Addr), Bool, Bool))      port_to_exe <- mkPort_Send("dec_to_exe");

  //Local Controller
  Vector#(1, Port_Control) inports  = newVector();
  Vector#(1, Port_Control) outports = newVector();
  inports[0]  = port_from_fet.ctrl;
  outports[0] = port_to_exe.ctrl;
  LocalController local_ctrl <- mkLocalController(inports, outports);


  //Stall functions

  function Action shiftDepInfo(Maybe#(ISA_DEPENDENCY_INFO) mdeps);
  action
  
    exe_stall_info <= mdeps;
    mem_stall_info <= exe_stall_info;
    wb_stall_info <= mem_stall_info;
  
  endaction
  endfunction

  function Action manageStalls(Bit#(2) k, Maybe#(Tuple5#(Token, ISA_DEPENDENCY_INFO, Maybe#(Addr), Bool, Bool)) newStall);
  action

    case (newStall) matches
      tagged Invalid:
      begin
        stall_toks[2] <= tagged Invalid;
        stall_toks[1] <= stall_toks[2];
        stall_toks[0] <= stall_toks[1];
      end
      tagged Valid .v:
      begin
        case (k)
          0:
          begin
            stall_toks[2] <= tagged Invalid;
            stall_toks[1] <= stall_toks[2];
            stall_toks[0] <= tagged Valid v;
          end
          1:
          begin
            stall_toks[2] <= tagged Invalid;
            stall_toks[1] <= tagged Valid v;
            stall_toks[0] <= stall_toks[1];
          end
          default:
          begin
            stall_toks[2] <= tagged Valid v;
            stall_toks[1] <= stall_toks[2];
            stall_toks[0] <= stall_toks[1];
          end
        endcase
      end
    endcase

  endaction
  endfunction
  
  function Bit#(2) longest_stalled_token();
  
    return isValid(stall_toks[2]) ? 2 : isValid(stall_toks[1]) ? 1 : 0;
  
  endfunction
  
  function Bit#(n) max(Bit#(n) x, Bit#(n) y);
  
    return (x > y) ? x : y;
  
  endfunction

  function Bool isCalculating(PRName pr, Maybe#(ISA_DEPENDENCY_INFO) mdeps);
    
    case (mdeps) matches
      tagged Invalid:
        return False;
      tagged Valid {.srcDeps, .dstDeps}:
        case (dstDeps[0]) matches
          tagged Invalid:
            return False;
          tagged Valid {.rnm, .prnm}:
            return prnm == pr;
        endcase
    endcase
     
  endfunction
  
  function Bool loadStall(PRName pr);
    
    return exe_is_load ? isCalculating(pr, exe_stall_info) : False;

  endfunction
  
  function Bit#(2) stallsFor(PRName pr);
  
    Bit#(2) st_exe = isCalculating(pr, exe_stall_info) ? 3 : 0;
    Bit#(2) st_mem = isCalculating(pr, mem_stall_info) ? 2 : 0;
    Bit#(2) st_wb  = isCalculating(pr, wb_stall_info)  ? 1 : 0;
  
    return max(max(st_exe, st_mem), st_wb);
  
  endfunction

  function Bit#(2) stallLength(ISA_DEPENDENCY_INFO deps);
  
    match {.srcDeps, .dstDeps} = deps;
    
    if (`DEC_PIPELINE_IS_BYPASSED)
    begin
      // Only check for Loads in EXE

      Bit#(2) stallForLoad1 = case (srcDeps[0]) matches
                                  tagged Invalid: return 0;
                                  tagged Valid {.rnm, .prnm}: return loadStall(prnm) ? 1 : 0;
                              endcase;

      Bit#(2) stallForLoad2 = case (srcDeps[1]) matches
                                  tagged Invalid: return 0;
                                  tagged Valid {.rnm, .prnm}: return loadStall(prnm) ? 1 : 0;
                              endcase;

      return max(stallForLoad1, stallForLoad2);

    end
    else
    begin

      Bit#(2) stall1 = 
        case (srcDeps[0]) matches
          tagged Invalid:
            return 0;
          tagged Valid {.rnm, .prnm}:
            return stallsFor(prnm);
        endcase;

      Bit#(2) stall2 = 
        case (srcDeps[2]) matches
          tagged Invalid:
            return 0;
          tagged Valid {.rnm, .prnm}:
            return stallsFor(prnm);
        endcase;
    
      return max(stall1, stall2);
    
    end
      
  endfunction

  
  //Rules

  rule decodeReq (!in_flight);
  
    local_ctrl.startModelCC();
  
    let mtup <- port_from_fet.receive();
    
    case (mtup) matches
      tagged Invalid:
      begin
        case (stall_toks[0]) matches
          tagged Invalid: // We can go ahead and pass it through
          begin
            port_to_exe.send(tagged Invalid);
            event_dec.recordEvent(tagged Invalid);
            shiftDepInfo(tagged Invalid);
            manageStalls(0, Invalid);
            exe_is_load <= False;
          end
          tagged Valid {.stall_tok, .stall_deps, .addr, .isLoad, .isStore}: // We should pass through the stalled guy. (Compress the bubble)
          begin
            port_to_exe.send(tagged Valid tuple4(stall_tok, addr, isLoad, isStore));
            event_dec.recordEvent(tagged Valid zeroExtend(stall_tok.index));
            shiftDepInfo(tagged Valid stall_deps);
            manageStalls(0, Invalid);
            exe_is_load <= isLoad;
          end
        endcase
      end
      tagged Valid {.tok, .maddr, .inst}:
      begin
        // Always send the request to the funcp
        $fdisplay(debug_file, "[%d]:DEC:REQ: %0d", curTick, tok.index);
        fp_dec_req.send(tok);
        infoQ.enq(tuple3(maddr, isLoad(inst), isStore(inst)));
        in_flight <= True;
      end
    endcase
    
  endrule

  rule decodeResp (in_flight);
  
    match {.tok, .deps} = fp_dec_resp.receive();
    fp_dec_resp.deq();
    
    match {.addr, .isLoad, .isStore} = infoQ.first();
    infoQ.deq();

    $fdisplay(debug_file, "[%d]:DEC:RSP: %0d", curTick, tok.index);

    in_flight <= False;

    Bit#(2) new_stall = stallLength(deps);
    
    let someone_is_stalled = isValid(stall_toks[0]) || isValid(stall_toks[1]) || isValid(stall_toks[2]);
    let we_are_stalling = (new_stall > 0) || someone_is_stalled;
        
    if (we_are_stalling)
    begin
      
      // We go at the back of the stall line
      let stall_length = max(new_stall, longest_stalled_token);
      manageStalls(stall_length, tagged Valid tuple5(tok, deps, addr, isLoad, isStore));

      // But what do we send onwards?
      case (stall_toks[0]) matches
        tagged Invalid: // Send Nothing
        begin
          port_to_exe.send(tagged Invalid);
          event_dec.recordEvent(tagged Invalid);
          shiftDepInfo(tagged Invalid);
          exe_is_load <= False;
        end
        tagged Valid {.stall_tok, .stall_deps, .stall_addr, .stall_isLoad, .stall_isStore}:
        begin  // Send the stalled guy instead.
          port_to_exe.send(tagged Valid tuple4(stall_tok, stall_addr, stall_isLoad, stall_isStore));
          event_dec.recordEvent(tagged Valid zeroExtend(stall_tok.index));
          shiftDepInfo(tagged Valid stall_deps);
          exe_is_load <= stall_isLoad;
        end
      endcase

    end
    else  // No one's in front of us, we're free to go.
    begin
    
      port_to_exe.send(tagged Valid tuple4(tok, addr, isLoad, isStore));
      event_dec.recordEvent(tagged Valid zeroExtend(tok.index));
      shiftDepInfo(tagged Valid deps);
      manageStalls(0, tagged Invalid);
      exe_is_load <= isLoad;

    end
    
  endrule

endmodule
