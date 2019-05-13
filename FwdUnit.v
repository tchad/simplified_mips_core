/*
* EE275 Mini Project 2: MIPS Pipeline
* Tomasz Chadzynski
* SJSU, Spring 2018
* 
* Component: Forwarding unit
* Description: When enabled, the unit redirects inputs to the alu from memory
* or writeback if the register numbers match and the instruction from which
* data is forwarded has correct forwarding flag.
* Forwarding flag indicate the earliest stage from which the result can be
* forwarded if at all. The flag is assigned by control unit when decoding the
* instruction. The RESULT_FWD_NONE indicate not data can be forwarded.
* I applies to functions like branching function. The RESULT_FWD_MEM the
* result is available for forwarding in the memory stage( from EXE/MEM stage
* register). This applies to the majority of instructions that use alu to
* compute result. The last flag, RESULT_FWD_WB indicate the data can be
* forwarded from writeback stage. This applies to instruction like LW where
* the data has to be first read from memory and passed to the MEM/WB register.
*
* The module does not detect on high level whether an operand shoudl be
* forwarded or instruction has to be stalled. It is up to the hazard detection
* unit and control unit to set the pipeline in a state that match forwarding
* pattern.
*/

`include "defs.v"

module FwdUnit(enabled, supressMem, supressWb,
               exe_AluArg1, exe_AluArg2, exe_AluArg2Imm, 
               mem_DstReg, mem_FwdStage, wb_DstReg, wb_FwdStage,
               exe_AluArg1FwdMem, exe_AluArg1FwdWb, exe_AluArg2FwdMem, exe_AluArg2FwdWb);

   //Input enable forwardig unit
   input enabled;
   
   //Input disable forwarding from memory stage(not used)
   input supressMem;

   //Input disable forwarding from writeback stage(not used)
   input supressWb;

   //Input register number of first operand of alu
   input [`FLD_REGNUM_SIZE-1:0] exe_AluArg1;
   
   //Input register number of second operand of alu
   input [`FLD_REGNUM_SIZE-1:0] exe_AluArg2;

   //Input indicate whether second operand of alu is an immediate value
   //If 1-no forwarding occurs. Prevents accidental match of part of immediate
   //value with one of the destination registers from further stages.
   input exe_AluArg2Imm;

   //Input Destination register od instruction that is currently in memory
   //stage
   input [`FLD_REGNUM_SIZE-1:0] mem_DstReg;
   
   //Input forwarding flag od instruction that is currently in memory
   //stage
   input [1:0] mem_FwdStage;

   //Input Destination register od instruction that is currently in writeback
   //stage
   input [`FLD_REGNUM_SIZE-1:0] wb_DstReg;

   //Input forwarding flag od instruction that is currently in writeback
   //stage
   input [1:0] wb_FwdStage;

   //Indicate that first argument of alu should be forwarded from memory stage
   output exe_AluArg1FwdMem;
   //Indicate that first argument of alu should be forwarded from writeback stage
   output exe_AluArg1FwdWb;
   //Indicate that second argument of alu should be forwarded from memory stage
   output exe_AluArg2FwdMem;
   //Indicate that second argument of alu should be forwarded from writeback stage
   output exe_AluArg2FwdWb;

   reg exe_AluArg1FwdMem_d;
   reg exe_AluArg1FwdWb_d;
   reg exe_AluArg2FwdMem_d;
   reg exe_AluArg2FwdWb_d;
   always @ (*) begin : MAIN
       exe_AluArg1FwdMem_d = 1'b0;
       exe_AluArg1FwdWb_d = 1'b0;
       exe_AluArg2FwdMem_d = 1'b0;
       exe_AluArg2FwdWb_d = 1'b0;

       //Forwarding from mem stage
       if(enabled == 1'b1 && supressMem == 1'b0) begin
           //If forwarding enabled and mem forwarding not suppressed
           if(exe_AluArg1 == mem_DstReg && mem_FwdStage == `RESULT_FWD_MEM) begin
               //Forward to arg 1 of alu if register number match and
               //forwarding flag allows forwarding from mem stage
               exe_AluArg1FwdMem_d = 1'b1;
           end

           if(exe_AluArg2 == mem_DstReg && exe_AluArg2Imm == 1'b0
               && mem_FwdStage == `RESULT_FWD_MEM) begin
               //Forward to arg 2 of alu if register number match,
               //forwarding flag allows forwarding from mem stage and second
               //argument is not immediate operand
               exe_AluArg2FwdMem_d = 1'b1;
           end
       end

       //Forwarding from wb stage
       if(enabled == 1'b1 && supressWb == 1'b0) begin
           //If forwarding enabled and writeback forwarding not suppressed
           if(exe_AluArg1 == wb_DstReg && wb_FwdStage != `RESULT_FWD_NONE &&
              exe_AluArg1FwdMem_d == 1'b0) begin
               //Forward to arg 1 of alu if register number match and
               //forwarding flag allows forwarding from any
               //and result is not already forwarded from mem stage
               exe_AluArg1FwdWb_d = 1'b1;
           end

           if(exe_AluArg2 == wb_DstReg && exe_AluArg2Imm == 1'b0 && 
               wb_FwdStage != `RESULT_FWD_NONE && exe_AluArg2FwdMem_d == 1'b0 ) begin
               //Forward to arg 2 of alu if register number match and
               //forwarding flag allows forwarding from any
               //and result is not already forwarded from mem stage
               //and second operand is no an immediate operand
               exe_AluArg2FwdWb_d = 1'b1;
           end
       end
   end

   assign exe_AluArg1FwdMem = exe_AluArg1FwdMem_d;
   assign exe_AluArg1FwdWb = exe_AluArg1FwdWb_d;
   assign exe_AluArg2FwdMem = exe_AluArg2FwdMem_d;
   assign exe_AluArg2FwdWb = exe_AluArg2FwdWb_d;
endmodule
