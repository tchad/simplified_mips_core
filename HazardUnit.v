/*
* EE275 Mini Project 2: MIPS Pipeline
* Tomasz Chadzynski
* SJSU, Spring 2018
* 
* Component: Hazard detection unit.
* Description: This unit detects hazards within the instruction flow. If
* a hazard is detected the hazard unit stalls the pipeline on the IF level and
* instruct Control unit to feed nop instructions until hazard is resolved.
* The component works in two modes with forwarding enabled or disabled. 
* When forwarding disabled then the pipeline stalls until all alu arguments
* are written into reg file. When forwarding is enabled then for this current
* set of instructions the code can be executed with almost no stalls except
* when an instruction right after LW need its result. The pipeline has to be
* stalled for one cycle to allow the LW data to pass to MEM/WB register.
*/

`include "defs.v"

module HazardUnit(fwd_Active, instr,
                  exe_DstReg, exe_FwdNfo, mem_DstReg, mem_FwdNfo, wb_DstReg, wb_FwdNfo,
                  pc_Stop, ctrl_forceNop, fwd_UnitEnable);
    `include "util_instr.v"

    //Input enabling or disabling the forwarding
    input fwd_Active;

    //input current instruction in decode stage
    input [`INSTR_SIZE-1:0] instr;

    //Destination register currently in exe stage
    input [`FLD_REGNUM_SIZE-1:0] exe_DstReg;

    //forwarding flag currently in exe stage 
    //See forwarding unit for forwarding field description
    input [1:0] exe_FwdNfo;

    //Destination register currently in mem stage
    input [`FLD_REGNUM_SIZE-1:0] mem_DstReg;

    //forwarding flag currently in mem stage 
    input [1:0] mem_FwdNfo;

    //Destination register currently in wb stage
    input [`FLD_REGNUM_SIZE-1:0] wb_DstReg;

    //forwarding flag currently in wb stage 
    input [1:0] wb_FwdNfo;

    //Stop the program counter
    output pc_Stop;

    //instruct control unit to inject nop instruction
    output ctrl_forceNop;

    //enable forwarding unit
    output fwd_UnitEnable;

    reg pc_Stop_d;
    reg ctrl_forceNop_d;
    always @ (*) begin: MAIN_LOGIC
        reg isRInstruction;
        reg isIInstruction;
        reg isJInstruction;
        
        reg [`FLD_REGNUM_SIZE-1:0] arg1;
        reg arg1Blocked;
        reg [`FLD_REGNUM_SIZE-1:0] arg2;
        reg arg2Blocked;

        //Detect if currently analyzed instruction is an R-Type instruction
        isRInstruction = InstrIsR(instr);
        //Detect if currently analyzed instruction is an I-Type instruction
        isIInstruction = InstrIsI(instr);

        //Extract reg number of both arguments into alu
        //If instruction is not an R-Type the second arg will be ignored
        arg1 = InstrRFieldRs(instr);
        arg2 = InstrRFieldRt(instr);
        arg1Blocked = 1'b0;
        arg2Blocked = 1'b0;

        if(InstrIsJ(instr) == 1'b1 || InstrIsNop(instr) == 1'b1) begin
            //J instruction and nop does not write back
            arg1Blocked = 1'b0;
            arg2Blocked = 1'b0;
        end else if(fwd_Active == 1'b0) begin
            //Case with no forwarding, stall at any applicable hazard
            if(arg1 == 0) begin
                //No stalling when input argument is register 0
                arg1Blocked = 1'b0;
            end else
            if((arg1 == exe_DstReg && exe_FwdNfo != `RESULT_FWD_NONE) || 
                (arg1 == mem_DstReg && mem_FwdNfo != `RESULT_FWD_NONE) || 
                (arg1 == wb_DstReg && wb_FwdNfo != `RESULT_FWD_NONE)) begin
                //Indicate argument 1 blocked if any of the destination
                //registers match the argument 1 register number
                //and the forward flag in matching stage is not equal NONE
                //Any data that is writen back to reg file can be forwarded
                //and therefore carry flag other than NONE. Therefore NONE
                //implicitly means no data writen back to reg file
                arg1Blocked = 1'b1;
            end

            if(arg2 == 0) begin
                //No stalling when input argument is register 0
                arg2Blocked = 1'b0;
            end else
            if((arg2 == exe_DstReg && exe_FwdNfo != `RESULT_FWD_NONE) || 
               (arg2 == mem_DstReg && mem_FwdNfo != `RESULT_FWD_NONE) || 
               (arg2 == wb_DstReg && wb_FwdNfo != `RESULT_FWD_NONE)) begin
                //Indicate argument 2 blocked if any of the destination
                //registers match the argument 2 register number
                //any data that is writen back to reg file can be forwarded
                //Any data that is writen back to reg file can be forwarded
                //and therefore carry flag other than NONE. Therefore NONE
                //implicitly means no data writen back to reg file
                arg2Blocked = 1'b1;
            end
        end else begin
            //With forwarding enabled
            //Conditions for blocking:
            //In current set of instruction only the LW instruction cannot
            //forward from mem stage. When there is a dependency on
            //destination register that is filled by lw instruction. If
            //a dependency instruction is at current point in time in exe
            //stage and can forward from wb stage only then stall for one
            //cycle to allow this instruction to pass one step further.
            if(arg1 == 0) begin
                //Ommit if argument is a r0
                arg1Blocked = 1'b0;
            end else
            if(arg1 == exe_DstReg && exe_FwdNfo == `RESULT_FWD_WB) begin
                //Set argument 1 as blocked if blocking condition occurs
                arg1Blocked = 1'b1;
            end

            if(arg2 == 0) begin
                //Ommit if argument is a r0
                arg2Blocked = 1'b0;
            end else
            if(arg2 == exe_DstReg && exe_FwdNfo == `RESULT_FWD_WB) begin
                //Set argument 1 as blocked if blocking condition occurs
                arg2Blocked = 1'b1;
            end
        end

        if(arg1Blocked == 1'b1 || arg2Blocked == 1'b1) begin
            //If any of the arguments are blocked stall, and inject nop
            pc_Stop_d = 1'b1;
            ctrl_forceNop_d = 1'b1;
        end else begin
            //Otherwise allow instruction to progress
            pc_Stop_d = 1'b0;
            ctrl_forceNop_d = 1'b0;
        end
    end

    assign fwd_UnitEnable = fwd_Active;
    assign pc_Stop = pc_Stop_d;
    assign ctrl_forceNop = ctrl_forceNop_d;
endmodule
