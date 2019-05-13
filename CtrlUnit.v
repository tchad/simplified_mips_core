/*
* EE275 Mini Project 2: MIPS Pipeline
* Tomasz Chadzynski
* SJSU, Spring 2018
* 
* Component: Control unit
* Description: This unit decodes instruction in the decode stage and provides
* next stages with signals determining the way instruction is processed in
* next cycles. Detects branch taken event which ends in flushing/canceling instructions. 
* When instructed to stall pipeline it injects nop(no op) cycles into the
* pipeline). It has the ability to supress portions of the forwarding unit(not used in
* this implementation)
*/

`include "defs.v"

module CtrlUnit(instr, injectNop, branchTaken,
                e_RegJmp, e_LongJmp, e_RegDst, e_AluOp, e_AluSrc, e_AluArg2Imm, 
                m_MemRead, m_MemWrite, m_Branch,
                w_RegWrite, w_MemToReg, w_FwdStage,
                id_Flush, exe_Flush, mem_Flush, wb_Flush,
                fwd_SupressMemFwd, fwd_SupressWbFwd,
                halt);
    parameter DEBUG = 1;

    `include "util_instr.v"

    //Full instruction input
    input [`INSTR_SIZE-1:0] instr;

    //Input indicating that controller should inject nop into the pipeline
    input injectNop;

    //Input indicating the branch is taken
    input branchTaken;

    //Indicate far jump, jump register instruction 
    //Selects: PC=Reg[Rs]
    output e_RegJmp; 

    //Indicate long jump J instruction
    //Selcts: {PC+4[31:28]##IMM[25:0]##2'b00} 
    output e_LongJmp; 

    //Indicate destination register for an instruction
    //0-Rt(Instr[20:16]), 1-Rd(Instr[15-11])
    output e_RegDst; 

    //Alu operation. 
    //if MSB == 0 then ALUOP selected by FUNCT field(Instr[5:0]
    //else then MSB == 1 and ALUOP selected by control
    output [3:0] e_AluOp; 

    //Second operand to alu
    //0- Rt from reg file, 1- sign ext imm(Instr[15:0]}
    output e_AluSrc; 

    //Indicate whether second alu argument is a register or immediate operand
    output e_AluArg2Imm;

    //Data memory in read mode if 1
    output m_MemRead; 

    //Data memory in write mode if 1
    output m_MemWrite; 

    //Branch mode activated in MEM stage if ALU.Zero == 1
    output m_Branch; 

    //Enables reg file write port
    output w_RegWrite; 

    //Selects source for reg write
    //1- from data memory, 0- from ALU output
    output w_MemToReg;

    //Indicate if and from what earliest stage the result data of instruction
    //can be forwarded
    output [1:0] w_FwdStage;

    //Places nop instruction in f/id register
    output id_Flush;

    //Flushes id/exe register in next cycle
    output exe_Flush;

    //Flushes exe/mem register in next cycle
    output mem_Flush;
    
    //Flushes mem/wb register in next cycle
    output wb_Flush;

    //Supressed forwarding from mem stage
    output fwd_SupressMemFwd;

    //Supresses forwarding from wb stage
    output fwd_SupressWbFwd;

    //Stops the machine
    output halt;

    reg e_RegJmp_d; 
    reg e_LongJmp_d; 
    reg e_RegDst_d; 
    reg [3:0] e_AluOp_d; 
    reg e_AluSrc_d; 
    reg e_AluArg2Imm_d;
    reg m_MemRead_d; 
    reg m_MemWrite_d; 
    reg m_Branch_d; 
    reg w_RegWrite_d; 
    reg w_MemToReg_d;
    reg [1:0] w_FwdStage_d;
    reg id_Flush_d;
    reg exe_Flush_d;
    reg mem_Flush_d;
    reg wb_Flush_d;
    reg fwd_SupressMemFwd_d;
    reg fwd_SupressWbFwd_d;
    reg halt_d;

    always @ (*) begin : MAIN_LOGIC
        if(InstrIsHlt(instr) == 1'b1) begin
            //Inject nop if the decoded instruction is a halt instruction
            e_RegJmp_d = 1'b0; 
            e_LongJmp_d = 1'b0; 
            e_RegDst_d = 1'b0; 
            e_AluOp_d = `ALU_CTRL_ADD_U; 
            e_AluSrc_d = 1'b0; 
            e_AluArg2Imm_d = 1'b1;
            m_MemRead_d = 1'b0; 
            m_MemWrite_d = 1'b0; 
            m_Branch_d = 1'b0; 
            w_RegWrite_d = 0; 
            w_MemToReg_d = 1'b0;
            w_FwdStage_d = `RESULT_FWD_NONE;
            halt_d = 1'b1;
        end else if(injectNop) begin
            //Inject nop when instructed by the input
            e_RegJmp_d = 1'b0; 
            e_LongJmp_d = 1'b0; 
            e_RegDst_d = 1'b0; 
            e_AluOp_d = `ALU_CTRL_ADD_U; 
            e_AluSrc_d = 1'b0; 
            e_AluArg2Imm_d = 1'b1;
            m_MemRead_d = 1'b0; 
            m_MemWrite_d = 1'b0; 
            m_Branch_d = 1'b0; 
            w_RegWrite_d = 0; 
            w_MemToReg_d = 1'b0;
            w_FwdStage_d = `RESULT_FWD_NONE;
            halt_d = 1'b0;
        end else if(InstrIsNop(instr) == 1'b1) begin
            //Inject nop when the decoded instruction is a nop instruction
            e_RegJmp_d = 1'b0; 
            e_LongJmp_d = 1'b0; 
            e_RegDst_d = 1'b0; 
            e_AluOp_d = `ALU_CTRL_ADD_U; 
            e_AluSrc_d = 1'b0; 
            e_AluArg2Imm_d = 1'b1;
            m_MemRead_d = 1'b0; 
            m_MemWrite_d = 1'b0; 
            m_Branch_d = 1'b0; 
            w_RegWrite_d = 0; 
            w_MemToReg_d = 1'b0;
            w_FwdStage_d = `RESULT_FWD_NONE;
            halt_d = 1'b0;
        end else if(InstrIsJ(instr) == 1'b1) begin
            //Set pipeline for J type instruction
            e_RegJmp_d =   1'b0; 
            e_LongJmp_d =  1'b1; 
            e_RegDst_d =   1'b0; 
            e_AluOp_d =    `ALU_CTRL_ADD_U; //Irrelevant
            e_AluSrc_d =   1'b0; 
            e_AluArg2Imm_d = 1'b1;
            m_MemRead_d =  1'b0; 
            m_MemWrite_d = 1'b0; 
            m_Branch_d =   1'b1; 
            w_RegWrite_d = 1'b0; 
            w_MemToReg_d = 1'b0;
            w_FwdStage_d = `RESULT_FWD_NONE;
            halt_d = 1'b0;
        end else if(InstrIsI(instr) == 1'b1) begin
            //Set pipeline for each I type instruction
            if(InstrFieldOpcode(instr) == `OPCODE_I_BEQ) begin
                //Branch instruction BEQ
                e_RegJmp_d =   1'b0; 
                e_LongJmp_d =  1'b0; 
                e_RegDst_d =   1'b0; 
                e_AluOp_d =    `ALU_CTRL_SUB_S;  
                e_AluSrc_d =   1'b0; //register file 
                e_AluArg2Imm_d = 1'b1;
                m_MemRead_d =  1'b0; 
                m_MemWrite_d = 1'b0; 
                m_Branch_d =   1'b1; 
                w_RegWrite_d = 1'b0; 
                w_MemToReg_d = 1'b0;
                w_FwdStage_d = `RESULT_FWD_NONE;
                halt_d = 1'b0;
            end else if(InstrFieldOpcode(instr) == `OPCODE_I_LW) begin
                //Load word instruction LW
                e_RegJmp_d =   1'b0; 
                e_LongJmp_d =  1'b0; 
                e_RegDst_d =   1'b0; 
                e_AluOp_d =    `ALU_CTRL_ADD_S;  
                e_AluSrc_d =   1'b1; 
                e_AluArg2Imm_d = 1'b1;
                m_MemRead_d =  1'b1; 
                m_MemWrite_d = 1'b0; 
                m_Branch_d =   1'b0; 
                w_RegWrite_d = 1'b1; 
                w_MemToReg_d = 1'b1;
                w_FwdStage_d = `RESULT_FWD_WB;
                halt_d = 1'b0;
            end else if(InstrFieldOpcode(instr) == `OPCODE_I_ADDIU) begin
                //Add immediate unsigned
                e_RegJmp_d =   1'b0; 
                e_LongJmp_d =  1'b0; 
                e_RegDst_d =   1'b0; 
                e_AluOp_d =    `ALU_CTRL_ADD_U;  
                e_AluSrc_d =   1'b1; 
                e_AluArg2Imm_d = 1'b1;
                m_MemRead_d =  1'b0; 
                m_MemWrite_d = 1'b0; 
                m_Branch_d =   1'b0; 
                w_RegWrite_d = 1'b1; 
                w_MemToReg_d = 1'b0;
                w_FwdStage_d = `RESULT_FWD_MEM;
                halt_d = 1'b0;
            end
        end else if(InstrIsR(instr) == 1'b1) begin
            //Set pipeline for R-Type instructions
            if(InstrRFieldFunct(instr) == `FUNCT_JR) begin
                //Jump register instruction (special case)
                e_RegJmp_d =   1'b1; 
                e_LongJmp_d =  1'b0; 
                e_RegDst_d =   1'b0; 
                e_AluOp_d =    `ALU_CTRL_ADD_U; //irrelevant 
                e_AluSrc_d =   1'b0; 
                e_AluArg2Imm_d = 1'b0;
                m_MemRead_d =  1'b0; 
                m_MemWrite_d = 1'b0; 
                m_Branch_d =   1'b1; 
                w_RegWrite_d = 1'b0; 
                w_MemToReg_d = 1'b0;
                w_FwdStage_d = `RESULT_FWD_NONE;
                halt_d = 1'b0;
            end else begin
                //Other R-Type instructions
                e_RegJmp_d =   1'b0; 
                e_LongJmp_d =  1'b0; 
                e_RegDst_d =   1'b1; //rd register 
                e_AluOp_d =    `ALU_CTRL_FUNC; 
                e_AluSrc_d =   1'b0; //second operand from register
                e_AluArg2Imm_d = 1'b0;
                m_MemRead_d =  1'b0; 
                m_MemWrite_d = 1'b0; 
                m_Branch_d =   1'b0; 
                w_RegWrite_d = 1'b1; //writeback to register
                w_MemToReg_d = 1'b0;
                w_FwdStage_d = `RESULT_FWD_MEM;
                halt_d = 1'b0;
            end
        end else begin
            //Exception unexpected instruction
            if(DEBUG == 1) begin
                $display("CtrlUnit: Unknown instruction %b",instr);
            end

            e_RegJmp_d = 1'b0; 
            e_LongJmp_d = 1'b0; 
            e_RegDst_d = 1'b0; 
            e_AluOp_d = 4'b0000; 
            e_AluSrc_d = 1'b0; 
            e_AluArg2Imm_d = 1'b1;
            m_MemRead_d = 1'b0; 
            m_MemWrite_d = 1'b0; 
            m_Branch_d = 1'b0; 
            w_RegWrite_d = 0; 
            w_MemToReg_d = 1'b0;
            w_FwdStage_d = `RESULT_FWD_NONE;
            halt_d = 1'b0;
        end

        //Flush relevant pipeline stages when branch taken
        id_Flush_d = 1'b0;
        exe_Flush_d = 1'b0;
        mem_Flush_d = 1'b0;
        wb_Flush_d = 1'b0;
        fwd_SupressMemFwd_d = 1'b0;
        fwd_SupressWbFwd_d = 1'b0;

        if(branchTaken == 1'b1) begin
            //If  branch taken flush/the pipeline registers in next cycle
            //This will empty the stage registers in next clock cycle
            id_Flush_d = 1'b1;
            exe_Flush_d = 1'b1;
            mem_Flush_d = 1'b1;
            wb_Flush_d = 1'b1;
        end
    end

    assign e_RegJmp = e_RegJmp_d; 
    assign e_LongJmp = e_LongJmp_d; 
    assign e_RegDst = e_RegDst_d; 
    assign e_AluOp = e_AluOp_d; 
    assign e_AluSrc = e_AluSrc_d; 
    assign e_AluArg2Imm = e_AluArg2Imm_d;
    assign m_MemRead = m_MemRead_d; 
    assign m_MemWrite = m_MemWrite_d; 
    assign m_Branch = m_Branch_d; 
    assign w_RegWrite = w_RegWrite_d; 
    assign w_MemToReg = w_MemToReg_d;
    assign w_FwdStage = w_FwdStage_d;

    assign id_Flush = id_Flush_d;
    assign exe_Flush = exe_Flush_d;
    assign mem_Flush = mem_Flush_d;
    assign wb_Flush = wb_Flush_d;
    assign fwd_SupressMemFwd = fwd_SupressMemFwd_d;
    assign fwd_SupressWbFwd = fwd_SupressWbFwd_d;
    assign halt = halt_d;
endmodule
