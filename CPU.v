/*
* EE275 Mini Project 2: MIPS Pipeline
* Tomasz Chadzynski
* SJSU, Spring 2018
* 
* Component: Central Processing Unit
* Description: Main cpu module connecting all other components and providing
* proper connection logic
*/

`include "defs.v"
`include "ALU.v"
`include "ALUCtrl.v"
`include "CtrlUnit.v"
`include "FwdUnit.v"
`include "HazardUnit.v"
`include "PC.v"
`include "RegFile.v"

//NOTE: Signals in this module are grouped by stage with name instead of as 
//outputs of modules. Exception, cross stage signals are named by module
//function and placed before stages logic
//
//NOTE: Register notation. The state registers are names as following:
//S_source stage_ destination stage_purpose_name, 
//the puspose section differentiate between regular data path and control
//lines. The regular data path name looks for example like:
// S_ID_EXE_DATA_Imm. For the control signal the purpose segment takes the
// name of last stage the control signal is used S_EXE_MEM_MEM_Branch.
//
// NOTE: In some cases verilog requires the data type to be reg when value is
// written always block even though such reg value is treated and synthesized
// as wire.
//
// NOTE: wire connections to registers have the same name as their destination
// register and additional _d suffix
//
// NOTE: During decode stage all the data lines are processed regardless of
// the instruction type and then proper lines are selected in exe stage

module CPU(clk,
           imem_clk, imem_addr, imem_instr, 
           dmem_clk, dmem_addr, dmem_dta_write, dmem_dta_out, dmem_mem_read, dmem_mem_write,
           halt);
    `include "util_instr.v"

    parameter DEBUG = 1;
    parameter DEBUG_REG_FILE = 1;
    parameter DEBUG_ALU = 1;
    parameter FORWARDING_EN = 1'b1;
    parameter REG_FILE_DATA_FILE = "reg.txt";
    parameter REG_FILE_DUMP_FILE = "regfile.txt";

    input clk;
    
    // Instruction bus from the instruction memory
    input [`WORD_SIZE-1:0] imem_instr;
    //pass clock to instruction memory
    output imem_clk;
    //Address bus to instruction memory
    output [`ADDR_SIZE-1:0] imem_addr;


    //output from the data memory
    input [`WORD_SIZE-1:0] dmem_dta_out;
    //pass clock to the data memory
    output dmem_clk;
    //Address bus to the data memory
    output [`ADDR_SIZE-1:0] dmem_addr;
    //write data to the data memory
    output [`WORD_SIZE-1:0] dmem_dta_write;
    //read signal to the data memory
    output dmem_mem_read;
    //write signal to the data memory
    output dmem_mem_write;

    //Output halt signal
    output halt;

    //---SEQUENTIUAL STATE OF THE MACHINE---//
    //Data path for IF/ID register
    //Program counter
    reg [`ADDR_SIZE-1:0]  S_IF_ID_DATA_Pc;
    //Full instruction
    reg [`INSTR_SIZE-1:0] S_IF_ID_DATA_Instr;

    //Data path for ID/EXE register
    //Program counter
    reg [`ADDR_SIZE-1:0] S_ID_EXE_DATA_Pc;
    //Data from reg file alu argument 1
    reg [`REG_SIZE-1:0] S_ID_EXE_DATA_Regfile1;
    //Data from reg file alu argument 2
    reg [`REG_SIZE-1:0] S_ID_EXE_DATA_Regfile2;
    //Immediate operand
    reg [`REG_SIZE-1:0] S_ID_EXE_DATA_Imm;
    //address for J instruction
    reg [`FLD_JADDR_SIZE-1:0] S_ID_EXE_DATA_JAddr;
    //shift amount field
    reg [`FLD_SHAMT_SIZE-1:0] S_ID_EXE_DATA_Shamt;
    // funct field
    reg [`FLD_FUNCT_SIZE-1:0] S_ID_EXE_DATA_Funct;
    //potential destination register in rt
    reg [`FLD_REGNUM_SIZE-1:0] S_ID_EXE_DATA_DstRt;
    //potential destination register in rd
    reg [`FLD_REGNUM_SIZE-1:0] S_ID_EXE_DATA_DstRd;
    //Register number used as arg 1 
    reg [`FLD_REGNUM_SIZE-1:0] S_ID_EXE_DATA_AluArgRegNum1;
    //Register number used as arg 2
    reg [`FLD_REGNUM_SIZE-1:0] S_ID_EXE_DATA_AluArgRegNum2;


    //Control path for ID/EXE
    //Select dmem out or alu out for writeback
    reg S_ID_EXE_WB_MemToReg;
    //enable writeback to reg file
    reg S_ID_EXE_WB_RegWrite;
    //forwarding flag
    reg [1:0] S_ID_EXE_WB_FwdStage;
    //indicate branch instruction
    reg S_ID_EXE_MEM_Branch;
    //enable dmem write
    reg S_ID_EXE_MEM_MemWrite;
    //enable dmem read
    reg S_ID_EXE_MEM_MemRead;
    //1- elect immediate operand as alu arg 2, 0- sleect regfile output
    reg S_ID_EXE_EXE_AluSrc;
    //alu operation input to alu ctrl
    reg [3:0] S_ID_EXE_EXE_AluOp;
    //select rt or rd as destination reg
    reg S_ID_EXE_EXE_RegDst;
    //indicate long jump J instruction
    reg S_ID_EXE_EXE_LongJmp;
    //indicate reg jumt jr instruction
    reg S_ID_EXE_EXE_RegJmp;
    //1 - imm operand as alu arg 2 instead of reg file
    reg S_ID_EXE_EXE_AluArg2Imm;

    //Data path for EXE/MEM register
    //claculated jump address
    reg [`ADDR_SIZE-1:0] S_EXE_MEM_DATA_JmpAddr;
    //1 if two alu operands have equal value, used for branch
    reg S_EXE_MEM_DATA_AluZero;
    //result of alu operation
    reg [`REG_SIZE-1:0] S_EXE_MEM_DATA_AluResult;
    //mem write data line from reg file
    reg [`REG_SIZE-1:0] S_EXE_MEM_DATA_MemWrtData;
    //writeback destination register
    reg [`FLD_REGNUM_SIZE-1:0] S_EXE_MEM_DATA_DstReg;

    //Control path for EXE/MEM
    //Select dmem out or alu out for writeback
    reg S_EXE_MEM_WB_MemToReg;
    //enable writeback to reg file
    reg S_EXE_MEM_WB_RegWrite;
    //forwarding flag
    reg [1:0] S_EXE_MEM_WB_FwdStage;
    //indicate branch instruction
    reg S_EXE_MEM_MEM_Branch;
    //enable dmem write
    reg S_EXE_MEM_MEM_MemWrite;
    //enable dmem read
    reg S_EXE_MEM_MEM_MemRead;

    //Data path for MEM/WB register
    //data read from dmem
    reg [`REG_SIZE-1:0] S_MEM_WB_DATA_MemReadData;
    //alu result passed from exe stage
    reg [`REG_SIZE-1:0] S_MEM_WB_DATA_AluResult;
    //writeback destination register
    reg [`FLD_REGNUM_SIZE-1:0] S_MEM_WB_DATA_DstReg;

    //Control path for MEM/WB
    //Select dmem out or alu out for writeback
    reg S_MEM_WB_WB_MemToReg;
    //enable writeback to reg file
    reg S_MEM_WB_WB_RegWrite;
    //forwarding flag
    reg [1:0] S_MEM_WB_WB_FwdStage;

    //---COMBINATIONAL LOGIC and MODULES---//
    //Wire connections Hazard unit
    //Hazard indicate program counter stop
    wire hz_PcStop;
    //Hazard indicate inject nop instruction
    wire hz_InjectNop;
    //hazard indicate enable forwarding unit
    wire hz_FwdUnitEnable;

    //Wire connections from Control unit
    //flush ID stage register
    wire ctrl_IdFlush;
    //flush exe stage register
    wire ctrl_ExeFlush;
    //flush mem stage register
    wire ctrl_MemFlush;
    //flush wb stage register
    wire ctrl_WbFlush;
    //suppress forwarding from mem stage ( not used)
    wire ctrl_SupressMemFwd;
    //suppress forwarding from writeback stage(not used)
    wire ctrl_SupressWbFwd;
    //indicate machine halt
    wire ctrl_Halt;

    //control signals written into control portion of state register duting
    //decode stage
    wire ctrl_MemToReg;
    wire ctrl_RegWrite;
    wire [1:0] ctrl_FwdStage;
    wire ctrl_Branch;
    wire ctrl_MemWrite;
    wire ctrl_MemRead;
    wire ctrl_AluSrc;
    wire [3:0] ctrl_AluOp;
    wire ctrl_RegDst;
    wire ctrl_LongJmp;
    wire ctrl_RegJmp;

    //Wire connections from Forwarding unit
    //indicating which channels to use for alu arguments
    wire fwd_AluArg1FwdMem;
    wire fwd_AluArg1FwdWb;
    wire fwd_AluArg2FwdMem;
    wire fwd_AluArg2FwdWb;

    //Wire connections F stage
    reg [`ADDR_SIZE-1:0] f_NextPC;
    wire [`ADDR_SIZE-1:0] f_PC;
    reg [`INSTR_SIZE-1:0] f_Instr;

    reg [`ADDR_SIZE-1:0]  S_IF_ID_DATA_Pc_d;
    reg [`INSTR_SIZE-1:0] S_IF_ID_DATA_Instr_d;

    //Wire connections D stage
    reg [`FLD_REGNUM_SIZE-1:0] id_RegFileRead1;
    reg [`FLD_REGNUM_SIZE-1:0] id_RegFileRead2;

    reg S_ID_EXE_WB_MemToReg_d;
    reg S_ID_EXE_WB_RegWrite_d;
    reg [1:0] S_ID_EXE_WB_FwdStage_d;
    reg S_ID_EXE_MEM_Branch_d;
    reg S_ID_EXE_MEM_MemWrite_d;
    reg S_ID_EXE_MEM_MemRead_d;
    reg S_ID_EXE_EXE_AluSrc_d;
    reg [3:0] S_ID_EXE_EXE_AluOp_d;
    reg S_ID_EXE_EXE_RegDst_d;
    reg S_ID_EXE_EXE_LongJmp_d;
    reg S_ID_EXE_EXE_RegJmp_d;
    wire S_ID_EXE_EXE_AluArg2Imm_d;

    reg [`ADDR_SIZE-1:0] S_ID_EXE_DATA_Pc_d;
    wire [`REG_SIZE-1:0] S_ID_EXE_DATA_Regfile1_d;
    wire [`REG_SIZE-1:0] S_ID_EXE_DATA_Regfile2_d;
    reg [`REG_SIZE-1:0] S_ID_EXE_DATA_Imm_d;
    reg [`FLD_JADDR_SIZE-1:0] S_ID_EXE_DATA_JAddr_d;
    reg [`FLD_SHAMT_SIZE-1:0] S_ID_EXE_DATA_Shamt_d;
    reg [`FLD_FUNCT_SIZE-1:0] S_ID_EXE_DATA_Funct_d;
    reg [`FLD_REGNUM_SIZE-1:0] S_ID_EXE_DATA_DstRt_d;
    reg [`FLD_REGNUM_SIZE-1:0] S_ID_EXE_DATA_DstRd_d;
    reg [`FLD_REGNUM_SIZE-1:0] S_ID_EXE_DATA_AluArgRegNum1_d;
    reg [`FLD_REGNUM_SIZE-1:0] S_ID_EXE_DATA_AluArgRegNum2_d;

    //Wire connections E stage
    reg [`REG_SIZE-1:0] exe_AluArg1;
    reg [`REG_SIZE-1:0] exe_AluArg2;
    wire [3:0] exe_AluOp;

    reg [`ADDR_SIZE-1:0] S_EXE_MEM_DATA_JmpAddr_d;
    wire S_EXE_MEM_DATA_AluZero_d;
    wire [`REG_SIZE-1:0] S_EXE_MEM_DATA_AluResult_d;
    reg [`REG_SIZE-1:0] S_EXE_MEM_DATA_MemWrtData_d;
    reg [`FLD_REGNUM_SIZE-1:0] S_EXE_MEM_DATA_DstReg_d;

    reg S_EXE_MEM_WB_MemToReg_d;
    reg S_EXE_MEM_WB_RegWrite_d;
    reg [1:0] S_EXE_MEM_WB_FwdStage_d;
    reg S_EXE_MEM_MEM_Branch_d;
    reg S_EXE_MEM_MEM_MemWrite_d;
    reg S_EXE_MEM_MEM_MemRead_d;

    //Wire connections M stage
    reg mem_PcSrc;
    reg mem_BranchTaken;

    reg [`REG_SIZE-1:0] S_MEM_WB_DATA_MemReadData_d;
    reg [`REG_SIZE-1:0] S_MEM_WB_DATA_AluResult_d;
    reg [`FLD_REGNUM_SIZE-1:0] S_MEM_WB_DATA_DstReg_d;

    reg S_MEM_WB_WB_MemToReg_d;
    reg S_MEM_WB_WB_RegWrite_d;
    reg [1:0] S_MEM_WB_WB_FwdStage_d;

    //Wire connections W stage
    reg [`REG_SIZE-1:0] wb_ValueWriteback;

    //Hazard unit instance
    HazardUnit U3(.fwd_Active(FORWARDING_EN),
                  .instr(S_IF_ID_DATA_Instr),
                  .exe_DstReg(S_EXE_MEM_DATA_DstReg_d), //Not a bug, connection to comb logic(exe stage)
                  .exe_FwdNfo(S_ID_EXE_WB_FwdStage),
                  .mem_DstReg(S_EXE_MEM_DATA_DstReg), //Not a bug, connection from register(mem stage)
                  .mem_FwdNfo(S_EXE_MEM_WB_FwdStage),
                  .wb_DstReg(S_MEM_WB_DATA_DstReg),
                  .wb_FwdNfo(S_MEM_WB_WB_FwdStage),
                  .pc_Stop(hz_PcStop),
                  .ctrl_forceNop(hz_InjectNop),
                  .fwd_UnitEnable(hz_FwdUnitEnable));

    //Control unit instance
    CtrlUnit U4(.instr(S_IF_ID_DATA_Instr),
                .injectNop(hz_InjectNop),
                .branchTaken(mem_BranchTaken), 
                .e_RegJmp(ctrl_RegJmp),
                .e_LongJmp(ctrl_LongJmp),
                .e_RegDst(ctrl_RegDst),
                .e_AluOp(ctrl_AluOp),
                .e_AluSrc(ctrl_AluSrc),
                .e_AluArg2Imm(S_ID_EXE_EXE_AluArg2Imm_d),
                .m_MemRead(ctrl_MemRead),
                .m_MemWrite(ctrl_MemWrite),
                .m_Branch(ctrl_Branch),
                .w_RegWrite(ctrl_RegWrite),
                .w_MemToReg(ctrl_MemToReg),
                .w_FwdStage(ctrl_FwdStage),
                .id_Flush(ctrl_IdFlush),
                .exe_Flush(ctrl_ExeFlush),
                .mem_Flush(ctrl_MemFlush),
                .wb_Flush(ctrl_WbFlush),
                .fwd_SupressMemFwd(ctrl_SupressMemFwd),
                .fwd_SupressWbFwd(ctrl_SupressWbFwd),
                .halt(ctrl_Halt));

    

    //Forwarding unit instance
    FwdUnit U5(.enabled(hz_FwdUnitEnable), 
               .supressMem(ctrl_SupressMemFwd),
               .supressWb(ctrl_SupressWbFwd),
               .exe_AluArg1(S_ID_EXE_DATA_AluArgRegNum1),
               .exe_AluArg2(S_ID_EXE_DATA_AluArgRegNum2),
               .exe_AluArg2Imm(S_ID_EXE_EXE_AluArg2Imm),
               .mem_DstReg(S_EXE_MEM_DATA_DstReg),
               .mem_FwdStage(S_EXE_MEM_WB_FwdStage),
               .wb_DstReg(S_MEM_WB_DATA_DstReg),
               .wb_FwdStage(S_MEM_WB_WB_FwdStage),
               .exe_AluArg1FwdMem(fwd_AluArg1FwdMem),
               .exe_AluArg1FwdWb(fwd_AluArg1FwdWb),
               .exe_AluArg2FwdMem(fwd_AluArg2FwdMem),
               .exe_AluArg2FwdWb(fwd_AluArg2FwdWb));


    //----Fetch stage logic----
    PC U6(.clk(clk), .next(f_NextPC), .stop(hz_PcStop), .curr(f_PC));

    //Assign clock to imem
    assign imem_clk = clk;
    //Address IMEM with current PC
    assign imem_addr = f_PC;

    always @ (*) begin : STAGE_F_LOGIC
        //Assign next PC
        if(ctrl_Halt == 1'b1) begin
            //If halt recirculate PC
            f_NextPC = f_PC;
        end else if(mem_PcSrc == 1'b0) begin
            //Reguler PC+4 assignment
            f_NextPC = f_PC + 4;
        end else begin
            //Assignment of destination address in case of branch taken
            f_NextPC = S_EXE_MEM_DATA_JmpAddr;
        end

        //Pass data to register or flush depending on controller
        if(ctrl_Halt == 1'b1 || hz_PcStop == 1'b1) begin
            //Recirculate IF/ID register in case of halt or stall
            S_IF_ID_DATA_Pc_d = S_IF_ID_DATA_Pc;
            S_IF_ID_DATA_Instr_d = S_IF_ID_DATA_Instr;
        end else if(ctrl_IdFlush == 1'b1) begin
            //Assign default value in case of flush
            S_IF_ID_DATA_Pc_d = `ADDR_SIZE'b0;
            S_IF_ID_DATA_Instr_d = `MakeNOP;
        end else begin
            //Regular assignment from imem and pc
            S_IF_ID_DATA_Pc_d = f_PC + 4;
            S_IF_ID_DATA_Instr_d = imem_instr;
        end
    end

    //----Decode stage logic----
    defparam U7.DEBUG = DEBUG_REG_FILE;
    defparam U7.DATA_FILE = REG_FILE_DATA_FILE;
    defparam U7.DUMP_FILE = REG_FILE_DUMP_FILE;
    RegFile U7(.clk(clk),
               .reg1ReadAddr(id_RegFileRead1),
               .reg2ReadAddr(id_RegFileRead2),
               .regWrite(S_MEM_WB_WB_RegWrite),
               .regWriteAddr(S_MEM_WB_DATA_DstReg),
               .regWriteData(wb_ValueWriteback),
               .reg1ReadData(S_ID_EXE_DATA_Regfile1_d),
               .reg2ReadData(S_ID_EXE_DATA_Regfile2_d));

    always @ (*) begin : STAGE_D_LOGIC
        reg [`FLD_IMM_SIZE-1:0] tmpImm;
        //Get both addresses of regfile registers
        id_RegFileRead1 = InstrRFieldRs(S_IF_ID_DATA_Instr);
        id_RegFileRead2 = InstrRFieldRt(S_IF_ID_DATA_Instr);

        //Passthrough PC
        S_ID_EXE_DATA_Pc_d = S_IF_ID_DATA_Pc;

        //Sign extend immediate operand
        tmpImm = InstrIFieldImm(S_IF_ID_DATA_Instr);
        S_ID_EXE_DATA_Imm_d = {{(`REG_SIZE-`FLD_IMM_SIZE){tmpImm[`FLD_IMM_SIZE-1]}},
                                 {tmpImm}};
        
        //J instruction address
        S_ID_EXE_DATA_JAddr_d = InstrJFieldAddr(S_IF_ID_DATA_Instr);

        //Passthrough shamt and funct fields
        S_ID_EXE_DATA_Shamt_d = InstrRFieldShamt(S_IF_ID_DATA_Instr);
        S_ID_EXE_DATA_Funct_d = InstrRFieldFunct(S_IF_ID_DATA_Instr);
                       
        //Take out the register arguments out of the consideration to prevent
        //Hazard unit from blocking the pipeline forever
        if(hz_InjectNop == 1'b1) begin
            //If inject nop then clean arguemnts for hazard unit to see
            S_ID_EXE_DATA_AluArgRegNum1_d = 0;
            S_ID_EXE_DATA_AluArgRegNum2_d = 0;
            S_ID_EXE_DATA_DstRt_d = 0;
            S_ID_EXE_DATA_DstRd_d = 0;
        end else begin
            //Otherwise passthrough All reg addresses 
            S_ID_EXE_DATA_AluArgRegNum1_d = InstrRFieldRs(S_IF_ID_DATA_Instr);
            S_ID_EXE_DATA_AluArgRegNum2_d = InstrRFieldRt(S_IF_ID_DATA_Instr);
            S_ID_EXE_DATA_DstRt_d = InstrRFieldRt(S_IF_ID_DATA_Instr);
            S_ID_EXE_DATA_DstRd_d = InstrRFieldRd(S_IF_ID_DATA_Instr);

        end

        //ID/EXE register flush or pass control state 
        if(ctrl_IdFlush == 1'b1) begin
            S_ID_EXE_WB_MemToReg_d = 1'b0;
            S_ID_EXE_WB_RegWrite_d = 1'b0;
            S_ID_EXE_WB_FwdStage_d = `RESULT_FWD_NONE;
            S_ID_EXE_MEM_Branch_d = 1'b0;
            S_ID_EXE_MEM_MemWrite_d = 1'b0;
            S_ID_EXE_MEM_MemRead_d = 1'b0;
            S_ID_EXE_EXE_AluSrc_d = 1'b0;
            S_ID_EXE_EXE_AluOp_d = `ALU_CTRL_ADD_U;
            S_ID_EXE_EXE_RegDst_d = 1'b0;
            S_ID_EXE_EXE_LongJmp_d = 1'b0;
            S_ID_EXE_EXE_RegJmp_d = 1'b0;
        end else begin
            S_ID_EXE_WB_MemToReg_d = ctrl_MemToReg;
            S_ID_EXE_WB_RegWrite_d = ctrl_RegWrite;
            S_ID_EXE_WB_FwdStage_d = ctrl_FwdStage;
            S_ID_EXE_MEM_Branch_d = ctrl_Branch;
            S_ID_EXE_MEM_MemWrite_d = ctrl_MemWrite;
            S_ID_EXE_MEM_MemRead_d = ctrl_MemRead;
            S_ID_EXE_EXE_AluSrc_d = ctrl_AluSrc;
            S_ID_EXE_EXE_AluOp_d = ctrl_AluOp;
            S_ID_EXE_EXE_RegDst_d = ctrl_RegDst;
            S_ID_EXE_EXE_LongJmp_d = ctrl_LongJmp;
            S_ID_EXE_EXE_RegJmp_d = ctrl_RegJmp;
        end

    end

    //----Execute stage logic----
    
    //ALUControl
    ALUCtrl U8(.ctrl(S_ID_EXE_EXE_AluOp), .funct(S_ID_EXE_DATA_Funct), .aluop(exe_AluOp));

    //ALU
    defparam U9.DEBUG = DEBUG_ALU;

    ALU U9(.alu_op(exe_AluOp), .arg1(exe_AluArg1), .arg2(exe_AluArg2),
               .result(S_EXE_MEM_DATA_AluResult_d), .zero(S_EXE_MEM_DATA_AluZero_d));
    
    always @ (*) begin : STAGE_E_LOGIC
        //signed values to force signed add
        reg signed [`REG_SIZE-1:0] tmpJmpAddr;
        reg signed [`REG_SIZE-1:0] tmpPC;

        //Calculate address for next jump 
        if(S_ID_EXE_EXE_RegJmp == 1'b1) begin
            //Case JR instruction overwrite jump address
            S_EXE_MEM_DATA_JmpAddr_d = S_ID_EXE_DATA_Regfile1;
        end else if(S_ID_EXE_EXE_LongJmp == 1'b1) begin
            //Case long jump J instruction. Use 4MSB bits of current pc
            //and replace rest with J instruction addr field and toe zero bits
            S_EXE_MEM_DATA_JmpAddr_d = {S_ID_EXE_DATA_Pc[31:28], 
                                          S_ID_EXE_DATA_JAddr,
                                          2'b00};

        end else begin
            //Case BEQ and other. Calculate PC based jump address
            tmpJmpAddr = S_ID_EXE_DATA_Imm << 2;
            tmpPC = S_ID_EXE_DATA_Pc;
            S_EXE_MEM_DATA_JmpAddr_d = tmpPC + tmpJmpAddr;
        end

        //Passthrough Memory write data coming from rt(SW instruction case,
        //not implemented)
        S_EXE_MEM_DATA_MemWrtData_d = S_ID_EXE_DATA_Regfile2;

        //Arg1 mux selecting operand based on control and forwarding data
        if(fwd_AluArg1FwdMem == 1'b1) begin
            //Forward from mem stage
            exe_AluArg1 = S_EXE_MEM_DATA_AluResult;
        end else if(fwd_AluArg1FwdWb == 1'b1) begin
            //forward from wb stage
            exe_AluArg1 = wb_ValueWriteback;
        end else begin
            //no forwarding use reg file output
            exe_AluArg1 = S_ID_EXE_DATA_Regfile1;
        end

        //Arg2 mux selecting operand based on control and forwarding data
        if(fwd_AluArg2FwdMem == 1'b1) begin
            //Forward from mem stage
            exe_AluArg2 = S_EXE_MEM_DATA_AluResult;
        end else if(fwd_AluArg2FwdWb == 1'b1) begin
            //Forward from wb stage
            exe_AluArg2 = wb_ValueWriteback;
        end else if(S_ID_EXE_EXE_AluSrc == 1'b1) begin
            //no forwarding use immediate operand
            exe_AluArg2 = S_ID_EXE_DATA_Imm;
        end else begin
            //no forwarding use reg file output 
            exe_AluArg2 = S_ID_EXE_DATA_Regfile2;
        end

        //Determine destination register
        if(S_ID_EXE_EXE_RegDst == 1'b0) begin
            S_EXE_MEM_DATA_DstReg_d = S_ID_EXE_DATA_DstRt;
        end else begin
            S_EXE_MEM_DATA_DstReg_d = S_ID_EXE_DATA_DstRd;
        end
        
        //Shift left -not needed for this function set

        //flush or copy control data to MEM stage 
        if(ctrl_ExeFlush == 1'b1) begin
            S_EXE_MEM_WB_MemToReg_d = 1'b0;
            S_EXE_MEM_WB_RegWrite_d = 1'b0;
            S_EXE_MEM_WB_FwdStage_d = `RESULT_FWD_NONE;
            S_EXE_MEM_MEM_Branch_d = 1'b0;
            S_EXE_MEM_MEM_MemWrite_d = 1'b0;
            S_EXE_MEM_MEM_MemRead_d = 1'b0;
        end else begin
            S_EXE_MEM_WB_MemToReg_d = S_ID_EXE_WB_MemToReg;
            S_EXE_MEM_WB_RegWrite_d = S_ID_EXE_WB_RegWrite;
            S_EXE_MEM_WB_FwdStage_d = S_ID_EXE_WB_FwdStage;
            S_EXE_MEM_MEM_Branch_d = S_ID_EXE_MEM_Branch;
            S_EXE_MEM_MEM_MemWrite_d = S_ID_EXE_MEM_MemWrite;
            S_EXE_MEM_MEM_MemRead_d = S_ID_EXE_MEM_MemRead;
        end

    end
    
    //----Memory stage logic----

    //Connections to data memory
    assign dmem_clk = clk;
    assign dmem_addr = S_EXE_MEM_DATA_AluResult;
    assign dmem_dta_write = S_EXE_MEM_DATA_MemWrtData;
    assign dmem_mem_write = S_EXE_MEM_MEM_MemWrite;
    assign dmem_mem_read = S_EXE_MEM_MEM_MemRead;

    always @ (*) begin : STAGE_M_LOGIC
        //Branch gate, high if branch and alu_zero
        mem_BranchTaken = S_EXE_MEM_DATA_AluZero & S_EXE_MEM_MEM_Branch;

        //Signal pc source 
        if(mem_BranchTaken == 1'b1) begin
            //next pc from jump address
            mem_PcSrc = 1'b1;
        end else begin
            //regular PC+4
            mem_PcSrc = 1'b0;
        end

        //data memory output
        S_MEM_WB_DATA_MemReadData_d = dmem_dta_out;
        //alu result output
        S_MEM_WB_DATA_AluResult_d = S_EXE_MEM_DATA_AluResult;
        //pass destination register number
        S_MEM_WB_DATA_DstReg_d = S_EXE_MEM_DATA_DstReg;

        //flush or copy control data to WB stage 
        if(ctrl_WbFlush == 1'b1) begin
            S_MEM_WB_WB_MemToReg_d = 1'b0;
            S_MEM_WB_WB_RegWrite_d = 1'b0;
            S_MEM_WB_WB_FwdStage_d = `RESULT_FWD_NONE;
        end else begin
            S_MEM_WB_WB_MemToReg_d = S_EXE_MEM_WB_MemToReg;
            S_MEM_WB_WB_RegWrite_d = S_EXE_MEM_WB_RegWrite;
            S_MEM_WB_WB_FwdStage_d = S_EXE_MEM_WB_FwdStage;
        end
    end

    //----Writeback stage logic----
    always @ (*) begin : STAGE_W_LOGIC
        if(S_MEM_WB_WB_MemToReg == 1'b1) begin
            //slect dmem output for writeback
            wb_ValueWriteback = S_MEM_WB_DATA_MemReadData;
        end else begin
            //select alu output for writeback
            wb_ValueWriteback = S_MEM_WB_DATA_AluResult;
        end
    end
    
    //Sequential assign of state registers
    always @(posedge clk) begin: SEQUENTIAL_ASSIGN
        //F stage
        S_IF_ID_DATA_Pc <= #1 S_IF_ID_DATA_Pc_d;
        S_IF_ID_DATA_Instr <= #1 S_IF_ID_DATA_Instr_d;

        //D stage
        S_ID_EXE_WB_MemToReg <= #1 S_ID_EXE_WB_MemToReg_d;
        S_ID_EXE_WB_RegWrite <= #1 S_ID_EXE_WB_RegWrite_d;
        S_ID_EXE_WB_FwdStage <= #1 S_ID_EXE_WB_FwdStage_d;
        S_ID_EXE_MEM_Branch <= #1 S_ID_EXE_MEM_Branch_d;
        S_ID_EXE_MEM_MemWrite <= #1 S_ID_EXE_MEM_MemWrite_d;
        S_ID_EXE_MEM_MemRead <= #1 S_ID_EXE_MEM_MemRead_d;
        S_ID_EXE_EXE_AluSrc <= #1 S_ID_EXE_EXE_AluSrc_d;
        S_ID_EXE_EXE_AluOp <= #1 S_ID_EXE_EXE_AluOp_d;
        S_ID_EXE_EXE_RegDst <= #1 S_ID_EXE_EXE_RegDst_d;
        S_ID_EXE_EXE_LongJmp <= #1 S_ID_EXE_EXE_LongJmp_d;
        S_ID_EXE_EXE_RegJmp <= #1 S_ID_EXE_EXE_RegJmp_d;
        S_ID_EXE_EXE_AluArg2Imm <= #1 S_ID_EXE_EXE_AluArg2Imm_d;

        S_ID_EXE_DATA_Pc <= #1 S_ID_EXE_DATA_Pc_d;
        S_ID_EXE_DATA_Regfile1 <= #1 S_ID_EXE_DATA_Regfile1_d;
        S_ID_EXE_DATA_Regfile2 <= #1 S_ID_EXE_DATA_Regfile2_d;
        S_ID_EXE_DATA_Imm <= #1 S_ID_EXE_DATA_Imm_d;
        S_ID_EXE_DATA_JAddr <= #1 S_ID_EXE_DATA_JAddr_d;
        S_ID_EXE_DATA_Shamt <= #1 S_ID_EXE_DATA_Shamt_d;
        S_ID_EXE_DATA_Funct <= #1 S_ID_EXE_DATA_Funct_d;
        S_ID_EXE_DATA_DstRt <= #1 S_ID_EXE_DATA_DstRt_d;
        S_ID_EXE_DATA_DstRd <= #1 S_ID_EXE_DATA_DstRd_d;
        S_ID_EXE_DATA_AluArgRegNum1 <= #1 S_ID_EXE_DATA_AluArgRegNum1_d;
        S_ID_EXE_DATA_AluArgRegNum2 <= #1 S_ID_EXE_DATA_AluArgRegNum2_d;

        //E stage
        S_EXE_MEM_DATA_JmpAddr <= #1 S_EXE_MEM_DATA_JmpAddr_d;
        S_EXE_MEM_DATA_AluZero <= #1 S_EXE_MEM_DATA_AluZero_d;
        S_EXE_MEM_DATA_AluResult <= #1 S_EXE_MEM_DATA_AluResult_d;
        S_EXE_MEM_DATA_MemWrtData <= #1 S_EXE_MEM_DATA_MemWrtData_d;
        S_EXE_MEM_DATA_DstReg <= #1 S_EXE_MEM_DATA_DstReg_d;

        S_EXE_MEM_WB_MemToReg <= #1 S_EXE_MEM_WB_MemToReg_d;
        S_EXE_MEM_WB_RegWrite <= #1 S_EXE_MEM_WB_RegWrite_d;
        S_EXE_MEM_WB_FwdStage <= #1 S_EXE_MEM_WB_FwdStage_d;
        S_EXE_MEM_MEM_Branch <= #1 S_EXE_MEM_MEM_Branch_d;
        S_EXE_MEM_MEM_MemWrite <= #1 S_EXE_MEM_MEM_MemWrite_d;
        S_EXE_MEM_MEM_MemRead <= #1 S_EXE_MEM_MEM_MemRead_d;

        //Wire connections M stage
        S_MEM_WB_DATA_MemReadData <= #1 S_MEM_WB_DATA_MemReadData_d;
        S_MEM_WB_DATA_AluResult <= #1 S_MEM_WB_DATA_AluResult_d;
        S_MEM_WB_DATA_DstReg <= #1 S_MEM_WB_DATA_DstReg_d;

        S_MEM_WB_WB_MemToReg <= #1 S_MEM_WB_WB_MemToReg_d;
        S_MEM_WB_WB_RegWrite <= #1 S_MEM_WB_WB_RegWrite_d;
        S_MEM_WB_WB_FwdStage <= #1 S_MEM_WB_WB_FwdStage_d;
    end

    //Initial state of the machine
    initial begin
        S_IF_ID_DATA_Instr = `MakeNOP;
        S_IF_ID_DATA_Pc = 0;

        //D stage
        S_ID_EXE_WB_MemToReg = 0;
        S_ID_EXE_WB_RegWrite = 0;
        S_ID_EXE_WB_FwdStage = 0;
        S_ID_EXE_MEM_Branch = 0;
        S_ID_EXE_MEM_MemWrite = 0;
        S_ID_EXE_MEM_MemRead = 0;
        S_ID_EXE_EXE_AluSrc = 0;
        S_ID_EXE_EXE_AluOp = `ALU_CTRL_ADD_U;
        S_ID_EXE_EXE_AluArg2Imm = 0;
        S_ID_EXE_EXE_RegDst = 0;
        S_ID_EXE_EXE_LongJmp = 0;
        S_ID_EXE_EXE_RegJmp = 0;

        S_ID_EXE_DATA_Pc = 0;
        S_ID_EXE_DATA_Regfile1 = 0;
        S_ID_EXE_DATA_Regfile2 = 0;
        S_ID_EXE_DATA_Imm = 0;
        S_ID_EXE_DATA_JAddr = 0;
        S_ID_EXE_DATA_Shamt = 0;
        S_ID_EXE_DATA_Funct = 0;
        S_ID_EXE_DATA_DstRt = 0;
        S_ID_EXE_DATA_DstRd = 0;
        S_ID_EXE_DATA_AluArgRegNum1 = 0;
        S_ID_EXE_DATA_AluArgRegNum2 = 0;

        //E stage
        S_EXE_MEM_DATA_JmpAddr = 0;
        S_EXE_MEM_DATA_AluZero = 0;
        S_EXE_MEM_DATA_AluResult = 0;
        S_EXE_MEM_DATA_MemWrtData = 0;
        S_EXE_MEM_DATA_DstReg = 0;

        S_EXE_MEM_WB_MemToReg = 0;
        S_EXE_MEM_WB_RegWrite = 0;
        S_EXE_MEM_WB_FwdStage = 0;
        S_EXE_MEM_MEM_Branch = 0;
        S_EXE_MEM_MEM_MemWrite = 0;
        S_EXE_MEM_MEM_MemRead = 0;

        //Wire connections M stage
        S_MEM_WB_DATA_MemReadData = 0;
        S_MEM_WB_DATA_AluResult = 0;
        S_MEM_WB_DATA_DstReg = 0;

        S_MEM_WB_WB_MemToReg = 0;
        S_MEM_WB_WB_RegWrite = 0;
        S_MEM_WB_WB_FwdStage = 0;
    end

    //Output halt signal
    assign halt = ctrl_Halt;
endmodule
