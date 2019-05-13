/*
* EE275 Mini Project 2: MIPS Pipeline
* Tomasz Chadzynski
* SJSU, Spring 2018
* 
* Description: Utility functions
*/

`include "defs.v"

`ifndef __util_instr__
`define __uril_instr__

//General functions extracting fields from instructions
function [`FLD_OPCODE_SIZE-1:0] InstrFieldOpcode;
    input [`REG_SIZE-1:0] instr;
    begin
        InstrFieldOpcode = instr[31:26];
    end
endfunction

//Functions extracting fields from R-type instruction
function [`FLD_REGNUM_SIZE-1:0] InstrRFieldRs;
    input [`REG_SIZE-1:0] instr;
    begin
        InstrRFieldRs = instr[25:21];
    end
endfunction

function [`FLD_REGNUM_SIZE-1:0] InstrRFieldRt;
    input [`REG_SIZE-1:0] instr;
    begin
        InstrRFieldRt = instr[20:16];
    end
endfunction

function [`FLD_REGNUM_SIZE-1:0] InstrRFieldRd;
    input [`REG_SIZE-1:0] instr;
    begin
        InstrRFieldRd = instr[15:11];
    end
endfunction

function [`FLD_SHAMT_SIZE-1:0] InstrRFieldShamt;
    input [`REG_SIZE-1:0] instr;
    begin
        InstrRFieldShamt = instr[10:6];
    end
endfunction

function [`FLD_FUNCT_SIZE-1:0] InstrRFieldFunct;
    input [`REG_SIZE-1:0] instr;
    begin
        InstrRFieldFunct = instr[5:0];
    end
endfunction

function [`FLD_REGNUM_SIZE-1:0] InstrIFieldRs;
    input [`REG_SIZE-1:0] instr;
    begin
        InstrIFieldRs = instr[25:21];
    end
endfunction

function [`FLD_REGNUM_SIZE-1:0] InstrIFieldRt;
    input [`REG_SIZE-1:0] instr;
    begin
        InstrIFieldRt = instr[20:16];
    end
endfunction

function [`FLD_IMM_SIZE-1:0] InstrIFieldImm;
    input [`REG_SIZE-1:0] instr;
    begin
        InstrIFieldImm = instr[15:0];
    end
endfunction

function [`FLD_JADDR_SIZE:0] InstrJFieldAddr;
    input [`REG_SIZE-1:0] instr;
    begin
        InstrJFieldAddr = instr[25:0];
    end
endfunction


//Functions checking the type of instruction
function InstrIsR;
    input [`REG_SIZE-1:0] instr;
    reg [5:0] opcode;
    begin
        opcode = InstrFieldOpcode(instr);
        InstrIsR = (opcode == `OPCODE_R) ? 1'b1 : 1'b0;
    end
endfunction
    
function InstrIsI;
    input [`REG_SIZE-1:0] instr;
    reg [5:0] opcode;
    begin
        opcode = InstrFieldOpcode(instr);
        case(opcode)
            `OPCODE_I_BEQ: InstrIsI = 1'b1;
            `OPCODE_I_LW: InstrIsI = 1'b1;
            `OPCODE_I_ADDIU: InstrIsI = 1'b1;
            default : InstrIsI = 1'b0;
        endcase
    end
endfunction

function InstrIsJ;
    input [`REG_SIZE-1:0] instr;
    reg [5:0] opcode;
    begin
        opcode = InstrFieldOpcode(instr);
        InstrIsJ = (opcode == `OPCODE_J_J) ? 1'b1 : 1'b0;
    end
endfunction

function InstrIsNop;
    input [`REG_SIZE-1:0] instr;
    reg [5:0] opcode;
    begin
        opcode = InstrFieldOpcode(instr);
        InstrIsNop = (opcode == `OPCODE_NOP) ? 1'b1 : 1'b0;
    end
endfunction

function InstrIsHlt;
    input [`REG_SIZE-1:0] instr;
    reg [5:0] opcode;
    begin
        opcode = InstrFieldOpcode(instr);
        InstrIsHlt = (opcode == `OPCODE_HLT) ? 1'b1 : 1'b0;
    end
endfunction

`endif
