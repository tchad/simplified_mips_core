/*
* EE275 Mini Project 2: MIPS Pipeline
* Tomasz Chadzynski
* SJSU, Spring 2018
* 
* Description: Set of global definitions used throughout the codebase
*/


`ifndef __defs__
`define __defs__

`define REG_SIZE 32 //Size in bits of the register
`define INSTR_SIZE 32 //Size in bits of an instruction
`define ADDR_SIZE 32 //Size in bits of an address
`define WORD_SIZE 32 //Size in bits of the word

//Field sizes
//
`define FLD_OPCODE_SIZE 6 //Size of opcode field (R,I,J)
`define FLD_REGNUM_SIZE 5 //Size of the registe number field (R,I,J)
`define FLD_SHAMT_SIZE 5 //Size of the shamt field (R)
`define FLD_FUNCT_SIZE 6 //Size of the funct field (R)
`define FLD_IMM_SIZE 16 //Size of the immediate field (I)
`define FLD_JADDR_SIZE 26 //Size od the address field (J)

//Opcodes and function fields
`define OPCODE_R 6'b000000 //Opcode for R instructions
`define FUNCT_JR 6'b001000 //Opcode of the JR instruction
`define FUNCT_ADDU 6'b100001 //Opcode of the ADDU instruction
`define FUNCT_ADD 6'b100000 //Opcode of the ADD instruction
`define FUNCT_MUL 6'b011000 //Opcode of the mul instruction

`define OPCODE_J_J 6'b000010 //Opcode of the J instruction

`define OPCODE_I_BEQ 6'b000100 //Opcode of the BEQ instruction
`define OPCODE_I_LW 6'b100011 //Opcode of the LW instruction
`define OPCODE_I_ADDIU 6'b001001 //Opcode of the ADDIU instruction

`define OPCODE_NOP 6'b111111 //Opcode of the NOP instruction
`define OPCODE_HLT 6'b011111 //Opcode of the HLT(Halt) instruction

//ALU Operations
//Operation signals going directly into alu
`define ALUOP_ADD_U 4'h0 
`define ALUOP_ADD_S 4'h1
`define ALUOP_MUL_S 4'h2
`define ALUOP_SUB_S 4'h3

//ALU Control flags
//Operation signals going into alu control
`define ALU_CTRL_FUNC 4'b0000
`define ALU_CTRL_SUB_S 4'b1000
`define ALU_CTRL_ADD_S 4'b1001
`define ALU_CTRL_ADD_U 4'b1010

//Forwarding gield values
`define RESULT_FWD_NONE 2'h0
`define RESULT_FWD_MEM 2'h1
`define RESULT_FWD_WB 2'h2

//Macro creating a NOP onstruction
`define MakeNOP {`OPCODE_NOP,26'b0}

`endif
