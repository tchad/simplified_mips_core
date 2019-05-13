/*
* EE275 Mini Project 2: MIPS Pipeline
* Tomasz Chadzynski
* SJSU, Spring 2018
* 
* Component: Alu Operation Control
* Description: The component takes the signel defining alu operation
* from two sources. Either the alu operation is determined by control unit 
* and reside in the control portion of ID/EXE stage register
* or if the stage register contains ALU_CTRL_FUNC then the alu operation is
* determined by the funct field in R-type instruction
*/

`include "defs.v"

module ALUCtrl(ctrl, funct, aluop);
    parameter DEBUG = 1;

    //Control signal from control unit
    input [3:0] ctrl;

    //Funct field from R-Type instruction
    input [`FLD_FUNCT_SIZE-1:0] funct;

    //Determined alu operation
    output [3:0] aluop;

    reg [3:0] aluop_d;
    always @ (*) begin
        if(ctrl == `ALU_CTRL_FUNC) begin
            //Determine alu op based on funct field
            case(funct)
                `FUNCT_JR: begin
                    //ALU Operation not important
                   aluop_d = `ALUOP_ADD_S;
                end
                `FUNCT_ADDU: begin
                    aluop_d = `ALUOP_ADD_U;
                end
                `FUNCT_ADD: begin
                    aluop_d = `ALUOP_ADD_S;
                end
                `FUNCT_MUL: begin
                    aluop_d = `ALUOP_MUL_S;
                end
                default: begin
                    //Default unexpected argument
                    if(DEBUG == 1) begin 
                        $display("ALUCtrl: Unrecognized FUNCT in R-INSTR section: %b", funct);
                    end
                    aluop_d = `ALUOP_ADD_S;
                end
            endcase
        end else begin
            //Determine alu operation dictated by control unit
            case(ctrl)
                `ALU_CTRL_SUB_S: begin
                    aluop_d = `ALUOP_SUB_S;
                end
                `ALU_CTRL_ADD_S: begin
                    aluop_d = `ALUOP_ADD_S;
                end
                `ALU_CTRL_ADD_U: begin
                    aluop_d = `ALUOP_ADD_U;
                end
                default: begin
                    if(DEBUG == 1) 
                        $display("ALUCtrl: Unrecognized ALU_CTRL_OP in ctrl section: %b", ctrl);
                   aluop_d = `ALUOP_ADD_S;
               end
           endcase 
        end
    end

    assign aluop = aluop_d;
endmodule
