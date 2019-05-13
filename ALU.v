/*
* EE275 Mini Project 2: MIPS Pipeline
* Tomasz Chadzynski
* SJSU, Spring 2018
* 
* Component: ALU
* Description: This component implements the arithmetic logic unit
*/

`include "defs.v"

module ALU(alu_op, arg1, arg2, result, zero);
    parameter DEBUG = 1;
    parameter ALU_ARG_SIZE = 32;
    parameter ALU_RESULT_SIZE = 32;

    //Alu Operation(definitions in defs.v)
    input [3:0] alu_op;

    //First argument
    input [ALU_ARG_SIZE-1:0] arg1;

    //Second argument
    input [ALU_ARG_SIZE-1:0] arg2;

    //Result
    output [ALU_RESULT_SIZE-1:0] result;

    //Output 1'b1 when two operands are equal
    output zero;

    reg [ALU_RESULT_SIZE-1:0] result_d;
    always @(*) begin: MAIN
        //Prepare two set od registers for signed and unsigned operations
        reg unsigned [ALU_ARG_SIZE-1:0] ua, ub;
        reg signed [ALU_ARG_SIZE-1:0] sa, sb;

        case(alu_op) 
            //Unsigned add
            `ALUOP_ADD_U: begin
                ua = arg1;
                ub = arg2;
                result_d = ua + ub;
            end
            //Signed add
            `ALUOP_ADD_S: begin
                sa = arg1;
                sb = arg2;
                result_d = sa + sb;
            end
            //Signed multiplication
            `ALUOP_MUL_S: begin
                sa = arg1;
                sb = arg2;
                result_d = sa * sb;
            end
            //Signed subtraction
            `ALUOP_SUB_S: begin
                sa = arg1;
                sb = arg2;
                result_d = sa - sb;
            end
            default: begin
                //Default action not expected to occur,
                //Enable debug to catch
                if(DEBUG == 1) begin
                    $display("ALU: Unknown operation %x" , alu_op);
                end

                result_d = -1;
            end
        endcase
    end

    assign zero = (result_d == 0) ? 1'b1 : 1'b0;
    assign result = result_d;
    
endmodule
