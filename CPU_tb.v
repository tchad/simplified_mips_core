/*
* EE275 Mini Project 2: MIPS Pipeline
* Tomasz Chadzynski
* SJSU, Spring 2018
* 
* Component: Master testbench
* Description: Master testbench, instantiates data memory, instruction memory
* and cpu components. Runs the clock unit the halt signal appears. After halt
* appears, dump the memory content of data memory and register file and pring
* cycles.
*/

`timescale 1ns/10ps

`include "defs.v"
`include "IMem.v"
`include "DMem.v"
`include "CPU.v"

/*
* NOTE: The assumed convention is that the connecting wires carry the module
* instance name for which they are the output signal
*
* NOTE: The memories are treated as external modules which provides
* flexibility in switching between simulated memory modules and more realistic ones.
* Currently the main CPU module is using simulated instruction and data memory modules.
*/

module CPU_tb();
    //Define 0- no forwarding, 1-forwarding enabled
    parameter ENABLE_FORWARDING = 1'b1;
    
    //Main clock signal
    reg clk = 1'b0;
    
    //Cycle count
    integer clk_count = 0;

    //Instruction memory output connections
    wire [`WORD_SIZE-1:0] U0_imem_instr;

    //Data Memory output connections
    wire [`WORD_SIZE-1:0] U1_dmem_dta_out;

    //CPU module output connections
    wire U2_imem_clk;
    wire [`ADDR_SIZE-1:0] U2_imem_addr;
    wire U2_dmem_clk;
    wire U2_dmem_mem_read;
    wire U2_dmem_mem_write;
    wire [`ADDR_SIZE-1:0] U2_dmem_addr;
    wire [`WORD_SIZE-1:0] U2_dmem_dta_write;

    //Halt signal
    wire halt;

    //Instantiate Instruction memory
    defparam U0.DATA_FILE = "input/instr.txt";
    defparam U0.DEBUG = 1;
    IMem U0(.clk(U2_imem_clk),
            .addr(U2_imem_addr),
            .instr(U0_imem_instr));

    //Instantiate data memory
    defparam U1.DATA_FILE = "input/data.txt";
    defparam U1.DUMP_FILE = "report/dmem.txt";
    defparam U1.DEBUG = 1;
    DMem U1(.clk(U2_dmem_clk),
            .addr(U2_dmem_addr),
            .dta_write(U2_dmem_dta_write),
            .mem_read(U2_dmem_mem_read),
            .mem_write(U2_dmem_mem_write),
            .dta_out(U1_dmem_dta_out));


    //Instantiate CPU
    defparam U2.DEBUG = 0;
    defparam U2.DEBUG_REG_FILE = 0;
    defparam U2.DEBUG_ALU = 0;
    defparam U2.FORWARDING_EN = ENABLE_FORWARDING;
    defparam U2.REG_FILE_DATA_FILE = "input/reg.txt";
    defparam U2.REG_FILE_DUMP_FILE = "report/regfile.txt";
    CPU U2(.clk(clk),
           .imem_clk(U2_imem_clk),
           .imem_addr(U2_imem_addr),
           .imem_instr(U0_imem_instr),
           .dmem_clk(U2_dmem_clk),
           .dmem_addr(U2_dmem_addr),
           .dmem_dta_write(U2_dmem_dta_write),
           .dmem_dta_out(U1_dmem_dta_out),
           .dmem_mem_read(U2_dmem_mem_read),
           .dmem_mem_write(U2_dmem_mem_write),
           .halt(halt));

       initial begin : TEST_MAIN
           //Dump variables for timing diagrams
           $dumpfile("report/cpu_run.vcd");
           $dumpvars;

           //Begin clock generation
           forever begin: CLK_BLOCK
               clk = #2 1'b1;
               clk = #2 1'b0;
               clk_count = clk_count +1;
               //Until halt signal appears
               if(halt == 1'b1) begin
                   //Write final DMEM content
                   U1.dump_mem;
                   //Write Final Reg File content
                   U2.U7.dump_mem;
                   //Print cycle count and finish
                   $display("Cycle count: %d", clk_count);
                   $finish;
               end
           end
       end
endmodule
