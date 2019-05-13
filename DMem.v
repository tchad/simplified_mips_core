/*
* EE275 Mini Project 2: MIPS Pipeline
* Tomasz Chadzynski
* SJSU, Spring 2018
* 
* Component: Simulated data  memory module
* Description: This module serves as a helper module in the test rig. It
* populates the content of data memory from file. This module expects 4 byte
* aligned addresses. Operates on falling edge of the clock.
*/

`include "defs.v"

module DMem(clk, addr, dta_write, mem_read, mem_write, dta_out);
    //Number of words withing the memory (must match number of lines in input
    //file)
    parameter SIZE_WORDS = 32;

    //Input data file
    parameter DATA_FILE = "data.txt";

    //Output destination file
    parameter DUMP_FILE = "dmem.txt";
    parameter DEBUG = 1;

    //Clock
    input clk;
    input [`ADDR_SIZE-1:0] addr;
    input [`WORD_SIZE-1:0] dta_write;
    input mem_read;
    input mem_write;

    output [`WORD_SIZE-1:0] dta_out;

    reg [`WORD_SIZE-1:0] mem [0:SIZE_WORDS-1];

    initial begin : DMEM_INIT
        //Initialize memory content from file on start.
        //Reads 0 or 1 characters from file and sets corresponding
        //bit value
        integer f_in, char, bit, word;
        reg [`WORD_SIZE-1:0] tmp;

        f_in = $fopen(DATA_FILE, "r");

        if(f_in == 0) begin
            $display("Error opening data memory data: %s",DATA_FILE);
            $finish;
        end else begin
            for(word = 0; word < SIZE_WORDS; word = word + 1) begin
                for(bit = 0; bit < `WORD_SIZE; bit = bit + 1) begin
                    char = $fgetc(f_in);

                    if(char == "0") begin
                        tmp[`WORD_SIZE - 1 - bit] = 1'b0;
                    end else if(char == "1") begin
                        tmp[`WORD_SIZE - 1 - bit] = 1'b1;
                    end else begin
                        $display("Unrecognized character %c in %s", char, DATA_FILE);
                        $finish;
                    end

                end
                char = $fgetc(f_in);
                mem[word] = tmp;
            end
        end

        $fclose(f_in);

        if(DEBUG == 1) begin
            $display("Data memory loaded");
            $display("Source file: %s", DATA_FILE);
            $display("Addr size: %3d bits, Word size: %3d bits, Word count: %3d", `ADDR_SIZE, `WORD_SIZE, SIZE_WORDS);
            $display("Content:");
            for(word = 0; word < SIZE_WORDS; word = word + 1) begin
                $display("0x%x: %b", word, mem[word]);
            end
        end
    end // DMEM_INIT

    task dump_mem;
        //Output memory content into file when called
        integer f_out, word;

        begin
            f_out = $fopen(DUMP_FILE, "w");
            
            for(word = 0; word < SIZE_WORDS; word = word + 1) begin
                $fwrite(f_out, "0x%x: %b (%d)\n", word, mem[word], mem[word]);
            end
            
            $fclose(f_out);
        end
    endtask


    reg [`WORD_SIZE-1:0] dta_out_d;
    always @(negedge clk) begin
        if(mem_read == 0 && mem_write == 0) begin
            //Output 0 when not in reading and writing mode
            dta_out_d <= #1 `WORD_SIZE'b0;
        end else if( mem_read == 1 && mem_write == 1) begin
            //Special case, cannot read and write at the same time
            if(DEBUG == 1) begin
                $display("DMem error, attempt read and write at the same time");
                $display("%x: %b (%d)",addr, dta_write, dta_write);
                $finish;
            end
        end else if( mem_read == 1 && mem_write == 0) begin
            //Read from memory
            //Address shif two positions left to align with cpu addressing
            dta_out_d = mem[addr >> 2];
        end else if( mem_read == 0 && mem_write == 1) begin
            //Write to memory 
            //Address shif two positions left to align with cpu addressing
            mem[addr >> 2] <= #1 dta_write;
            dta_out_d <= #1 `WORD_SIZE'b0;
        end
    end

    assign dta_out = dta_out_d;
endmodule
