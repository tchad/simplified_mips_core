`include "defs.v"

module IMem(clk, addr, instr);
    parameter SIZE_WORDS = 17;
    parameter DATA_FILE = "instr.txt";
    parameter DEBUG = 1;

    input clk;
    input [`ADDR_SIZE-1:0] addr;

    output [`WORD_SIZE-1:0] instr;

    reg [`WORD_SIZE-1:0] mem [0:SIZE_WORDS-1];
    reg [`ADDR_SIZE-1:0] addr_d;

    initial begin : IMEM_INIT
        integer f_in, char, bit, word;
        reg [`WORD_SIZE-1:0] tmp;

        f_in = $fopen(DATA_FILE, "r");

        if(f_in == 0) begin
            $display("Error opening instruction memory data: %s",DATA_FILE);
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

            addr_d = 0;
        end

        $fclose(f_in);

        if(DEBUG == 1) begin
            $display("Instruction memory loaded");
            $display("Source file: %s", DATA_FILE);
            $display("Addr size: %3d bits, Word size: %3d bits, Word count: %3d", `ADDR_SIZE, `WORD_SIZE, SIZE_WORDS);
            $display("Content:");
            for(word = 0; word < SIZE_WORDS; word = word + 1) begin
                $display("0x%x: %b", word, mem[word]);
            end
        end
    end //IMEM_INIT

    always @(negedge clk) begin
        //This memory is expected always to read with word granularity 
        addr_d <= #1 addr >> 2;
    end

    assign instr = mem[addr_d];
endmodule
    
