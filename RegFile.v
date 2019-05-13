`include "defs.v"

module RegFile(clk, reg1ReadAddr, reg2ReadAddr, 
               regWrite, regWriteAddr, regWriteData,
               reg1ReadData, reg2ReadData);

    parameter DEBUG = 1;
    parameter DATA_FILE = "reg.txt";
    parameter DUMP_FILE = "regfile.txt";
    parameter REG_ADDR_SIZE = `FLD_REGNUM_SIZE;

    input clk;
    input [REG_ADDR_SIZE-1:0] reg1ReadAddr;
    input [REG_ADDR_SIZE-1:0] reg2ReadAddr;
    input regWrite;
    input [REG_ADDR_SIZE-1:0] regWriteAddr;
    input [`REG_SIZE-1:0] regWriteData;

    output [`REG_SIZE-1:0] reg1ReadData;
    output [`REG_SIZE-1:0] reg2ReadData;

    reg [`REG_SIZE-1:0] data [0:31];

    reg [`REG_SIZE-1:0] reg1ReadData_d;
    reg [`REG_SIZE-1:0] reg2ReadData_d;

    initial begin : REGFILE_INIT
        integer f_in, char, bit, word;
        reg [`REG_SIZE-1:0] tmp;

        f_in = $fopen(DATA_FILE, "r");

        if(f_in == 0) begin
            $display("Error opening register data: %s",DATA_FILE);
            $finish;
        end else begin
            for(word = 0; word < 32; word = word + 1) begin
                for(bit = 0; bit < `REG_SIZE; bit = bit + 1) begin
                    char = $fgetc(f_in);

                    if(char == "0") begin
                        tmp[`REG_SIZE - 1 - bit] = 1'b0;
                    end else if(char == "1") begin
                        tmp[`REG_SIZE - 1 - bit] = 1'b1;
                    end else begin
                        $display("Unrecognized character %c in %s", char, DATA_FILE);
                        $finish;
                    end

                end
                char = $fgetc(f_in);
                data[word] = tmp;
            end
        end

        $fclose(f_in);

        if(DEBUG == 1) begin
            $display("Register file loaded");
            $display("Source file: %s", DATA_FILE);
            $display("Content:");
            for(word = 0; word < 32; word = word + 1) begin
                $display("Reg:%d: %b", word, data[word]);
            end
        end
    end // REGFILE_INIT

    task dump_mem;
        integer f_out, word;

        begin
            f_out = $fopen(DUMP_FILE, "w");
            
            for(word = 0; word < 32; word = word + 1) begin
                $fwrite(f_out, "Reg:%d: %b (%d)\n", word, data[word], data[word]);
            end
            
            $fclose(f_out);
        end
    endtask

    always @(negedge clk) begin : MAIN
        //Write to reg file if write flag is high
        if(regWrite == 1'b1) begin

            $display("RegFile: Writing Reg[%2d]: %d", regWriteAddr, regWriteData);
            data[regWriteAddr] <= #1 regWriteData;
        end

        //read from reg file with write passthrough if write enabled
        if(reg1ReadAddr == 0) begin
            if(DEBUG == 1)
                $display("RegFile: Read reg1 bypass %x: %x", reg1ReadAddr, regWriteData);
            reg1ReadData_d <= #1 `REG_SIZE'b0;
        end else if(regWrite == 1'b1 && regWriteAddr == reg1ReadAddr) begin
            if(DEBUG == 1)
                $display("RegFile: Read reg1 bypass %x: %x", reg1ReadAddr, regWriteData);

            reg1ReadData_d <= #1 regWriteData;
        end else begin
            if(DEBUG == 1)
                $display("RegFile: Read reg1 %x: %x", reg1ReadAddr, data[reg1ReadAddr]);

            reg1ReadData_d <= #1 data[reg1ReadAddr];
        end

        if(reg2ReadAddr == 0) begin
            if(DEBUG == 1)
                $display("RegFile: Read reg2 bypass %x: %x", reg2ReadAddr, regWriteData);
            reg2ReadData_d <= #1 `REG_SIZE'b0;
        end else if(regWrite == 1'b1 && regWriteAddr == reg2ReadAddr) begin
            if(DEBUG == 1)
                $display("RegFile: Read reg2 bypass %x: %x", reg2ReadAddr, regWriteData);

            reg2ReadData_d <= #1 regWriteData;
        end else begin
            if(DEBUG == 1)
                $display("RegFile: Read reg2 %x: %x", reg2ReadAddr, data[reg2ReadAddr]);

            reg2ReadData_d <= #1 data[reg2ReadAddr];
        end
    end

    assign reg1ReadData = reg1ReadData_d;
    assign reg2ReadData = reg2ReadData_d;
endmodule

