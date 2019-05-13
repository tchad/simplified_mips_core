`include "defs.v"

module PC(clk, next, stop, curr);
    input clk;
    input stop;
    input [`ADDR_SIZE-1:0] next;

    output [`ADDR_SIZE-1:0] curr;

    reg [`ADDR_SIZE-1:0] curr_d;

    initial begin
        curr_d = `ADDR_SIZE'b0;
    end

    always @ (posedge clk) begin
        if(stop == 1'b0) begin
            curr_d <= #1 next;
        end
    end

    assign curr = curr_d;

endmodule
