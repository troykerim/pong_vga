`timescale 1ns / 1ps

module VGA_DRIVER(
input wire clk,
output wire vga_hs_o,
output wire vga_vs_o,
output wire vga_r,
output wire vga_b,
output wire vga_g
    );
    

localparam frame_width = 1920,
frame_height = 1080,
h_fp = 88,
h_pw = 44,
h_max = 2200,
v_fp = 4,
v_pw = 5,
v_max = 1125,
h_pol = 1'b1,
v_pol = 1'b1;



endmodule
