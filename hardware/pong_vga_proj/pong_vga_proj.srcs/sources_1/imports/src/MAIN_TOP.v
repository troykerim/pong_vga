`timescale 1ns / 1ps

module MAIN_TOP(
input wire CLK_I,
input wire up,
output wire an,
input wire down,
output wire [6:0] seg,
output wire VGA_HS_O,
output wire VGA_VS_O,
output wire [3:0] VGA_R,
output wire [3:0] VGA_B,
output wire [3:0] VGA_G,
input wire pause_active,
output wire status,
input wire restart,
input wire[2:0] game_select
    );
   
reg CLK_I1, CLK_I2, CLK_I3;
reg up1, up2, up3;
wire an1, an2, an3;
reg down1, down2, down3;
wire [6:0] seg1, seg2, seg3;
wire VGA_HS_O1, VGA_HS_O2, VGA_HS_O3;
wire VGA_VS_O1, VGA_VS_O2, VGA_VS_O3;
wire [3:0] VGA_R1, VGA_R2, VGA_R3, VGA_B1, VGA_B2, VGA_B3,VGA_G1, VGA_G2, VGA_G3;
reg pause_active1, pause_active2, pause_active3;
wire status1, status2, status3;
reg restart1, restart2, restart3;
wire clk_out;
reg clk_out1, clk_out2, clk_out3;

top_part1 top1(.CLK_I(CLK_I1),.up(up1), .an(an1), .down(down1), .seg(seg1), .VGA_HS_O(VGA_HS_O1),
.VGA_VS_O(VGA_VS_O1), .VGA_R(VGA_R1), .VGA_G(VGA_G1), .VGA_B(VGA_B1), .pause_active(pause_active)
,.pxl_clk(clk_out1)
);

top top2(.CLK_I(CLK_I2),.up(up2), .an(an2), .down(down2), .seg(seg2), .VGA_HS_O(VGA_HS_O2),
.VGA_VS_O(VGA_VS_O2), .VGA_R(VGA_R2), .VGA_G(VGA_G2), .VGA_B(VGA_B2), .pause_active(pause_active2), 
.status(status2), .restart(restart2),.pxl_clk(clk_out2)
);

top_part3 top3(.CLK_I(CLK_I3),.up(up3), .an(an3), .down(down3), .seg(seg3), .VGA_HS_O(VGA_HS_O3),
.VGA_VS_O(VGA_VS_O3), .VGA_R(VGA_R3), .VGA_G(VGA_G3), .VGA_B(VGA_B3), .pause_active(pause_active3), 
.status(status3), .restart(restart3),.pxl_clk(clk_out3)
);

clk_wiz_0 my_clk (.clk_in1(CLK_I), .clk_out1(clk_out));

//inputs
always @*
begin

if(game_select == 4) 
begin
    CLK_I1 = CLK_I;
    clk_out1 = clk_out;
    up1 = up;
    down1 = down;
    pause_active1 = pause_active;
    restart1 = restart;   
end

else if(game_select == 6) 
begin
    CLK_I2 = CLK_I;
    clk_out2 = clk_out;
    up2 = up;
    down2 = down;
    pause_active2 = pause_active;
    restart2 = restart;   
end

else 
begin
    CLK_I3 = CLK_I;
    clk_out3 = clk_out;
    up3 = up;
    down3 = down;
    pause_active3 = pause_active;
    restart3 = restart;   
end       
end

//outputs
assign seg = (game_select==4) ?  seg1 : (game_select == 6) ? seg2 : seg3;
assign an = (game_select==4) ?  an1 : (game_select == 6) ? an2 : an3 ;
assign VGA_HS_O = (game_select==4) ?  VGA_HS_O1 : (game_select == 6) ? VGA_HS_O2 : VGA_HS_O3;
assign VGA_VS_O = (game_select==4) ?  VGA_VS_O1 : (game_select == 6) ? VGA_VS_O2 : VGA_VS_O3;
assign VGA_R = (game_select==4) ?  VGA_R1 : (game_select == 6) ? VGA_R2 : VGA_R3;
assign VGA_G = (game_select==4) ?  VGA_G1 : (game_select == 6) ? VGA_G2 : VGA_G3;
assign VGA_B = (game_select==4) ?  VGA_B1 : (game_select == 6) ? VGA_B2 : VGA_B3;
assign status = (game_select==4) ?  status1 : (game_select == 6) ? status2 : status3;
    
endmodule
