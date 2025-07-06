----------------------------------------------------------------------------------
-- Company: Digilent
-- Engineer: Arthur Brown, Sam Bobrowicz
-- 
--
-- Create Date:    13:01:51 02/15/2013 
-- Project Name:   pmodvga
-- Tool versions:  2016.4
-- Additional Comments: 
--
-- Copyright Digilent 2017
----------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
USE IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

ENTITY top IS
    PORT (
        pxl_clk: IN STD_LOGIC;
        CLK_I : IN STD_LOGIC;
        up : IN STD_LOGIC;
        an : OUT STD_LOGIC;
        down : IN STD_LOGIC;
        seg : OUT STD_LOGIC_VECTOR(6 DOWNTO 0);
        VGA_HS_O : OUT STD_LOGIC;
        VGA_VS_O : OUT STD_LOGIC;
        VGA_R : OUT STD_LOGIC_VECTOR (3 DOWNTO 0);
        VGA_B : OUT STD_LOGIC_VECTOR (3 DOWNTO 0);
        VGA_G : OUT STD_LOGIC_VECTOR (3 DOWNTO 0);
        pause_active : IN STD_LOGIC;
        status : OUT STD_LOGIC;
        restart : IN STD_LOGIC);
END top;

ARCHITECTURE Behavioral OF top IS

    COMPONENT DisplayController IS
        PORT (
            --output from the Decoder
            DispVal : IN STD_LOGIC_VECTOR (3 DOWNTO 0);
            --controls which digit to display
            segOut : OUT STD_LOGIC_VECTOR (6 DOWNTO 0));
    END COMPONENT;

    COMPONENT DeBounce IS
        GENERIC (
            clk_freq : INTEGER := 50_000_000; --system clock frequency in Hz 
            stable_time : INTEGER := 10); --time button must remain stable in ms
        PORT (
            clk : IN STD_LOGIC; --input clock
            reset_n : IN STD_LOGIC; --asynchronous active low reset
            button : IN STD_LOGIC; --input signal to be debounced
            result : OUT STD_LOGIC); --debounced signal
    END COMPONENT;

    COMPONENT clk_wiz_0
        PORT (-- Clock in ports
            CLK_IN1 : IN STD_LOGIC;
            -- Clock out ports
            CLK_OUT1 : OUT STD_LOGIC
        );
    END COMPONENT;

    --***1920x1080@60Hz***-- Requires 148.5 MHz pxl_clk
    SIGNAL score : STD_LOGIC_VECTOR(3 DOWNTO 0);

    CONSTANT FRAME_WIDTH : NATURAL := 1920;
    CONSTANT FRAME_HEIGHT : NATURAL := 1080;

    SIGNAL pulse_out : STD_LOGIC;
    SIGNAL reset : STD_LOGIC := '0';

    CONSTANT H_FP : NATURAL := 88; --H front porch width (pixels)
    CONSTANT H_PW : NATURAL := 44; --H sync pulse width (pixels)
    CONSTANT H_MAX : NATURAL := 2200; --H total period (pixels)

    CONSTANT V_FP : NATURAL := 4; --V front porch width (lines)
    CONSTANT V_PW : NATURAL := 5; --V sync pulse width (lines)
    CONSTANT V_MAX : NATURAL := 1125; --V total period (lines)

    CONSTANT H_POL : STD_LOGIC := '1';
    CONSTANT V_POL : STD_LOGIC := '1';

    SIGNAL pause : STD_LOGIC;

    SIGNAL dig_sel : STD_LOGIC;

    --Moving Box constants
    CONSTANT BOX_WIDTH : NATURAL := 30;
    SIGNAL BOX_CLK_DIV : INTEGER := 500000; --MAX=(2^25 - 1) 1000000
    SIGNAL BOX_CLK_DIV_PREV : INTEGER;
    SIGNAL BOX_X_MAX : INTEGER := (FRAME_WIDTH);
    SIGNAL BOX_Y_MAX : INTEGER := (FRAME_HEIGHT);

    SIGNAL BOX_X_MIN : INTEGER := 0;
    SIGNAL BOX_Y_MIN : INTEGER := 0;

    CONSTANT BOX_X_INIT : STD_LOGIC_VECTOR(11 DOWNTO 0) := x"000";
    CONSTANT BOX_Y_INIT : STD_LOGIC_VECTOR(11 DOWNTO 0) := x"190"; --400

   
    SIGNAL active : STD_LOGIC;

    SIGNAL h_cntr_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
    SIGNAL v_cntr_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');

    SIGNAL h_cntr_reg_sqr : signed(23 DOWNTO 0) := (OTHERS => '0');
    SIGNAL v_cntr_reg_sqr : signed(23 DOWNTO 0) := (OTHERS => '0');
    SIGNAL expr : signed (24 DOWNTO 0) := (OTHERS => '0');
    SIGNAL radius : signed(3 DOWNTO 0) := "1111";

    SIGNAL h_sync_reg : STD_LOGIC := NOT(H_POL);
    SIGNAL v_sync_reg : STD_LOGIC := NOT(V_POL);

    SIGNAL h_sync_dly_reg : STD_LOGIC := NOT(H_POL);
    SIGNAL v_sync_dly_reg : STD_LOGIC := NOT(V_POL);

    SIGNAL vga_red_reg : STD_LOGIC_VECTOR(3 DOWNTO 0) := (OTHERS => '0');
    SIGNAL vga_green_reg : STD_LOGIC_VECTOR(3 DOWNTO 0) := (OTHERS => '0');
    SIGNAL vga_blue_reg : STD_LOGIC_VECTOR(3 DOWNTO 0) := (OTHERS => '0');

    SIGNAL vga_red : STD_LOGIC_VECTOR(3 DOWNTO 0);
    SIGNAL vga_green : STD_LOGIC_VECTOR(3 DOWNTO 0);
    SIGNAL vga_blue : STD_LOGIC_VECTOR(3 DOWNTO 0);

    SIGNAL box_x_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := BOX_X_INIT;
    SIGNAL box_x_dir : STD_LOGIC := '1';
    SIGNAL box_y_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := BOX_Y_INIT;
    SIGNAL box_y_dir : STD_LOGIC := '1';
    SIGNAL box_cntr_reg : STD_LOGIC_VECTOR(24 DOWNTO 0) := (OTHERS => '0');

    SIGNAL mover : signed(32 DOWNTO 0) := (OTHERS => '0'); --300
    SIGNAL mover2 : signed(32 DOWNTO 0) := (OTHERS => '0');

    SIGNAL update_box : STD_LOGIC;
    SIGNAL pixel_in_box : STD_LOGIC;

    SIGNAL h_cntr_val : signed(23 DOWNTO 0);
    SIGNAL v_cntr_val : signed(23 DOWNTO 0);

    SIGNAL trigger : STD_LOGIC;
    SIGNAL player1 : STD_LOGIC;
    SIGNAL player2 : STD_LOGIC;

    SIGNAL horz1 : STD_LOGIC_VECTOR(8 DOWNTO 0) := "000010100"; --20
    SIGNAL horz2 : STD_LOGIC_VECTOR(8 DOWNTO 0) := "000110010"; --50

    SIGNAL horz1_p2 : STD_LOGIC_VECTOR(10 DOWNTO 0) := "11100111010"; --1850 11101101100
    SIGNAL horz2_p2 : STD_LOGIC_VECTOR(10 DOWNTO 0) := "11101011000"; --1900 11100111010

    SIGNAL vert1_p2 : STD_LOGIC_VECTOR(10 DOWNTO 0) := (OTHERS => '0');
    SIGNAL vert2_p2 : STD_LOGIC_VECTOR(10 DOWNTO 0) := "00001100100";

    SIGNAL vert1 : STD_LOGIC_VECTOR(10 DOWNTO 0) := (OTHERS => '0');
    SIGNAL vert2 : STD_LOGIC_VECTOR(10 DOWNTO 0) := "00001100100";

    SIGNAL racketp1 : INTEGER := 0;
    SIGNAL racketp2 : INTEGER := 0;

    SIGNAL counter : INTEGER := 0;
    SIGNAL counter2 : INTEGER := 0;
    SIGNAL counter3 : INTEGER := 0;
    SIGNAL counter4 : INTEGER := 0;
    SIGNAL count_main2 : STD_LOGIC_VECTOR(19 DOWNTO 0) := (OTHERS => '0');

    SIGNAL scorep1, scorep2 : STD_LOGIC_VECTOR(3 DOWNTO 0);
    SIGNAL segp1, segp2 : STD_LOGIC_VECTOR(6 DOWNTO 0);
    SIGNAL game_end : STD_LOGIC := '0';

    SIGNAL trigger_clock : STD_LOGIC := '0';

BEGIN

    --disp: DisplayController                  
    --port map (segOut=>Seg, DispVal => score);

    disp : DisplayController
    PORT MAP(segOut => segp1, DispVal => scorep1);

    disp1 : DisplayController
    PORT MAP(segOut => segp2, DispVal => scorep2);

    --component DeBounce IS
    --  GENERIC(
    --    clk_freq    : INTEGER := 50_000_000;  --system clock frequency in Hz 
    --    stable_time : INTEGER := 10);         --time button must remain stable in ms
    --  PORT(
    --    clk     : IN  STD_LOGIC;  --input clock
    --    reset_n : IN  STD_LOGIC;  --asynchronous active low reset
    --    button  : IN  STD_LOGIC;  --input signal to be debounced
    --    result  : OUT STD_LOGIC); --debounced signal
    --END component;

    debounce_1 : DeBounce
    GENERIC MAP(clk_freq => 150_000_000, stable_time => 20)
    PORT MAP(clk => pxl_clk, reset_n => reset, button => pause_active, result => pulse_out);

--    clk_div_inst : clk_wiz_0
--    PORT MAP
--    (-- Clock in ports
--        CLK_IN1 => CLK_I,
--        -- Clock out ports
--        CLK_OUT1 => pxl_clk);
    ----------------------------------------------------
    -------         TEST PATTERN LOGIC           -------
    ----------------------------------------------------
    h_cntr_reg_sqr <= to_signed((to_integer(signed(h_cntr_reg) - signed(box_x_reg)) ** 2), 24);
    v_cntr_reg_sqr <= to_signed((to_integer(signed(v_cntr_reg) - signed(box_y_reg)) ** 2), 24);
    expr <= (v_cntr_reg_sqr(23) & v_cntr_reg_sqr) + (h_cntr_reg_sqr(23) & h_cntr_reg_sqr);
    vga_red <= (OTHERS => trigger);
    vga_blue <= (OTHERS => trigger);
    vga_green <= (OTHERS => trigger);
    ------------------------------------------------------
    -------        VIDEO GAME LOGIC              ------
    ------------------------------------------------------

    --choose display--
    seg <= segp1 WHEN (count_main2(19) = '1') ELSE
        segp2;
    an <= count_main2(19);

    PROCESS (pxl_clk) BEGIN

        IF (rising_edge(pxl_clk)) THEN
            IF (pause_active = '1') THEN
                pause <= NOT pause;
            END IF;
        END IF;

    END PROCESS;

    PROCESS (pxl_clk)
    BEGIN

        IF (rising_edge(pxl_clk)) THEN
            IF (update_box = '1') THEN
                IF (box_x_dir = '0') THEN
                    IF (box_x_reg = 70) THEN
                        game_end <= '1';
                    END IF;
                END IF;

                IF (box_x_dir = '1') THEN
                    IF (box_x_reg = 71) THEN
                        game_end <= '0';
                    END IF;
                END IF;

            END IF;
        END IF;
    END PROCESS;
    PROCESS (pxl_clk)
    BEGIN

        IF (rising_edge(pxl_clk)) THEN

            count_main2 <= count_main2 + 1;

        END IF;

    END PROCESS;

    --p1 has missed the ball so add score
    PROCESS (game_end)
    BEGIN
        IF (rising_edge(game_end)) THEN
            trigger_clock <= '1';
            scorep2 <= STD_LOGIC_VECTOR(unsigned(scorep2) + 1);
        END IF;
    END PROCESS;

    --main ball logic---
    PROCESS (pxl_clk)
    BEGIN

        IF (rising_edge(pxl_clk)) THEN

            IF (update_box = '1') THEN

                IF (game_end = '1') THEN --player 1 misses the game
                    IF (trigger_clock = '1' AND box_clk_div /= 600000) THEN
                        box_clk_div <= box_clk_div + 100;
                    END IF;
                    racketp1 <= 0;
                END IF;

            END IF;

            IF (pause = '0') THEN
                IF (update_box = '1') THEN

                    IF (box_x_dir = '1') THEN
                        box_x_reg <= box_x_reg + 1;

                        IF (box_x_reg = 155 AND box_y_reg <= (vert2 + 30) AND box_y_reg >= (vert1 - 30)) THEN
                            racketp1 <= racketp1 + 1; --count hits
                        END IF;

                    ELSE
                        --+1 point granted to player 2
                        box_x_reg <= box_x_reg - 1;
                        IF (box_x_reg = 1700) THEN
                            racketp1 <= racketp1 + 1;
                        END IF;

                    END IF;

                    IF (box_y_dir = '1') THEN
                        box_y_reg <= box_y_reg + 1;
                    ELSE
                        box_y_reg <= box_y_reg - 1;
                    END IF;

                    IF (racketp1 /= 0 AND (racketp1 MOD 8 = 0)) THEN
                        box_clk_div <= box_clk_div - 100;
                        status <= '1';
                    ELSE
                        status <= '0';
                    END IF;

                END IF;-- update

            END IF; --pause = 0

        END IF; -- rising edge of the clock
    END PROCESS;

    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF (pause = '0') THEN
                IF (update_box = '1') THEN
                    IF ((box_x_dir = '1' AND (box_x_reg = BOX_X_MAX - 1)) OR (box_x_dir = '0' AND (box_x_reg = BOX_X_MIN + 1))) THEN
                        box_x_dir <= NOT(box_x_dir);
                    END IF;
                    IF ((box_y_dir = '1' AND (box_y_reg = BOX_Y_MAX - 1)) OR (box_y_dir = '0' AND (box_y_reg = BOX_Y_MIN + 1))) THEN
                        box_y_dir <= NOT(box_y_dir);
                    END IF;
                END IF;
            END IF;
        END IF;
    END PROCESS;

    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF (box_cntr_reg = (BOX_CLK_DIV - 1)) THEN
                box_cntr_reg <= (OTHERS => '0');
            ELSE
                box_cntr_reg <= box_cntr_reg + 1;
            END IF;
        END IF;
    END PROCESS;

    update_box <= '1' WHEN box_cntr_reg = (BOX_CLK_DIV - 1) ELSE
        '0';

    --  pixel_in_box <= '1' when (((h_cntr_reg >= box_x_reg) and (h_cntr_reg < (box_x_reg + BOX_WIDTH))) and
    --                            ((v_cntr_reg >= box_y_reg) and (v_cntr_reg < (box_y_reg + BOX_WIDTH)))) else
    --                  '0';
    --deflecting the ball from the racket
    PROCESS (pxl_clk)
    BEGIN

        IF (rising_edge(pxl_clk)) THEN
            IF (pause = '0') THEN
                IF (update_box = '1') THEN

                    IF (box_y_reg <= (vert2 + 60) AND box_y_reg >= (vert1 - 90) AND box_x_reg >= horz1 AND box_x_reg <= 150) THEN

                        box_x_min <= to_integer(unsigned(horz2)) + 60; --old value: 5

                    ELSE
                        box_x_min <= 0;
                        box_y_min <= 0;
                    END IF;

                    IF (box_y_reg <= vert2_p2 AND box_y_reg >= vert1_p2) THEN

                        box_x_max <= to_integer(unsigned(horz2_p2)) - 100; --old value: 5
                        -- box_y_max <= to_integer(unsigned(vert2_p2))- 100;

                    END IF;
                END IF;
            END IF;
        END IF;

    END PROCESS;

    PROCESS (pxl_clk)
    BEGIN

        IF (rising_edge(pxl_clk)) THEN
            IF (pause = '0') THEN
                IF (update_box = '1') THEN

                    IF (box_y_reg < vert2_p2 AND box_y_reg < vert2_p2 AND vert1_p2 /= 0) THEN
                        vert2_p2 <= vert2_p2 - 1;
                        vert1_p2 <= vert1_p2 - 1;
                    ELSIF (box_y_reg > vert2_p2 AND box_y_reg > vert2_p2) THEN
                        vert2_p2 <= vert2_p2 + 1;
                        vert1_p2 <= vert1_p2 + 1;
                    END IF;
                END IF;
            END IF;
        END IF;
    END PROCESS;
    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF (pause = '0') THEN
                IF (up = '1') THEN

                    IF (counter = 500000) THEN

                        vert1 <= STD_LOGIC_VECTOR(unsigned(vert1) + 1);
                        vert2 <= STD_LOGIC_VECTOR(unsigned(vert2) + 1);
                        counter <= 0;

                    ELSE

                        counter <= counter + 1;

                    END IF;

                ELSIF (down = '1') THEN

                    IF (counter = 500000) THEN

                        vert1 <= STD_LOGIC_VECTOR(unsigned(vert1) - 1);
                        vert2 <= STD_LOGIC_VECTOR(unsigned(vert2) - 1);
                        counter <= 0;

                    ELSE

                        counter <= counter + 1;

                    END IF;

                END IF;
            END IF;

        END IF;

    END PROCESS;

    player2 <= '1' WHEN ((active = '1' AND (((h_cntr_reg > horz1_p2 AND h_cntr_reg < horz2_p2)) AND ((v_cntr_reg > vert1_p2 AND v_cntr_reg < vert2_p2))))) ELSE
        '0';

    player1 <= '1' WHEN ((active = '1' AND (((h_cntr_reg > horz1 AND h_cntr_reg < horz2)) AND ((v_cntr_reg > vert1 AND v_cntr_reg < vert2))))) ELSE
        '0';
    trigger <= '1' WHEN (active = '1' AND (expr <= 1024)) OR (active = '1' AND player1 = '1') OR (active = '1' AND player2 = '1') ELSE
        '0';

    ------------------------------------------------------------------------------------------------------------
    -------                        SYNC GENERATION    [DO NOT CHANGE ANYTHING IN THIS]                    ------
    ------------------------------------------------------------------------------------------------------------

    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF (h_cntr_reg = (H_MAX - 1)) THEN
                h_cntr_reg <= (OTHERS => '0');
            ELSE
                h_cntr_reg <= h_cntr_reg + 1;
            END IF;
        END IF;
    END PROCESS;

    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF ((h_cntr_reg = (H_MAX - 1)) AND (v_cntr_reg = (V_MAX - 1))) THEN
                v_cntr_reg <= (OTHERS => '0');
            ELSIF (h_cntr_reg = (H_MAX - 1)) THEN
                v_cntr_reg <= v_cntr_reg + 1;
            END IF;
        END IF;
    END PROCESS;

    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF (h_cntr_reg >= (H_FP + FRAME_WIDTH - 1)) AND (h_cntr_reg < (H_FP + FRAME_WIDTH + H_PW - 1)) THEN
                h_sync_reg <= H_POL;
            ELSE
                h_sync_reg <= NOT(H_POL);
            END IF;
        END IF;
    END PROCESS;
    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF (v_cntr_reg >= (V_FP + FRAME_HEIGHT - 1)) AND (v_cntr_reg < (V_FP + FRAME_HEIGHT + V_PW - 1)) THEN
                v_sync_reg <= V_POL;
            ELSE
                v_sync_reg <= NOT(V_POL);
            END IF;
        END IF;
    END PROCESS;
    active <= '1' WHEN ((h_cntr_reg < FRAME_WIDTH) AND (v_cntr_reg < FRAME_HEIGHT))ELSE
        '0';

    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            v_sync_dly_reg <= v_sync_reg;
            h_sync_dly_reg <= h_sync_reg;
            vga_red_reg <= vga_red;
            vga_green_reg <= vga_green;
            vga_blue_reg <= vga_blue;
        END IF;
    END PROCESS;

    VGA_HS_O <= h_sync_dly_reg;
    VGA_VS_O <= v_sync_dly_reg;
    VGA_R <= vga_red_reg;
    VGA_G <= vga_green_reg;
    VGA_B <= vga_blue_reg;

END Behavioral;