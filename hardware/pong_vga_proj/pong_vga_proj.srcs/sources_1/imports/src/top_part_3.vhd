
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
USE IEEE.NUMERIC_STD.ALL;
USE IEEE.MATH_REAL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

ENTITY top_part3 IS
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
END top_part3;

ARCHITECTURE Behavioral OF top_part3 IS

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

    --Moving objects (BALL AND GHOSTS)
    CONSTANT BOX_WIDTH : NATURAL := 30;
    SIGNAL BOX_CLK_DIV : INTEGER := 600000; --MAX=(2^25 - 1) 1000000
    SIGNAL GHOST_CLK_DIV : INTEGER := 600000; --MAX=(2^25 - 1) 1000000
    SIGNAL GHOST2_CLK_DIV : INTEGER := 600000; --MAX=(2^25 - 1) 1000000
    SIGNAL GHOST3_CLK_DIV: INTEGER := 600000;
    SIGNAL BOX_X_MAX : INTEGER := (FRAME_WIDTH);
    SIGNAL BOX_Y_MAX : INTEGER := (FRAME_HEIGHT);

    SIGNAL GHOST_X_MAX : INTEGER := (FRAME_WIDTH/2 + 75);
    SIGNAL GHOST_X2_MAX : INTEGER := (300);
    SIGNAL GHOST_X3_MAX : INTEGER := (1500);
    SIGNAL GHOST_Y_MAX : INTEGER := (FRAME_HEIGHT);

    SIGNAL BOX_X_MIN : INTEGER := 0;
    SIGNAL BOX_Y_MIN : INTEGER := 0;

    SIGNAL GHOST_X_MIN : INTEGER := (FRAME_WIDTH/2 - 75);
    SIGNAL GHOST_X2_MIN : INTEGER := (180);
    SIGNAL GHOST_X3_MIN : INTEGER := (1400);
    SIGNAL GHOST_Y_MIN : INTEGER := 0;

    CONSTANT BOX_X_INIT : STD_LOGIC_VECTOR(11 DOWNTO 0) := x"000";
    CONSTANT BOX_Y_INIT : STD_LOGIC_VECTOR(11 DOWNTO 0) := x"190"; --400

    SIGNAL active : STD_LOGIC;

    SIGNAL h_cntr_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
    SIGNAL v_cntr_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');

    SIGNAL ghost_h_cntr_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
    SIGNAL ghost_v_cntr_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');

    --circle equation variables to form a ball--
    SIGNAL h_cntr_reg_sqr : signed(23 DOWNTO 0) := (OTHERS => '0');
    SIGNAL v_cntr_reg_sqr : signed(23 DOWNTO 0) := (OTHERS => '0');
    SIGNAL expr : signed (24 DOWNTO 0) := (OTHERS => '0');
    SIGNAL radius : signed(3 DOWNTO 0) := "1111";

    --display--
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

    --ball directions-
    SIGNAL box_x_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
    SIGNAL box_x_dir : STD_LOGIC := '1';
    SIGNAL box_y_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
    SIGNAL box_y_dir : STD_LOGIC := '1';
    SIGNAL box_cntr_reg : STD_LOGIC_VECTOR(24 DOWNTO 0) := (OTHERS => '0');
    SIGNAL update_box : STD_LOGIC;
    SIGNAL pixel_in_box : STD_LOGIC;

    --ghost directions--
    --GHOST 1
    SIGNAL GHOST_X_INIT: STD_LOGIC_VECTOR(11 downto 0):= (others => '0');
    SIGNAL ghost_x_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := x"C30";
    SIGNAL ghost_x_dir : STD_LOGIC := '1';
    SIGNAL ghost_y_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := (others => '0');
    SIGNAL ghost_y_dir : STD_LOGIC := '1';
    SIGNAL ghost_cntr_reg : STD_LOGIC_VECTOR(24 DOWNTO 0) := (OTHERS => '0');
    SIGNAL update_ghost, update_ghost3, update_ghost2 : STD_LOGIC;
    SIGNAL ghost_in_box : STD_LOGIC;
    --GHOST 2
    SIGNAL ghost_x2_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := x"C12";
    SIGNAL ghost_x2_dir : STD_LOGIC := '1';
    SIGNAL ghost_y2_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := BOX_Y_INIT;
    SIGNAL ghost_y2_dir : STD_LOGIC := '1';
    SIGNAL ghost_cntr2_reg : STD_LOGIC_VECTOR(24 DOWNTO 0) := (OTHERS => '0');
    SIGNAL ghost_in_box2 : STD_LOGIC;
    --GHOST 3
    SIGNAL ghost_x3_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := x"5DC";
    SIGNAL ghost_x3_dir : STD_LOGIC := '1';
    SIGNAL ghost_y3_reg : STD_LOGIC_VECTOR(11 DOWNTO 0) := BOX_Y_INIT;
    SIGNAL ghost_y3_dir : STD_LOGIC := '1';
    SIGNAL ghost_cntr3_reg : STD_LOGIC_VECTOR(24 DOWNTO 0) := (OTHERS => '0');
    SIGNAL ghost_in_box3 : STD_LOGIC;

    --PADDLES AND BALL--
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
    SIGNAL counter : INTEGER := 0;

    --SEVEN SEGMENT DISPLAY--
    SIGNAL count_main2 : STD_LOGIC_VECTOR(19 DOWNTO 0) := (OTHERS => '0');
    SIGNAL scorep1, scorep2 : STD_LOGIC_VECTOR(3 DOWNTO 0);
    SIGNAL segp1, segp2 : STD_LOGIC_VECTOR(6 DOWNTO 0);
    SIGNAL game_end : STD_LOGIC := '0';
    
    --EXTRA SIGNALS
    SIGNAL trigger_clock : STD_LOGIC := '0';
    SIGNAL remove_ghost1, remove_ghost2, remove_ghost3 : STD_LOGIC := '0';
    SIGNAL winnerP1, winnerP2: STD_LOGIC := '0';
     

    --COUNTERS AND DELAYS
    SIGNAL counter_ghost, counter_ghost2, counter_ghost3 : INTEGER := 0;
    SIGNAL decr, incr, decr2, incr2, incr3, decr3: STD_LOGIC;
    SIGNAL ghost_random: INTEGER := 0;
    SIGNAL hold, hold2,hold3: INTEGER;

    --ROMS--
    TYPE rom_type IS ARRAY (0 TO 12) OF STD_LOGIC_VECTOR(12 DOWNTO 0);
   
    

    -- ROM definition
    CONSTANT ghost : rom_type :=
    (
    "0110000000110",
    "1010000000101",
    "1111000001111",
    "0011011101100",
    "0000111110000",
    "0001001001000",
    "0001011101000",
    "0001110111000",
    "0000111110000",
    "0010101010100",
    "1111000001111",
    "1010000000101",
    "0110000000110"
    );
    
    
    
    --show up three wandering ghosts
    SIGNAL winnerp2_point: STD_LOGIC;
    SIGNAL ghost_point : STD_LOGIC;
    SIGNAL vert, horz : INTEGER;
BEGIN

   
    disp : DisplayController
    PORT MAP(segOut => segp1, DispVal => scorep1);

    disp1 : DisplayController
    PORT MAP(segOut => segp2, DispVal => scorep2);

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

    --choose display for score--
    seg <= segp1 WHEN (count_main2(19) = '1') ELSE
        segp2;
    an <= count_main2(19);
    
    PROCESS (pxl_clk) BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF(ghost_random = 400) THEN
                ghost_random <= 0;
            ELSE
                ghost_random <= ghost_random + 50;
            END IF;
        END IF;
    END PROCESS;

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

    --seven segment display refresh-
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

    --BALL AND GHOST 1 INTERACTION
    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF (update_box = '1') THEN
                IF(remove_ghost1 = '0') THEN
                    HOLD <= ghost_random;
                    IF (box_x_dir = '1') THEN  
                        IF (box_x_reg <= GHOST_X_REG + 40 and box_x_reg >= GHOST_X_REG - 40 and box_y_reg <= GHOST_Y_REG + 40 and box_y_reg >= GHOST_Y_REG - 40) THEN                            
                            incr <= '1';
                            GHOST_X_MIN <= (FRAME_WIDTH/2 - HOLD);
                            GHOST_X_MAX <= (FRAME_WIDTH/2 + HOLD);
                            remove_ghost1 <= '1';
                            counter_ghost <= 0;                          
                        END IF;
                    ELSIF (box_x_dir = '0') THEN
                        IF (box_x_reg <= GHOST_X_REG + 40 and box_x_reg >= GHOST_X_REG - 40 and box_y_reg <= GHOST_Y_REG + 40 and box_y_reg >= GHOST_Y_REG + 40) THEN                            
                            decr <= '1';
                            GHOST_X_MIN <= (FRAME_WIDTH/2 - HOLD);
                            GHOST_X_MAX <= (FRAME_WIDTH/2 + HOLD);
                            remove_ghost1 <= '1';
                            counter_ghost <= 0;                        
                        END IF;
                    END IF;
                ELSE             
                    incr<= '0';
                    decr<= '0';
                    IF(counter_ghost = 3400) THEN
                        remove_ghost1 <= '0';
                    ELSE
                        counter_ghost <= counter_ghost + 1;
                    END IF;
                END IF;
            END IF;
        END IF;
    END PROCESS;
  
      --BALL AND GHOST 2 INTERACTION

    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF (update_box = '1') THEN            
                IF(remove_ghost2 = '0') THEN
                    hold2<=ghost_random;
                    IF (box_x_dir = '1') THEN
                        IF (box_x_reg <= GHOST_X2_REG + 40 and box_x_reg >= GHOST_X2_REG - 40 and box_y_reg <= GHOST_Y2_REG + 40 and box_y_reg >= GHOST_Y2_REG - 40) THEN                            
                            incr2 <= '1';
                            GHOST_X2_MIN <= (350 - hold2);
                            GHOST_X2_MAX <= (350 + hold2);
                            remove_ghost2 <= '1';
                            counter_ghost2 <= 0;                          
                        END IF;
                    ELSIF (box_x_dir = '0') THEN
                        IF (box_x_reg <= GHOST_X2_REG + 40 and box_x_reg >= GHOST_X2_REG - 40 and box_y_reg <= GHOST_Y2_REG + 40 and box_y_reg >= GHOST_Y2_REG - 40) THEN                            
                            decr2 <= '1';
                            GHOST_X2_MIN <= (350 - hold2);
                            GHOST_X2_MAX <= (350 + hold2);
                            remove_ghost2 <= '1';
                            counter_ghost2 <= 0;                        
                        END IF;
                    END IF;
                ELSE           
                    incr2 <= '0';
                    decr2 <= '0';  
                    IF(counter_ghost2 = 3400) THEN
                        remove_ghost2 <= '0';
                    ELSE
                        counter_ghost2 <= counter_ghost2 + 1;
                    END IF;
                END IF;
            END IF;
        END IF;
    END PROCESS;
  
     --BALL AND GHOST 3 INTERACTION
  
    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF (update_box = '1') THEN            
                IF(remove_ghost3 = '0') THEN
                    hold3<=ghost_random;
                    IF (box_x_dir = '1') THEN
                        IF (box_x_reg <= GHOST_X3_REG + 40 and box_x_reg >= GHOST_X3_REG - 40 and box_y_reg <= GHOST_Y3_REG + 40 and box_y_reg >= GHOST_Y3_REG - 40) THEN                            
                            incr3 <= '1';
                            GHOST_X3_MIN <= (1400 - hold3);
                            GHOST_X3_MAX <= (1400 + hold3);
                            remove_ghost3 <= '1';
                            counter_ghost3 <= 0;                          
                        END IF;
                    ELSIF (box_x_dir = '0') THEN
                        IF (box_x_reg <= GHOST_X3_REG + 40 and box_x_reg >= GHOST_X3_REG - 40 and box_y_reg <= GHOST_Y3_REG + 40 and box_y_reg >= GHOST_Y3_REG - 40) THEN                            
                            decr3 <= '1';
                            GHOST_X3_MIN <= (1400 - hold3);
                            GHOST_X3_MAX <= (1400 + hold3);
                            remove_ghost3 <= '1';
                            counter_ghost3 <= 0;                        
                        END IF;
                    END IF;
                ELSE           
                    incr3 <= '0';
                    decr3 <= '0';  
                    IF(counter_ghost3 = 3400) THEN
                        remove_ghost3 <= '0';
                    ELSE
                        counter_ghost3 <= counter_ghost3 + 1;
                    END IF;
                END IF;
            END IF;
        END IF;
    END PROCESS;   
  
  --BALL AND GHOST SCORE ACCUMULATOR/DECREMENTOR FOR PLAYER 1
      
    PROCESS (pxl_clk) 
    BEGIN
        IF(rising_edge(pxl_clk)) THEN
            IF(update_box = '1') THEN
                IF(incr = '1' or incr2 = '1' or incr3 = '1') THEN
                    scorep1 <= scorep1 + 1;               
                ELSIF(decr = '1' or decr2 = '1' or decr3 = '1') THEN
                    scorep1 <= scorep1 - 1;
                END IF;              
            END IF;
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
                        box_clk_div <= box_clk_div - 30;
                        status <= '1';
                        --                        racketp1 <= 0;
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
    --moving the computer racket
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

    --moving the player racket
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

    --main ghost logic--
    --ghost 1
    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN

            IF (update_ghost = '1') THEN

                IF (pause = '0') THEN
                    IF (update_ghost = '1') THEN

                        IF (ghost_x_dir = '1') THEN
                            ghost_x_reg <= ghost_x_reg + 1;

                        ELSE

                            ghost_x_reg <= ghost_x_reg - 1;

                        END IF;
                    END IF;

                    IF (ghost_y_dir = '1') THEN
                        ghost_y_reg <= ghost_y_reg + 1;
                    ELSE
                        ghost_y_reg <= ghost_y_reg - 1;
                    END IF;

                END IF;-- update               
            END IF; --pause =       
        END IF; -- rising edge of the clock
    END PROCESS;

    --ghost2 
    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN

            IF (update_ghost2 = '1') THEN

                IF (pause = '0') THEN
                    IF (update_ghost2 = '1') THEN

                        IF (ghost_x2_dir = '1') THEN
                            ghost_x2_reg <= ghost_x2_reg + 1;

                        ELSE

                            ghost_x2_reg <= ghost_x2_reg - 1;

                        END IF;
                    END IF;

                    IF (ghost_y2_dir = '1') THEN
                        ghost_y2_reg <= ghost_y2_reg + 1;
                    ELSE
                        ghost_y2_reg <= ghost_y2_reg - 1;
                    END IF;

                END IF;-- update               
            END IF; --pause = 0

        END IF; -- rising edge of the clock
    END PROCESS;

    --GHOST 3
    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN

            IF (update_ghost3 = '1') THEN

                IF (pause = '0') THEN
                    IF (update_ghost3 = '1') THEN

                        IF (ghost_x3_dir = '1') THEN
                            ghost_x3_reg <= ghost_x3_reg + 1;

                        ELSE

                            ghost_x3_reg <= ghost_x3_reg - 1;

                        END IF;
                    END IF;

                    IF (ghost_y3_dir = '1') THEN
                        ghost_y3_reg <= ghost_y3_reg + 1;
                    ELSE
                        ghost_y3_reg <= ghost_y3_reg - 1;
                    END IF;

                END IF;-- update               
            END IF; --pause = 0

        END IF; -- rising edge of the clock
    END PROCESS;


    --ghost direction logic--    
    --ghost 1
    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF (pause = '0') THEN
                IF (update_ghost = '1') THEN
                    IF ((ghost_x_dir = '1' AND (ghost_x_reg = ghost_X_MAX - 1)) OR (ghost_x_dir = '0' AND (ghost_x_reg = ghost_X_MIN + 1))) THEN
                        ghost_x_dir <= NOT(ghost_x_dir);
                    END IF;
                    IF ((ghost_y_dir = '1' AND (ghost_y_reg = ghost_Y_MAX - 1)) OR (ghost_y_dir = '0' AND (ghost_y_reg = ghost_Y_MIN + 1))) THEN
                        ghost_y_dir <= NOT(ghost_y_dir);
                    END IF;
                END IF;
            END IF;
        END IF;
    END PROCESS;
    --ghost 2
    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF (pause = '0') THEN
                IF (update_ghost2 = '1') THEN
                    IF ((ghost_x2_dir = '1' AND (ghost_x2_reg = (ghost_X2_MAX) - 1)) OR (ghost_x2_dir = '0' AND (ghost_x2_reg = (ghost_X2_MIN) + 1))) THEN
                        ghost_x2_dir <= NOT(ghost_x2_dir);
                    END IF;
                    IF ((ghost_y2_dir = '1' AND (ghost_y2_reg = ghost_Y_MAX - 1)) OR (ghost_y2_dir = '0' AND (ghost_y2_reg = ghost_Y_MIN + 1))) THEN
                        ghost_y2_dir <= NOT(ghost_y2_dir);
                    END IF;
                END IF;

            END IF;
        END IF;
    END PROCESS;
    --ghost 3
    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF (pause = '0') THEN
                IF (update_ghost3 = '1') THEN
                    IF ((ghost_x3_dir = '1' AND (ghost_x3_reg = (ghost_X3_MAX) - 1)) OR (ghost_x3_dir = '0' AND (ghost_x3_reg = (ghost_X3_MIN) + 1))) THEN
                        ghost_x3_dir <= NOT(ghost_x3_dir);
                    END IF;
                    IF ((ghost_y3_dir = '1' AND (ghost_y3_reg = ghost_Y_MAX - 1)) OR (ghost_y3_dir = '0' AND (ghost_y3_reg = ghost_Y_MIN + 1))) THEN
                        ghost_y3_dir <= NOT(ghost_y3_dir);
                    END IF;
                END IF;

            END IF;
        END IF; 
     END PROCESS; 

    --ghost clock divider--
    --ghost 1
    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF (ghost_cntr_reg = (ghost_CLK_DIV - 1)) THEN
                ghost_cntr_reg <= (OTHERS => '0');
            ELSE
                ghost_cntr_reg <= ghost_cntr_reg + 1;
            END IF;
        END IF;
    END PROCESS;

    --ghost2
    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF (ghost_cntr2_reg = (ghost2_CLK_DIV - 1)) THEN
                ghost_cntr2_reg <= (OTHERS => '0');
            ELSE
                ghost_cntr2_reg <= ghost_cntr2_reg + 1;
            END IF;
        END IF;
    END PROCESS;
    
    --ghost 3
    PROCESS (pxl_clk)
    BEGIN
        IF (rising_edge(pxl_clk)) THEN
            IF (ghost_cntr3_reg = (ghost3_CLK_DIV - 1)) THEN
                ghost_cntr3_reg <= (OTHERS => '0');
            ELSE
                ghost_cntr3_reg <= ghost_cntr3_reg + 1;
            END IF;
        END IF;
    END PROCESS;


    update_ghost <= '1' WHEN ghost_cntr_reg = (ghost_CLK_DIV - 1) ELSE
        '0';

    update_ghost2 <= '1' WHEN ghost_cntr2_reg = (ghost2_CLK_DIV - 1) ELSE
        '0';
        
    update_ghost3 <= '1' WHEN ghost_cntr3_reg = (ghost3_CLK_DIV - 1) ELSE
        '0';    

    player2 <= '1' WHEN ((active = '1' AND (((h_cntr_reg > horz1_p2 AND h_cntr_reg < horz2_p2)) AND ((v_cntr_reg > vert1_p2 AND v_cntr_reg < vert2_p2))))) ELSE
        '0';

    player1 <= '1' WHEN ((active = '1' AND (((h_cntr_reg > horz1 AND h_cntr_reg < horz2)) AND ((v_cntr_reg > vert1 AND v_cntr_reg < vert2))))) ELSE
        '0';

    --generate pixels
    trigger <= '1' WHEN (active = '1' AND winnerp2 = '0' AND (expr <= 1024)) OR (active = '1' AND winnerp2 = '0' AND player1 = '1') OR (active = '1' AND winnerp2 = '0' AND player2 = '1')
        ELSE
        ghost_point WHEN ((ghost_point = ghost(conv_integer(v_cntr_reg(6 DOWNTO 2) - ghost_y_reg(11 DOWNTO 2)))(conv_integer(h_cntr_reg(6 DOWNTO 2) - ghost_x_reg(11 DOWNTO 2)))) AND winnerp2 = '0')
        OR
        (ghost_point = ghost(conv_integer(v_cntr_reg(6 DOWNTO 2) - ghost_y2_reg(11 DOWNTO 2)))(conv_integer(h_cntr_reg(6 DOWNTO 2) - ghost_x2_reg(11 DOWNTO 2))) AND winnerp2 = '0') 
        OR
        (ghost_point = ghost(conv_integer(v_cntr_reg(6 DOWNTO 2) - ghost_y3_reg(11 DOWNTO 2)))(conv_integer(h_cntr_reg(6 DOWNTO 2) - ghost_x3_reg(11 DOWNTO 2))) AND winnerp2 = '0')
        ELSE
        '0';

    --generate ghost pixels
    ghost_point <= ghost(conv_integer(v_cntr_reg(6 DOWNTO 2) - ghost_y_reg(11 DOWNTO 2)))(conv_integer(h_cntr_reg(6 DOWNTO 2) - ghost_x_reg(11 DOWNTO 2)))
        WHEN ((h_cntr_reg >= ghost_x_reg) AND (h_cntr_reg < ghost_x_reg + 55)) AND
        ((v_cntr_reg >= ghost_y_reg) AND (v_cntr_reg < ghost_y_reg + 55)) AND remove_ghost1 = '0'
        ELSE
        ghost(conv_integer(v_cntr_reg(6 DOWNTO 2) - ghost_y2_reg(11 DOWNTO 2)))(conv_integer(h_cntr_reg(6 DOWNTO 2) - ghost_x2_reg(11 DOWNTO 2)))
        WHEN 
        ((h_cntr_reg >= ghost_x2_reg) AND (h_cntr_reg < ghost_x2_reg + 55)) AND 
        ((v_cntr_reg >= ghost_y2_reg) AND (v_cntr_reg < ghost_y2_reg + 55)) AND remove_ghost2 = '0'
        ELSE
        ghost(conv_integer(v_cntr_reg(6 DOWNTO 2) - ghost_y3_reg(11 DOWNTO 2)))(conv_integer(h_cntr_reg(6 DOWNTO 2) - ghost_x3_reg(11 DOWNTO 2)))
        WHEN 
        ((h_cntr_reg >= ghost_x3_reg) AND (h_cntr_reg < ghost_x3_reg + 55)) AND 
        ((v_cntr_reg >= ghost_y3_reg) AND (v_cntr_reg < ghost_y3_reg + 55)) AND remove_ghost3 = '0'      
        ELSE
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