----------------------------------------------------------------------------------
-- Company: 
-- Engineer: David ruiz barajas
-- 
-- Create Date: 08.07.2020 00:01:12
-- Design Name: 
-- Module Name: detectores_TB - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library work;
use work.states_debug.all;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity detectores_TB is
--  Port ( );
end detectores_TB;

architecture Behavioral of detectores_TB is

component input_detector is
        port( clk : in STD_LOGIC;
           reset : in STD_LOGIC;
           rst_d : in STD_LOGIC;
           input_spk : in STD_LOGIC;
           spk_detected : out STD_LOGIC);
        end component;
signal reset,spk_detected: std_logic;
signal clk,input_spk,rst_d: std_logic:='0';
constant a:time:=40ns;
constant b:time:=20ns;
constant c:time:=25ns;

begin
debug_cpn: input_detector port map(clk,reset,rst_d,input_spk,spk_detected);
clk<=not clk after 10ns;
reset<= '1','0' after 25 ns;
process
begin
    wait for a;
    input_spk<='1';
    wait for b;
    input_spk<='0';
    wait for c;
    
        input_spk<='1';
    wait for b;
    input_spk<='0';
    wait for c;
        input_spk<='1';
    wait for b;
    input_spk<='0';
    rst_d<='1';
    wait for 5ns;
    rst_d<='0';
    wait for c;
        input_spk<='1';
    wait for b;
    input_spk<='0';
    wait for 200ns; --aqui
        input_spk<='1';
            rst_d<='1';
    wait for b;
    input_spk<='0';
        rst_d<='1';
    wait for c;
    input_spk<='1';
    wait for b;
    input_spk<='0';
    wait for c;
        input_spk<='1';
        input_spk<='1';
    wait for b;
    input_spk<='0';
    wait for c;
        input_spk<='1';
    
    wait;
end process;

end Behavioral;
