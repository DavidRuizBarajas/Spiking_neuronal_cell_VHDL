----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 19.06.2020 15:16:03
-- Design Name: 
-- Module Name: imput_detector - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.02 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity input_detector is
    Port ( clk : in STD_LOGIC;
           rst_d : in STD_LOGIC;
           input_spk : in STD_LOGIC;
           spk_detected : out STD_LOGIC);
end input_detector;

 architecture Behavioral of input_detector is
signal input_spk_old: std_logic;
begin
process(rst_d,clk)
begin
    if rst_d = '1' then
        input_spk_old <= '0' ;
    elsif rising_edge(clk) then
        if  (input_spk='1' and input_spk_old='0')then 
            input_spk_old <= '1';
        end if;
    end if;
end process;

spk_detected <= '1' when (input_spk='1' and rst_d='0') or input_spk_old='1' else  '0'; 
end Behavioral;
