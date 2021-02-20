----------------------------------------------------------------------------------
-- Company: 
-- Engineer: David ruiz barajas
-- 
-- Create Date: 08.07.2020 00:01:12
-- Design Name: 
-- Module Name: uart_basico_TB - Behavioral
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
use work.states_debug.all;

entity uart_basico_TB is

end uart_basico_TB;

architecture Behavioral of uart_basico_TB is
-----------------------------------------
-- COMPONENT usb-uart basys3 emulation
-----------------------------
component uart GENERIC(
		clk_freq		:	INTEGER		:= 100_000_000;	--frequency of system clock in Hertz
		baud_rate	:	INTEGER		:= 19_200;		--data link baud rate in bits/second
		os_rate		:	INTEGER		:= 16;			--oversampling rate to find center of receive bits (in samples per baud period)
		d_width		:	INTEGER		:= 8; 			--data bus width
		parity		:	INTEGER		:= 1;				--0 for no parity, 1 for parity
		parity_eo	:	STD_LOGIC	:= '0');			--'0' for even, '1' for odd parity
	PORT(
		clk		:	IN		STD_LOGIC;										--system clock
		reset	:	IN		STD_LOGIC;										--ascynchronous reset
		tx_enable	:	IN		STD_LOGIC;										--initiate transmission
		tx_data	:	IN		STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  --data to transmit
		rx			:	IN		STD_LOGIC;										--receive pin
		rx_busy	:	OUT	STD_LOGIC;
		rx_done: out std_logic;												--data reception in progress
		rx_error	:	OUT	STD_LOGIC;										--start, parity, or stop bit error detected
		rx_data	:	OUT	STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);	--data received
		tx_busy	:	OUT	STD_LOGIC;  									--transmission in progress
		tx			:	OUT	STD_LOGIC;
		rx_buffer_debug:  out STD_LOGIC_VECTOR(d_width DOWNTO 0);
        tx_buffer_debug:  out STD_LOGIC_VECTOR(d_width+2 DOWNTO 0);
		baud_clk: out std_logic);										--transmit pin
        end component;
---pc----        
constant d_width: integer:=8;
signal tx_enable_pc: std_logic:='0';
signal tx_data_pc: std_logic_vector(d_width-1 downto 0):=(others=>'0');
signal rx_pc: std_logic;
signal rx_busy_pc: std_logic;
signal rx_done_pc: std_logic;
signal rx_error_pc: std_logic;
signal rx_data_pc:std_logic_vector(d_width-1 downto 0);
signal tx_busy_pc: std_logic;
signal tx_pc: std_logic;
signal baud_clk_pc: std_logic;
----2---
signal tx_enable_2: std_logic:='0';
signal tx_data_2: std_logic_vector(d_width-1 downto 0):=(others=>'0');
signal rx_2: std_logic;
signal rx_busy_2: std_logic;
signal rx_done_2: std_logic;
signal rx_error_2: std_logic;
signal rx_data_2:std_logic_vector(d_width-1 downto 0);
signal tx_busy_2: std_logic;
signal tx_2: std_logic;
signal baud_clk_2: std_logic;
----
signal clk, reset:std_logic:='0';

--others
signal rx_buffer_pc:  STD_LOGIC_VECTOR(d_width DOWNTO 0);
signal	tx_buffer_pc:  STD_LOGIC_VECTOR(d_width+2 DOWNTO 0);
signal rx_buffer_2:  STD_LOGIC_VECTOR(d_width DOWNTO 0);
signal	tx_buffer_2:  STD_LOGIC_VECTOR(d_width+2 DOWNTO 0);

begin

basys: uart generic map(100000000,19200,32,8,1,'0') port map(clk,
reset,
tx_enable_pc,
tx_data_pc,
rx_pc,
rx_busy_pc,
rx_done_pc,
rx_error_pc,
rx_data_pc,
tx_busy_pc,
tx_pc,
rx_buffer_pc,
tx_buffer_pc,
baud_clk_pc);
basys2: uart generic map(100000000,19200,32,8,1,'0') port map(clk,
reset,
tx_enable_2,
tx_data_2,
rx_2,
rx_busy_2,
rx_done_2,
rx_error_2,
rx_data_2,
tx_busy_2,
tx_2,
rx_buffer_2,
tx_buffer_2,
baud_clk_2);
--conexion basys3 con la celda
rx_2<=tx_pc;
rx_pc<=tx_2;

clk<=not clk after 5 ns;
reset<= '1','0' after 20 ns;

process
begin
        tx_Data_pc<=x"01";
        WAIT FOR 20 ns;
        tx_enable_pc<='1';
        wait for 20 ns;
        tx_enable_pc<='0';
        wait for 2000 us;
        
        
        tx_Data_pc<=x"10";
        WAIT FOR 20 ns;
        tx_enable_pc<='1';
        wait for 20 ns;
        tx_enable_pc<='0';
        wait for 2000 us;
        
        
        tx_Data_pc<=x"00";
        WAIT FOR 20 ns;
        tx_enable_pc<='1';
        wait for 20 ns;
        tx_enable_pc<='0';
        wait for 2000 us;
        
        
        tx_Data_pc<=x"FF";
        WAIT FOR 20 ns;
        tx_enable_pc<='1';
        wait for 20 ns;
        tx_enable_pc<='0';
        wait for 650 us;
        
end process;
end Behavioral;

