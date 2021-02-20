----------------------------------------------------------------------------------
-- Company: 
-- Engineer: David Ruiz Barajas
-- 
-- Create Date: 02.09.2020 14:23:49
-- Design Name: Red neuronal spiking
-- Module Name: spiking_Web - Behavioral
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


use IEEE.NUMERIC_STD.ALL;


entity spiking_Web is

    Generic( web_inputs:INTEGER range 1 to 65536:=10;
             L: integer range 1 to 256:=10; --leak numbers 2^7 para todas las neuronas
             C: integer range 2 to 52:=3;  -- columnas totales (entrda+ocultas+salida)
             clk_freq:	INTEGER		:= 100_000_000;	--frequency of system clock in Hertz
		         baud_rate:	INTEGER range 19200 to 12000000:= 19_200		--data link baud rate in bits/second max: 12Mbouds/s 12_000_000 para basys3
             ); 

    Port (
          clk: in std_logic;
          reset: in std_logic; --elimina pesos, threshold, leak 

          input: in std_logic_vector(web_inputs-1 downto 0);
          output: out std_logic_vector(web_inputs-1 downto 0);
          --control de recepcion

          rx_com: in std_logic;
          tx_com: out std_logic;
          new_instruction_b:in std_logic;
          
          --control de acciones
          setting: in std_logic; --modo set parametros
          soft_rst_b: in std_logic;
          stop: in std_logic --paramos la red neuronal, standby

    );
end spiking_Web;

architecture Behavioral of spiking_Web is


--componente neuronal--
component neuronal_cell 
    
    Generic (
             N:integer range 1 to 65536:= 10; --numero de entradas genericas a la celda 2^6
             ID: integer :=0; --id de la celda max value: 2^16
             L: integer range 1 to 256:=10; --leak numbers 2^8
             clk_freq:	INTEGER		:= 100_000_000;	--frequency of system clock in Hertz
		         baud_rate:	INTEGER		:= 19_200		--data link baud rate in bits/second max: 12Mbouds/s 12_000_000
             ); 

    Port (
          input: in std_logic_vector(N-1 downto 0); 
          clk: in std_logic;
          reset: in std_logic; --elimina pesos, threshold, leak 
          spike: out std_logic; --pulso de salida
          --control de recepcion
          new_instruction: inout std_logic;
          rx_com: in std_logic;
          tx_com: out std_logic;
         
          --control de acciones
          setting: in std_logic; --modo set parametros
          soft_rst_b: in std_logic;
          stop: in std_logic --paramos la red neuronal, standby

    );
end component;
--parametros del componente neuronal
constant N: integer := web_inputs; --numero de entradas de cada una de las neuronas de una columna


--señales sinapticas
type web is array (C-1 downto 0) of std_logic_vector(web_inputs-1 downto 0);
signal synapsis: web;


--otras señales
signal new_instruction_com: std_logic;



begin

--boton especial en la configuracion.

new_instruction_com <= '1' when new_instruction_b='1' else 'Z';

--columna entrada con una sola entrada por neurona, esta columna no debe cambiarse.

Column0: for i in input'range generate

    begin
    input_neuron: neuronal_cell generic map (N=>1,ID=>i,L=>L,clk_freq=>clk_freq,baud_rate=>baud_rate)
                             port map(
                              input(0)=>input(i),
                              clk=>clk,
                              reset=>reset,
                              spike=>synapsis(0)(i),                          

                              new_instruction=>new_instruction_com,
                              rx_com=>rx_com,
                              tx_com=>tx_com,
                              
                              setting=>setting,
                              soft_rst_b=>soft_rst_b,
                              stop=>stop
                              );
end generate;

--capas intermedias + capa final :  1-> C-1

ColumnX: for x in  1 to (C-1) generate --genera las columnas (x)de celdas neuronales. TOTAL= x= C-1 CAPAS INTERMEDIAS + FINAL
    --columna x
    NeuronY: for i in 0 to N-1 generate  --genera las celdas (i) de X columna. TOTAL = 0->I'range.
    
        --neurona y de la columna x 

        
        input_neuronY: neuronal_cell generic map (N,( input'length+1 + ((x-1)*(input'length)) + i+1),L,clk_freq,baud_rate) 
                                
                                --para asignar el id contamos con las neuronas de la fila anterior (al tener una entrada podemos usar el valor input'length)
                                
                                --debemos contar luego con las columnas anteriores, empezamos en la 1 (la 0 es la entrada) por lo que le restamos 1. luego multiplicamos por el numero de neuronas de entrada tambien
                                --todas las columans tienen el mismo numero de neuronas
                                
                                --finalmente sumamos las neuronas instanciadas de la propia columna +1 para el siguiente valor al empezar en 0.
                                 port map(
                                 
                                  input=>synapsis(x-1),
                                  clk=>clk,
                                  reset=>reset,
                                  spike=>synapsis(x)(i),                           
    
                                  new_instruction=>new_instruction_com,
                                  rx_com=>rx_com,
                                  tx_com=>tx_com,
                                  
                                  setting=>setting,
                                  soft_rst_b=>soft_rst_b,
                                  stop=>stop
                                  );
    end generate;
end generate;


output<=synapsis(C-1); --salida de la red neuronal

end Behavioral;
