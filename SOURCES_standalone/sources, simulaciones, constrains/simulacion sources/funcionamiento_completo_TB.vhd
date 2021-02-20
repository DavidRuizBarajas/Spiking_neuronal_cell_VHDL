----------------------------------------------------------------------------------
-- Company: 
-- Engineer: DAVID RUIZ BARAJAS
-- 
-- Create Date: 06.07.2020 12:36:31
-- Design Name: 
-- Module Name: celda neuronal completa- testbench
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
use work.states_debug.all;



entity funcionamiento_completo_TB is
--  Port ( );
end funcionamiento_completo_TB;

architecture Behavioral of funcionamiento_completo_TB is

---------------------------------------------
--que prueba activamos?
---------------------------------------------
signal prueba_n: integer:= 3;
    --1 enviamos isntruccion sin seleccionar 
    --2 enviamos el id mal -> no hay ningun match, no existe esa neurona
    --3 enviamos el id bien 
signal prueba_sub_n: integer:= 0;
    --0 WRITE PESO 
    --1 READ PESO 
    --2 WRITE PESO INDEX ERROR 
    --3 READ PESO INDEX ERROR 
    --4 WRITE LEAK 
    --5 WRITE LEAK INDEX ERROR 
    --6 read leak 
    --7 read leak index error 
    --8 write activarion leak        y leida despues
    --9 read activation leak
    --10 write vth 
    --11 read vth 
    --12 read voltaje actual 
    --13 read leak pointer position 
    --14 debug error state 
    -- obligatoria end instruction 
    

-----------------------------
--COMPONENTE neuronal_cell
--------------------------
component neuronal_cell is
    
    Generic (
             N:integer range 1 to 64:= 2; --numero de entradas genericas a la celda neuronal 2^6
             ID: integer :=0; --id de la celda neuronal  max value: 2^16
             L: integer range 1 to 256:=10; --leak numbers 2^7
             clk_freq:	INTEGER		:= 100_000_000;	--frequency of system clock in Hertz
		     baud_rate:	INTEGER		:= 19_200		--data link baud rate in bits/second max: 12Mbouds/s 12_000_000
             ); 

    Port (
          input: in std_logic_vector(N-1 downto 0); 
          clk: in std_logic;
          reset: in std_logic; --elimina pesos, threshold, leak 
          spike: out std_logic; --pulso de salida
          
          --bus de configuracion/comunicacion

          new_instruction: inout std_logic;
          rx_com: in std_logic;
          tx_com: out std_logic;
          
          --control de acciones
          setting: in std_logic; --modo set parametros
          soft_rst_b: in std_logic;
          stop: in std_logic --paramos la red neuronal, standby

    );
           end component;
--------------------------------------------------
--BARIABLES SIMULACION Y CONSTANTES SIMULACION
-------------------------------------------------


--CONSTANTES Y VARIABLES DE TIEMPO
constant N:integer:=2;
constant a:time:=40ns;
constant b:time:=10ns;
constant c:time:=25ns;
constant clk_freq: integer:= 100000000;
constant baud_rate: integer:= 115200;
signal simulacion_de_pulsos_completa: integer:=0;
constant word_wait: time:=650 us; --=(11/19200 us)=572.9126

--seNales del componente (no debug)
signal tx_com: std_logic;--   :='Z';
signal rx_com: std_logic:='1';
signal clk,reset,spike,setting,stop,soft_rst_b: std_logic:='0';
signal input: std_logic_vector (N-1 downto 0):=(others=>'0');
signal new_instruction: std_logic:='Z';


-------------------------
--INSTRUCCIONES
--------------------------
constant null_instruction:std_logic_vector:= x"00";
constant ready_instruction:std_logic_vector:= x"01";
constant done_instruction :std_logic_vector:= x"02";
constant end_instructions:std_logic_vector := x"03";
constant id_instruction :std_logic_vector:= x"04";
constant w_peso_instruction :std_logic_vector:= x"05";
constant w_leak_instruction :std_logic_vector:= x"06";
constant w_leak_act_instruction :std_logic_vector:= x"07";
constant w_vth_instruction :std_logic_vector:= x"08";
constant error_rango_instruction :std_logic_vector:= x"09";
constant r_peso_instruction :std_logic_vector:= x"0A";
constant r_leak_instruction:std_logic_vector := x"0B";
constant r_vth_instruction :std_logic_vector:= x"0C";
constant r_voltage_instruction :std_logic_vector:= x"0D";
constant r_leakP_instruction:std_logic_vector := x"0E";
constant r_leak_act_instruction :std_logic_vector:= x"0F";




--------------------------------------------------
-- COMPONENT usb-uart basys3 emulation
-------------------------------------------------
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
		tx			:	OUT	STD_LOGIC);										--transmit pin
        end component;
--constantes urart y se?ales uart       
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

--simulacion de numeros pc--
signal dato_w: integer range -32768  to 32767 :=0;
signal vector_w: std_logic_vector(15 downto 0);
signal dato_w1: std_logic_vector(7 downto 0);
signal dato_w2: std_logic_vector(7 downto 0);



----------------------
--START SIM
----------------------
begin


--conexiones concurrentes

vector_w<=std_logic_vector(to_signed(dato_w,16));
dato_w1<=vector_w(7 downto 0);
dato_w2<=vector_w(15 downto 8);


debug1: neuronal_cell port map(input,clk,reset,spike,new_instruction,rx_com,tx_com,setting,soft_rst_b,stop);
--                      100000000,19200,16,8,1,'0')
basys: uart generic map(100000000,19200,8,8,1,'0') port map(clk,reset,tx_enable_pc,tx_data_pc,rx_pc,rx_busy_pc,rx_done_pc,rx_error_pc,rx_data_pc,tx_busy_pc,tx_pc);

--conexion basys3 con la celda

rx_com<=tx_pc;
rx_pc<=tx_com;

--SE?ALES CLOCK Y RESET

clk<=not clk after 5 ns;
reset<= '1','0' after 25 ns;





process
begin
    stop<='1';
    wait for 25ns;
    stop<='0';
    wait until simulacion_de_pulsos_completa=1; --esperamos a que probemos el procesado de pulsos, 
                                                --luego entramos en el modo configuracion 
    stop<='1';
    wait;
    
    
end process;
----------PRUEBA procesado de pulsos INICIAL-----------
process
begin
    --funcionamineto basico
    wait for a;
    input(0)<='1';
    wait for b;
    input(0)<='0';
    wait for c;
    
    input(0)<='1';
    wait for b;
    input(0)<='0';
    wait for c;
    input(0)<='1';
    wait for b;
    input(0)<='0';
    wait for c;
    input(0)<='1';
    wait for b;
    input(0)<='0';
    wait for c;   
    input(0)<='1';
    wait for b;
    input(0)<='0';
    input(1)<='1';
    wait for c ;
    input(1)<='0';
    wait for b;
    input(1)<='1';
    wait for c;
    input(1)<='0';
    wait for b;
    input(0)<='1';
    wait for b;
    input(0)<='0';
    wait for c;
    input(0)<='1';
    wait for b;
    input(0)<='0';
    
    
    wait for 100ns;
    input(0)<='1';
    wait for b;
    input(0)<='0';
     wait for c;
    input(0)<='1';
    wait for b;
    input(0)<='0';
     wait for c;
    input(0)<='1';
    wait for b;
    input(0)<='0';
     wait for c;
    input(0)<='1';
    wait for b;
    input(0)<='0';
     wait for c;
    input(0)<='1';
    wait for b;
    input(0)<='0';
    
    
    --for leak
    wait for 500 ns;
    
    
    simulacion_de_pulsos_completa<=1; --para parar la primera parte de la simulacion.
    
    --**Podemos empezar la config con soft reset desabilitando -- de las siguientes linas
    --parametro que borra el valor de trabajo de la celda, como el potencial o los detectores.**
    
    --soft_rst_b<='1';
    wait for 10 ns;
    --soft_rst_b<='0';
    
    
---------------segunda prueba: comunicacion y configuracion ------------
    
    --entrar en modo configuracio, despues esperamos hasta que nos lo indique que esta lista la celda ID 00
    wait for 30 ns;
    setting<='1';
    wait for 10 ns;
    setting<='0';
    
    wait until rx_done_pc='1';
    

case prueba_n is
when 1 =>
    if(rx_data_pc=ready_instruction) then -- COMPRUEBA que esta lista para configurarse 
                                          -- prueba 1: instruccion aleatoria sin marcar id (solucion: pulsar new_instr_b)
    
        tx_Data_pc<=w_peso_instruction;--ENVIAMOS UNA ISNTRUCCION aleatoria SIN MARCAR EL ID: ejemplo de se ha selccionado antes o no se ha seleccionado
        tx_enable_pc<='1';
        wait for 15 ns;
        tx_enable_pc<='0';
        
        
        wait for word_wait ;  --esperamos a que la celda vea que no esta seleccionada (reciba el dato
        
        
        new_instruction<='1'; --cuando la celda sleccionada termina la instruccion como debug se pulsa el boton
        wait for 10 ns;
        new_instruction<='Z'; --cuando la celda sleccionada termina la instruccion como debug se pulsa el boton
        
    end if;
    
    wait for 2000 ns ;
when 2=>   
    if(rx_data_pc=ready_instruction) then --prueba 2: no existe ese ID (solucion: pulsar new_instr_b) / el id no hace match espera a que la celd que hizo match de la orden de nueva instruccion.
    
        tx_Data_pc<=id_instruction; --seleccion id
        wait for 10ns;
        tx_enable_pc<='1';
        wait for 15 ns;
        tx_enable_pc<='0';
        
        
        WAIT FOR word_wait ;
        
        --id 2/2
        tx_Data_pc<=x"00";
        WAIT FOR 20 ns;
        tx_enable_pc<='1';
        wait for 20 ns;
        tx_enable_pc<='0';
        
        
        WAIT FOR word_wait ;
        --id 1/2
        tx_Data_pc<=x"11";
        WAIT FOR 20 ns;
        tx_enable_pc<='1';
        wait for 20 ns;
        tx_enable_pc<='0';
        
        WAIT FOR word_wait;
        
        new_instruction<='1'; --cuando la celda sleccionada termina la instruccion como debug se pulsa el boton
        wait for 30 ns;
        new_instruction<='Z'; --cuando la celda sleccionada termina la instruccion como debug se pulsa el boton
        WAIT FOR 30ns;
    end if;
    
 -- esta siempre se ejecuta para que las demas se puedan hacer correctamente
when 3 => 
    if(rx_data_pc=ready_instruction) then -- selccionamos bien el id
    
        tx_Data_pc<=id_instruction; --seleccion id
        wait for 10ns;
        tx_enable_pc<='1';
        wait for 15 ns;
        tx_enable_pc<='0';
        
        
        WAIT FOR word_wait ;
        
        --id 2/2
        tx_Data_pc<=x"00";
        WAIT FOR 20 ns;
        tx_enable_pc<='1';
        wait for 20 ns;
        tx_enable_pc<='0';
        
        
        WAIT FOR word_wait ;
        --id 1/2
        tx_Data_pc<=x"00";
        WAIT FOR 20 ns;
        tx_enable_pc<='1';
        wait for 20 ns;
        tx_enable_pc<='0';
        
        
        case prueba_sub_n is
          when 0 =>
          
          wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba : write PESO
                
                tx_Data_pc<=W_PESO_INSTRUCTION; --con id seleccionado, WRITE PESO
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                
                
                WAIT FOR word_wait ;
                
                --pointer 2/2
                tx_Data_pc<=x"00";
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0';
                
                
                WAIT FOR word_wait ;
                --pointer 1/2
                tx_Data_pc<=x"00";
                WAIT FOR 20 ns;
                tx_enable_pc<='1'; 
                wait for 20 ns;
                tx_enable_pc<='0';
                
                WAIT FOR word_wait ;
                
                --peso 2/2   peso: 03E8==1000
                
                
                dato_w<=1000;
                wait for 100 ns;
                tx_Data_pc<=dato_w2;
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0';
                
                
                WAIT FOR word_wait ;
                --peso 1/2
                tx_Data_pc<=dato_w1;
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0';
                
          when 1 =>      
          wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba : read peso  instruction
                
                
                tx_Data_pc<=r_PESO_INSTRUCTION; --leemos el peso que guardo
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                
                
                WAIT FOR word_wait ;
                
                --pointer 2/2
                tx_Data_pc<=x"00";
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0';
                
                
                WAIT FOR word_wait ;
                
                --pointer 1/2
                tx_Data_pc<=x"00";
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0';
                
           when 2 =>     
           wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba: WRITE peso instruction INDEX ERROR
                
                tx_Data_pc<=W_PESO_INSTRUCTION; 
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                
                
                WAIT FOR word_wait ;
                
                --pointer 2/2
                tx_Data_pc<=x"00";
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0';
                
                
                WAIT FOR word_wait ;
                --pointer 1/2
                tx_Data_pc<=x"03";
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0';
                
                WAIT FOR word_wait ;
                
                --peso 2/2   peso: 03E8==1000
                
                
                dato_w<=1000;
                wait for 100 ns;
                tx_Data_pc<=dato_w2;
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0';
                
                
                WAIT FOR word_wait ;
                --peso 1/2
                tx_Data_pc<=dato_w1;
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0';
                
             when 3 =>   
             wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba: READ peso instruction INDEX ERROR
                
                tx_Data_pc<=r_PESO_INSTRUCTION; 
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                
                
                WAIT FOR word_wait ;
                
                --pointer 2/2
                tx_Data_pc<=x"00";
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0';
                
                
                WAIT FOR word_wait ;
                --pointer 1/2
                tx_Data_pc<=x"03";
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0';
                
            when 4 =>   
             wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba: WRITE LEAK
                
                tx_Data_pc<= W_LEAK_INSTRUCTION; 
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                              
                WAIT FOR word_wait ;
                --pointer 8bits
                tx_Data_pc<=x"00";
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0';
                
                WAIT FOR word_wait ;
                
                --leak 8 BITS
                tx_Data_pc<=x"10"; --valor de (-)16 
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0';
                
           when 5 =>   
             wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba: WRITE LEAK INDEx ERROR
                
                tx_Data_pc<= W_LEAK_INSTRUCTION; 
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                 
                WAIT FOR word_wait ;
                
                --pointer 8bits
                tx_Data_pc<=x"0B"; --si L es 10 el maximo es 9 (9-0) hemos elegido el 11
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0';       
           
                WAIT FOR word_wait ;
                
                --valor 8bits
                tx_Data_pc<=x"10"; --max 255
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0';
            when 6 =>   
             wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba: read LEAK 
                
                tx_Data_pc<= R_LEAK_INSTRUCTION; 
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                        
           
                WAIT FOR word_wait ;
                
                --pointer 8bits
                tx_Data_pc<=x"09"; --si L es 10 el maximo es 9 (9-0)
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0'; 
                
             when 7 =>   
             wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba: read LEAK WITH INDEX ERROR
                
                tx_Data_pc<= R_LEAK_INSTRUCTION; 
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                        
           
                WAIT FOR word_wait ;
                
                --pointer 8bits
                tx_Data_pc<=x"0A"; --si L es 10 el maximo es 9 (9-0)
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0'; 
                
             when 8 =>   
             wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba: WRITE ACTIVATION OF LEAK
                
                tx_Data_pc<= W_LEAK_ACT_INSTRUCTION; 
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                        
           
                WAIT FOR word_wait ;
                
                --pointer 8bits
                tx_Data_pc<=x"00"; --DESACTIVO , por defecto esta activada
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0'; 
            when 9 =>   
             wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba: READ ACTIVATION OF LEAK (DEFECTO ESTA A 1)
                
                tx_Data_pc<= R_LEAK_ACT_INSTRUCTION; 
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                        
           
                WAIT FOR word_wait ;
                
            when 10 =>   
             wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba: write vth
                
                tx_Data_pc<= W_VTH_INSTRUCTION; 
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                        
           
                WAIT FOR word_wait ; 

                
                --valor H
                tx_Data_pc<=x"00"; --DESACTIVO , por defecto esta activada
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0'; 
                
                
                WAIT FOR word_wait ; 
                
                --valor L
                tx_Data_pc<=x"FF"; --DESACTIVO , por defecto esta activada
                WAIT FOR 20 ns;
                tx_enable_pc<='1';
                wait for 20 ns;
                tx_enable_pc<='0'; 
                
            when 11 =>   
             wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba: read vth
                
                tx_Data_pc<= R_VTH_INSTRUCTION; 
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                        
           
                WAIT FOR word_wait ;
                
            when 12 =>   
             wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba:  read voltaje actual
                
                tx_Data_pc<= R_VOLTAGE_INSTRUCTION; 
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                        
           
                WAIT FOR word_wait ;
            when 13 =>   
             wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba:  read leak pointer position
                
                tx_Data_pc<= R_LEAKP_INSTRUCTION; 
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                        
           
                WAIT FOR word_wait ;
                
           when 14 =>   
             wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba:  debug error state
                
                tx_Data_pc<= null_instruction; 
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                        
           
                WAIT FOR word_wait ;  
         when others=>
    
         end case;
      END IF;
    when others=>
    
    end case;
   
   
   ----------------------------------------------------------------
   --INSERTAR AQUI SEGUNDA ACCION PARA LEER LO ESCRITO O ESCRIBIR MAS
   -----------------------------------------------------------------
--    wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba: READ ACTIVATION OF LEAK (DEFECTO ESTA A 1)
                
--                tx_Data_pc<= R_LEAK_ACT_INSTRUCTION; 
--                wait for 10ns;
--                tx_enable_pc<='1';
--                wait for 15 ns;
--                tx_enable_pc<='0';
                        
           
--                WAIT FOR word_wait ;
 
    
    
    
    
    
    
    
    
    
    
    
    
    
-------------------------------------------------------------------------------------------- 
--terminamos de enviar cualquier instruccion/es con la intruccion end_instructions 0X03
-------------------------------------------------------------------------------------------- 
wait until (rx_data_pc=error_rango_instruction or rx_data_pc=done_instruction) and rx_done_pc='1';  --prueba : end instrucci?n/es
                
                tx_Data_pc<=END_INSTRUCTIONS; 
                wait for 10ns;
                tx_enable_pc<='1';
                wait for 15 ns;
                tx_enable_pc<='0';
                WAIT;
    
end process;
end Behavioral;
