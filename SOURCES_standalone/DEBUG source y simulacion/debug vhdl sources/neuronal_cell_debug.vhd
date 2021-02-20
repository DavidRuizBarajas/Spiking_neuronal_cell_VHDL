----------------------------------------------------------------------------------
-- Company: David Ruiz Barajas, UC3M (spain), free code
-- Engineer: David Ruiz Barajas
-- 
-- Create Date: 09.03.2020 17:27:08
-- Design Name: neuronal_cell
-- Module Name: neuronal_cell - Behavioral
-- Project Name: celda neuronal para red neuronal
-- Target Devices: basys3
-- Tool Versions: vivado 2019.1
-- Description: 
-- 
-- Dependencies: uart, states_debug and input_detector (.vhd)
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


entity neuronal_cell is
    
    Generic (
             N:integer range 1 to 65536:= 2; --numero de entradas genericas a la celda 2^6
             ID: integer :=0; --id de la celda  max value: 2^16
             L: integer range 1 to 256:=10; --leak numbers 2^8
             clk_freq:	INTEGER		:= 100_000_000;	--frequency of system clock in Hertz frecuencia de basys3 
		     baud_rate:	INTEGER		:= 19_200		--data link baud rate in bits/second max: 12Mbouds/s 12_000_000
             ); 

    Port (
          input: in std_logic_vector(N-1 downto 0); 
          clk: in std_logic;
          reset: in std_logic; --elimina pesos, threshold, leak 
          spike: out std_logic; --pulso de salida
	--------------------------------
	--        señales DEBUG       --
	--------------------------------
          voltage_debug: out integer range -32768  to 32767;
          holder_p_debug:out integer range 0 to N;
          leak_p_debug: out integer;
          estado_debug: out estado;
          spk_detected_debug:out std_logic_vector(N-1 downto 0);
          detecto_reset:out std_logic_vector(N-1 downto 0);
          --------------------
          tx_cell:out std_logic; --vemos que se mantiene en 0 pero en la comun es Z
          tx_data_debug: out std_logic_vector(7 downto 0);--vemos que datos envia 
          rx_data_debug: out std_logic_vector(7 downto 0); --vemos que datos recibe
          
          instruction_b_debug: out std_logic_vector(7 downto 0); --vemos el buffer de instrucciones_
          buffer_rx_01: out std_logic_vector(15 downto 0);
          buffer_rx_23: out std_logic_vector(15 downto 0);
          id_match_debug: out std_logic;
          rx_error_debug: out std_logic;
          t_blocks: out integer range 0 to 4;
          r_blocks: out integer range 0 to 2;
          range_err_debug: out std_logic;
        --------------------------------
	--    Fin de señales DEBUG    --
	--------------------------------


          --bus de configuracion/comunicacion

          new_instruction: inout std_logic;
          rx_com: in std_logic;
          tx_com: out std_logic;
          
          --control de acciones
          setting: in std_logic; --modo set parametros
          soft_rst_b: in std_logic;
          stop: in std_logic --paramos la red neuronal, standby

    );
end neuronal_cell;

architecture Behavioral of neuronal_cell is
----------------------------------------------
--INSTRUCCIONES DE CONFIGURACION/CONMUNICACION
----------------------------------------------
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

---------------------------
--VARIABLES Y CONSTANTES
---------------------------

--MAQUINA DE ESTADOS (VARIABLES)
signal actual,siguiente :estado;

--MEMORIAS: PESOS, PERDIDA, UMBRAL Y VOLTAJE (weigth,leak,threshold,voltage)

type weight_mem is array (N-1 downto 0) of integer range -32768  to 32767; --2^15 + 1 bit polarity 
signal pesos: weight_mem;

type leak_mem is array (L-1 downto 0) of integer range 0 to 255; --SON NEGATIVOS pues se restan
signal leak_values: leak_mem;

signal vth: integer range 0 to 32767;
signal voltage: integer range -32768  to 32767; ---2^15 + 1 bit polarity


--POINTERS DE MEMORIA & BOUNDS 


signal holder_pointer: integer range 0 to N; --N bits pointer de los holders y pesos en working mode
signal leak_pointer: integer range 0 to L-1;

constant holder_p_max: integer:= N-1; --n-1 
constant leak_p_max: integer:= L-1 ; 

--PULSO de salida (spike activation output)
signal spike_flag:std_logic;
signal leak_wrk:std_logic;
--VARIABLES CONFIGURACION & COMUNICACION (config & comunication)

signal id_match,end_instruction,read_new_instruction: std_logic;
signal tx_busy_out:std_logic; 
signal error_rango: std_logic;

constant max_block_can_recieve:integer:=4;
signal recieve_blocks: integer range 0 to (max_block_can_recieve);
signal transmit_blocks: integer range 0 to 2;

--BUFFERS CONFIGURACION & COMUNICACION (buffers config & comunication)
signal instruction_buffer: std_logic_vector (7 downto 0);--integer range 0 to 255; --8 bits

type data is array (max_block_can_recieve downto 1) of std_logic_vector (7 downto 0); 
signal data_buffer: data;

signal dato_rx_1: std_logic_vector (15 downto 0) ;
signal dato_rx_2:std_logic_vector (15 downto 0) ;

--------------------------------------------
--SEÑALES DE COMPONENTES (component signals)
--------------------------------------------

--INPUT spike detectors (detectores de pulsos)

signal spk_detected: std_logic_vector(N-1 downto 0);
signal rst_d: std_logic_vector(N-1 downto 0);
signal spk_no_detected: std_logic;

--señales componente UART

constant d_width:	INTEGER		:= 8; 			--data bus width
signal tx_enable:	STD_LOGIC;										--initiate transmission
signal tx_data:		STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  --data to transmit
signal rx:			STD_LOGIC;										--receive pin
signal rx_busy:		STD_LOGIC;										--data reception in progress
signal rx_error:	STD_LOGIC;										--start, parity, or stop bit error detected
signal rx_data:		STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);	--data received
signal tx_busy:		STD_LOGIC;  									--transmission in progress
signal tx:		    STD_LOGIC;
signal rx_done:  std_logic;		
-- baud_clk: std_logic;

-------------------------------------------------------------
--    DECALARACION DE COMPONENTES - component declaracion
-------------------------------------------------------------

component input_detector port(
           clk : in STD_LOGIC;
           rst_d : in STD_LOGIC;
           input_spk : in STD_LOGIC;
           spk_detected : out STD_LOGIC);
           end component;
-- - - - - - - - - - - - - - - -           
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
		tx			:	OUT	STD_LOGIC--;
		--baud_clk:         out std_logic
		);										--transmit pin
        end component;
------------------------------------------------------------

begin

-------------------------------------------------------------
--      instanciaciones detectores de pulsos (spikes input)
-------------------------------------------------------------  

detectores: for i in input'range generate
    holderx:    input_detector port map(clk=>clk,rst_d=>rst_d(i),input_spk=>input(i),spk_detected=>spk_detected(i));
end generate;
--holderx:    input_detector port map(clk=>clk,reset=>reset,rst_d=>rst_d(0),input_spk=>input(0),spk_detected=>spk_detected(0));

-------------------------------------------------------------
--       UART component- instanciacion
-------------------------------------------------------------  

uartx:    uart generic map(100000000,19200,8,8,1,'0')
               port map(clk=>clk, reset=>reset, tx_enable=>tx_enable, tx_data=>tx_data, rx=>rx, rx_busy=>rx_busy, rx_done=>rx_done, rx_error=>rx_error, rx_data=>rx_data, tx_busy=>tx_busy, tx=>tx
               --,baud_clk=>baud_clk
               );


-------------------------------------------------------------
--     control maestro (maquina de estados)
-------------------------------------------------------------
process(clk,reset)
begin

if reset='1' then
--memorias reset --los valores van desde el N-1 A 0 (orden de las posiciones)
    pesos<=(1,5); --valor por defecto para probar sin tener que configurar
    --pesos<=(others=>0); --valor por defecto para el diseño
    leak_values<=(255,128,64,32,16,8,4,2,1,0);
    voltage<=0;
    vth<=15; --valor mitad por defecto
--punteros
    holder_pointer<=0;
    leak_pointer<=0;
--detectores
    rst_d<=(others=>'1');
    spk_no_detected<='1';
--maquina de estados solo aqui 
    actual<=reset_iddle;
--senales output
    spike_flag<='0';
--otros
    leak_wrk<='1'; --leak activation
--comunicacion y configuracion
    id_match<='0';
    instruction_buffer<=(others=>'0');
    data_buffer<=(others=>(others=>'0'));
    transmit_blocks<=0;
    recieve_blocks<=0;
    end_instruction<='0';
    error_rango<='0';
elsif rising_edge(clk) then

    actual<=siguiente;
    rst_d(to_integer(to_unsigned(holder_pointer,N-1)))<='0'; --a menos que entremos en driver y este encendido el detector (se aplicara 1)
    spike_flag<='0'; -- a menos que en suma se pase de vth o sea negativo
    
    if actual= working_stopped then
        --mantenemos todos biestables. esto en verdad se puede borrar.
        pesos<=pesos;
        leak_values<=leak_values;
        voltage<=voltage;
        vth<=vth;
        leak_wrk<=leak_wrk;
        holder_pointer<=holder_pointer;
        leak_pointer<=leak_pointer;
        
        rst_d<=rst_d;
        spk_no_detected<=spk_no_detected;

        spike_flag<=spike_flag;
    elsif actual= holder_pointer_driver then
        if stop='0' then
             if holder_pointer=holder_p_max then --fin del ciclo 
                if spk_detected(to_integer(to_unsigned(holder_pointer,N-1)))='1' then -- fin de ciclo, se ha detectado una señal en el ultimo next: suma
                     rst_d(holder_pointer)<='1';
                     --leak_pointer<=0; --borramos posicion del leak, mejor en la sumaaa
                     -- el punteor lo reseteo en la suma
                     
                 elsif spk_detected(to_integer(to_unsigned(holder_pointer,N-1)))='0' then 
                    if spk_no_detected='0' or leak_wrk='0' then --fin del ciclo, se detecto algun spike -------------- next: holder_pointer_driver no necesita leak
                    
                         leak_pointer<=0; --borramos posicion del leak pues no entra en la suma
                         holder_pointer<=0; --empezamos un nuevo ciclo
                         
                    --else spk_no_detected='1' and leak_wrk='1' then --fin de ciclo, no se detecto nada, reiniciamos ciclo pero vamos a leak primero
                           
                    end if;
                 end if;
                 
                 spk_no_detected<='1';-- reset de detecciones del ciclo   
             
             elsif holder_pointer<holder_p_max then
                if spk_detected( to_integer(to_unsigned(holder_pointer,N-1)))='0' then --NEXT : holder_pointer_driver
                    holder_pointer<=holder_pointer+1;
                    leak_pointer<=leak_pointer;
                    spk_no_detected<=spk_no_detected; --puede ser 1 o 0
                    
                    
                else --spk_detected( to_integer(to_unsigned(holder_pointer,N-1)))='1'  --NEXT : suma
                    holder_pointer<=holder_pointer; --luego lo incrementamos
                    leak_pointer<=0; --si detecta 1 pulso el leak lo ponemos a 0
                    spk_no_detected<='0'; --hemos detectado 1 pulso, se mantiene para todo el ciclo a 0
                    rst_d(holder_pointer)<='1'; -- borramos el flanco de deteccion 
                end if;   
             end if;  --else back to working stopped
        end if;
    elsif actual= voltage_sum then
        rst_d(holder_pointer)<='0';
        spk_no_detected<='0';
        leak_pointer<=0;
        if voltage+pesos(holder_pointer)<0 or voltage+pesos(holder_pointer)>=vth then
            voltage<=0;
            spike_flag<='1';
        else
            spike_flag<='0';
            voltage<=voltage+pesos(holder_pointer);
        end if;
        
        if holder_pointer=holder_p_max then
            holder_pointer<=0;
        elsif holder_pointer<holder_p_max then
            holder_pointer<=holder_pointer+1; 
        end if;
        --NEXT : always to drive pointer TO DO: comprobar que suma le peso correcto antes de sumar +1 el pointer
    elsif actual= voltage_leak then
        holder_pointer<=0;
        spk_no_detected<='1';--mantenemos a 1
        if leak_pointer /= leak_p_max then 
            leak_pointer <=leak_pointer  +1; -- else se mantiene en el maximo
         end if;
         if voltage>leak_values(leak_pointer) then
            voltage<=voltage-leak_values(leak_pointer);
         else
            voltage<=0;
         end if;

     
     elsif actual= soft_rst then --como reiniciar la maquina sin borrar los datos de memorias
        --memorias reset 
           voltage<=0;
        --punteros
            holder_pointer<=0;
            leak_pointer<=0;
        --detectores
            rst_d<=(others=>'1');
            spk_no_detected<='1';
        --senales output
            spike_flag<='0';
     ------------------------------------------------
     --maquina de estados II, estados de comunicacion
     ------------------------------------------------
     elsif actual= config_init then        

        --punteros reset
            --holder_pointer<=0;
            --leak_pointer<=0;
        --detectores reset
            --rst_d<=(others=>'1');
            --spk_no_detected<='1';
        --senales output reset
            spike_flag<='0';
         --variables de comunicacion--
            id_match<='0';
            instruction_buffer<=(others=>'0');
            data_buffer<=(others=>(others=>'0'));
            transmit_blocks<=0;
            recieve_blocks<=0;
            end_instruction<='0';
            error_rango<='0';
            -- el envio de datos es concurrente, no queremos un registro de los datos que queremos transmitir
       elsif actual= wait_init then
            --espera a enviar el primer mensaje
       elsif actual= wait_instruction then 
            -- reset de retorno de instruccion
            -- nunca id_match<='0' eso es mediante una instruccion
            instruction_buffer<=(others=>'0');
            data_buffer<=(others=>(others=>'0'));
            transmit_blocks<=0;
            recieve_blocks<=0;     
            end_instruction<='0';
            error_rango<='0';
            
       elsif actual= set_instruction then 
                 
                instruction_buffer<=rx_data;
                case rx_data is
                    when null_instruction =>

                        transmit_blocks<=1;
                        recieve_blocks<=0;
                    when ready_instruction =>

                        transmit_blocks<=1;
                        recieve_blocks<=0;
                    when done_instruction =>

                        transmit_blocks<=1;
                        recieve_blocks<=0;
                    when end_instructions =>

                        transmit_blocks<=0;
                        recieve_blocks<=0;
                        end_instruction<='1';
                    when id_instruction =>

                        transmit_blocks<=0;
                        recieve_blocks<=2;
                    when w_peso_instruction=>

                        transmit_blocks<=0;
                        recieve_blocks<=4;
                    when w_leak_instruction=>

                        transmit_blocks<=0;
                        recieve_blocks<=2;
                    when w_leak_act_instruction =>

                        transmit_blocks<=0;
                        recieve_blocks<=1;
                    when w_vth_instruction =>

                        transmit_blocks<=0;
                        recieve_blocks<=2;
                    when error_rango_instruction =>

                        transmit_blocks<=0;
                        recieve_blocks<=0;
                    when r_peso_instruction =>

                        transmit_blocks<=2;
                        recieve_blocks<=2;
                    when r_leak_instruction => 

                        transmit_blocks<=1;
                        recieve_blocks<=1;
                    when r_vth_instruction =>

                        transmit_blocks<=2;
                        recieve_blocks<=0;
                    when r_voltage_instruction =>

                        transmit_blocks<=2;
                        recieve_blocks<=0;
                    when r_leakP_instruction => 

                        transmit_blocks<=1;
                        recieve_blocks<=0;
                    when r_leak_act_instruction => 

                        transmit_blocks<=1;
                        recieve_blocks<=0;
                    when others =>
                        --error                        
                    end case;
          
       elsif actual= wait_read_8b then 
       
            --nada
            
       elsif actual= save_8bit then --done
       
            if recieve_blocks/=0 then
                recieve_blocks<=recieve_blocks-1;
                data_buffer(recieve_blocks)<=rx_data;
               
            else
                data_buffer(recieve_blocks)<=data_buffer(recieve_blocks);
                recieve_blocks<=recieve_blocks;
            end if;
      elsif actual=buffer_refresh then
            --      
      elsif actual= execute_write then
      
          case instruction_buffer is
            
                    when id_instruction =>
                        if to_integer(unsigned( dato_rx_1 ))=ID then
                            id_match<='1';
                        else 
                            id_match<='0';
                        end if;
                    when w_peso_instruction=>
                    
                       if to_integer(unsigned( dato_rx_2 )) > pesos'high then
                            error_rango<='1';
                       else
                            pesos(to_integer(unsigned( dato_rx_2 )))<=to_integer(signed( dato_rx_1 ));
                       end if;
                    when w_leak_instruction=>
                        if to_integer(unsigned( dato_rx_1(15 downto 8) )) > LEAK_VALUES'HIGH then
                            error_rango<='1';
                        else
                            leak_values(to_integer(unsigned( dato_rx_1(15 downto 8) )))<=to_integer(unsigned( dato_rx_1(7 downto 0) ));
                        end if;   
                    when w_leak_act_instruction =>
                        
                        leak_wrk<= std_logic'(data_buffer(1)(0));

                    when w_vth_instruction =>
                        
                        vth<=to_integer(signed( dato_rx_1 ));
                        
                    when others =>
                    
                                             
           end case;
                    
      elsif actual= send_8b then
      
        transmit_blocks<=transmit_blocks-1;
        
        if instruction_buffer=r_peso_instruction then
            if  to_integer(unsigned( dato_rx_1 )) > pesos'high then
                error_rango<='1';
            end if;
         elsif instruction_buffer = r_leak_instruction then
            if to_integer(unsigned( dato_rx_1(7 downto 0)  )) > LEAK_VALUES'HIGH then
                 error_rango<='1';
            end if;
         end if;
                          
      elsif actual= wait_sending then
      
             --espera
      
      elsif actual= end_config then
      
            id_match<='0';
            instruction_buffer<=(others=>'0');
            data_buffer<=(others=>(others=>'0'));
            transmit_blocks<=0;
            recieve_blocks<=0;
            end_instruction<='0';
            
      elsif actual= send_instruction_done then
      elsif actual= wait_sending_done then
      elsif actual= notmatch_wait then
      elsif actual= instruction_driver then
      else
      --nada
    end if;
    
end if;

end process;

process(actual,holder_pointer,voltage,stop,setting,spk_detected,spk_no_detected,soft_rst_b,leak_wrk,tx_busy,rx_done,rx_error,instruction_buffer,id_match,read_new_instruction,pesos,dato_rx_2,dato_rx_1,error_rango,leak_values,vth,leak_pointer,recieve_blocks,transmit_blocks,end_instruction)
begin

tx_enable<='0';
tx_data<="00000000";
siguiente<=soft_rst;

    case actual is
        when reset_iddle =>
            siguiente<= working_stopped;

        when working_stopped =>
        
            if stop='1' then
                if setting='1' then
                    siguiente <= config_init;                                                       
                elsif soft_rst_b='1' then
                    siguiente <= soft_rst;                                                       
                else
                    siguiente <= working_stopped;
                end if;
            else
               siguiente <= holder_pointer_driver;
            end if;
            
           
        when voltage_sum => 

                siguiente<=holder_pointer_driver;

        when holder_pointer_driver =>
            if stop='0' then
            
                if holder_pointer = holder_p_max then
                    if spk_detected(to_integer(to_unsigned(holder_pointer,N-1)))='1' then -- ultimo check. fin de ciclo, se ha detectado una señal en el ultimo next: suma
    
                         siguiente<= voltage_sum;
                         
                     elsif spk_detected(to_integer(to_unsigned(holder_pointer,N-1)))='0' then -- ultimo check.
                        if spk_no_detected='0' or leak_wrk='0' then --fin del ciclo y se detecto algo antes, no leak next: holder_pointer_driver
                             
                             siguiente<=holder_pointer_driver;
                             
                        else --spk_no_detected='1' and leak_wrk='1' then --fin de ciclo, no se detecto nada, next: leak ?
                                siguiente<=voltage_leak;                                             -- todo: implmentar leak off
                        end if;
                      else --error
                        siguiente<=reset_iddle;
                     end if;

                else --holder_pointer < holder_p_max =N-1
                    if spk_detected( to_integer(to_unsigned(holder_pointer,N-1)))='0' then
                        siguiente <= holder_pointer_driver;
                    else-- spk_detected( to_integer(to_unsigned(holder_pointer,N-1)))='1'
                        siguiente <= voltage_sum;
                    end if;
                end if;

            else --stop =1
            
                siguiente<=working_stopped;
                
            end if;
        when voltage_leak =>
        
            siguiente <= holder_pointer_driver;
            
        when soft_rst =>
            if soft_rst_b='0' then
                siguiente<=working_stopped;
            else
                siguiente<=soft_rst;
            end if;
        ---------------------------------------- 
        --CONFIGURACION (maquina de estados II)
        ----------------------------------------
        when config_init =>
        
            siguiente <= wait_init;
            tx_data<="00000001"; --0x01 instruccion redy to configure. 
            if ID=0 then
                tx_enable<='1';
            else
                tx_enable<='0';
            end if;            
        when wait_init =>
        
            if (tx_busy='0' AND ID=0 )or read_new_instruction='1'  then
                siguiente<= wait_instruction;
            else
                siguiente<=wait_init;
            end if;
            
        when wait_instruction =>
        
             if(rx_done='1') then
                siguiente<= set_instruction;  
             else
                siguiente<= wait_instruction; 
             end if;
         when set_instruction =>
            siguiente<= intruction_routing;
         when intruction_routing =>
         
             if end_instruction='1' then
                siguiente<=end_config;
             elsif id_match='0' and instruction_buffer/= id_instruction then
                siguiente<=notmatch_wait;
             elsif recieve_blocks/=0 then
                siguiente<= wait_read_8b;
             elsif recieve_blocks=0 and transmit_blocks/=0 then
                siguiente<= send_8b;
             else --otras 
                siguiente<=send_instruction_done;
             end if;    
             
          when wait_read_8b => --aqui restamos-1
          
            if rx_done='1' then
                siguiente<=save_8bit;
            else
                siguiente<=wait_read_8b;
            end if;               
          when save_8bit =>
          
          
            if recieve_blocks=1 then
                if transmit_blocks=0 then
                    siguiente<=buffer_refresh; 
                else
                    siguiente<=send_8b; --todo:
                end if;
            else
              siguiente<= wait_read_8b;
            end if; 
            
          when buffer_refresh=>   siguiente<=execute_write;
          
          when execute_write =>   
            
                siguiente<=send_instruction_done;

          when send_8b => 
          
          
            tx_enable<='1';
            siguiente<= wait_sending;
            
            case instruction_buffer is
                    when null_instruction =>  tx_data<=x"00";
                    when ready_instruction => tx_data<=x"00";
                    when done_instruction =>  tx_data<=x"00";
                    when r_peso_instruction =>
                    
--                        transmit_blocks<=2;
--                        recieve_blocks<=2;
                        if to_integer(unsigned( dato_rx_1 )) > pesos'high then
                            tx_data<=x"00";--error
                         else
                            if(transmit_blocks=2)then
                                tx_data<=std_logic_vector(to_signed(pesos( to_integer(unsigned( dato_rx_1 ))),16)(15 downto 8));
                            else
                                tx_data<=std_logic_vector(to_signed(pesos( to_integer(unsigned( dato_rx_1 ))),16)(7 downto 0));
                            end if;
                         end if;   
                    when r_leak_instruction => 
--                        transmit_blocks<=1;
--                        recieve_blocks<=2;
                        if to_integer(unsigned( dato_rx_1(7 downto 0)  )) > LEAK_VALUES'HIGH then
                            tx_data<=x"00";
                        else
                           tx_data<=std_logic_vector(to_unsigned(   leak_values( to_integer(unsigned( dato_rx_1(7 downto 0) ))),   8));
                           
                        END IF;
                    when r_vth_instruction =>
                        --read vth
--                        transmit_blocks<=2;
--                        recieve_blocks<=0;
                        if(transmit_blocks=2)then
                            tx_data<=std_logic_vector(to_signed(vth,16)(15 downto 8));
                        else
                            tx_data<=std_logic_vector(to_unsigned(vth,16)(7 downto 0));
                        end if;
                    when r_voltage_instruction =>
--                        transmit_blocks<=2;
--                        recieve_blocks<=0;
                        if(transmit_blocks=2)then
                            tx_data<=std_logic_vector(to_signed(voltage,16)(15 downto 8));
                        else
                            tx_data<=std_logic_vector(to_unsigned(voltage,16)(7 downto 0));
                        end if;
                    when r_leakP_instruction => 

--                        transmit_blocks<=1;
--                        recieve_blocks<=0;
                        tx_data<=std_logic_vector(to_unsigned(leak_pointer,8));
                        
                    when r_leak_act_instruction => 

--                        transmit_blocks<=1;
--                        recieve_blocks<=0;
                        tx_data<="0000000"&leak_wrk;
                        
                    when others =>
                        --error                        
                    end case;
            
            
          when wait_sending => --aqui restamos -1
          
          
            if tx_busy='0' then
                if transmit_blocks=0 then
                   
                  siguiente<=send_instruction_done;
                    
                else 
                    siguiente<=send_8b;
                end if;
            else
                siguiente<=wait_sending;
            end if;
            
          
            
          when send_instruction_done =>  
          
            IF ID_MATCH='1' THEN
                tx_enable<='1';
                siguiente<=wait_sending_done;
                
                if error_rango='1' then --error de rango
                    tx_data<=error_rango_instruction;
                else
                    tx_Data<=done_instruction;
                end if;
            else
                tx_enable<='0';
                tx_Data<=done_instruction;
                siguiente<=notmatch_wait;
            END IF;
            
            
          when wait_sending_done =>  
          
          
            if tx_busy='0' then
                siguiente<=instruction_driver;
            else
                siguiente<=wait_sending_done;
            end if;
          when instruction_driver =>  
          
            siguiente<=wait_instruction;
            
          when notmatch_wait => 
          
          
            if read_new_instruction='1' then
                siguiente<=wait_instruction;
            else
                siguiente<=notmatch_wait;
            end if;
            
          when end_config =>
            if setting='0' then
                siguiente<=working_stopped;
            else
                siguiente<=end_config;
            end if;        
            
        when others =>
        
            siguiente <= reset_iddle ;--fatal error se resetea la maquina de estados
    end case;
end process;

-------------------------------------
--Señales de salida (output signals)
------------------------------------
spike<=spike_flag;


tx_com<=tx when id_match='1' else tx when (actual=config_init or actual=wait_init) and id=0 else 'Z';

rx<=rx_com;

new_instruction<='1' when (actual=instruction_driver AND id_match='1')or (id=0 and actual=wait_init and tx_busy='0') else 'Z';
read_new_instruction<=new_instruction;

--------------------------------------------
--ORDENAMIENTO DE LOS BUFFERS (DE LOS DATOS)
--------------------------------------------
dato_rx_1 <= data_buffer(2)&data_buffer(1);
dato_rx_2 <= data_buffer(4)&data_buffer(3);

--------------------------------------------
--Salida para las señales DEBUG
--------------------------------------------
voltage_debug<=voltage;
estado_debug<=actual; 
holder_p_debug<=holder_pointer;
leak_p_debug<=leak_pointer;
spk_detected_debug<=spk_detected;
detecto_reset<=rst_d;

--Debug de la comunicacion UART 

tx_cell<=tx;
tx_data_debug<=tx_data;
rx_data_debug<=rx_data;
instruction_b_debug<=instruction_buffer;
buffer_rx_01 <= data_buffer(2)&data_buffer(1);
buffer_rx_23 <= data_buffer(4)&data_buffer(3);

id_match_debug<=id_match;
rx_error_debug<=rx_error;
t_blocks<=transmit_blocks;
r_blocks<=recieve_blocks;
range_err_debug<=error_rango;



end Behavioral;