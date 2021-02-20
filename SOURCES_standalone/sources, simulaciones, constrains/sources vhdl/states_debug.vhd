----------------------------------------------------------------------------------
-- Company: 
-- Engineer: David Ruiz Barajas
-- 
-- Create Date: 07.07.2020 13:21:34
-- Design Name: 
-- Module Name: states_debug - Behavioral
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

package states_debug is
type estado is (    --estados de pulsos
                    reset_iddle,soft_rst, working_stopped, voltage_sum, holder_pointer_driver, voltage_leak,
                    --configuracion inicio, obtencion de la instruccion
                    config_init, wait_init, wait_instruction, set_instruction,intruction_routing,
                    --recepcion de datos
                    wait_read_8b, save_8bit, execute_write,buffer_refresh,
                    --envio de datos
                    send_8b,wait_sending,
                    --finalizacion de la instruccion y finalizacion de la configuracion total
                    send_instruction_done, wait_sending_done,instruction_driver,
                    --otros de configuracion
                    notmatch_wait,end_config
                    );

end package;
