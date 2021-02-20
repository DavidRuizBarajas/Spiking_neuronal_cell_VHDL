@echo off
REM ****************************************************************************
REM Vivado (TM) v2019.1 (64-bit)
REM
REM Filename    : simulate.bat
REM Simulator   : Xilinx Vivado Simulator
REM Description : Script for simulating the design by launching the simulator
REM
REM Generated by Vivado on Wed Jan 06 18:23:29 +0100 2021
REM SW Build 2552052 on Fri May 24 14:49:42 MDT 2019
REM
REM Copyright 1986-2019 Xilinx, Inc. All Rights Reserved.
REM
REM usage: simulate.bat
REM
REM ****************************************************************************
echo "xsim funcionamiento_completo_TB_behav -key {Behavioral:sim_1:Functional:funcionamiento_completo_TB} -tclbatch funcionamiento_completo_TB.tcl -view C:/Users/David/Desktop/TFG VHDL versiones/23_12_2020/neuronal_cell_debug/funcionamiento_completo_simulacion_debug.wcfg -log simulate.log"
call xsim  funcionamiento_completo_TB_behav -key {Behavioral:sim_1:Functional:funcionamiento_completo_TB} -tclbatch funcionamiento_completo_TB.tcl -view C:/Users/David/Desktop/TFG VHDL versiones/23_12_2020/neuronal_cell_debug/funcionamiento_completo_simulacion_debug.wcfg -log simulate.log
if "%errorlevel%"=="0" goto SUCCESS
if "%errorlevel%"=="1" goto END
:END
exit 1
:SUCCESS
exit 0
