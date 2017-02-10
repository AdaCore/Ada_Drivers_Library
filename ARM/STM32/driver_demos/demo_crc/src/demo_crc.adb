------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017, AdaCore                           --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

--  This program demonstrates use of the CRC processor on STM32F4x MCUs. (The
--  STM32F3 MCUs, among others, have additional CRC capabilities.)

--  The checksum for a given block of 32-bit words is calculated two ways and
--  then compared.

--  The checksum is first computed on the data block by calling a routine that
--  transfers the block content to the CRC processor in a loop, which is to say
--  that the CPU does the transfer. In the second approach, a different routine
--  is called. This second routine uses DMA to transfer the data block to
--  the CRC processor. Obviously for a large data block, this second approach
--  is far quicker.

--  The first routine includes a parameter that provides the checksum. The
--  second routine does not have this output parameter since the caller will
--  return before the transfer completes (in practice) and the checksum is
--  computed. Hence after calling the second routine the caller blocks,
--  waiting for the DMA transfer completion interrupt.

--  If the two checksums are equal then the green LED is turned on, otherwise
--  the red LED is turned on. Both checksum values are displayed.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with STM32.Device;    use STM32.Device;
with STM32.Board;     use STM32.Board;
with STM32.CRC;       use STM32.CRC;
with STM32.CRC.DMA;   use STM32.CRC.DMA;
with LCD_Std_Out;     use LCD_Std_Out;
with HAL;             use HAL;
with STM32.DMA;       use STM32.DMA;
with Ada.Real_Time;   use Ada.Real_Time;
with Memory_Transfer; use Memory_Transfer;
with Interfaces;      use Interfaces;

procedure Demo_CRC is

   Checksum_CPU : UInt32 := 0;
   Checksum_DMA : UInt32 := 0;

   --  see STM32Cube_FW_F4_V1.6.0\Projects\STM324x9I_EVAL\Examples\CRC\CRC_Example\Src\main.c
   --  for data and expected CRC value

   Data : constant Block_32 :=
   (16#00001021#, 16#20423063#, 16#408450a5#, 16#60c670e7#, 16#9129a14a#, 16#b16bc18c#,
    16#d1ade1ce#, 16#f1ef1231#, 16#32732252#, 16#52b54294#, 16#72f762d6#, 16#93398318#,
    16#a35ad3bd#, 16#c39cf3ff#, 16#e3de2462#, 16#34430420#, 16#64e674c7#, 16#44a45485#,
    16#a56ab54b#, 16#85289509#, 16#f5cfc5ac#, 16#d58d3653#, 16#26721611#, 16#063076d7#,
    16#569546b4#, 16#b75ba77a#, 16#97198738#, 16#f7dfe7fe#, 16#c7bc48c4#, 16#58e56886#,
    16#78a70840#, 16#18612802#, 16#c9ccd9ed#, 16#e98ef9af#, 16#89489969#, 16#a90ab92b#,
    16#4ad47ab7#, 16#6a961a71#, 16#0a503a33#, 16#2a12dbfd#, 16#fbbfeb9e#, 16#9b798b58#,
    16#bb3bab1a#, 16#6ca67c87#, 16#5cc52c22#, 16#3c030c60#, 16#1c41edae#, 16#fd8fcdec#,
    16#ad2abd0b#, 16#8d689d49#, 16#7e976eb6#, 16#5ed54ef4#, 16#2e321e51#, 16#0e70ff9f#,
    16#efbedfdd#, 16#cffcbf1b#, 16#9f598f78#, 16#918881a9#, 16#b1caa1eb#, 16#d10cc12d#,
    16#e16f1080#, 16#00a130c2#, 16#20e35004#, 16#40257046#, 16#83b99398#, 16#a3fbb3da#,
    16#c33dd31c#, 16#e37ff35e#, 16#129022f3#, 16#32d24235#, 16#52146277#, 16#7256b5ea#,
    16#95a88589#, 16#f56ee54f#, 16#d52cc50d#, 16#34e224c3#, 16#04817466#, 16#64475424#,
    16#4405a7db#, 16#b7fa8799#, 16#e75ff77e#, 16#c71dd73c#, 16#26d336f2#, 16#069116b0#,
    16#76764615#, 16#5634d94c#, 16#c96df90e#, 16#e92f99c8#, 16#b98aa9ab#, 16#58444865#,
    16#78066827#, 16#18c008e1#, 16#28a3cb7d#, 16#db5ceb3f#, 16#fb1e8bf9#, 16#9bd8abbb#,
    16#4a755a54#, 16#6a377a16#, 16#0af11ad0#, 16#2ab33a92#, 16#ed0fdd6c#, 16#cd4dbdaa#,
    16#ad8b9de8#, 16#8dc97c26#, 16#5c644c45#, 16#3ca22c83#, 16#1ce00cc1#, 16#ef1fff3e#,
    16#df7caf9b#, 16#bfba8fd9#, 16#9ff86e17#, 16#7e364e55#, 16#2e933eb2#, 16#0ed11ef0#);

   --  expected CRC value is 379E9F06 hex, or 933142278 decimal

   Next_DMA_Interrupt : DMA_Interrupt;

   procedure Panic;

   procedure Panic is
   begin
      loop
         Toggle_LEDs (All_LEDs);
         delay until Clock + Milliseconds (100);
      end loop;
   end Panic;

begin
   delay until Clock + Seconds (1);  --  work-around for LCD issue
   Clear_Screen;
   Initialize_LEDs;

   Enable_Clock (CRC_Unit);

   Reset_CRC (CRC_Unit);

   Update_CRC (CRC_Unit, Input => Data, Output => Checksum_CPU);

   Put_Line ("CRC:" & Checksum_CPU'Img);

   Reset_CRC (CRC_Unit);

   Update_CRC (CRC_Unit, Controller'Access, Stream, Input => Data);

   DMA_IRQ_Handler.Await_Event (Next_DMA_Interrupt);

   if Next_DMA_Interrupt /= Transfer_Complete_Interrupt then
      Panic;  --  forever
   end if;

   Checksum_DMA := CRC_Value (CRC_Unit);

   Put_Line ("CRC:" & Checksum_DMA'Img);

   if Checksum_CPU = Checksum_DMA then
      Turn_On (Green_LED);
   else
      Turn_On (Red_LED);
   end if;

   loop
      delay until Time_Last;
   end loop;
end Demo_CRC;
