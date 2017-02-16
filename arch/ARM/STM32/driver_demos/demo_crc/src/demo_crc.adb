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

with Ada.Unchecked_Conversion;
with System;

procedure Demo_CRC is

   Checksum_CPU : UInt32 := 0;
   --  the checksum obtained by calling a routine that uses the CPU to transfer
   --  the memory block to the CRC processor

   Checksum_DMA : UInt32 := 0;
   --  the checksum obtained by calling a routine that uses DMA to transfer the
   --  memory block to the CRC processor

   --  see the STM32Cube_FW_F4_V1.6.0\Projects\ CRC example for data and
   --  expected CRC checksum value

   Section1 : constant Block_32 :=
   (16#00001021#, 16#20423063#, 16#408450A5#, 16#60C670E7#, 16#9129A14A#, 16#B16BC18C#,
    16#D1ADE1CE#, 16#F1EF1231#, 16#32732252#, 16#52B54294#, 16#72F762D6#, 16#93398318#,
    16#A35AD3BD#, 16#C39CF3FF#, 16#E3DE2462#, 16#34430420#, 16#64E674C7#, 16#44A45485#,
    16#A56AB54B#, 16#85289509#, 16#F5CFC5AC#, 16#D58D3653#, 16#26721611#, 16#063076D7#,
    16#569546B4#, 16#B75BA77A#, 16#97198738#, 16#F7DFE7FE#, 16#C7BC48C4#, 16#58E56886#,
    16#78A70840#, 16#18612802#, 16#C9CCD9ED#, 16#E98EF9AF#, 16#89489969#, 16#A90AB92B#,
    16#4AD47AB7#, 16#6A961A71#, 16#0A503A33#, 16#2A12DBFD#, 16#FBBFEB9E#, 16#9B798B58#,
    16#BB3BAB1A#, 16#6CA67C87#, 16#5CC52C22#, 16#3C030C60#, 16#1C41EDAE#, 16#FD8FCDEC#,
    16#AD2ABD0B#, 16#8D689D49#, 16#7E976EB6#, 16#5ED54EF4#, 16#2E321E51#, 16#0E70FF9F#);

   Section2 : constant Block_32 :=
   (16#EFBEDFDD#, 16#CFFCBF1B#, 16#9F598F78#, 16#918881A9#, 16#B1CAA1EB#, 16#D10CC12D#,
    16#E16F1080#, 16#00A130C2#, 16#20E35004#, 16#40257046#, 16#83B99398#, 16#A3FBB3DA#,
    16#C33DD31C#, 16#E37FF35E#, 16#129022F3#, 16#32D24235#, 16#52146277#, 16#7256B5EA#,
    16#95A88589#, 16#F56EE54F#, 16#D52CC50D#, 16#34E224C3#, 16#04817466#, 16#64475424#,
    16#4405A7DB#, 16#B7FA8799#, 16#E75FF77E#, 16#C71DD73C#, 16#26D336F2#, 16#069116B0#,
    16#76764615#, 16#5634D94C#, 16#C96DF90E#, 16#E92F99C8#, 16#B98AA9AB#, 16#58444865#,
    16#78066827#, 16#18C008E1#, 16#28A3CB7D#, 16#DB5CEB3F#, 16#FB1E8BF9#, 16#9BD8ABBB#,
    16#4A755A54#, 16#6A377A16#, 16#0AF11AD0#, 16#2AB33A92#, 16#ED0FDD6C#, 16#CD4DBDAA#,
    16#AD8B9DE8#, 16#8DC97C26#, 16#5C644C45#, 16#3CA22C83#, 16#1CE00CC1#, 16#EF1FFF3E#,
    16#DF7CAF9B#, 16#BFBA8FD9#, 16#9FF86E17#, 16#7E364E55#, 16#2E933EB2#, 16#0ED11EF0#);

   --  expected CRC value for the data above is 379E9F06 hex, or 933142278 dec
   Expected_Checksum : constant UInt32 := 933142278;

   Next_DMA_Interrupt : DMA_Interrupt;

   procedure Panic with No_Return;
   --  flash the on-board LEDs, indefinitely, to indicate a failure

   procedure Panic is
   begin
      loop
         Toggle_LEDs (All_LEDs);
         delay until Clock + Milliseconds (100);
      end loop;
   end Panic;

begin
   Clear_Screen;
   Initialize_LEDs;

   Enable_Clock (CRC_Unit);

   --  get the checksum using the CPU to transfer memory to the CRC processor;
   --  verify it is the expected value

   Update_CRC (CRC_Unit, Input => Section1, Output => Checksum_CPU);
   Update_CRC (CRC_Unit, Input => Section2, Output => Checksum_CPU);

   Put_Line ("CRC:" & Checksum_CPU'Img);

   if Checksum_CPU /= Expected_Checksum then
      Panic;
   end if;

   --  get the checksum using DMA to transfer memory to the CRC processor

   Enable_Clock (Controller);

   Reset (Controller);

   Reset_Calculator (CRC_Unit);

   Update_CRC (CRC_Unit, Controller'Access, Stream, Input => Section1);

   DMA_IRQ_Handler.Await_Event (Next_DMA_Interrupt);

   if Next_DMA_Interrupt /= Transfer_Complete_Interrupt then
      Panic;
   end if;

   --  In this code fragment we use the approach suited for the case in which
   --  we are calculating the CRC for a section of system memory rather than a
   --  block of application data. We pretend that Section2 is a memory section.
   --  All we need is a known starting address and a known length. Given that,
   --  we can create a view of it as if it is an object of type Block_32 (or
   --  whichever block type is appropriate).
   declare
      subtype Memory_Section is Block_32 (1 .. Section2'Length);
      type Section_Pointer is access all Memory_Section with Storage_Size => 0;
      function As_Memory_Section_Reference is new Ada.Unchecked_Conversion
        (Source   => System.Address, Target => Section_Pointer);
   begin
      Update_CRC
        (CRC_Unit,
         Controller'Access,
         Stream,
         Input => As_Memory_Section_Reference (Section2'Address).all);
   end;

   DMA_IRQ_Handler.Await_Event (Next_DMA_Interrupt);

   if Next_DMA_Interrupt /= Transfer_Complete_Interrupt then
      Panic;
   end if;

   Checksum_DMA := Value (CRC_Unit);

   Put_Line ("CRC:" & Checksum_DMA'Img);

   --  verify the two checksums are identical (one of which is already verified
   --  as the expected value)

   if Checksum_CPU = Checksum_DMA then
      Turn_On (Green_LED);
   else
      Turn_On (Red_LED);
   end if;

   loop
      delay until Time_Last;
   end loop;
end Demo_CRC;
