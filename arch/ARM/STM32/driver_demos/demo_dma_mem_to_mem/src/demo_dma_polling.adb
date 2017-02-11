------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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

--  The file declares the main procedure for an alternative version of the
--  demonstration. This one uses polling to recognize completion of the DMA
--  transfer.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is an AdaCore-defined routine that is called when
--  an exception is propagated. This specific version is an overriding of the
--  predefined version. We need it in the executable, therefore it must be
--  somewhere in the closure of the context clauses.

with STM32.Board;    use STM32.Board;
with STM32.Device;   use STM32.Device;

with Ada.Real_Time;  use Ada.Real_Time;
with STM32.DMA;      use STM32.DMA;
with STM32.GPIO;     use STM32.GPIO;

with Peripherals;    use Peripherals;

procedure Demo_DMA_Polling is

   type Data is array (1 .. 100) of Integer;

   Source_Block      : constant Data := (others => 42);
   Destination_Block : Data := (others => 0);

   Config : DMA_Stream_Configuration;

   Status : DMA_Error_Code;

begin
   Initialize_LEDs;

   --  just to signal that we are indeed running...
   for K in 1 .. 3 loop
      All_LEDs_On;
      delay until Clock + Milliseconds (200);
      All_LEDs_Off;
      delay until Clock + Milliseconds (200);
   end loop;

   Enable_Clock (Controller);

   Config.Channel                  := Channel_0;
   Config.Direction                := Memory_To_Memory;
   Config.Operation_Mode           := Normal_Mode;  -- non-circular
   Config.Priority                 := Priority_Medium;
   Config.Peripheral_Data_Format   := Words;
   Config.Memory_Data_Format       := Words;
   Config.Increment_Memory_Address := True;

   Configure (Controller, Stream, Config);
   --  note the controller is disabled by the call to Configure

   Start_Transfer
     (Controller,
      Stream,
      Source => Source_Block'Address,
      Destination => Destination_Block'Address,
      Data_Count => Data'Length); -- Integer is same size as Word

   Poll_For_Completion
     (Controller,
      Stream,
      Expected_Level => Full_Transfer,
      Timeout        => Time_Span_Last,
      Result         => Status);

   if Status /= DMA_No_Error then
      --  signal the problem
      loop
         Green_LED.Toggle;
         delay until Clock + Milliseconds (200);
      end loop;
   end if;

   if Source_Block = Destination_Block then
      Turn_On (Green_LED);
   else
      Turn_On (Red_LED);
   end if;

   loop
      null;
   end loop;
end Demo_DMA_Polling;
