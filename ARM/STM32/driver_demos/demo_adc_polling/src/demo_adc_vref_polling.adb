------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2016, AdaCore                        --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

--  This program demonstrates reading the VRef (reference voltage) value from
--  an ADC unit, using polling.

--  Note that you will likely need to reset the board manually after loading.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with STM32.Board;  use STM32.Board;
with STM32.Device; use STM32.Device;

with HAL;          use HAL;
with STM32.ADC;    use STM32.ADC;
with STM32.GPIO;   use STM32.GPIO;

with LCD_Std_Out;

procedure Demo_ADC_VRef_Polling is

   Raw     : Word;
   VRefInt : Word;

   Successful : Boolean;
   Timed_Out  : exception;

   procedure Print (X, Y : Natural; Value : Word; Suffix : String := "");

   -----------
   -- Print --
   -----------

   procedure Print (X, Y : Natural; Value : Word; Suffix : String := "") is
      Value_Image : constant String := Value'Img;
   begin
      LCD_Std_Out.Put (X, Y, Value_Image (2 .. Value_Image'Last) & Suffix & "   ");
   end Print;

begin
   Initialize_LEDs;

   Enable_Clock (ADC_1);

   Reset_All_ADC_Units;

   Configure_Common_Properties
     (Mode           => Independent,
      Prescalar      => PCLK2_Div_2,
      DMA_Mode       => Disabled,
      Sampling_Delay => Sampling_Delay_5_Cycles);

   Configure_Unit
     (ADC_1,
      Resolution => ADC_Resolution_12_Bits,
      Alignment  => Right_Aligned);

   Configure_Regular_Conversions
     (ADC_1,
      Continuous  => False,
      Trigger     => Software_Triggered,
      Enable_EOC  => True,
      Conversions => (1 => (Channel => VRef_Channel, Sample_Time => Sample_144_Cycles)));

   Enable (ADC_1);

   loop
      Start_Conversion (ADC_1);

      Poll_For_Status (ADC_1, Regular_Channel_Conversion_Complete, Successful);
      if not Successful then
         raise Timed_Out;
      end if;

      Raw := Word (Conversion_Value (ADC_1));
      Print (0, 0, Raw);

      VRefInt := Word ((Float (Raw) / 4096.0) * Float (ADC_Supply_Voltage));

      Print (0, 24, VRefInt, "mv");

      Toggle (Green);
   end loop;
end Demo_ADC_VRef_Polling;
