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

--  This program demonstrates basic DAC use, using explicit calls to control
--  the output value and conversion starting/stopping.

--  The user increments the percentage of the DAC output (VRef+) voltage
--  using the blue User button on the STM32F429 Discovery board. The current
--  percentage is displayed on the LCD. When incremented, the new percentage
--  is converted into an absolute value based on the selected resolution. For
--  example, a digital value of 2048 represents 50% in 12-bit resolution.

--  Attach a voltmeter to PA4 (or PA5 if channel 2 is used) to see the voltage
--  changes.

with Last_Chance_Handler;      pragma Unreferenced (Last_Chance_Handler);

with Interfaces;   use Interfaces;

with STM32_Board;  use STM32_Board;
with STM32.Device; use STM32.Device;

with STM32;        use STM32;
with STM32.DAC;    use STM32.DAC;
with STM32.GPIO;   use STM32.GPIO;

with Bitmapped_Drawing;
with BMP_Fonts;
with STM32.ILI9341;

procedure Demo_DAC_Basic is

   Output_Channel : constant DAC_Channel := Channel_1;  -- arbitrary

   procedure Configure_DAC_GPIO (Output_Channel : DAC_Channel);
   --  Once the channel is enabled, the corresponding GPIO pin is automatically
   --  connected to the analog converter output. However, in order to avoid
   --  parasitic consumption, the PA4 pin (Channel_1) or PA5 pin (Channel_2)
   --  should first be configured to analog mode. See the note in the RM, page
   --  431.

   -----------------
   -- LCD_Drawing --
   -----------------

   package LCD_Drawing is new Bitmapped_Drawing
     (Color     => STM32.ILI9341.Colors,
      Set_Pixel => STM32.ILI9341.Set_Pixel);

   ---------
   -- Put --
   ---------

   procedure Put (X, Y : Natural; Msg : String) is
      use LCD_Drawing, BMP_Fonts, STM32.ILI9341;
   begin
      Draw_String
        ((X, Y),
         Msg,
         Font       => Font16x24,
         Foreground => White,
         Background => Black);
   end Put;

   -----------
   -- Print --
   -----------

   procedure Print (Value : Word) is
      Value_Image : constant String := Value'Img;
   begin
      Put (170, 52, Value_Image (2 .. Value_Image'Last) & "   ");
   end Print;

   ------------------
   -- Await_Button --
   ------------------

   procedure Await_Button is
   begin
      Await_Pressed : loop
         exit when Set (User_Button_Point);
      end loop Await_Pressed;

      Await_Released : loop
         exit when not Set (User_Button_Point);
      end loop Await_Released;
   end Await_Button;

   ------------------------
   -- Configure_DAC_GPIO --
   ------------------------

   procedure Configure_DAC_GPIO (Output_Channel : DAC_Channel) is
      Output : constant GPIO_Point := (if Output_Channel = Channel_1
                                       then DAC_Channel_1_IO
                                       else DAC_Channel_2_IO);
      Config : GPIO_Port_Configuration;
   begin
      Enable_Clock (Output.Port.all);
      Config.Mode := Mode_Analog;
      Config.Resistors := Floating;
      Configure_IO (Output, Config);
   end Configure_DAC_GPIO;


begin
   Initialize_LEDs;
   All_LEDs_Off;
   Configure_User_Button_GPIO;

   Configure_DAC_GPIO (Output_Channel);

   Enable_Clock (DAC_1);

   Reset (DAC_1);

   Select_Trigger (DAC_1, Output_Channel, Software_Trigger);

   Enable_Trigger (DAC_1, Output_Channel);

   Enable (DAC_1, Output_Channel);

   declare
      Value   : Word := 0;
      Percent : Word;

      Resolution : constant DAC_Resolution := DAC_Resolution_12_Bits;
      -- Arbitrary, change as desired.  All else will automatically adjust.

      Max_Counts : constant Word := (if Resolution = DAC_Resolution_12_Bits
                                     then Max_12bit_Resolution
                                     else Max_8bit_Resolution);
   begin
      Put (0, 0, "VRef+ is 3.3V"); -- but you may not see that voltage!
      Put (0, 25, "Button advances");
      Put (0, 52, "Current %:");
      loop
         for K in Word range 0 .. 10 loop
            Percent := K * 10;
            Print (Percent);

            Value := (Percent * Max_Counts) / 100;

            Set_Output
              (DAC_1,
               Output_Channel,
               Value,
               Resolution,
               Right_Aligned);

            Trigger_Conversion_By_Software (DAC_1, Output_Channel);

            Await_Button;
         end loop;
      end loop;
   end;
end Demo_DAC_Basic;
