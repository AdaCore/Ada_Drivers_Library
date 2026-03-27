------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2026, AdaCore                             --
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

--  Simple test/example for STM32F429disco with Bmp280 sensor attached

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with Ada.Exceptions;

with HAL.SPI;
with STM32.Board;    use STM32.Board;
with STM32.Device;
with STM32.GPIO;     use STM32.GPIO;
with STM32.SPI;      use STM32.SPI;

with BMP280;         use BMP280;
with Watchdogs;      use Watchdogs;

--  Service
with Ada.Real_Time;  use Ada.Real_Time;
with HAL.Bitmap;     use HAL.Bitmap;
with BMP_Fonts;
with LCD_Std_Out;

--   We use the following pins on STM32F429Discovery:
--   CSN PC12 CSB
--   IRQ PB4
--     SPI4 Pins:
--   PE2  SPI4_SCK
--   PE5  SPI4_MISO SDO
--   PE6  SPI4_MOSI SDA

procedure Bmp280_Example is

   CSN       : GPIO_Point renames STM32.Device.PC12;
   SPI4_Pins : constant GPIO_Points :=
     (STM32.Device.PE2, STM32.Device.PE5, STM32.Device.PE6);
   SPI       : SPI_Port renames STM32.Device.SPI_4;
   Device    : BMP280_Pressure_Sensor;
   Error     : Boolean;

   procedure Init_Pins;
   --  Initialize pins

   procedure Init_SPI;
   --  Initialize SPI

   procedure Print (Msg : String);
   --  Print the message on display

   procedure On_Error (Msg : String);
   --  Output the error message and blink the red led

   ---------------
   -- Init_Pins --
   ---------------

   procedure Init_Pins is
      --  Initialize pins
   begin
      STM32.Device.Enable_Clock (GPIO_Points'(CSN, IRQ));

      Configure_IO
        (CSN,
         Config =>
           (Mode        => Mode_Out,
            Resistors   => Floating,
            Output_Type => Push_Pull,
            Speed       => Speed_Low));

      CSN.Set;
   end Init_Pins;

   --------------
   -- Init_SPI --
   --------------

   procedure Init_SPI
   --  Initialize SPI
   is
      use STM32.Device;

      Conf      : constant GPIO_Port_Configuration :=
        (Mode           => Mode_AF,
         Resistors      => Floating,
         AF_Output_Type => Push_Pull,
         AF_Speed       => Speed_Very_High,
         AF             => GPIO_AF_SPI4_5);

      SPI_Conf  : constant SPI_Configuration :=
        (Direction           => D2Lines_FullDuplex,
         Mode                => Master,
         Data_Size           => HAL.SPI.Data_Size_8b,
         Clock_Polarity      => Low,
         Clock_Phase         => P1Edge,
         Slave_Management    => Software_Managed,
         Baud_Rate_Prescaler => BRP_16,
         First_Bit           => MSB,
         CRC_Poly            => 10);
   begin
      Enable_Clock (SPI4_Pins);
      Enable_Clock (SPI);

      Configure_IO (SPI4_Pins, Conf);

      Reset (SPI);
      if not SPI.Enabled then
         SPI.Configure (SPI_Conf);
         SPI.Enable;
      end if;
   end Init_SPI;

   Period       : constant Time_Span := Seconds (2);
   Next_Release : Time := Clock;
   BG           : constant Bitmap_Color := (Alpha => 255, others => 64);

   -----------
   -- Print --
   -----------

   procedure Print (Msg : String) is
   begin
      LCD_Std_Out.Put_Line (Msg);
      Display.Update_Layer (1, Copy_Back => True);
   end Print;

   --------------
   -- On_Error --
   --------------

   procedure On_Error (Msg : String) is
   begin
      LCD_Std_Out.Put_Line (Msg);
      Display.Update_Layer (1, Copy_Back => True);

      loop
         STM32.Board.Toggle (Red_LED);
         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end On_Error;

begin
   delay 2.0;

   --  Initialize GPIO/SPI
   Init_Pins;
   Init_SPI;
   delay 0.1;

   --  Init display infrastructure
   STM32.Board.Initialize_LEDs;
   Display.Initialize;
   Display.Initialize_Layer (1, ARGB_8888);
   LCD_Std_Out.Set_Font (BMP_Fonts.Font12x12);
   LCD_Std_Out.Current_Background_Color := BG;
   Display.Hidden_Buffer (1).Set_Source (BG);
   Display.Hidden_Buffer (1).Fill;
   LCD_Std_Out.Clear_Screen;

   --  test --
   Initialize (Device, CSN'Access, SPI'Access, Error);
   if Error then
      On_Error ("Initialization failed");

   else
      --  Initialization is OK
      Print ("Chip_ID :" & Get_Chip_ID (Device)'Img);
      Print ("All tests passed");
      delay 3.0;

      --  adjust the sensor settings
      Set_Filter (Device, 7);
      Set_Temp_Precision (Device, Low_Power);
      Set_Press_Precision (Device, Ultra_High_Resolution);
      Set_Power_Mode (Device, Normal);
      Set_Time_Standby (Device, 62.5);
      Set_SPI_Mode (Device, SPI4W);

      loop
         --  display the temperature and pressure in the loop
         STM32.Board.Toggle (Green_LED);
         Next_Release := Next_Release + Period;
         delay until Next_Release;
         LCD_Std_Out.Clear_Screen;

         LCD_Std_Out.Put_Line ("T :" & Get_Temperature (Device)'Img);
         LCD_Std_Out.Put_Line ("P :" & Get_Pressure (Device)'Img);
         Display.Update_Layer (1, Copy_Back => True);
      end loop;
   end if;

exception
   when E : others =>
      LCD_Std_Out.Put_Line (Ada.Exceptions.Exception_Information (E));
end Bmp280_Example;
