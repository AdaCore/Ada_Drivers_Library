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

--  This program demonstrates some basic functionality of the ILI9341 LCD
--  component driver. Note that much higher level facilities are available.
--  This program simply demonstrates the basic setup requirements for the
--  device itself.

with ILI9341;      use ILI9341;
with STM32.Device; use STM32.Device;
with STM32.SPI;    use STM32.SPI;
with HAL.SPI;
with STM32.GPIO;   use STM32.GPIO;

procedure Demo_ILI9341 is

   LCD_SPI     : SPI_Port renames SPI_5;
   LCD_CSX     : STM32.GPIO.GPIO_Point renames PC2;
   LCD_WRX_DCX : STM32.GPIO.GPIO_Point renames PD13;
   LCD_RESET   : STM32.GPIO.GPIO_Point renames PD12;

   SPI5_SCK    : GPIO_Point renames PF7;
   SPI5_MISO   : GPIO_Point renames PF8;
   SPI5_MOSI   : GPIO_Point renames PF9;

   LCD : ILI9341_Device
     (STM32.Device.SPI_5'Access,
      Chip_Select => LCD_CSX'Access,
      WRX         => LCD_WRX_DCX'Access,
      Reset       => LCD_RESET'Access);

   procedure Init_SPI;
   procedure Init_LCD_GPIO_Pins;

   --------------
   -- Init_SPI --
   --------------

   procedure Init_SPI is
      Conf     : GPIO_Port_Configuration;
      SPI_Conf : SPI_Configuration;
      SPI_Pins : constant GPIO_Points := (SPI5_SCK, SPI5_MOSI, SPI5_MISO);

   begin
      Enable_Clock (SPI_Pins);
      Enable_Clock (LCD_SPI);

      Conf.Speed       := Speed_100MHz;
      Conf.Mode        := Mode_AF;
      Conf.Output_Type := Push_Pull;
      Conf.Resistors   := Floating;

      Configure_Alternate_Function (SPI_Pins, GPIO_AF_SPI5);
      Configure_IO (SPI_Pins, Conf);

      Reset (LCD_SPI);

      SPI_Conf :=
        (Direction           => D2Lines_FullDuplex,
         Mode                => Master,
         Data_Size           => HAL.SPI.Data_Size_8b,
         Clock_Polarity      => Low,
         Clock_Phase         => P1Edge,
         Slave_Management    => Software_Managed,
         Baud_Rate_Prescaler => BRP_32,
         First_Bit           => MSB,
         CRC_Poly            => 7);

      Configure (LCD_SPI, SPI_Conf);

      STM32.SPI.Enable (LCD_SPI);
   end Init_SPI;

   ------------------------
   -- Init_LCD_GPIO_Pins --
   ------------------------

   procedure Init_LCD_GPIO_Pins is
   begin
      Enable_Clock (GPIO_Points'(LCD_CSX, LCD_WRX_DCX));

      Configure_IO
        (Points => (LCD_CSX, LCD_WRX_DCX, LCD_RESET),
         Config => (Speed       => Speed_50MHz,
                    Mode        => Mode_Out,
                    Output_Type => Push_Pull,
                    Resistors   => Floating));
   end Init_LCD_GPIO_Pins;

begin
   Init_LCD_GPIO_Pins;
   Set (LCD_CSX);  -- deselect the chip
   Init_SPI;

   Initialize (LCD, Mode => ILI9341.SPI_Mode);

   Set_Orientation (LCD, To => Portrait_2);

   Fill (LCD, Color => White);

   --  draw an arbitrary line
   for K in 1 .. 200 loop
      Set_Pixel (LCD, X => 10, Y => K, Color => Black);
   end loop;

   loop
      null;
   end loop;
end Demo_ILI9341;
