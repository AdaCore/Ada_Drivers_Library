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

with Ada.Real_Time; use Ada.Real_Time;

with STM32.Board;   use STM32.Board;
with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;
with STM32.SPI;     use STM32.SPI;
with ILI9341;       use ILI9341;

with HAL.SPI;

package body LCD_ILI9341 is

   LCD_SPI : SPI_Port renames SPI_5;

   ILI9341_Dev : ILI9341_Device
     (Port        => LCD_SPI'Access,
      Chip_Select => LCD_CSX'Access,
      WRX         => LCD_WRX_DCX'Access,
      Reset       => LCD_RESET'Access);

   --  LCD Registers
   LCD_SLEEP_OUT     : constant := 16#11#;  --  Sleep out
   LCD_GAMMA         : constant := 16#26#;  --  Gamma
--     LCD_DISPLAY_OFF   : constant := 16#28#;  --  Display off
   LCD_DISPLAY_ON    : constant := 16#29#;  --  Display on
   LCD_COLUMN_ADDR   : constant := 16#2A#;  --  Colomn address
   LCD_PAGE_ADDR     : constant := 16#2B#;  --  Page address
   LCD_GRAM          : constant := 16#2C#;  --  GRAM
   LCD_MAC           : constant := 16#36#;  --  Memory Access Control
--     LCD_PIXEL_FORMAT  : constant := 16#3A#;  --  Pixel Format
--     LCD_WDB           : constant := 16#51#;  --  Write Brightness Display
--     LCD_WCD           : constant := 16#53#;  --  Write Control Display
   LCD_RGB_INTERFACE : constant := 16#B0#;  --  RGB Interface Signal Control
   LCD_FRC           : constant := 16#B1#;  --  Frame Rate Control
--     LCD_BPC           : constant := 16#B5#;  --  Blanking Porch Control
   LCD_DFC           : constant := 16#B6#;  --  Display Function Control
   LCD_POWER1        : constant := 16#C0#;  --  Power Control 1
   LCD_POWER2        : constant := 16#C1#;  --  Power Control 2
   LCD_VCOM1         : constant := 16#C5#;  --  VCOM Control 1
   LCD_VCOM2         : constant := 16#C7#;  --  VCOM Control 2
   LCD_POWERA        : constant := 16#CB#;  --  Power control A
   LCD_POWERB        : constant := 16#CF#;  --  Power control B
   LCD_PGAMMA        : constant := 16#E0#;  --  Positive Gamma Correction
   LCD_NGAMMA        : constant := 16#E1#;  --  Negative Gamma Correction
   LCD_DTCA          : constant := 16#E8#;  --  Driver timing control A
   LCD_DTCB          : constant := 16#EA#;  --  Driver timing control B
   LCD_POWER_SEQ     : constant := 16#ED#;  --  Power on sequence
   LCD_3GAMMA_EN     : constant := 16#F2#;  --  3 Gamma enable
   LCD_INTERFACE     : constant := 16#F6#;  --  Interface control
   LCD_PRC           : constant := 16#F7#;  --  Pump ratio control

   procedure My_Delay (Ms : Integer);
   procedure LCD_SPI_Init;
   procedure LCD_Pins_Init;
   procedure LCD_For_LTDC_Init;

   --------------
   -- My_Delay --
   --------------

   procedure My_Delay (Ms : Integer) is
   begin
      delay until Clock + Milliseconds (Ms);
   end My_Delay;

   ------------------
   -- LCD_SPI_Init --
   ------------------

   procedure LCD_SPI_Init
   is
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

      if not LCD_SPI.Enabled then
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
         LCD_SPI.Configure (SPI_Conf);
         LCD_SPI.Enable;
      end if;
   end LCD_SPI_Init;

   -------------------
   -- LCD_Pins_Init --
   -------------------

   procedure LCD_Pins_Init is
      Points : constant GPIO_Points :=
        GPIO_Points'(LCD_CSX, LCD_WRX_DCX);
   begin
      Enable_Clock (Points);
      Enable_Clock (LCD_PINS);

      Configure_IO
        (Points => Points,
         Config => (Speed       => Speed_50MHz,
                    Mode        => Mode_Out,
                    Output_Type => Push_Pull,
                    Resistors   => Floating));
--        Lock (Points => (LCD_CSX, LCD_WRX_DCX));

      Configure_Alternate_Function (LCD_PINS, GPIO_AF_LTDC);
      Configure_Alternate_Function (LCD_RGB_AF9, GPIO_AF_LTDC_2);
      Configure_IO
        (Points => LCD_PINS,
         Config => (Speed       => Speed_50MHz,
                    Mode        => Mode_AF,
                    Output_Type => Push_Pull,
                    Resistors   => Floating));
--        Lock (LCD_PINS);
   end LCD_Pins_Init;

   -----------------
   -- LCD_PowerOn --
   -----------------

   procedure LCD_For_LTDC_Init is
   begin
      ILI9341_Dev.Send_Command (16#CA#);
      ILI9341_Dev.Send_Data (16#C3#);
      ILI9341_Dev.Send_Data (16#08#);
      ILI9341_Dev.Send_Data (16#50#);
      ILI9341_Dev.Send_Command (LCD_POWERB);
      ILI9341_Dev.Send_Data (16#00#);
      ILI9341_Dev.Send_Data (16#C1#);
      ILI9341_Dev.Send_Data (16#30#);
      ILI9341_Dev.Send_Command (LCD_POWER_SEQ);
      ILI9341_Dev.Send_Data (16#64#);
      ILI9341_Dev.Send_Data (16#03#);
      ILI9341_Dev.Send_Data (16#12#);
      ILI9341_Dev.Send_Data (16#81#);
      ILI9341_Dev.Send_Command (LCD_DTCA);
      ILI9341_Dev.Send_Data (16#85#);
      ILI9341_Dev.Send_Data (16#00#);
      ILI9341_Dev.Send_Data (16#78#);
      ILI9341_Dev.Send_Command (LCD_POWERA);
      ILI9341_Dev.Send_Data (16#39#);
      ILI9341_Dev.Send_Data (16#2C#);
      ILI9341_Dev.Send_Data (16#00#);
      ILI9341_Dev.Send_Data (16#34#);
      ILI9341_Dev.Send_Data (16#02#);
      ILI9341_Dev.Send_Command (LCD_PRC);
      ILI9341_Dev.Send_Data (16#20#);
      ILI9341_Dev.Send_Command (LCD_DTCB);
      ILI9341_Dev.Send_Data (16#00#);
      ILI9341_Dev.Send_Data (16#00#);
      ILI9341_Dev.Send_Command (LCD_FRC);
      ILI9341_Dev.Send_Data (16#00#);
      ILI9341_Dev.Send_Data (16#1B#);
      ILI9341_Dev.Send_Command (LCD_DFC);
      ILI9341_Dev.Send_Data (16#0A#);
      ILI9341_Dev.Send_Data (16#A2#);
      ILI9341_Dev.Send_Command (LCD_POWER1);
      ILI9341_Dev.Send_Data (16#10#);
      ILI9341_Dev.Send_Command (LCD_POWER2);
      ILI9341_Dev.Send_Data (16#10#);
      ILI9341_Dev.Send_Command (LCD_VCOM1);
      ILI9341_Dev.Send_Data (16#45#);
      ILI9341_Dev.Send_Data (16#15#);
      ILI9341_Dev.Send_Command (LCD_VCOM2);
      ILI9341_Dev.Send_Data (16#90#);
      ILI9341_Dev.Send_Command (LCD_MAC);
      ILI9341_Dev.Send_Data (16#C8#);
      ILI9341_Dev.Send_Command (LCD_3GAMMA_EN);
      ILI9341_Dev.Send_Data (16#00#);
      ILI9341_Dev.Send_Command (LCD_RGB_INTERFACE);
      ILI9341_Dev.Send_Data (16#C2#);
      ILI9341_Dev.Send_Command (LCD_DFC);
      ILI9341_Dev.Send_Data (16#0A#);
      ILI9341_Dev.Send_Data (16#A7#);
      ILI9341_Dev.Send_Data (16#27#);
      ILI9341_Dev.Send_Data (16#04#);

      --  colomn address set
      ILI9341_Dev.Send_Command (LCD_COLUMN_ADDR);
      ILI9341_Dev.Send_Data (16#00#);
      ILI9341_Dev.Send_Data (16#00#);
      ILI9341_Dev.Send_Data (16#00#);
      ILI9341_Dev.Send_Data (16#EF#);
      --  Page Address Set
      ILI9341_Dev.Send_Command (LCD_PAGE_ADDR);
      ILI9341_Dev.Send_Data (16#00#);
      ILI9341_Dev.Send_Data (16#00#);
      ILI9341_Dev.Send_Data (16#01#);
      ILI9341_Dev.Send_Data (16#3F#);
      ILI9341_Dev.Send_Command (LCD_INTERFACE);
      ILI9341_Dev.Send_Data (16#01#);
      ILI9341_Dev.Send_Data (16#00#);
      ILI9341_Dev.Send_Data (16#06#);

      ILI9341_Dev.Send_Command (LCD_GRAM);
      My_Delay (200);

      ILI9341_Dev.Send_Command (LCD_GAMMA);
      ILI9341_Dev.Send_Data (16#01#);

      ILI9341_Dev.Send_Command (LCD_PGAMMA);
      ILI9341_Dev.Send_Data (16#0F#);
      ILI9341_Dev.Send_Data (16#29#);
      ILI9341_Dev.Send_Data (16#24#);
      ILI9341_Dev.Send_Data (16#0C#);
      ILI9341_Dev.Send_Data (16#0E#);
      ILI9341_Dev.Send_Data (16#09#);
      ILI9341_Dev.Send_Data (16#4E#);
      ILI9341_Dev.Send_Data (16#78#);
      ILI9341_Dev.Send_Data (16#3C#);
      ILI9341_Dev.Send_Data (16#09#);
      ILI9341_Dev.Send_Data (16#13#);
      ILI9341_Dev.Send_Data (16#05#);
      ILI9341_Dev.Send_Data (16#17#);
      ILI9341_Dev.Send_Data (16#11#);
      ILI9341_Dev.Send_Data (16#00#);
      ILI9341_Dev.Send_Command (LCD_NGAMMA);
      ILI9341_Dev.Send_Data (16#00#);
      ILI9341_Dev.Send_Data (16#16#);
      ILI9341_Dev.Send_Data (16#1B#);
      ILI9341_Dev.Send_Data (16#04#);
      ILI9341_Dev.Send_Data (16#11#);
      ILI9341_Dev.Send_Data (16#07#);
      ILI9341_Dev.Send_Data (16#31#);
      ILI9341_Dev.Send_Data (16#33#);
      ILI9341_Dev.Send_Data (16#42#);
      ILI9341_Dev.Send_Data (16#05#);
      ILI9341_Dev.Send_Data (16#0C#);
      ILI9341_Dev.Send_Data (16#0A#);
      ILI9341_Dev.Send_Data (16#28#);
      ILI9341_Dev.Send_Data (16#2F#);
      ILI9341_Dev.Send_Data (16#0F#);

      ILI9341_Dev.Send_Command (LCD_SLEEP_OUT);
      ILI9341_Dev.Send_Command (LCD_DISPLAY_ON);
      My_Delay (200);
      --  GRAM start writing
      ILI9341_Dev.Send_Command (LCD_GRAM);
   end LCD_For_LTDC_Init;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
   begin
      LCD_Pins_Init;
      LCD_SPI_Init;

      ILI9341_Dev.Initialize;
      LCD_For_LTDC_Init;
   end Initialize;

end LCD_ILI9341;
