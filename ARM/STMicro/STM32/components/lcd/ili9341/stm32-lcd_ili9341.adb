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

package body STM32.LCD_ILI9341 is

   LCD_SPI : SPI_Port renames SPI_5;

   --  LCD Registers
   LCD_SLEEP_OUT     : constant := 16#11#;  --  Sleep out register
   LCD_GAMMA         : constant := 16#26#;  --  Gamma register
--     LCD_DISPLAY_OFF   : constant := 16#28#;  --  Display off register
   LCD_DISPLAY_ON    : constant := 16#29#;  --  Display on register
   LCD_COLUMN_ADDR   : constant := 16#2A#;  --  Colomn address register
   LCD_PAGE_ADDR     : constant := 16#2B#;  --  Page address register
   LCD_GRAM          : constant := 16#2C#;  --  GRAM register
   LCD_MAC           : constant := 16#36#;  --  Memory Access Control register
--     LCD_PIXEL_FORMAT  : constant := 16#3A#;  --  Pixel Format register
--     LCD_WDB           : constant := 16#51#;  --  Write Brightness Display register
--     LCD_WCD           : constant := 16#53#;  --  Write Control Display register
   LCD_RGB_INTERFACE : constant := 16#B0#;  --  RGB Interface Signal Control
   LCD_FRC           : constant := 16#B1#;  --  Frame Rate Control register
--     LCD_BPC           : constant := 16#B5#;  --  Blanking Porch Control register
   LCD_DFC           : constant := 16#B6#;  --  Display Function Control register
   LCD_POWER1        : constant := 16#C0#;  --  Power Control 1 register
   LCD_POWER2        : constant := 16#C1#;  --  Power Control 2 register
   LCD_VCOM1         : constant := 16#C5#;  --  VCOM Control 1 register
   LCD_VCOM2         : constant := 16#C7#;  --  VCOM Control 2 register
   LCD_POWERA        : constant := 16#CB#;  --  Power control A register
   LCD_POWERB        : constant := 16#CF#;  --  Power control B register
   LCD_PGAMMA        : constant := 16#E0#;  --  Positive Gamma Correction register
   LCD_NGAMMA        : constant := 16#E1#;  --  Negative Gamma Correction register
   LCD_DTCA          : constant := 16#E8#;  --  Driver timing control A
   LCD_DTCB          : constant := 16#EA#;  --  Driver timing control B
   LCD_POWER_SEQ     : constant := 16#ED#;  --  Power on sequence register
   LCD_3GAMMA_EN     : constant := 16#F2#;  --  3 Gamma enable register
   LCD_INTERFACE     : constant := 16#F6#;  --  Interface control register
   LCD_PRC           : constant := 16#F7#;  --  Pump ratio control register

   --------------
   -- My_Delay --
   --------------

   procedure My_Delay (Ms : Integer) is
   begin
      delay until Clock + Milliseconds (Ms);
   end My_Delay;

   -----------------
   -- Chip_Select --
   -----------------

   procedure Chip_Select (Enabled : Boolean) is
   begin
      if Enabled then
         Clear (LCD_CSX);
      else
         Set (LCD_CSX);
      end if;
   end Chip_Select;

   --------------
   -- Wait_SPI --
   --------------

   procedure Wait_SPI is
   begin
      while Is_Busy (LCD_SPI) loop
         null;
      end loop;
   end Wait_SPI;

   ----------------------
   -- LCD_WriteCommand --
   ----------------------

   procedure LCD_WriteCommand (Cmd : Short) is
   begin
      Wait_SPI;
      Clear (LCD_WRX_DCX);

      Chip_Select (true);

      Send (LCD_SPI, Cmd);

      Wait_SPI;

      Chip_Select (false);
   end LCD_WriteCommand;

   -------------------
   -- LCD_WriteData --
   -------------------

   procedure LCD_WriteData (Cmd : Short) is
   begin
      Wait_SPI;

      Set (LCD_WRX_DCX);

      Chip_Select (true);

      Send (LCD_SPI, Cmd);

      Wait_SPI;

      Chip_Select (false);
   end LCD_WriteData;

   ------------------
   -- LCD_SPI_Init --
   ------------------

   procedure LCD_SPI_Init
   is
      Conf     : GPIO_Port_Configuration;
      SPI_Conf : SPI_Configuration;
      SPI_Pins : constant GPIO_Points :=
                   (SPI5_SCK, SPI5_MOSI, SPI5_MISO);

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

      if not Enabled (LCD_SPI) then
         SPI_Conf :=
           (Direction           => D2Lines_FullDuplex,
            Mode                => Master,
            Data_Size           => Data_8,
            Clock_Polarity      => Low,
            Clock_Phase         => P1Edge,
            Slave_Management    => Software_Managed,
            Baud_Rate_Prescaler => BRP_32,
            First_Bit           => MSB,
            CRC_Poly            => 7);
         Configure (LCD_SPI, SPI_Conf);
         STM32.SPI.Enable (LCD_SPI);
      end if;
   end LCD_SPI_Init;

   -------------------
   -- LCD_Pins_Init --
   -------------------

   procedure LCD_Pins_Init is
   begin
      Enable_CLock (GPIO_Points'(LCD_CSX, LCD_WRX_DCX));
      Enable_Clock (LCD_PINS);

      Configure_IO
        (Points => (LCD_CSX, LCD_WRX_DCX),
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

      Chip_Select (False);
   end LCD_Pins_Init;

   -----------------
   -- LCD_PowerOn --
   -----------------

   procedure LCD_For_LTDC_Init is
   begin
      LCD_WriteCommand (16#CA#);
      LCD_WriteData (16#C3#);
      LCD_WriteData (16#08#);
      LCD_WriteData (16#50#);
      LCD_WriteCommand (LCD_POWERB);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#C1#);
      LCD_WriteData (16#30#);
      LCD_WriteCommand (LCD_POWER_SEQ);
      LCD_WriteData (16#64#);
      LCD_WriteData (16#03#);
      LCD_WriteData (16#12#);
      LCD_WriteData (16#81#);
      LCD_WriteCommand (LCD_DTCA);
      LCD_WriteData (16#85#);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#78#);
      LCD_WriteCommand (LCD_POWERA);
      LCD_WriteData (16#39#);
      LCD_WriteData (16#2C#);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#34#);
      LCD_WriteData (16#02#);
      LCD_WriteCommand (LCD_PRC);
      LCD_WriteData (16#20#);
      LCD_WriteCommand (LCD_DTCB);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#00#);
      LCD_WriteCommand (LCD_FRC);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#1B#);
      LCD_WriteCommand (LCD_DFC);
      LCD_WriteData (16#0A#);
      LCD_WriteData (16#A2#);
      LCD_WriteCommand (LCD_POWER1);
      LCD_WriteData (16#10#);
      LCD_WriteCommand (LCD_POWER2);
      LCD_WriteData (16#10#);
      LCD_WriteCommand (LCD_VCOM1);
      LCD_WriteData (16#45#);
      LCD_WriteData (16#15#);
      LCD_WriteCommand (LCD_VCOM2);
      LCD_WriteData (16#90#);
      LCD_WriteCommand (LCD_MAC);
      LCD_WriteData (16#C8#);
      LCD_WriteCommand (LCD_3GAMMA_EN);
      LCD_WriteData (16#00#);
      LCD_WriteCommand (LCD_RGB_INTERFACE);
      LCD_WriteData (16#C2#);
      LCD_WriteCommand (LCD_DFC);
      LCD_WriteData (16#0A#);
      LCD_WriteData (16#A7#);
      LCD_WriteData (16#27#);
      LCD_WriteData (16#04#);

      --  colomn address set
      LCD_WriteCommand (LCD_COLUMN_ADDR);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#EF#);
      --  Page Address Set
      LCD_WriteCommand (LCD_PAGE_ADDR);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#01#);
      LCD_WriteData (16#3F#);
      LCD_WriteCommand (LCD_INTERFACE);
      LCD_WriteData (16#01#);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#06#);

      LCD_WriteCommand (LCD_GRAM);
      My_Delay (200);

      LCD_WriteCommand (LCD_GAMMA);
      LCD_WriteData (16#01#);

      LCD_WriteCommand (LCD_PGAMMA);
      LCD_WriteData (16#0F#);
      LCD_WriteData (16#29#);
      LCD_WriteData (16#24#);
      LCD_WriteData (16#0C#);
      LCD_WriteData (16#0E#);
      LCD_WriteData (16#09#);
      LCD_WriteData (16#4E#);
      LCD_WriteData (16#78#);
      LCD_WriteData (16#3C#);
      LCD_WriteData (16#09#);
      LCD_WriteData (16#13#);
      LCD_WriteData (16#05#);
      LCD_WriteData (16#17#);
      LCD_WriteData (16#11#);
      LCD_WriteData (16#00#);
      LCD_WriteCommand (LCD_NGAMMA);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#16#);
      LCD_WriteData (16#1B#);
      LCD_WriteData (16#04#);
      LCD_WriteData (16#11#);
      LCD_WriteData (16#07#);
      LCD_WriteData (16#31#);
      LCD_WriteData (16#33#);
      LCD_WriteData (16#42#);
      LCD_WriteData (16#05#);
      LCD_WriteData (16#0C#);
      LCD_WriteData (16#0A#);
      LCD_WriteData (16#28#);
      LCD_WriteData (16#2F#);
      LCD_WriteData (16#0F#);

      LCD_WriteCommand (LCD_SLEEP_OUT);
      LCD_WriteCommand (LCD_DISPLAY_ON);
      My_Delay (200);
      --  GRAM start writing
      LCD_WriteCommand (LCD_GRAM);
   end LCD_For_LTDC_Init;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
   begin
      LCD_Pins_Init;
      Chip_Select (True);
      LCD_SPI_Init;
      LCD_For_LTDC_Init;
   end Initialize;

end STM32.LCD_ILI9341;
