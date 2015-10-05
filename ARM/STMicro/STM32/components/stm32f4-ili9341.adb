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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    ili9341.c                                                      --
--   @author  MCD Application Team                                          --
--   @version V1.0.2                                                        --
--   @date    02-December-2014                                              --
--   @brief   This file provides a set of functions needed to manage the    --
--            L3GD20, ST MEMS motion sensor,  3-axis digital output         --
--            gyroscope.                                                    --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Real_Time;  use Ada.Real_Time;
with Ada.Unchecked_Conversion;

package body STM32F4.ILI9341 is

   procedure Init_LCD;

   procedure Chip_Select_High with Inline;
   procedure Chip_Select_Low  with Inline;

   procedure Init_SPI;

   function As_Half_Word is new Ada.Unchecked_Conversion
     (Source => Colors, Target => Half_Word);

   -------------------
   -- Current_Width --
   -------------------

   function Current_Width return Natural is
      (Selected_Width);

   --------------------
   -- Current_Height --
   --------------------

   function Current_Height return Natural is
     (Selected_Height);

   -------------------------
   -- Current_Orientation --
   -------------------------

   function Current_Orientation return Orientations is
      (Selected_Orientation);

   --------------
   -- Init_SPI --
   --------------

   procedure Init_SPI is
      SPI_Conf  : SPI_Configuration;
   begin
      SPI.Disable (SPI_Chip.all);

      SPI_Conf.Direction           := D2Lines_FullDuplex;
      SPI_Conf.Mode                := Master;
      SPI_Conf.Data_Size           := Data_8;
      SPI_Conf.Clock_Polarity      := Low;
      SPI_Conf.Clock_Phase         := P1Edge;
      SPI_Conf.Slave_Management    := Software_Managed;
      SPI_Conf.Baud_Rate_Prescaler := BRP_16;
      SPI_Conf.First_Bit           := MSB;
      SPI_Conf.CRC_Poly            := 7;

      SPI.Configure (SPI_Chip.all, SPI_Conf);

      SPI.Enable (SPI_Chip.all);
   end Init_SPI;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Chip_Select             : GPIO_Point;
      Enable_CS_GPIO_Clock    : not null access procedure;
      WRX                     : GPIO_Point;
      Enable_WRX_GPIO_Clock   : not null access procedure;
      Reset                   : GPIO_Point;
      Enable_Reset_GPIO_Clock : not null access procedure;
      SPI_Chip                : access SPI_Port;
      Enable_SPI_Clock        : not null access procedure;
      SPI_GPIO                : access GPIO_Port;
      Enable_SPI_GPIO_Clock   : not null access procedure;
      SPI_AF                  : GPIO_Alternate_Function;
      SCK_Pin                 : GPIO_Pin;
      MISO_Pin                : GPIO_Pin;
      MOSI_Pin                : GPIO_Pin)
   is
      Config : GPIO_Port_Configuration;
   begin
      ILI9341.Chip_Select := Chip_Select;
      ILI9341.WRX := WRX;
      ILI9341.Reset := Reset;
      ILI9341.SPI_Chip := SPI_Chip;

      Config := (Mode => Mode_Out, Output_Type => Push_Pull,
                 Resistors => Floating, Speed => Speed_25MHz, Locked => False);

      Enable_WRX_GPIO_Clock.all;
      Configure_IO (WRX.Port.all, WRX.Pin, Config);

      Enable_CS_GPIO_Clock.all;
      Configure_IO (Chip_Select.Port.all, Chip_Select.Pin, Config);

      Enable_Reset_GPIO_Clock.all;
      Config.Speed := Speed_2MHz;  -- low
      Configure_IO (Reset.Port.all, Reset.Pin, Config);

      Chip_Select_High;

      Enable_SPI_GPIO_Clock.all;
      Config.Speed := Speed_100MHz; -- high
      Config.Mode := Mode_AF;
      Configure_IO (SPI_GPIO.all, SCK_Pin & MISO_Pin & MOSI_Pin, Config);
      Configure_Alternate_Function (SPI_GPIO.all, SCK_Pin & MISO_Pin & MOSI_Pin, SPI_AF);

      Enable_SPI_Clock.all;
      Init_SPI;

      Init_LCD;

      Selected_Width := Device_Width;
      Selected_Height := Device_Height;
      Selected_Orientation := Portrait_2;
    end Initialize;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (X : Width; Y : Height; Color : Colors) is
      Color_High_Byte : constant Byte := Byte (Shift_Right (As_Half_Word (Color), 8));
      Color_Low_Byte  : constant Byte := Byte (As_Half_Word (Color) and 16#FF#);
   begin
      Set_Cursor_Position (X, Y, X, Y);
      Send_Command (ILI9341_GRAM);
      Send_Data (Color_High_Byte);
      Send_Data (Color_Low_Byte);
   end Set_Pixel;

   ----------
   -- Fill --
   ----------

   procedure Fill (Color : Colors) is
      Color_High_Byte : constant Byte := Byte (Shift_Right (As_Half_Word (Color), 8));
      Color_Low_Byte  : constant Byte := Byte (As_Half_Word (Color) and 16#FF#);
   begin
      Set_Cursor_Position (X1 => 0,
                           Y1 => 0,
                           X2 => Selected_Width - 1,
                           Y2 => Selected_Height - 1);

      Send_Command (ILI9341_GRAM);
      for N in 1 .. (Device_Width * Device_Height) loop
         Send_Data (Color_High_Byte);
         Send_Data (Color_Low_Byte);
      end loop;
   end Fill;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation (To : Orientations) is
   begin
      Send_Command (ILI9341_MAC);
      case To is
         when Portrait_1  => Send_Data (16#58#);
         when Portrait_2  => Send_Data (16#88#);
         when Landscape_1 => Send_Data (16#28#);
         when Landscape_2 => Send_Data (16#E8#);
      end case;

      case To is
         when Portrait_1 | Portrait_2 =>
            Selected_Width  := Device_Width;
            Selected_Height := Device_Height;
         when Landscape_1 | Landscape_2 =>
            Selected_Width  := Device_Height;
            Selected_Height := Device_Width;
      end case;

      Selected_Orientation := To;
   end Set_Orientation;

   --------------------
   -- Enable_Display --
   --------------------

   procedure Enable_Display is
   begin
      Send_Command (ILI9341_DISPLAY_ON);
   end Enable_Display;

   ---------------------
   -- Disable_Display --
   ---------------------

   procedure Disable_Display is
   begin
      Send_Command (ILI9341_DISPLAY_OFF);
   end Disable_Display;

   -------------------------
   -- Set_Cursor_Position --
   -------------------------

   procedure Set_Cursor_Position
     (X1 : Width;
      Y1 : Height;
      X2 : Width;
      Y2 : Height)
   is
      X1_High : constant Byte := Byte (Shift_Right (Half_Word (X1), 8));
      X1_Low  : constant Byte := Byte (Half_Word (X1) and 16#FF#);
      X2_High : constant Byte := Byte (Shift_Right (Half_Word (X2), 8));
      X2_Low  : constant Byte := Byte (Half_Word (X2) and 16#FF#);

      Y1_High : constant Byte := Byte (Shift_Right (Half_Word (Y1), 8));
      Y1_Low  : constant Byte := Byte (Half_Word (Y1) and 16#FF#);
      Y2_High : constant Byte := Byte (Shift_Right (Half_Word (Y2), 8));
      Y2_Low  : constant Byte := Byte (Half_Word (Y2) and 16#FF#);
   begin
      Send_Command (ILI9341_COLUMN_ADDR);
      Send_Data (X1_High);
      Send_Data (X1_Low);
      Send_Data (X2_High);
      Send_Data (X2_Low);

      Send_Command (ILI9341_PAGE_ADDR);
      Send_Data (Y1_High);
      Send_Data (Y1_Low);
      Send_Data (Y2_High);
      Send_Data (Y2_Low);
   end Set_Cursor_Position;

   ----------------------
   -- Chip_Select_High --
   ----------------------

   procedure Chip_Select_High is
   begin
      GPIO.Set (Chip_Select.Port.all, Chip_Select.Pin);
   end Chip_Select_High;

   ---------------------
   -- Chip_Select_Low --
   ---------------------

   procedure Chip_Select_Low is
   begin
      GPIO.Clear (Chip_Select.Port.all, Chip_Select.Pin);
   end Chip_Select_Low;

   ---------------
   -- Send_Data --
   ---------------

   procedure Send_Data (Data : Byte) is
   begin
      GPIO.Set (WRX.Port.all, WRX.Pin);
      Chip_Select_Low;
      SPI.Transmit (SPI_Chip.all, Data);
      Chip_Select_High;
   end Send_Data;

   ------------------
   -- Send_Command --
   ------------------

   procedure Send_Command (Cmd : Byte) is
   begin
      GPIO.Clear (WRX.Port.all, WRX.Pin);
      Chip_Select_Low;
      SPI.Transmit (SPI_Chip.all, Cmd);
      Chip_Select_High;
   end Send_Command;

   --------------
   -- Init_LCD --
   --------------

   procedure Init_LCD is
   begin
      GPIO.Set (Reset.Port.all, Reset.Pin);
      Send_Command (ILI9341_RESET);
      delay until Clock + Milliseconds (5);  -- per document ILI9341_DS_V1.02

      Send_Command (ILI9341_POWERA);
      Send_Data (16#39#);
      Send_Data (16#2C#);
      Send_Data (16#00#);
      Send_Data (16#34#);
      Send_Data (16#02#);
      Send_Command (ILI9341_POWERB);
      Send_Data (16#00#);
      Send_Data (16#C1#);
      Send_Data (16#30#);
      Send_Command (ILI9341_DTCA);
      Send_Data (16#85#);
      Send_Data (16#00#);
      Send_Data (16#78#);
      Send_Command (ILI9341_DTCB);
      Send_Data (16#00#);
      Send_Data (16#00#);
      Send_Command (ILI9341_POWER_SEQ);
      Send_Data (16#64#);
      Send_Data (16#03#);
      Send_Data (16#12#);
      Send_Data (16#81#);
      Send_Command (ILI9341_PRC);
      Send_Data (16#20#);
      Send_Command (ILI9341_POWER1);
      Send_Data (16#23#);
      Send_Command (ILI9341_POWER2);
      Send_Data (16#10#);
      Send_Command (ILI9341_VCOM1);
      Send_Data (16#3E#);
      Send_Data (16#28#);
      Send_Command (ILI9341_VCOM2);
      Send_Data (16#86#);
      Send_Command (ILI9341_MAC);
      Send_Data (16#48#);
      Send_Command (ILI9341_PIXEL_FORMAT);
      Send_Data (16#55#);
      Send_Command (ILI9341_FRC);
      Send_Data (16#00#);
      Send_Data (16#18#);
      Send_Command (ILI9341_DFC);
      Send_Data (16#08#);
      Send_Data (16#82#);
      Send_Data (16#27#);
      Send_Command (ILI9341_3GAMMA_EN);
      Send_Data (16#00#);
      Send_Command (ILI9341_COLUMN_ADDR);
      Send_Data (16#00#);
      Send_Data (16#00#);
      Send_Data (16#00#);
      Send_Data (16#EF#);
      Send_Command (ILI9341_PAGE_ADDR);
      Send_Data (16#00#);
      Send_Data (16#00#);
      Send_Data (16#01#);
      Send_Data (16#3F#);
      Send_Command (ILI9341_GAMMA);
      Send_Data (16#01#);
      Send_Command (ILI9341_PGAMMA);
      Send_Data (16#0F#);
      Send_Data (16#31#);
      Send_Data (16#2B#);
      Send_Data (16#0C#);
      Send_Data (16#0E#);
      Send_Data (16#08#);
      Send_Data (16#4E#);
      Send_Data (16#F1#);
      Send_Data (16#37#);
      Send_Data (16#07#);
      Send_Data (16#10#);
      Send_Data (16#03#);
      Send_Data (16#0E#);
      Send_Data (16#09#);
      Send_Data (16#00#);
      Send_Command (ILI9341_NGAMMA);
      Send_Data (16#00#);
      Send_Data (16#0E#);
      Send_Data (16#14#);
      Send_Data (16#03#);
      Send_Data (16#11#);
      Send_Data (16#07#);
      Send_Data (16#31#);
      Send_Data (16#C1#);
      Send_Data (16#48#);
      Send_Data (16#08#);
      Send_Data (16#0F#);
      Send_Data (16#0C#);
      Send_Data (16#31#);
      Send_Data (16#36#);
      Send_Data (16#0F#);
      Send_Command (ILI9341_SLEEP_OUT);

      delay until Clock + Milliseconds (5);  -- per document ILI9341_DS_V1.02

      Send_Command (ILI9341_DISPLAY_ON);
      Send_Command (ILI9341_GRAM);
   end Init_LCD;

end STM32F4.ILI9341;
