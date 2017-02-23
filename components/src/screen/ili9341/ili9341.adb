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

with Ada.Unchecked_Conversion;

with ILI9341_Regs; use ILI9341_Regs;

package body ILI9341 is

   function As_UInt16 is new Ada.Unchecked_Conversion
     (Source => Colors, Target => UInt16);

   -------------------
   -- Current_Width --
   -------------------

   function Current_Width (This : ILI9341_Device) return Natural is
      (This.Selected_Width);

   --------------------
   -- Current_Height --
   --------------------

   function Current_Height (This : ILI9341_Device) return Natural is
     (This.Selected_Height);

   -------------------------
   -- Current_Orientation --
   -------------------------

   function Current_Orientation (This : ILI9341_Device) return Orientations is
      (This.Selected_Orientation);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : in out ILI9341_Device;
      Mode : ILI9341_Mode)
   is
   begin
      if This.Initialized then
         return;
      end if;

      This.Chip_Select_High;

      This.Init_LCD (Mode);

      This.Selected_Width := Device_Width;
      This.Selected_Height := Device_Height;
      This.Selected_Orientation := Portrait_2;

      This.Initialized := True;
   end Initialize;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel
     (This  : in out ILI9341_Device;
      X     : Width;
      Y     : Height;
      Color : Colors)
   is
      Color_High_UInt8 : constant UInt8 :=
        UInt8 (Shift_Right (As_UInt16 (Color), 8));
      Color_Low_UInt8  : constant UInt8 :=
        UInt8 (As_UInt16 (Color) and 16#FF#);
   begin
      This.Set_Cursor_Position (X, Y, X, Y);
      This.Send_Command (ILI9341_GRAM);
      This.Send_Data (Color_High_UInt8);
      This.Send_Data (Color_Low_UInt8);
   end Set_Pixel;

   ----------
   -- Fill --
   ----------

   procedure Fill (This : in out ILI9341_Device; Color : Colors) is
      Color_High_UInt8 : constant UInt8 :=
        UInt8 (Shift_Right (As_UInt16 (Color), 8));
      Color_Low_UInt8  : constant UInt8 :=
        UInt8 (As_UInt16 (Color) and 16#FF#);
   begin
      This.Set_Cursor_Position (X1 => 0,
                                Y1 => 0,
                                X2 => This.Selected_Width - 1,
                                Y2 => This.Selected_Height - 1);

      This.Send_Command (ILI9341_GRAM);
      for N in 1 .. (Device_Width * Device_Height) loop
         This.Send_Data (Color_High_UInt8);
         This.Send_Data (Color_Low_UInt8);
      end loop;
   end Fill;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation (This : in out ILI9341_Device;
                              To   : Orientations)
   is
   begin
      This.Send_Command (ILI9341_MAC);
      case To is
         when Portrait_1  => This.Send_Data (16#58#);
         when Portrait_2  => This.Send_Data (16#88#);
         when Landscape_1 => This.Send_Data (16#28#);
         when Landscape_2 => This.Send_Data (16#E8#);
      end case;

      case To is
         when Portrait_1 | Portrait_2 =>
            This.Selected_Width  := Device_Width;
            This.Selected_Height := Device_Height;
         when Landscape_1 | Landscape_2 =>
            This.Selected_Width  := Device_Height;
            This.Selected_Height := Device_Width;
      end case;

      This.Selected_Orientation := To;
   end Set_Orientation;

   --------------------
   -- Enable_Display --
   --------------------

   procedure Enable_Display (This : in out ILI9341_Device) is
   begin
      This.Send_Command (ILI9341_DISPLAY_ON);
   end Enable_Display;

   ---------------------
   -- Disable_Display --
   ---------------------

   procedure Disable_Display (This : in out ILI9341_Device) is
   begin
      This.Send_Command (ILI9341_DISPLAY_OFF);
   end Disable_Display;

   -------------------------
   -- Set_Cursor_Position --
   -------------------------

   procedure Set_Cursor_Position
     (This : in out ILI9341_Device;
      X1   : Width;
      Y1   : Height;
      X2   : Width;
      Y2   : Height)
   is
      X1_High : constant UInt8 := UInt8 (Shift_Right (UInt16 (X1), 8));
      X1_Low  : constant UInt8 := UInt8 (UInt16 (X1) and 16#FF#);
      X2_High : constant UInt8 := UInt8 (Shift_Right (UInt16 (X2), 8));
      X2_Low  : constant UInt8 := UInt8 (UInt16 (X2) and 16#FF#);

      Y1_High : constant UInt8 := UInt8 (Shift_Right (UInt16 (Y1), 8));
      Y1_Low  : constant UInt8 := UInt8 (UInt16 (Y1) and 16#FF#);
      Y2_High : constant UInt8 := UInt8 (Shift_Right (UInt16 (Y2), 8));
      Y2_Low  : constant UInt8 := UInt8 (UInt16 (Y2) and 16#FF#);
   begin
      This.Send_Command (ILI9341_COLUMN_ADDR);
      This.Send_Data (X1_High);
      This.Send_Data (X1_Low);
      This.Send_Data (X2_High);
      This.Send_Data (X2_Low);

      This.Send_Command (ILI9341_PAGE_ADDR);
      This.Send_Data (Y1_High);
      This.Send_Data (Y1_Low);
      This.Send_Data (Y2_High);
      This.Send_Data (Y2_Low);
   end Set_Cursor_Position;

   ----------------------
   -- Chip_Select_High --
   ----------------------

   procedure Chip_Select_High (This : in out ILI9341_Device) is
   begin
      This.Chip_Select.Set;
   end Chip_Select_High;

   ---------------------
   -- Chip_Select_Low --
   ---------------------

   procedure Chip_Select_Low (This : in out ILI9341_Device) is
   begin
      This.Chip_Select.Clear;
   end Chip_Select_Low;

   ---------------
   -- Send_Data --
   ---------------

   procedure Send_Data (This : in out ILI9341_Device; Data : UInt8) is
      Status : SPI_Status;
   begin
      This.WRX.Set;
      This.Chip_Select_Low;
      This.Port.Transmit (SPI_Data_8b'(1 => Data), Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
      This.Chip_Select_High;
   end Send_Data;

   ------------------
   -- Send_Command --
   ------------------

   procedure Send_Command (This : in out ILI9341_Device; Cmd : UInt8) is
      Status : SPI_Status;
   begin
      This.WRX.Clear;
      This.Chip_Select_Low;
      This.Port.Transmit (SPI_Data_8b'(1 => Cmd), Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
      This.Chip_Select_High;
   end Send_Command;

   --------------
   -- Init_LCD --
   --------------

   procedure Init_LCD (This : in out ILI9341_Device;
                       Mode : ILI9341_Mode) is
   begin
      This.Reset.Set;
      This.Send_Command (ILI9341_RESET);
      This.Time.Delay_Milliseconds (5);

      This.Send_Command (ILI9341_POWERA);
      This.Send_Data (16#39#);
      This.Send_Data (16#2C#);
      This.Send_Data (16#00#);
      This.Send_Data (16#34#);
      This.Send_Data (16#02#);
      This.Send_Command (ILI9341_POWERB);
      This.Send_Data (16#00#);
      This.Send_Data (16#C1#);
      This.Send_Data (16#30#);
      This.Send_Command (ILI9341_DTCA);
      This.Send_Data (16#85#);
      This.Send_Data (16#00#);
      This.Send_Data (16#78#);
      This.Send_Command (ILI9341_DTCB);
      This.Send_Data (16#00#);
      This.Send_Data (16#00#);
      This.Send_Command (ILI9341_POWER_SEQ);
      This.Send_Data (16#64#);
      This.Send_Data (16#03#);
      This.Send_Data (16#12#);
      This.Send_Data (16#81#);
      This.Send_Command (ILI9341_PRC);
      This.Send_Data (16#20#);
      This.Send_Command (ILI9341_POWER1);
      This.Send_Data (16#23#);
      This.Send_Command (ILI9341_POWER2);
      This.Send_Data (16#10#);
      This.Send_Command (ILI9341_VCOM1);
      This.Send_Data (16#3E#);
      This.Send_Data (16#28#);
      This.Send_Command (ILI9341_VCOM2);
      This.Send_Data (16#86#);
      This.Send_Command (ILI9341_MAC);
      This.Send_Data (16#C8#);
      This.Send_Command (ILI9341_FRC);
      This.Send_Data (16#00#);
      This.Send_Data (16#18#);
      case Mode is
         when RGB_Mode =>
            This.Send_Command (ILI9341_RGB_INTERFACE);
            This.Send_Data (16#C2#);
            This.Send_Command (ILI9341_INTERFACE);
            This.Send_Data (16#01#);
            This.Send_Data (16#00#);
            This.Send_Data (16#06#);
            This.Send_Command (ILI9341_DFC);
            This.Send_Data (16#0A#);
            This.Send_Data (16#A7#);
            This.Send_Data (16#27#);
            This.Send_Data (16#04#);
         when SPI_Mode =>
            This.Send_Command (ILI9341_PIXEL_FORMAT);
            This.Send_Data (16#55#);
            This.Send_Command (ILI9341_DFC);
            This.Send_Data (16#08#);
            This.Send_Data (16#82#);
            This.Send_Data (16#27#);
      end case;
      This.Send_Command (ILI9341_3GAMMA_EN);
      This.Send_Data (16#00#);
      This.Send_Command (ILI9341_COLUMN_ADDR);
      This.Send_Data (16#00#);
      This.Send_Data (16#00#);
      This.Send_Data (16#00#);
      This.Send_Data (16#EF#);
      This.Send_Command (ILI9341_PAGE_ADDR);
      This.Send_Data (16#00#);
      This.Send_Data (16#00#);
      This.Send_Data (16#01#);
      This.Send_Data (16#3F#);
      This.Send_Command (ILI9341_GAMMA);
      This.Send_Data (16#01#);
      This.Send_Command (ILI9341_PGAMMA);
      This.Send_Data (16#0F#);
      This.Send_Data (16#31#);
      This.Send_Data (16#2B#);
      This.Send_Data (16#0C#);
      This.Send_Data (16#0E#);
      This.Send_Data (16#08#);
      This.Send_Data (16#4E#);
      This.Send_Data (16#F1#);
      This.Send_Data (16#37#);
      This.Send_Data (16#07#);
      This.Send_Data (16#10#);
      This.Send_Data (16#03#);
      This.Send_Data (16#0E#);
      This.Send_Data (16#09#);
      This.Send_Data (16#00#);
      This.Send_Command (ILI9341_NGAMMA);
      This.Send_Data (16#00#);
      This.Send_Data (16#0E#);
      This.Send_Data (16#14#);
      This.Send_Data (16#03#);
      This.Send_Data (16#11#);
      This.Send_Data (16#07#);
      This.Send_Data (16#31#);
      This.Send_Data (16#C1#);
      This.Send_Data (16#48#);
      This.Send_Data (16#08#);
      This.Send_Data (16#0F#);
      This.Send_Data (16#0C#);
      This.Send_Data (16#31#);
      This.Send_Data (16#36#);
      This.Send_Data (16#0F#);
      This.Send_Command (ILI9341_SLEEP_OUT);

      case Mode is
         when RGB_Mode =>
            This.Time.Delay_Milliseconds (150);
         when SPI_Mode =>
            This.Time.Delay_Milliseconds (20);
      end case;
      --  document ILI9341_DS_V1.02, section 11.2, pg 205 says we need
      --  either 120ms or 5ms, depending on the mode, but seems incorrect.

      This.Send_Command (ILI9341_DISPLAY_ON);
      This.Send_Command (ILI9341_GRAM);
   end Init_LCD;

end ILI9341;
