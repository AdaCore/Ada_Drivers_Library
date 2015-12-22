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

with STM32.DMA2D.Polling; use STM32.DMA2D;

package body LCD_Std_Out is

   use Bitmapped_Drawing;

   --  We don't make the current font visible to clients because changing it
   --  requires recomputation of the screen layout (eg the char height) and
   --  issuance of commands to the LCD component driver (eg to refill).

   Current_Font : BMP_Font := Default_Font;

   Char_Width  : Natural := BMP_Fonts.Char_Width (Current_Font);
   Char_Height : Natural := BMP_Fonts.Char_Height (Current_Font);

   Max_Width   : Natural := STM32.LCD.Pixel_Width - Char_Width;
   --  The last place on the current "line" on the LCD where a char of the
   --  current font size can be printed

   Max_Height : Natural := STM32.LCD.Pixel_Height - Char_Height;
   --  The last "line" on the LCD where a char of this current font size can be
   --  printed

   Current_Y : Natural := 0;
   --  The current "line" that the text will appear upon. Note this wraps
   --  around to the top of the screen.

   Char_Count : Natural := 0;
   --  The number of characters currently printed on the current line

   procedure Draw_Char (X, Y : Natural; Msg : Character);
   --  Convenience routine for call Drawing.Draw_Char

   procedure Recompute_Screen_Dimensions (Font : BMP_Font);
   --  Determins the max height and width for the specified font, given the
   --  current LCD orientation

   ---------------------------------
   -- Recompute_Screen_Dimensions --
   ---------------------------------

   procedure Recompute_Screen_Dimensions (Font : BMP_Font) is
   begin
      Char_Width  := BMP_Fonts.Char_Width (Font);
      Char_Height := BMP_Fonts.Char_Height (Font);
      Max_Width   := STM32.LCD.Pixel_Width - Char_Width - 1;
      Max_Height  := STM32.LCD.Pixel_Height - Char_Height - 1;
   end Recompute_Screen_Dimensions;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font (To : in BMP_Font) is
   begin
      Current_Font := To;
      Recompute_Screen_Dimensions (Current_Font);
   end Set_Font;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation (To : in STM32.LCD.Orientation_Mode) is
   begin
      STM32.LCD.Set_Orientation (To);
      Recompute_Screen_Dimensions (Current_Font);
      Clear_Screen;
   end Set_Orientation;

   ------------------
   -- Clear_Screen --
   ------------------

   procedure Clear_Screen is
   begin
      DMA2D_Fill (Screen_Buffer, Current_Background_Color);
      Current_Y := 0;
      Char_Count := 0;
   end Clear_Screen;

   ---------
   -- Put --
   ---------

   procedure Put (Msg : String) is
   begin
      for C of Msg loop
         if C = ASCII.LF then
            New_Line;
         else
            Put (C);
         end if;
      end loop;
   end Put;

   ---------------
   -- Draw_Char --
   ---------------

   procedure Draw_Char (X, Y : Natural; Msg : Character) is
   begin
      Bitmapped_Drawing.Draw_Char
        (Screen_Buffer,
         Start      => (X, Y),
         Char       => Msg,
         Font       => Current_Font,
         Foreground => Current_Text_Color,
         Background => Current_Background_Color);
   end Draw_Char;

   ---------
   -- Put --
   ---------

   procedure Put (Msg : Character) is
      X : Natural;
   begin
      if Char_Count * Char_Width > Max_Width then
         --  go to the next line down
         Current_Y := Current_Y + Char_Height;
         if Current_Y > Max_Height then
            Current_Y := 0;
         end if;
         --  and start at beginning of the line
         X := 0;
         Char_Count := 0;
      else
         X := Char_Count * Char_Width;
      end if;
      Draw_Char (X, Current_Y, Msg);
      Char_Count := Char_Count + 1;
   end Put;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Char_Count := 0; -- next char printed will be at the start of a new line
      if Current_Y + Char_Height > Max_Height then
         Current_Y := 0;
      else
         Current_Y := Current_Y + Char_Height;
      end if;
   end New_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Msg : String) is
   begin
      Put (Msg);
      New_Line;
   end Put_Line;

   ---------
   -- Put --
   ---------

   procedure Put (X, Y : Natural; Msg : Character) renames Draw_Char;

   ---------
   -- Put --
   ---------

   procedure Put (X, Y : Natural; Msg : String) is
      Count  : Natural := 0;
      Next_X : Natural;
   begin
      for C of Msg loop
         Next_X := X + Count * Char_Width;
         Draw_Char (Next_X, Y, C);
         Count := Count + 1;
      end loop;
   end Put;

begin
   STM32.LCD.Initialize (STM32.LCD.Pixel_Fmt_ARGB1555);
   STM32.DMA2D.Polling.Initialize;

--     --  The values for the package global state are already initialized to the
--     --  default values. However we do need to clear/fill the screen and set
--     --  the orientation, which Initialize does do, so we call those routines
--     --  explicitly.
--
--     LCD.Set_Orientation (To => Default_Orientation);

   Clear_Screen;

end LCD_Std_Out;
