------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                    Copyright (C) 2015-2016, AdaCore                      --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with STM32.Board; use STM32.Board;
with Bitmapped_Drawing;

package body LCD_Std_Out is

   --  We don't make the current font visible to clients because changing it
   --  requires recomputation of the screen layout (eg the char height) and
   --  issuance of commands to the LCD component driver (eg to refill).

   Current_Font : BMP_Font := Default_Font;

   Char_Width  : Natural := BMP_Fonts.Char_Width (Current_Font);
   Char_Height : Natural := BMP_Fonts.Char_Height (Current_Font);

   Max_Width   : Natural := LCD_Natural_Width - Char_Width;
   --  The last place on the current "line" on the LCD where a char of the
   --  current font size can be printed

   Max_Height : Natural := LCD_Natural_Height - Char_Height;
   --  The last "line" on the LCD where a char of this current font size can be
   --  printed

   Current_Y : Natural := 0;
   --  The current "line" that the text will appear upon. Note this wraps
   --  around to the top of the screen.

   Char_Count : Natural := 0;
   --  The number of characters currently printed on the current line

   Initialized : Boolean := False;

   procedure Draw_Char (X, Y : Natural; Msg : Character);
   --  Convenience routine for call Drawing.Draw_Char

   procedure Recompute_Screen_Dimensions (Font : BMP_Font);
   --  Determins the max height and width for the specified font, given the
   --  current LCD orientation

   procedure Check_Initialized with Inline;
   --  Ensures that the LCD display is initialized and DMA2D
   --  is up and running

   procedure Internal_Put (Msg : String);
   --  Puts a new String in the frame buffer

   procedure Internal_Put (Msg : Character);
   --  Puts a new character in the frame buffer.

   ------------------------
   -- Assert_Initialized --
   ------------------------

   procedure Check_Initialized
   is
   begin
      if Initialized then
         return;
      end if;

      Initialized := True;

      if Display.Initialized then
         --  Ensure we use polling here: LCD_Std_Out may be called from the
         --  Last chance handler, and we don't want unexpected tasks or
         --  protected objects calling an entry not meant for that
         Display.Set_Mode (HAL.Framebuffer.Polling);
      else
         Display.Initialize (Mode => HAL.Framebuffer.Polling);
         Display.Initialize_Layer (1, HAL.Bitmap.RGB_565);
         Clear_Screen;
      end if;
   end Check_Initialized;

   ---------------------------------
   -- Recompute_Screen_Dimensions --
   ---------------------------------

   procedure Recompute_Screen_Dimensions (Font : BMP_Font) is
   begin
      Check_Initialized;
      Char_Width  := BMP_Fonts.Char_Width (Font);
      Char_Height := BMP_Fonts.Char_Height (Font);
      Max_Width   := Display.Get_Width - Char_Width - 1;
      Max_Height  := Display.Get_Height - Char_Height - 1;
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

   procedure Set_Orientation (To : in HAL.Framebuffer.Display_Orientation) is
   begin
      Display.Set_Orientation (To);
      Recompute_Screen_Dimensions (Current_Font);
      Clear_Screen;
   end Set_Orientation;

   ------------------
   -- Clear_Screen --
   ------------------

   procedure Clear_Screen
   is
   begin
      Check_Initialized;
      Display.Get_Hidden_Buffer (1).Fill (Current_Background_Color);
      Current_Y := 0;
      Char_Count := 0;
      Display.Update_Layer (1, True);
   end Clear_Screen;

   ------------------
   -- Internal_Put --
   ------------------

   procedure Internal_Put (Msg : String) is
   begin
      for C of Msg loop
         if C = ASCII.LF then
            New_Line;
         else
            Internal_Put (C);
         end if;
      end loop;
   end Internal_Put;

   ---------
   -- Put --
   ---------

   procedure Put (Msg : String) is
   begin
      Internal_Put (Msg);
      Display.Update_Layer (1, True);
   end Put;

   ---------------
   -- Draw_Char --
   ---------------

   procedure Draw_Char (X, Y : Natural; Msg : Character)
   is
   begin
      Check_Initialized;
      Bitmapped_Drawing.Draw_Char
        (Display.Get_Hidden_Buffer (1),
         Start      => (X, Y),
         Char       => Msg,
         Font       => Current_Font,
         Foreground =>
           HAL.Bitmap.Bitmap_Color_To_Word (Display.Get_Color_Mode (1),
                                            Current_Text_Color),
         Background =>
           HAL.Bitmap.Bitmap_Color_To_Word (Display.Get_Color_Mode (1),
                                            Current_Background_Color));
   end Draw_Char;

   ---------
   -- Put --
   ---------

   procedure Internal_Put (Msg : Character) is
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
   end Internal_Put;

   ---------
   -- Put --
   ---------

   procedure Put (Msg : Character) is
   begin
      Internal_Put (Msg);
      Display.Update_Layer (1, True);
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

   procedure Put (X, Y : Natural; Msg : Character) is
   begin
      Draw_Char (X, Y, Msg);
      Display.Update_Layer (1, True);
   end Put;

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

      Display.Update_Layer (1, True);
   end Put;

end LCD_Std_Out;
