------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2023, AdaCore                     --
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

--  A very simple draw application.
--  Use your finger to draw pixels.

with HAL.Bitmap;            use HAL.Bitmap;
with HAL.Touch_Panel;
with STM32.Board;           use STM32.Board;
with STM32.User_Button;

with Bitmapped_Drawing;
with BMP_Fonts;
with Display_ILI9341;

procedure Draw is

   BG : constant Bitmap_Color := (Alpha => 255, others => 64);

   procedure Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear is

      procedure Put_Line (Text : String);

      Y : Natural := 10;

      procedure Put_Line (Text : String) is
      begin
         Bitmapped_Drawing.Draw_String
           (TFT_Bitmap, (5, Y), Text, BMP_Fonts.Font8x8, Green, BG);

         Y := Y + 10;
      end Put_Line;
   begin
      TFT_Bitmap.Set_Source (BG);
      TFT_Bitmap.Fill;
      TFT_Bitmap.Set_Source (White);

      Put_Line ("Touch the screen to draw or");
      Put_Line ("press the K-UP button for");
      Put_Line ("a demo of drawing primitives.");
   end Clear;

   Last_X : Integer := -1;
   Last_Y : Integer := -1;

   type Mode is (Drawing_Mode, Bitmap_Showcase_Mode);

   Current_Mode : Mode := Drawing_Mode;

begin
   --  Initialize LCD
   Display.Initialize;

   --  Initialize touch panel
   Touch_Panel.Initialize;

   --  Initialize button
   STM32.User_Button.Initialize;

   --  Clear LCD (set background)
   Clear;

   --  The application: set pixel where the finger is (so that you
   --  cannot see what you are drawing).
   loop
      if STM32.User_Button.Has_Been_Pressed then
         case Current_Mode is
            when Drawing_Mode =>
               Current_Mode := Bitmap_Showcase_Mode;
            when Bitmap_Showcase_Mode =>
               Clear;
               Current_Mode := Drawing_Mode;
         end case;
      end if;

      if Current_Mode = Drawing_Mode then

         TFT_Bitmap.Set_Source (HAL.Bitmap.Green);

         declare
            State : constant HAL.Touch_Panel.TP_State :=
              Touch_Panel.Get_All_Touch_Points;
         begin
            if State'Length /= 1 then
               Last_X := -1;
               Last_Y := -1;

            else
               --  Lines can be drawn between two consecutive points only when
               --  one touch point is active: the order of the touch data is not
               --  necessarily preserved by the hardware.
               if Last_X > 0 then
                  TFT_Bitmap.Draw_Line
                    (Start     => (Last_X, Last_Y),
                     Stop      => (State (State'First).X, State (State'First).Y),
                     Thickness => State (State'First).Weight,
                     Fast      => False);
               end if;

               Last_X := State (State'First).X;
               Last_Y := State (State'First).Y;
            end if;

            for Id in State'Range loop
               TFT_Bitmap.Fill_Circle
                 (Center => (State (Id).X, State (Id).Y),
                  Radius => State (Id).Weight);
            end loop;
         end;
      else

         --  Show some of the supported drawing primitives

         TFT_Bitmap.Set_Source (Black);
         TFT_Bitmap.Fill;

         TFT_Bitmap.Set_Source (Green);
         TFT_Bitmap.Fill_Rounded_Rect
           (((10, 10), 100, 100), 20);

         TFT_Bitmap.Set_Source (HAL.Bitmap.Red);
         TFT_Bitmap.Draw_Rounded_Rect
           (((10, 10), 100, 100), 20, Thickness => 4);

         TFT_Bitmap.Set_Source (HAL.Bitmap.Yellow);
         TFT_Bitmap.Fill_Circle ((60, 60), 20);

         TFT_Bitmap.Set_Source (HAL.Bitmap.Blue);
         TFT_Bitmap.Draw_Circle ((60, 60), 20);

         TFT_Bitmap.Set_Source (HAL.Bitmap.Violet);
         TFT_Bitmap.Cubic_Bezier (P1        => (10, 10),
                                                 P2        => (60, 10),
                                                 P3        => (60, 60),
                                                 P4        => (100, 100),
                                                 N         => 200,
                                                 Thickness => 5);

         Display_ILI9341.ILI9341_Bitmap.Copy_Rect
           (Src_Buffer  => TFT_Bitmap,
            Src_Pt      => (0, 0),
            Dst_Buffer  => TFT_Bitmap,
            Dst_Pt      => (100, 100),
            Width       => 100,
            Height      => 100,
            Synchronous => True);
      end if;
   end loop;
end Draw;
