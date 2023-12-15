------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

with Ada.Text_IO;

package body GUI is

   use type GUI_Buttons.Boolean_Array;

   Prev  : GUI_Buttons.Boolean_Array (Buttons'Range) := not State;

   procedure On_Click
     (Button : Button_Kind;
      Update : in out Boolean);

   -----------------
   -- Check_Touch --
   -----------------

   procedure Check_Touch
     (TP     : in out HAL.Touch_Panel.Touch_Panel_Device'Class;
      Update : out Boolean)
   is
      Touch  : constant HAL.Touch_Panel.TP_State := TP.Get_All_Touch_Points;
      Index  : Natural := Buttons'First;
      Min    : Natural := Natural'Last;
   begin
      Update := False;

      if Touch'Length = 0 then
         return;
      end if;

      for J in Buttons'Range loop
         declare
            Center : constant HAL.Bitmap.Point := Buttons (J).Center;
            Dist   : constant Natural :=
              (Center.X - Touch (Touch'First).X) ** 2
                + (Center.Y - Touch (Touch'First).Y)**2;
         begin
            if Min > Dist then
               Min := Dist;
               Index := J;
            end if;
         end;
      end loop;

      if Min < 600 then
         On_Click (Button_Kind'Val (Index), Update);
      end if;
   end Check_Touch;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (LCD   : in out HAL.Bitmap.Bitmap_Buffer'Class;
      Clear : Boolean := False) is
   begin
      if Clear then
         LCD.Set_Source (HAL.Bitmap.Black);
         LCD.Fill;
         Prev := not State;
      end if;

      if State /= Prev then
         GUI_Buttons.Draw (LCD, Buttons, State, Prev);
         Prev := State;
      end if;
   end Draw;

   -----------------
   -- Dump_Screen --
   -----------------

   procedure Dump_Screen (LCD : in out HAL.Bitmap.Bitmap_Buffer'Class) is
      Color : HAL.UInt32;
   begin
      for Y in 0 .. LCD.Height - 1 loop
         for X in 0 .. LCD.Width - 1 loop
            Color := LCD.Pixel ((X, Y));
            Ada.Text_IO.Put_Line (Color'Image);
         end loop;
      end loop;
   end Dump_Screen;

   --------------
   -- On_Click --
   --------------

   procedure On_Click
     (Button : Button_Kind;
      Update : in out Boolean) is
   begin
      case Button is
         when Te | Hu | Pr =>
            State (+Button) := not State (+Button);
         when Fi =>
            null;
         when Te_X1 .. Te_16 =>
            Update := not State (+Button);
            State (+Te_X1 .. +Te_16) := (others => False);
            State (+Button) := True;
         when Hu_X1 .. Hu_16 =>
            Update := not State (+Button);
            State (+Hu_X1 .. +Hu_16) := (others => False);
            State (+Button) := True;
         when Pr_X1 .. Pr_16 =>
            Update := not State (+Button);
            State (+Pr_X1 .. +Pr_16) := (others => False);
            State (+Button) := True;
         when Fi_No .. Fi_16 =>
            Update := not State (+Button);
            State (+Fi_No .. +Fi_16) := (others => False);
            State (+Button) := True;
      end case;
   end On_Click;

end GUI;
