------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

--  A very simple draw application.
--  Use your finger to draw pixels.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with STM32.Board;           use STM32.Board;
with HAL.Bitmap;            use HAL.Bitmap;
with HAL.Touch_Panel;       use HAL.Touch_Panel;
with STM32.Button;

with Bitmapped_Drawing;     use Bitmapped_Drawing;

procedure Draw
is
   procedure Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Display.Get_Hidden_Buffer (1).Fill ((Alpha => 255, others => 64));
      Display.Update_Layer (1, Copy_Back => True);
   end Clear;

   Last_X : Integer := -1;
   Last_Y : Integer := -1;

begin
   --  Initialize LCD
   Display.Initialize;
   Display.Initialize_Layer (1, ARGB_8888);

   --  Initialize touch panel
   Touch_Panel.Initialize;

   --  Initialize button
   STM32.Button.Initialize;

   --  Clear LCD (set background)
   Clear;

   --  The application: set pixel where the finger is (so that you
   --  cannot see what you are drawing).
   loop
      if STM32.Button.Has_Been_Pressed then
         Clear;
      end if;

      declare
         State : constant TP_State := Touch_Panel.Get_All_Touch_Points;
      begin
         if State'Length = 0 then
            Last_X := -1;
            Last_Y := -1;

         elsif State'Length = 1 then
            --  Lines can be drawn between two consecutive points only when
            --  one touch point is active: the order of the touch data is not
            --  necessarily preserved by the hardware.
            if Last_X > 0 then
               Draw_Line
                 (Display.Get_Hidden_Buffer (1),
                  Start     => (Last_X, Last_Y),
                  Stop      => (State (State'First).X, State (State'First).Y),
                  Hue       => HAL.Bitmap.Green,
                  Thickness => State (State'First).Weight / 2,
                  Fast      => False);
            end if;

            Last_X := State (State'First).X;
            Last_Y := State (State'First).Y;

         else
            Last_X := -1;
            Last_Y := -1;
         end if;

         for Id in State'Range loop
            Fill_Circle
              (Display.Get_Hidden_Buffer (1),
               Center => (State (Id).X, State (Id).Y),
               Radius => State (Id).Weight / 4,
               Hue    => HAL.Bitmap.Green);
         end loop;

         if State'Length > 0 then
            Display.Update_Layer (1, Copy_Back => True);
         end if;
      end;
   end loop;
end Draw;
