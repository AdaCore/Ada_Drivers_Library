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

with Ada.Real_Time;    use Ada.Real_Time;

with STM32.Board;           use STM32.Board;
with HAL.Framebuffer;       use HAL.Framebuffer;
with HAL.Bitmap;            use HAL.Bitmap;

with Screen_Interface;
with Railroad;

package body Driver is

   ----------------
   -- Controller --
   ----------------

   task body Controller is
      Period : constant Time_Span := Milliseconds (60);
      --  arbitrary, but directly affects how fast the trains move
      --  and how quickly the screen responds to touch

      Next_Start : Time := Clock + Seconds (1);

      Current  : Screen_Interface.Touch_State;
      Previous : Screen_Interface.Touch_State;
   begin
      delay until Next_Start;

      Display.Initialize (Portrait, Interrupt);
      Display.Initialize_Layer (1, RGB_565);
      Display.Initialize_Layer (2, ARGB_1555);
      Screen_Interface.Initialize;
      Railroad.Initialize;

      Current  := Screen_Interface.Current_Touch_State;
      Previous := Current;

      loop
         Current := Screen_Interface.Current_Touch_State;

         if Current.Touch_Detected /= Previous.Touch_Detected then
            if Current.Touch_Detected then
               Railroad.Respond_To_Touch (Current.X, Current.Y);
            end if;
            Previous := Current;
         end if;

         Railroad.Step_Simulation;
         Display.Get_Hidden_Buffer (2).Fill (0);
         Railroad.Draw_Layout (Display.Get_Hidden_Buffer (2));
         Display.Update_Layer (2);

         Next_Start := Next_Start + Period;
         delay until Next_Start;
      end loop;
   end Controller;

end Driver;
