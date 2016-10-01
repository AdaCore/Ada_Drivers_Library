------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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
      Touch_Panel.Initialize (Portrait);
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
