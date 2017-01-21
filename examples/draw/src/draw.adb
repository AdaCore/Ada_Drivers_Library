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

--  A very simple draw application.
--  Use your finger to draw pixels.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with STM32.Board;           use STM32.Board;
with HAL.Bitmap;            use HAL.Bitmap;
with HAL.Touch_Panel;       use HAL.Touch_Panel;
with STM32.User_Button;     use STM32;

procedure Draw
is
   procedure Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Display.Hidden_Buffer (1).Fill ((Alpha => 255, others => 64));
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
   User_Button.Initialize;

   --  Clear LCD (set background)
   Clear;

   --  The application: set pixel where the finger is (so that you
   --  cannot see what you are drawing).
   loop
      if User_Button.Has_Been_Pressed then
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
                 (Display.Hidden_Buffer (1).all,
                  Color     => HAL.Bitmap.Green,
                  Start     => (Last_X, Last_Y),
                  Stop      => (State (State'First).X, State (State'First).Y),
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
              (Display.Hidden_Buffer (1).all,
               Color => HAL.Bitmap.Green,
               Center => (State (Id).X, State (Id).Y),
               Radius => State (Id).Weight / 4);
         end loop;

         if State'Length > 0 then
            Display.Update_Layer (1, Copy_Back => True);
         end if;
      end;
   end loop;
end Draw;
