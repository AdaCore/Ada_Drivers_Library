------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
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

with Ada.Real_Time;

with HAL.Bitmap;

with STM32.Board;
with Display_ILI9341;

procedure Main is
   use type Ada.Real_Time.Time;

   Next : Ada.Real_Time.Time := Ada.Real_Time.Clock;

   Bitmap : Display_ILI9341.Bitmap_Buffer renames STM32.Board.TFT_Bitmap;

begin
   --  STM32.Board.Initialize_LEDs;
   STM32.Board.Display.Initialize;

   Bitmap.Set_Source (HAL.Bitmap.Black);
   Bitmap.Fill;

   for X in 0 .. 31 loop
      for Y in 0 .. 31 loop
         Bitmap.Set_Source
           ((Alpha => 255,
             Green  => 128,
             Red    => HAL.UInt8 (X * 8),
             Blue   => HAL.UInt8 (Y * 8)));

         Bitmap.Fill_Rect (((X * 7, Y * 10), 6, 9));
      end loop;
   end loop;

   loop
      --  STM32.Board.Toggle (STM32.Board.D1_LED);

      Next := Next + Ada.Real_Time.Milliseconds (500);
      delay until Next;
   end loop;
end Main;
