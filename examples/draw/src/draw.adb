------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--             Copyright (C) 2015, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  A very simple draw application.
--  Use your finger to draw pixels.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with STM32.LCD;             use STM32.LCD;
with STM32.DMA2D.Interrupt; use STM32.DMA2D;
with STM32.Touch_Panel;     use STM32.Touch_Panel;
with STM32.Button;

with Bitmapped_Drawing;     use Bitmapped_Drawing;
with Double_Buffer;         use Double_Buffer;

procedure Draw
is
   FG_Buffer    : DMA2D_Buffer;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      DMA2D_Fill (FG_Buffer, Color => (Alpha => 255, others => 64));
   end Clear;

   Red : constant DMA2D_Color := (Alpha => 255,
                                  Red   => 255, Green => 0, Blue => 0);

begin
   --  Initialize LCD
   STM32.LCD.Initialize (Pixel_Fmt_RGB888);
   STM32.DMA2D.Interrupt.Initialize;
   Double_Buffer.Initialize (Layer_Background => Layer_Single_Buffer,
                             Layer_Foreground => Layer_Inactive);
   FG_Buffer := Double_Buffer.Get_Visible_Buffer (Background);

   --  Initialize touch panel
   STM32.Touch_Panel.Initialize;

   --  Initialize button
   STM32.Button.Initialize;

   --  Clear LCD (set background)
   Clear;

   --  The application: set pixel where the finger is (so that you
   --  cannot see what you are drawing).
   loop
      if STM32.Button.Has_Been_Pressed then
         Clear;
      else
         declare
            State : constant TP_State := Get_State;
         begin
            for Touch of State loop
               --  Workaround some strange readings from the STM32F429 TP
               if Touch.X < FG_Buffer.Width - 1 then
                  Fill_Circle
                    (FG_Buffer, (Touch.X, Touch.Y), Touch.Weight / 4, Red);
               end if;
            end loop;
         end;
      end if;
   end loop;
end Draw;
