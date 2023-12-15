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

with Bitmapped_Drawing;
with BMP_Fonts;

package body GUI_Buttons is

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Buffer  : in out HAL.Bitmap.Bitmap_Buffer'Class;
      Buttons : Button_Info_Array;
      State   : Boolean_Array) is
   begin
      Draw (Buffer, Buttons, State, not State);
   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Buffer     : in out HAL.Bitmap.Bitmap_Buffer'Class;
      Buttons    : Button_Info_Array;
      State      : Boolean_Array;
      Prev_State : Boolean_Array) is
   begin
      for J in Buttons'Range loop
         if State (J) /= Prev_State (J) then
            declare
               use type HAL.Bitmap.Point;

               Button : Button_Info renames Buttons (J);

               Area   : constant HAL.Bitmap.Rect :=
                 (Button.Center - (10, 6),
                  Width => 20, Height => 13);

               Foreground : constant HAL.Bitmap.Bitmap_Color :=
                 (if State (J) then HAL.Bitmap.Black else Button.Color);

               Background : constant HAL.Bitmap.Bitmap_Color :=
                 (if State (J) then Button.Color else HAL.Bitmap.Black);
            begin
               Buffer.Set_Source (Background);
               Buffer.Fill_Rounded_Rect (Area, 3);

               Bitmapped_Drawing.Draw_String
                 (Buffer,
                  Start      => Area.Position + (1, 2),
                  Msg        => Button.Label,
                  Font       => BMP_Fonts.Font8x8,
                  Foreground => Foreground,
                  Background => Background);

               if not State (J) then
                  Buffer.Set_Source (Foreground);
                  Buffer.Draw_Rounded_Rect (Area, 3);
               end if;
            end;
         end if;
      end loop;
   end Draw;

end GUI_Buttons;
