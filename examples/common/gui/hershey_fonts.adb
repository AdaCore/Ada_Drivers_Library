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

package body Hershey_Fonts is

   function Get_Glyph (C : Character) return Glyph_Index;

   ---------------
   -- Get_Glyph --
   ---------------

   function Get_Glyph (C : Character) return Glyph_Index
   is
      Ret : constant Integer := Character'Pos (C) - 31;
   begin
      if Ret <= 0 then
         return Glyph_Index'First;
      else
         return Glyph_Index (Ret);
      end if;
   end Get_Glyph;

   ----------------
   -- Draw_Glyph --
   ----------------

   procedure Draw_Glyph
     (Fnt    : Hershey_Font;
      C      : Character;
      X      : in out Natural;
      Y      : Natural;
      Height : Natural;
      Bold   : Boolean)
   is
      Ratio     : constant Float := Float (Height) / Float (Fnt.Font_Height);
      Thickness : Natural := Natural ((if Bold then 3.0 else 2.0) * Ratio);
      G         : constant Glyph := Fnt.Glyphs (Get_Glyph (C));
      First     : Boolean := True;
      Current   : Coord;

   begin
      if Thickness < 1 then
         Thickness := 1;
      end if;

      for V in 1 .. G.Vertices loop
         if G.Coords (V) = Raise_Pen then
            First := True;
         elsif First then
            Current := G.Coords (V);
            First   := False;
         else
            declare
               X0, Y0, X1, Y1 : Natural;
            begin
               X0 := Natural (Float (Current.X) * Ratio) + X;
               Y0 := Natural (Float (Current.Y) * Ratio) + Y;
               Current := G.Coords (V);
               X1 := Natural (Float (Current.X) * Ratio) + X;
               Y1 := Natural (Float (Current.Y) * Ratio) + Y;
               Draw_Line (X0, Y0, X1, Y1, Thickness);
            end;
         end if;
      end loop;

      X := X + Natural (Float (G.Width) * Ratio);
   end Draw_Glyph;

   ------------
   -- Strlen --
   ------------

   function Strlen (S      : String;
                    Fnt    : Hershey_Font;
                    Height : Natural) return Natural
   is
      Ret   : Natural := 0;
      Ratio : constant Float := Float (Height) / Float (Fnt.Font_Height);
   begin
      for C of S loop
         declare
            G : Glyph renames Fnt.Glyphs (Get_Glyph (C));
         begin
            Ret := Ret + Natural (Float (G.Width) * Ratio);
         end;
      end loop;

      return Ret;
   end Strlen;

   ----------
   -- Read --
   ----------

   function Read (Fnt : Font_Desc) return Hershey_Font
   is
      Ret : Hershey_Font;
   begin
      Read (Fnt, Ret);
      return Ret;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (Fnt : Font_Desc;
                   Ret : out Hershey_Font)
   is
      Glyph_Idx   : Natural := 0;
      Fnt_Idx     : Natural;
      Fnt_Y_Min   : Integer_8 := Integer_8'Last;

      function To_UInt8 (S : String) return Natural;

      -------------
      -- To_UInt8 --
      -------------

      function To_UInt8 (S : String) return Natural
      is
         Ret : Natural := 0;
      begin
         for J in S'Range loop
            if S (J) /= ' ' then
               Ret := 10 * Ret + Character'Pos (S (J)) - Character'Pos ('0');
            end if;
         end loop;
         return Ret;
      end To_UInt8;

   begin
      Fnt_Idx := Fnt'First;

      while Fnt_Idx <= Fnt'Last loop
         --  Skip the 5 first characters of the glyph header
         Fnt_Idx := Fnt_Idx + 5;

         declare
            Size  : constant Natural := To_UInt8 (Fnt (Fnt_Idx .. Fnt_Idx + 2));
            G     : Glyph;
            CStr  : String (1 .. 2);
            C     : Coord;
            Left  : Integer_8;
            XMin  : Integer_8 := Integer_8'Last;
            YMin  : Integer_8 := Integer_8'Last;
            XMax  : Integer_8 := Integer_8'First;
            YMax  : Integer_8 := Integer_8'First;

         begin
            Fnt_Idx := Fnt_Idx + 3;

            for Idx in 0 .. Size - 1 loop
               for J in CStr'Range loop
                  while Fnt (Fnt_Idx) = ASCII.LF loop
                     Fnt_Idx := Fnt_Idx + 1;
                  end loop;

                  CStr (J) := Fnt (Fnt_Idx);
                  Fnt_Idx := Fnt_Idx + 1;
               end loop;

               if CStr = " R" then
                  G.Coords (Idx) := Raise_Pen;
               else
                  C := (Character'Pos (CStr (1)) - Character'Pos ('R'),
                        Character'Pos (CStr (2)) - Character'Pos ('R'));

                  if Idx = 0 then
                     --  First coordinate tells the left and right borders of
                     --  the glyph.
                     Left := C.X;
                     G.Width := UInt8 (C.Y - C.X + 1);
                  else
                     C.X := C.X - Left;
                     XMin := Integer_8'Min (XMin, C.X);
                     YMin := Integer_8'Min (YMin, C.Y);
                     XMax := Integer_8'Max (XMax, C.X);
                     YMax := Integer_8'Max (XMax, C.Y);

                     G.Coords (Idx) := C;
                  end if;
               end if;
            end loop;

            G.Vertices := Size - 1;

            if G.Vertices = 0 then
               G.Height := 0;
            else
               G.Height := UInt8 (YMax - YMin);
               Fnt_Y_Min := Integer_8'Min (YMin, Fnt_Y_Min);
            end if;

            Glyph_Idx := Glyph_Idx + 1;
            Ret.Glyphs (Glyph_Index (Glyph_Idx)) := G;
         end;
      end loop;

      Ret.Number_Of_Glyphs := Glyph_Index (Glyph_Idx);
      Ret.Font_Height := 0;

      for Idx in 1 .. Ret.Number_Of_Glyphs loop
         declare
            G : Glyph renames Ret.Glyphs (Idx);
         begin
            for Vert in 1 .. G.Vertices loop
               if G.Coords (Vert) /= Raise_Pen then
                  G.Coords (Vert).Y := G.Coords (Vert).Y - Fnt_Y_Min;
               end if;
            end loop;

            Ret.Baseline := UInt8 (-Fnt_Y_Min);
            Ret.Font_Height := UInt8'Max (Ret.Font_Height, G.Height);
         end;
      end loop;
   end Read;

end Hershey_Fonts;
