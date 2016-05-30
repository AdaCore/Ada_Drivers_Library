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

      function To_Byte (S : String) return Natural;

      -------------
      -- To_Byte --
      -------------

      function To_Byte (S : String) return Natural
      is
         Ret : Natural := 0;
      begin
         for J in S'Range loop
            if S (J) /= ' ' then
               Ret := 10 * Ret + Character'Pos (S (J)) - Character'Pos ('0');
            end if;
         end loop;
         return Ret;
      end To_Byte;

   begin
      Fnt_Idx := Fnt'First;

      while Fnt_Idx <= Fnt'Last loop
         --  Skip the 5 first characters of the glyph header
         Fnt_Idx := Fnt_Idx + 5;

         declare
            Size  : constant Natural := To_Byte (Fnt (Fnt_Idx .. Fnt_Idx + 2));
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
                     G.Width := Unsigned_8 (C.Y - C.X + 1);
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
               G.Height := Unsigned_8 (YMax - YMin);
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

            Ret.Baseline := Unsigned_8 (-Fnt_Y_Min);
            Ret.Font_Height := Unsigned_8'Max (Ret.Font_Height, G.Height);
         end;
      end loop;
   end Read;

end Hershey_Fonts;
