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

--  This package handles the Hershey font format.

with Interfaces; use Interfaces;

package Hershey_Fonts is

   type Font_Desc is access constant String;

   type Hershey_Font is private;

   type Glyph_Index is private;

   function Read (Fnt : Font_Desc) return Hershey_Font;
   procedure Read (Fnt : Font_Desc;
                   Ret : out Hershey_Font);

   function Strlen (S      : String;
                    Fnt    : Hershey_Font;
                    Height : Natural) return Natural;

   generic
      with procedure Draw_Line
        (X0, Y0, X1, Y1 : Natural;
         Width          : Positive);
   procedure Draw_Glyph
     (Fnt     : Hershey_Font;
      C       : Character;
      X       : in out Natural;
      Y       : Natural;
      Height  : Natural;
      Bold    : Boolean);

private

   type Coord is record
      X : Interfaces.Integer_8;
      Y : Interfaces.Integer_8;
   end record with Size => 16;

   Raise_Pen : constant Coord := (others => Integer_8'First);

   type Coord_Array is array (Positive range <>) of Coord
     with Component_Size => 16;

   type Glyph is record
      Vertices : Natural;
      Width    : Unsigned_8;
      Height   : Unsigned_8;
      Coords   : Coord_Array (1 .. 120);
   end record with Pack;

   type Glyph_Index is new Positive range 1 .. 96;

   type Glyph_Array is array (Glyph_Index) of Glyph;

   type Hershey_Font is record
      Number_Of_Glyphs : Glyph_Index;
      Glyphs           : Glyph_Array;
      Font_Height      : Unsigned_8;
      Baseline         : Unsigned_8;
   end record;

end Hershey_Fonts;
