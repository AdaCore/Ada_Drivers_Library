
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
