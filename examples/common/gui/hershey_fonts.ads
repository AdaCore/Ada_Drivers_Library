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

--  This package handles the Hershey font format.

with Interfaces; use Interfaces;
with HAL;        use HAL;

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
      Width    : UInt8;
      Height   : UInt8;
      Coords   : Coord_Array (1 .. 120);
   end record with Pack;

   type Glyph_Index is new Positive range 1 .. 96;

   type Glyph_Array is array (Glyph_Index) of Glyph;

   type Hershey_Font is record
      Number_Of_Glyphs : Glyph_Index;
      Glyphs           : Glyph_Array;
      Font_Height      : UInt8;
      Baseline         : UInt8;
   end record;

end Hershey_Fonts;
