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

with Ada.Real_Time;                     use Ada.Real_Time;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;

with Interfaces;                        use Interfaces;

with Cortex_M.Cache;

with STM32.Button;
with STM32.DMA2D_Bitmap;                use STM32.DMA2D_Bitmap;

with HAL.Bitmap;                        use HAL.Bitmap;
with Bitmapped_Drawing;
with BMP_Fonts;

with Textures.Greystone;
with Textures.Greyada;
with Textures.Redbrick;
with Textures.Redada;
with Textures.Colorstone;
with Textures.Colorada;

with Cos;                               use Cos;

package body Raycaster is

   type Vector is record
      X : Float;
      Y : Float;
   end record;

   LCD_W : constant Natural :=
             (if LCD_Natural_Width > LCD_Natural_Height
              then LCD_Natural_Width
              else LCD_Natural_Height);

   LCD_H : constant Natural :=
             (if LCD_Natural_Width > LCD_Natural_Height
              then LCD_Natural_Height
              else LCD_Natural_Width);

   Texture_Size : constant := Textures.Texture'Length (1);

   ColorAda_Dark   : Textures.Texture;
   pragma Linker_Section (ColorAda_Dark, ".ccmdata");
   ColorStone_Dark : Textures.Texture;
   pragma Linker_Section (ColorStone_Dark, ".ccmdata");
   GreyAda_Dark    : Textures.Texture;
   pragma Linker_Section (GreyAda_Dark, ".ccmdata");
   GreyStone_Dark  : Textures.Texture;
   pragma Linker_Section (GreyStone_Dark, ".ccmdata");
   RedAda_Dark     : Textures.Texture;
   pragma Linker_Section (RedAda_Dark, ".ccmdata");
   RedBrick_Dark   : Textures.Texture;
   pragma Linker_Section (RedBrick_Dark, ".ccmdata");

   --  1 pixel = 1/10 degree
   FOV_Vect : array (0 .. LCD_W - 1) of Degree;
   pragma Linker_Section (FOV_Vect, ".ccmdata");

   Sin_Table : array (Cos_Table'Range) of Float;

   type Column_Type is array (0 .. LCD_H - 1) of Unsigned_16
     with Component_Size => 16, Alignment => 32;

   Tmp_1       : aliased Column_Type;
   Tmp_2       : aliased Column_Type;
   Tmp         : access Column_Type := Tmp_2'Access;
   Tmp_Buf     : DMA2D_Bitmap_Buffer;

   Prev_X      : Natural := 0;
   pragma Linker_Section (Prev_X, ".ccmdata");
   Prev_Scale  : Natural := 0;
   pragma Linker_Section (Prev_Scale, ".ccmdata");
   Prev_Tile   : Cell := Empty;
   pragma Linker_Section (Prev_Tile, ".ccmdata");

   function To_Unit_Vector (Angle : Degree) return Vector with Inline_Always;
   function Sin (Angle : Degree) return Float with Inline_Always;
   function Tan (Angle : Degree) return Float with Inline_Always;
   function Arctan (F : Float) return Degree with Inline_Always;

   procedure Draw_Column (Col  : Natural)
     with Inline_Always;

   procedure Distance
     (Pos      : Position;
      Vert_Hit : out Boolean;
      Offset   : out Float;
      Dist     : out Float;
      Tile     : out Cell)
     with Inline_Always;

   function Darken (Col : Unsigned_16) return Unsigned_16 is
     (Shift_Right (Col and 2#11110_111110_11110#, 1)) with Inline_Always;

   function Color
     (Tile   : Cell;
      X, Y   : Natural;
      Darken : Boolean) return Unsigned_16
     with Inline_Always, Pure_Function;

   --------------------
   -- To_Unit_Vector --
   --------------------

   function To_Unit_Vector (Angle : Degree) return Vector is
   begin
      return (Cos_Table (Angle),
              -Sin_Table (Angle)); --  -sin (angle)
   end To_Unit_Vector;

   ---------
   -- Sin --
   ---------

   function Sin (Angle : Degree) return Float
   is
   begin
      return Cos_Table (Angle + 2700);
   end Sin;

   ---------
   -- Tan --
   ---------

   function Tan (Angle : Degree) return Float
   is
   begin
      return Sin (Angle) / Cos_Table (Angle);
   end Tan;

   ------------
   -- Arctan --
   ------------

   function Arctan (F : Float) return Degree
   is
      --  Very dumb version, but OK as we're using it only during init
      A : Degree := 2700; -- -Pi/2
   begin
      while Tan (A) < F loop
         A := A + 1;
      end loop;

      return A;
   end Arctan;

   -----------------------
   -- Initialize_Tables --
   -----------------------

   procedure Initialize_Tables
   is
      X0, Xn : Float;
      X      : Float;
      FOV    : constant Cos.Degree :=
                 2 * Arctan
                   (Float (LCD_W) / (2.0 * Height_Multiplier));
   begin
      X0 := Tan (-FOV / 2);
      Xn := -X0;

      --  FOV vector
      for Col in FOV_Vect'Range loop
         --  Calculate the X on the virtual screen
         --  Left to right is decreasing angles
         X := Xn + Float (Col) / Float (FOV_Vect'Length) * (X0 - Xn);
         --  and find the angle of that pixel
         FOV_Vect (Col) := Arctan (X);
      end loop;

      for Angle in Sin_Table'Range loop
         Sin_Table (Angle) := Sin (Angle);
      end loop;

      Tmp_Buf :=
        (Addr       => Tmp.all'Address,
         Width      => 1,
         Height     => LCD_H,
         Color_Mode => Display.Get_Color_Mode (1),
         Swapped    => Display.Is_Swapped);

      for Y in Textures.Texture'Range (1) loop
         for X in Textures.Texture'Range (2) loop
            ColorAda_Dark (Y, X) := Darken (Textures.Colorada.Bmp (Y, X));
            ColorStone_Dark (Y, X) := Darken (Textures.Colorstone.Bmp (Y, X));
            GreyAda_Dark (Y, X) := Darken (Textures.Greyada.Bmp (Y, X));
            GreyStone_Dark (Y, X) := Darken (Textures.Greystone.Bmp (Y, X));
            RedAda_Dark (Y, X) := Darken (Textures.Redada.Bmp (Y, X));
            RedBrick_Dark (Y, X) := Darken (Textures.Redbrick.Bmp (Y, X));
         end loop;
      end loop;
   end Initialize_Tables;

   --------------
   -- Distance --
   --------------

   procedure Distance
     (Pos      : Position;
      Vert_Hit : out Boolean;
      Offset   : out Float;
      Dist     : out Float;
      Tile     : out Cell)
   is
      f_Dist_X, f_Dist_Y : Float;
      --  Distance between origin and the first vertical or horizontal line
      dist_X, dist_Y     : Float;
      d_Dist_X, d_Dist_Y : Float;
      --  Distance between two consecutive vertical/horizontal line along the
      --  ray
      Step_X, Step_Y     : Integer;
      --  Next square in map in X or Y direction (in the range -1 .. 1)
      Map_X, Map_Y       : Integer;
      --  Position in map coordinates
      Ray_Vect           : constant Vector := To_Unit_Vector (Pos.Angle);
      --  Unitary vector representing the ray cast

   begin
      Map_X := Natural (Float'Floor (Pos.X));
      Map_Y := Natural (Float'Floor (Pos.Y));

      --  Distance along the ray between two consecutive X coordinates
      d_Dist_X := abs (1.0 / Ray_Vect.X);
      --  Same for Y
      d_Dist_Y := abs (1.0 / Ray_Vect.Y);

      if Ray_Vect.X < 0.0 then
         Step_X := -1;
         f_Dist_X := (Pos.X - Float'Floor (Pos.X)) * d_Dist_X;
      else
         Step_X := 1;
         f_Dist_X := (Float'Ceiling (Pos.X) - Pos.X) * d_Dist_X;
      end if;

      if Ray_Vect.Y < 0.0 then
         Step_Y := -1;
         f_Dist_Y := (Pos.Y - Float'Floor (Pos.Y)) * d_Dist_Y;
      else
         Step_Y := 1;
         f_Dist_Y := (Float'Ceiling (Pos.Y) - Pos.Y) * d_Dist_Y;
      end if;

      dist_X := f_Dist_X;
      dist_Y := f_Dist_Y;

      loop
         --  Check the next distance (shortest)
         if dist_X < dist_Y then
            --  Move to the next X tile
            Map_X := Map_X + Step_X;
            Vert_Hit := True;
            --  If not empty, then we found it
            exit when Map (Map_Y, Map_X) /= Empty;
            --  Set the tentative distance for the next X tile
            dist_X := dist_X + d_Dist_X;
         else
            Map_Y := Map_Y + Step_Y;
            Vert_Hit := False;
            exit when Map (Map_Y, Map_X) /= Empty;
            dist_Y := dist_Y + d_Dist_Y;
         end if;
      end loop;

      Tile := Map (Map_Y, Map_X);

      if Vert_Hit then
         Dist := dist_X;
         --  Calculate the offset  (in X Coordinate) of the hit relative
         --  to the current tile (used for finding the proper column for the
         --  texture).
         --  First, we calculate the distance of the point where the ray hit
         --  the wall form (0,0): Sin (Angle) * dist + Initial Y position
         Offset := Pos.Y - Sin_Table (Pos.Angle) * dist_X;
         --  Offset from the tile's coordinates:
         Offset := Offset - Float'Floor (Offset);

         if Step_X < 0 then
            Offset := 1.0 - Offset;

            if Offset = 1.0 then
               Offset := 0.0;
            end if;
         end if;

      else
         Dist := dist_Y;
         --  Similar to above, but where we use the sinus: so
         --  -cos (Pos.Angle - Pi / 2), e.g. 900 in tenth of degrees
         Offset := Pos.X + Cos_Table (Pos.Angle) * dist_Y;
         Offset := Offset - Float'Floor (Offset);

         if Step_Y > 0 then
            Offset := 1.0 - Offset;

            if Offset = 1.0 then
               Offset := 0.0;
            end if;
         end if;
      end if;

      --  Multiply by Cos (Pos.Angle - Current.Angle) to fix the fisheye
      --  effect: we wnat a vertical projection on the virtual screen, that is
      --  perpendicular to the current player's angle
      Dist := Cos_Table (Pos.Angle - Current.Angle) * Dist;
   end Distance;

      -----------
      -- Color --
      -----------

   function Color
     (Tile   : Cell;
      X, Y   : Natural;
      Darken : Boolean) return Unsigned_16
   is
   begin
      case Tile is
         when Empty =>
            return 0;
         when Grey_Stone =>
            if not Darken then
               return Textures.Greystone.Bmp (Y, X);
            else
               return GreyStone_Dark (Y, X);
            end if;
         when Grey_Ada =>
            if not Darken then
               return Textures.Greyada.Bmp (Y, X);
            else
               return GreyAda_Dark (Y, X);
            end if;
         when Red_Brick =>
            if not Darken then
               return Textures.Redbrick.Bmp (Y, X);
            else
               return RedBrick_Dark (Y, X);
            end if;
         when Red_Ada =>
            if not Darken then
               return Textures.Redada.Bmp (Y, X);
            else
               return RedAda_Dark (Y, X);
            end if;
         when Color_Stone =>
            if not Darken then
               return Textures.Colorstone.Bmp (Y, X);
            else
               return ColorStone_Dark (Y, X);
            end if;
         when Color_Ada =>
            if not Darken then
               return Textures.Colorada.Bmp (Y, X);
            else
               return ColorAda_Dark (Y, X);
            end if;
      end case;
   end Color;

   -----------------
   -- Draw_Column --
   -----------------

   procedure Draw_Column
     (Col  : Natural)
   is
      Buf      : constant HAL.Bitmap.Bitmap_Buffer'Class :=
                   Display.Get_Hidden_Buffer (1);
      Col_Pos  : Position := Current;
      Off      : Float;
      Dist     : Float;
      Tile     : Cell;
      Side     : Boolean;
      Height   : Natural;
      Scale    : Natural;
      X, Y, dY : Natural;

   begin
      Col_Pos.Angle := Current.Angle + FOV_Vect (Col);
      Distance
        (Pos      => Col_Pos,
         Vert_Hit => Side,
         Offset   => Off,
         Dist     => Dist,
         Tile     => Tile);

      if Tile = Empty then
         return;
      end if;

      X := Natural
        (Float'Floor (Off * Float (Textures.Texture'Length (2))));

      Scale := Natural (Height_Multiplier / Dist);

      if Scale > LCD_H then
         dY := (Scale - LCD_H) / 2;
         Height := LCD_H;
      else
         dY := 0;
         Height := Scale;
      end if;

      if Height = 0 then
         return;
      end if;

      --  Do not recompute the temp column if we have an identical situation
      if Prev_Scale /= Scale
        or else Prev_X /= X
        or else Prev_Tile /= Tile
      then
         --  While the Tmp buffer is being transfered, do not attempt to write
         --  to it. We use a double buffer system here to prevent such modif
         --  while transfering
         if Tmp = Tmp_1'Access then
            Tmp := Tmp_2'Access;
         else
            Tmp := Tmp_1'Access;
         end if;

         Tmp_Buf.Addr := Tmp.all'Address;

         if Scale <= Texture_Size then
            --  Shrinking case
            for Row in 0 .. Height - 1 loop
               Y := (Row + dY) * Texture_Size / Scale;
               Tmp (Row) := Color (Tile, X, Y, Side);
            end loop;

         else
            --  Expanding case
            declare
               Y0      : constant Natural :=
                           (dY * Texture_Size) / Scale;
               Y1      : constant Natural :=
                           ((Height - 1 + dY) * Texture_Size) / Scale;
               Row     : Natural;
               R_Next  : Natural := 0;
               Col     : Unsigned_16;

            begin
               for Y in Y0 .. Y1 loop
                  Col := Color (Tile, X, Y, Side);
                  Row := R_Next;

                  if Y = Y1 then
                     R_Next := Height;
                  else
                     R_Next := ((Y + 1) * Scale) / Texture_Size - dY;
                  end if;

                  Tmp (Row .. R_Next - 1) := (others => Col);
               end loop;
            end;
         end if;

         Cortex_M.Cache.Clean_DCache (Tmp (0)'Address, Height * 2);

         Prev_Scale := Scale;
         Prev_X := X;
         Prev_Tile := Tile;
      end if;

      --  Start next column as soon as possible, so don't wait for the DMA
      --  transfer to terminate (Synchronous is False).
      Copy_Rect
        (Src_Buffer  => Tmp_Buf,
         X_Src       => 0,
         Y_Src       => 0,
         Dst_Buffer  => Buf,
         X_Dst       => Col,
         Y_Dst       => (LCD_H - Height) / 2,
         Width       => 1,
         Height      => Height,
         Synchronous => False);
   end Draw_Column;

   ----------
   -- Draw --
   ----------

   Last : Time := Clock;
   FPS  : Natural := 0;

   procedure Draw
   is
      Buf : constant HAL.Bitmap.Bitmap_Buffer'Class :=
              Display.Get_Hidden_Buffer (1);
      FG  : constant HAL.Bitmap.Bitmap_Buffer'Class :=
              Display.Get_Hidden_Buffer (2);
   begin
      Buf.Fill_Rect
        (Color  => (255, others => 45),
         X      => 0,
         Y      => 0,
         Width  => LCD_W,
         Height => LCD_H / 2);
      Buf.Fill_Rect
        (Color  => (255, others => 97),
         X      => 0,
         Y      => LCD_H / 2,
         Width  => LCD_W,
         Height => LCD_H / 2);

      for X in FOV_Vect'Range loop
         Draw_Column (X);
      end loop;

      FPS := FPS + 1;

      if Clock - Last > Milliseconds (500) then
         Ada.Text_IO.Put_Line (Natural'Image (FPS * 2) & " fps");
         FG.Fill (Transparent);
         Cortex_M.Cache.Invalidate_DCache (FG.Addr, FG.Buffer_Size);
         Bitmapped_Drawing.Draw_String
           (Buffer     => FG,
            Start      => (0, 0),
            Msg        => Natural'Image (FPS * 2) & " fps",
            Font       => BMP_Fonts.Font12x12,
            Foreground => HAL.Bitmap.White,
            Background => HAL.Bitmap.Transparent);
         FPS := 0;
         Last := Clock;
         Display.Update_Layers;
      else
         Display.Update_Layer (1);
      end if;

      if STM32.Button.Has_Been_Pressed then
         while not STM32.Button.Has_Been_Pressed loop
            null;
         end loop;
      end if;
   end Draw;

end Raycaster;
