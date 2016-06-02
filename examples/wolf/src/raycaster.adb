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

with Ada.Real_Time;                     use Ada.Real_Time;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;

with Interfaces;                        use Interfaces;

with Cortex_M.Cache;                    use Cortex_M.Cache;

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

   --  1 pixel = 1/10 degree
   FOV_Vect : array (0 .. LCD_W - 1) of Degree;
   pragma Linker_Section (FOV_Vect, ".ccmdata");

   type Column_Type is array (0 .. LCD_H - 1) of Unsigned_16
     with Component_Size => 16, Alignment => 32;
   Tmp      : Column_Type;

   Tmp_Buf   : DMA2D_Bitmap_Buffer;

   function To_Unit_Vector (Angle : Degree) return Vector with Inline_Always;
   function Sin (Angle : Degree) return Float with Inline_Always;
   function Tan (Angle : Degree) return Float with Inline_Always;
   function Arctan (F : Float) return Degree with Inline_Always;

   procedure Draw_Column
     (Buf  : HAL.Bitmap.Bitmap_Buffer'Class;
      Col  : Natural)
     with Inline_Always;

   procedure Distance
     (Pos      : Position;
      Vert_Hit : out Boolean;
      Offset   : out Float;
      Distance : out Float;
      Tile     : out Cell)
     with Inline_Always;

   function Darken (Col : Unsigned_16) return Unsigned_16 with Inline;

   --------------------
   -- To_Unit_Vector --
   --------------------

   function To_Unit_Vector (Angle : Degree) return Vector is
   begin
      return (Cos_Table (Angle),
              -Sin (Angle)); --  -sin (angle)
   end To_Unit_Vector;

   ---------
   -- Sin --
   ---------

   function Sin (Angle : Degree) return Float
   is
   begin
      return Cos_Table (Angle - 900);
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

      for Col in FOV_Vect'Range loop
         --  Calculate the X on the virtual screen
         --  Left to right is decreasing angles
         X := Xn + Float (Col) / Float (FOV_Vect'Length) * (X0 - Xn);
         --  and find the angle of that pixel
         FOV_Vect (Col) := Arctan (X);
      end loop;

      Tmp_Buf :=
        (Addr       => Tmp (0)'Address,
         Width      => 1,
         Height     => LCD_H,
         Color_Mode => Display.Get_Color_Mode (1),
         Swapped    => Display.Is_Swapped);
   end Initialize_Tables;

   --------------
   -- Distance --
   --------------

   procedure Distance
     (Pos      : Position;
      Vert_Hit : out Boolean;
      Offset   : out Float;
      Distance : out Float;
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

      --  Distance along the ray between to consecutive X coordinates
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
         Distance := dist_X;
         --  Calculate the offset  (in X Coordinate) of the hit relative
         --  to the current tile (used for finding the proper column for the
         --  texture).
         --  First, we calculate the distance of the point where the ray hit
         --  the wall form (0,0): Sin (Angle) * dist + Initial Y position
         Offset := Pos.Y - Cos_Table (Pos.Angle - 900) * dist_X;
         --  Offset from the tile's coordinates:
         Offset := Offset - Float'Floor (Offset);
         if Step_X < 0 then
            Offset := 1.0 - Offset;
            if Offset = 1.0 then
               Offset := 0.0;
            end if;
         end if;
      else
         Distance := dist_Y;
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
      Distance := Cos_Table (Pos.Angle - Current.Angle) * Distance;
   end Distance;

   Prev_Color : Unsigned_16 := 0;
   pragma Linker_Section (Prev_Color, ".ccmdata");
   Prev_Res   : Unsigned_16 := 0;
   pragma Linker_Section (Prev_Res, ".ccmdata");

   ------------
   -- Darken --
   ------------

   function Darken (Col : Unsigned_16) return Unsigned_16
   is
   begin
      if Col /= Prev_Color then
         Prev_Res := Shift_Right (Col and 2#11110_111110_11110#, 1);
         Prev_Color := Col;
      end if;

      return Prev_Res;
   end Darken;

   Prev_X      : Natural := 0;
   pragma Linker_Section (Prev_X, ".ccmdata");
   Prev_Scale  : Natural := 0;
   pragma Linker_Section (Prev_Scale, ".ccmdata");
   Prev_Tile   : Cell := Empty;
   pragma Linker_Section (Prev_Tile, ".ccmdata");

   -----------------
   -- Draw_Column --
   -----------------

   procedure Draw_Column
     (Buf  : HAL.Bitmap.Bitmap_Buffer'Class;
      Col  : Natural)
   is
      Col_Pos  : Position := Current;
      Off      : Float;
      Dist     : Float;
      Tile     : Cell;
      Side     : Boolean;
      Height   : Natural;
      Scale    : Natural;
      X, Y, dY : Natural;
      Bmp      : access constant Textures.Texture;

   begin
      Col_Pos.Angle := Current.Angle + FOV_Vect (Col);
      Distance
        (Pos      => Col_Pos,
         Vert_Hit => Side,
         Offset   => Off,
         Distance => Dist,
         Tile     => Tile);

      case Tile is
         when Empty =>
            return;
         when Grey_Stone =>
            Bmp := Textures.Greystone.Bmp'Access;
         when Grey_Ada =>
            Bmp := Textures.Greyada.Bmp'Access;
         when Red_Brick =>
            Bmp := Textures.Redbrick.Bmp'Access;
         when Red_Ada =>
            Bmp := Textures.Redada.Bmp'Access;
         when Color_Stone =>
            Bmp := Textures.Colorstone.Bmp'Access;
         when Color_Ada =>
            Bmp := Textures.Colorada.Bmp'Access;
      end case;

      X := Natural
        (Float'Floor (Off * Float (Bmp'Length (2))));

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
         for Row in 0 .. Height - 1 loop
            Y := (Row + dY) * Bmp'Length (1) / Scale;
            Tmp (Row) := (if Side then Darken (Bmp (Y, X)) else Bmp (Y, X));
         end loop;

         --  Flush the data cache to actual memory before activating the DMA
         Cortex_M.Cache.Clean_DCache
           (Tmp (0)'Address, Tmp'Size / 8);

         Prev_Scale := Scale;
         Prev_X := X;
         Prev_Tile := Tile;
      end if;

      Copy_Rect
        (Src_Buffer  => Tmp_Buf,
         X_Src       => 0,
         Y_Src       => 0,
         Dst_Buffer  => Buf,
         X_Dst       => Col,
         Y_Dst       => (LCD_H - Height) / 2,
         Bg_Buffer   => Null_Buffer,
         X_Bg        => 0,
         Y_Bg        => 0,
         Width       => 1,
         Height      => Height);
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
         Draw_Column (Buf, X);
      end loop;

      FPS := FPS + 1;
      if Clock - Last > Milliseconds (500) then
         Last := Clock;
         Ada.Text_IO.Put_Line (Natural'Image (FPS * 2) & " fps");
         Bitmapped_Drawing.Draw_String
           (Buffer     => Display.Get_Hidden_Buffer (2),
            Start      => (0, 0),
            Msg        => Natural'Image (FPS * 2) & " fps",
            Font       => BMP_Fonts.Font12x12,
            Foreground => HAL.Bitmap.White,
            Background => HAL.Bitmap.Transparent);
         FPS := 0;
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
