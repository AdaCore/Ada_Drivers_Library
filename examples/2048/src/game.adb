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

with Interfaces;           use Interfaces;
with Interfaces.Bit_Types; use Interfaces.Bit_Types;

with STM32.RNG;
with STM32.Board;          use STM32.Board;
with STM32.SDRAM;          use STM32.SDRAM;
with Malloc;               use Malloc;

with STM32.DMA2D_Bitmap;   use STM32.DMA2D_Bitmap;

package body Game is

   Background_Buffer       : DMA2D_Bitmap_Buffer;
   Background_Slide_Buffer : DMA2D_Bitmap_Buffer;
   Cells_Buffer            : DMA2D_Bitmap_Buffer;

   Cell_Size               : Natural := 0;
   Ext_Border              : Natural;
   Int_Border              : Natural;
   Up_Margin               : Natural;

   procedure Draw_Cell
     (Coord : Point;
      Value : Integer;
      Dst   : Bitmap_Buffer'Class);

   function Cell_To_Coordinate (X : Size; Y : Size) return Point;

   procedure Draw_Background (Dst : Bitmap_Buffer'Class);

   procedure Draw_Grid_To_Grid
     (Src : Bitmap_Buffer'Class;
      Dst : Bitmap_Buffer'Class);

   procedure Draw_Cell_Background
     (X, Y   : Integer;
      Color  : Bitmap_Color;
      Buffer : Bitmap_Buffer'Class;
      Border : Boolean);

   ---------------
   -- Get_Score --
   ---------------

   function Get_Score return Natural is
   begin
      return Grid.Score;
   end Get_Score;

   ---------------------
   -- Get_Status_Area --
   ---------------------

   function Get_Status_Area return Bitmapped_Drawing.Rect
   is
   begin
      pragma Warnings (Off, "condition is always *");
      if LCD_Natural_Height > LCD_Natural_Width then
         return
           (Position => (0, 0),
            Width    => LCD_Natural_Width,
            Height   => LCD_Natural_Height - LCD_Natural_Width);
      else
         return
           (Position => (LCD_Natural_Height, 0),
            Width    => LCD_Natural_Width - LCD_Natural_Height,
            Height   => LCD_Natural_Height);
      end if;
      pragma Warnings (On, "condition is always *");
   end Get_Status_Area;

   ----------
   -- Init --
   ----------

   procedure Init
   is
      Screen_Size : Natural;
      CM          : constant Bitmap_Color_Mode :=
                      Display.Get_Color_Mode (1);
      Pixel_Size  : constant Natural := Display.Get_Pixel_Size (1);

   begin
      Screen_Size  := Natural'Min (Display.Get_Width, Display.Get_Height);
      Cell_Size    := (Screen_Size - 5 * 8) / 4;
      Int_Border   := 8;
      Ext_Border   := (Screen_Size - 4 * Cell_Size - 3 * Int_Border) / 2;
      Up_Margin    := Display.Get_Height - Screen_Size;
      --  Up_Margin    := 0;

      Background_Buffer :=
        (Addr       => Reserve (Word (Screen_Size * Screen_Size * Pixel_Size)),
         Color_Mode => CM,
         Width      => Screen_Size,
         Height     => Screen_Size,
         Swapped    => Display.Is_Swapped);
      Background_Slide_Buffer :=
        (Addr       => Reserve (Word (Screen_Size * Screen_Size * Pixel_Size)),
         Color_Mode => CM,
         Width      => Screen_Size,
         Height     => Screen_Size,
         Swapped    => Display.Is_Swapped);
      Cells_Buffer :=
        (Addr       =>
           Reserve (Word (Cell_Size * Cell_Size * 16 * Pixel_Size)),
         Color_Mode => CM,
         Width      => Cell_Size,
         Height     => Cell_Size * 16,
         Swapped    => Display.Is_Swapped);

      Init_Background_Buffer;
      Init_Cells_Buffer;
   end Init;

   --------------------------
   -- Draw_Cell_Background --
   --------------------------

   procedure Draw_Cell_Background
     (X, Y   : Integer;
      Color  : Bitmap_Color;
      Buffer : Bitmap_Buffer'Class;
      Border : Boolean)
   is
      Radius : constant Natural := Cell_Size / 10;
   begin
      Fill_Rounded_Rectangle
        (Buffer,
         X      => X,
         Y      => Y,
         Width  => Cell_Size,
         Height => Cell_Size,
         Radius => Radius,
         Hue    => Color);

      if Border then
         Draw_Rounded_Rectangle
           (Buffer,
            Area      => ((X, Y), Cell_Size, Cell_Size),
            Radius    => Radius,
            Hue       => (255, 128, 128, 128),
            Thickness => 1);
      end if;
   end Draw_Cell_Background;


   ----------------------------
   -- Init_Background_Buffer --
   ----------------------------

   procedure Init_Background_Buffer is
   begin
      Background_Buffer.Fill
        ((Alpha => 255,
          Red   => 187,
          Green => 173,
          Blue  => 160));

      for Y in 0 .. 3 loop
         for X in 0 .. 3 loop
            Draw_Cell_Background
              (X          => Ext_Border + (Int_Border + Cell_Size) * X,
               Y          => Ext_Border + (Int_Border + Cell_Size) * Y,
               Color      => (Alpha => 255,
                              Red   => 16#CD#,
                              Green => 16#C0#,
                              Blue  => 16#B4#),
               Buffer     => Background_Buffer,
               Border     => False);
         end loop;
      end loop;
   end Init_Background_Buffer;

   -----------------------
   -- Init_Cells_Buffer --
   -----------------------

   procedure Init_Cells_Buffer is
      Colors : constant array (0 .. 15) of Bitmap_Color :=
                 ((255, 238, 228, 218),
                  (255, 237, 224, 200),
                  (255, 242, 177, 121),
                  (255, 245, 149, 99),
                  (255, 246, 124, 95),
                  (255, 246, 94, 59),
                  (255, 237, 207, 114),
                  (255, 237, 204, 97),
                  (255, 237, 200, 80),
                  (255, 237, 197, 63),
                  (255, 237, 194, 46),
                  (255, 60, 58, 50),
                  (255, 60, 209, 50),
                  (255, 35, 107, 29),
                  (255, 50, 136, 209),
                  (255, 17, 15, 104));

      Num : Natural;

   begin

      Fill (Cells_Buffer, Transparent);

      for I in Colors'Range loop
         Draw_Cell_Background
           (0, I * Cell_Size, Colors (I), Cells_Buffer, True);

         Num := 2 ** (I + 1);
         declare
            S      : constant String := Num'Img;
            Height : constant Natural := Cell_Size * 3 / 5;
            Max_W  : constant Natural := Cell_Size * 3 / 4;
            Fg     : Bitmap_Color := White;
            Str_Area : constant Rect :=
                         (Position =>
                            ((Cell_Size - Max_W) / 2,
                             (Cell_Size - Height) / 2 + I * Cell_Size),
                          Width    => Max_W,
                          Height   => Height);
         begin
            if Num = 2 or else Num = 4 then
               Fg := (255, 100, 90, 80);
            end if;

            Bitmapped_Drawing.Draw_String
              (Cells_Buffer,
               Area       => Str_Area,
               Msg        => S (S'First + 1 .. S'Last),
               Font       => Times,
               Bold       => True,
               Outline    => True,
               Foreground => Fg,
               Fast       => False);
         end;
      end loop;
   end Init_Cells_Buffer;

   -----------------------
   -- Draw_Grid_To_Grid --
   -----------------------

   procedure Draw_Grid_To_Grid
     (Src : Bitmap_Buffer'Class;
      Dst : Bitmap_Buffer'Class) is
   begin
      Copy_Rect
        (Src_Buffer => Src,
         X_Src      => 0,
         Y_Src      => 0,
         Dst_Buffer => Dst,
         X_Dst      => 0,
         Y_Dst      => Dst.Height - Background_Buffer.Height,
         Width      => Src.Width,
         Height     => Src.Height);
   end Draw_Grid_To_Grid;

   ---------------------
   -- Draw_Background --
   ---------------------

   procedure Draw_Background (Dst : Bitmap_Buffer'Class) is
   begin
      Draw_Grid_To_Grid (Background_Buffer, Dst);
   end Draw_Background;

   ------------------------
   -- Cell_To_Coordinate --
   ------------------------

   function Cell_To_Coordinate (X : Size; Y : Size) return Point is
   begin
      return (X => Ext_Border + Integer (X) * (Cell_Size + Int_Border),
              Y => Ext_Border + Integer (Y) * (Cell_Size + Int_Border));
   end Cell_To_Coordinate;

   ---------------
   -- Draw_Cell --
   ---------------

   procedure Draw_Cell
     (Coord : Point;
      Value : Integer;
      Dst   : Bitmap_Buffer'Class)
   is
   begin
      Copy_Rect_Blend
        (Src_Buffer => Cells_Buffer,
         X_Src      => 0,
         Y_Src      => (Value - 1) * Cell_Size,
         Dst_Buffer => Dst,
         X_Dst      => Coord.X,
         Y_Dst      => Coord.Y,
         Width      => Cell_Size,
         Height     => Cell_Size);
   end Draw_Cell;

   ----------
   -- Draw --
   ----------

   procedure Draw (Dst : Bitmap_Buffer'Class) is
      Value : Integer := 0;
   begin
      Draw_Background (Dst);
      for Y in Standard.Grid.Size loop
         for X in Standard.Grid.Size loop
            Value := Grid.Get (X, Y);
            if Value /= 0 then
               declare
                  P : Point := Cell_To_Coordinate (X, Y);
               begin
                  P.Y := P.Y + Dst.Height - Background_Buffer.Height;
                  Draw_Cell (P, Value, Dst);
               end;
            end if;
         end loop;
      end loop;
   end Draw;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      STM32.RNG.Enable_RNG_Clock;
      STM32.RNG.Enable_RNG;
      Grid.Init;
      Add_Value;
   end Start;

   ---------------
   -- Add_Value --
   ---------------

   procedure Add_Value is
      Rand_Pos : Word := STM32.RNG.RNG_Data;
      N_Free   : Natural := 0;
      Val      : constant Natural :=
                   (if STM32.RNG.RNG_Data mod 10 = 0 then 2 else 1);
   begin
      for X in Size loop
         for Y in Size loop
            if Grid.Get (X, Y) = 0 then
               N_Free := N_Free + 1;
            end if;
         end loop;
      end loop;

      Rand_Pos := Rand_Pos mod Word (N_Free);
      N_Free := 0;
      Main_Loop :
      for X in Size loop
         for Y in Size loop
            if Grid.Get (X, Y) = 0 then
               if N_Free = Natural (Rand_Pos) then
                  Grid.Set (X, Y, Val);
                  exit Main_Loop;
               end if;
               N_Free := N_Free + 1;
            end if;
         end loop;
      end loop Main_Loop;
   end Add_Value;


   ----------------
   -- Init_Slide --
   ----------------

   procedure Init_Slide (Old_Grid : CGrid; Trace : Trace_Grid_T)
   is
      Is_Moving : Boolean := False;
   begin

      Slide_Start_Time := Clock;
      Draw_Background (Background_Slide_Buffer);
      for Y in Size loop
         for X in Size loop
            declare
               I : constant Moving_Cells_Index_T :=
                     Moving_Cells_Index_T (Y * Size'Range_Length + X);
               P : Cell_Position_T renames Trace (X, Y);
            begin
               if P.X /= X or else P.Y /= Y then
                  declare
                     Src : Point := Cell_To_Coordinate (X, Y);
                     Dst : Point := Cell_To_Coordinate (P.X, P.Y);
                  begin
                     Src.Y := Src.Y + Up_Margin;
                     Dst.Y := Dst.Y + Up_Margin;
                     Moving_Cells (I) :=
                       (Src => Src,
                        Dst => Dst,
                        Src_Value => Old_Grid.Get (X, Y),
                        Dst_Value => Grid.Get (P.X, P.Y),
                        V         => (X => (if P.X /= X
                                            then (if P.X - X > 0
                                              then  1 else -1)
                                            else 0),
                                      Y => (if P.Y /= Y
                                            then (if P.Y - Y > 0
                                              then  1 else -1)
                                            else 0)),
                        Max_Length =>
                           abs (Dst.X - Src.X) + abs (Dst.Y - Src.Y),
                        Moving     => True);
                     Is_Moving := True;
                  end;
               else
                  Moving_Cells (I) := (Src => (0, 0),
                                       Dst => (0, 0),
                                       Src_Value => 0,
                                       Dst_Value => 0,
                                       V     => (0, 0),
                                       Max_Length => 0,
                                       Moving     => False);
                  if Old_Grid.Get (X, Y) /= 0 then
                     Draw_Cell (Cell_To_Coordinate (X, Y),
                                Old_Grid.Get (X, Y),
                                Background_Slide_Buffer);
                  end if;
               end if;
            end;
         end loop;
      end loop;

      if Is_Moving then
         Sliding := True;
      end if;
   end Init_Slide;

   -----------
   -- Slide --
   -----------

   function Slide (Dst : Bitmap_Buffer'Class) return Boolean
   is
      Length      : Float;
      Is_Moving   : Boolean := False;
      Slide_Speed : constant Float :=
                      Float (Background_Buffer.Width) * 8.0;
   begin
      Length := Slide_Speed * Float (To_Duration (Clock - Slide_Start_Time));

      for Cell of Moving_Cells loop
         if Cell.Moving then
            if Integer (Length) >= Cell.Max_Length then
               Cell.Moving := False;
               Draw_Cell (Coord => (Cell.Dst.X, Cell.Dst.Y - Up_Margin),
                          Value => Cell.Dst_Value,
                          Dst   => Background_Slide_Buffer);
            end if;
         end if;
      end loop;

      Draw_Grid_To_Grid (Background_Slide_Buffer, Dst);

      for Cell of Moving_Cells loop
         if Cell.Moving then
            Is_Moving := True;
            Draw_Cell (Coord => (Cell.Src.X + Integer (Length) * Cell.V.X,
                                 Cell.Src.Y + Integer (Length) * Cell.V.Y),
                       Value => Cell.Src_Value,
                       Dst   => Dst);
         end if;
      end loop;

      if not Is_Moving then
         Sliding := False;
      end if;

      return Sliding;
   end Slide;

   --------------
   -- Can_Move --
   --------------

   function Can_Move (Direction : Direction_E) return Boolean is
   begin
      return Grid.Can_Move (Direction);
   end Can_Move;

   ----------
   -- Move --
   ----------

   procedure Move (Direction : Direction_E) is
      Previous_Grid : constant CGrid := Grid;
      Trace         : Trace_Grid_T;
      --  trace to be used for graphical move, storing where each cell goes
   begin
      Trace := Move (Grid, Direction);
      Init_Slide (Previous_Grid, Trace);
   end Move;

   ----------------
   -- Is_Sliding --
   ----------------

   function Is_Sliding return Boolean is
   begin
      return Sliding;
   end Is_Sliding;

   -----------------
   -- Treat_Touch --
   -----------------

   procedure Treat_Touch (V : TP.Touch_Vector)
   is
      Vx       : constant Integer := V.End_Point.X - V.Start_Point.X;
      Vy       : constant Integer := V.End_Point.Y - V.Start_Point.Y;
      Vx2      : constant Integer := Vx * Vx;
      Vy2      : constant Integer := Vy * Vy;

   begin

      if Vx2 > 500 or else Vy2 > 500 then
         if Vx2 > Vy2 then
            --  left or right
            if Vx > 0 then
               if Can_Move (Right) then
                  Move (Right);
               end if;
            else
               if Can_Move (Left) then
                  Move (Left);
               end if;
            end if;
         else
            --  top or down
            if Vy > 0 then
               if Can_Move (Down) then
                  Move (Down);
               end if;
            else
               if Can_Move (Up) then
                  Move (Up);
               end if;
            end if;
         end if;
      end if;
   end Treat_Touch;


end Game;
