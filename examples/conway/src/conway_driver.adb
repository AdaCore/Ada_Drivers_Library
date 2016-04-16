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

with Ada.Unchecked_Conversion;
with Interfaces;               use Interfaces;
with Interfaces.Bit_Types;     use Interfaces.Bit_Types;
with System;                   use System;

with STM32.Button;             use STM32.Button;
with STM32.LCD;                use STM32.LCD;
with STM32.DMA2D.Polling;      use STM32.DMA2D;
with STM32.RNG.Interrupts;     use STM32.RNG.Interrupts;
with STM32.SDRAM;

with Double_Buffer;            use Double_Buffer;
with Bitmapped_Drawing;        use Bitmapped_Drawing;
with Cortex_M.Cache;

package body Conway_Driver is

   type Width is mod LCD_Natural_Width;
   type Height is mod LCD_Natural_Height;

   type Pattern is record
      W   : Natural;
      H   : Natural;
      RLE : access constant String;
   end record;

   Ptrn1_RLE : aliased constant String :=
     "2bo14bo34bo5b$o2bo2b3o7bob4o15b2o11b2obo4b$o2bo12b3obobo9bo5bo7bo3bo7b" &
     "$bobo2bo21b3o2bobob3o5bobobo4bo3b$2b2o6bo4bo16b6o2bo2bo3bo3bo2bo3b$bob" &
     "o3b2o5bo3b2o6b2ob2o2bo2b2o2bobob2o2bo9b$2bo5bob2o2bo3b2o6b2o4bobobo2bo" &
     "2bobobo6b2o2bo$2bo4b3o2bo9b3o7b3obob2o5bo7b3obo$12b9o3bo8b2ob3o3b4o9bo" &
     "bo2$12b9o3bo8b2ob3o3b4o9bobo$2bo4b3o2bo9b3o7b3obob2o5bo7b3obo$2bo5bob" &
     "2o2bo3b2o6b2o4bobobo2bo2bobobo6b2o2bo$bobo3b2o5bo3b2o6b2ob2o2bo2b2o2bo" &
     "bob2o2bo9b$2b2o6bo4bo16b6o2bo2bo3bo3bo2bo3b$bobo2bo21b3o2bobob3o5bobob" &
     "o4bo3b$o2bo12b3obobo9bo5bo7bo3bo7b$o2bo2b3o7bob4o15b2o11b2obo4b$2bo14b" &
     "o34bo!";
   Ptrn1 : constant Pattern :=
             (W   => 58,
              H   => 19,
              RLE => Ptrn1_RLE'Access);

   Ptrn2_RLE : aliased constant String :=
     "4o2b2o3b2ob2o3bo$2obo2b2ob3obobo2b2o$bob2o4bo5b2ob2o$bo4bo4bo4bob2o$o" &
     "5bobobo4bobo$b3obo2b5obobob2o$ob2o2b4obob2ob4o$2bo6b2obobobobo$b4o4bo" &
     "4b2obobo$b2o6b3o3b3o$bo3bo2bo6b5o$3o3bo2bo4bob3o$b2obobob2o4b4o$obo2b" &
     "2obo2bobob4o$2o4bobob2obo2bobo$b2o3bobob7o$o3bobo6b3o2b2o$2b2o2bo2bobo" &
     "b2obo2bo$3bobobobobo4b2o$2o2b2obobo2bo!";
   Ptrn2 : constant Pattern :=
             (W   => 20,
              H   => 20,
              RLE => Ptrn2_RLE'Access);

   --  Name:   23334M
   --  Author: Tomas Rokicki
   Ptrn3_RLE : aliased constant String :=
                 "2bo2b$2o3b$bo3b$o2bob$4bo$bo2bo$2bobo$bo!";
   Ptrn3 : constant Pattern :=
             (W   => 5,
              H   => 8,
              RLE => Ptrn3_RLE'Access);

   --  Name:   Rabbits
   --  Author: Andrew Trevorrow
   Ptrn4_RLE : aliased constant String :=
                 "o3b3o$3o2bob$bo!";
   Ptrn4     : constant Pattern :=
                 (W   => 7,
                  H   => 3,
                  RLE => Ptrn4_RLE'Access);

   --  Name:   7468M
   --  Author: Tomas Rokicki
   Ptrn5_RLE : aliased constant String :=
                 "4bob$4b2o$2ob2ob$o!";
   Ptrn5 : constant Pattern :=
             (W   => 6,
              H   => 4,
              RLE => Ptrn5_RLE'Access);

   --  Name: Acorn
   --  Author Charles Coderman
   Ptrn6_RLE : aliased constant String :=
                 "bo5b$3bo3b$2o2b3o!";
   Ptrn6 : constant Pattern :=
             (W   => 7,
              H   => 3,
              RLE => Ptrn6_RLE'Access);

   --  Name: Blom
   --  Author: Dean Hickerson
   Ptrn7_RLE : aliased constant String :=
                 "o10bo$b4o6bo$2b2o7bo$10bob$8bobo!";
   Ptrn7     : constant Pattern :=
                   (W   => 12,
                    H   => 5,
                    RLE => Ptrn7_RLE'Access);

   --  Name: Bunnies
   --  Author: Robert Wainwright and Andrew Trevorrow
   Ptrn8_RLE : aliased constant String :=
                 "o5bob$2bo3bob$2bo2bobo$bobo!";
   Ptrn8     : constant Pattern :=
                   (W   => 8,
                    H   => 4,
                    RLE => Ptrn8_RLE'Access);

   Patterns  : constant array (Positive range <>) of Pattern :=
                 (Ptrn1, Ptrn2, Ptrn3, Ptrn4, Ptrn5, Ptrn6, Ptrn7, Ptrn8);

   type Cell_State is (Dead, Alive)
     with Size => 1;
   type Neighbor_Cnt is range 0 .. 8
     with Size => 7;

   type Cell is record
      State    : Cell_State := Dead;
      Neighbor : Neighbor_Cnt := 0;
   end record with Pack, Size => 8;

   type Line is array (Width) of Cell;

   type Grid is array (Height) of Line;
   type Grid_Access is access all Grid;

   function To_Grid is new Ada.Unchecked_Conversion
     (System.Address, Grid_Access);

   G, G2, Tmp : Grid_Access;

   Format : constant Pixel_Format := Pixel_Fmt_RGB565;
   Colors : constant array (Cell_State) of Word :=
     (case Format is
         when Pixel_Fmt_ARGB1555 => (Alive => 16#ffff#, Dead => 16#801f#),
         when Pixel_Fmt_ARGB4444 => (Alive => 16#ffff#, Dead => 16#f00f#),
         when Pixel_Fmt_ARGB8888 => (Alive => 16#ffffffff#,
                                     Dead => 16#ff0000ff#),
         when Pixel_Fmt_RGB565   => (Alive => 16#ffff#, Dead => 16#001f#),
         when Pixel_Fmt_RGB888   => (Alive => 16#ffffff#, Dead => 16#0000ff#));

   Buffer : DMA2D_Buffer;

   procedure Draw_Grid;
   function Init_Grid (Ptrn : Pattern) return Boolean;
   procedure Step;

   ---------------
   -- Draw_Grid --
   ---------------

   procedure Draw_Grid is
   begin
      for Y in Grid'Range loop
         for X in Line'Range loop
            DMA2D_Set_Pixel
              (Buffer, Natural (X), Natural (Y),
               Colors (G (Y) (X).State));
         end loop;
      end loop;
   end Draw_Grid;

   ---------------
   -- Init_Grid --
   ---------------

   function Init_Grid (Ptrn : Pattern) return Boolean
   is
      function Count_Neighbors
        (Y : Height;
         X : Width) return Neighbor_Cnt
        with Inline;

      function To_Width (S : String) return Width;

      ---------------------
      -- Count_Neighbors --
      ---------------------

      function Count_Neighbors
        (Y : Height;
         X : Width) return Neighbor_Cnt
      is
         type Coordinates is record
            X : Width;
            Y : Height;
         end record;

         Neighbors : constant array (1 .. 8) of Coordinates :=
           ((X - 1, Y - 1), (X, Y - 1), (X + 1, Y - 1),
            (X - 1, Y),                 (X + 1, Y),
            (X - 1, Y + 1), (X, Y + 1), (X + 1, Y + 1));
         --  Enumerate all of the coordinates we're supposed to check.

         Count : Neighbor_Cnt := 0;
      begin
         for Idx in Neighbors'Range loop
            --  Implement wraparound for coordinates that end up off
            --  our grid.
            if G (Neighbors (Idx).Y) (Neighbors (Idx).X).State = Alive then
               Count := Count + 1;
            end if;
         end loop;

         return Count;
      end Count_Neighbors;

      --------------
      -- To_Width --
      --------------

      function To_Width (S : String) return Width
      is
         Ret : Width := 0;
         Tmp : Width;
      begin
         for C of S loop
            Tmp := Character'Pos (C) - Character'Pos ('0');
            Ret := Ret * 10 + Tmp;
         end loop;

         return Ret;
      end To_Width;

   begin

      G.all := (others => (others => (Dead, 0)));

      if Ptrn.W > Line'Length
        or else Ptrn.H > Grid'Length
      then
         return False;
      end if;

      declare
         Y : Height := Height (Natural (Grid'Length) - Ptrn.H) / 2;
         X : Width;
         Length : Width;
         C      : Character;
         Idx    : Natural := Ptrn.RLE'First;
         Cur    : Natural;
      begin
         Main_Loop :
         loop
            X := Width (Natural (Line'Length) - Ptrn.W) / 2;

            Line_Loop :
            loop
               Cur := Idx;
               while Ptrn.RLE (Cur) in '0' .. '9' loop
                  Cur := Cur + 1;
               end loop;

               if Cur = Idx then
                  Length := 1;
               else
                  Length := To_Width (Ptrn.RLE (Idx .. Cur - 1));
               end if;

               C := Ptrn.RLE (Cur);

               Idx := Cur + 1;

               if C = 'b' then
                  X := X + Length;

               elsif C = 'o' then
                  for J in 1 .. Length loop
                     G (Y) (X + J - 1).State := Alive;
                  end loop;
                  X := X + Length;

               elsif C = '$' then
                  exit Line_Loop;

               elsif C = '!' then
                  exit Main_Loop;
               end if;
            end loop Line_Loop;

            Y := Y + 1;
         end loop Main_Loop;
      end;

      --  We now count for each cell the number of active neighbours
      for Y in Grid'Range loop
         for X in  Line'Range loop
            G (Y) (X).Neighbor := Count_Neighbors (Y, X);
         end loop;
      end loop;

      Draw_Grid;

      return True;
   end Init_Grid;

   ----------
   -- Step --
   ----------

   procedure Step is

      type Coordinates is record
         X : Width;
         Y : Height;
      end record;

      procedure Update_Neighbors
        (X     : Width;
         Y     : Height;
         State : Cell_State);

      ----------------------
      -- Update_Neighbors --
      ----------------------

      procedure Update_Neighbors
        (X     : Width;
         Y     : Height;
         State : Cell_State)
      is
         Neighbors : constant array (1 .. 8) of Coordinates :=
                       ((X - 1, Y - 1), (X, Y - 1), (X + 1, Y - 1),
                        (X - 1, Y),                 (X + 1, Y),
                        (X - 1, Y + 1), (X, Y + 1), (X + 1, Y + 1));

         procedure Modif_Neighbor_Cnt (Idx : Natural; Amount : Integer)
           with Inline;

         ----------------
         -- Modif_Grid --
         ----------------

         procedure Modif_Neighbor_Cnt (Idx : Natural; Amount : Integer)
         is
            Val : Neighbor_Cnt renames
                    G2 (Neighbors (Idx).Y) (Neighbors (Idx).X).Neighbor;
         begin
            Val := Neighbor_Cnt (Integer (Val) + Amount);
         end Modif_Neighbor_Cnt;

      begin
         for Idx in Neighbors'Range loop
            --  Implement wraparound for coordinates that end up off
            --  our grid.
            if State = Alive then
               Modif_Neighbor_Cnt (Idx, 1);
            else
               Modif_Neighbor_Cnt (Idx, -1);
            end if;
         end loop;
      end Update_Neighbors;

      function As_Byte is new Ada.Unchecked_Conversion (Cell, Byte);
   begin
      --
      --  For every cell in the Grid, we'll determine whether or not the
      --  cell is alive based on Conway's rules (from Wikipedia):
      --
      --  1. Any live cell with fewer than two live neighbours dies, as
      --     if caused by under-population.
      --
      --  2. Any live cell with two or three live neighbours lives on to
      --     the next generation.
      --
      --  3. Any live cell with more than three live neighbours dies, as
      --     if by overcrowding.
      --
      --  4. Any dead cell with exactly three live neighbours becomes a
      --     live cell, as if by reproduction.
      --

      --  Init the next generation from the current one: we'll only update
      --  it, not recalculate from scratch
      G2.all := G.all;

      for Y in Grid'Range loop
         for X in Line'Range loop
            if As_Byte (G (Y) (X)) /= 0 then
               case G (Y) (X).State is
                  when Alive =>
                     if G (Y) (X).Neighbor not in 2 .. 3 then
                        G2 (Y) (X).State := Dead;
                        Update_Neighbors (X, Y, Dead);
                        Put_Pixel
                          (Buffer, (Natural (X), Natural (Y)),
                           Colors (Dead));
                     end if;

                  when Dead =>
                     if G (Y) (X).Neighbor = 3 then
                        G2 (Y) (X).State := Alive;
                        Update_Neighbors (X, Y, Alive);
                        Put_Pixel
                          (Buffer, (Natural (X), Natural (Y)),
                           Colors (Alive));
                     end if;
               end case;
            end if;
         end loop;
      end loop;

      Cortex_M.Cache.Clean_DCache
        (Buffer.Addr,
         Len =>
           Buffer.Width * Buffer.Height * Bytes_Per_Pixel (Buffer.Color_Mode));

      --  Swap buffers
      Tmp := G;
      G := G2;
      G2 := Tmp;
   end Step;

   ------------
   -- Driver --
   ------------

   task body Driver
   is
      Ptrn : Positive := Patterns'First;
      Res  : Boolean;
   begin
      Initialize_RNG;
      STM32.LCD.Initialize (Format);
--        STM32.LCD.Set_Orientation (Portrait);
      STM32.DMA2D.Polling.Initialize;
      Double_Buffer.Initialize
        (Layer_Background => Double_Buffer.Layer_Double_Buffer,
         Layer_Foreground => Double_Buffer.Layer_Inactive);
      STM32.SDRAM.Initialize;
      STM32.Button.Initialize;

      DMA2D_Fill (Double_Buffer.Get_Visible_Buffer (Background), Color => 0);
      DMA2D_Fill (Double_Buffer.Get_Hidden_Buffer (Background), Color => 0);

      G  := To_Grid (STM32.SDRAM.Reserve (Grid'Size / 8));
      G2 := To_Grid (STM32.SDRAM.Reserve (Grid'Size / 8));

      loop
         Buffer := Get_Hidden_Buffer (Background);
         loop
            Res := Init_Grid (Patterns (Ptrn));

            Ptrn := Ptrn + 1;
            if Ptrn > Patterns'Last then
               Ptrn := Patterns'First;
            end if;

            exit when Res;
         end loop;

         Swap_Buffers;

         loop
            Buffer := Get_Hidden_Buffer (Background);
            DMA2D_Copy_Rect
              (Double_Buffer.Get_Visible_Buffer (Background),
               0, 0,
               Buffer,
               0, 0,
               Null_Buffer,
               0, 0,
               Buffer.Width, Buffer.Height,
               Synchronous => True);

            Step;

            Swap_Buffers (True);

            exit when STM32.Button.Has_Been_Pressed;
         end loop;
      end loop;
   end Driver;

end Conway_Driver;
