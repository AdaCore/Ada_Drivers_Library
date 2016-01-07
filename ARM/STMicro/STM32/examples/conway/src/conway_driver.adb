------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

with Ada.Unchecked_Conversion;
with Interfaces;               use Interfaces;
with System;                   use System;

with STM32.Button;             use STM32.Button;
with STM32.LCD;                use STM32.LCD;
with STM32.DMA2D.Polling;      use STM32.DMA2D;
with STM32.RNG.Interrupts;     use STM32.RNG.Interrupts;
with STM32.SDRAM;

with Double_Buffer;            use Double_Buffer;

package body Conway_Driver is
   use STM32;

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
                 "4bo6bo4b$3bobob2obobo3b$3b2o2b2o2b2o3b$7b2o7b2$5b2o2b2o5b$5b2o2b2o5b$" &
                 "6b4o6b$6bo2bo6b$5bo4bo5b$5bo4bo5b$5bob2obo5b$6bo2bo6b$6bo2bo6b2$o14bo$" &
                 "2o4bo2bo4b2o$o5b4o5bo$b2o4b2o4b2ob$2bo10bo2b$obo10bobo$b5o4b5ob$3bo2b" &
                 "4o2bo3b$2bo4b2o4bo2b$6bo2bo6b$7b2o7b$4b2ob2ob2o4b2$3bo8bo3b$2b3o6b3o2b" &
                 "$2bo2bo4bo2bo2b$b2o10b2o!";
   Ptrn2 : constant Pattern :=
             (W   => 16,
              H   => 32,
              RLE => Ptrn2_RLE'Access);

   --  Name:   23334M
   --  Author: Tomas Rokicki
   Ptrn3_RLE : aliased constant String :=
                 "2bo2b$2o3b$bo3b$o2bob$4bo$bo2bo$2bobo$bo!";
   Ptrn3 : constant Pattern :=
             (W   => 5,
              H   => 8,
              RLE => Ptrn3_RLE'Access);

   --  Name:   7468M
   --  Author: Tomas Rokicki
   Ptrn4_RLE : aliased constant String :=
                 "4bob$4b2o$2ob2ob$o!";
   Ptrn4 : constant Pattern :=
             (W   => 6,
              H   => 4,
              RLE => Ptrn4_RLE'Access);

   --  Name: Acorn
   --  Author Charles Coderman
   Ptrn5_RLE : aliased constant String :=
                 "bo5b$3bo3b$2o2b3o!";
   Ptrn5 : constant Pattern :=
             (W   => 7,
              H   => 3,
              RLE => Ptrn5_RLE'Access);

   --  Name: Blom
   --  Author: Dean Hickerson
   Ptrn6_RLE : aliased constant String :=
                 "o10bo$b4o6bo$2b2o7bo$10bob$8bobo!";
   Ptrn6     : constant Pattern :=
                   (W   => 12,
                    H   => 5,
                    RLE => Ptrn6_RLE'Access);

   Patterns  : constant array (Positive range <>) of Pattern :=
                 (Ptrn1, Ptrn2, Ptrn3, Ptrn4, Ptrn5, Ptrn6);

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

   Format : constant Pixel_Format := Pixel_Fmt_ARGB1555;
   Colors : constant array (Cell_State) of Stm32.Word :=
              (case Format is
                  when Pixel_Fmt_ARGB1555 => (Alive => 16#ffff#, Dead => 16#801f#),
                  when Pixel_Fmt_ARGB4444 => (Alive => 16#ffff#, Dead => 16#f00f#),
                  when Pixel_Fmt_ARGB8888 => (Alive => 16#ffffffff#, Dead => 16#ff0000ff#),
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
           ( (X - 1, Y - 1), (X, Y - 1), (X + 1, Y - 1),
             (X - 1, Y    ),             (X + 1, Y    ),
             (X - 1, Y + 1), (X, Y + 1), (X + 1, Y + 1) );
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

      ----------------------
      -- Update_Neighbors --
      ----------------------

      procedure Update_Neighbors
        (X     : Width;
         Y     : Height;
         State : Cell_State)
      is
         Neighbors : constant array (1 .. 8) of Coordinates :=
                       ( (X - 1, Y - 1), (X, Y - 1), (X + 1, Y - 1),
                         (X - 1, Y    ),             (X + 1, Y    ),
                         (X - 1, Y + 1), (X, Y + 1), (X + 1, Y + 1) );

      begin
         for Idx in Neighbors'Range loop
            --  Implement wraparound for coordinates that end up off
            --  our grid.
            if State = Alive then
               G2 (Neighbors (Idx).Y) (Neighbors (Idx).X).Neighbor :=
                 G2 (Neighbors (Idx).Y) (Neighbors (Idx).X).Neighbor + 1;
            else
               G2 (Neighbors (Idx).Y) (Neighbors (Idx).X).Neighbor :=
                 G2 (Neighbors (Idx).Y) (Neighbors (Idx).X).Neighbor - 1;
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

      G2.all := G.all;
      DMA2D_Copy_Rect
        (Double_Buffer.Get_Visible_Buffer (Background),
         0, 0,
         Buffer,
         0, 0,
         Null_Buffer,
         0, 0,
         Buffer.Width, Buffer.Height);

      for Y in Grid'Range loop
         for X in Line'Range loop
            if As_Byte (G (Y) (X)) /= 0 then
               case G (Y) (X).State is
                  when Alive =>
                     if G (Y) (X).Neighbor not in 2 .. 3 then
                        G2 (Y) (X).State := Dead;
                        Update_Neighbors (X, Y, Dead);
                        DMA2D_Set_Pixel
                          (Buffer, Natural (X), Natural (Y),
                           Colors (Dead));
                     end if;

                  when Dead =>
                     if G (Y) (X).Neighbor = 3 then
                        G2 (Y) (X).State := Alive;
                        Update_Neighbors (X, Y, Alive);
                        DMA2D_Set_Pixel
                          (Buffer, Natural (X), Natural (Y),
                           Colors (Alive));
                     end if;
               end case;
            end if;
         end loop;
      end loop;

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
            Step;
            Swap_Buffers (True);

            exit when STM32.Button.Has_Been_Pressed;
         end loop;
      end loop;
   end Driver;

end Conway_Driver;
