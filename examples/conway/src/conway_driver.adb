
with Ada.Unchecked_Conversion;
with Interfaces;               use Interfaces;
with HAL;                      use HAL;
with System;                   use System;

with STM32.Button;             use STM32.Button;
with STM32.Board;              use STM32.Board;
with STM32.RNG.Interrupts;     use STM32.RNG.Interrupts;
with STM32.SDRAM;

with HAL.Bitmap;               use HAL.Bitmap;
with HAL.Framebuffer;          use HAL.Framebuffer;

package body Conway_Driver is

   LCD_W : constant := (if LCD_Natural_Width > LCD_Natural_Height
                        then LCD_Natural_Width else LCD_Natural_Height);
   LCD_H : constant := (if LCD_Natural_Width > LCD_Natural_Height
                        then LCD_Natural_Height else LCD_Natural_Width);

   type Width is mod LCD_W;
   type Height is mod LCD_H;

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

   Format : constant HAL.Framebuffer.FB_Color_Mode := RGB_565;
   Colors : constant array (Cell_State) of UInt32 :=
     (case Format is
         when ARGB_1555 => (Alive => 16#ffff#, Dead => 16#801f#),
         when ARGB_4444 => (Alive => 16#ffff#, Dead => 16#f00f#),
         when ARGB_8888 => (Alive => 16#ffffffff#, Dead => 16#ff0000ff#),
         when RGB_565   => (Alive => 16#ffff#, Dead => 16#001f#),
         when RGB_888   => (Alive => 16#ffffff#, Dead => 16#0000ff#),
         when L_8       => (Alive => 16#00#, Dead => 16#FF#),
         when AL_44     => (Alive => 16#F0#, Dead => 16#FF#),
         when AL_88     => (Alive => 16#FF00#, Dead => 16#FFFF#));

   procedure Draw_Grid;
   function Init_Grid (Ptrn : Pattern) return Boolean;
   procedure Step;

   ---------------
   -- Draw_Grid --
   ---------------

   procedure Draw_Grid
   is
      Buffer : constant Bitmap_Buffer'Class :=
                 Display.Get_Hidden_Buffer (1);
   begin
      for Y in Grid'Range loop
         for X in Line'Range loop
            Buffer.Set_Pixel
              (Natural (X), Natural (Y),
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

      Buffer : constant Bitmap_Buffer'Class :=
                 Display.Get_Hidden_Buffer (1);
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
                        Buffer.Set_Pixel
                          (Natural (X), Natural (Y),
                           Colors (Dead));
                     end if;

                  when Dead =>
                     if G (Y) (X).Neighbor = 3 then
                        G2 (Y) (X).State := Alive;
                        Update_Neighbors (X, Y, Alive);
                        Buffer.Set_Pixel
                          (Natural (X), Natural (Y),
                           Colors (Alive));
                     end if;
               end case;
            end if;
         end loop;
      end loop;

      Display.Update_Layer (1, Copy_Back => True);

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
      Display.Initialize (Orientation => Landscape);
      Display.Initialize_Layer (1, Format);
      STM32.Button.Initialize;

      G  := To_Grid (STM32.SDRAM.Reserve (Grid'Size / 8));
      G2 := To_Grid (STM32.SDRAM.Reserve (Grid'Size / 8));

      loop
         loop
            Res := Init_Grid (Patterns (Ptrn));

            Ptrn := Ptrn + 1;
            if Ptrn > Patterns'Last then
               Ptrn := Patterns'First;
            end if;

            exit when Res;
         end loop;

         loop
            Step;

            exit when STM32.Button.Has_Been_Pressed;
         end loop;
      end loop;
   end Driver;

end Conway_Driver;
