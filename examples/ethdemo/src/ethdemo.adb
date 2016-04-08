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

with Ada.Real_Time;
with STM32.RNG.Interrupts;     use STM32.RNG.Interrupts;
with Ada.Unchecked_Conversion;
with Interfaces;               use Interfaces;
with Interfaces.Bit_Types;     use Interfaces.Bit_Types;
with STM32.LCD;                use STM32.LCD;
with STM32.DMA2D.Polling;
with STM32.Eth;
with Lcd_Std_Out;              use Lcd_Std_Out;

procedure Ethdemo is

   Format : constant Pixel_Format := Pixel_Fmt_RGB888;

   type Cell is (Alive, Dead)
     with Size => 1;

   type Grid is
     array (Integer range <>, Integer range <>) of Cell
     with Pack;

   G, G2 : Grid (0 .. 199, 0 .. 199);

   procedure Draw_Grid (L : LCD_Layer);
   pragma Unreferenced (Draw_Grid);
   procedure Randomize_Grid;
   pragma Unreferenced (Randomize_Grid);
   procedure Step;
   pragma Unreferenced (Step);

   ---------------
   -- Draw_Grid --
   ---------------

   procedure Draw_Grid (L : LCD_Layer) is
   begin
      case Format is
         when Pixel_Fmt_ARGB1555 =>
            declare
               Colors : constant array (Cell) of Short :=
                 (Alive => 16#ffff#, Dead => 16#801f#);
            begin
               for I in G'Range (1) loop
                  for J in G'Range (2) loop
                     Set_Pixel (L, I, J, Colors (G (I, J)));
                  end loop;
               end loop;
            end;
         when Pixel_Fmt_ARGB8888 =>
            declare
               Colors : constant array (Cell) of Word :=
                 (Alive => 16#ffffffff#, Dead => 16#ff0000ff#);
            begin
               for I in G'Range (1) loop
                  for J in G'Range (2) loop
                     Set_Pixel (L, I, J, Colors (G (I, J)));
                  end loop;
               end loop;
            end;
         when Pixel_Fmt_RGB888 =>
            declare
               Colors : constant array (Cell) of Uint24 :=
                 (Alive => 16#ffffff#, Dead => 16#ff0000#);
            begin
               for I in G'Range (1) loop
                  for J in G'Range (2) loop
                     Set_Pixel (L, I, J, Colors (G (I, J)));
                  end loop;
               end loop;
            end;
         when others =>
            raise Program_Error;
      end case;
   end Draw_Grid;

   --------------------
   -- Randomize_Grid --
   --------------------

   procedure Randomize_Grid is

      type Cell_Array is array (0 .. 31) of Cell with Pack;
      function To_Cell_Array is
        new Ada.Unchecked_Conversion
          (Source => Unsigned_32,
           Target => Cell_Array);

      Tmp : Unsigned_32;
   begin
      --  Start with a blank canvas.
      G := (others => (others => Dead));

      for I in G'Range (1) loop
         Tmp := Random;
         for J in G'Range (2) loop
            if J mod 32 = 0 then
               Tmp := Random;
            end if;
            G (I, J) := To_Cell_Array (Tmp)(J mod 32);
         end loop;
      end loop;
   end Randomize_Grid;

   ----------
   -- Step --
   ----------

   procedure Step is

      function Count_Neighbors (I, J : Integer) return Natural
        with Pre  => I in G'Range(1) and
                     J in G'Range(2),
             Post => Count_Neighbors'Result in 0 .. 8;

      ---------------------
      -- Count_Neighbors --
      ---------------------

      function Count_Neighbors (I, J : Integer) return Natural is
         type Coordinates is record
            X : Integer;
            Y : Integer;
         end record;

         Neighbors : array (1 .. 8) of Coordinates :=
           ( (I - 1, J - 1), (I, J - 1), (I + 1, J - 1),
             (I - 1, J    ),             (I + 1, J    ),
             (I - 1, J + 1), (I, J + 1), (I + 1, J + 1) );
         --  Enumerate all of the coordinates we're supposed to check.

         Count : Natural := 0;
      begin
         for Idx in Neighbors'Range loop
            --  Implement wraparound for coordinates that end up off
            --  our grid.
            if Neighbors (Idx).X < G'First (1) then
               Neighbors (Idx).X := G'Last (1);
            end if;
            if Neighbors (Idx).X > G'Last (1) then
               Neighbors (Idx).X := G'First (1);
            end if;
            if Neighbors (Idx).Y < G'First (2) then
               Neighbors (Idx).Y := G'Last (2);
            end if;
            if Neighbors (Idx).Y > G'Last (2) then
               Neighbors (Idx).Y := G'First (2);
            end if;

            --  If our neighbor is alive, increment the count.
            if G (Neighbors (Idx).X, Neighbors (Idx).Y) = Alive then
               Count := Count + 1;
            end if;
         end loop;

         return Count;
      end Count_Neighbors;

      Neighbors : Natural;

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

      G2 := G;

      for I in G'Range(1) loop
         for J in G'Range(2) loop

            Neighbors := Count_Neighbors (I, J);

            case G (I, J) is

               when Alive =>
                  if Neighbors < 2 or Neighbors > 3 then
                     G2 (I, J) := Dead;
                  end if;

               when Dead =>
                  if Neighbors = 3 then
                     G2 (I, J) := Alive;
                  end if;

            end case;

         end loop;
      end loop;

      G := G2;
   end Step;

   subtype String4 is String (1 .. 4);

   Hex : constant array (Natural range 0 .. 15) of Character :=
     "0123456789ABCDEF";

   function Hex_Image (V : Unsigned_16) return String4
   is
      Res : String4;
   begin
      for I in Res'Range loop
         Res (I) := Hex (Natural (Shift_Right (V, (4 - I) * 4) and 15));
      end loop;
      return Res;
   end Hex_Image;


begin

   Initialize_RNG;
   STM32.LCD.Initialize (Format);
   STM32.LCD.Set_Orientation (Portrait);
   STM32.DMA2D.Polling.Initialize;

   Set_Font (Default_Font);
   Clear_Screen;
   Put_Line ("Hello");

   if False then
      declare
         use Ada.Real_Time;
         T : Time := Clock;
         P : constant Time_Span := Seconds (1);
      begin
         for I in 1 .. 10 loop
            T := T + P;
            delay until T;
            Put ("Time #");
            Put (Hex (I));
            New_Line;
         end loop;
      end;
   end if;

   STM32.Eth.Initialize_RMII;

   --  Wait until link is up.
   declare
      use Stm32.Eth;
      use Ada.Real_Time;
      T : Time := Clock;
      P : constant Time_Span := Seconds (2);
      V : Unsigned_16;
   begin
      loop
         Read_MMI (1, V);
         exit when (V and 4) /= 0;
         Clear_Screen;
         Put_Line ("Eth link is down");
         delay until T;
         T := T + P;
      end loop;
   end;

   Clear_Screen;
   Put_Line ("Starting");

   Stm32.Eth.Init_Mac;
   Stm32.Eth.Start_Rx;

   declare
      Pkt_Count : Unsigned_16;
   begin
      Pkt_Count := 0;
      loop
         Stm32.Eth.Wait_Packet;
         Pkt_Count := Pkt_Count + 1;
         Put (0, 0, Hex_Image (Pkt_Count));
      end loop;
   end;
end Ethdemo;
