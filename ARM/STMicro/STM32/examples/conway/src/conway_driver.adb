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

with Ada.Real_Time;
with STM32.RNG.Interrupts;     use STM32.RNG.Interrupts;
with Ada.Unchecked_Conversion;
with Interfaces;
with STM32_SVD;
with STM32.LCD; use STM32.LCD;
with STM32.DMA2D.Polling;

package body Conway_Driver is

   Format : constant Pixel_Format := Pixel_Fmt_ARGB1555;

   type Cell is (Alive, Dead)
     with Size => 1;

   type Grid is
     array (Integer range <>, Integer range <>) of Cell
     with Pack;

   G, G2 : Grid (0 .. 199, 0 .. 199);

   procedure Draw_Grid;
   procedure Randomize_Grid;
   procedure Step;

   ---------------
   -- Draw_Grid --
   ---------------

   procedure Draw_Grid is
   begin
      case Format is
         when Pixel_Fmt_ARGB1555 =>
            declare
               Colors : constant array (Cell) of Stm32.Half_Word :=
                 (Alive => 16#ffff#, Dead => 16#801f#);
            begin
               for I in G'Range (1) loop
                  for J in G'Range (2) loop
                     Set_Pixel (Layer1, I, J, Colors (G (I, J)));
                  end loop;
               end loop;
            end;
         when Pixel_Fmt_ARGB8888 =>
            declare
               Colors : constant array (Cell) of Stm32.Word :=
                 (Alive => 16#ffffffff#, Dead => 16#ff0000ff#);
            begin
               for I in G'Range (1) loop
                  for J in G'Range (2) loop
                     Set_Pixel (Layer1, I, J, Colors (G (I, J)));
                  end loop;
               end loop;
            end;
         when Pixel_Fmt_RGB888 =>
            declare
               Colors : constant array (Cell) of STM32_SVD.Uint24 :=
                 (Alive => 16#ffffff#, Dead => 16#ff0000#);
            begin
               for I in G'Range (1) loop
                  for J in G'Range (2) loop
                     Set_Pixel (Layer1, I, J, Colors (G (I, J)));
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
      use Interfaces;

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

   ------------
   -- Driver --
   ------------

   task body Driver is
      use Ada.Real_Time;

      Period : constant Time_Span := Milliseconds (250);
      --  The amount of time to wait between displaying iterations.

      Next_Activation : Time;
   begin
      Initialize_RNG;
      STM32.LCD.Initialize (Format);
      STM32.LCD.Set_Orientation (Portrait);
      STM32.DMA2D.Polling.Initialize;

      Randomize_Grid;
      STM32.DMA2D.DMA2D_Fill
        (Buffer =>
           (Addr       => STM32.LCD.Current_Frame_Buffer (Layer1),
            Width      => STM32.LCD.Pixel_Width,
            Height     => STM32.LCD.Pixel_Height,
            Color_Mode => STM32.LCD.Get_Pixel_Fmt,
            Swap_X_Y   => STM32.LCD.SwapXY),
         Color => 0);

      Next_Activation := Clock + Period;
      loop
         delay until Next_Activation;
         Next_Activation := Next_Activation + Period;

         Step;
         Draw_Grid;
      end loop;
   end Driver;

end Conway_Driver;
