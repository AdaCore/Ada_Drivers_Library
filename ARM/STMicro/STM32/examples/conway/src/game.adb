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

package body Game is

   function Step (G0 : Grid) return Grid is

      function Count_Neighbors (I, J : Integer) return Natural
        with Pre  => I in G0'Range(1) and
                     J in G0'Range(2),
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
            if Neighbors (Idx).X < G0'First (1) then
               Neighbors (Idx).X := G0'Last (1);
            end if;
            if Neighbors (Idx).X > G0'Last (1) then
               Neighbors (Idx).X := G0'First (1);
            end if;
            if Neighbors (Idx).Y < G0'First (2) then
               Neighbors (Idx).Y := G0'Last (2);
            end if;
            if Neighbors (Idx).Y > G0'Last (2) then
               Neighbors (Idx).Y := G0'First (2);
            end if;

            --  If our neighbor is alive, increment the count.
            if G0 (Neighbors (Idx).X, Neighbors (Idx).Y) = Alive then
               Count := Count + 1;
            end if;
         end loop;

         return Count;
      end Count_Neighbors;

      Neighbors : Natural;
      G : Grid (G0'Range(1), G0'Range(2)) := G0;

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
      for I in G0'Range(1) loop
         for J in G0'Range(2) loop

            Neighbors := Count_Neighbors (I, J);

            case G0 (I, J) is

               when Alive =>
                  if Neighbors < 2 or Neighbors > 3 then
                     G (I, J) := Dead;
                  end if;

               when Dead =>
                  if Neighbors = 3 then
                     G (I, J) := Alive;
                  end if;

            end case;

         end loop;
      end loop;

      return G;
   end Step;

end Game;
