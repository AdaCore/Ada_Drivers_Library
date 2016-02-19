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
