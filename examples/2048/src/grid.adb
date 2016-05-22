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

package body Grid is

   function Can_Move_Row (Row : in Row_T) return Boolean;
   function Can_Move_Rows (Rows : in Rows_T) return Boolean;

   ----------
   -- Init --
   ----------

   procedure Init (G : in out CGrid) is
   begin
      G.Grid := (others => (others => 0));
      G.Score := 0;
   end Init;

   ---------
   -- Get --
   ---------

   function Get (G : in CGrid; X : Size; Y : Size) return Integer is
   begin
      return G.Grid (X, Y);
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (G : in out CGrid; X : Size; Y : Size; Value : Integer) is
   begin
      G.Grid (X, Y) := Value;
   end Set;


   -----------------
   -- Compact_Row --
   -----------------

   procedure Compact_Row (Row : in out Row_T;
                          Score : in out Natural)
   is
      Cur_Idx : Size := 0;
      Prev_Value : Integer := 0;
   begin
      for X in Size loop
         if Row (X) /= 0 then
            if Prev_Value = Row (X) then
               --  merging cells
               Row (Cur_Idx - 1) := Row (Cur_Idx - 1) + 1;
               Score := Score + 2 ** Row (Cur_Idx - 1);
               Prev_Value := 0;
               Row (X) := 0;
            else
               --  filling blanks
               Row (Cur_Idx) := Row (X);
               Prev_Value := Row (Cur_Idx);

               if Cur_Idx /= X then
                  Row (X) := 0;
               end if;
               if Cur_Idx /= Size'Last then
                  Cur_Idx := Cur_Idx + 1;
               end if;
            end if;
         end if;
      end loop;
   end Compact_Row;

   -----------------
   -- Compact_Row --
   -----------------

   function Compact_Row (Row : in out Row_T;
                         Score : in out Natural) return Row_Trace_T
   is
      Trace : Row_Trace_T := (others => 0);
      Cur_Idx : Size := 0;
      Prev_Value : Integer := 0;
   begin
      for X in Size loop
         if Row (X) /= 0 then
            if Prev_Value = Row (X) then
               --  merging cells
               Row (Cur_Idx - 1) := Row (Cur_Idx - 1) + 1;
               Score := Score + 2 ** Row (Cur_Idx - 1);
               Trace (X) :=  Cur_Idx - 1;
               Prev_Value := 0;
               Row (X) := 0;
--               Trace (X) := 0;

            else
--               if Cur_Idx /= X then
                  --  filling blanks
               Row (Cur_Idx) := Row (X);
               Trace (X) := Cur_Idx;

               Prev_Value := Row (Cur_Idx);

               if Cur_Idx /= X then
                  Row (X) := 0;
                  --  Trace (X) := X;

               end if;
               if Cur_Idx /= Size'Last then
                  Cur_Idx := Cur_Idx + 1;
               end if;
            end if;
         else
            Trace (X) := X;
         end if;
      end loop;
      return Trace;
   end Compact_Row;

   ------------------
   -- Compact_Rows --
   ------------------

   procedure Compact_Rows (Rows  : in out Rows_T;
                           Score : in out Natural) is
   begin
      for Y in Size loop
         Compact_Row (Rows (Y), Score);
      end loop;
   end Compact_Rows;

   function Compact_Rows (Rows  : in out Rows_T;
                          Score : in out Natural) return Rows_Traces_T is
      Traces : Rows_Traces_T;
   begin
      for Y in Size loop
         Traces (Y) := Compact_Row (Rows (Y), Score);
      end loop;
      return Traces;
   end Compact_Rows;

   ------------------
   -- Can_Move_Row --
   ------------------

   function Can_Move_Row (Row : in Row_T) return Boolean
   is
      Prev_Value : Integer := 0;
      Hole : Boolean := False;
   begin
      for X in Size loop
         if Row (X) = 0 then
            Hole := True;
         else
            if Hole then
               return True;
            end if;
            if Prev_Value = Row (X) then
               return True;
            end if;
            Prev_Value := Row (X);
         end if;
      end loop;
      return False;
   end Can_Move_Row;

   -------------------
   -- Can_Move_Rows --
   -------------------

   function Can_Move_Rows (Rows : in Rows_T) return Boolean is
   begin
      for Y in Size loop
         if Can_Move_Row (Rows (Y)) then
            return True;
         end if;
      end loop;
      return False;
   end Can_Move_Rows;

   ------------------
   -- Grid_To_Rows --
   ------------------

   function Grid_To_Rows (G : in CGrid; Direction : Direction_E)  return Rows_T
   is
      Rows : Rows_T;
   begin
      case Direction is
      when Left =>
         for Y in Size loop
            for X in Size loop
               Rows (Y) (X) := G.Grid (X, Y);
            end loop;
         end loop;
      when Right =>
         for Y in Size loop
            for X in Size loop
               Rows (Y) (Size'Last - X) := G.Grid (X, Y);
            end loop;
         end loop;
      when Up =>
         for X in Size loop
            for Y in Size loop
               Rows (X) (Y) := G.Grid (X, Y);
            end loop;
         end loop;
      when Down =>
         for X in Size loop
            for Y in Size loop
               Rows (X) (Size'Last - Y) := G.Grid (X, Y);
            end loop;
         end loop;
      end case;
      return Rows;
   end Grid_To_Rows;

   ------------------
   -- Rows_To_Grid --
   ------------------

   procedure Rows_To_Grid
     (G         : in out CGrid;
      Rows      : Rows_T;
      Direction : Direction_E)
   is
   begin
      case Direction is
         when Left =>
            for Y in Size loop
               for X in Size loop
                  G.Grid (X, Y) := Rows (Y) (X);
               end loop;
            end loop;
         when Right =>
            for Y in Size loop
               for X in Size loop
                  G.Grid (X, Y) := Rows (Y) (Size'Last - X);
               end loop;
            end loop;
         when Up =>
            for X in Size loop
               for Y in Size loop
                  G.Grid (X, Y) := Rows (X) (Y);
               end loop;
            end loop;
         when Down =>
            for X in Size loop
               for Y in Size loop
                  G.Grid (X, Y) := Rows (X) (Size'Last - Y);
               end loop;
            end loop;
      end case;
   end Rows_To_Grid;

   -------------------------
   -- Traces_To_Move_Grid --
   -------------------------

   function Traces_To_Move_Grid
     (Traces    : Rows_Traces_T;
      Direction : Direction_E) return Trace_Grid_T
   is
      Move_Grid : Trace_Grid_T;
   begin
      case Direction is
         when Left =>
            for Y in Size loop
               for X in Size loop
                  Move_Grid (X, Y) := (Traces (Y) (X), Y);
               end loop;
            end loop;
         when Right =>
            for Y in Size loop
               for X in Size loop
                  Move_Grid (Size'Last - X, Y) :=
                    (Size'Last - Traces (Y) (X), Y);
               end loop;
            end loop;
         when Up =>
            for X in Size loop
               for Y in Size loop
                  Move_Grid (X, Y) := (X, Traces (X) (Y));
               end loop;
            end loop;
         when Down =>
            for X in Size loop
               for Y in Size loop
                  Move_Grid (X, Size'Last - Y) :=
                    (X, Size'Last - Traces (X) (Y));
               end loop;
            end loop;
      end case;
      return Move_Grid;
   end Traces_To_Move_Grid;

   --------------
   -- Can_Move --
   --------------

   function Can_Move
     (G         : in out CGrid;
      Direction : Direction_E) return Boolean
   is
      Rows : constant Rows_T := G.Grid_To_Rows (Direction);
   begin
      return Can_Move_Rows (Rows);
   end Can_Move;

   procedure Move (G : in out CGrid; Direction : Direction_E) is
      Rows : Rows_T := G.Grid_To_Rows (Direction);
   begin
      Compact_Rows (Rows, G.Score);
      G.Rows_To_Grid (Rows, Direction);
   end Move;

   ----------
   -- Move --
   ----------

   function Move
     (G         : in out CGrid;
      Direction : Direction_E) return Trace_Grid_T
   is
      Rows : Rows_T := G.Grid_To_Rows (Direction);
      Traces : Rows_Traces_T;
   begin
      Traces := Compact_Rows (Rows, G.Score);
      G.Rows_To_Grid (Rows, Direction);
      return Traces_To_Move_Grid (Traces, Direction);
   end Move;

end Grid;
