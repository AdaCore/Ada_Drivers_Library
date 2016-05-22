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

--  This code is based on the solver by nneonneo in github:
--  https://github.com/nneonneo/2048-ai
--  No specific license is described there, so I assumed some MIT-like license
--  to apply. If not, please contact us immediately.
--  Adapted to Ada and restricted targets, but the core of the algo and the
--  constants are directly taken from there.

with Interfaces;                        use Interfaces;
with Ada.Containers.Hashed_Maps;
with Ada.Unchecked_Conversion;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Malloc;
with Game;
with Grid;
with Status;

package body Solver is

   type Cell_T is mod 2 ** 4 with Size => 4;

   type Row_T is array (1 .. 4) of Cell_T with Pack, Size => 16;
   type Board_T is array (1 .. 4) of Row_T with Pack, Size => 64;

   function Hash (Board : Board_T) return Ada.Containers.Hash_Type;
   function To_DWord is new Ada.Unchecked_Conversion
     (Board_T, Unsigned_64);
   function To_Board is new Ada.Unchecked_Conversion
     (Unsigned_64, Board_T);

   G_Progress    : array (0 .. 2) of Natural := (others => 0);

   -------------------
   -- GLOBAL TABLES --
   -------------------

   type Score_Table is array (Unsigned_16) of Float with Pack;

   type Translation_Table is array (Unsigned_16) of Unsigned_16 with Pack;

   Row_Left_Table   : access Translation_Table;
   Row_Right_Table  : access Translation_Table;
   Heur_Score_Table : access Score_Table;

   --  Heuristic scoring settings
   SCORE_LOST_PENALITY       : constant Float := 200000.0;
   SCORE_MONOTONICITY_POWER  : constant Float := 4.0;
   SCORE_MONOTONICITY_WEIGHT : constant Float := 47.0;
   SCORE_SUM_POWER           : constant Float := 3.5;
   SCORE_SUM_WEIGHT          : constant Float := 11.0;
   SCORE_MERGES_WEIGHT       : constant Float := 700.0;
   SCORE_EMPTY_WEIGHT        : constant Float := 270.0;

   procedure Update_Progress;
   function Reverse_Row (Row : Row_T) return Row_T;
   function Move_Up (Board : Board_T) return Board_T;
   function Move_Down (Board : Board_T) return Board_T;
   function Move_Left (Board : Board_T) return Board_T;
   function Move_Right (Board : Board_T) return Board_T;
   function Execute_Move
     (Direction : Valid_Move_Type; Board : Board_T) return Board_T;
   function Score_Helper
     (Board : Board_T;
      Table : access Score_Table) return Float;

   ---------------------
   -- Update_Progress --
   ---------------------

   procedure Update_Progress is
      Pct : Float := 0.0;
   begin
      for J in G_Progress'Range loop
         Pct := Pct + (0.25 ** Float (J + 1)) * Float (G_Progress (J));
      end loop;

      Status.Progress (Pct);
   end Update_Progress;

   -----------------------
   -- UTILITY FUNCTIONS --
   -----------------------

   function Transpose (Board : Board_T) return Board_T
     with Inline;

   function Count_Empty (Board : Board_T) return Natural
     with Inline;

   ----------
   -- Hash --
   ----------

   function Hash (Board : Board_T) return Ada.Containers.Hash_Type
   is
      use Ada.Containers;
      UInt : constant Unsigned_64 := To_DWord (Board);
   begin
      return Hash_Type (Shift_Right (UInt, 32)) xor
        Hash_Type (UInt and 16#FFFF_FFFF#);
   end Hash;

   --------------
   -- To_Int16 --
   --------------

   function To_Int16 is new Ada.Unchecked_Conversion
     (Row_T, Unsigned_16);

   ------------
   -- To_Row --
   ------------

   function To_Row is new Ada.Unchecked_Conversion
     (Unsigned_16, Row_T);

   -----------------
   -- Reverse_Row --
   -----------------

   function Reverse_Row (Row : Row_T) return Row_T
   is
   begin
      return To_Row
        (Shift_Left (Unsigned_16 (Row (1)), 12) +
         Shift_Left (Unsigned_16 (Row (2)), 8) +
         Shift_Left (Unsigned_16 (Row (3)), 4) +
         Unsigned_16 (Row (4)));
   end Reverse_Row;

   ---------------
   -- Transpose --
   ---------------

   function Transpose (Board : Board_T) return Board_T
   is
      Dw            : constant Unsigned_64 := To_DWord (Board);
      A1, A2, A3, A : Unsigned_64;
      B1, B2, B3    : Unsigned_64;
   begin
      --  0123      048c
      --  4567  ==> 159d
      --  89ab      26ae
      --  cdef      37bf
      A1 := Dw and 16#F0F0_0F0F_F0F0_0F0F#;
      A2 := Dw and 16#0000_F0F0_0000_F0F0#;
      A3 := Dw and 16#0F0F_0000_0F0F_0000#;
      A := A1 or Shift_Left (A2, 12) or Shift_Right (A3, 12);
      B1 := A and 16#FF00_FF00_00FF_00FF#;
      B2 := A and 16#00FF_00FF_0000_0000#;
      B3 := A and 16#0000_0000_FF00_FF00#;
      return To_Board (B1 or Shift_Right (B2, 24) or Shift_Left (B3, 24));
   end Transpose;

   -----------------
   -- Count_Empty --
   -----------------

   function Count_Empty (Board : Board_T) return Natural
   is
      Dw : Unsigned_64 := To_DWord (Board);
   begin
      Dw := Dw or (Shift_Right (Dw, 2) and 16#3333_3333_3333_3333#);
      Dw := Dw or Shift_Right (Dw, 1);
      Dw := (not Dw) and 16#1111_1111_1111_1111#;
      --  At this point, each cell is 0 if the original cell in non zero,
      --  else 1. We thus just need to sum them up
      Dw := Dw + Shift_Right (Dw, 32);
      Dw := Dw + Shift_Right (Dw, 16);
      Dw := Dw + Shift_Right (Dw, 8);
      Dw := Dw + Shift_Right (Dw, 4);
      --  Result valid only if the board is non-empty: else 16 empty cell
      --  overflow the 16#F# mask.
      --  But empty boards never happen.
      return Natural (Dw and 16#F#);
   end Count_Empty;

   -----------------
   -- Init_Solver --
   -----------------

   procedure Init_Solver
   is
   begin
      Row_Left_Table   := new Translation_Table;
      Row_Right_Table  := new Translation_Table;
      Heur_Score_Table := new Score_Table;

      --  Precalculation of various results for every variations of a given
      --  row or column
      for Row in Unsigned_16'Range loop
         Status.Progress (Float (Row) / Float (Unsigned_16'Last));

         declare
            Line               : Row_T := To_Row (Row);
            Rev_Row            : constant Unsigned_16 :=
                                   To_Int16 (Reverse_Row (Line));
            Rank               : Cell_T;
            Sum                : Float;
            Empty              : Natural;
            Merges             : Natural;
            Prev               : Cell_T;
            Counter            : Natural;
            Monotonicity_Left  : Float;
            Monotonicity_Right : Float;
            Result             : Unsigned_16;
            Rev_Result         : Unsigned_16;

         begin
            --  Heuristic score
            Sum     := 0.0;
            Empty   := 0;
            Merges  := 0;
            Prev    := 0;
            Counter := 0;

            for J in Line'Range loop
               Rank := Line (J);
               Sum := Sum + Float (Rank) ** SCORE_SUM_POWER;

               if Rank = 0 then
                  Empty := Empty + 1;
               else
                  if Prev = Rank then
                     Counter := Counter + 1;
                  elsif Counter > 0 then
                     Merges := Merges + 1 + Counter;
                     Counter := 0;
                  end if;

                  Prev := Rank;
               end if;
            end loop;

            if Counter > 0 then
               Merges := Merges + 1 + Counter;
            end if;

            Monotonicity_Left := 0.0;
            Monotonicity_Right := 0.0;

            for J in Line'First + 1 .. Line'Last loop
               if Line (J - 1) > Line (J) then
                  Monotonicity_Left := Monotonicity_Left +
                    Float (Line (J - 1)) ** SCORE_MONOTONICITY_POWER -
                    Float (Line (J)) ** SCORE_MONOTONICITY_POWER;
               else
                  Monotonicity_Right := Monotonicity_Right +
                    Float (Line (J)) ** SCORE_MONOTONICITY_POWER -
                    Float (Line (J - 1)) ** SCORE_MONOTONICITY_POWER;
               end if;
            end loop;

            Heur_Score_Table (Row) :=
              SCORE_LOST_PENALITY +
              (SCORE_EMPTY_WEIGHT * Float (Empty)) +
              (SCORE_MERGES_WEIGHT * Float (Merges)) -
              (SCORE_MONOTONICITY_WEIGHT *
                 Float'Min (Monotonicity_Left, Monotonicity_Right)) -
              (SCORE_SUM_WEIGHT * Sum);

            --  Execute a move to the left
            declare
               Idx1, Idx2 : Natural;
            begin
               Idx1 := Line'First;
               while Idx1 <= Line'Last - 1 loop
                  Idx2 := Idx1 + 1;
                  while Idx2 <= Line'Last loop
                     exit when Line (Idx2) /= 0;
                     Idx2 := Idx2 + 1;
                  end loop;

                  exit when Idx2 > Line'Last;

                  if Line (Idx1) = 0 then
                     Line (Idx1) := Line (Idx2);
                     Line (Idx2) := 0;
                     --  We don't move Idx1 to retry this entry
                  elsif Line (Idx1) = Line (Idx2) then
                     if Line (Idx1) /= 16#F# then
                        --  16#F# is the highest represented value: 32768
                        --  So in case we achieve that, we cap the value
                        Line (Idx1) := Line (Idx1) + 1;
                     end if;
                     Line (Idx2) := 0;
                  else
                     Idx1 := Idx1 + 1;
                  end if;
               end loop;
            end;

            Result := To_Int16 (Line);
            Rev_Result := To_Int16 (Reverse_Row (Line));

            Row_Left_Table (Row)      := Result;
            Row_Right_Table (Rev_Row) := Rev_Result;
         end;
      end loop;

      Status.Clear_Progress;
   end Init_Solver;

   -------------
   -- Move_Up --
   -------------

   function Move_Up (Board : Board_T) return Board_T
   is
      T : constant Board_T := Transpose (Board);
      Ret : Board_T;
   begin
      for R in Board'Range loop
         Ret (R) := To_Row (Row_Left_Table (To_Int16 (T (R))));
      end loop;

      return Transpose (Ret);
   end Move_Up;

   ---------------
   -- Move_Down --
   ---------------

   function Move_Down (Board : Board_T) return Board_T
   is
      T : constant Board_T := Transpose (Board);
      Ret : Board_T;
   begin
      for R in Board'Range loop
         Ret (R) := To_Row (Row_Right_Table (To_Int16 (T (R))));
      end loop;

      return Transpose (Ret);
   end Move_Down;

   ---------------
   -- Move_Left --
   ---------------

   function Move_Left (Board : Board_T) return Board_T
   is
      Ret : Board_T;
   begin
      for R in Board'Range loop
         Ret (R) := To_Row (Row_Left_Table (To_Int16 (Board (R))));
      end loop;

      return Ret;
   end Move_Left;

   ----------------
   -- Move_Right --
   ----------------

   function Move_Right (Board : Board_T) return Board_T
   is
      Ret : Board_T;
   begin
      for R in Board'Range loop
         Ret (R) := To_Row (Row_Right_Table (To_Int16 (Board (R))));
      end loop;

      return Ret;
   end Move_Right;

   ------------------
   -- Execute_Move --
   ------------------

   function Execute_Move
     (Direction : Valid_Move_Type; Board : Board_T) return Board_T
   is
   begin
      case Direction is
         when Up =>
            return Move_Up (Board);
         when Down =>
            return Move_Down (Board);
         when Left =>
            return Move_Left (Board);
         when Right =>
            return Move_Right (Board);
      end case;
   end Execute_Move;

   ----------------------------------------------
   -- Cache definition to speed up calculation --
   ----------------------------------------------

   type Trans_Table_Entry_T is record
      Depth     : Natural;
      Heuristic : Float;
   end record;

   package Trans_Table_Maps is new Ada.Containers.Hashed_Maps
     (Board_T, Trans_Table_Entry_T,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Eval_State is record
      Trans_Table  : Trans_Table_Maps.Map;
      Max_Depth    : Natural := 0;
      Cur_Depth    : Natural := 0;
      Cache_Hits   : Natural := 0;
      Moves_Evaled : Unsigned_64 := 0;
      Depth_Limit  : Natural := 0;
   end record;

   --  Statistics and controls
   --  cprob: cumulative probability
   --  don't recurse into a node with a cprob less than this threshold
   CPROB_THRESH_BASE : constant Float := 0.0001;
   CACHE_DEPTH_LIMIT : constant Natural := 15;

   function Score_Heur_Board (Board : Board_T) return Float;
   function Score_Move_Node
     (State : in out Eval_State; Board : Board_T; CProb : Float) return Float;
   function Score_Tilechoose_Node
     (State : in out Eval_State; Board : Board_T; CProb : Float) return Float;

   ------------------
   -- Score_Helper --
   ------------------

   function Score_Helper
     (Board : Board_T;
      Table : access Score_Table) return Float
   is
   begin
      return Table (To_Int16 (Board (1))) +
        Table (To_Int16 (Board (2))) +
        Table (To_Int16 (Board (3))) +
        Table (To_Int16 (Board (4)));
   end Score_Helper;

   ----------------------
   -- Score_Heur_Board --
   ----------------------

   function Score_Heur_Board (Board : Board_T) return Float
   is
   begin
      return Score_Helper (Board, Heur_Score_Table) +
        Score_Helper (Transpose (Board), Heur_Score_Table);
   end Score_Heur_Board;

   ---------------------------
   -- Score_Tilechoose_Node --
   ---------------------------

   function Score_Tilechoose_Node
     (State : in out Eval_State;
      Board : Board_T;
      CProb : Float) return Float
   is
      Cursor      : Trans_Table_Maps.Cursor;
      Num_Open    : Natural;
      Res         : Float;
      Tmp         : Board_T;
      N_CProb     : Float := CProb;
      use type Ada.Containers.Count_Type;

   begin
      if CProb < CPROB_THRESH_BASE
        or else State.Cur_Depth >= State.Depth_Limit
      then
         State.Max_Depth := Natural'Max (State.Cur_Depth, State.Max_Depth);
         return Score_Heur_Board (Board);
      end if;

      Cursor := State.Trans_Table.Find (Board);
      if Trans_Table_Maps.Has_Element (Cursor) then
         declare
            Ent : Trans_Table_Entry_T renames
                    Trans_Table_Maps.Element (Cursor);
         begin
            if Ent.Depth <= State.Cur_Depth then
               State.Cache_Hits := State.Cache_Hits + 1;
               return Ent.Heuristic;
            end if;
         end;
      end if;

      Num_Open := Count_Empty (Board);
      N_CProb := N_CProb / Float (Num_Open);

      Res := 0.0;
      Tmp := Board;

      for Col in Board_T'Range loop
         for Row in Row_T'Range loop
            if not Solver_Enabled then
               return 0.0;
            end if;

            if State.Cur_Depth = 0 and then State.Depth_Limit > 2 then
               Update_Progress;
               G_Progress (2) := G_Progress (2) + 1;
            end if;
            if Board (Col) (Row) = 0 then
               Tmp (Col) (Row) := 1;
               Res := Res + Score_Move_Node (State, Tmp, N_CProb * 0.9) * 0.9;
               Tmp (Col) (Row) := 2;
               Res := Res + Score_Move_Node (State, Tmp, N_CProb * 0.1) * 0.1;
               Tmp (Col) (Row) := 0;
            end if;
         end loop;

         if State.Cur_Depth = 0 then
            G_Progress (2) := 0;
            G_Progress (1) := G_Progress (1) + 1;
         end if;
      end loop;

      if State.Cur_Depth = 0 then
         G_Progress (1) := 0;
      end if;

      Res := Res / Float (Num_Open);

      if State.Cur_Depth < CACHE_DEPTH_LIMIT
        and then not Malloc.Is_Full
        and then not State.Trans_Table.Contains (Board)
      then
         State.Trans_Table.Insert
           (Board,
            (Depth     => State.Cur_Depth,
             Heuristic => Res));
      end if;

      return Res;
   end Score_Tilechoose_Node;

   ---------------------
   -- Score_Move_Node --
   ---------------------

   function Score_Move_Node
     (State : in out Eval_State; Board : Board_T; CProb : Float) return Float
   is
      Best : Float := 0.0;
   begin
      State.Cur_Depth := State.Cur_Depth + 1;

      for Move in Valid_Move_Type'Range loop
         if not Solver_Enabled then
            return 0.0;
         end if;

         declare
            N_Board : constant Board_T := Execute_Move (Move, Board);
         begin
            State.Moves_Evaled := State.Moves_Evaled + 1;

            if Board /= N_Board then
               Best := Float'Max
                 (Best, Score_Tilechoose_Node (State, N_Board, CProb));
            end if;
         end;
      end loop;

      State.Cur_Depth := State.Cur_Depth - 1;

      return Best;
   end Score_Move_Node;

   G_Best_Previous : Float := 2.0E6;

   ---------------
   -- Next_Move --
   ---------------

   function Next_Move return Move_Type is
      Ret     : Move_Type := None;
      Best    : Float := 0.0;
      Res     : Float;
      Board   : Board_T;
      N_Board : Board_T;
      State   : Eval_State;
      Val     : Natural;

   begin
      for Row in 0 .. 3 loop
         for Col in reverse 0 .. 3 loop
            Val := Grid.Get (Game.Grid, Grid.Size (Col), Grid.Size (Row));
            if Val > Natural (Cell_T'Last) then
               Board (Row + 1) (Col + 1) := Cell_T'Last;
            else
               Board (Row + 1) (Col + 1) := Cell_T (Val);
            end if;
         end loop;
      end loop;

      --  Start malloc fast mode: preallocate 4MB buffer, and do not free
      --  until next call.
      Malloc.Start_Fast_Malloc (5 * 1024 * 1024);

      --  Start with a low value: 2, then up 1 at each high score starting 4096
      --  Speed up at first, thinking a bit more when tere's too many tiles
      Res := G_Best_Previous;
      if Res > 1.5E6 or else Maximum_Depth = 2 then
         State.Depth_Limit := 2;
      elsif Res > 1.3E6 or else Maximum_Depth = 3 then
         State.Depth_Limit := 3;
      elsif Res > 1.1E6 or else Maximum_Depth = 4 then
         State.Depth_Limit := 4;
      else
         State.Depth_Limit := 5;
         --  We don't have enough room to go deeper on this board.
      end if;

      G_Progress := (others => 0);

      for Move in Valid_Move_Type'Range loop
         exit when not Solver_Enabled;

         N_Board := Execute_Move (Move, Board);
         if N_Board /= Board then
            Res := Score_Tilechoose_Node (State, N_Board, 1.0) + 0.000001;
         else
            Res := 0.0;
         end if;

         G_Progress (0) := G_Progress (0) + 1;

         if Res > Best then
            Best := Res;
            Ret  := Move;
            G_Best_Previous := Best;
         end if;
      end loop;

      State.Trans_Table.Clear;

      Status.Clear_Progress;

      if Solver_Enabled then
         return Ret;
      else
         return None;
      end if;
   end Next_Move;

end Solver;
