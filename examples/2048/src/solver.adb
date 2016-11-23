------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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
--     3. Neither the name of the copyright holder nor the names of its     --
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

with STM32.Board;

package body Solver is

   type Cell_T is mod 2 ** 4 with Size => 4;

   type Row_T is array (1 .. 4) of Cell_T with Pack, Size => 16;
   type Board_T is array (1 .. 4) of Row_T with Pack, Size => 64;

   type Board_32 is record
      Row_12 : Unsigned_32;
      Row_34 : Unsigned_32;
   end record with Size => 64;

   for Board_32 use record
      Row_12 at 0 range 0 .. 31;
      Row_34 at 4 range 0 .. 31;
   end record;

   function Hash (Board : Board_T) return Ada.Containers.Hash_Type;

   function To_UInt16 is new Ada.Unchecked_Conversion
     (Row_T, Unsigned_16);
   function To_Row is new Ada.Unchecked_Conversion
     (Unsigned_16, Row_T);
   function To_UInt64 is new Ada.Unchecked_Conversion
     (Board_T, Unsigned_64);
   function To_Board is new Ada.Unchecked_Conversion
     (Unsigned_64, Board_T);
   function To_Board32 is new Ada.Unchecked_Conversion
     (Board_T, Board_32);
   function To_Board is new Ada.Unchecked_Conversion
     (Board_32, Board_T);

   G_Progress    : array (0 .. 2) of Natural := (others => 0);

   -------------------
   -- GLOBAL TABLES --
   -------------------

   type Score_Table is array (Unsigned_16) of Float with Pack;

   type Row_Translation_Table is array (Unsigned_16) of Unsigned_16 with Pack;
   type Col_Translation_Table is array (Unsigned_16) of Unsigned_64 with Pack;

   Row_Left_Table   : access Row_Translation_Table;
   Row_Right_Table  : access Row_Translation_Table;
   Col_Up_Table     : access Col_Translation_Table;
   Col_Down_Table   : access Col_Translation_Table;
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
   function Reverse_Row (Row : Row_T) return Row_T with Inline_Always;
   function Move_Up (Board : Board_T) return Board_T with Inline_Always;
   function Move_Down (Board : Board_T) return Board_T with Inline_Always;
   function Move_Left (Board : Board_T) return Board_T with Inline_Always;
   function Move_Right (Board : Board_T) return Board_T with Inline_Always;
   function Execute_Move
     (Direction : Valid_Move_Type; Board : Board_T)
      return Board_T with Inline_Always;

   -----------------------
   -- UTILITY FUNCTIONS --
   -----------------------

   function Transpose (Board : Board_T) return Board_T with Inline_Always;

   function Count_Empty (Board : Board_T) return Natural
     with Inline_Always;

   function Count_Distinct_Tiles (Board : Board_T) return Natural;

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

   function Score_Heur_Board (Board : Board_T) return Float with Inline_Always;
   function Score_Move_Node
     (State : in out Eval_State; Board : Board_T; CProb : Float) return Float;
   function Score_Tilechoose_Node
     (State : in out Eval_State; Board : Board_T; CProb : Float) return Float;

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

   ----------
   -- Hash --
   ----------

   function Hash (Board : Board_T) return Ada.Containers.Hash_Type
   is
      use Ada.Containers;
      UInt : constant Unsigned_64 := To_UInt64 (Board);
   begin
      return Hash_Type (Shift_Right (UInt, 32)) xor
        Hash_Type (UInt and 16#FFFF_FFFF#);
   end Hash;

   -----------------
   -- Reverse_Row --
   -----------------

   function Reverse_Row (Row : Row_T) return Row_T
   is
   begin
      return To_Row
        (Shift_Left (Unsigned_16 (Row (1)), 12) or
         Shift_Left (Unsigned_16 (Row (2)), 8) or
         Shift_Left (Unsigned_16 (Row (3)), 4) or
         Unsigned_16 (Row (4)));
   end Reverse_Row;

   ---------------
   -- Transpose --
   ---------------

   function Transpose (Board : Board_T) return Board_T
   is
      B32           : constant Board_32 := To_Board32 (Board);
      A1, A2, A3, A : Board_32;
      B             : Board_32;

   begin
      --  0123      048c
      --  4567  ==> 159d
      --  89ab      26ae
      --  cdef      37bf

      --  if Board is ((0, 1, 2, 3), (4, 5, 6, 7), (8, 9, a, b), (c, d, e, f))
      --  the underlying representation will be:
      --  Row_12 => 0x76543210
      --  Row_34 => 0xfedcba98
      A1 := (Row_12 => B32.Row_12 and 16#F0F0_0F0F#,            --  0_2__5_7
             Row_34 => B32.Row_34 and 16#F0F0_0F0F#);           --  8_a__d_f
      A2 := (Row_12 =>
               Shift_Right (B32.Row_12 and 16#0F0F_0000#, 12),  --  _4_6____
             Row_34 =>
               Shift_Right (B32.Row_34 and 16#0F0F_0000#, 12)); --  _c_e____
      A3 := (Row_12 =>
               Shift_Left (B32.Row_12 and 16#0000_F0F0#, 12),   --  ____1_3_
             Row_34 =>
               Shift_Left (B32.Row_34 and 16#0000_F0F0#, 12));  --  ____9_b_
      A  := (Row_12 => A1.Row_12 or A2.Row_12 or A3.Row_12,     --  04261537
             Row_34 => A1.Row_34 or A2.Row_34 or A3.Row_34);    --  8cae9dbf

      --  And now assemble the final value
      B.Row_12 := (A.Row_12 and 16#00FF_00FF#) or
        Shift_Left (A.Row_34 and 16#00FF_00FF#, 8); --  048c159d
      B.Row_34 := (A.Row_34 and 16#FF00_FF00#) or
        Shift_Right (A.Row_12 and 16#FF00_FF00#, 8);  --  26ae37bf

      return To_Board (B);
   end Transpose;

   -----------------
   -- Count_Empty --
   -----------------

   function Count_Empty (Board : Board_T) return Natural
   is
      Dw : Unsigned_64 := To_UInt64 (Board);
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

   --------------------------
   -- Count_Distinct_Tiles --
   --------------------------

   function Count_Distinct_Tiles (Board : Board_T) return Natural
   is
      Bitset : Unsigned_16 := 0;
      Count  : Natural := 0;
   begin
      for Row of Board loop
         for Cell of Row loop
            Bitset := Bitset or Shift_Left (Unsigned_16'(1), Natural (Cell));
         end loop;
      end loop;

      Bitset := Shift_Right (Bitset, 1);

      while Bitset > 0 loop
         Bitset := Bitset and (Bitset - 1);
         Count := Count + 1;
      end loop;

      return Count;
   end Count_Distinct_Tiles;

   -----------------
   -- Init_Solver --
   -----------------

   procedure Init_Solver
   is
      function Unpack_Col (Row : Unsigned_16) return Unsigned_64 is
        (16#000F_000F_000F_000F# and
           (Unsigned_64 (Row)
            or Shift_Left (Unsigned_64 (Row), 12)
            or Shift_Left (Unsigned_64 (Row), 24)
            or Shift_Left (Unsigned_64 (Row), 36)));
   begin
      Row_Left_Table   := new Row_Translation_Table;
      Row_Right_Table  := new Row_Translation_Table;
      Col_Up_Table     := new Col_Translation_Table;
      Col_Down_Table   := new Col_Translation_Table;
      Heur_Score_Table := new Score_Table;

      --  Precalculation of various results for every variations of a given
      --  row or column
      for Row in Unsigned_16'Range loop
         Status.Progress (Float (Row) / Float (Unsigned_16'Last));

         declare
            Line               : Row_T := To_Row (Row);
            Rev_Row            : constant Unsigned_16 :=
                                   To_UInt16 (Reverse_Row (Line));
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
                     Idx1 := Idx1 + 1;
                  else
                     Idx1 := Idx1 + 1;
                  end if;
               end loop;
            end;

            Result     := To_UInt16 (Line);
            Rev_Result := To_UInt16 (Reverse_Row (Line));

            Row_Left_Table (Row)      := Result;
            Row_Right_Table (Rev_Row) := Rev_Result;
            Col_Up_Table (Row)        := Unpack_Col (Result);
            Col_Down_Table (Rev_Row)  := Unpack_Col (Rev_Result);
         end;
      end loop;

      Status.Clear_Progress;
   end Init_Solver;

   -------------
   -- Move_Up --
   -------------

   function Move_Up (Board : Board_T) return Board_T
   is
      T   : constant Board_T := Transpose (Board);

   begin
      return To_Board
        (Col_Up_Table (To_UInt16 (T (1)))
         or Shift_Left (Col_Up_Table (To_UInt16 (T (2))), 4)
         or Shift_Left (Col_Up_Table (To_UInt16 (T (3))), 8)
         or Shift_Left (Col_Up_Table (To_UInt16 (T (4))), 12));
   end Move_Up;

   ---------------
   -- Move_Down --
   ---------------

   function Move_Down (Board : Board_T) return Board_T
   is
      T   : constant Board_T := Transpose (Board);
      Ret : Unsigned_64 := To_UInt64 (Board);
   begin
      Ret := Col_Down_Table (To_UInt16 (T (1)))
        or Shift_Left (Col_Down_Table (To_UInt16 (T (2))), 4)
        or Shift_Left (Col_Down_Table (To_UInt16 (T (3))), 8)
        or Shift_Left (Col_Down_Table (To_UInt16 (T (4))), 12);

      return To_Board (Ret);
   end Move_Down;

   ---------------
   -- Move_Left --
   ---------------

   function Move_Left (Board : Board_T) return Board_T
   is
      Ret : Board_32;
   begin
      Ret :=
        (Row_12 => Unsigned_32 (Row_Left_Table (To_UInt16 (Board (1))))
           or Shift_Left (Unsigned_32 (Row_Left_Table (To_UInt16 (Board (2)))), 16),
         Row_34 => Unsigned_32 (Row_Left_Table (To_UInt16 (Board (3))))
           or Shift_Left (Unsigned_32 (Row_Left_Table (To_UInt16 (Board (4)))), 16));

      return To_Board (Ret);
   end Move_Left;

   ----------------
   -- Move_Right --
   ----------------

   function Move_Right (Board : Board_T) return Board_T
   is
      Ret : Board_32;
   begin
      Ret :=
        (Unsigned_32 (Row_Right_Table (To_UInt16 (Board (1))))
         or Shift_Left (Unsigned_32 (Row_Right_Table (To_UInt16 (Board (2)))), 16),
         Unsigned_32 (Row_Right_Table (To_UInt16 (Board (3))))
         or Shift_Left (Unsigned_32 (Row_Right_Table (To_UInt16 (Board (4)))), 16));

      return To_Board (Ret);
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

   ----------------------
   -- Score_Heur_Board --
   ----------------------

   function Score_Heur_Board (Board : Board_T) return Float
   is
      T : constant Board_T := Transpose (Board);
   begin
      return Heur_Score_Table (To_UInt16 (Board (1))) +
        Heur_Score_Table (To_UInt16 (Board (2))) +
        Heur_Score_Table (To_UInt16 (Board (3))) +
        Heur_Score_Table (To_UInt16 (Board (4))) +
        Heur_Score_Table (To_UInt16 (T (1))) +
        Heur_Score_Table (To_UInt16 (T (2))) +
        Heur_Score_Table (To_UInt16 (T (3))) +
        Heur_Score_Table (To_UInt16 (T (4)));
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
            if Board (Row) (Col) = 0 then
               Tmp (Row) (Col) := 1;
               Res := Res + Score_Move_Node (State, Tmp, N_CProb * 0.9) * 0.9;
               Tmp (Row) (Col) := 2;
               Res := Res + Score_Move_Node (State, Tmp, N_CProb * 0.1) * 0.1;
               Tmp (Row) (Col) := 0;
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
      Malloc.Start_Fast_Malloc
        (STM32.Board.SDRAM_Size - Malloc.Allocated - 128 -
           Standard'Maximum_Alignment);

      State.Depth_Limit :=
        Integer'Max (2, Count_Distinct_Tiles (Board) / 2 - 1);

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
