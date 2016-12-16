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

package Grid is
   type Size is new Integer range 0 .. 3;

   type Grid_T is array (Size, Size) of Integer;

   type Direction_E is (Left, Up, Right, Down);

   type CGrid is tagged record
      Grid  : Grid_T;
      Score : Natural;
   end record;


   type Cell_Position_T is record
      X : Size;
      Y : Size;
   end record;
   type Moving_Tile_T is record
      Current : Cell_Position_T;
      Previous : Cell_Position_T;
   end record;
   type Trace_Grid_T is array (Size, Size) of Cell_Position_T;

   procedure Init (G : in out CGrid);

   function Get
     (G : in CGrid;
      X : Size;
      Y : Size) return Integer;

   procedure Set
     (G     : in out CGrid;
      X     : Size;
      Y     : Size;
      Value : Integer);

   procedure Move
     (G         : in out CGrid;
      Direction : Direction_E);

   function Move
     (G         : in out CGrid;
      Direction : Direction_E) return Trace_Grid_T;

   function Can_Move
     (G         : in out CGrid;
      Direction : Direction_E) return Boolean;

private
   type Row_T is array (Size) of Integer;
   type Row_Trace_T is array (Size) of Size;

   type Rows_T is array (Size) of Row_T;

   type Rows_Traces_T is array (Size) of Row_Trace_T;

   procedure Compact_Row (Row   : in out Row_T;
                          Score : in out Natural);

   function Compact_Row (Row   : in out Row_T;
                         Score : in out Natural) return Row_Trace_T;

   procedure Compact_Rows (Rows  : in out Rows_T;
                           Score : in out Natural);

   function Compact_Rows (Rows  : in out Rows_T;
                          Score : in out Natural) return Rows_Traces_T;

   function Grid_To_Rows
     (G         : in CGrid;
      Direction : Direction_E)  return Rows_T;

   procedure Rows_To_Grid
     (G         : in out CGrid;
      Rows      : Rows_T;
      Direction : Direction_E);

   function Traces_To_Move_Grid
     (Traces    : Rows_Traces_T;
      Direction : Direction_E) return Trace_Grid_T;

end Grid;
