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

with HAL.Bitmap;            use HAL.Bitmap;

with Bitmapped_Drawing;     use Bitmapped_Drawing;
with Hershey_Fonts.FuturaL; use Hershey_Fonts;

with Grid; use Grid;
with Ada.Real_Time;    use Ada.Real_Time;
with TP;

package Game is

   Times : constant Hershey_Font := Read (FuturaL.Font);

   Grid : CGrid := (Grid  => (others => (others => 0)),
                    Score => 0);
   procedure Init;
   procedure Init_Background_Buffer;

   function Get_Score return Natural;
   function Get_Status_Area return Rect;

   procedure Init_Cells_Buffer;
   procedure Draw (Dst : Bitmap_Buffer'Class);
   procedure Init_Slide (Old_Grid : CGrid; Trace : Trace_Grid_T);
   function Slide (Dst : Bitmap_Buffer'Class) return Boolean;
   procedure Start;
   procedure Add_Value;
   function Can_Move (Direction : Direction_E) return Boolean;
   --   procedure Move (Direction : Direction_E);
   procedure Move (Direction : Direction_E);
   function Is_Sliding return Boolean;
   procedure Treat_Touch (V : TP.Touch_Vector);


private

   type Speed is record
      X : Integer;
      Y : Integer;
   end record;

   type Moving_Cell_T is record
      Src            : Point;
      Dst            : Point;
      Src_Value      : Integer;
      Dst_Value      : Integer;
      V              : Speed;
      Max_Length     : Integer;
      Moving         : Boolean := False;
   end record;
   type Moving_Cells_Index_T is new Integer
     range 0 .. Integer (Size'Range_Length * Size'Range_Length - 1);
   type Moving_Cells_A is array (Moving_Cells_Index_T) of Moving_Cell_T;


   Moving_Cells : Moving_Cells_A;


   Sliding                : Boolean := False;
   Slide_Start_Time       : Time;


end Game;
