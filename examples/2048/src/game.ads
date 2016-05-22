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
