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

with Bitmapped_Drawing;    use Bitmapped_Drawing;
with HAL.Bitmap;           use HAL.Bitmap;

package Status is

   procedure Init_Area (Buffer : HAL.Bitmap.Bitmap_Buffer'Class);

   procedure Progress (Pct : Float);
   procedure Clear_Progress;

   procedure Set_Score
     (Score : Natural);

   function Has_Buttons return Boolean;

   procedure Set_Autoplay
     (State : Boolean);

   procedure Set_Autoplay_Depth
     (Depth : Natural);

   function Get_Autoplay_Btn_Area return Rect;
   function Get_Autoplay_Depth_Btn_Area return Rect;
   function Get_Autoplay_Depth (X, Y : Natural) return Natural;

end Status;
