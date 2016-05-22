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

with Bitmapped_Drawing; use Bitmapped_Drawing;

package TP is

   Max_Buttons : constant := 2;
   --  The maximum number of buttons handled by this packge

   type Touch_Vector is record
      Start_Point : Point;
      End_Point   : Point;
   end record;

   procedure Update;

   type Button_Callback is access procedure (X, Y : Natural);
   --  Called when a Button area has been clicked.
   --  Coord contains the click position relative to the button area

   procedure Add_Button_Area
     (Area     : Rect;
      Callback : Button_Callback);

   type Slide_Callback is access procedure (Vector : Touch_Vector);

   procedure Set_Slide_Callback
     (Callback : Slide_Callback);

end TP;
