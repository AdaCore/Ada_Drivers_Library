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

--  This file provides subprograms to initialize and use double buffering
--  for GUI management.

with STM32.DMA2D;
with STM32.LCD;

package Double_Buffer is

   type Layer_Activation_Type is
     (Layer_Inactive,
      Layer_Single_Buffer,
      Layer_Double_Buffer);

   Background : STM32.LCD.LCD_Layer renames STM32.LCD.Layer1;
   Foreground : STM32.LCD.LCD_Layer renames STM32.LCD.Layer2;

   procedure Initialize
     (Layer_Background : Layer_Activation_Type := Layer_Single_Buffer;
      Layer_Foreground : Layer_Activation_Type := Layer_Inactive);

   procedure Swap_Buffers (VSync : Boolean := True);

   function Get_Visible_Buffer
     (Layer : STM32.LCD.LCD_Layer := Background)
      return STM32.DMA2D.DMA2D_Buffer;

   function Get_Hidden_Buffer
     (Layer : STM32.LCD.LCD_Layer := Background)
      return STM32.DMA2D.DMA2D_Buffer;

end Double_Buffer;
