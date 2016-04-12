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

with System;               use System;
with Interfaces.Bit_Types; use Interfaces.Bit_Types;

with STM32.LCD;            use STM32.LCD, STM32;
with STM32.DMA2D;          use STM32.DMA2D;
with STM32.SDRAM;          use STM32.SDRAM;

package body Double_Buffer is

   type FB_Type is (FB1, FB2);
   Visible_FB : FB_Type := FB1;

   Frame_Buffers : array (STM32.LCD.LCD_Layer, FB_Type) of System.Address :=
                     (others => (others => Null_Address));

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Layer_Background : Layer_Activation_Type := Layer_Single_Buffer;
      Layer_Foreground : Layer_Activation_Type := Layer_Inactive)
   is
      FB_Size : constant Word :=
                  Word (Pixel_Width *
                        Pixel_Height * Bytes_Per_Pixel (Get_Pixel_Fmt));
   begin
      if not STM32.LCD.Initialized then
         raise Constraint_Error with
           "LCD needs to be initialized before the double buffer";
      end if;

      case Layer_Background is
         when Layer_Inactive =>
            STM32.LCD.Set_Layer_State (Layer1, Disabled);
         when Layer_Single_Buffer =>
            STM32.LCD.Set_Layer_State (Layer1, Enabled);
            Frame_Buffers (Layer1, FB1) := STM32.LCD.Current_Frame_Buffer (Layer1);
         when Layer_Double_Buffer =>
            STM32.LCD.Set_Layer_State (Layer1, Enabled);
            Frame_Buffers (Layer1, FB1) := STM32.LCD.Current_Frame_Buffer (Layer1);
            Frame_Buffers (Layer1, FB2) := STM32.SDRAM.Reserve (FB_Size, 32);
      end case;

      case Layer_Foreground is
         when Layer_Inactive =>
            STM32.LCD.Set_Layer_State (Layer2, Disabled);
         when Layer_Single_Buffer =>
            STM32.LCD.Set_Layer_State (Layer2, Enabled);
            Frame_Buffers (Layer2, FB1) := STM32.LCD.Current_Frame_Buffer (Layer2);
         when Layer_Double_Buffer =>
            STM32.LCD.Set_Layer_State (Layer2, Enabled);
            Frame_Buffers (Layer2, FB1) := STM32.LCD.Current_Frame_Buffer (Layer2);
            Frame_Buffers (Layer2, FB2) := STM32.SDRAM.Reserve (FB_Size, 32);
      end case;
   end Initialize;

   ------------------
   -- Swap_Buffers --
   ------------------

   procedure Swap_Buffers (VSync : Boolean := True)
   is
   begin
      if Visible_FB = FB1 then
         Visible_FB := FB2;
      else
         Visible_FB := FB1;
      end if;

      for Layer in STM32.LCD.LCD_Layer loop
         if Frame_Buffers (Layer, Visible_FB) /= System.Null_Address then
            Set_Frame_Buffer (Layer, Frame_Buffers (Layer, Visible_FB));
         end if;
      end loop;

      STM32.DMA2D.DMA2D_Wait_Transfer;
      Reload_Config (Immediate => not VSync);
   end Swap_Buffers;

   ------------------------
   -- Get_Visible_Buffer --
   ------------------------

   function Get_Visible_Buffer
     (Layer : STM32.LCD.LCD_Layer := Background)
      return STM32.DMA2D.DMA2D_Buffer
   is
   begin
      if Frame_Buffers (Layer, Visible_FB) = Null_Address then
         --  Only one buffer accessible here
         return Get_Hidden_Buffer (Layer);
      end if;

      return (Addr       => Frame_Buffers (Layer, Visible_FB),
              Width      => Pixel_Width,
              Height     => Pixel_Height,
              Color_Mode => Get_Pixel_Fmt,
              Swap_X_Y   => SwapXY);

   end Get_Visible_Buffer;

   -----------------------
   -- Get_Hidden_Buffer --
   -----------------------

   function Get_Hidden_Buffer
     (Layer : STM32.LCD.LCD_Layer := Background)
      return STM32.DMA2D.DMA2D_Buffer
   is
      Hidden : constant FB_Type := (if Visible_FB = FB1 then FB2 else FB1);
   begin
      if Frame_Buffers (Layer, Hidden) = Null_Address then
         --  Only one buffer accessible here
         return Get_Visible_Buffer (Layer);
      end if;

      return (Addr       => Frame_Buffers (Layer, Hidden),
              Width      => Pixel_Width,
              Height     => Pixel_Height,
              Color_Mode => Get_Pixel_Fmt,
              Swap_X_Y   => SwapXY);

   end Get_Hidden_Buffer;

end Double_Buffer;
