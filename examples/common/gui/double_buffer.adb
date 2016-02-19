------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

with System;    use System;

with STM32.LCD; use STM32.LCD, STM32;
with STM32.SDRAM; use STM32.SDRAM;

package body Double_Buffer is

--     type Layer_Activation_Type is
--       (Layer_Inactive,
--        Layer_Single_Buffer,
--        Layer_Double_Buffer);
--
--     Background : STM32.LCD.LCD_Layer renames STM32.LCD.Layer1;
--     Foreground : STM32.LCD.LCD_Layer renames STM32.LCD.Layer2;
--
--     type LCD_Configuration is record
--        Layer_Background     : Layer_Activation_Type := Layer_Single_Buffer;
--        Layer_Foreground     : Layer_Activation_Type := Layer_Inactive;
--     end record;

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
            Frame_Buffers (Layer1, FB2) := STM32.SDRAM.Reserve (FB_Size);
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
            Frame_Buffers (Layer2, FB2) := STM32.SDRAM.Reserve (FB_Size);
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
