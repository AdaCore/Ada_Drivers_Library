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

   type LCD_Configuration is record
      Layer_Background     : Layer_Activation_Type := Layer_Single_Buffer;
      Layer_Foreground     : Layer_Activation_Type := Layer_Inactive;
   end record;

   procedure Initialize
     (Config : LCD_Configuration);

   procedure Swap_Buffers (VSync : Boolean := True);

   function Get_Visible_Buffer
     (Layer : STM32.LCD.LCD_Layer := Background)
      return STM32.DMA2D.DMA2D_Buffer;

   function Get_Hidden_Buffer
     (Layer : STM32.LCD.LCD_Layer := Background)
      return STM32.DMA2D.DMA2D_Buffer;

end Double_Buffer;
