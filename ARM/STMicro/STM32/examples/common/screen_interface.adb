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

with STM32F4.Touch_Panel;

with STM32F4;     use STM32F4;
with STM32F4.LTDC; use STM32F4.LTDC;

package body Screen_Interface is

   Draw_Layer : LCD_Layer := Layer1;

   Initialized : Boolean := False;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if not Initialized then
         Initialized := True;

         STM32F4.LTDC.Initialize;
         STM32F4.LTDC.Set_Background (16#00#, 16#00#, 16#00#);
         STM32F4.Touch_Panel.Initialize;

         --  At init the draw layer is the one displayed, this will keep
         --  compatibility with projects not doing double buffering.

         Draw_Layer := Layer1;
         Set_Layer_State (Layer1, Enabled);
         Set_Layer_State (Layer2, Disabled);
      end if;
   end Initialize;

   ------------------
   -- Swap_Buffers --
   ------------------

   procedure Swap_Buffers is
   begin
      if Draw_Layer = Layer1 then
         Draw_Layer := Layer2;
         Set_Layer_State (Layer1, Enabled);
         Set_Layer_State (Layer2, Disabled);
      else
         Draw_Layer := Layer1;
         Set_Layer_State (Layer1, Disabled);
         Set_Layer_State (Layer2, Enabled);
      end if;
      Reload_Config (Immediate => False);
   end Swap_Buffers;

   -------------------------
   -- Current_Touch_State --
   -------------------------

   function Current_Touch_State return Touch_State is
      TS    : Touch_State;
      ST_TS : STM32F4.Touch_Panel.TP_State;
   begin
      ST_TS := STM32F4.Touch_Panel.Current_State;

      TS.Touch_Detected := ST_TS.Touch_Detected;
      TS.X := ST_TS.X;
      TS.Y := ST_TS.Y;
      return TS;
   end Current_Touch_State;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (P : Point; Col : Color) is
   begin
      STM32F4.LTDC.Set_Pixel (Draw_Layer, P.X, P.Y, Col);
   end Set_Pixel;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (X, Y : Natural; Col : Color) is
   begin
      STM32F4.LTDC.Set_Pixel (Draw_Layer, X, Y, Col);
   end Set_Pixel;

   -----------------
   -- Fill_Screen --
   -----------------

   procedure Fill_Screen (Col : Color) is
      FB : constant Frame_Buffer_Access := Current_Frame_Buffer (Draw_Layer);
   begin
      FB.all := (others => Col);
   end Fill_Screen;

   --------------
   -- As_Color --
   --------------

   function As_Color (R, G, B : RGB_Value) return Color is
      RF : constant Float := (Float (R) / 255.0) * 31.0;
      GF : constant Float := (Float (G) / 255.0) * 31.0;
      BF : constant Float := (Float (B) / 255.0) * 31.0;
   begin
      return 16#8000# or
        (Color (RF) * (2**10)) or (Color (GF) * (2**5)) or Color (BF);
   end As_Color;

end Screen_Interface;
