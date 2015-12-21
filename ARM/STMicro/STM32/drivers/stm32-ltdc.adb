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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32f429i_discovery_lcd.c                                    --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   This file includes the LTDC driver to control LCD display.    --         --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Real_Time;  use Ada.Real_Time;

with STM32_SVD.LTDC; use STM32_SVD.LTDC;
with STM32_SVD.RCC;  use STM32_SVD.RCC;

with STM32.SDRAM;    use STM32.SDRAM;
with STM32.RCC;      use STM32.RCC;

package body STM32.LTDC is

   Frame_Buffer_Array : array (LCD_Layer) of Frame_Buffer_Access;
   Current_Pixel_Fmt : Pixel_Format := Pixel_Fmt_ARGB1555;

   type FB32 is array (Natural range 0 .. LCD_Height - 1,
                       Natural range 0 .. LCD_Width - 1) of Word
     with Component_Size => 32, Volatile;
   type FB24 is array (Natural range 0 .. LCD_Height - 1,
                       Natural range 0 .. (LCD_Width * 3) - 1) of Byte
     with Component_Size => 8, Volatile;
   type FB16 is array (Natural range 0 .. LCD_Height - 1,
                       Natural range 0 .. LCD_Width - 1) of Short
     with Component_Size => 16, Volatile;

   type Frame_Buffer (Pixel_Fmt : Pixel_Format) is record
      case Pixel_Fmt is
         when Pixel_Fmt_ARGB8888 =>
            Arr32 : FB32;
         when Pixel_Fmt_RGB888 =>
            Arr24 : FB24;
         when others =>
            Arr16 : FB16;
      end case;
   end record with Unchecked_Union, Volatile;

   type Frame_Buffer_Int_Access is access all Frame_Buffer;

   function Pixel_Size (Fmt : Pixel_Format) return Positive;

   ----------
   -- Init --
   ----------

   package Init is
      BF1_Constant_Alpha : constant := 2#100#;
      BF2_Constant_Alpha : constant := 2#101#;
      BF1_Pixel_Alpha    : constant := 2#110#;
      BF2_Pixel_Alpha    : constant := 2#111#;

      procedure Reload_Config (Immediate : Boolean);

      procedure Set_Layer_CFBA
        (Layer : LCD_Layer;
         FBA   : Frame_Buffer_Access);
      --  Sets the frame buffer of the specified layer

      --  Gets the frame buffer of the specified layer

      procedure LTDC_Init;

      procedure Layer_Init
        (Layer    : LCD_Layer;
         Config   : Pixel_Format;
         BF1, BF2 : UInt3);

      procedure Set_Layer_State
        (Layer : LCD_Layer;
         State : Boolean);
      --  Enables/Disables the specified layer

   end Init;

   package body Init is separate;

   -------------------
   -- Reload_Config --
   -------------------

   procedure Reload_Config
     (Immediate : Boolean := True)
      renames Init.Reload_Config;

   ---------------------
   -- Set_Layer_State --
   ---------------------

   procedure Set_Layer_State
     (Layer : LCD_Layer;
      State : Layer_State)
   is
   begin
      Init.Set_Layer_State (Layer, State = Enabled);
   end Set_Layer_State;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Pixel_Fmt : Pixel_Format := Pixel_Fmt_ARGB1555)
   is
      FB_Size : constant Word :=
                  Word
                    (LCD_Width * LCD_Height * Pixel_Size (Pixel_Fmt));
   begin
      Initialize_LCD;

      delay until Clock + Milliseconds (200);

      Init.LTDC_Init;
      STM32.SDRAM.Initialize;

      Current_Pixel_Fmt := Pixel_Fmt;

      Frame_Buffer_Array (Layer1) := STM32.SDRAM.Reserve (FB_Size);
      Frame_Buffer_Array (Layer2) := STM32.SDRAM.Reserve (FB_Size);

      Init.Layer_Init
        (Layer1, Pixel_Fmt,
         Init.BF1_Constant_Alpha,
         Init.BF2_Constant_Alpha);
      Init.Layer_Init
        (Layer2, Pixel_Fmt,
         Init.BF1_Pixel_Alpha,
         Init.BF2_Pixel_Alpha);

      Init.Set_Layer_State (Layer1, True);
      Init.Set_Layer_State (Layer2, False);

      Reload_Config (True);

      --  enable Dither
      LTDC_Periph.GCR.DEN := 1;

      Reload_Config (True);
   end Initialize;

   ----------------
   -- Pixel_Size --
   ----------------

   function Pixel_Size (Fmt : Pixel_Format) return Positive is
     (case Fmt is
         when Pixel_Fmt_ARGB8888 => 4,
         when Pixel_Fmt_RGB888   => 3,
         when Pixel_Fmt_RGB565 | Pixel_Fmt_ARGB1555 | Pixel_Fmt_ARGB4444 => 2);

   -------------------
   -- Get_Pixel_Fmt --
   -------------------

   function Get_Pixel_Fmt return Pixel_Format is
      (Current_Pixel_Fmt);

   --------------------------
   -- Current_Frame_Buffer --
   --------------------------

   function Current_Frame_Buffer
     (Layer : LCD_Layer)
      return Frame_Buffer_Access
   is
   begin
      return Frame_Buffer_Array (Layer);
   end Current_Frame_Buffer;

--     ---------------------
--     -- Set_Layer_State --
--     ---------------------
--
--     procedure Set_Layer_State (Layer : LCD_Layer; State : Layer_State) is
--        L : constant Layer_Access := Get_Layer (Layer);
--     begin
--        L.LCR.LEN := (if State = Enabled then 1 else 0);
--     end Set_Layer_State;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (R, G, B : Byte) is
   begin
      LTDC_Periph.BCCR.BC :=
        UInt24 (R * 2 ** 16) or UInt24 (G * 2 ** 8) or UInt24 (B);
   end Set_Background;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel
     (Layer : LCD_Layer;
      X     : Width;
      Y     : Height;
      Value : Word)
   is
      function To_FB_Access is new Ada.Unchecked_Conversion
        (System.Address, Frame_Buffer_Int_Access);
      Buff : constant Frame_Buffer_Int_Access :=
               To_FB_Access (Frame_Buffer_Array (Layer));
   begin
      if Y >= LCD_PIXEL_HEIGHT
        or else X >= LCD_PIXEL_WIDTH
      then
         return;
      end if;

      Buff.Arr32 (Y, X) := Value;
   end Set_Pixel;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel
     (Layer : LCD_Layer;
      X     : Width;
      Y     : Height;
      Value : UInt24)
   is
      function To_FB_Access is new Ada.Unchecked_Conversion
        (System.Address, Frame_Buffer_Int_Access);
      Buff : constant Frame_Buffer_Int_Access :=
               To_FB_Access (Frame_Buffer_Array (Layer));
   begin
      if Y >= LCD_PIXEL_HEIGHT
        or else X >= LCD_PIXEL_WIDTH
      then
         return;
      end if;

      --  Red:
      Buff.Arr24 (Y, (X * 3) + 2) := Byte (Value and 16#FF#);
      --  Green:
      Buff.Arr24 (Y, (X * 3) + 1) := Byte ((Value and 16#FF00#) / (2 ** 8));
      --  Blue:
      Buff.Arr24 (Y, (X * 3) + 0) := Byte ((Value and 16#FF0000#) / (2 ** 16));
   end Set_Pixel;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel
     (Layer : LCD_Layer;
      X     : Width;
      Y     : Height;
      Value : Half_Word)
   is
      function To_FB_Access is new Ada.Unchecked_Conversion
        (System.Address, Frame_Buffer_Int_Access);
      Buff : constant Frame_Buffer_Int_Access :=
               To_FB_Access (Frame_Buffer_Array (Layer));
   begin
      if Y >= LCD_PIXEL_HEIGHT
        or else X >= LCD_PIXEL_WIDTH
      then
         return;
      end if;

      Buff.Arr16 (Y, X) := Value;
   end Set_Pixel;

   -----------------
   -- Pixel_Value --
   -----------------

   function Pixel_Value
     (Layer : LCD_Layer;
      X     : Width;
      Y     : Height)
      return Word
   is
      function To_FB_Access is new Ada.Unchecked_Conversion
        (System.Address, Frame_Buffer_Int_Access);
      Buff : constant Frame_Buffer_Int_Access :=
               To_FB_Access (Frame_Buffer_Array (Layer));
   begin
      return Buff.Arr32 (Y, X);
   end Pixel_Value;

   -----------------
   -- Pixel_Value --
   -----------------

   function Pixel_Value
     (Layer : LCD_Layer;
      X     : Width;
      Y     : Height)
      return UInt24
   is
      function To_FB_Access is new Ada.Unchecked_Conversion
        (System.Address, Frame_Buffer_Int_Access);
      Buff : constant Frame_Buffer_Int_Access :=
               To_FB_Access (Frame_Buffer_Array (Layer));
      Ret : UInt24;
   begin
      Ret := UInt24 (Buff.Arr24 (Y, X * 3 + 2)) or
        UInt24 (Buff.Arr24 (Y, X * 3 + 1)) * 2 ** 8 or
        UInt24 (Buff.Arr24 (Y, X * 3 + 0)) * 2 ** 16;
      return Ret;
   end Pixel_Value;

   -----------------
   -- Pixel_Value --
   -----------------

   function Pixel_Value
     (Layer : LCD_Layer;
      X     : Width;
      Y     : Height)
      return Half_Word
   is
      function To_FB_Access is new Ada.Unchecked_Conversion
        (System.Address, Frame_Buffer_Int_Access);
      Buff : constant Frame_Buffer_Int_Access :=
               To_FB_Access (Frame_Buffer_Array (Layer));
   begin
      return Buff.Arr16 (Y, X);
   end Pixel_Value;

end STM32.LTDC;

