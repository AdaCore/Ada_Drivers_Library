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

package body STM32.LTDC is

   Frame_Buffer_Array : array (LCD_Layer) of Frame_Buffer_Access;
   Current_Pixel_Fmt  : Pixel_Format := Pixel_Fmt_ARGB1555;
   Swap_X_Y           : Boolean := False;

   type FB32 is array (Natural range 0 .. LCD_Height - 1,
                       Natural range 0 .. LCD_Width - 1) of Word
     with Component_Size => 32, Volatile;
   type Swap_FB32 is array (Natural range 0 .. LCD_Width - 1,
                            Natural range 0 .. LCD_Height - 1) of Word
     with Component_Size => 32, Volatile;
   type FB24 is array (Natural range 0 .. LCD_Height - 1,
                       Natural range 0 .. (LCD_Width * 3) - 1) of Byte
     with Component_Size => 8, Volatile;
   type Swap_FB24 is array (Natural range 0 .. LCD_Width - 1,
                            Natural range 0 .. LCD_Height * 3 - 1) of Byte
     with Component_Size => 8, Volatile;
   type FB16 is array (Natural range 0 .. LCD_Height - 1,
                       Natural range 0 .. LCD_Width - 1) of Short
     with Component_Size => 16, Volatile;
   type Swap_FB16 is array (Natural range 0 .. LCD_Width - 1,
                            Natural range 0 .. LCD_Height - 1) of Short
     with Component_Size => 16, Volatile;

   type Frame_Buffer (Pixel_Fmt : Pixel_Format;
                      Swap      : Boolean) is record
      case Swap is
         when False =>
            case Pixel_Fmt is
            when Pixel_Fmt_ARGB8888 =>
               Arr32 : FB32;
            when Pixel_Fmt_RGB888 =>
               Arr24 : FB24;
            when others =>
               Arr16 : FB16;
            end case;
         when True =>
            case Pixel_Fmt is
            when Pixel_Fmt_ARGB8888 =>
               SwArr32 : Swap_FB32;
            when Pixel_Fmt_RGB888 =>
               SwArr24 : Swap_FB24;
            when others =>
               SwArr16 : Swap_FB16;
            end case;
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

      function Get_Layer_CFBA
        (Layer : LCD_Layer) return Frame_Buffer_Access;
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
      if Initialized then
         return;
      end if;

      Pre_LTDC_Initialize;

      delay until Clock + Milliseconds (200);

      Init.LTDC_Init;
      STM32.SDRAM.Initialize;

      Current_Pixel_Fmt := Pixel_Fmt;

      Frame_Buffer_Array (Layer1) := STM32.SDRAM.Reserve (FB_Size);

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

      Post_LTDC_Initialize;
   end Initialize;

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean is
   begin
      return Frame_Buffer_Array (Layer1) /= Null_Address;
   end Initialized;

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
     renames Init.Get_Layer_CFBA;

   ----------------------
   -- Set_Frame_Buffer --
   ----------------------

   procedure Set_Frame_Buffer
     (Layer : LCD_Layer; Addr : Frame_Buffer_Access)
      renames Init.Set_Layer_CFBA;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation (Orientation : Orientation_Mode)
   is
   begin
      case Orientation is
         when Portrait =>
            Swap_X_Y := LCD_Width > LCD_Height;
         when Landscape =>
            Swap_X_Y := LCD_Height > LCD_Width;
      end case;
   end Set_Orientation;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation return Orientation_Mode
   is
   begin
      return (if Pixel_Width > Pixel_Height then Landscape else Portrait);
   end Get_Orientation;

   ------------
   -- SwapXY --
   ------------

   function SwapXY return Boolean
   is (Swap_X_Y);

   -----------------
   -- Pixel_Width --
   -----------------

   function Pixel_Width return Natural
   is (if Swap_X_Y then LCD_Height else LCD_Width);

   ------------------
   -- Pixel_Height --
   ------------------

   function Pixel_Height return Natural
   is (if Swap_X_Y then LCD_Width else LCD_Height);

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
      X     : Natural;
      Y     : Natural;
      Value : Word)
   is
      function To_FB_Access is new Ada.Unchecked_Conversion
        (System.Address, Frame_Buffer_Int_Access);
      Buff : constant Frame_Buffer_Int_Access :=
               To_FB_Access (Frame_Buffer_Array (Layer));
   begin
      if Y >= Pixel_Height
        or else X >= Pixel_Width
      then
         return;
      end if;

      if not Swap_X_Y then
         Buff.Arr32 (Y, X) := Value;
      else
         Buff.SwArr32 (X, Y) := Value;
      end if;
   end Set_Pixel;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel
     (Layer : LCD_Layer;
      X     : Natural;
      Y     : Natural;
      Value : UInt24)
   is
      function To_FB_Access is new Ada.Unchecked_Conversion
        (System.Address, Frame_Buffer_Int_Access);
      Buff : constant Frame_Buffer_Int_Access :=
               To_FB_Access (Frame_Buffer_Array (Layer));
   begin
      if Y >= Pixel_Height
        or else X >= Pixel_Width
      then
         return;
      end if;

      if not Swap_X_Y then
         --  Red:
         Buff.Arr24 (Y, (X * 3) + 2) := Byte (Value and 16#FF#);
         --  Green:
         Buff.Arr24 (Y, (X * 3) + 1) := Byte ((Value and 16#FF00#) / (2 ** 8));
         --  Blue:
         Buff.Arr24 (Y, (X * 3) + 0) := Byte ((Value and 16#FF0000#) / (2 ** 16));
      else
         --  Red:
         Buff.SwArr24 (X, (Y * 3) + 2) := Byte (Value and 16#FF#);
         --  Green:
         Buff.SwArr24 (X, (Y * 3) + 1) := Byte ((Value and 16#FF00#) / (2 ** 8));
         --  Blue:
         Buff.SwArr24 (X, (Y * 3) + 0) := Byte ((Value and 16#FF0000#) / (2 ** 16));
      end if;
   end Set_Pixel;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel
     (Layer : LCD_Layer;
      X     : Natural;
      Y     : Natural;
      Value : Half_Word)
   is
      function To_FB_Access is new Ada.Unchecked_Conversion
        (System.Address, Frame_Buffer_Int_Access);
      Buff : constant Frame_Buffer_Int_Access :=
               To_FB_Access (Frame_Buffer_Array (Layer));
   begin
      if Y >= Pixel_Height
        or else X >= Pixel_Width
      then
         return;
      end if;

      if not Swap_X_Y then
         Buff.Arr16 (Y, X) := Value;
      else
         Buff.SwArr16 (X, Y) := Value;
      end if;
   end Set_Pixel;

   -----------------
   -- Pixel_Value --
   -----------------

   function Pixel_Value
     (Layer : LCD_Layer;
      X     : Natural;
      Y     : Natural)
      return Word
   is
      function To_FB_Access is new Ada.Unchecked_Conversion
        (System.Address, Frame_Buffer_Int_Access);
      Buff : constant Frame_Buffer_Int_Access :=
               To_FB_Access (Frame_Buffer_Array (Layer));
   begin
      if not Swap_X_Y then
         return Buff.Arr32 (Y, X);
      else
         return Buff.SwArr32 (X, Y);
      end if;
   end Pixel_Value;

   -----------------
   -- Pixel_Value --
   -----------------

   function Pixel_Value
     (Layer : LCD_Layer;
      X     : Natural;
      Y     : Natural)
      return UInt24
   is
      function To_FB_Access is new Ada.Unchecked_Conversion
        (System.Address, Frame_Buffer_Int_Access);
      Buff : constant Frame_Buffer_Int_Access :=
               To_FB_Access (Frame_Buffer_Array (Layer));
      Ret : UInt24;
   begin
      if not Swap_X_Y then
         Ret := UInt24 (Buff.Arr24 (Y, X * 3 + 2)) or
           UInt24 (Buff.Arr24 (Y, X * 3 + 1)) * 2 ** 8 or
           UInt24 (Buff.Arr24 (Y, X * 3 + 0)) * 2 ** 16;
      else
         Ret := UInt24 (Buff.SwArr24 (X, Y * 3 + 2)) or
           UInt24 (Buff.SwArr24 (X, Y * 3 + 1)) * 2 ** 8 or
           UInt24 (Buff.SwArr24 (X, Y * 3 + 0)) * 2 ** 16;
      end if;

      return Ret;
   end Pixel_Value;

   -----------------
   -- Pixel_Value --
   -----------------

   function Pixel_Value
     (Layer : LCD_Layer;
      X     : Natural;
      Y     : Natural)
      return Half_Word
   is
      function To_FB_Access is new Ada.Unchecked_Conversion
        (System.Address, Frame_Buffer_Int_Access);
      Buff : constant Frame_Buffer_Int_Access :=
               To_FB_Access (Frame_Buffer_Array (Layer));
   begin
      if not Swap_X_Y then
         return Buff.Arr16 (Y, X);
      else
         return Buff.SwArr16 (X, Y);
      end if;
   end Pixel_Value;

end STM32.LTDC;

