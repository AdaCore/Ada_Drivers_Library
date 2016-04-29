------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2016, AdaCore                         --
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
--   @brief   This file includes the LTDC driver to control LCD display.    --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Interrupts.Names;
with Ada.Unchecked_Conversion;

with System;         use System;

with STM32_SVD.LTDC; use STM32_SVD.LTDC;
with STM32_SVD.RCC;  use STM32_SVD.RCC;

package body STM32.LTDC is

   pragma Warnings (Off, "* is not referenced");
   type LCD_Polarity is
     (Polarity_Active_Low,
      Polarity_Active_High) with Size => 1;
   type LCD_PC_Polarity is
     (Input_Pixel_Clock,
      Inverted_Input_Pixel_Clock) with Size => 1;
   pragma Warnings (On, "* is not referenced");

   function To_Bool is new Ada.Unchecked_Conversion (LCD_Polarity, Boolean);
   function To_Bool is new Ada.Unchecked_Conversion (LCD_PC_Polarity, Boolean);

   --  Extracted from STM32F429x.LTDC
   type Layer_Type is record
      LCR        : L1CR_Register;
      --  Layerx Control Register

      LWHPCR     : L1WHPCR_Register;
      --  Layerx Window Horizontal Position Configuration Register

      LWVPCR     : L1WVPCR_Register;
      --  Layerx Window Vertical Position Configuration Register

      LCKCR      : L1CKCR_Register;
      --  Layerx Color Keying Configuration Register

      LPFCR      : L1PFCR_Register;
      --  Layerx Pixel Format Configuration Register

      LCACR      : L1CACR_Register;
      --  Layerx Constant Alpha Configuration Register

      LDCCR      : L1DCCR_Register;
      --  Layerx Default Color Configuration Register

      LBFCR      : L1BFCR_Register;
      --  Layerx Blending Factors Configuration Register

      Reserved_0 : Word;
      Reserved_1 : Word;

      LCFBAR     : Word;
      --  Layerx Color Frame Buffer Address Register

      LCFBLR     : L1CFBLR_Register;
      --  Layerx Color Frame Buffer Length Register

      LCFBLNR    : L1CFBLNR_Register;
      --  Layerx ColorFrame Buffer Line Number Register

      Reserved_2 : Word;
      Reserved_3 : Word;
      Reserved_4 : Word;

      LCLUTWR    : L1CLUTWR_Register;
      --  Layerx CLUT Write Register
   end record with Volatile;

   for Layer_Type use record
      LCR        at  0 range 0 .. 31;
      LWHPCR     at  4 range 0 .. 31;
      LWVPCR     at  8 range 0 .. 31;
      LCKCR      at 12 range 0 .. 31;
      LPFCR      at 16 range 0 .. 31;
      LCACR      at 20 range 0 .. 31;
      LDCCR      at 24 range 0 .. 31;
      LBFCR      at 28 range 0 .. 31;
      Reserved_0 at 32 range 0 .. 31;
      Reserved_1 at 36 range 0 .. 31;
      LCFBAR     at 40 range 0 .. 31;
      LCFBLR     at 44 range 0 .. 31;
      LCFBLNR    at 48 range 0 .. 31;
      Reserved_2 at 52 range 0 .. 31;
      Reserved_3 at 56 range 0 .. 31;
      Reserved_4 at 60 range 0 .. 31;
      LCLUTWR    at 64 range 0 .. 31;
   end record;
   type Layer_Access is access all Layer_Type;

   BF1_Constant_Alpha : constant := 2#100#;
   BF2_Constant_Alpha : constant := 2#101#;
   BF1_Pixel_Alpha    : constant := 2#110#;
   BF2_Pixel_Alpha    : constant := 2#111#;

   G_Layer1_Reg : aliased Layer_Type
     with Import, Address => LTDC_Periph.L1CR'Address;
   G_Layer2_Reg : aliased Layer_Type
     with Import, Address => LTDC_Periph.L2CR'Address;

   function Get_Layer (Layer : LCD_Layer) return Layer_Access;
   --  Retrieve the layer's registers

   protected Sync is
      --  Apply pending buffers on Vertical Sync. Caller must call Wait
      --  afterwards.
      procedure Apply_On_VSync;

      --  Wait for an interrupt.
      entry Wait;

      procedure Interrupt;
      pragma Attach_Handler
        (Interrupt, Ada.Interrupts.Names.LCD_TFT_Interrupt);

   private
      Not_Pending : Boolean := True;
   end Sync;

   ----------
   -- Sync --
   ----------

   protected body Sync is

      ----------
      -- Wait --
      ----------

      entry Wait when Not_Pending is
      begin
         null;
      end Wait;

      --------------------
      -- Apply_On_VSync --
      --------------------

      procedure Apply_On_VSync is
      begin
         Not_Pending := False;
         --  Enable the Register Refresh interrupt
         LTDC_Periph.IER.RRIE := True;
         --  And tell the LTDC to apply the layer registers on refresh
         LTDC_Periph.SRCR.VBR := True;
      end Apply_On_VSync;

      ---------------
      -- Interrupt --
      ---------------

      procedure Interrupt
      is
      begin
         if LTDC_Periph.ISR.RRIF then
            LTDC_Periph.IER.RRIE := False;
            LTDC_Periph.ICR.CRRIF  := True;

            Not_Pending := True;
         end if;
      end Interrupt;
   end Sync;

   ---------------
   -- Get_Layer --
   ---------------

   function Get_Layer (Layer : LCD_Layer) return Layer_Access is
   begin
      if Layer = Layer1 then
         return G_Layer1_Reg'Access;
      else
         return G_Layer2_Reg'Access;
      end if;
   end Get_Layer;

   -------------------
   -- Reload_Config --
   -------------------

   procedure Reload_Config
     (Immediate : Boolean := False)
   is
   begin
      if Immediate then
         LTDC_Periph.SRCR.IMR := True;

         loop
            exit when not LTDC_Periph.SRCR.IMR;
         end loop;
      else
         Sync.Apply_On_VSync;
         Sync.Wait;
      end if;
   end Reload_Config;

   ---------------------
   -- Set_Layer_State --
   ---------------------

   procedure Set_Layer_State
     (Layer   : LCD_Layer;
      Enabled : Boolean)
   is
      L   : constant Layer_Access := Get_Layer (Layer);
   begin
      if L.LCR.LEN /= Enabled then
         L.LCR.LEN := Enabled;
         Reload_Config (Immediate => True);
      end if;
   end Set_Layer_State;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Width         : Positive;
      Height        : Positive;
      H_Sync        : Natural;
      H_Back_Porch  : Natural;
      H_Front_Porch : Natural;
      V_Sync        : Natural;
      V_Back_Porch  : Natural;
      V_Front_Porch : Natural;
      PLLSAI_N      : UInt9;
      PLLSAI_R      : UInt3;
      DivR          : Natural)
   is
      DivR_Val : PLLSAI_DivR;

   begin
      if Initialized then
         return;
      end if;

      if DivR = 2 then
         DivR_Val := PLLSAI_DIV2;
      elsif DivR = 4 then
         DivR_Val := PLLSAI_DIV4;
      elsif DivR = 8 then
         DivR_Val := PLLSAI_DIV8;
      elsif DivR = 16 then
         DivR_Val := PLLSAI_DIV16;
      else
         raise Constraint_Error with "Invalid DivR value: 2, 4, 8, 16 allowed";
      end if;

      Disable_PLLSAI;

      RCC_Periph.APB2ENR.LTDCEN := True;

      LTDC_Periph.GCR.VSPOL := To_Bool (Polarity_Active_Low);
      LTDC_Periph.GCR.HSPOL := To_Bool (Polarity_Active_Low);
      LTDC_Periph.GCR.DEPOL := To_Bool (Polarity_Active_Low);
      LTDC_Periph.GCR.PCPOL := To_Bool (Inverted_Input_Pixel_Clock);

      Set_PLLSAI_Factors
        (LCD  => PLLSAI_R,
         VCO  => PLLSAI_N,
         DivR => DivR_Val);
      Enable_PLLSAI;

      --  Synchronization size
      LTDC_Periph.SSCR :=
        (HSW => SSCR_HSW_Field (H_Sync - 1),
         VSH => SSCR_VSH_Field (V_Sync - 1),
         others => <>);

      --  Accumulated Back Porch
      LTDC_Periph.BPCR :=
        (AHBP => BPCR_AHBP_Field (H_Sync + H_Back_Porch - 1),
         AVBP => BPCR_AVBP_Field (V_Sync + V_Back_Porch - 1),
         others => <>);

      --  Accumulated Active Width/Height
      LTDC_Periph.AWCR :=
        (AAW => AWCR_AAW_Field (H_Sync + H_Back_Porch + Width - 1),
         AAH => AWCR_AAH_Field (V_Sync + V_Back_Porch + Height - 1),
         others => <>);

      --  VTotal Width/Height
      LTDC_Periph.TWCR :=
        (TOTALW =>
           TWCR_TOTALW_Field
             (H_Sync + H_Back_Porch + Width + H_Front_Porch - 1),
         TOTALH =>
           TWCR_TOTALH_Field
             (V_Sync + V_Back_Porch + Height + V_Front_Porch - 1),
         others => <>);

      --  Background color to black
      LTDC_Periph.BCCR.BC := 0;

      LTDC_Periph.GCR.LTDCEN := True;
   end Initialize;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      LTDC_Periph.GCR.LTDCEN := True;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      LTDC_Periph.GCR.LTDCEN := False;
   end Stop;

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean is
   begin
      return LTDC_Periph.GCR.LTDCEN;
   end Initialized;

   ----------------
   -- Layer_Init --
   ----------------

   procedure Layer_Init
     (Layer          : LCD_Layer;
      Config         : Pixel_Format;
      Buffer         : System.Address;
      X, Y           : Natural;
      W, H           : Positive;
      Constant_Alpha : Byte := 255;
      BF             : Blending_Factor := BF_Pixel_Alpha_X_Constant_Alpha)
   is
      L        : constant Layer_Access := Get_Layer (Layer);
      CFBL     : L1CFBLR_Register := L.LCFBLR;
   begin
      --  Horizontal start and stop = sync + Back Porch
      L.LWHPCR :=
        (WHSTPOS => L1WHPCR_WHSTPOS_Field (LTDC_Periph.BPCR.AHBP) + 1 +
             L1WHPCR_WHSTPOS_Field (X),
         WHSPPOS => L1WHPCR_WHSPPOS_Field (LTDC_Periph.BPCR.AHBP) +
             L1WHPCR_WHSPPOS_Field (X + W),
         others  => <>);

      --  Vertical start and stop
      L.LWVPCR :=
        (WVSTPOS => LTDC_Periph.BPCR.AVBP + 1 + UInt11 (Y),
         WVSPPOS => LTDC_Periph.BPCR.AVBP + UInt11 (Y + H),
         others  => <>);

      L.LPFCR.PF := Pixel_Format'Enum_Rep (Config);

      L.LCACR.CONSTA := Constant_Alpha;

      L.LDCCR := (others => 0);

      case BF is
         when BF_Constant_Alpha =>
            L.LBFCR.BF1 := BF1_Constant_Alpha;
            L.LBFCR.BF2 := BF2_Constant_Alpha;
         when BF_Pixel_Alpha_X_Constant_Alpha =>
            L.LBFCR.BF1 := BF1_Pixel_Alpha;
            L.LBFCR.BF2 := BF2_Pixel_Alpha;
      end case;

      CFBL.CFBLL := UInt13 (W * Bytes_Per_Pixel (Config)) + 3;
      CFBL.CFBP := UInt13 (W * Bytes_Per_Pixel (Config));
      L.LCFBLR := CFBL;

      L.LCFBLNR.CFBLNBR := UInt11 (H);

      Set_Frame_Buffer (Layer, Buffer);

      L.LCR.LEN := True;
      Reload_Config (True);
   end Layer_Init;

   ----------------------
   -- Set_Frame_Buffer --
   ----------------------

   procedure Set_Frame_Buffer
     (Layer : LCD_Layer; Addr : Frame_Buffer_Access)
   is
      function To_Word is new Ada.Unchecked_Conversion
        (Frame_Buffer_Access, Word);
   begin
      if Layer = Layer1 then
         LTDC_Periph.L1CFBAR := To_Word (Addr);
      else
         LTDC_Periph.L2CFBAR := To_Word (Addr);
      end if;
   end Set_Frame_Buffer;

   ----------------------
   -- Get_Frame_Buffer --
   ----------------------

   function Get_Frame_Buffer
     (Layer : LCD_Layer)
      return Frame_Buffer_Access
   is
      L : constant Layer_Access := Get_Layer (Layer);
      function To_FBA is new Ada.Unchecked_Conversion
        (Word, Frame_Buffer_Access);
   begin
      return To_FBA (L.LCFBAR);
   end Get_Frame_Buffer;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (R, G, B : Byte) is
      RShift : constant Word := Shift_Left (Word (R), 16);
      GShift : constant Word := Shift_Left (Word (G), 8);
   begin
      LTDC_Periph.BCCR.BC :=
        UInt24 (RShift) or UInt24 (GShift) or UInt24 (B);
   end Set_Background;

end STM32.LTDC;
