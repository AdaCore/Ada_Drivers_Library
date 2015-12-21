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

with Ada.Unchecked_Conversion;
pragma Warnings (Off, "* is an internal GNAT unit");
with System.BB.Parameters;
pragma Warnings (On, "* is an internal GNAT unit");
with STM32.Device;             use STM32.Device;
with STM32_SVD.PWR;            use STM32_SVD.PWR;
with STM32_SVD.RCC;            use STM32_SVD.RCC;

package body STM32.RCC is

   HSE_VALUE : constant := System.BB.Parameters.HSE_Clock;
   --  External oscillator in Hz

   HSI_VALUE : constant := 16_000_000;
   --  Internal oscillator in Hz

   HPRE_Presc_Table : constant array (UInt4) of Word :=
     (1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 8, 16, 64, 128, 256, 512);

   PPRE_Presc_Table : constant array (UInt3) of Word :=
     (1, 1, 1, 1, 2, 4, 8, 16);

   function To_AHB1RSTR_T is new Ada.Unchecked_Conversion
     (Word, AHB1RSTR_Register);
   function To_AHB2RSTR_T is new Ada.Unchecked_Conversion
     (Word, AHB2RSTR_Register);
   function To_APB1RSTR_T is new Ada.Unchecked_Conversion
     (Word, APB1RSTR_Register);
   function To_APB2RSTR_T is new Ada.Unchecked_Conversion
     (Word, APB2RSTR_Register);

   ------------------------------
   -- System_Clock_Frequencies --
   ------------------------------

   function System_Clock_Frequencies return RCC_System_Clocks
   is
      Source       : constant UInt2 := RCC_Periph.CFGR.SWS.Val;
      Result       : RCC_System_Clocks;
   begin
      case Source is
         when 0 =>
            --  HSI as source
            Result.SYSCLK := HSI_VALUE;
         when 1 =>
            --  HSE as source
            Result.SYSCLK := HSE_VALUE;
         when 2 =>
            --  PLL as source
            declare
               Pllsource : constant Bit := RCC_Periph.PLLCFGR.PLLSRC;
               Pllm      : constant Word :=
                             Word (RCC_Periph.PLLCFGR.PLLM.Val);
               Plln      : constant Word :=
                             Word (RCC_Periph.PLLCFGR.PLLN.Val);
               Pllp      : constant Word :=
                             (Word (RCC_Periph.PLLCFGR.PLLP.Val) + 1) * 2;
               Pllvco    : Word;
            begin
               if Pllsource = 0 then
                  Pllvco := (HSI_VALUE / Pllm) * Plln;
               else
                  Pllvco := (HSE_VALUE / Pllm) * Plln;
               end if;
               Result.SYSCLK := Pllvco / Pllp;
            end;
         when others =>
            Result.SYSCLK := HSI_VALUE;
      end case;

      declare
         HPRE  : constant UInt4 := RCC_Periph.CFGR.HPRE;
         PPRE1 : constant UInt3 := RCC_Periph.CFGR.PPRE.Arr (0);
         PPRE2 : constant UInt3 := RCC_Periph.CFGR.PPRE.Arr (1);
      begin
         Result.HCLK  := Result.SYSCLK / HPRE_Presc_Table (HPRE);
         Result.PCLK1 := Result.HCLK / PPRE_Presc_Table (PPRE1);
         Result.PCLK2 := Result.HCLK / PPRE_Presc_Table (PPRE2);

         --  Timer clocks
         --  See Dedicated clock cfg register documentation.
         if Get_ABP_Timer_Prescaler_Mode = Mode_0 then
            if PPRE_Presc_Table (PPRE1) = 1 then
               Result.TIMCLK1 := Result.PCLK1;
            else
               Result.TIMCLK1 := Result.PCLK1 * 2;
            end if;
            if PPRE_Presc_Table (PPRE2) = 1 then
               Result.TIMCLK2 := Result.PCLK2;
            else
               Result.TIMCLK2 := Result.PCLK2 * 2;
            end if;
         else
            if PPRE_Presc_Table (PPRE1) in 1 .. 4 then
               Result.TIMCLK1 := Result.HCLK;
            else
               Result.TIMCLK1 := Result.PCLK1 * 4;
            end if;
            if PPRE_Presc_Table (PPRE2) in 1 .. 4 then
               Result.TIMCLK2 := Result.HCLK;
            else
               Result.TIMCLK2 := Result.PCLK1 * 4;
            end if;
         end if;
      end;

      return Result;
   end System_Clock_Frequencies;

   ------------------
   -- PLLSAI_Ready --
   ------------------

   function PLLSAI_Ready return Boolean is
   begin
      --  SHould be PLLSAIRDY, but not binded by the SVD file
      --  PLLSAIRDY: bit 29
      return (RCC_Periph.CR.Reserved_28_31 and 2) /= 0;
   end PLLSAI_Ready;

   -------------------
   -- Enable_PLLSAI --
   -------------------;

   procedure Enable_PLLSAI is
   begin
      --  Should be PLLSAION, but not binded by the SVD file
      --  PLLSAION: bit 28
      RCC_Periph.CR.Reserved_28_31 :=
        RCC_Periph.CR.Reserved_28_31 or 1;

      --  Wait for PLLSAI activation
      loop
         exit when PLLSAI_Ready;
      end loop;
   end Enable_PLLSAI;

   -------------------
   -- Enable_PLLSAI --
   -------------------;

   procedure Disable_PLLSAI is
   begin
      --  Should be PLLSAION, but not binded by the SVD file
      --  PLLSAION: bit 28
      RCC_Periph.CR.Reserved_28_31 :=
        RCC_Periph.CR.Reserved_28_31 and not 1;
   end Disable_PLLSAI;

   ------------------------
   -- Set_PLLSAI_Factors --
   ------------------------

   procedure Set_PLLSAI_Factors (LCD  : UInt3;
                                 VCO  : UInt9;
                                 DivR : PLLSAI_DivR)
   is
      PLLSAICFGR : PLLSAICFGR_Register;
   begin
      PLLSAICFGR.PLLSAIR := LCD;
      PLLSAICFGR.PLLSAIN := VCO;
      RCC_Periph.PLLSAICFGR := PLLSAICFGR;

      --  The exact bit name is device-specific
      Set_PLLSAIDIVR (UInt2 (DivR));
   end Set_PLLSAI_Factors;

   ---------------------------------------------------------------------------
   -------  Enable/Disable/Reset Routines  -----------------------------------
   ---------------------------------------------------------------------------

   procedure CRC_Clock_Enable is
   begin
      RCC_Periph.AHB1ENR.CRCEN := 1;
   end CRC_Clock_Enable;

   procedure BKPSRAM_Clock_Enable is
   begin
      RCC_Periph.AHB1ENR.BKPSRAMEN := 1;
   end BKPSRAM_Clock_Enable;

   procedure CCMDATARAMEN_Clock_Enable is
   begin
      RCC_Periph.AHB1ENR.CCMDATARAMEN := 1;
   end CCMDATARAMEN_Clock_Enable;

   procedure DMA2D_Clock_Enable is
   begin
      RCC_Periph.AHB1ENR.DMA2DEN := 1;
   end DMA2D_Clock_Enable;


   procedure WWDG_Clock_Enable is
   begin
      RCC_Periph.APB1ENR.WWDGEN := 1;
   end WWDG_Clock_Enable;

--     procedure SDIO_Clock_Enable is
--     begin
--        RCC_Periph.APB2ENR.SDIOEN := 1;
--     end SDIO_Clock_Enable;

   procedure SYSCFG_Clock_Enable is
   begin
      RCC_Periph.APB2ENR.SYSCFGEN := 1;
   end SYSCFG_Clock_Enable;

--     procedure SDIO_Clock_Disable is
--     begin
--        RCC_Periph.APB2ENR.SDIOEN := 0;
--     end SDIO_Clock_Disable;


   procedure AHB1_Force_Reset
   is
   begin
      RCC_Periph.AHB1RSTR := To_AHB1RSTR_T(16#FFFF_FFFF#);
   end AHB1_Force_Reset;

   procedure AHB1_Release_Reset is
   begin
      RCC_Periph.AHB1RSTR := To_AHB1RSTR_T (0);
   end AHB1_Release_Reset;

   procedure AHB2_Force_Reset is
   begin
      RCC_Periph.AHB2RSTR := To_AHB2RSTR_T (16#FFFF_FFFF#);
   end AHB2_Force_Reset;

   procedure AHB2_Release_Reset is
   begin
      RCC_Periph.AHB2RSTR := To_AHB2RSTR_T (0);
   end AHB2_Release_Reset;

   procedure APB1_Force_Reset is
   begin
      RCC_Periph.APB1RSTR := To_APB1RSTR_T (16#FFFF_FFFF#);
   end APB1_Force_Reset;

   procedure APB1_Release_Reset is
   begin
      RCC_Periph.APB1RSTR := To_APB1RSTR_T (0);
   end APB1_Release_Reset;

   procedure APB2_Force_Reset is
   begin
      RCC_Periph.APB2RSTR := To_APB2RSTR_T (16#FFFF_FFFF#);
   end APB2_Force_Reset;

   procedure APB2_Release_Reset is
   begin
      RCC_Periph.APB2RSTR := To_APB2RSTR_T (0);
   end APB2_Release_Reset;

   procedure CRC_Force_Reset is
   begin
      RCC_Periph.AHB1RSTR.CRCRST := 1;
   end CRC_Force_Reset;

   procedure CRC_Release_Reset is
   begin
      RCC_Periph.AHB1RSTR.CRCRST := 0;
   end CRC_Release_Reset;

   procedure DMA2D_Force_Reset is
   begin
      RCC_Periph.AHB1RSTR.DMA2DRST := 1;
   end DMA2D_Force_Reset;

   procedure DMA2D_Release_Reset is
   begin
      RCC_Periph.AHB1RSTR.DMA2DRST := 0;
   end DMA2D_Release_Reset;

   procedure OTGFS_Force_Reset is
   begin
      RCC_Periph.AHB2RSTR.OTGFSRST := 1;
   end OTGFS_Force_Reset;

   procedure OTGFS_Release_Reset is
   begin
      RCC_Periph.AHB2RSTR.OTGFSRST := 0;
   end OTGFS_Release_Reset;

   procedure WWDG_Force_Reset is
   begin
      RCC_Periph.APB1RSTR.WWDGRST := 1;
   end WWDG_Force_Reset;

   procedure WWDG_Release_Reset is
   begin
      RCC_Periph.APB1RSTR.WWDGRST := 0;
   end WWDG_Release_Reset;

--     procedure SDIO_Force_Reset is
--     begin
--        RCC_Periph.APB2RSTR.SDIORST := 1;
--     end SDIO_Force_Reset;

   procedure SYSCFG_Force_Reset is
   begin
      RCC_Periph.APB2RSTR.SYSCFGRST := 1;
   end SYSCFG_Force_Reset;

   procedure SYSCFG_Release_Reset is
   begin
      RCC_Periph.APB2RSTR.SYSCFGRST := 0;
   end SYSCFG_Release_Reset;

--     procedure SDIO_Release_Reset is
--     begin
--        RCC_Periph.APB2RSTR.SDIORST := 0;
--     end SDIO_Release_Reset;

end STM32.RCC;
