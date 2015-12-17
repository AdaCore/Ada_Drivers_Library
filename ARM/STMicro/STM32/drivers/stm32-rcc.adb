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
--   @file    stm32f4xx_hal_rcc.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   RCC HAL module driver.                                        --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Interfaces; use Interfaces;

package body STM32.RCC is

   HSE_VALUE : constant :=  8_000_000; --  External oscillator in Hz
   HSI_VALUE : constant := 16_000_000; --  Internal oscillator in Hz

   HPRE_Presc_Table : constant array (Bits_4) of Word :=
     (1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 8, 16, 64, 128, 256, 512);

   PPRE_Presc_Table : constant array (Bits_3) of Word :=
     (1, 1, 1, 1, 2, 4, 8, 16);

   -------------------------
   -- Get_Clock_Frequency --
   -------------------------

   function System_Clock_Frequencies return RCC_System_Clocks is
      RCC_CFGR_SWS : constant := 16#C#;
      Source       : constant Word := Register.CFGR and RCC_CFGR_SWS;
      Result       : RCC_System_Clocks;
   begin
      case Source is
         when 16#00# =>
            --  HSI as source
            Result.SYSCLK := HSI_VALUE;
         when 16#04# =>
            --  HSE as source
            Result.SYSCLK := HSE_VALUE;
         when 16#08# =>
            --  PLL as source
            declare
               Pllsource : constant Word :=
                 (Register.PLLCFGR and 16#00400000#) / (2**22);
               Pllm : constant Word := Register.PLLCFGR and 16#0000003F#;
               Plln : constant Word := (Register.PLLCFGR and 16#00007FC0#) / (2**6);
               Pllp : constant Word := (((Register.PLLCFGR and 16#00030000#) / (2**16)) + 1) * 2;
               Pllvco : Word;
            begin
               if Pllsource /= 0 then
                  Pllvco := (HSE_VALUE / Pllm) * Plln;
               else
                  Pllvco := (HSI_VALUE / Pllm) * Plln;
               end if;
               Result.SYSCLK := Pllvco / Pllp;
            end;
         when others =>
            Result.SYSCLK := HSI_VALUE;
      end case;

      declare
         HPRE  : constant Bits_4 := Bits_4 ((Register.CFGR and 16#00F0#) / (2**4));
         PPRE1 : constant Bits_3 := Bits_3 ((Register.CFGR and 16#1C00#) / (2**10));
         PPRE2 : constant Bits_3 := Bits_3 ((Register.CFGR and 16#E000#) / (2**13));
         TIMPR : constant Word   := (Register.DCKCFGR / (2**24)) and 1;
      begin
         Result.HCLK  := Result.SYSCLK / HPRE_Presc_Table (HPRE);
         Result.PCLK1 := Result.HCLK / PPRE_Presc_Table (PPRE1);
         Result.PCLK2 := Result.HCLK / PPRE_Presc_Table (PPRE2);

         --  Timer clocks
         --  See Dedicated clock cfg register documentation.
         if TIMPR = 0 then
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

   ------------------------
   -- Set_PLLSAI_Factors --
   ------------------------

   procedure Set_PLLSAI_Factors (LCD  : Bits_3;
                                 SAI1 : Bits_4;
                                 VCO  : Bits_9;
                                 DivR : Bits_2) is
   begin
      Register.PLLSAICFGR := (Word (VCO) * (2**6)) or
                        (Word (SAI1) * (2**24)) or
        (Word (LCD) * (2**28));

      Register.DCKCFGR := Register.DCKCFGR and (not (16#30000#));
      Register.DCKCFGR := Register.DCKCFGR or (Word(DivR) * (2**16));
   end Set_PLLSAI_Factors;

   -------------------
   -- Enable_PLLSAI --
   -------------------

   procedure Enable_PLLSAI is
   begin
      Register.CR := Register.CR or (2**28);

      --  Wait for PLLSAI activation
      loop
         exit when (Register.CR and (2**29)) /= 0;
      end loop;
   end Enable_PLLSAI;

   ---------------------------------------------------------------------------

   -------  Enable/Disable/Reset Routines  -----------------------------------

   ---------------------------------------------------------------------------

   procedure GPIOA_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_GPIOAEN;
   end GPIOA_Clock_Enable;

   procedure GPIOB_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_GPIOBEN;
   end GPIOB_Clock_Enable;

   procedure GPIOC_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_GPIOCEN;
   end GPIOC_Clock_Enable;

   procedure GPIOD_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_GPIODEN;
   end GPIOD_Clock_Enable;

   procedure GPIOE_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_GPIOEEN;
   end GPIOE_Clock_Enable;

   procedure GPIOF_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_GPIOFEN;
   end GPIOF_Clock_Enable;

   procedure GPIOG_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_GPIOGEN;
   end GPIOG_Clock_Enable;

   procedure GPIOH_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_GPIOHEN;
   end GPIOH_Clock_Enable;

   procedure GPIOI_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_GPIOIEN;
   end GPIOI_Clock_Enable;

   procedure GPIOJ_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_GPIOJEN;
   end GPIOJ_Clock_Enable;

   procedure GPIOK_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_GPIOKEN;
   end GPIOK_Clock_Enable;

   procedure CRC_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_CRCEN;
   end CRC_Clock_Enable;

   procedure BKPSRAM_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_BKPSRAMEN;
   end BKPSRAM_Clock_Enable;

   procedure CCMDATARAMEN_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_CCMDATARAMEN;
   end CCMDATARAMEN_Clock_Enable;

   procedure DMA1_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_DMA1EN;
   end DMA1_Clock_Enable;

   procedure DMA2_Clock_Enable is
   begin
      Register.AHB1ENR := Register.AHB1ENR or AHB1ENR_DMA2EN;
   end DMA2_Clock_Enable;

   procedure GPIOA_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_GPIOAEN;
   end GPIOA_Clock_Disable;

   procedure GPIOB_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_GPIOBEN;
   end GPIOB_Clock_Disable;

   procedure GPIOC_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_GPIOCEN;
   end GPIOC_Clock_Disable;

   procedure GPIOD_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_GPIODEN;
   end GPIOD_Clock_Disable;

   procedure GPIOE_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_GPIOEEN;
   end GPIOE_Clock_Disable;

   procedure GPIOF_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_GPIOFEN;
   end GPIOF_Clock_Disable;

   procedure GPIOG_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_GPIOGEN;
   end GPIOG_Clock_Disable;

   procedure GPIOH_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_GPIOHEN;
   end GPIOH_Clock_Disable;

   procedure GPIOI_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_GPIOIEN;
   end GPIOI_Clock_Disable;

   procedure GPIOJ_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_GPIOJEN;
   end GPIOJ_Clock_Disable;

   procedure GPIOK_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_GPIOKEN;
   end GPIOK_Clock_Disable;

   procedure CRC_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_CRCEN;
   end CRC_Clock_Disable;

   procedure BKPSRAM_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_BKPSRAMEN;
   end BKPSRAM_Clock_Disable;

   procedure CCMDATARAMEN_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_CCMDATARAMEN;
   end CCMDATARAMEN_Clock_Disable;

   procedure DMA1_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_DMA1EN;
   end DMA1_Clock_Disable;

   procedure DMA2_Clock_Disable is
   begin
      Register.AHB1ENR := Register.AHB1ENR and not AHB1ENR_DMA2EN;
   end DMA2_Clock_Disable;

   procedure RNG_Clock_Enable is
   begin
      Register.AHB2ENR := Register.AHB2ENR or AHB2ENR_RNGEN;
   end RNG_Clock_Enable;

   procedure RNG_Clock_Disable is
   begin
      Register.AHB2ENR := Register.AHB2ENR and not AHB2ENR_RNGEN;
   end RNG_Clock_Disable;

   procedure TIM2_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_TIM2EN;
   end TIM2_Clock_Enable;

   procedure TIM3_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_TIM3EN;
   end TIM3_Clock_Enable;

   procedure TIM4_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_TIM4EN;
   end TIM4_Clock_Enable;

   procedure TIM5_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_TIM5EN;
   end TIM5_Clock_Enable;

   procedure TIM6_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_TIM6EN;
   end TIM6_Clock_Enable;

   procedure TIM7_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_TIM7EN;
   end TIM7_Clock_Enable;

   procedure TIM12_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_TIM12EN;
   end TIM12_Clock_Enable;

   procedure TIM13_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_TIM13EN;
   end TIM13_Clock_Enable;

   procedure TIM14_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_TIM14EN;
   end TIM14_Clock_Enable;

   procedure WWDG_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_WWDGEN;
   end WWDG_Clock_Enable;

   procedure SPI2_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_SPI2EN;
   end SPI2_Clock_Enable;

   procedure SPI3_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_SPI3EN;
   end SPI3_Clock_Enable;

   procedure USART2_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_USART2EN;
   end USART2_Clock_Enable;

   procedure USART3_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_USART3EN;
   end USART3_Clock_Enable;

   procedure UART4_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_UART4EN;
   end UART4_Clock_Enable;

   procedure UART5_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_UART5EN;
   end UART5_Clock_Enable;

   procedure UART7_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_UART7EN;
   end UART7_Clock_Enable;

   procedure DAC_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_DACEN;
   end DAC_Clock_Enable;

   procedure UART8_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_UART8EN;
   end UART8_Clock_Enable;

   procedure I2C1_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_I2C1EN;
   end I2C1_Clock_Enable;

   procedure I2C2_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_I2C2EN;
   end I2C2_Clock_Enable;

   procedure I2C3_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_I2C3EN;
   end I2C3_Clock_Enable;

   procedure PWR_Clock_Enable is
   begin
      Register.APB1ENR := Register.APB1ENR or APB1ENR_PWREN;
   end PWR_Clock_Enable;

   procedure TIM2_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_TIM2EN;
   end TIM2_Clock_Disable;

   procedure TIM3_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_TIM3EN;
   end TIM3_Clock_Disable;

   procedure TIM4_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_TIM4EN;
   end TIM4_Clock_Disable;

   procedure TIM5_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_TIM5EN;
   end TIM5_Clock_Disable;

   procedure TIM6_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_TIM6EN;
   end TIM6_Clock_Disable;

   procedure TIM7_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_TIM7EN;
   end TIM7_Clock_Disable;

   procedure TIM12_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_TIM12EN;
   end TIM12_Clock_Disable;

   procedure TIM13_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_TIM13EN;
   end TIM13_Clock_Disable;

   procedure TIM14_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_TIM14EN;
   end TIM14_Clock_Disable;

   procedure WWDG_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_WWDGEN;
   end WWDG_Clock_Disable;

   procedure SPI2_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_SPI2EN;
   end SPI2_Clock_Disable;

   procedure SPI3_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_SPI3EN;
   end SPI3_Clock_Disable;

   procedure USART2_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_USART2EN;
   end USART2_Clock_Disable;

   procedure USART3_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_USART3EN;
   end USART3_Clock_Disable;

   procedure UART4_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_UART4EN;
   end UART4_Clock_Disable;

   procedure UART5_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_UART5EN;
   end UART5_Clock_Disable;

   procedure UART7_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_UART7EN;
   end UART7_Clock_Disable;

   procedure DAC_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_DACEN;
   end DAC_Clock_Disable;

   procedure UART8_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_UART8EN;
   end UART8_Clock_Disable;

   procedure I2C1_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_I2C1EN;
   end I2C1_Clock_Disable;

   procedure I2C2_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_I2C2EN;
   end I2C2_Clock_Disable;

   procedure I2C3_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_I2C3EN;
   end I2C3_Clock_Disable;

   procedure PWR_Clock_Disable is
   begin
      Register.APB1ENR := Register.APB1ENR and not APB1ENR_PWREN;
   end PWR_Clock_Disable;

   procedure TIM1_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_TIM1EN;
   end TIM1_Clock_Enable;

   procedure TIM8_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_TIM8EN;
   end TIM8_Clock_Enable;

   procedure USART1_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_USART1EN;
   end USART1_Clock_Enable;

   procedure USART6_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_USART6EN;
   end USART6_Clock_Enable;

   procedure ADC1_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_ADC1EN;
   end ADC1_Clock_Enable;

   procedure ADC2_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_ADC2EN;
   end ADC2_Clock_Enable;

   procedure ADC3_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_ADC3EN;
   end ADC3_Clock_Enable;

   procedure SDIO_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_SDIOEN;
   end SDIO_Clock_Enable;

   procedure SPI1_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_SPI1EN;
   end SPI1_Clock_Enable;

   procedure SPI4_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_SPI4EN;
   end SPI4_Clock_Enable;

   procedure SYSCFG_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_SYSCFGEN;
   end SYSCFG_Clock_Enable;

   procedure TIM9_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_TIM9EN;
   end TIM9_Clock_Enable;

   procedure TIM10_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_TIM10EN;
   end TIM10_Clock_Enable;

   procedure TIM11_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_TIM11EN;
   end TIM11_Clock_Enable;

   procedure SPI5_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_SPI5EN;
   end SPI5_Clock_Enable;

   procedure SPI6_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_SPI6EN;
   end SPI6_Clock_Enable;

   procedure LTDC_Clock_Enable is
   begin
      Register.APB2ENR := Register.APB2ENR or APB2ENR_LTDCEN;
   end LTDC_Clock_Enable;

   procedure TIM1_Clock_Disable is
   begin
      Register.APB2ENR :=  Register.APB2ENR and not APB2ENR_TIM1EN;
   end TIM1_Clock_Disable;

   procedure TIM8_Clock_Disable is
   begin
      Register.APB2ENR :=  Register.APB2ENR and not APB2ENR_TIM8EN;
   end TIM8_Clock_Disable;

   procedure USART1_Clock_Disable is
   begin
      Register.APB2ENR :=  Register.APB2ENR and not APB2ENR_USART1EN;
   end USART1_Clock_Disable;

   procedure USART6_Clock_Disable is
   begin
      Register.APB2ENR :=  Register.APB2ENR and not APB2ENR_USART6EN;
   end USART6_Clock_Disable;

   procedure ADC1_Clock_Disable is
   begin
      Register.APB2ENR :=  Register.APB2ENR and not APB2ENR_ADC1EN;
   end ADC1_Clock_Disable;

   procedure SDIO_Clock_Disable is
   begin
      Register.APB2ENR :=  Register.APB2ENR and not APB2ENR_SDIOEN;
   end SDIO_Clock_Disable;

   procedure SPI1_Clock_Disable is
   begin
      Register.APB2ENR :=  Register.APB2ENR and not APB2ENR_SPI1EN;
   end SPI1_Clock_Disable;

   procedure SPI4_Clock_Disable is
   begin
      Register.APB2ENR :=  Register.APB2ENR and not APB2ENR_SPI4EN;
   end SPI4_Clock_Disable;

   procedure SYSCFG_Clock_Disable is
   begin
      Register.APB2ENR :=  Register.APB2ENR and not APB2ENR_SYSCFGEN;
   end SYSCFG_Clock_Disable;

   procedure TIM9_Clock_Disable is
   begin
      Register.APB2ENR :=  Register.APB2ENR and not APB2ENR_TIM9EN;
   end TIM9_Clock_Disable;

   procedure TIM10_Clock_Disable is
   begin
      Register.APB2ENR :=  Register.APB2ENR and not APB2ENR_TIM10EN;
   end TIM10_Clock_Disable;

   procedure TIM11_Clock_Disable is
   begin
      Register.APB2ENR :=  Register.APB2ENR and not APB2ENR_TIM11EN;
   end TIM11_Clock_Disable;

   procedure SPI5_Clock_Disable is
   begin
      Register.APB2ENR :=  Register.APB2ENR and not APB2ENR_SPI5EN;
   end SPI5_Clock_Disable;

   procedure SPI6_Clock_Disable is
   begin
      Register.APB2ENR :=  Register.APB2ENR and not APB2ENR_SPI6EN;
   end SPI6_Clock_Disable;

   procedure LTDC_Clock_Disable is
   begin
      Register.APB2ENR :=  Register.APB2ENR and not APB2ENR_LTDCEN;
   end LTDC_Clock_Disable;

   procedure AHB1_Force_Reset is
   begin
      Register.AHB1RSTR := 16#FFFF_FFFF#;
   end AHB1_Force_Reset;

   procedure GPIOA_Force_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR or AHB1RSTR_GPIOARST;
   end GPIOA_Force_Reset;

   procedure GPIOB_Force_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR or AHB1RSTR_GPIOBRST;
   end GPIOB_Force_Reset;

   procedure GPIOC_Force_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR or AHB1RSTR_GPIOCRST;
   end GPIOC_Force_Reset;

   procedure GPIOD_Force_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR or AHB1RSTR_GPIODRST;
   end GPIOD_Force_Reset;

   procedure GPIOE_Force_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR or AHB1RSTR_GPIOERST;
   end GPIOE_Force_Reset;

   procedure GPIOF_Force_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR or AHB1RSTR_GPIOFRST;
   end GPIOF_Force_Reset;

   procedure GPIOG_Force_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR or AHB1RSTR_GPIOGRST;
   end GPIOG_Force_Reset;

   procedure GPIOH_Force_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR or AHB1RSTR_GPIOHRST;
   end GPIOH_Force_Reset;

   procedure GPIOI_Force_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR or AHB1RSTR_GPIOIRST;
   end GPIOI_Force_Reset;

   procedure GPIOJ_Force_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR or AHB1RSTR_GPIOJRST;
   end GPIOJ_Force_Reset;

   procedure GPIOK_Force_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR or AHB1RSTR_GPIOKRST;
   end GPIOK_Force_Reset;

   procedure CRC_Force_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR or AHB1RSTR_CRCRST;
   end CRC_Force_Reset;

   procedure DMA1_Force_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR or AHB1RSTR_DMA1RST;
   end DMA1_Force_Reset;

   procedure DMA2_Force_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR or AHB1RSTR_DMA2RST;
   end DMA2_Force_Reset;


   procedure AHB1_Release_Reset is
   begin
      Register.AHB1RSTR := 0;
   end AHB1_Release_Reset;


   procedure GPIOA_Release_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR and not AHB1RSTR_GPIOARST;
   end GPIOA_Release_Reset;

   procedure GPIOB_Release_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR and not AHB1RSTR_GPIOBRST;
   end GPIOB_Release_Reset;

   procedure GPIOC_Release_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR and not AHB1RSTR_GPIOCRST;
   end GPIOC_Release_Reset;

   procedure GPIOD_Release_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR and not AHB1RSTR_GPIODRST;
   end GPIOD_Release_Reset;

   procedure GPIOE_Release_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR and not AHB1RSTR_GPIOERST;
   end GPIOE_Release_Reset;

   procedure GPIOF_Release_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR and not AHB1RSTR_GPIOFRST;
   end GPIOF_Release_Reset;

   procedure GPIOG_Release_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR and not AHB1RSTR_GPIOGRST;
   end GPIOG_Release_Reset;

   procedure GPIOH_Release_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR and not AHB1RSTR_GPIOHRST;
   end GPIOH_Release_Reset;

   procedure GPIOI_Release_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR and not AHB1RSTR_GPIOIRST;
   end GPIOI_Release_Reset;

   procedure GPIOJ_Release_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR and not AHB1RSTR_GPIOJRST;
   end GPIOJ_Release_Reset;

   procedure GPIOK_Release_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR and not AHB1RSTR_GPIOKRST;
   end GPIOK_Release_Reset;

   procedure CRC_Release_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR and not AHB1RSTR_CRCRST;
   end CRC_Release_Reset;

   procedure DMA1_Release_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR and not AHB1RSTR_DMA1RST;
   end DMA1_Release_Reset;

   procedure DMA2_Release_Reset is
   begin
      Register.AHB1RSTR := Register.AHB1RSTR and not AHB1RSTR_DMA2RST;
   end DMA2_Release_Reset;

   procedure AHB2_Force_Reset is
   begin
      Register.AHB2RSTR := 16#FFFF_FFFF#;
   end AHB2_Force_Reset;

   procedure OTGFS_Force_Reset is
   begin
      Register.AHB2RSTR := Register.AHB2RSTR or AHB2RSTR_OTGFSRST;
   end OTGFS_Force_Reset;

   procedure AHB2_Release_Reset is
   begin
      Register.AHB2RSTR := 0;
   end AHB2_Release_Reset;

   procedure OTGFS_Release_Reset is
   begin
      Register.AHB2RSTR := Register.AHB2RSTR and not AHB2RSTR_OTGFSRST;
   end OTGFS_Release_Reset;


   procedure RNG_Force_Reset is
   begin
      Register.AHB2RSTR := Register.AHB2RSTR or AHB2RSTR_RNGRST;
   end RNG_Force_Reset;

   procedure RNG_Release_Reset is
   begin
      Register.AHB2RSTR := Register.AHB2RSTR and not AHB2RSTR_RNGRST;
   end RNG_Release_Reset;

   procedure APB1_Force_Reset is
   begin
      Register.APB1RSTR := 16#FFFF_FFFF#;
   end APB1_Force_Reset;

   procedure TIM2_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_TIM2RST;
   end TIM2_Force_Reset;

   procedure TIM3_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_TIM3RST;
   end TIM3_Force_Reset;

   procedure TIM4_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_TIM4RST;
   end TIM4_Force_Reset;

   procedure TIM5_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_TIM5RST;
   end TIM5_Force_Reset;

   procedure TIM6_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_TIM6RST;
   end TIM6_Force_Reset;

   procedure TIM7_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_TIM7RST;
   end TIM7_Force_Reset;

   procedure TIM12_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_TIM12RST;
   end TIM12_Force_Reset;

   procedure TIM13_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_TIM13RST;
   end TIM13_Force_Reset;

   procedure TIM14_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_TIM14RST;
   end TIM14_Force_Reset;

   procedure WWDG_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_WWDGRST;
   end WWDG_Force_Reset;

   procedure SPI2_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_SPI2RST;
   end SPI2_Force_Reset;

   procedure SPI3_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_SPI3RST;
   end SPI3_Force_Reset;

   procedure USART2_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_USART2RST;
   end USART2_Force_Reset;

   procedure USART3_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_USART3RST;
   end USART3_Force_Reset;

   procedure UART4_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_UART4RST;
   end UART4_Force_Reset;

   procedure UART5_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_UART5RST;
   end UART5_Force_Reset;

   procedure UART7_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_UART7RST;
   end UART7_Force_Reset;

   procedure DAC_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_DACRST;
   end DAC_Force_Reset;

   procedure UART8_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_UART8RST;
   end UART8_Force_Reset;

   procedure I2C1_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_I2C1RST;
   end I2C1_Force_Reset;

   procedure I2C2_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_I2C2RST;
   end I2C2_Force_Reset;

   procedure I2C3_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_I2C3RST;
   end I2C3_Force_Reset;

   procedure PWR_Force_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR or APB1RSTR_PWRRST;
   end PWR_Force_Reset;

   procedure APB1_Release_Reset is
   begin
      Register.APB1RSTR := 0;
   end APB1_Release_Reset;

   procedure TIM2_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_TIM2RST;
   end TIM2_Release_Reset;

   procedure TIM3_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_TIM3RST;
   end TIM3_Release_Reset;

   procedure TIM4_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_TIM4RST;
   end TIM4_Release_Reset;

   procedure TIM5_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_TIM5RST;
   end TIM5_Release_Reset;

   procedure TIM6_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_TIM6RST;
   end TIM6_Release_Reset;

   procedure TIM7_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_TIM7RST;
   end TIM7_Release_Reset;

   procedure TIM12_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_TIM12RST;
   end TIM12_Release_Reset;

   procedure TIM13_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_TIM13RST;
   end TIM13_Release_Reset;

   procedure TIM14_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_TIM14RST;
   end TIM14_Release_Reset;

   procedure WWDG_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_WWDGRST;
   end WWDG_Release_Reset;

   procedure SPI2_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_SPI2RST;
   end SPI2_Release_Reset;

   procedure SPI3_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_SPI3RST;
   end SPI3_Release_Reset;

   procedure USART2_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_USART2RST;
   end USART2_Release_Reset;

   procedure USART3_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_USART3RST;
   end USART3_Release_Reset;

   procedure UART4_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_UART4RST;
   end UART4_Release_Reset;

   procedure UART5_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_UART5RST;
   end UART5_Release_Reset;

   procedure UART7_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_UART7RST;
   end UART7_Release_Reset;

   procedure UART8_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_UART8RST;
   end UART8_Release_Reset;

   procedure DAC_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_DACRST;
   end DAC_Release_Reset;

   procedure I2C1_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_I2C1RST;
   end I2C1_Release_Reset;

   procedure I2C2_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_I2C2RST;
   end I2C2_Release_Reset;

   procedure I2C3_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_I2C3RST;
   end I2C3_Release_Reset;

   procedure PWR_Release_Reset is
   begin
      Register.APB1RSTR := Register.APB1RSTR and not APB1RSTR_PWRRST;
   end PWR_Release_Reset;

   procedure APB2_Force_Reset is
   begin
      Register.APB2RSTR := 16#FFFF_FFFF#;
   end APB2_Force_Reset;

   procedure TIM1_Force_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR or APB2RSTR_TIM1RST;
   end TIM1_Force_Reset;

   procedure TIM8_Force_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR or APB2RSTR_TIM8RST;
   end TIM8_Force_Reset;

   procedure USART1_Force_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR or APB2RSTR_USART1RST;
   end USART1_Force_Reset;

   procedure USART6_Force_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR or APB2RSTR_USART6RST;
   end USART6_Force_Reset;

   procedure ADC_Force_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR or APB2RSTR_ADCRST;
   end ADC_Force_Reset;

   procedure SDIO_Force_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR or APB2RSTR_SDIORST;
   end SDIO_Force_Reset;

   procedure SPI1_Force_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR or APB2RSTR_SPI1RST;
   end SPI1_Force_Reset;

   procedure SPI4_Force_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR or APB2RSTR_SPI4RST;
   end SPI4_Force_Reset;

   procedure SYSCFG_Force_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR or APB2RSTR_SYSCFGRST;
   end SYSCFG_Force_Reset;

   procedure TIM9_Force_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR or APB2RSTR_TIM9RST;
   end TIM9_Force_Reset;

   procedure TIM10_Force_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR or APB2RSTR_TIM10RST;
   end TIM10_Force_Reset;

   procedure TIM11_Force_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR or APB2RSTR_TIM11RST;
   end TIM11_Force_Reset;

   procedure SPI5_Force_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR or APB2RSTR_SPI5RST;
   end SPI5_Force_Reset;

   procedure SPI6_Force_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR or APB2RSTR_SPI6RST;
   end SPI6_Force_Reset;

   procedure LTDC_Force_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR or APB2RSTR_LTDCRST;
   end LTDC_Force_Reset;

   procedure APB2_Release_Reset is
   begin
      Register.APB2RSTR := 0;
   end APB2_Release_Reset;

   procedure TIM1_Release_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR and not APB2RSTR_TIM1RST;
   end TIM1_Release_Reset;

   procedure TIM8_Release_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR and not APB2RSTR_TIM8RST;
   end TIM8_Release_Reset;

   procedure USART1_Release_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR and not APB2RSTR_USART1RST;
   end USART1_Release_Reset;

   procedure USART6_Release_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR and not APB2RSTR_USART6RST;
   end USART6_Release_Reset;

   procedure ADC_Release_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR and not APB2RSTR_ADCRST;
   end ADC_Release_Reset;

   procedure SDIO_Release_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR and not APB2RSTR_SDIORST;
   end SDIO_Release_Reset;

   procedure SPI1_Release_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR and not APB2RSTR_SPI1RST;
   end SPI1_Release_Reset;

   procedure SPI4_Release_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR and not APB2RSTR_SPI4RST;
   end SPI4_Release_Reset;

   procedure SYSCFG_Release_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR and not APB2RSTR_SYSCFGRST;
   end SYSCFG_Release_Reset;

   procedure TIM9_Release_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR and not APB2RSTR_TIM9RST;
   end TIM9_Release_Reset;

   procedure TIM10_Release_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR and not APB2RSTR_TIM10RST;
   end TIM10_Release_Reset;

   procedure TIM11_Release_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR and not APB2RSTR_TIM11RST;
   end TIM11_Release_Reset;

   procedure SPI5_Release_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR and not APB2RSTR_SPI5RST;
   end SPI5_Release_Reset;

   procedure SPI6_Release_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR and not APB2RSTR_SPI6RST;
   end SPI6_Release_Reset;

   procedure LTDC_Release_Reset is
   begin
      Register.APB2RSTR := Register.APB2RSTR and not APB2RSTR_LTDCRST;
   end LTDC_Release_Reset;

   procedure FSMC_Clock_Enable is
   begin
     Register.AHB3ENR := Register.AHB3ENR or AHB3ENR_FSMCEN;
   end FSMC_Clock_Enable;

   procedure FSMC_Clock_Disable is
   begin
     Register.AHB3ENR := Register.AHB3ENR and not AHB3ENR_FSMCEN;
   end FSMC_Clock_Disable;

end STM32.RCC;
