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
--   @file    stm32f4xx_hal_rcc.h                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   Header file of RCC HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides Reset and Clock Control definitions for the STM32F4
--  (ARM Cortex M4F) microcontrollers from ST Microelectronics.

private with STM32_SVD;

package STM32.RCC is

   type RCC_System_Clocks is record
      SYSCLK  : Word;
      HCLK    : Word;
      PCLK1   : Word;
      PCLK2   : Word;
      TIMCLK1 : Word;
      TIMCLK2 : Word;
   end record;

   function System_Clock_Frequencies return RCC_System_Clocks;

   procedure Set_PLLSAI_Factors
     (LCD  : Bits_3;
      SAI1 : Bits_4;
      VCO  : Bits_9;
      DivR : Bits_2);

   procedure Enable_PLLSAI;

   procedure GPIOA_Clock_Enable with Inline;
   procedure GPIOB_Clock_Enable with Inline;
   procedure GPIOC_Clock_Enable with Inline;
   procedure GPIOD_Clock_Enable with Inline;
   procedure GPIOE_Clock_Enable with Inline;
   procedure GPIOF_Clock_Enable with Inline;
   procedure GPIOG_Clock_Enable with Inline;
   procedure GPIOH_Clock_Enable with Inline;
   procedure GPIOI_Clock_Enable with Inline;
   procedure GPIOJ_Clock_Enable with Inline;
   procedure GPIOK_Clock_Enable with Inline;
   procedure CRC_Clock_Enable with Inline;
   procedure BKPSRAM_Clock_Enable with Inline;
   procedure CCMDATARAMEN_Clock_Enable with Inline;
   procedure DMA1_Clock_Enable with Inline;
   procedure DMA2_Clock_Enable with Inline;

   procedure GPIOA_Clock_Disable with Inline;
   procedure GPIOB_Clock_Disable with Inline;
   procedure GPIOC_Clock_Disable with Inline;
   procedure GPIOD_Clock_Disable with Inline;
   procedure GPIOE_Clock_Disable with Inline;
   procedure GPIOF_Clock_Disable with Inline;
   procedure GPIOG_Clock_Disable with Inline;
   procedure GPIOH_Clock_Disable with Inline;
   procedure GPIOI_Clock_Disable with Inline;
   procedure GPIOJ_Clock_Disable with Inline;
   procedure GPIOK_Clock_Disable with Inline;

   procedure CRC_Clock_Disable with Inline;
   procedure BKPSRAM_Clock_Disable with Inline;
   procedure CCMDATARAMEN_Clock_Disable with Inline;
   procedure DMA1_Clock_Disable with Inline;
   procedure DMA2_Clock_Disable with Inline;

   procedure RNG_Clock_Enable with Inline;
   procedure RNG_Clock_Disable with Inline;

   procedure TIM2_Clock_Enable with Inline;
   procedure TIM3_Clock_Enable with Inline;
   procedure TIM4_Clock_Enable with Inline;
   procedure TIM5_Clock_Enable with Inline;
   procedure TIM6_Clock_Enable with Inline;
   procedure TIM7_Clock_Enable with Inline;
   procedure TIM12_Clock_Enable with Inline;
   procedure TIM13_Clock_Enable with Inline;
   procedure TIM14_Clock_Enable with Inline;
   procedure WWDG_Clock_Enable with Inline;
   procedure SPI2_Clock_Enable with Inline;
   procedure SPI3_Clock_Enable with Inline;
   procedure USART2_Clock_Enable with Inline;
   procedure USART3_Clock_Enable with Inline;
   procedure UART4_Clock_Enable with Inline;
   procedure UART5_Clock_Enable with Inline;
   procedure UART7_Clock_Enable with Inline;
   procedure DAC_Clock_Enable with Inline;
   procedure UART8_Clock_Enable with Inline;

   procedure I2C1_Clock_Enable with Inline;
   procedure I2C2_Clock_Enable with Inline;
   procedure I2C3_Clock_Enable with Inline;
   procedure PWR_Clock_Enable with Inline;

   procedure TIM2_Clock_Disable with Inline;
   procedure TIM3_Clock_Disable with Inline;
   procedure TIM4_Clock_Disable with Inline;
   procedure TIM5_Clock_Disable with Inline;
   procedure TIM6_Clock_Disable with Inline;
   procedure TIM7_Clock_Disable with Inline;
   procedure TIM12_Clock_Disable with Inline;
   procedure TIM13_Clock_Disable with Inline;
   procedure TIM14_Clock_Disable with Inline;
   procedure WWDG_Clock_Disable with Inline;
   procedure SPI2_Clock_Disable with Inline;
   procedure SPI3_Clock_Disable with Inline;
   procedure USART2_Clock_Disable with Inline;
   procedure USART3_Clock_Disable with Inline;
   procedure UART4_Clock_Disable with Inline;
   procedure UART5_Clock_Disable with Inline;
   procedure UART7_Clock_Disable with Inline;
   procedure DAC_Clock_Disable with Inline;
   procedure UART8_Clock_Disable with Inline;
   procedure I2C1_Clock_Disable with Inline;
   procedure I2C2_Clock_Disable with Inline;
   procedure I2C3_Clock_Disable with Inline;
   procedure PWR_Clock_Disable with Inline;

   procedure TIM1_Clock_Enable with Inline;
   procedure TIM8_Clock_Enable with Inline;
   procedure USART1_Clock_Enable with Inline;
   procedure USART6_Clock_Enable with Inline;
   procedure ADC1_Clock_Enable with Inline;
   procedure ADC2_Clock_Enable with Inline;
   procedure ADC3_Clock_Enable with Inline;
   procedure SDIO_Clock_Enable with Inline;
   procedure SPI1_Clock_Enable with Inline;
   procedure SPI4_Clock_Enable with Inline;
   procedure SYSCFG_Clock_Enable with Inline;
   procedure TIM9_Clock_Enable with Inline;
   procedure TIM10_Clock_Enable with Inline;
   procedure TIM11_Clock_Enable with Inline;
   procedure SPI5_Clock_Enable with Inline;
   procedure SPI6_Clock_Enable with Inline;
   procedure LTDC_Clock_Enable with Inline;

   procedure TIM1_Clock_Disable with Inline;
   procedure TIM8_Clock_Disable with Inline;
   procedure USART1_Clock_Disable with Inline;
   procedure USART6_Clock_Disable with Inline;
   procedure ADC1_Clock_Disable with Inline;
   procedure SDIO_Clock_Disable with Inline;
   procedure SPI1_Clock_Disable with Inline;
   procedure SPI4_Clock_Disable with Inline;
   procedure SYSCFG_Clock_Disable with Inline;
   procedure TIM9_Clock_Disable with Inline;
   procedure TIM10_Clock_Disable with Inline;
   procedure TIM11_Clock_Disable with Inline;
   procedure SPI5_Clock_Disable with Inline;
   procedure SPI6_Clock_Disable with Inline;
   procedure LTDC_Clock_Disable with Inline;

   procedure AHB1_Force_Reset with Inline;
   procedure GPIOA_Force_Reset with Inline;
   procedure GPIOB_Force_Reset with Inline;
   procedure GPIOC_Force_Reset with Inline;
   procedure GPIOD_Force_Reset with Inline;
   procedure GPIOE_Force_Reset with Inline;
   procedure GPIOF_Force_Reset with Inline;
   procedure GPIOG_Force_Reset with Inline;
   procedure GPIOH_Force_Reset with Inline;
   procedure GPIOI_Force_Reset with Inline;
   procedure GPIOJ_Force_Reset with Inline;
   procedure GPIOK_Force_Reset with Inline;
   procedure CRC_Force_Reset with Inline;
   procedure DMA1_Force_Reset with Inline;
   procedure DMA2_Force_Reset with Inline;

   procedure AHB1_Release_Reset with Inline;
   procedure GPIOA_Release_Reset with Inline;
   procedure GPIOB_Release_Reset with Inline;
   procedure GPIOC_Release_Reset with Inline;
   procedure GPIOD_Release_Reset with Inline;
   procedure GPIOE_Release_Reset with Inline;
   procedure GPIOF_Release_Reset with Inline;
   procedure GPIOG_Release_Reset with Inline;
   procedure GPIOH_Release_Reset with Inline;
   procedure GPIOI_Release_Reset with Inline;
   procedure GPIOJ_Release_Reset with Inline;
   procedure GPIOK_Release_Reset with Inline;
   procedure CRC_Release_Reset with Inline;
   procedure DMA1_Release_Reset with Inline;
   procedure DMA2_Release_Reset with Inline;

   procedure AHB2_Force_Reset with Inline;
   procedure OTGFS_Force_Reset with Inline;

   procedure AHB2_Release_Reset with Inline;
   procedure OTGFS_Release_Reset with Inline;

   procedure RNG_Force_Reset with Inline;
   procedure RNG_Release_Reset with Inline;

   procedure APB1_Force_Reset with Inline;
   procedure TIM2_Force_Reset with Inline;
   procedure TIM3_Force_Reset with Inline;
   procedure TIM4_Force_Reset with Inline;
   procedure TIM5_Force_Reset with Inline;
   procedure TIM6_Force_Reset with Inline;
   procedure TIM7_Force_Reset with Inline;
   procedure TIM12_Force_Reset with Inline;
   procedure TIM13_Force_Reset with Inline;
   procedure TIM14_Force_Reset with Inline;
   procedure WWDG_Force_Reset with Inline;
   procedure SPI2_Force_Reset with Inline;
   procedure SPI3_Force_Reset with Inline;
   procedure USART2_Force_Reset with Inline;
   procedure USART3_Force_Reset with Inline;
   procedure UART4_Force_Reset with Inline;
   procedure UART5_Force_Reset with Inline;
   procedure UART7_Force_Reset with Inline;
   procedure UART8_Force_Reset with Inline;
   procedure DAC_Force_Reset with Inline;
   procedure I2C1_Force_Reset with Inline;
   procedure I2C2_Force_Reset with Inline;
   procedure I2C3_Force_Reset with Inline;
   procedure PWR_Force_Reset with Inline;

   procedure APB1_Release_Reset with Inline;
   procedure TIM2_Release_Reset with Inline;
   procedure TIM3_Release_Reset with Inline;
   procedure TIM4_Release_Reset with Inline;
   procedure TIM5_Release_Reset with Inline;
   procedure TIM6_Release_Reset with Inline;
   procedure TIM7_Release_Reset with Inline;
   procedure TIM12_Release_Reset with Inline;
   procedure TIM13_Release_Reset with Inline;
   procedure TIM14_Release_Reset with Inline;
   procedure WWDG_Release_Reset with Inline;
   procedure SPI2_Release_Reset with Inline;
   procedure SPI3_Release_Reset with Inline;
   procedure USART2_Release_Reset with Inline;
   procedure USART3_Release_Reset with Inline;
   procedure UART4_Release_Reset with Inline;
   procedure UART5_Release_Reset with Inline;
   procedure UART7_Release_Reset with Inline;
   procedure UART8_Release_Reset with Inline;
   procedure DAC_Release_Reset with Inline;
   procedure I2C1_Release_Reset with Inline;
   procedure I2C2_Release_Reset with Inline;
   procedure I2C3_Release_Reset with Inline;
   procedure PWR_Release_Reset with Inline;


   procedure APB2_Force_Reset with Inline;
   procedure TIM1_Force_Reset with Inline;
   procedure TIM8_Force_Reset with Inline;
   procedure USART1_Force_Reset with Inline;
   procedure USART6_Force_Reset with Inline;
   procedure ADC_Force_Reset with Inline;
   procedure SDIO_Force_Reset with Inline;
   procedure SPI1_Force_Reset with Inline;
   procedure SPI4_Force_Reset with Inline;
   procedure SYSCFG_Force_Reset with Inline;
   procedure TIM9_Force_Reset with Inline;
   procedure TIM10_Force_Reset with Inline;
   procedure TIM11_Force_Reset with Inline;
   procedure SPI5_Force_Reset with Inline;
   procedure SPI6_Force_Reset with Inline;
   procedure LTDC_Force_Reset with Inline;

   procedure APB2_Release_Reset with Inline;
   procedure TIM1_Release_Reset with Inline;
   procedure TIM8_Release_Reset with Inline;
   procedure USART1_Release_Reset with Inline;
   procedure USART6_Release_Reset with Inline;
   procedure ADC_Release_Reset with Inline;
   procedure SDIO_Release_Reset with Inline;
   procedure SPI1_Release_Reset with Inline;
   procedure SPI4_Release_Reset with Inline;
   procedure SYSCFG_Release_Reset with Inline;
   procedure TIM9_Release_Reset with Inline;
   procedure TIM10_Release_Reset with Inline;
   procedure TIM11_Release_Reset with Inline;
   procedure SPI5_Release_Reset with Inline;
   procedure SPI6_Release_Reset with Inline;
   procedure LTDC_Release_Reset with Inline;

   procedure FSMC_Clock_Enable with Inline;
   procedure FSMC_Clock_Disable with Inline;

private

   type RCC_Registers is record
      CR          : Word;  --  clock control register at 16#00#
      PLLCFGR     : Word;  --  PLL configuration register at 16#04#
      CFGR        : Word;  --  clock configuration register at 16#08#
      CIR         : Word;  --  clock interrupt register at 16#0C#
      AHB1RSTR    : Word;  --  AHB1 peripheral reset register at 16#10#
      AHB2RSTR    : Word;  --  AHB2 peripheral reset register at 16#14#
      AHB3RSTR    : Word;  --  AHB3 peripheral reset register at 16#18#
      Reserved_0  : Word;  --  Reserved at 16#1C#
      APB1RSTR    : Word;  --  APB1 peripheral reset register at 16#20#
      APB2RSTR    : Word;  --  APB2 peripheral reset register at 16#24#
      Reserved_1  : Word;  --  Reserved at 16#28#
      Reserved_2  : Word;  --  Reserved at 16#2c#
      AHB1ENR     : Word;  --  AHB1 peripheral clock register at 16#30#
      AHB2ENR     : Word;  --  AHB2 peripheral clock register at 16#34#
      AHB3ENR     : Word;  --  AHB3 peripheral clock register at 16#38#
      Reserved_3  : Word;  --  Reserved at 16#0C#
      APB1ENR     : Word;  --  APB1 peripheral clock enable at 16#40#
      APB2ENR     : Word;  --  APB2 peripheral clock enable at 16#44#
      Reserved_4  : Word;  --  Reserved at 16#48#
      Reserved_5  : Word;  --  Reserved at 16#4c#
      AHB1LPENR   : Word;  --  AHB1 periph. low power clk en. at 16#50#
      AHB2LPENR   : Word;  --  AHB2 periph. low power clk en. at 16#54#
      AHB3LPENR   : Word;  --  AHB3 periph. low power clk en. at 16#58#
      Reserved_6  : Word;  --  Reserved, 16#5C#
      APB1LPENR   : Word;  --  APB1 periph. low power clk en. at 16#60#
      APB2LPENR   : Word;  --  APB2 periph. low power clk en. at 16#64#
      Reserved_7  : Word;  --  Reserved at 16#68#
      Reserved_8  : Word;  --  Reserved at 16#6C#
      BDCR        : Word;  --  Backup domain control register at 16#70#
      CSR         : Word;  --  clock control/status register at 16#74#
      Reserved_9  : Word;  --  Reserved at 16#78#
      Reserved_10 : Word;  --  Reserved at 16#7C#
      SSCGR       : Word;  --  spread spectrum clk gen. reg. at 16#80#
      PLLI2SCFGR  : Word;  --  PLLI2S configuration register at 16#84#
      PLLSAICFGR  : Word;  --  PLLSAI configuration register at 16#88#
      DCKCFGR     : Word;  --  DCK configuration register at 16#8C#
   end record
     with Volatile;

   for RCC_Registers use record
      CR          at   0 range 0 .. 31;
      PLLCFGR     at   4 range 0 .. 31;
      CFGR        at   8 range 0 .. 31;
      CIR         at  12 range 0 .. 31;
      AHB1RSTR    at  16 range 0 .. 31;
      AHB2RSTR    at  20 range 0 .. 31;
      AHB3RSTR    at  24 range 0 .. 31;
      Reserved_0  at  28 range 0 .. 31;
      APB1RSTR    at  32 range 0 .. 31;
      APB2RSTR    at  36 range 0 .. 31;
      Reserved_1  at  40 range 0 .. 31;
      Reserved_2  at  44 range 0 .. 31;
      AHB1ENR     at  48 range 0 .. 31;
      AHB2ENR     at  52 range 0 .. 31;
      AHB3ENR     at  56 range 0 .. 31;
      Reserved_3  at  60 range 0 .. 31;
      APB1ENR     at  64 range 0 .. 31;
      APB2ENR     at  68 range 0 .. 31;
      Reserved_4  at  72 range 0 .. 31;
      Reserved_5  at  76 range 0 .. 31;
      AHB1LPENR   at  80 range 0 .. 31;
      AHB2LPENR   at  84 range 0 .. 31;
      AHB3LPENR   at  88 range 0 .. 31;
      Reserved_6  at  92 range 0 .. 31;
      APB1LPENR   at  96 range 0 .. 31;
      APB2LPENR   at 100 range 0 .. 31;
      Reserved_7  at 104 range 0 .. 31;
      Reserved_8  at 108 range 0 .. 31;
      BDCR        at 112 range 0 .. 31;
      CSR         at 116 range 0 .. 31;
      Reserved_9  at 120 range 0 .. 31;
      Reserved_10 at 124 range 0 .. 31;
      SSCGR       at 128 range 0 .. 31;
      PLLI2SCFGR  at 132 range 0 .. 31;
      PLLSAICFGR  at 136 range 0 .. 31;
      DCKCFGR     at 140 range 0 .. 31;
   end record;


   Register : RCC_Registers with
     Volatile,
     Address => STM32_SVD.RCC_Base,
     Import;

   ---------------------  Constants for RCC_CR register  ---------------------

   HSION  : constant := 16#00000001#;
   HSIRDY : constant := 16#00000002#;

   HSITRIM   : constant := 16#000000F8#;
   HSITRIM_0 : constant := 16#00000008#; -- Bit 0
   HSITRIM_1 : constant := 16#00000010#; -- Bit 1
   HSITRIM_2 : constant := 16#00000020#; -- Bit 2
   HSITRIM_3 : constant := 16#00000040#; -- Bit 3
   HSITRIM_4 : constant := 16#00000080#; -- Bit 4

   HSICAL   : constant := 16#0000FF00#;
   HSICAL_0 : constant := 16#00000100#; -- Bit 0
   HSICAL_1 : constant := 16#00000200#; -- Bit 1
   HSICAL_2 : constant := 16#00000400#; -- Bit 2
   HSICAL_3 : constant := 16#00000800#; -- Bit 3
   HSICAL_4 : constant := 16#00001000#; -- Bit 4
   HSICAL_5 : constant := 16#00002000#; -- Bit 5
   HSICAL_6 : constant := 16#00004000#; -- Bit 6
   HSICAL_7 : constant := 16#00008000#; -- Bit 7

   HSEON     : constant := 16#00010000#;
   HSERDY    : constant := 16#00020000#;
   HSEBYP    : constant := 16#00040000#;
   CSSON     : constant := 16#00080000#;
   PLLON     : constant := 16#01000000#;
   PLLRDY    : constant := 16#02000000#;
   PLLI2SON  : constant := 16#04000000#;
   PLLI2SRDY : constant := 16#08000000#;

   ------------------  Constants for RCC_PLLCFGR register  --------------------

   PLLM   : constant := 16#0000003F#;
   PLLM_0 : constant := 16#00000001#;
   PLLM_1 : constant := 16#00000002#;
   PLLM_2 : constant := 16#00000004#;
   PLLM_3 : constant := 16#00000008#;
   PLLM_4 : constant := 16#00000010#;
   PLLM_5 : constant := 16#00000020#;

   PLLN   : constant := 16#00007FC0#;
   PLLN_0 : constant := 16#00000040#;
   PLLN_1 : constant := 16#00000080#;
   PLLN_2 : constant := 16#00000100#;
   PLLN_3 : constant := 16#00000200#;
   PLLN_4 : constant := 16#00000400#;
   PLLN_5 : constant := 16#00000800#;
   PLLN_6 : constant := 16#00001000#;
   PLLN_7 : constant := 16#00002000#;
   PLLN_8 : constant := 16#00004000#;

   PLLP   : constant := 16#00030000#;
   PLLP_0 : constant := 16#00010000#;
   PLLP_1 : constant := 16#00020000#;

   PLLSRC     : constant := 16#00400000#;
   PLLSRC_HSE : constant := 16#00400000#;
   PLLSRC_HSI : constant := 16#00000000#;

   PLLQ   : constant := 16#0F000000#;
   PLLQ_0 : constant := 16#01000000#;
   PLLQ_1 : constant := 16#02000000#;
   PLLQ_2 : constant := 16#04000000#;
   PLLQ_3 : constant := 16#08000000#;

   ------------------  Constants for RCC_CFGR register  ---------------------

   --  SW configuration
   SW   : constant := 16#00000003#;      --  SW[1:0] bits (System clock Switch)
   SW_0 : constant := 16#00000001#;      --  Bit 0
   SW_1 : constant := 16#00000002#;      --  Bit 1

   SW_HSI : constant := 16#00000000#;    --  HSI selected as system clock
   SW_HSE : constant := 16#00000001#;    --  HSE selected as system clock
   SW_PLL : constant := 16#00000002#;    --  PLL selected as system clock

   --  SWS configuration
   SWS   : constant := 16#0000000C#;     --  System Clock Switch Status bits
   SWS_0 : constant := 16#00000004#;     --  Bit 0
   SWS_1 : constant := 16#00000008#;     --  Bit 1

   SWS_HSI : constant := 16#00000000#;   --  HSI used as system clock
   SWS_HSE : constant := 16#00000004#;   --  HSE used as system clock
   SWS_PLL : constant := 16#00000008#;   --  PLL used as system clock

   --  HPRE configuration
   HPRE   : constant := 16#000000F0#;    --  HPRE[3:0] bits (AHB prescaler)
   HPRE_0 : constant := 16#00000010#;    --  Bit 0
   HPRE_1 : constant := 16#00000020#;    --  Bit 1
   HPRE_2 : constant := 16#00000040#;    --  Bit 2
   HPRE_3 : constant := 16#00000080#;    --  Bit 3

   HPRE_DIV1   : constant := 16#00000000#;         --  SYSCLK not divided
   HPRE_DIV2   : constant := 16#00000080#;         --  SYSCLK divided by 2
   HPRE_DIV4   : constant := 16#00000090#;         --  SYSCLK divided by 4
   HPRE_DIV8   : constant := 16#000000A0#;         --  SYSCLK divided by 8
   HPRE_DIV16  : constant := 16#000000B0#;         --  SYSCLK divided by 16
   HPRE_DIV64  : constant := 16#000000C0#;         --  SYSCLK divided by 64
   HPRE_DIV128 : constant := 16#000000D0#;         --  SYSCLK divided by 128
   HPRE_DIV256 : constant := 16#000000E0#;         --  SYSCLK divided by 256
   HPRE_DIV512 : constant := 16#000000F0#;         --  SYSCLK divided by 512

   --  PPRE1 configuration
   PPRE1   : constant := 16#00001C00#;         --  APB1 prescaler bits
   PPRE1_0 : constant := 16#00000400#;         --  Bit 0
   PPRE1_1 : constant := 16#00000800#;         --  Bit 1
   PPRE1_2 : constant := 16#00001000#;         --  Bit 2

   PPRE1_DIV1  : constant := 16#00000000#;         --  HCLK not divided
   PPRE1_DIV2  : constant := 16#00001000#;         --  HCLK divided by 2
   PPRE1_DIV4  : constant := 16#00001400#;         --  HCLK divided by 4
   PPRE1_DIV8  : constant := 16#00001800#;         --  HCLK divided by 8
   PPRE1_DIV16 : constant := 16#00001C00#;         --  HCLK divided by 16

   --  PPRE2 configuration
   PPRE2   : constant := 16#0000E000#;         --  APB2 prescaler bits
   PPRE2_0 : constant := 16#00002000#;         --  Bit 0
   PPRE2_1 : constant := 16#00004000#;         --  Bit 1
   PPRE2_2 : constant := 16#00008000#;         --  Bit 2

   PPRE2_DIV1  : constant := 16#00000000#;         --  HCLK not divided
   PPRE2_DIV2  : constant := 16#00008000#;         --  HCLK divided by 2
   PPRE2_DIV4  : constant := 16#0000A000#;         --  HCLK divided by 4
   PPRE2_DIV8  : constant := 16#0000C000#;         --  HCLK divided by 8
   PPRE2_DIV16 : constant := 16#0000E000#;         --  HCLK divided by 16

   --  RTCPRE configuration
   RTCPRE   : constant := 16#001F0000#;
   RTCPRE_0 : constant := 16#00010000#;
   RTCPRE_1 : constant := 16#00020000#;
   RTCPRE_2 : constant := 16#00040000#;
   RTCPRE_3 : constant := 16#00080000#;
   RTCPRE_4 : constant := 16#00100000#;

   --  MCO1 configuration
   MCO1   : constant := 16#00600000#;
   MCO1_0 : constant := 16#00200000#;
   MCO1_1 : constant := 16#00400000#;

   I2SSRC : constant := 16#00800000#;

   MCO1PRE   : constant := 16#07000000#;
   MCO1PRE_0 : constant := 16#01000000#;
   MCO1PRE_1 : constant := 16#02000000#;
   MCO1PRE_2 : constant := 16#04000000#;

   MCO2PRE   : constant := 16#38000000#;
   MCO2PRE_0 : constant := 16#08000000#;
   MCO2PRE_1 : constant := 16#10000000#;
   MCO2PRE_2 : constant := 16#20000000#;

   MCO2   : constant := 16#C0000000#;
   MCO2_0 : constant := 16#40000000#;
   MCO2_1 : constant := 16#80000000#;

   ------------------  Constants for RCC_CIR register  ------------------------

   LSIRDYF    : constant := 16#00000001#;
   LSERDYF    : constant := 16#00000002#;
   HSIRDYF    : constant := 16#00000004#;
   HSERDYF    : constant := 16#00000008#;
   PLLRDYF    : constant := 16#00000010#;
   PLLI2SRDYF : constant := 16#00000020#;

   CSSF        : constant := 16#00000080#;
   LSIRDYIE    : constant := 16#00000100#;
   LSERDYIE    : constant := 16#00000200#;
   HSIRDYIE    : constant := 16#00000400#;
   HSERDYIE    : constant := 16#00000800#;
   PLLRDYIE    : constant := 16#00001000#;
   PLLI2SRDYIE : constant := 16#00002000#;

   LSIRDYC    : constant := 16#00010000#;
   LSERDYC    : constant := 16#00020000#;
   HSIRDYC    : constant := 16#00040000#;
   HSERDYC    : constant := 16#00080000#;
   PLLRDYC    : constant := 16#00100000#;
   PLLI2SRDYC : constant := 16#00200000#;

   CSSC : constant := 16#00800000#;

   ------------------  Constants for RCC_AHB1RSTR register  -------------------

   --  Note: we include the prefix for the sake of being able to check, when
   --  using the constants, that they are being assigned to the right bus.

   AHB1RSTR_GPIOARST  : constant := 16#00000001#;
   AHB1RSTR_GPIOBRST  : constant := 16#00000002#;
   AHB1RSTR_GPIOCRST  : constant := 16#00000004#;
   AHB1RSTR_GPIODRST  : constant := 16#00000008#;
   AHB1RSTR_GPIOERST  : constant := 16#00000010#;
   AHB1RSTR_GPIOFRST  : constant := 16#00000020#;
   AHB1RSTR_GPIOGRST  : constant := 16#00000040#;
   AHB1RSTR_GPIOHRST  : constant := 16#00000080#;
   AHB1RSTR_GPIOIRST  : constant := 16#00000100#;
   AHB1RSTR_GPIOJRST  : constant := 16#00000200#;
   AHB1RSTR_GPIOKRST  : constant := 16#00000400#;
   AHB1RSTR_CRCRST    : constant := 16#00001000#;
   AHB1RSTR_DMA1RST   : constant := 16#00200000#;
   AHB1RSTR_DMA2RST   : constant := 16#00400000#;
   AHB1RSTR_ETHMACRST : constant := 16#02000000#;
   AHB1RSTR_OTGHRST   : constant := 16#10000000#;

   ------------------  Constants for RCC_AHB2RSTR register  -------------------

   AHB2RSTR_DCMIRST  : constant := 16#00000001#;
   AHB2RSTR_RNGRST   : constant := 16#00000040#;
   AHB2RSTR_OTGFSRST : constant := 16#00000080#;

   ------------------  Constants for RCC_AHB3RSTR register  -------------------

   AHB3RSTR_FSMCRST : constant := 16#00000001#;

   ------------------  Constants for RCC_APB1RSTR register  -------------------

   APB1RSTR_TIM2RST   : constant := 16#00000001#;
   APB1RSTR_TIM3RST   : constant := 16#00000002#;
   APB1RSTR_TIM4RST   : constant := 16#00000004#;
   APB1RSTR_TIM5RST   : constant := 16#00000008#;
   APB1RSTR_TIM6RST   : constant := 16#00000010#;
   APB1RSTR_TIM7RST   : constant := 16#00000020#;
   APB1RSTR_TIM12RST  : constant := 16#00000040#;
   APB1RSTR_TIM13RST  : constant := 16#00000080#;
   APB1RSTR_TIM14RST  : constant := 16#00000100#;
   APB1RSTR_WWDGRST   : constant := 16#00000800#;
   APB1RSTR_SPI2RST   : constant := 16#00004000#;
   APB1RSTR_SPI3RST   : constant := 16#00008000#;
   APB1RSTR_USART2RST : constant := 16#00020000#;
   APB1RSTR_USART3RST : constant := 16#00040000#;
   APB1RSTR_UART4RST  : constant := 16#00080000#;
   APB1RSTR_UART5RST  : constant := 16#00100000#;
   APB1RSTR_I2C1RST   : constant := 16#00200000#;
   APB1RSTR_I2C2RST   : constant := 16#00400000#;
   APB1RSTR_I2C3RST   : constant := 16#00800000#;
   APB1RSTR_CAN1RST   : constant := 16#02000000#;
   APB1RSTR_CAN2RST   : constant := 16#04000000#;
   APB1RSTR_PWRRST    : constant := 16#10000000#;
   APB1RSTR_DACRST    : constant := 16#20000000#;
   APB1RSTR_UART7RST  : constant := 16#40000000#;
   APB1RSTR_UART8RST  : constant := 16#80000000#;

   ------------------  Constants for RCC_APB2RSTR register  -------------------

   APB2RSTR_TIM1RST   : constant := 16#00000001#;
   APB2RSTR_TIM8RST   : constant := 16#00000002#;
   APB2RSTR_USART1RST : constant := 16#00000010#;
   APB2RSTR_USART6RST : constant := 16#00000020#;
   APB2RSTR_ADCRST    : constant := 16#00000100#;
   APB2RSTR_SDIORST   : constant := 16#00000800#;
   APB2RSTR_SPI1RST   : constant := 16#00001000#;
   APB2RSTR_SPI4RST   : constant := 16#00002000#;
   APB2RSTR_SYSCFGRST : constant := 16#00004000#;
   APB2RSTR_TIM9RST   : constant := 16#00010000#;
   APB2RSTR_TIM10RST  : constant := 16#00020000#;
   APB2RSTR_TIM11RST  : constant := 16#00040000#;
   APB2RSTR_SPI5RST   : constant := 16#00100000#;
   APB2RSTR_SPI6RST   : constant := 16#00200000#;
   APB2RSTR_LTDCRST   : constant := 16#04000000#;

   ------------------  Constants for RCC_AHB1ENR register  --------------------

   AHB1ENR_GPIOAEN      : constant := 16#00000001#;
   AHB1ENR_GPIOBEN      : constant := 16#00000002#;
   AHB1ENR_GPIOCEN      : constant := 16#00000004#;
   AHB1ENR_GPIODEN      : constant := 16#00000008#;
   AHB1ENR_GPIOEEN      : constant := 16#00000010#;
   AHB1ENR_GPIOFEN      : constant := 16#00000020#;
   AHB1ENR_GPIOGEN      : constant := 16#00000040#;
   AHB1ENR_GPIOHEN      : constant := 16#00000080#;
   AHB1ENR_GPIOIEN      : constant := 16#00000100#;
   AHB1ENR_GPIOJEN      : constant := 16#00000200#;
   AHB1ENR_GPIOKEN      : constant := 16#00000400#;

   AHB1ENR_CRCEN        : constant := 16#00001000#;
   AHB1ENR_BKPSRAMEN    : constant := 16#00040000#;
   AHB1ENR_CCMDATARAMEN : constant := 16#00100000#;
   AHB1ENR_DMA1EN       : constant := 16#00200000#;
   AHB1ENR_DMA2EN       : constant := 16#00400000#;

   AHB1ENR_ETHMACEN    : constant := 16#02000000#;
   AHB1ENR_ETHMACTXEN  : constant := 16#04000000#;
   AHB1ENR_ETHMACRXEN  : constant := 16#08000000#;
   AHB1ENR_ETHMACPTPEN : constant := 16#10000000#;
   AHB1ENR_OTGHSEN     : constant := 16#20000000#;
   AHB1ENR_OTGHSULPIEN : constant := 16#40000000#;

   ------------------  Constants for RCC_AHB2ENR register  --------------------

   AHB2ENR_DCMIEN  : constant := 16#00000001#;
   AHB2ENR_RNGEN   : constant := 16#00000040#;
   AHB2ENR_OTGFSEN : constant := 16#00000080#;

   ------------------  Constants for RCC_AHB3ENR register  --------------------

   AHB3ENR_FSMCEN : constant := 16#00000001#;

   ------------------  Constants for RCC_APB1ENR register  --------------------

   APB1ENR_TIM2EN   : constant := 16#00000001#;
   APB1ENR_TIM3EN   : constant := 16#00000002#;
   APB1ENR_TIM4EN   : constant := 16#00000004#;
   APB1ENR_TIM5EN   : constant := 16#00000008#;
   APB1ENR_TIM6EN   : constant := 16#00000010#;
   APB1ENR_TIM7EN   : constant := 16#00000020#;
   APB1ENR_TIM12EN  : constant := 16#00000040#;
   APB1ENR_TIM13EN  : constant := 16#00000080#;
   APB1ENR_TIM14EN  : constant := 16#00000100#;
   APB1ENR_WWDGEN   : constant := 16#00000800#;
   APB1ENR_SPI2EN   : constant := 16#00004000#;
   APB1ENR_SPI3EN   : constant := 16#00008000#;
   APB1ENR_USART2EN : constant := 16#00020000#;
   APB1ENR_USART3EN : constant := 16#00040000#;
   APB1ENR_UART4EN  : constant := 16#00080000#;
   APB1ENR_UART5EN  : constant := 16#00100000#;
   APB1ENR_I2C1EN   : constant := 16#00200000#;
   APB1ENR_I2C2EN   : constant := 16#00400000#;
   APB1ENR_I2C3EN   : constant := 16#00800000#;
   APB1ENR_CAN1EN   : constant := 16#02000000#;
   APB1ENR_CAN2EN   : constant := 16#04000000#;
   APB1ENR_PWREN    : constant := 16#10000000#;
   APB1ENR_DACEN    : constant := 16#20000000#;
   APB1ENR_UART7EN  : constant := 16#40000000#;
   APB1ENR_UART8EN  : constant := 16#80000000#;

   ------------------  Constants for RCC_APB2ENR register  --------------------

   APB2ENR_TIM1EN   : constant := 16#00000001#;
   APB2ENR_TIM8EN   : constant := 16#00000002#;
   APB2ENR_USART1EN : constant := 16#00000010#;
   APB2ENR_USART6EN : constant := 16#00000020#;
   APB2ENR_ADC1EN   : constant := 16#00000100#;
   APB2ENR_ADC2EN   : constant := 16#00000200#;
   APB2ENR_ADC3EN   : constant := 16#00000400#;
   APB2ENR_SDIOEN   : constant := 16#00000800#;
   APB2ENR_SPI1EN   : constant := 16#00001000#;
   APB2ENR_SPI4EN   : constant := 16#00002000#;
   APB2ENR_SYSCFGEN : constant := 16#00004000#;
   APB2ENR_TIM9EN   : constant := 16#00010000#;
   APB2ENR_TIM10EN  : constant := 16#00020000#;
   APB2ENR_TIM11EN  : constant := 16#00040000#;
   APB2ENR_SPI5EN   : constant := 16#00100000#;
   APB2ENR_SPI6EN   : constant := 16#00200000#;
   APB2ENR_LTDCEN   : constant := 16#04000000#;

   ------------------  Constants for RCC_AHB1LPENR register  ------------------

   AHB1LPENR_GPIOALPEN     : constant := 16#00000001#;
   AHB1LPENR_GPIOBLPEN     : constant := 16#00000002#;
   AHB1LPENR_GPIOCLPEN     : constant := 16#00000004#;
   AHB1LPENR_GPIODLPEN     : constant := 16#00000008#;
   AHB1LPENR_GPIOELPEN     : constant := 16#00000010#;
   AHB1LPENR_GPIOFLPEN     : constant := 16#00000020#;
   AHB1LPENR_GPIOGLPEN     : constant := 16#00000040#;
   AHB1LPENR_GPIOHLPEN     : constant := 16#00000080#;
   AHB1LPENR_GPIOILPEN     : constant := 16#00000100#;
   AHB1LPENR_CRCLPEN       : constant := 16#00001000#;
   AHB1LPENR_FLITFLPEN     : constant := 16#00008000#;
   AHB1LPENR_SRAM1LPEN     : constant := 16#00010000#;
   AHB1LPENR_SRAM2LPEN     : constant := 16#00020000#;
   AHB1LPENR_BKPSRAMLPEN   : constant := 16#00040000#;
   AHB1LPENR_SRAM3LPEN     : constant := 16#00080000#;
   AHB1LPENR_DMA1LPEN      : constant := 16#00200000#;
   AHB1LPENR_DMA2LPEN      : constant := 16#00400000#;
   AHB1LPENR_ETHMACLPEN    : constant := 16#02000000#;
   AHB1LPENR_ETHMACTXLPEN  : constant := 16#04000000#;
   AHB1LPENR_ETHMACRXLPEN  : constant := 16#08000000#;
   AHB1LPENR_ETHMACPTPLPEN : constant := 16#10000000#;
   AHB1LPENR_OTGHSLPEN     : constant := 16#20000000#;
   AHB1LPENR_OTGHSULPILPEN : constant := 16#40000000#;

   ------------------  Constants for RCC_AHB2LPENR register  ------------------

   AHB2LPENR_DCMILPEN  : constant := 16#00000001#;
   AHB2LPENR_RNGLPEN   : constant := 16#00000040#;
   AHB2LPENR_OTGFSLPEN : constant := 16#00000080#;

   ------------------  Constants for RCC_AHB3LPENR register  ------------------

   AHB3LPENR_FSMCLPEN : constant := 16#00000001#;

   ------------------  Constants for RCC_APB1LPENR register  ------------------

   APB1LPENR_TIM2LPEN   : constant := 16#00000001#;
   APB1LPENR_TIM3LPEN   : constant := 16#00000002#;
   APB1LPENR_TIM4LPEN   : constant := 16#00000004#;
   APB1LPENR_TIM5LPEN   : constant := 16#00000008#;
   APB1LPENR_TIM6LPEN   : constant := 16#00000010#;
   APB1LPENR_TIM7LPEN   : constant := 16#00000020#;
   APB1LPENR_TIM12LPEN  : constant := 16#00000040#;
   APB1LPENR_TIM13LPEN  : constant := 16#00000080#;
   APB1LPENR_TIM14LPEN  : constant := 16#00000100#;
   APB1LPENR_WWDGLPEN   : constant := 16#00000800#;
   APB1LPENR_SPI2LPEN   : constant := 16#00004000#;
   APB1LPENR_SPI3LPEN   : constant := 16#00008000#;
   APB1LPENR_USART2LPEN : constant := 16#00020000#;
   APB1LPENR_USART3LPEN : constant := 16#00040000#;
   APB1LPENR_UART4LPEN  : constant := 16#00080000#;
   APB1LPENR_UART5LPEN  : constant := 16#00100000#;
   APB1LPENR_I2C1LPEN   : constant := 16#00200000#;
   APB1LPENR_I2C2LPEN   : constant := 16#00400000#;
   APB1LPENR_I2C3LPEN   : constant := 16#00800000#;
   APB1LPENR_CAN1LPEN   : constant := 16#02000000#;
   APB1LPENR_CAN2LPEN   : constant := 16#04000000#;
   APB1LPENR_PWRLPEN    : constant := 16#10000000#;
   APB1LPENR_DACLPEN    : constant := 16#20000000#;

   ------------------  Constants for RCC_APB2LPENR register  ------------------

   APB2LPENR_TIM1LPEN   : constant := 16#00000001#;
   APB2LPENR_TIM8LPEN   : constant := 16#00000002#;
   APB2LPENR_USART1LPEN : constant := 16#00000010#;
   APB2LPENR_USART6LPEN : constant := 16#00000020#;
   APB2LPENR_ADC1LPEN   : constant := 16#00000100#;
   APB2LPENR_ADC2LPEN   : constant := 16#00000200#;
   APB2LPENR_ADC3LPEN   : constant := 16#00000400#;
   APB2LPENR_SDIOLPEN   : constant := 16#00000800#;
   APB2LPENR_SPI1LPEN   : constant := 16#00001000#;
   APB2LPENR_SPI4LPEN   : constant := 16#00002000#;
   APB2LPENR_SYSCFGLPEN : constant := 16#00004000#;
   APB2LPENR_TIM9LPEN   : constant := 16#00010000#;
   APB2LPENR_TIM10LPEN  : constant := 16#00020000#;
   APB2LPENR_TIM11LPEN  : constant := 16#00040000#;
   APB2LPENR_SPI5LPEN   : constant := 16#00100000#;
   APB2LPENR_SPI6LPEN   : constant := 16#00200000#;
   APB2LPENR_LTDCLPEN   : constant := 16#04000000#;

   ------------------  Constants for RCC_BDCR register  ---------------------

   LSEON  : constant := 16#00000001#;
   LSERDY : constant := 16#00000002#;
   LSEBYP : constant := 16#00000004#;

   RTCSEL   : constant := 16#00000300#;
   RTCSEL_0 : constant := 16#00000100#;
   RTCSEL_1 : constant := 16#00000200#;

   RTCEN : constant := 16#00008000#;
   BDRST : constant := 16#00010000#;

   ------------------  Constants for RCC_CSR register  ---------------------

   LSION    : constant := 16#00000001#;
   LSIRDY   : constant := 16#00000002#;
   RMVF     : constant := 16#01000000#;
   BORRSTF  : constant := 16#02000000#;
   PADRSTF  : constant := 16#04000000#;
   PORRSTF  : constant := 16#08000000#;
   SFTRSTF  : constant := 16#10000000#;
   WDGRSTF  : constant := 16#20000000#;
   WWDGRSTF : constant := 16#40000000#;
   LPWRRSTF : constant := 16#80000000#;

   ------------------  Constants for RCC_SSCGR register  ---------------------

   MODPER    : constant := 16#00001FFF#;
   INCSTEP   : constant := 16#0FFFE000#;
   SPREADSEL : constant := 16#40000000#;
   SSCGEN    : constant := 16#80000000#;

   ------------------  Constants for RCC_PLLI2SCFGR register  -----------------

   PLLI2SN   : constant := 16#00007FC0#;
   PLLI2SN_0 : constant := 16#00000040#;
   PLLI2SN_1 : constant := 16#00000080#;
   PLLI2SN_2 : constant := 16#00000100#;
   PLLI2SN_3 : constant := 16#00000200#;
   PLLI2SN_4 : constant := 16#00000400#;
   PLLI2SN_5 : constant := 16#00000800#;
   PLLI2SN_6 : constant := 16#00001000#;
   PLLI2SN_7 : constant := 16#00002000#;
   PLLI2SN_8 : constant := 16#00004000#;

   PLLI2SR   : constant := 16#70000000#;
   PLLI2SR_0 : constant := 16#10000000#;
   PLLI2SR_1 : constant := 16#20000000#;
   PLLI2SR_2 : constant := 16#40000000#;

end STM32.RCC;
