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

with STM32F4.RCC; use STM32F4.RCC;
with System;

package body STM32F42xxx is

   use type System.Address;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out GPIO_Port) is
   begin
      if This'Address = GPIOA_Base then
         GPIOA_Clock_Enable;
      elsif This'Address = GPIOB_Base then
         GPIOB_Clock_Enable;
      elsif This'Address = GPIOC_Base then
         GPIOC_Clock_Enable;
      elsif This'Address = GPIOD_Base then
         GPIOD_Clock_Enable;
      elsif This'Address = GPIOE_Base then
         GPIOE_Clock_Enable;
      elsif This'Address = GPIOF_Base then
         GPIOF_Clock_Enable;
      elsif This'Address = GPIOG_Base then
         GPIOG_Clock_Enable;
      elsif This'Address = GPIOH_Base then
         GPIOH_Clock_Enable;
      elsif This'Address = GPIOI_Base then
         GPIOI_Clock_Enable;
      elsif This'Address = GPIOJ_Base then
         GPIOJ_Clock_Enable;
      elsif This'Address = GPIOK_Base then
         GPIOK_Clock_Enable;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased in out GPIO_Port) is
   begin
      if This'Address = GPIOA_Base then
         GPIOA_Force_Reset;
         GPIOA_Release_Reset;
      elsif This'Address = GPIOB_Base then
         GPIOB_Force_Reset;
         GPIOB_Release_Reset;
      elsif This'Address = GPIOC_Base then
         GPIOC_Force_Reset;
         GPIOC_Release_Reset;
      elsif This'Address = GPIOD_Base then
         GPIOD_Force_Reset;
         GPIOD_Release_Reset;
      elsif This'Address = GPIOE_Base then
         GPIOE_Force_Reset;
         GPIOE_Release_Reset;
      elsif This'Address = GPIOF_Base then
         GPIOF_Force_Reset;
         GPIOF_Release_Reset;
      elsif This'Address = GPIOG_Base then
         GPIOG_Force_Reset;
         GPIOG_Release_Reset;
      elsif This'Address = GPIOH_Base then
         GPIOH_Force_Reset;
         GPIOH_Release_Reset;
      elsif This'Address = GPIOI_Base then
         GPIOI_Force_Reset;
         GPIOI_Release_Reset;
      elsif This'Address = GPIOJ_Base then
         GPIOJ_Force_Reset;
         GPIOJ_Release_Reset;
      elsif This'Address = GPIOK_Base then
         GPIOK_Force_Reset;
         GPIOK_Release_Reset;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out Analog_To_Digital_Converter) is
   begin
      if This'Address = ADC1_Base then
         ADC1_Clock_Enable;
      elsif This'Address = ADC2_Base then
         ADC2_Clock_Enable;
      elsif This'Address = ADC3_Base then
         ADC3_Clock_Enable;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -------------------------
   -- Reset_All_ADC_Units --
   -------------------------

   procedure Reset_All_ADC_Units is
   begin
      ADC_Force_Reset;
      ADC_Release_Reset;
   end Reset_All_ADC_Units;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out Digital_To_Analog_Converter) is
      pragma Unreferenced (This);
   begin
      RCC.DAC_Clock_Enable;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased in out Digital_To_Analog_Converter) is
      pragma Unreferenced (This);
   begin
      RCC.DAC_Force_Reset;
      RCC.DAC_Release_Reset;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out USART) is
   begin
      if This'Address = USART1_Base then
         USART1_Clock_Enable;
      elsif This'Address = USART2_Base then
         USART2_Clock_Enable;
      elsif This'Address = USART3_Base then
         USART3_Clock_Enable;
      elsif This'Address = USART6_Base then
         USART6_Clock_Enable;
      elsif This'Address = UART4_Base then
         UART4_Clock_Enable;
      elsif This'Address = UART5_Base then
         UART5_Clock_Enable;
      elsif This'Address = UART7_Base then
         UART7_Clock_Enable;
      elsif This'Address = UART8_Base then
         UART8_Clock_Enable;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased in out USART) is
   begin
      if This'Address = USART1_Base then
         USART1_Force_Reset;
         USART1_Release_Reset;
      elsif This'Address = USART2_Base then
         USART2_Force_Reset;
         USART2_Release_Reset;
      elsif This'Address = USART3_Base then
         USART3_Force_Reset;
         USART3_Release_Reset;
      elsif This'Address = USART6_Base then
         USART6_Force_Reset;
         USART6_Release_Reset;
      elsif This'Address = UART4_Base then
         UART4_Force_Reset;
         UART4_Release_Reset;
      elsif This'Address = UART5_Base then
         UART5_Force_Reset;
         UART5_Release_Reset;
      elsif This'Address = UART7_Base then
         UART7_Force_Reset;
         UART7_Release_Reset;
      elsif This'Address = UART8_Base then
         UART8_Force_Reset;
         UART8_Release_Reset;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out DMA_Controller) is
   begin
      if This'Address = STM32F4.DMA1_Base then
         DMA1_Clock_Enable;
      elsif This'Address = STM32F4.DMA2_Base then
         DMA2_Clock_Enable;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : aliased in out DMA_Controller) is
   begin
      if This'Address = STM32F4.DMA1_Base then
         DMA1_Force_Reset;
         DMA1_Release_Reset;
      elsif This'Address = STM32F4.DMA2_Base then
         DMA2_Force_Reset;
         DMA2_Release_Reset;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out I2C_Port) is
   begin
      if This'Address = I2C1_Base then
         I2C1_Clock_Enable;
      elsif This'Address = I2C2_Base then
         I2C2_Clock_Enable;
      elsif This'Address = I2C3_Base then
         I2C3_Clock_Enable;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out I2C_Port) is
   begin
      if This'Address = I2C1_Base then
         I2C1_Force_Reset;
         I2C1_Release_Reset;
      elsif This'Address = I2C2_Base then
         I2C2_Force_Reset;
         I2C2_Release_Reset;
      elsif This'Address = I2C3_Base then
         I2C3_Force_Reset;
         I2C3_Release_Reset;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out SPI_Port) is
   begin
      if This'Address = SPI1_Base then
         SPI1_Force_Reset;
         SPI1_Release_Reset;
      elsif This'Address = SPI2_Base then
         SPI2_Force_Reset;
         SPI2_Release_Reset;
      elsif This'Address = SPI3_Base then
         SPI3_Force_Reset;
         SPI3_Release_Reset;
      elsif This'Address = SPI4_Base then
         SPI4_Force_Reset;
         SPI4_Release_Reset;
      elsif This'Address = SPI5_Base then
         SPI5_Force_Reset;
         SPI5_Release_Reset;
      elsif This'Address = SPI6_Base then
         SPI6_Force_Reset;
         SPI6_Release_Reset;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out SPI_Port) is
   begin
      if This'Address = SPI1_Base then
         SPI1_Clock_Enable;
      elsif This'Address = SPI2_Base then
         SPI2_Clock_Enable;
      elsif This'Address = SPI3_Base then
         SPI3_Clock_Enable;
      elsif This'Address = SPI4_Base then
         SPI4_Clock_Enable;
      elsif This'Address = SPI5_Base then
         SPI5_Clock_Enable;
      elsif This'Address = SPI6_Base then
         SPI6_Clock_Enable;
      else
         raise Unknown_Device;
      end if;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : in out Timer) is
   begin
      if This'Address = TIM1_Base then
         TIM1_Clock_Enable;
      elsif This'Address = TIM2_Base then
         TIM2_Clock_Enable;
      elsif This'Address = TIM3_Base then
         TIM3_Clock_Enable;
      elsif This'Address = TIM4_Base then
         TIM4_Clock_Enable;
      elsif This'Address = TIM5_Base then
         TIM5_Clock_Enable;
      elsif This'Address = TIM6_Base then
         TIM6_Clock_Enable;
      elsif This'Address = TIM7_Base then
         TIM7_Clock_Enable;
      elsif This'Address = TIM8_Base then
         TIM8_Clock_Enable;
      elsif This'Address = TIM9_Base then
         TIM9_Clock_Enable;
      elsif This'Address = TIM10_Base then
         TIM10_Clock_Enable;
      elsif This'Address = TIM11_Base then
         TIM11_Clock_Enable;
      elsif This'Address = TIM12_Base then
         TIM12_Clock_Enable;
      elsif This'Address = TIM13_Base then
         TIM13_Clock_Enable;
      elsif This'Address = TIM14_Base then
         TIM14_Clock_Enable;
      else
         raise Unknown_Device;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Timer) is
   begin
      if This'Address = TIM1_Base then
         TIM1_Force_Reset;
         TIM1_Release_Reset;
      elsif This'Address = TIM2_Base then
         TIM2_Force_Reset;
         TIM2_Release_Reset;
      elsif This'Address = TIM3_Base then
         TIM3_Force_Reset;
         TIM3_Release_Reset;
      elsif This'Address = TIM4_Base then
         TIM4_Force_Reset;
         TIM4_Release_Reset;
      elsif This'Address = TIM5_Base then
         TIM5_Force_Reset;
         TIM5_Release_Reset;
      elsif This'Address = TIM6_Base then
         TIM6_Force_Reset;
         TIM6_Release_Reset;
      elsif This'Address = TIM7_Base then
         TIM7_Force_Reset;
         TIM7_Release_Reset;
      elsif This'Address = TIM8_Base then
         TIM8_Force_Reset;
         TIM8_Release_Reset;
      elsif This'Address = TIM9_Base then
         TIM9_Force_Reset;
         TIM9_Release_Reset;
      elsif This'Address = TIM10_Base then
         TIM10_Force_Reset;
         TIM10_Release_Reset;
      elsif This'Address = TIM11_Base then
         TIM11_Force_Reset;
         TIM11_Release_Reset;
      elsif This'Address = TIM12_Base then
         TIM12_Force_Reset;
         TIM12_Release_Reset;
      elsif This'Address = TIM13_Base then
         TIM13_Force_Reset;
         TIM13_Release_Reset;
      elsif This'Address = TIM14_Base then
         TIM14_Force_Reset;
         TIM14_Release_Reset;
      else
         raise Unknown_Device;
      end if;
   end Reset;

end STM32F42xxx;
