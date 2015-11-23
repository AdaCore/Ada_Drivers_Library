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
--   @file    stm32f429xx.h                                                 --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   CMSIS STM32F407xx Device Peripheral Access Layer Header File. --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides declarations for devices on the STM32F42xxx MCUs
--  manufactured by ST Microelectronics.  For example, an STM32F429.

with STM32F4.GPIO;    use STM32F4.GPIO;
with STM32F4.ADC;     use STM32F4.ADC;
with STM32F4.DMA;     use STM32F4.DMA;
with STM32F4.USARTs;  use STM32F4.USARTs;
with STM32F4.I2C;     use STM32F4.I2C;
with STM32F4.SPI;     use STM32F4.SPI;
with STM32F4.Timers;  use STM32F4.Timers;
with STM32F4.DAC;     use STM32F4.DAC;

use STM32F4;  -- for base addresses

package STM32F42xxx is
   pragma Elaborate_Body;

   Unknown_Device : exception;
   --  Raised by the routines below for a device passed as an actual parameter
   --  when that device is not present on the given hardware instance.

   GPIO_A : aliased GPIO_Port with Volatile, Address => GPIOA_Base;
   GPIO_B : aliased GPIO_Port with Volatile, Address => GPIOB_Base;
   GPIO_C : aliased GPIO_Port with Volatile, Address => GPIOC_Base;
   GPIO_D : aliased GPIO_Port with Volatile, Address => GPIOD_Base;
   GPIO_E : aliased GPIO_Port with Volatile, Address => GPIOE_Base;
   GPIO_F : aliased GPIO_Port with Volatile, Address => GPIOF_Base;
   GPIO_G : aliased GPIO_Port with Volatile, Address => GPIOG_Base;
   GPIO_H : aliased GPIO_Port with Volatile, Address => GPIOH_Base;
   GPIO_I : aliased GPIO_Port with Volatile, Address => GPIOI_Base;
   GPIO_J : aliased GPIO_Port with Volatile, Address => GPIOJ_Base;
   GPIO_K : aliased GPIO_Port with Volatile, Address => GPIOK_Base;

   procedure Enable_Clock (This : aliased in out GPIO_Port);

   procedure Reset (This : aliased in out GPIO_Port);

   ADC_1 : aliased Analog_To_Digital_Converter with Volatile, Address => ADC1_Base;
   ADC_2 : aliased Analog_To_Digital_Converter with Volatile, Address => ADC2_Base;
   ADC_3 : aliased Analog_To_Digital_Converter with Volatile, Address => ADC3_Base;

   VBat               : constant ADC_Point := (ADC_1'Access, Channel => VBat_Channel);
   Temperature_Sensor : constant ADC_Point := VBat;
   --  see RM pg 410, section 13.10, also pg 389

   VBat_Bridge_Divisor : constant := 4;
   --  The VBAT pin is internally connected to a bridge divider. The actual
   --  voltage is the raw conversion value * the divisor. See section 13.11,
   --  pg 412 of the RM.

   procedure Enable_Clock (This : aliased in out Analog_To_Digital_Converter);

   procedure Reset_All_ADC_Units;

   DAC_1 : aliased Digital_To_Analog_Converter with Volatile, Address => DAC_Base;

   DAC_Channel_1_IO : constant GPIO_Point := (GPIO_A'Access, Pin_4);
   DAC_Channel_2_IO : constant GPIO_Point := (GPIO_A'Access, Pin_5);

   procedure Enable_Clock (This : aliased in out Digital_To_Analog_Converter);

   procedure Reset (This : aliased in out Digital_To_Analog_Converter);

   USART_1 : aliased USART with Volatile, Address => USART1_Base;
   USART_2 : aliased USART with Volatile, Address => USART2_Base;
   USART_3 : aliased USART with Volatile, Address => USART3_Base;
   UART_4  : aliased USART with Volatile, Address => UART4_Base;
   UART_5  : aliased USART with Volatile, Address => UART5_Base;
   USART_6 : aliased USART with Volatile, Address => USART6_Base;
   UART_7  : aliased USART with Volatile, Address => UART7_Base;
   UART_8  : aliased USART with Volatile, Address => UART8_Base;

   procedure Enable_Clock (This : aliased in out USART);

   procedure Reset (This : aliased in out USART);

   DMA_1 : aliased DMA_Controller with Volatile, Address => DMA1_Base;
   DMA_2 : aliased DMA_Controller with Volatile, Address => DMA2_Base;

   procedure Enable_Clock (This : aliased in out DMA_Controller);

   procedure Reset (This : aliased in out DMA_Controller);

   I2C_1 : aliased I2C_Port with Volatile, Address => I2C1_Base;
   I2C_2 : aliased I2C_Port with Volatile, Address => I2C2_Base;
   I2C_3 : aliased I2C_Port with Volatile, Address => I2C3_Base;

   procedure Enable_Clock (This : aliased in out I2C_Port);

   procedure Reset (This : in out I2C_Port);

   SPI_1 : aliased SPI_Port with Volatile, Address => SPI1_Base;
   SPI_2 : aliased SPI_Port with Volatile, Address => SPI2_Base;
   SPI_3 : aliased SPI_Port with Volatile, Address => SPI3_Base;
   SPI_4 : aliased SPI_Port with Volatile, Address => SPI4_Base;
   SPI_5 : aliased SPI_Port with Volatile, Address => SPI5_Base;
   SPI_6 : aliased SPI_Port with Volatile, Address => SPI6_Base;

   procedure Enable_Clock (This : aliased in out SPI_Port);

   procedure Reset (This : in out SPI_Port);

   Timer_1 : aliased Timer with Volatile, Address => TIM1_Base;
   pragma Import (Ada, Timer_1);
   Timer_2 : aliased Timer with Volatile, Address => TIM2_Base;
   pragma Import (Ada, Timer_2);
   Timer_3 : aliased Timer with Volatile, Address => TIM3_Base;
   pragma Import (Ada, Timer_3);
   Timer_4 : aliased Timer with Volatile, Address => TIM4_Base;
   pragma Import (Ada, Timer_4);
   Timer_5 : aliased Timer with Volatile, Address => TIM5_Base;
   pragma Import (Ada, Timer_5);
   Timer_6 : aliased Timer with Volatile, Address => TIM6_Base;
   pragma Import (Ada, Timer_6);
   Timer_7 : aliased Timer with Volatile, Address => TIM7_Base;
   pragma Import (Ada, Timer_7);
   Timer_8 : aliased Timer with Volatile, Address => TIM8_Base;
   pragma Import (Ada, Timer_8);
   Timer_9 : aliased Timer with Volatile, Address => TIM9_Base;
   pragma Import (Ada, Timer_9);
   Timer_10 : aliased Timer with Volatile, Address => TIM10_Base;
   pragma Import (Ada, Timer_10);
   Timer_11 : aliased Timer with Volatile, Address => TIM11_Base;
   pragma Import (Ada, Timer_11);
   Timer_12 : aliased Timer with Volatile, Address => TIM12_Base;
   pragma Import (Ada, Timer_12);
   Timer_13 : aliased Timer with Volatile, Address => TIM13_Base;
   pragma Import (Ada, Timer_13);
   Timer_14 : aliased Timer with Volatile, Address => TIM14_Base;
   pragma Import (Ada, Timer_14);

   procedure Enable_Clock (This : in out Timer);

   procedure Reset (This : in out Timer);

end STM32F42xxx;
