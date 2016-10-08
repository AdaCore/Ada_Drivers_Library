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
--   @file    stm32f40[5|7]xx.h                                             --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   CMSIS STM32F407xx Device Peripheral Access Layer Header File. --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides declarations for devices on the STM32F40xxx MCUs
--  manufactured by ST Microelectronics.  For example, an STM32F405.

with STM32_SVD;     use STM32_SVD;

with STM32.DMA;     use STM32.DMA;
with STM32.GPIO;    use STM32.GPIO;
with STM32.ADC;     use STM32.ADC;
with STM32.USARTs;  use STM32.USARTs;
with STM32.SPI;     use STM32.SPI;
with STM32.Timers;  use STM32.Timers;
with STM32.DAC;     use STM32.DAC;
with STM32.I2C;     use STM32.I2C;

package STM32.Device is
   pragma Elaborate_Body;

   Unknown_Device : exception;
   --  Raised by the routines below for a device passed as an actual parameter
   --  when that device is not present on the given hardware instance.

   procedure Enable_Clock (This : aliased in out GPIO_Port)
     with Inline;
   procedure Enable_Clock (Point : GPIO_Point)
     with Inline;
   procedure Enable_Clock (Points : GPIO_Points)
     with Inline;

   procedure Reset (This : aliased in out GPIO_Port)
     with Inline;
   procedure Reset (Point : GPIO_Point)
     with Inline;
   procedure Reset (Points : GPIO_Points)
     with Inline;

   function GPIO_Port_Representation (Port : GPIO_Port) return UInt4
     with Inline;

   GPIO_A : aliased GPIO_Port with Import, Volatile, Address => GPIOA_Base;
   GPIO_B : aliased GPIO_Port with Import, Volatile, Address => GPIOB_Base;
   GPIO_C : aliased GPIO_Port with Import, Volatile, Address => GPIOC_Base;
   GPIO_D : aliased GPIO_Port with Import, Volatile, Address => GPIOD_Base;
   GPIO_E : aliased GPIO_Port with Import, Volatile, Address => GPIOE_Base;
   GPIO_F : aliased GPIO_Port with Import, Volatile, Address => GPIOF_Base;
   GPIO_G : aliased GPIO_Port with Import, Volatile, Address => GPIOG_Base;
   GPIO_H : aliased GPIO_Port with Import, Volatile, Address => GPIOH_Base;
   GPIO_I : aliased GPIO_Port with Import, Volatile, Address => GPIOI_Base;

   PA0  : aliased GPIO_Point := (GPIO_A'Access, 0);
   PA1  : aliased GPIO_Point := (GPIO_A'Access, 1);
   PA2  : aliased GPIO_Point := (GPIO_A'Access, 2);
   PA3  : aliased GPIO_Point := (GPIO_A'Access, 3);
   PA4  : aliased GPIO_Point := (GPIO_A'Access, 4);
   PA5  : aliased GPIO_Point := (GPIO_A'Access, 5);
   PA6  : aliased GPIO_Point := (GPIO_A'Access, 6);
   PA7  : aliased GPIO_Point := (GPIO_A'Access, 7);
   PA8  : aliased GPIO_Point := (GPIO_A'Access, 8);
   PA9  : aliased GPIO_Point := (GPIO_A'Access, 9);
   PA10 : aliased GPIO_Point := (GPIO_A'Access, 10);
   PA11 : aliased GPIO_Point := (GPIO_A'Access, 11);
   PA12 : aliased GPIO_Point := (GPIO_A'Access, 12);
   PA13 : aliased GPIO_Point := (GPIO_A'Access, 13);
   PA14 : aliased GPIO_Point := (GPIO_A'Access, 14);
   PA15 : aliased GPIO_Point := (GPIO_A'Access, 15);
   PB0  : aliased GPIO_Point := (GPIO_B'Access, 0);
   PB1  : aliased GPIO_Point := (GPIO_B'Access, 1);
   PB2  : aliased GPIO_Point := (GPIO_B'Access, 2);
   PB3  : aliased GPIO_Point := (GPIO_B'Access, 3);
   PB4  : aliased GPIO_Point := (GPIO_B'Access, 4);
   PB5  : aliased GPIO_Point := (GPIO_B'Access, 5);
   PB6  : aliased GPIO_Point := (GPIO_B'Access, 6);
   PB7  : aliased GPIO_Point := (GPIO_B'Access, 7);
   PB8  : aliased GPIO_Point := (GPIO_B'Access, 8);
   PB9  : aliased GPIO_Point := (GPIO_B'Access, 9);
   PB10 : aliased GPIO_Point := (GPIO_B'Access, 10);
   PB11 : aliased GPIO_Point := (GPIO_B'Access, 11);
   PB12 : aliased GPIO_Point := (GPIO_B'Access, 12);
   PB13 : aliased GPIO_Point := (GPIO_B'Access, 13);
   PB14 : aliased GPIO_Point := (GPIO_B'Access, 14);
   PB15 : aliased GPIO_Point := (GPIO_B'Access, 15);
   PC0  : aliased GPIO_Point := (GPIO_C'Access, 0);
   PC1  : aliased GPIO_Point := (GPIO_C'Access, 1);
   PC2  : aliased GPIO_Point := (GPIO_C'Access, 2);
   PC3  : aliased GPIO_Point := (GPIO_C'Access, 3);
   PC4  : aliased GPIO_Point := (GPIO_C'Access, 4);
   PC5  : aliased GPIO_Point := (GPIO_C'Access, 5);
   PC6  : aliased GPIO_Point := (GPIO_C'Access, 6);
   PC7  : aliased GPIO_Point := (GPIO_C'Access, 7);
   PC8  : aliased GPIO_Point := (GPIO_C'Access, 8);
   PC9  : aliased GPIO_Point := (GPIO_C'Access, 9);
   PC10 : aliased GPIO_Point := (GPIO_C'Access, 10);
   PC11 : aliased GPIO_Point := (GPIO_C'Access, 11);
   PC12 : aliased GPIO_Point := (GPIO_C'Access, 12);
   PC13 : aliased GPIO_Point := (GPIO_C'Access, 13);
   PC14 : aliased GPIO_Point := (GPIO_C'Access, 14);
   PC15 : aliased GPIO_Point := (GPIO_C'Access, 15);
   PD0  : aliased GPIO_Point := (GPIO_D'Access, 0);
   PD1  : aliased GPIO_Point := (GPIO_D'Access, 1);
   PD2  : aliased GPIO_Point := (GPIO_D'Access, 2);
   PD3  : aliased GPIO_Point := (GPIO_D'Access, 3);
   PD4  : aliased GPIO_Point := (GPIO_D'Access, 4);
   PD5  : aliased GPIO_Point := (GPIO_D'Access, 5);
   PD6  : aliased GPIO_Point := (GPIO_D'Access, 6);
   PD7  : aliased GPIO_Point := (GPIO_D'Access, 7);
   PD8  : aliased GPIO_Point := (GPIO_D'Access, 8);
   PD9  : aliased GPIO_Point := (GPIO_D'Access, 9);
   PD10 : aliased GPIO_Point := (GPIO_D'Access, 10);
   PD11 : aliased GPIO_Point := (GPIO_D'Access, 11);
   PD12 : aliased GPIO_Point := (GPIO_D'Access, 12);
   PD13 : aliased GPIO_Point := (GPIO_D'Access, 13);
   PD14 : aliased GPIO_Point := (GPIO_D'Access, 14);
   PD15 : aliased GPIO_Point := (GPIO_D'Access, 15);
   PE0  : aliased GPIO_Point := (GPIO_E'Access, 0);
   PE1  : aliased GPIO_Point := (GPIO_E'Access, 1);
   PE2  : aliased GPIO_Point := (GPIO_E'Access, 2);
   PE3  : aliased GPIO_Point := (GPIO_E'Access, 3);
   PE4  : aliased GPIO_Point := (GPIO_E'Access, 4);
   PE5  : aliased GPIO_Point := (GPIO_E'Access, 5);
   PE6  : aliased GPIO_Point := (GPIO_E'Access, 6);
   PE7  : aliased GPIO_Point := (GPIO_E'Access, 7);
   PE8  : aliased GPIO_Point := (GPIO_E'Access, 8);
   PE9  : aliased GPIO_Point := (GPIO_E'Access, 9);
   PE10 : aliased GPIO_Point := (GPIO_E'Access, 10);
   PE11 : aliased GPIO_Point := (GPIO_E'Access, 11);
   PE12 : aliased GPIO_Point := (GPIO_E'Access, 12);
   PE13 : aliased GPIO_Point := (GPIO_E'Access, 13);
   PE14 : aliased GPIO_Point := (GPIO_E'Access, 14);
   PE15 : aliased GPIO_Point := (GPIO_E'Access, 15);
   PF0  : aliased GPIO_Point := (GPIO_F'Access, 0);
   PF1  : aliased GPIO_Point := (GPIO_F'Access, 1);
   PF2  : aliased GPIO_Point := (GPIO_F'Access, 2);
   PF3  : aliased GPIO_Point := (GPIO_F'Access, 3);
   PF4  : aliased GPIO_Point := (GPIO_F'Access, 4);
   PF5  : aliased GPIO_Point := (GPIO_F'Access, 5);
   PF6  : aliased GPIO_Point := (GPIO_F'Access, 6);
   PF7  : aliased GPIO_Point := (GPIO_F'Access, 7);
   PF8  : aliased GPIO_Point := (GPIO_F'Access, 8);
   PF9  : aliased GPIO_Point := (GPIO_F'Access, 9);
   PF10 : aliased GPIO_Point := (GPIO_F'Access, 10);
   PF11 : aliased GPIO_Point := (GPIO_F'Access, 11);
   PF12 : aliased GPIO_Point := (GPIO_F'Access, 12);
   PF13 : aliased GPIO_Point := (GPIO_F'Access, 13);
   PF14 : aliased GPIO_Point := (GPIO_F'Access, 14);
   PF15 : aliased GPIO_Point := (GPIO_F'Access, 15);
   PG0  : aliased GPIO_Point := (GPIO_G'Access, 0);
   PG1  : aliased GPIO_Point := (GPIO_G'Access, 1);
   PG2  : aliased GPIO_Point := (GPIO_G'Access, 2);
   PG3  : aliased GPIO_Point := (GPIO_G'Access, 3);
   PG4  : aliased GPIO_Point := (GPIO_G'Access, 4);
   PG5  : aliased GPIO_Point := (GPIO_G'Access, 5);
   PG6  : aliased GPIO_Point := (GPIO_G'Access, 6);
   PG7  : aliased GPIO_Point := (GPIO_G'Access, 7);
   PG8  : aliased GPIO_Point := (GPIO_G'Access, 8);
   PG9  : aliased GPIO_Point := (GPIO_G'Access, 9);
   PG10 : aliased GPIO_Point := (GPIO_G'Access, 10);
   PG11 : aliased GPIO_Point := (GPIO_G'Access, 11);
   PG12 : aliased GPIO_Point := (GPIO_G'Access, 12);
   PG13 : aliased GPIO_Point := (GPIO_G'Access, 13);
   PG14 : aliased GPIO_Point := (GPIO_G'Access, 14);
   PG15 : aliased GPIO_Point := (GPIO_G'Access, 15);
   PH0  : aliased GPIO_Point := (GPIO_H'Access, 0);
   PH1  : aliased GPIO_Point := (GPIO_H'Access, 1);
   PH2  : aliased GPIO_Point := (GPIO_H'Access, 2);
   PH3  : aliased GPIO_Point := (GPIO_H'Access, 3);
   PH4  : aliased GPIO_Point := (GPIO_H'Access, 4);
   PH5  : aliased GPIO_Point := (GPIO_H'Access, 5);
   PH6  : aliased GPIO_Point := (GPIO_H'Access, 6);
   PH7  : aliased GPIO_Point := (GPIO_H'Access, 7);
   PH8  : aliased GPIO_Point := (GPIO_H'Access, 8);
   PH9  : aliased GPIO_Point := (GPIO_H'Access, 9);
   PH10 : aliased GPIO_Point := (GPIO_H'Access, 10);
   PH11 : aliased GPIO_Point := (GPIO_H'Access, 11);
   PH12 : aliased GPIO_Point := (GPIO_H'Access, 12);
   PH13 : aliased GPIO_Point := (GPIO_H'Access, 13);
   PH14 : aliased GPIO_Point := (GPIO_H'Access, 14);
   PH15 : aliased GPIO_Point := (GPIO_H'Access, 15);
   PI0  : aliased GPIO_Point := (GPIO_I'Access, 0);
   PI1  : aliased GPIO_Point := (GPIO_I'Access, 1);
   PI2  : aliased GPIO_Point := (GPIO_I'Access, 2);
   PI3  : aliased GPIO_Point := (GPIO_I'Access, 3);
   PI4  : aliased GPIO_Point := (GPIO_I'Access, 4);
   PI5  : aliased GPIO_Point := (GPIO_I'Access, 5);
   PI6  : aliased GPIO_Point := (GPIO_I'Access, 6);
   PI7  : aliased GPIO_Point := (GPIO_I'Access, 7);
   PI8  : aliased GPIO_Point := (GPIO_I'Access, 8);
   PI9  : aliased GPIO_Point := (GPIO_I'Access, 9);
   PI10 : aliased GPIO_Point := (GPIO_I'Access, 10);
   PI11 : aliased GPIO_Point := (GPIO_I'Access, 11);
   PI12 : aliased GPIO_Point := (GPIO_I'Access, 12);
   PI13 : aliased GPIO_Point := (GPIO_I'Access, 13);
   PI14 : aliased GPIO_Point := (GPIO_I'Access, 14);
   PI15 : aliased GPIO_Point := (GPIO_I'Access, 15);

   GPIO_AF_0_RTC_50Hz  : constant GPIO_Alternate_Function;
   GPIO_AF_0_MCO       : constant GPIO_Alternate_Function;
   GPIO_AF_0_TAMPER    : constant GPIO_Alternate_Function;
   GPIO_AF_0_SWJ       : constant GPIO_Alternate_Function;
   GPIO_AF_0_TRACE     : constant GPIO_Alternate_Function;
   GPIO_AF_1_TIM1      : constant GPIO_Alternate_Function;
   GPIO_AF_1_TIM2      : constant GPIO_Alternate_Function;
   GPIO_AF_2_TIM3      : constant GPIO_Alternate_Function;
   GPIO_AF_2_TIM4      : constant GPIO_Alternate_Function;
   GPIO_AF_2_TIM5      : constant GPIO_Alternate_Function;
   GPIO_AF_3_TIM8      : constant GPIO_Alternate_Function;
   GPIO_AF_3_TIM9      : constant GPIO_Alternate_Function;
   GPIO_AF_3_TIM10     : constant GPIO_Alternate_Function;
   GPIO_AF_3_TIM11     : constant GPIO_Alternate_Function;
   GPIO_AF_4_I2C1      : constant GPIO_Alternate_Function;
   GPIO_AF_4_I2C2      : constant GPIO_Alternate_Function;
   GPIO_AF_4_I2C3      : constant GPIO_Alternate_Function;
   GPIO_AF_5_SPI1      : constant GPIO_Alternate_Function;
   GPIO_AF_5_SPI2      : constant GPIO_Alternate_Function;
   GPIO_AF_5_I2S2      : constant GPIO_Alternate_Function;
   GPIO_AF_5_I2S2ext   : constant GPIO_Alternate_Function;
   GPIO_AF_6_SPI3      : constant GPIO_Alternate_Function;
   GPIO_AF_6_I2S3      : constant GPIO_Alternate_Function;
   GPIO_AF_6_I2Sext    : constant GPIO_Alternate_Function;
   GPIO_AF_7_I2S3ext   : constant GPIO_Alternate_Function;
   GPIO_AF_7_USART1    : constant GPIO_Alternate_Function;
   GPIO_AF_7_USART2    : constant GPIO_Alternate_Function;
   GPIO_AF_7_USART3    : constant GPIO_Alternate_Function;
   GPIO_AF_8_UART4     : constant GPIO_Alternate_Function;
   GPIO_AF_8_UART5     : constant GPIO_Alternate_Function;
   GPIO_AF_8_USART6    : constant GPIO_Alternate_Function;
   GPIO_AF_9_CAN1      : constant GPIO_Alternate_Function;
   GPIO_AF_9_CAN2      : constant GPIO_Alternate_Function;
   GPIO_AF_9_TIM12     : constant GPIO_Alternate_Function;
   GPIO_AF_9_TIM13     : constant GPIO_Alternate_Function;
   GPIO_AF_9_TIM14     : constant GPIO_Alternate_Function;
   GPIO_AF_10_OTG_FS   : constant GPIO_Alternate_Function;
   GPIO_AF_10_OTG_HS   : constant GPIO_Alternate_Function;
   GPIO_AF_11_ETH      : constant GPIO_Alternate_Function;
   GPIO_AF_12_FMC      : constant GPIO_Alternate_Function;
   GPIO_AF_12_OTG_FS   : constant GPIO_Alternate_Function;
   GPIO_AF_13_DCMI     : constant GPIO_Alternate_Function;
   GPIO_AF_15_EVENTOUT : constant GPIO_Alternate_Function;

   ADC_1 : aliased Analog_To_Digital_Converter with Volatile, Import, Address => ADC1_Base;
   ADC_2 : aliased Analog_To_Digital_Converter with Volatile, Import, Address => ADC2_Base;
   ADC_3 : aliased Analog_To_Digital_Converter with Volatile, Import, Address => ADC3_Base;

   VBat : constant ADC_Point := (ADC_1'Access, Channel => VBat_Channel);

   Temperature_Channel : constant TemperatureSensor_Channel := 16;
   Temperature_Sensor  : constant ADC_Point :=
     (ADC_1'Access, Channel => Temperature_Channel);
   --  see RM pg 410, section 13.10, also pg 389

   VBat_Bridge_Divisor : constant := 2;
   --  The VBAT pin is internally connected to a bridge divider. The actual
   --  voltage is the raw conversion value * the divisor. See section 13.11,
   --  pg 412 of the RM.

   procedure Enable_Clock (This : aliased in out Analog_To_Digital_Converter);

   procedure Reset_All_ADC_Units;

   DAC_1 : aliased Digital_To_Analog_Converter with Import, Volatile, Address => DAC_Base;

   DAC_Channel_1_IO : GPIO_Point renames PA4;
   DAC_Channel_2_IO : GPIO_Point renames PA5;

   procedure Enable_Clock (This : aliased in out Digital_To_Analog_Converter);

   procedure Reset (This : aliased in out Digital_To_Analog_Converter);

   Internal_USART_1 : aliased Internal_USART with Import, Volatile, Address => USART1_Base;
   Internal_USART_2 : aliased Internal_USART with Import, Volatile, Address => USART2_Base;
   Internal_USART_3 : aliased Internal_USART with Import, Volatile, Address => USART3_Base;
   Internal_UART_4  : aliased Internal_USART with Import, Volatile, Address => UART4_Base;
   Internal_UART_5  : aliased Internal_USART with Import, Volatile, Address => UART5_Base;
   Internal_USART_6 : aliased Internal_USART with Import, Volatile, Address => USART6_Base;

   USART_1 : aliased USART (Internal_USART_1'Access);
   USART_2 : aliased USART (Internal_USART_2'Access);
   USART_3 : aliased USART (Internal_USART_3'Access);
   UART_4  : aliased USART (Internal_UART_4'Access);
   UART_5  : aliased USART (Internal_UART_5'Access);
   USART_6 : aliased USART (Internal_USART_6'Access);

   procedure Enable_Clock (This : aliased in out USART);

   procedure Reset (This : aliased in out USART);

   DMA_1 : aliased DMA_Controller with Import, Volatile, Address => DMA1_Base;
   DMA_2 : aliased DMA_Controller with Import, Volatile, Address => DMA2_Base;

   procedure Enable_Clock (This : aliased in out DMA_Controller);

   procedure Reset (This : aliased in out DMA_Controller);

   Internal_I2C_Port_1 : aliased Internal_I2C_Port with Import, Volatile, Address => I2C1_Base;
   Internal_I2C_Port_2 : aliased Internal_I2C_Port with Import, Volatile, Address => I2C2_Base;
   Internal_I2C_Port_3 : aliased Internal_I2C_Port with Import, Volatile, Address => I2C3_Base;

   type I2C_Port_Id is (I2C_Id_1, I2C_Id_2, I2C_Id_3);

   I2C_1 : aliased I2C_Port (Internal_I2C_Port_1'Access);
   I2C_2 : aliased I2C_Port (Internal_I2C_Port_2'Access);
   I2C_3 : aliased I2C_Port (Internal_I2C_Port_3'Access);

   function As_Port_Id (Port : I2C_Port) return I2C_Port_Id with Inline;

   procedure Enable_Clock (This : aliased I2C_Port);
   procedure Enable_Clock (This : I2C_Port_Id);

   procedure Reset (This : I2C_Port);
   procedure Reset (This : I2C_Port_Id);

   Internal_SPI_1 : aliased Internal_SPI_Port with Import, Volatile, Address => SPI1_Base;
   Internal_SPI_2 : aliased Internal_SPI_Port with Import, Volatile, Address => SPI2_Base;
   Internal_SPI_3 : aliased Internal_SPI_Port with Import, Volatile, Address => SPI3_Base;

   SPI_1 : aliased SPI_Port (Internal_SPI_1'Access);
   SPI_2 : aliased SPI_Port (Internal_SPI_2'Access);
   SPI_3 : aliased SPI_Port (Internal_SPI_3'Access);

   procedure Enable_Clock (This : SPI_Port);

   procedure Reset (This : in out SPI_Port);

   Timer_1  : aliased Timer with Import, Volatile, Address => TIM1_Base;
   Timer_2  : aliased Timer with Import, Volatile, Address => TIM2_Base;
   Timer_3  : aliased Timer with Import, Volatile, Address => TIM3_Base;
   Timer_4  : aliased Timer with Import, Volatile, Address => TIM4_Base;
   Timer_5  : aliased Timer with Import, Volatile, Address => TIM5_Base;
   Timer_6  : aliased Timer with Import, Volatile, Address => TIM6_Base;
   Timer_7  : aliased Timer with Import, Volatile, Address => TIM7_Base;
   Timer_8  : aliased Timer with Import, Volatile, Address => TIM8_Base;
   Timer_9  : aliased Timer with Import, Volatile, Address => TIM9_Base;
   Timer_10 : aliased Timer with Import, Volatile, Address => TIM10_Base;
   Timer_11 : aliased Timer with Import, Volatile, Address => TIM11_Base;
   Timer_12 : aliased Timer with Import, Volatile, Address => TIM12_Base;
   Timer_13 : aliased Timer with Import, Volatile, Address => TIM13_Base;
   Timer_14 : aliased Timer with Import, Volatile, Address => TIM14_Base;

   procedure Enable_Clock (This : in out Timer);

   procedure Reset (This : in out Timer);

   -----------------------------
   -- Reset and Clock Control --
   -----------------------------

   type RCC_System_Clocks is record
      SYSCLK  : UInt32;
      HCLK    : UInt32;
      PCLK1   : UInt32;
      PCLK2   : UInt32;
      TIMCLK1 : UInt32;
      TIMCLK2 : UInt32;
   end record;

   function System_Clock_Frequencies return RCC_System_Clocks;

   procedure Enable_DCMI_Clock;

   procedure Reset_DCMI;

private

   GPIO_AF_0_RTC_50Hz  : constant GPIO_Alternate_Function := 0;
   GPIO_AF_0_MCO       : constant GPIO_Alternate_Function := 0;
   GPIO_AF_0_TAMPER    : constant GPIO_Alternate_Function := 0;
   GPIO_AF_0_SWJ       : constant GPIO_Alternate_Function := 0;
   GPIO_AF_0_TRACE     : constant GPIO_Alternate_Function := 0;
   GPIO_AF_1_TIM1      : constant GPIO_Alternate_Function := 1;
   GPIO_AF_1_TIM2      : constant GPIO_Alternate_Function := 1;
   GPIO_AF_2_TIM3      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_2_TIM4      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_2_TIM5      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_3_TIM8      : constant GPIO_Alternate_Function := 3;
   GPIO_AF_3_TIM9      : constant GPIO_Alternate_Function := 3;
   GPIO_AF_3_TIM10     : constant GPIO_Alternate_Function := 3;
   GPIO_AF_3_TIM11     : constant GPIO_Alternate_Function := 3;
   GPIO_AF_4_I2C1      : constant GPIO_Alternate_Function := 4;
   GPIO_AF_4_I2C2      : constant GPIO_Alternate_Function := 4;
   GPIO_AF_4_I2C3      : constant GPIO_Alternate_Function := 4;
   GPIO_AF_5_SPI1      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_5_SPI2      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_5_I2S2      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_5_I2S2ext   : constant GPIO_Alternate_Function := 5;
   GPIO_AF_6_SPI3      : constant GPIO_Alternate_Function := 6;
   GPIO_AF_6_I2S3      : constant GPIO_Alternate_Function := 6;
   GPIO_AF_6_I2Sext    : constant GPIO_Alternate_Function := 6;
   GPIO_AF_7_I2S3ext   : constant GPIO_Alternate_Function := 7;
   GPIO_AF_7_USART1    : constant GPIO_Alternate_Function := 7;
   GPIO_AF_7_USART2    : constant GPIO_Alternate_Function := 7;
   GPIO_AF_7_USART3    : constant GPIO_Alternate_Function := 7;
   GPIO_AF_8_UART4     : constant GPIO_Alternate_Function := 8;
   GPIO_AF_8_UART5     : constant GPIO_Alternate_Function := 8;
   GPIO_AF_8_USART6    : constant GPIO_Alternate_Function := 8;
   GPIO_AF_9_CAN1      : constant GPIO_Alternate_Function := 9;
   GPIO_AF_9_CAN2      : constant GPIO_Alternate_Function := 9;
   GPIO_AF_9_TIM12     : constant GPIO_Alternate_Function := 9;
   GPIO_AF_9_TIM13     : constant GPIO_Alternate_Function := 9;
   GPIO_AF_9_TIM14     : constant GPIO_Alternate_Function := 9;
   GPIO_AF_10_OTG_FS   : constant GPIO_Alternate_Function := 10;
   GPIO_AF_10_OTG_HS   : constant GPIO_Alternate_Function := 10;
   GPIO_AF_11_ETH      : constant GPIO_Alternate_Function := 11;
   GPIO_AF_12_FMC      : constant GPIO_Alternate_Function := 12;
   GPIO_AF_12_OTG_FS   : constant GPIO_Alternate_Function := 12;
   GPIO_AF_13_DCMI     : constant GPIO_Alternate_Function := 13;
   GPIO_AF_15_EVENTOUT : constant GPIO_Alternate_Function := 15;

end STM32.Device;
