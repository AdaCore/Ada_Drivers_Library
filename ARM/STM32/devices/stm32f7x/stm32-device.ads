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

pragma Warnings (Off, "* is an internal GNAT unit");
private with System.BB.Parameters;
pragma Warnings (On, "* is an internal GNAT unit");

with STM32_SVD;     use STM32_SVD;

with STM32_SVD.DMA;
with STM32_SVD.SAI;

with STM32.GPIO;    use STM32.GPIO;
with STM32.ADC;     use STM32.ADC;
--  with STM32.USARTs;  use STM32.USARTs;
with STM32.I2C;     use STM32.I2C;
with STM32.SPI;     use STM32.SPI;
with STM32.Timers;  use STM32.Timers;
with STM32.DAC;     use STM32.DAC;

package STM32.Device is
   pragma Elaborate_Body;

   Unknown_Device : exception;
   --  Raised by the routines below for a device passed as an actual parameter
   --  when that device is not present on the given hardware instance.

   HSI_VALUE      : constant := 16_000_000;
   --  Internal oscillator in Hz

   HSE_VALUE      : constant Word;
   --  External oscillator in Hz

   procedure Enable_Clock (This : aliased in out Internal_GPIO_Port)
     with Inline;
   procedure Enable_Clock (Point : GPIO_Point)
     with Inline;
   procedure Enable_Clock (Points : GPIO_Points)
     with Inline;

   procedure Reset (This : aliased in out Internal_GPIO_Port)
     with Inline;
   procedure Reset (Point : GPIO_Point)
     with Inline;
   procedure Reset (Points : GPIO_Points)
     with Inline;

   type GPIO_Port_Id is
     (GPIO_Port_A, GPIO_Port_B, GPIO_Port_C, GPIO_Port_D, GPIO_Port_E,
      GPIO_Port_F, GPIO_Port_G, GPIO_Port_H, GPIO_Port_I, GPIO_Port_J,
      GPIO_Port_K)
     with Size => 4;

   function As_GPIO_Port_Id (Port : Internal_GPIO_Port) return GPIO_Port_Id
     with Inline;

   GPIO_A : aliased Internal_GPIO_Port
     with Import, Volatile, Address => GPIOA_Base;
   GPIO_B : aliased Internal_GPIO_Port
     with Import, Volatile, Address => GPIOB_Base;
   GPIO_C : aliased Internal_GPIO_Port
     with Import, Volatile, Address => GPIOC_Base;
   GPIO_D : aliased Internal_GPIO_Port
     with Import, Volatile, Address => GPIOD_Base;
   GPIO_E : aliased Internal_GPIO_Port
     with Import, Volatile, Address => GPIOE_Base;
   GPIO_F : aliased Internal_GPIO_Port
     with Import, Volatile, Address => GPIOF_Base;
   GPIO_G : aliased Internal_GPIO_Port
     with Import, Volatile, Address => GPIOG_Base;
   GPIO_H : aliased Internal_GPIO_Port
     with Import, Volatile, Address => GPIOH_Base;
   GPIO_I : aliased Internal_GPIO_Port
     with Import, Volatile, Address => GPIOI_Base;
   GPIO_J : aliased Internal_GPIO_Port
     with Import, Volatile, Address => GPIOJ_Base;
   GPIO_K : aliased Internal_GPIO_Port
     with Import, Volatile, Address => GPIOK_Base;

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
   PJ0  : aliased GPIO_Point := (GPIO_J'Access, 0);
   PJ1  : aliased GPIO_Point := (GPIO_J'Access, 1);
   PJ2  : aliased GPIO_Point := (GPIO_J'Access, 2);
   PJ3  : aliased GPIO_Point := (GPIO_J'Access, 3);
   PJ4  : aliased GPIO_Point := (GPIO_J'Access, 4);
   PJ5  : aliased GPIO_Point := (GPIO_J'Access, 5);
   PJ6  : aliased GPIO_Point := (GPIO_J'Access, 6);
   PJ7  : aliased GPIO_Point := (GPIO_J'Access, 7);
   PJ8  : aliased GPIO_Point := (GPIO_J'Access, 8);
   PJ9  : aliased GPIO_Point := (GPIO_J'Access, 9);
   PJ10 : aliased GPIO_Point := (GPIO_J'Access, 10);
   PJ11 : aliased GPIO_Point := (GPIO_J'Access, 11);
   PJ12 : aliased GPIO_Point := (GPIO_J'Access, 12);
   PJ13 : aliased GPIO_Point := (GPIO_J'Access, 13);
   PJ14 : aliased GPIO_Point := (GPIO_J'Access, 14);
   PJ15 : aliased GPIO_Point := (GPIO_J'Access, 15);
   PK0  : aliased GPIO_Point := (GPIO_K'Access, 0);
   PK1  : aliased GPIO_Point := (GPIO_K'Access, 1);
   PK2  : aliased GPIO_Point := (GPIO_K'Access, 2);
   PK3  : aliased GPIO_Point := (GPIO_K'Access, 3);
   PK4  : aliased GPIO_Point := (GPIO_K'Access, 4);
   PK5  : aliased GPIO_Point := (GPIO_K'Access, 5);
   PK6  : aliased GPIO_Point := (GPIO_K'Access, 6);
   PK7  : aliased GPIO_Point := (GPIO_K'Access, 7);
   PK8  : aliased GPIO_Point := (GPIO_K'Access, 8);
   PK9  : aliased GPIO_Point := (GPIO_K'Access, 9);
   PK10 : aliased GPIO_Point := (GPIO_K'Access, 10);
   PK11 : aliased GPIO_Point := (GPIO_K'Access, 11);
   PK12 : aliased GPIO_Point := (GPIO_K'Access, 12);
   PK13 : aliased GPIO_Point := (GPIO_K'Access, 13);
   PK14 : aliased GPIO_Point := (GPIO_K'Access, 14);
   PK15 : aliased GPIO_Point := (GPIO_K'Access, 15);

   ADC_1 : aliased Analog_To_Digital_Converter
     with Import, Volatile, Address => ADC1_Base;
   ADC_2 : aliased Analog_To_Digital_Converter
     with Import, Volatile, Address => ADC2_Base;
   ADC_3 : aliased Analog_To_Digital_Converter
     with Import, Volatile, Address => ADC3_Base;

   VBat               : constant ADC_Point :=
     (ADC_1'Access, Channel => VBat_Channel);
   Temperature_Sensor : constant ADC_Point := VBat;
   --  see RM pg 410, section 13.10, also pg 389

   VBat_Bridge_Divisor : constant := 4;
   --  The VBAT pin is internally connected to a bridge divider. The actual
   --  voltage is the raw conversion value * the divisor. See section 13.11,
   --  pg 412 of the RM.

   procedure Enable_Clock (This : aliased in out Analog_To_Digital_Converter);

   procedure Reset_All_ADC_Units;

   DAC_1 : aliased Digital_To_Analog_Converter
     with Import, Volatile, Address => DAC_Base;

   --  ??? Taken from the STM32F429 definition, TO BE CHECKED FOR THE F7
   DAC_Channel_1_IO : constant GPIO_Point := PA4;
   DAC_Channel_2_IO : constant GPIO_Point := PA5;

   procedure Enable_Clock (This : aliased in out Digital_To_Analog_Converter);
   procedure Reset (This : aliased in out Digital_To_Analog_Converter);

--     USART_1 : aliased USART with Import, Volatile, Address => USART1_Base;
--     USART_2 : aliased USART with Import, Volatile, Address => USART2_Base;
--     USART_3 : aliased USART with Import, Volatile, Address => USART3_Base;
--     UART_4  : aliased USART with Import, Volatile, Address => UART4_Base;
--     UART_5  : aliased USART with Import, Volatile, Address => UART5_Base;
--     USART_6 : aliased USART with Import, Volatile, Address => USART6_Base;
--     USART_7 : aliased USART with Import, Volatile, Address => UART7_Base;
--     USART_8 : aliased USART with Import, Volatile, Address => UART8_Base;
--
--     procedure Enable_Clock (This : aliased in out USART);
--
--     procedure Reset (This : aliased in out USART);

   subtype DMA_Controller is STM32_SVD.DMA.DMA_Peripheral;

   DMA_1 : DMA_Controller renames STM32_SVD.DMA.DMA1_Periph;
   DMA_2 : DMA_Controller renames STM32_SVD.DMA.DMA2_Periph;

   procedure Enable_Clock (This : aliased in out DMA_Controller);
   procedure Reset (This : aliased in out DMA_Controller);

   Internal_I2C_Port_1 : aliased Internal_I2C_Port
     with Import, Volatile, Address => I2C1_Base;
   Internal_I2C_Port_2 : aliased Internal_I2C_Port
     with Import, Volatile, Address => I2C2_Base;
   Internal_I2C_Port_3 : aliased Internal_I2C_Port
     with Import, Volatile, Address => I2C3_Base;
   Internal_I2C_Port_4 : aliased Internal_I2C_Port
     with Import, Volatile, Address => I2C3_Base;

   I2C_1 : aliased I2C_Port (Internal_I2C_Port_1'Access);
   I2C_2 : aliased I2C_Port (Internal_I2C_Port_2'Access);
   I2C_3 : aliased I2C_Port (Internal_I2C_Port_3'Access);
   I2C_4 : aliased I2C_Port (Internal_I2C_Port_4'Access);

   type I2C_Port_Id is (I2C_Id_1, I2C_Id_2, I2C_Id_3, I2C_Id_4);

   function As_Port_Id (Port : I2C_Port) return I2C_Port_Id with Inline;

   Internal_SPI_1 : aliased Internal_SPI_Port
     with Import, Volatile, Address => SPI1_Base;
   Internal_SPI_2 : aliased Internal_SPI_Port
     with Import, Volatile, Address => SPI2_Base;
   Internal_SPI_3 : aliased Internal_SPI_Port
     with Import, Volatile, Address => SPI3_Base;
   Internal_SPI_4 : aliased Internal_SPI_Port
     with Import, Volatile, Address => SPI4_Base;
   Internal_SPI_5 : aliased Internal_SPI_Port
     with Import, Volatile, Address => SPI5_Base;
   Internal_SPI_6 : aliased Internal_SPI_Port
     with Import, Volatile, Address => SPI6_Base;

   SPI_1 : aliased SPI_Port (Internal_SPI_1'Access);
   SPI_2 : aliased SPI_Port (Internal_SPI_2'Access);
   SPI_3 : aliased SPI_Port (Internal_SPI_3'Access);
   SPI_4 : aliased SPI_Port (Internal_SPI_4'Access);
   SPI_5 : aliased SPI_Port (Internal_SPI_5'Access);
   SPI_6 : aliased SPI_Port (Internal_SPI_6'Access);

   procedure Enable_Clock (This : SPI_Port);
   procedure Reset (This : SPI_Port);

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

   subtype SAI_Port is STM32_SVD.SAI.SAI_Peripheral;

   SAI_1 : SAI_Port renames STM32_SVD.SAI.SAI1_Periph;
   SAI_2 : SAI_Port renames STM32_SVD.SAI.SAI2_Periph;

   procedure Enable_Clock (This : in out SAI_Port);
   procedure Reset (This : in out SAI_Port);

   -----------------------------
   -- Reset and Clock Control --
   -----------------------------

   type RCC_System_Clocks is record
      SYSCLK  : Word;
      HCLK    : Word;
      PCLK1   : Word;
      PCLK2   : Word;
      TIMCLK1 : Word;
      TIMCLK2 : Word;
   end record;

   function System_Clock_Frequencies return RCC_System_Clocks;

   type PLLSAI_DivR is new UInt2;
   PLLSAI_DIV2  : constant PLLSAI_DivR := 0;
   PLLSAI_DIV4  : constant PLLSAI_DivR := 1;
   PLLSAI_DIV8  : constant PLLSAI_DivR := 2;
   PLLSAI_DIV16 : constant PLLSAI_DivR := 3;

   procedure Set_PLLSAI_Factors
     (LCD  : UInt3;
      VCO  : UInt9;
      DivR : PLLSAI_DivR);

   procedure Enable_PLLSAI;
   procedure Disable_PLLSAI;
   function PLLSAI_Ready return Boolean;

   subtype DIVQ is Natural range 1 .. 32;

   procedure Configure_SAI_I2S_Clock
     (Periph     : SAI_Port;
      PLLI2SN    : UInt9;
      PLLI2SQ    : UInt4;
      PLLI2SDIVQ : DIVQ);

   procedure Enable_DCMI_Clock;
   procedure Reset_DCMI;

private

   HSE_VALUE : constant Word := System.BB.Parameters.HSE_Clock;

   pragma Compile_Time_Error
     (not (GPIO_Port_Id'First = GPIO_Port_A and
           GPIO_Port_Id'Last  = GPIO_Port_K and
           GPIO_Port_A'Enum_Rep = 0 and
           GPIO_Port_B'Enum_Rep = 1 and
           GPIO_Port_C'Enum_Rep = 2 and
           GPIO_Port_D'Enum_Rep = 3 and
           GPIO_Port_E'Enum_Rep = 4 and
           GPIO_Port_F'Enum_Rep = 5 and
           GPIO_Port_G'Enum_Rep = 6 and
           GPIO_Port_H'Enum_Rep = 7 and
           GPIO_Port_I'Enum_Rep = 8 and
           GPIO_Port_J'Enum_Rep = 9 and
           GPIO_Port_K'Enum_Rep = 10),
      "Invalid representation for type GPIO_Port_Id");
   --  Confirming, but depended upon so we check it.

end STM32.Device;
