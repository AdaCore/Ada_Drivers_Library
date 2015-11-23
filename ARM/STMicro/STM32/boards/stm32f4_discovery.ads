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
--   @file    stm32f4_discovery.h                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   This file contains definitions for STM32F4-Discovery Kit      --
--            LEDs, push-buttons hardware resources.                        --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides declarations for devices on the STM32F4 Discovery kits
--  manufactured by ST Microelectronics.

with STM32F40xxx;

with STM32F4.GPIO;    use STM32F4.GPIO;
with STM32F4.ADC;     use STM32F4.ADC;
with STM32F4.DMA;     use STM32F4.DMA;
with STM32F4.USARTs;  use STM32F4.USARTs;
with STM32F4.I2C;     use STM32F4.I2C;
with STM32F4.SPI;     use STM32F4.SPI;
with STM32F4.Timers;  use STM32F4.Timers;
with STM32F4.DAC;     use STM32F4.DAC;

with STM32F4.LIS3DSH;   use STM32F4.LIS3DSH;

with Ada.Interrupts.Names; use Ada.Interrupts;

package STM32F4_Discovery is
   pragma Elaborate_Body;

   subtype User_LED is GPIO_Pin;

   Green  : User_LED renames Pin_12;
   Orange : User_LED renames Pin_13;
   Red    : User_LED renames Pin_14;
   Blue   : User_LED renames Pin_15;

   LED3 : User_LED renames Orange;
   LED4 : User_LED renames Green;
   LED5 : User_LED renames Red;
   LED6 : User_LED renames Blue;

   All_LEDs : constant GPIO_Pins := LED3 & LED4 & LED5 & LED6;

   procedure Initialize_LEDs;
   --  MUST be called prior to any use of the LEDs

   procedure Turn_On (This : User_LED) with Inline;
   procedure Turn_Off (This : User_LED) with Inline;
   procedure Toggle (This : User_LED) with Inline;

   procedure All_LEDs_Off with Inline;
   procedure All_LEDs_On  with Inline;
   procedure Toggle_LEDs (These : GPIO_Pins) with Inline;

   LED_Port : GPIO_Port renames STM32F40xxx.GPIO_D;
   --  Available for clients requiring a reference. Note that Initialize_LEDs
   --  will configure the GPIO port/pins for LED usage, specifically.

   Accelerometer : Three_Axis_Accelerometer;

   GPIO_A : GPIO_Port renames STM32F40xxx.GPIO_A;
   GPIO_B : GPIO_Port renames STM32F40xxx.GPIO_B;
   GPIO_C : GPIO_Port renames STM32F40xxx.GPIO_C;
   GPIO_D : GPIO_Port renames STM32F40xxx.GPIO_D;
   GPIO_E : GPIO_Port renames STM32F40xxx.GPIO_E;
   GPIO_F : GPIO_Port renames STM32F40xxx.GPIO_F;
   GPIO_G : GPIO_Port renames STM32F40xxx.GPIO_G;
   GPIO_H : GPIO_Port renames STM32F40xxx.GPIO_H;
   GPIO_I : GPIO_Port renames STM32F40xxx.GPIO_I;

   procedure Enable_Clock (This : aliased in out GPIO_Port)
     renames STM32F40xxx.Enable_Clock;

   procedure Reset (This : aliased in out GPIO_Port)
     renames STM32F40xxx.Reset;

   User_Button_Port      : GPIO_Port renames GPIO_A;
   User_Button_Pin       : constant GPIO_Pin := Pin_0;
   User_Button_Interrupt : constant Interrupt_Id := Names.EXTI0_Interrupt;

   procedure Configure_User_Button_GPIO;
   --  Configures the GPIO port/pin for the blue user button. Sufficient
   --  for polling the button, and necessary for having the button generate
   --  interrupts.

   ADC_1 : Analog_To_Digital_Converter renames STM32F40xxx.ADC_1;
   ADC_2 : Analog_To_Digital_Converter renames STM32F40xxx.ADC_2;
   ADC_3 : Analog_To_Digital_Converter renames STM32F40xxx.ADC_3;

   VBat               : ADC_Point renames STM32F40xxx.VBat;
   Temperature_Sensor : ADC_Point renames STM32F40xxx.Temperature_Sensor;

   VBat_Bridge_Divisor : constant := STM32F40xxx.VBat_Bridge_Divisor;

   procedure Enable_Clock (This : aliased in out Analog_To_Digital_Converter)
     renames STM32F40xxx.Enable_Clock;

   procedure Reset_All_ADC_Units
     renames STM32F40xxx.Reset_All_ADC_Units;

   DAC_1 : Digital_To_Analog_Converter renames STM32F40xxx.DAC_1;

   DAC_Channel_1_IO : GPIO_Point renames STM32F40xxx.DAC_Channel_1_IO;
   DAC_Channel_2_IO : GPIO_Point renames STM32F40xxx.DAC_Channel_2_IO;

   procedure Enable_Clock (This : aliased in out Digital_To_Analog_Converter)
     renames STM32F40xxx.Enable_Clock;

   procedure Reset (This : aliased in out Digital_To_Analog_Converter)
     renames STM32F40xxx.Reset;

   USART_1 : USART renames STM32F40xxx.USART_1;
   USART_2 : USART renames STM32F40xxx.USART_2;
   USART_3 : USART renames STM32F40xxx.USART_3;
   USART_6 : USART renames STM32F40xxx.USART_6;

   procedure Enable_Clock (This : aliased in out USART)
     renames STM32F40xxx.Enable_Clock;

   procedure Reset (This : aliased in out USART)
     renames STM32F40xxx.Reset;

   DMA_1 : DMA_Controller renames STM32F40xxx.DMA_1;
   DMA_2 : DMA_Controller renames STM32F40xxx.DMA_2;

   procedure Enable_Clock (This : aliased in out DMA_Controller)
     renames STM32F40xxx.Enable_Clock;

   procedure Reset (This : aliased in out DMA_Controller)
     renames STM32F40xxx.Reset;

   I2C_1 : I2C_Port renames STM32F40xxx.I2C_1;
   I2C_2 : I2C_Port renames STM32F40xxx.I2C_2;
   I2C_3 : I2C_Port renames STM32F40xxx.I2C_3;

   procedure Enable_Clock (This : aliased in out I2C_Port)
     renames STM32F40xxx.Enable_Clock;

   procedure Reset (This : in out I2C_Port)
     renames STM32F40xxx.Reset;

   SPI_1 : SPI_Port renames STM32F40xxx.SPI_1;
   SPI_2 : SPI_Port renames STM32F40xxx.SPI_2;
   SPI_3 : SPI_Port renames STM32F40xxx.SPI_3;

   procedure Enable_Clock (This : aliased in out SPI_Port)
     renames STM32F40xxx.Enable_Clock;

   procedure Reset (This : in out SPI_Port)
     renames STM32F40xxx.Reset;

   Timer_1  : Timer renames STM32F40xxx.Timer_1;
   Timer_2  : Timer renames STM32F40xxx.Timer_2;
   Timer_3  : Timer renames STM32F40xxx.Timer_3;
   Timer_4  : Timer renames STM32F40xxx.Timer_4;
   Timer_5  : Timer renames STM32F40xxx.Timer_5;
   Timer_6  : Timer renames STM32F40xxx.Timer_6;
   Timer_7  : Timer renames STM32F40xxx.Timer_7;
   Timer_8  : Timer renames STM32F40xxx.Timer_8;
   Timer_9  : Timer renames STM32F40xxx.Timer_9;
   Timer_10 : Timer renames STM32F40xxx.Timer_10;
   Timer_11 : Timer renames STM32F40xxx.Timer_11;
   Timer_12 : Timer renames STM32F40xxx.Timer_12;
   Timer_13 : Timer renames STM32F40xxx.Timer_13;
   Timer_14 : Timer renames STM32F40xxx.Timer_14;

   procedure Enable_Clock (This : in out Timer)
     renames STM32F40xxx.Enable_Clock;

   procedure Reset (This : in out Timer)
     renames STM32F40xxx.Reset;

end STM32F4_Discovery;
