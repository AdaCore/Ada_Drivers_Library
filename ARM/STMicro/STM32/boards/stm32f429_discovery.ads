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
--   @file    stm32f429i_discovery.h                                        --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   This file contains definitions for STM32F429I-Discovery Kit   --
--            LEDs, push-buttons hardware resources.                        --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides declarations for devices on the STM32F429 Discovery kits
--  manufactured by ST Microelectronics.

with STM32F42xxx;

with STM32F4.GPIO;    use STM32F4.GPIO;
with STM32F4.DMA;     use STM32F4.DMA;
with STM32F4.USARTs;  use STM32F4.USARTs;
with STM32F4.I2C;     use STM32F4.I2C;
with STM32F4.SPI;     use STM32F4.SPI;
with STM32F4.Timers;  use STM32F4.Timers;
with STM32F4.L3DG20;  use STM32F4.L3DG20;

with Ada.Interrupts.Names;  use Ada.Interrupts;

use STM32F4;  -- for base addresses

package STM32F429_Discovery is
   pragma Elaborate_Body;

   subtype User_LED is GPIO_Pin;

   Green : User_LED renames Pin_13;
   Red   : User_LED renames Pin_14;

   LED3  : User_LED renames Green;
   LED4  : User_LED renames Red;

   All_LEDs  : constant GPIO_Pins := LED3 & LED4;

   procedure Initialize_LEDs;
   --  MUST be called prior to any use of the LEDs

   procedure Turn_On (This : User_LED) with Inline;
   procedure Turn_Off (This : User_LED) with Inline;
   procedure Toggle (This : User_LED) with Inline;

   procedure Toggle_LEDs (These : GPIO_Pins) with Inline;
   procedure All_LEDs_Off with Inline;
   procedure All_LEDs_On  with Inline;

   LED_Port : GPIO_Port renames STM32F42xxx.GPIO_G;
   --  Available for clients requiring a reference. Note that Initialize_LEDs
   --  will configure the GPIO port/pins for LED usage, specifically.

   Gyro : Three_Axis_Gyroscope;

   GPIO_A : GPIO_Port renames STM32F42xxx.GPIO_A;
   GPIO_B : GPIO_Port renames STM32F42xxx.GPIO_B;
   GPIO_C : GPIO_Port renames STM32F42xxx.GPIO_C;
   GPIO_D : GPIO_Port renames STM32F42xxx.GPIO_D;
   GPIO_E : GPIO_Port renames STM32F42xxx.GPIO_E;
   GPIO_F : GPIO_Port renames STM32F42xxx.GPIO_F;
   GPIO_G : GPIO_Port renames STM32F42xxx.GPIO_G;
   GPIO_H : GPIO_Port renames STM32F42xxx.GPIO_H;
   GPIO_I : GPIO_Port renames STM32F42xxx.GPIO_I;
   GPIO_J : GPIO_Port renames STM32F42xxx.GPIO_J;
   GPIO_K : GPIO_Port renames STM32F42xxx.GPIO_K;

   procedure Enable_Clock (This : aliased in out GPIO_Port)
      renames STM32F42xxx.Enable_Clock;

   User_Button_Port      : GPIO_Port renames GPIO_A;
   User_Button_Pin       : constant GPIO_Pin := Pin_0;
   User_Button_Interrupt : constant Interrupt_Id := Names.EXTI0_Interrupt;

   procedure Configure_User_Button_GPIO;
   --  Configures the GPIO port/pin for the user button. Sufficient for polling
   --  the button, and necessary for having the button generate interrupts.

   --  Note that some of these are really UARTs, not USARTs
   USART_1 : USART renames STM32F42xxx.USART_1;
   USART_2 : USART renames STM32F42xxx.USART_2;
   USART_3 : USART renames STM32F42xxx.USART_3;
   UART_4  : USART renames STM32F42xxx.UART_4;
   UART_5  : USART renames STM32F42xxx.UART_5;
   USART_6 : USART renames STM32F42xxx.USART_6;
   UART_7  : USART renames STM32F42xxx.UART_7;
   UART_8  : USART renames STM32F42xxx.UART_8;

   procedure Enable_Clock (This : aliased in out USART)
      renames STM32F42xxx.Enable_Clock;

   DMA_1 : DMA_Controller renames STM32F42xxx.DMA_1;
   DMA_2 : DMA_Controller renames STM32F42xxx.DMA_2;

   procedure Enable_Clock (This : aliased in out DMA_Controller)
      renames STM32F42xxx.Enable_Clock;

   I2C_1 : I2C_Port renames STM32F42xxx.I2C_1;
   I2C_2 : I2C_Port renames STM32F42xxx.I2C_2;
   I2C_3 : I2C_Port renames STM32F42xxx.I2C_3;

   procedure Enable_Clock (This : aliased in out I2C_Port)
      renames STM32F42xxx.Enable_Clock;

   procedure Reset (This : in out I2C_Port)
      renames STM32F42xxx.Reset;

   SPI_1 : SPI_Port renames STM32F42xxx.SPI_1;
   SPI_2 : SPI_Port renames STM32F42xxx.SPI_2;
   SPI_3 : SPI_Port renames STM32F42xxx.SPI_3;
   SPI_4 : SPI_Port renames STM32F42xxx.SPI_4;
   SPI_5 : SPI_Port renames STM32F42xxx.SPI_5;
   SPI_6 : SPI_Port renames STM32F42xxx.SPI_6;

   procedure Enable_Clock (This : aliased in out SPI_Port)
      renames STM32F42xxx.Enable_Clock;

   procedure Reset (This : in out SPI_Port)
      renames STM32F42xxx.Reset;

   Timer_1  : Timer renames STM32F42xxx.Timer_1;
   Timer_2  : Timer renames STM32F42xxx.Timer_2;
   Timer_3  : Timer renames STM32F42xxx.Timer_3;
   Timer_4  : Timer renames STM32F42xxx.Timer_4;
   Timer_5  : Timer renames STM32F42xxx.Timer_5;
   Timer_6  : Timer renames STM32F42xxx.Timer_6;
   Timer_7  : Timer renames STM32F42xxx.Timer_7;
   Timer_8  : Timer renames STM32F42xxx.Timer_8;
   Timer_9  : Timer renames STM32F42xxx.Timer_9;
   Timer_10 : Timer renames STM32F42xxx.Timer_10;
   Timer_11 : Timer renames STM32F42xxx.Timer_11;
   Timer_12 : Timer renames STM32F42xxx.Timer_12;
   Timer_13 : Timer renames STM32F42xxx.Timer_13;
   Timer_14 : Timer renames STM32F42xxx.Timer_14;

   procedure Enable_Clock (This : in out Timer)
      renames STM32F42xxx.Enable_Clock;

   procedure Reset (This : in out Timer)
      renames STM32F42xxx.Reset;

end STM32F429_Discovery;
