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

with STM32.Device;   use STM32.Device;

with STM32.GPIO;     use STM32.GPIO;
with STM32.I2C;      use STM32.I2C;
with STM32.SPI;      use STM32.SPI;
with STM32.Timers;   use STM32.Timers;
with STM32.USARTs;   use STM32.USARTs;

with MPU9250;        use MPU9250;
with AK8963;         use AK8963;

with Ravenscar_Time;
package STM32.Board is
   pragma Elaborate_Body;

   subtype User_LED is GPIO_Point;

   LED_Blue_L  : User_LED renames PD2;

   LED_Green_L : User_LED renames PC1;
   LED_Green_R : User_LED renames PC2;
   LED_Red_L   : User_LED renames PC0;
   LED_Red_R   : User_LED renames PC3;

   RG_LEDs  : GPIO_Points :=
                LED_Green_L & LED_Green_R & LED_Red_L & LED_Red_R;
   All_LEDs : GPIO_Points :=
                RG_LEDs & LED_Blue_L;
   LCH_LED  : GPIO_Point renames LED_Blue_L;

   procedure Initialize_LEDs;
   --  MUST be called prior to any use of the LEDs

   procedure Turn_On (This : in out User_LED);
   procedure Turn_Off (This : in out User_LED);
   function Is_On (This : User_LED) return Boolean;


   procedure All_LEDs_Off with Inline;
   procedure All_LEDs_On  with Inline;
   procedure Toggle_LED (This : in out User_LED)
     renames STM32.GPIO.Toggle;
   procedure Toggle_LEDs (These : in out GPIO_Points)
     renames STM32.GPIO.Toggle;

   ---------
   -- PWM --
   ---------

   MOTOR_123_Timer : Timer renames Timer_2;
   MOTOR_4_Timer   : Timer renames Timer_4;
   MOTOR_1         : GPIO_Point renames PA1;
   MOTOR_1_AF      : GPIO_Alternate_Function renames GPIO_AF_TIM2_1;
   MOTOR_1_Channel : Timer_Channel renames Channel_2;
   MOTOR_2         : GPIO_Point renames PB11;
   MOTOR_2_AF      : GPIO_Alternate_Function renames GPIO_AF_TIM2_1;
   MOTOR_2_Channel : Timer_Channel renames Channel_4;
   MOTOR_3         : GPIO_Point renames PA15;
   MOTOR_3_AF      : GPIO_Alternate_Function renames GPIO_AF_TIM2_1;
   MOTOR_3_Channel : Timer_Channel renames Channel_1;
   MOTOR_4         : GPIO_Point renames PB9;
   MOTOR_4_AF      : GPIO_Alternate_Function renames GPIO_AF_TIM4_2;
   MOTOR_4_Channel : Timer_Channel renames Channel_4;

   ---------
   -- I2C --
   ---------

   procedure Initialize_I2C_GPIO (Port : in out I2C_Port)
     with
     --  I2C_2 is not accessible on this board
     Pre => As_Port_Id (Port) = I2C_Id_1
            or else As_Port_Id (Port) = I2C_Id_3;

   procedure Configure_I2C (Port : in out I2C_Port);

   I2C_MPU_Port : I2C_Port renames I2C_3;
   I2C_MPU_SCL  : GPIO_Point renames PA8;
   I2C_MPU_SDA  : GPIO_Point renames PC9;

   I2C_EXT_Port : I2C_Port renames I2C_1;
   I2C_EXT_SCL  : GPIO_Point renames PB6;
   I2C_EXT_SDA  : GPIO_Point renames PB7;

   ---------
   -- SPI --
   ---------

   EXT_SPI  : SPI_Port renames SPI_1;
   EXT_SCK  : GPIO_Point renames PA5;
   EXT_MISO : GPIO_Point renames PA6;
   EXT_MOSI : GPIO_Point renames PA7;
   EXT_CS0  : GPIO_Point renames PC12;
   EXT_CS1  : GPIO_Point renames PB4;
   EXT_CS2  : GPIO_Point renames PB5;
   EXT_CS3  : GPIO_Point renames PB8;

   ---------
   -- USB --
   ---------

   USB_ID : GPIO_Point renames PA10;
   USB_DM : GPIO_Point renames PA11;
   USB_DP : GPIO_Point renames PA12;

   --------------------------
   -- External connections --
   --------------------------

   EXT_USART1    : USART renames USART_3;
   EXT_USART1_AF : GPIO_Alternate_Function renames GPIO_AF_USART3_7;
   EXT_USART1_TX : GPIO_Point renames PC10;
   EXT_USART1_RX : GPIO_Point renames PC11;
   EXT_USART2    : USART renames USART_2;
   EXT_USART2_AF : GPIO_Alternate_Function renames GPIO_AF_USART2_7;
   EXT_USART2_TX : GPIO_Point renames PA2;
   EXT_USART2_RX : GPIO_Point renames PA3;

   -----------
   -- Radio --
   -----------

   NRF_USART     : USART renames USART_6;
   NRF_USART_AF  : GPIO_Alternate_Function renames GPIO_AF_USART6_8;
   NRF_RX        : GPIO_Point renames PC6;
   NRF_TX        : GPIO_Point renames PC7;
   NRF_SWCLK     : GPIO_Point renames PB13;
   NRF_SWIO      : GPIO_Point renames PB15;
   NRF_FLOW_CTRL : GPIO_Point renames PA4;

   ---------
   -- MPU --
   ---------

   MPU_Device   : MPU9250.MPU9250_Device (I2C_MPU_Port'Access,
                                          High,
                                          Ravenscar_Time.Delays);
   MPU_INT      : GPIO_Point renames PC13;
   MPU_FSYNC    : GPIO_Point renames PC14;

   MAG_Device   : AK8963_Device (I2C_MPU_Port'Access,
                                 Add_00,
                                 Ravenscar_Time.Delays);

end STM32.Board;
