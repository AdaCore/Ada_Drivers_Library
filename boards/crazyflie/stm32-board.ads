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

with STM32.Device;  use STM32.Device;

with STM32.GPIO;    use STM32.GPIO;
with STM32.I2C;     use STM32.I2C;

with MPU9250;       use MPU9250;

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

   procedure Turn_On (This : User_LED);
   procedure Turn_Off (This : User_LED);
   function Is_On (This : User_LED) return Boolean;


   procedure All_LEDs_Off with Inline;
   procedure All_LEDs_On  with Inline;
   procedure Toggle_LED (This : User_LED)
     renames STM32.GPIO.Toggle;
   procedure Toggle_LEDs (These : GPIO_Points)
     renames STM32.GPIO.Toggle;

   ---------
   -- I2C --
   ---------

   procedure Initialize_I2C_GPIO (Port : in out I2C_Port)
     with
     --  I2C_2 is not accessible on this board
     Pre => As_Port_Id (Port) = I2C_Id_1
            or else As_Port_Id (Port) = I2C_Id_3;

   procedure Configure_I2C (Port : in out I2C_Port);

   MPU_I2C_Port : I2C_Port renames I2C_3;
   EXT_I2C_Port : I2C_Port renames I2C_1;

   ---------
   -- MPU --
   ---------

   MPU_Device   : MPU9250.MPU9250_Device (MPU_I2C_Port'Access, High);

end STM32.Board;
