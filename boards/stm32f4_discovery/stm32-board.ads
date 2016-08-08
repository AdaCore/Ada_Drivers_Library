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

with LIS3DSH.SPI;   use LIS3DSH.SPI;

with Ada.Interrupts.Names; use Ada.Interrupts;
with STM32.SPI; use STM32.SPI;

package STM32.Board is
   pragma Elaborate_Body;

   subtype User_LED is GPIO_Point;

   Green  : User_LED renames PD12;
   Orange : User_LED renames PD13;
   Red    : User_LED renames PD14;
   Blue   : User_LED renames PD15;

   All_LEDs : GPIO_Points := Green & Orange & Red & Blue;
   LCH_LED  : GPIO_Point renames Red;

   procedure Initialize_LEDs;
   --  MUST be called prior to any use of the LEDs

   procedure Turn_On (This : in out User_LED)
     renames STM32.GPIO.Set;
   procedure Turn_Off (This : in out User_LED)
     renames STM32.GPIO.Clear;

   procedure All_LEDs_Off with Inline;
   procedure All_LEDs_On  with Inline;
   procedure Toggle_LEDs (These : in out GPIO_Points)
     renames STM32.GPIO.Toggle;


   Acc_SPI    : SPI_Port renames SPI_1;
   Acc_SPI_AF : GPIO_Alternate_Function renames GPIO_AF_SPI1;

   Acc_Chip_Select_Pin : GPIO_Point renames PE3;
   Acc_SPI_SCK_Pin     : GPIO_Point renames PA5;
   Acc_SPI_MISO_Pin    : GPIO_Point renames PA6;
   Acc_SPI_MOSI_Pin    : GPIO_Point renames PA7;


   Accelerometer : Three_Axis_Accelerometer_SPI
     (Port        => Acc_SPI'Access,
      Chip_Select => Acc_Chip_Select_Pin'Access);

   procedure Initialize_Accelerometer;

   --  GPIO Pins for FMC
   FMC_A : constant GPIO_Points :=
             (PF0, PF1, PF2, PF3, PF4, PF5,
              PF12, PF13, PF14, PF15, PG0, PG1);

   FMC_D : constant GPIO_Points :=
             (PD14, PD15, PD0, PD1, PE7, PE8, PE9, PE10, PE11, PE12,
              PE13, PE14, PE15, PD8, PD9, PD10);

   FMC_SDNWE     : GPIO_Point renames PH5;
   FMC_SDNRAS    : GPIO_Point renames PF11;
   FMC_SDNCAS    : GPIO_Point renames PG15;
   FMC_SDCLK     : GPIO_Point renames PG8;
   FMC_BA0       : GPIO_Point renames PG4;
   FMC_BA1       : GPIO_Point renames PG5;
   FMC_SDNE0     : GPIO_Point renames PH3;
   FMC_SDCKE0    : GPIO_Point renames PC3;
   FMC_NBL0      : GPIO_Point renames PE0;
   FMC_NBL1      : GPIO_Point renames PE1;

   SDRAM_PINS    : constant GPIO_Points :=
                     GPIO_Points'
                       (FMC_SDNWE,
                        FMC_SDNRAS,
                        FMC_SDNCAS,
                        FMC_SDCLK,
                        FMC_BA0,
                        FMC_BA1,
                        FMC_SDNE0,
                        FMC_SDCKE0,
                        FMC_NBL0,
                        FMC_NBL1) &
                     FMC_A & FMC_D;

   --  User button

   User_Button_Point     : GPIO_Point renames PA0;
   User_Button_Interrupt : constant Interrupt_ID := Names.EXTI0_Interrupt;

   procedure Configure_User_Button_GPIO;
   --  Configures the GPIO port/pin for the blue user button. Sufficient
   --  for polling the button, and necessary for having the button generate
   --  interrupts.

end STM32.Board;
