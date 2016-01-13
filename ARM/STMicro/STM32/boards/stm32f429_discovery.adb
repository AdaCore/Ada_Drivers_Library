------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2016, AdaCore                        --
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
--   @file    stm32f429i_discovery.c                                        --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief  This file provides set of firmware functions to manage Leds    --
--           and push-button available on STM32F429I-Discovery Kit from     --
--           STMicroelectronics.                                            --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with STM32F4.RCC;  use STM32F4.RCC;
with STM32F4.ILI9341;

package body STM32F429_Discovery is

   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (This : User_LED) is
   begin
      Set (LED_Port, This);
   end Turn_On;

   --------------
   -- Turn_Off --
   --------------

   procedure Turn_Off (This : User_LED) is
   begin
      Clear (LED_Port, This);
   end Turn_Off;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (This : User_LED) is
   begin
      Toggle (LED_Port, This);
   end Toggle;

   -----------------
   -- Toggle_LEDs --
   -----------------

   procedure Toggle_LEDs (These : GPIO_Pins) is
   begin
      Toggle (LED_Port, These);
   end Toggle_LEDs;

   ------------------
   -- All_LEDs_Off --
   ------------------

   procedure All_LEDs_Off is
   begin
      Clear (LED_Port, ALL_LEDs);
   end All_LEDs_Off;

   -----------------
   -- All_LEDs_On --
   -----------------

   procedure All_LEDs_On is
   begin
      Set (LED_Port, ALL_LEDs);
   end All_LEDs_On;

   ---------------------
   -- Initialize_LEDs --
   ---------------------

   procedure Initialize_LEDs is
      Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (LED_Port);

      Conf.Mode        := Mode_Out;
      Conf.Output_Type := Push_Pull;
      Conf.Speed       := Speed_100MHz;
      Conf.Resistors   := Floating;

      Configure_IO (LED_Port, All_LEDs, Conf);
   end Initialize_LEDs;

   --------------------------------
   -- Configure_User_Button_GPIO --
   --------------------------------

   procedure Configure_User_Button_GPIO is
      Config : GPIO_Port_Configuration;
   begin
      Enable_Clock (User_Button_Port);

      Config.Mode := Mode_In;
      Config.Resistors := Floating;

      Configure_IO (User_Button_Port, User_Button_Pin, Config);
   end Configure_User_Button_GPIO;

   ------------------------------
   -- Initialize_Gyro_Hardware --
   ------------------------------

   procedure Initialize_Gyro_Hardware is
   begin
      -- For the page numbers shown below, the required values are specified in
      -- the STM32F429 Discovery kit User Manual (UM1670) on those pages.
      L3DG20.Initialize_Gyro_Hardware
        (Gyro,
         L3GD20_SPI                  => SPI_5'Access,
         SPI_GPIO                    => GPIO_F'Access,  -- required, pg 23
         SPI_GPIO_AF                 => GPIO_AF_SPI5,
         SCK_Pin                     => Pin_7,          -- required, pg 23
         MISO_Pin                    => Pin_8,          -- required, pg 23
         MOSI_Pin                    => Pin_9,          -- required, pg 23
         CS_GPIO                     => GPIO_C'Access,  -- required, pg 21
         CS_Pin                      => Pin_1,          -- required, pg 21
         Enable_SPI_Clock            => RCC.SPI5_Clock_Enable'Access,
         Enable_SPI_GPIO_Clock       => RCC.GPIOF_Clock_Enable'Access,
         Enable_Chip_Select_Clock    => RCC.GPIOC_Clock_Enable'Access);

      if L3DG20.Device_Id (Gyro) /= L3DG20.I_Am_L3GD20 then
         raise Program_Error with "No L3DG20 found";
      end if;
   end Initialize_Gyro_Hardware;

   -----------------------------
   -- Initialize_LCD_Hardware --
   -----------------------------

   procedure Initialize_LCD_Hardware is
      --  On the STM32F429 Discovrey board, the ILI9341 LCD is always
      --  initialized with the following:
   begin
      STM32F4.ILI9341.Initialize
        (Chip_Select             => (GPIO_C'Access, Pin_2),
         Enable_CS_GPIO_Clock    => GPIOC_Clock_Enable'Access,
         WRX                     => (GPIO_D'Access, Pin_13),
         Enable_WRX_GPIO_Clock   => GPIOD_Clock_Enable'Access,
         Reset                   => (GPIO_D'Access, Pin_11),
         Enable_Reset_GPIO_Clock => GPIOD_Clock_Enable'Access,
         SPI_Chip                => SPI_5'Access,
         Enable_SPI_Clock        => SPI5_Clock_Enable'Access,
         SPI_GPIO                => GPIO_F'Access,
         Enable_SPI_GPIO_Clock   => GPIOF_Clock_Enable'Access,
         SPI_AF                  => GPIO_AF_SPI5,
         SCK_Pin                 => Pin_7,
         MISO_Pin                => Pin_8,
         MOSI_Pin                => Pin_9);
   end Initialize_LCD_Hardware;

end STM32F429_Discovery;
