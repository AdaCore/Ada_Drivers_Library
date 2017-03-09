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
--   @file    stm32f4_discovery.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief  This file provides set of firmware functions to manage Leds    --
--           and push-button available on STM32F42-Discovery Kit from       --
--           STMicroelectronics.                                            --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;
with STM32.Setup;
with HAL.SPI;
with LIS3DSH;       use LIS3DSH;

package body STM32.Board is

   ------------------
   -- All_LEDs_Off --
   ------------------

   procedure All_LEDs_Off is
   begin
      Clear (All_LEDs);
   end All_LEDs_Off;

   -----------------
   -- All_LEDs_On --
   -----------------

   procedure All_LEDs_On is
   begin
      Set (All_LEDs);
   end All_LEDs_On;

   ---------------------
   -- Initialize_LEDs --
   ---------------------

   procedure Initialize_LEDs is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (All_LEDs);

      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_100MHz;
      Configuration.Resistors   := Floating;
      Configure_IO (All_LEDs,
                    Config => Configuration);
   end Initialize_LEDs;

   ----------------------
   -- Initialize_Audio --
   ----------------------

   procedure Initialize_Audio is

      procedure Initialize_GPIO;

      ---------------------
      -- Initialize_GPIO --
      ---------------------

      procedure Initialize_GPIO is
      begin

         -- I2S --

         Enable_Clock (Audio_I2S_Points);

         Configure_IO (Audio_I2S_Points,
                       (Speed       => Speed_High,
                        Mode        => Mode_AF,
                        Output_Type => Push_Pull,
                        Resistors   => Floating));

         Configure_Alternate_Function (Audio_I2S_Points, GPIO_AF_I2S3_6);

         Lock (Audio_I2S_Points);

         -- DAC reset --

         Enable_Clock (DAC_Reset_Point);
         Configure_IO (DAC_Reset_Point,
                       (Speed       => Speed_High,
                        Mode        => Mode_Out,
                        Output_Type => Push_Pull,
                        Resistors   => Pull_Down));

         Lock (DAC_Reset_Point);
      end Initialize_GPIO;

      Conf : I2S_Configuration;

   begin
      Initialize_GPIO;

      STM32.Setup.Setup_I2C_Master (Port        => I2C_1,
                                    SDA         => Audio_I2C_SDA,
                                    SCL         => Audio_I2C_SCL,
                                    SDA_AF      => GPIO_AF_I2C1_4,
                                    SCL_AF      => GPIO_AF_I2C1_4,
                                    Clock_Speed => 100_000);

      Set_PLLI2S_Factors (Pll_N => 258,
                          Pll_R => 3);
      Enable_PLLI2S;

      Enable_Clock (Audio_I2S);

      Conf.Mode                     := Master_Transmit;
      Conf.Standard                 := I2S_Philips_Standard;
      Conf.Clock_Polarity           := Steady_State_Low;
      Conf.Data_Length              := Data_16bits;
      Conf.Chan_Length              := Channel_16bits;
      Conf.Master_Clock_Out_Enabled := True;
      Conf.Transmit_DMA_Enabled     := True;
      Conf.Receive_DMA_Enabled      := False;

      Audio_I2S.Configure (Conf);
      Audio_I2S.Set_Frequency (Audio_Freq_48kHz);
      Audio_I2S.Enable;


      DAC_Reset_Point.Clear;
      delay until Clock + Microseconds (200);
      DAC_Reset_Point.Set;
      delay until Clock + Microseconds (200);

      Audio_DAC.Init (Output    => CS43L22.Headphone,
                      Volume    => 0,
                      Frequency => Audio_Rate);

   end Initialize_Audio;

   --------------------------------
   -- Configure_User_Button_GPIO --
   --------------------------------

   procedure Configure_User_Button_GPIO is
      Config : GPIO_Port_Configuration;
   begin
      Enable_Clock (User_Button_Point);

      Config.Mode := Mode_In;
      Config.Resistors := Floating;

      Configure_IO (User_Button_Point, Config);
   end Configure_User_Button_GPIO;

   ------------------------------
   -- Initialize_Accelerometer --
   ------------------------------

   procedure Initialize_Accelerometer is
      procedure Init_SPI;
      procedure Init_GPIO;

      --------------
      -- Init_SPI --
      --------------

      procedure Init_SPI is
         Config : SPI_Configuration;
      begin
         Enable_Clock (Acc_SPI);

         Config.Mode := Master;
         Config.Baud_Rate_Prescaler := BRP_32;
         Config.Clock_Polarity := Low;
         Config.Clock_Phase := P1Edge;
         Config.First_Bit := MSB;
         Config.CRC_Poly := 7;
         Config.Slave_Management := Software_Managed;  --  essential!!
         Config.Direction := D2Lines_FullDuplex;
         Config.Data_Size := HAL.SPI.Data_Size_8b;

         Disable (Acc_SPI);
         Configure (Acc_SPI, Config);
         Enable (Acc_SPI);
      end Init_SPI;


      ---------------
      -- Init_GPIO --
      ---------------

      procedure Init_GPIO is
         Config : GPIO_Port_Configuration;
         SPI_Points : constant GPIO_Points := Acc_SPI_MOSI_Pin &
           Acc_SPI_MISO_Pin & Acc_SPI_SCK_Pin;
      begin
         Enable_Clock (SPI_Points);

         Config.Output_Type := Push_Pull;
         Config.Resistors   := Floating;
         Config.Speed       := Speed_50MHz;
         Config.Mode        := Mode_AF;

         Configure_IO (SPI_Points, Config);
         Configure_Alternate_Function (SPI_Points, Acc_SPI_AF);

         Enable_Clock (Acc_Chip_Select_Pin);

         Config.Mode        := Mode_Out;
         Config.Output_Type := Push_Pull;
         Config.Resistors   := Pull_Up;
         Config.Speed       := Speed_25MHz;

         Acc_Chip_Select_Pin.Configure_IO (Config);
         Acc_Chip_Select_Pin.Set;
      end Init_GPIO;

   begin
      Init_GPIO;
      Init_SPI;
   end Initialize_Accelerometer;


end STM32.Board;
