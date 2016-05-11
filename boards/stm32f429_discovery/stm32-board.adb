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

with HAL.SPI;

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
      Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (All_LEDs);

      Conf.Mode        := Mode_Out;
      Conf.Output_Type := Push_Pull;
      Conf.Speed       := Speed_100MHz;
      Conf.Resistors   := Floating;

      Configure_IO (All_LEDs, Conf);
   end Initialize_LEDs;

   ------------------------
   -- Initialize_Gyro_IO --
   ------------------------

   procedure Initialize_Gyro_IO is
      --  See the STM32F429 Discovery kit User Manual (UM1670) pages 21 and 23.
   begin
      Enable_Clock (Gyro_SPI);
      Enable_Clock (NCS_MEMS_SPI & SPI5_SCK & SPI5_MISO & SPI5_MOSI);

      Init_Chip_Select : declare
         Config : GPIO_Port_Configuration;
      begin
         Config.Speed := Speed_25MHz;
         Config.Mode := Mode_Out;
         Config.Output_Type := Push_Pull;
         Config.Resistors := Pull_Up;

         Configure_IO (NCS_MEMS_SPI, Config);
      end Init_Chip_Select;

      Init_SPI_IO_Pins : declare
         Config : GPIO_Port_Configuration;
      begin
         Config.Speed       := Speed_100MHz;
         Config.Mode        := Mode_AF;
         Config.Output_Type := Push_Pull;
         Config.Resistors   := Floating;

         Configure_IO (SPI5_SCK & SPI5_MISO & SPI5_MOSI, Config);

         Configure_Alternate_Function (SPI5_SCK & SPI5_MISO & SPI5_MOSI, GPIO_AF_SPI5);
      end Init_SPI_IO_Pins;

      Init_SPI_Port : declare
         Config : SPI_Configuration;
      begin
         Config :=
           (Direction           => D2Lines_FullDuplex,
            Mode                => Master,
            Data_Size           => HAL.SPI.Data_Size_8b,
            Clock_Polarity      => Low,
            Clock_Phase         => P1Edge,
            Slave_Management    => Software_Managed,
            Baud_Rate_Prescaler => BRP_32,
            First_Bit           => MSB,
            CRC_Poly            => 7);

         Configure (Gyro_SPI, Config);

         STM32.SPI.Enable (Gyro_SPI);
      end Init_SPI_Port;

      Gyro.Initialize
        (Port        => Gyro_SPI'Access,
         Chip_Select => NCS_MEMS_SPI'Access);

      if Gyro.Device_Id /= L3GD20.I_Am_L3GD20 then
         raise Program_Error with "No L3GD20 found";
      end if;
   end Initialize_Gyro_IO;

   --------------------------------
   -- Configure_User_Button_GPIO --
   --------------------------------

   procedure Configure_User_Button_GPIO is
      Config : GPIO_Port_Configuration;
   begin
      Enable_Clock (User_Button_Point);

      Config.Mode := Mode_In;
      Config.Resistors := Floating;

      User_Button_Point.Configure_IO (Config);
   end Configure_User_Button_GPIO;

end STM32.Board;
