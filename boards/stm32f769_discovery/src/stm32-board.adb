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
------------------------------------------------------------------------------

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

   -------------------------
   -- Initialize_I2C_GPIO --
   -------------------------

   procedure Initialize_I2C_GPIO (Port : in out I2C_Port)
   is
      Id     : constant I2C_Port_Id := As_Port_Id (Port);
      Points : GPIO_Points (1 .. 2);

   begin
      if Port_Enabled (Port) then
         return;
      end if;

      case Id is
         when I2C_Id_1 =>
            Points := (I2C1_SDA, I2C1_SCL);
         when I2C_Id_4 =>
            Points := (I2C4_SDA, I2C4_SCL);
         when others =>
            raise Unknown_Device with
              "This I2C_Port cannot be used on this board";
      end case;

      Enable_Clock (Points);

      if Id = I2C_Id_1 then
         Configure_Alternate_Function (Points, GPIO_AF_4_I2C1);
      else
         Configure_Alternate_Function (I2C4_SCL, GPIO_AF_4_I2C4);
         Configure_Alternate_Function (I2C4_SDA, GPIO_AF_11_I2C4);
      end if;

      Configure_IO (Points,
                    (Speed       => Speed_50MHz,
                     Mode        => Mode_AF,
                     Output_Type => Open_Drain,
                     Resistors   => Floating));
      Lock (Points);
   end Initialize_I2C_GPIO;

   -------------------
   -- TP_I2C_Config --
   -------------------

   procedure Configure_I2C (Port : in out I2C_Port)
   is
      I2C_Conf : I2C_Configuration;
   begin
      if not STM32.I2C.Is_Configured (Port) then
         I2C_Conf.Own_Address := 16#00#;
         I2C_Conf.Addressing_Mode := Addressing_Mode_7bit;
         I2C_Conf.General_Call_Enabled := False;
         I2C_Conf.Clock_Stretching_Enabled := True;

         I2C_Conf.Clock_Speed := 100_000;

         Configure (Port, I2C_Conf);
      end if;
   end Configure_I2C;

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
   -- Configure_SD_Device_GPIO --
   ------------------------------

   procedure Configure_SD_Device_GPIO
   is
      SD_Pins_AF_10 : constant STM32.GPIO.GPIO_Points :=
                        (PB3, PB4);
      SD_Pins_AF_11 : constant STM32.GPIO.GPIO_Points :=
                        (PD6, PD7, PG9, PG10);
      SD_DMA_Rx_Channel : constant DMA_Channel_Selector :=
                            Channel_11;
      SD_DMA_Tx_Channel : constant DMA_Channel_Selector :=
                            Channel_11;

   begin
      --  Enable the SDIO clock
      Enable_Clock (SD_Device);
      Reset (SD_Device);

      --  Enable the DMA2 clock
      Enable_Clock (SD_DMA);

      --  Enable the GPIOs
      Enable_Clock (SD_Pins_AF_10 & SD_Pins_AF_11 & SD_Detect_Pin);

      --  GPIO configuration for the SDIO pins
      Configure_IO
        (SD_Pins_AF_10 & SD_Pins_AF_11,
         (Mode        => Mode_AF,
          Output_Type => Push_Pull,
          Speed       => Speed_High,
          Resistors   => Pull_Up));
      --  ??? no GPIO_AF10_SDMMC or AF11 names for now. Waiting for AF
      --  functions to be moved outside of STM32.GPIO (that is common to
      --  all STM32)
      Configure_Alternate_Function (SD_Pins_AF_10, GPIO_AF_10_SDMMC2);
      Configure_Alternate_Function (SD_Pins_AF_11, GPIO_AF_11_SDMMC2);

      --  GPIO configuration for the SD-Detect pin
      Configure_IO
        (SD_Detect_Pin,
         (Mode        => Mode_In,
          Output_Type => Open_Drain,
          Speed       => Speed_High,
          Resistors   => Pull_Up));

      Disable (SD_DMA, SD_DMA_Rx_Stream);
      Configure
        (SD_DMA,
         SD_DMA_Rx_Stream,
         (Channel                      => SD_DMA_Rx_Channel,
          Direction                    => Peripheral_To_Memory,
          Increment_Peripheral_Address => False,
          Increment_Memory_Address     => True,
          Peripheral_Data_Format       => Words,
          Memory_Data_Format           => Words,
          Operation_Mode               => Peripheral_Flow_Control_Mode,
          Priority                     => Priority_Very_High,
          FIFO_Enabled                 => True,
          FIFO_Threshold               => FIFO_Threshold_Full_Configuration,
          Memory_Burst_Size            => Memory_Burst_Inc4,
          Peripheral_Burst_Size        => Peripheral_Burst_Inc4));
      Clear_All_Status (SD_DMA, SD_DMA_Rx_Stream);

      Disable (SD_DMA, SD_DMA_Tx_Stream);
      Configure
        (SD_DMA,
         SD_DMA_Tx_Stream,
         (Channel                      => SD_DMA_Tx_Channel,
          Direction                    => Memory_To_Peripheral,
          Increment_Peripheral_Address => False,
          Increment_Memory_Address     => True,
          Peripheral_Data_Format       => Words,
          Memory_Data_Format           => Words,
          Operation_Mode               => Peripheral_Flow_Control_Mode,
          Priority                     => Priority_Very_High,
          FIFO_Enabled                 => True,
          FIFO_Threshold               => FIFO_Threshold_Full_Configuration,
          Memory_Burst_Size            => Memory_Burst_Inc4,
          Peripheral_Burst_Size        => Peripheral_Burst_Inc4));
      Clear_All_Status (SD_DMA, SD_DMA_Tx_Stream);
   end Configure_SD_Device_GPIO;

end STM32.Board;
