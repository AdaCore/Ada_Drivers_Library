------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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
--     3. Neither the name of the copyright holder nor the names of its     --
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

with Ada.Real_Time;           use Ada.Real_Time;
with HAL.SDMMC;               use HAL.SDMMC;
with STM32_SVD.RCC;           use STM32_SVD.RCC;

with STM32.Board;             use STM32.Board;
with STM32.Device;            use STM32.Device;
with STM32.DMA;               use STM32.DMA;
with STM32.GPIO;              use STM32.GPIO;
with STM32.SDMMC;             use STM32.SDMMC;
with STM32.SDMMC_Interrupt;   use STM32.SDMMC_Interrupt;

package body SDCard is

   SD_Interrupt_Handler : aliased SDMMC_Interrupt_Handler (SD_Interrupt);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : in out SDCard_Controller)
   is
   begin
      Set_Clock_Source (This.Device.all, Src_Sysclk);

      --  Enable the GPIOs
      Enable_Clock (SD_Pins & SD_Pins_2 & SD_Detect_Pin);

      --  GPIO configuration for the SDIO pins
      Configure_IO
        (SD_Pins & SD_Pins_2,
         (Mode        => Mode_AF,
          Output_Type => Push_Pull,
          Speed       => Speed_High,
          Resistors   => Pull_Up));
      Configure_Alternate_Function (SD_Pins, SD_Pins_AF);
      Configure_Alternate_Function (SD_Pins_2, SD_Pins_AF_2);

      --  GPIO configuration for the SD-Detect pin
      Configure_IO
        (SD_Detect_Pin,
         (Mode        => Mode_In,
          Output_Type => Open_Drain,
          Speed       => Speed_High,
          Resistors   => Pull_Up));

      --  Enable the SDIO clock
      STM32.Device.Enable_Clock (This.Device.all);
      STM32.Device.Reset (This.Device.all);

      --  Enable the DMA2 clock
      Enable_Clock (SD_DMA);

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

      This.Device.Enable_DMA_Transfers (RX_Int => SD_Rx_DMA_Int'Access,
                                        TX_Int => SD_Tx_DMA_Int'Access,
                                        SD_Int => SD_Interrupt_Handler'Access);
   end Initialize;

   ------------------
   -- Card_Present --
   ------------------

   function Card_Present
     (This : in out SDCard_Controller) return Boolean
   is
   begin
      if STM32.GPIO.Set (SD_Detect_Pin) then
         --  No card
         This.Device.Clear_Card_Information;
         This.Card_Detected := False;
      else
         --  Card detected. Just wait a bit to unbounce the signal from the
         --  detect pin
         if not This.Card_Detected then
            delay until Clock + Milliseconds (50);
         end if;

         This.Card_Detected := not STM32.GPIO.Set (SD_Detect_Pin);
      end if;

      return This.Card_Detected;
   end Card_Present;

   --------------------------
   -- Get_Card_information --
   --------------------------

   function Get_Card_Information
     (This : in out SDCard_Controller)
      return HAL.SDMMC.Card_Information
   is
   begin
      This.Device.Ensure_Card_Informations;

      if not This.Device.Has_Card_Information then
         raise Device_Error;
      end if;

      return This.Device.Card_Information;
   end Get_Card_Information;

   ----------------
   -- Block_Size --
   ----------------

   function Block_Size
     (This : in out SDCard_Controller)
      return UInt32
   is
   begin
      return This.Get_Card_Information.Card_Block_Size;
   end Block_Size;

   -----------
   -- Write --
   -----------

   overriding function Write
     (This         : in out SDCard_Controller;
      Block_Number : UInt64;
      Data         : Block) return Boolean
   is
   begin
      return This.Device.Write (Block_Number, Data);
   end Write;

   ----------
   -- Read --
   ----------

   overriding function Read
     (This         : in out SDCard_Controller;
      Block_Number : UInt64;
      Data         : out Block) return Boolean
   is
   begin
      return This.Device.Read (Block_Number, Data);
   end Read;

end SDCard;
