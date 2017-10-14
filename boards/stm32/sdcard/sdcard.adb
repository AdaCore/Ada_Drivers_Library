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

with STM32.Board;             use STM32.Board;
with STM32.Device;            use STM32.Device;
with STM32.DMA;               use STM32.DMA;
with STM32.GPIO;              use STM32.GPIO;
with STM32.SDMMC;             use STM32.SDMMC;
with Cortex_M.Cache;

package body SDCard is

   procedure Ensure_Card_Informations (This : in out SDCard_Controller)
     with Inline_Always;
   --  Make sure the sdcard information is read and stored in the Controller
   --  structure

   ------------
   -- DMA_Rx --
   ------------

   protected DMA_Interrupt_Handler is
      pragma Interrupt_Priority (255);

      procedure Set_Transfer_State;
      --  Informes the DMA Int handler that a transfer is about to start

      procedure Clear_Transfer_State;

      function Buffer_Error return Boolean;

      entry Wait_Transfer (Status : out DMA_Error_Code);

   private

      procedure Interrupt_RX;
      pragma Attach_Handler (Interrupt_RX, SD_Rx_IRQ);

      procedure Interrupt_TX;
      pragma Attach_Handler (Interrupt_TX, SD_Tx_IRQ);

      Finished   : Boolean := True;
      DMA_Status : DMA_Error_Code := DMA_No_Error;
      Had_Buffer_Error : Boolean := False;
   end DMA_Interrupt_Handler;

   ------------------
   -- SDMMC_Status --
   ------------------

   protected SDMMC_Interrupt_Handler is
      pragma Interrupt_Priority (250);

      procedure Set_Transfer_State (This : SDCard_Controller);
      procedure Clear_Transfer_State;
      entry Wait_Transfer (Status : out SD_Error);

   private
      procedure Interrupt;
      pragma Attach_Handler (Interrupt, SD_Interrupt);

      Finished  : Boolean := True;
      SD_Status : SD_Error;
      Device    : access SDMMC_Controller;
   end SDMMC_Interrupt_Handler;

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
         This.Has_Info      := False;
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

   ------------------------------
   -- Ensure_Card_Informations --
   ------------------------------

   procedure Ensure_Card_Informations
     (This : in out SDCard_Controller)
   is
      Ret : SD_Error;
   begin
      if This.Has_Info then
         return;
      end if;

      Ret := STM32.SDMMC.Initialize
        (This.Device.all,
         This.Info);

      if Ret = OK then
         This.Has_Info := True;
      else
         This.Has_Info := False;
      end if;
   end Ensure_Card_Informations;

   --------------------------
   -- Get_Card_information --
   --------------------------

   function Get_Card_Information
     (This : in out SDCard_Controller)
      return Card_Information
   is
   begin
      Ensure_Card_Informations (This);

      if not This.Has_Info then
         raise Device_Error;
      end if;

      return This.Info;
   end Get_Card_Information;

   ----------------
   -- Block_Size --
   ----------------

   function Block_Size
     (This : in out SDCard_Controller)
      return UInt32
   is
   begin
      Ensure_Card_Informations (This);

      return This.Info.Card_Block_Size;
   end Block_Size;

   ---------------------------
   -- DMA_Interrupt_Handler --
   ---------------------------

   protected body DMA_Interrupt_Handler
   is

      function Buffer_Error return Boolean is (Had_Buffer_Error);

      -------------------
      -- Wait_Transfer --
      -------------------

      entry Wait_Transfer (Status : out DMA_Error_Code) when Finished is
      begin
         Status := DMA_Status;
      end Wait_Transfer;

      ------------------------
      -- Set_Transfer_State --
      ------------------------

      procedure Set_Transfer_State
      is
      begin
         Finished := False;
         DMA_Status := DMA_No_Error;
         Had_Buffer_Error := False;
      end Set_Transfer_State;

      --------------------------
      -- Clear_Transfer_State --
      --------------------------

      procedure Clear_Transfer_State
      is
      begin
         Finished := True;
         DMA_Status := DMA_Transfer_Error;
      end Clear_Transfer_State;

      ---------------
      -- Interrupt --
      ---------------

      procedure Interrupt_RX is
      begin

         if Status (SD_DMA, SD_DMA_Rx_Stream, Transfer_Complete_Indicated) then
            Disable_Interrupt
              (SD_DMA, SD_DMA_Rx_Stream, Transfer_Complete_Interrupt);
            Clear_Status
              (SD_DMA, SD_DMA_Rx_Stream, Transfer_Complete_Indicated);

            DMA_Status := DMA_No_Error;
            Finished := True;
         end if;

         if Status (SD_DMA, SD_DMA_Rx_Stream, FIFO_Error_Indicated) then
            Disable_Interrupt (SD_DMA, SD_DMA_Rx_Stream, FIFO_Error_Interrupt);
            Clear_Status (SD_DMA, SD_DMA_Rx_Stream, FIFO_Error_Indicated);

            --  see Interrupt_TX
            Had_Buffer_Error := True;
         end if;

         if Status (SD_DMA, SD_DMA_Rx_Stream, Transfer_Error_Indicated) then
            Disable_Interrupt
              (SD_DMA, SD_DMA_Rx_Stream, Transfer_Error_Interrupt);
            Clear_Status (SD_DMA, SD_DMA_Rx_Stream, Transfer_Error_Indicated);

            DMA_Status := DMA_Transfer_Error;
            Finished := True;
         end if;

         if Finished then
            for Int in STM32.DMA.DMA_Interrupt loop
               Disable_Interrupt (SD_DMA, SD_DMA_Rx_Stream, Int);
            end loop;
         end if;
      end Interrupt_RX;

      ------------------
      -- Interrupt_TX --
      ------------------

      procedure Interrupt_TX is
      begin

         if Status (SD_DMA, SD_DMA_Tx_Stream, Transfer_Complete_Indicated) then
            Disable_Interrupt
              (SD_DMA, SD_DMA_Tx_Stream, Transfer_Complete_Interrupt);
            Clear_Status
              (SD_DMA, SD_DMA_Tx_Stream, Transfer_Complete_Indicated);

            DMA_Status := DMA_No_Error;
            Finished := True;
         end if;

         if Status (SD_DMA, SD_DMA_Tx_Stream, FIFO_Error_Indicated) then
            --  this signal can be ignored when transfer is completed
            --  however, it comes before Transfer_Complete_Indicated and
            --  We cannot use the value of the NDT register either, because
            --  it's a race condition (the register lacks behind).
            --  As a result, we have to ignore it.
            Disable_Interrupt (SD_DMA, SD_DMA_Tx_Stream, FIFO_Error_Interrupt);
            Clear_Status (SD_DMA, SD_DMA_Tx_Stream, FIFO_Error_Indicated);
            Had_Buffer_Error := True;

         end if;

         if Status (SD_DMA, SD_DMA_Tx_Stream, Transfer_Error_Indicated) then
            Disable_Interrupt
              (SD_DMA, SD_DMA_Tx_Stream, Transfer_Error_Interrupt);
            Clear_Status (SD_DMA, SD_DMA_Tx_Stream, Transfer_Error_Indicated);

            DMA_Status := DMA_Transfer_Error;
            Finished := True;
         end if;

         if Finished then
            for Int in STM32.DMA.DMA_Interrupt loop
               Disable_Interrupt (SD_DMA, SD_DMA_Tx_Stream, Int);
            end loop;
         end if;
      end Interrupt_TX;

   end DMA_Interrupt_Handler;

   -----------------------------
   -- SDMMC_Interrupt_Handler --
   -----------------------------

   protected body SDMMC_Interrupt_Handler
   is

      -------------------
      -- Wait_Transfer --
      -------------------

      entry Wait_Transfer (Status : out SD_Error) when Finished is
      begin
         Status := SD_Status;
      end Wait_Transfer;

      ----------------------
      -- Set_Transferring --
      ----------------------

      procedure Set_Transfer_State (This : SDCard_Controller)
      is
      begin
         Finished  := False;
         Device    := This.Device.all'Unchecked_Access;
      end Set_Transfer_State;

      --------------------------
      -- Clear_Transfer_State --
      --------------------------

      procedure Clear_Transfer_State
      is
      begin
         Finished := True;
         SD_Status := Error;
      end Clear_Transfer_State;

      ---------------
      -- Interrupt --
      ---------------

      procedure Interrupt
      is
      begin
         Finished := True;

         if Get_Flag (Device.all, Data_End) then
            Clear_Flag (Device.all, Data_End);
            SD_Status := OK;

         elsif Get_Flag (Device.all, Data_CRC_Fail) then
            Clear_Flag (Device.all, Data_CRC_Fail);
            SD_Status := CRC_Check_Fail;

         elsif Get_Flag (Device.all, Data_Timeout) then
            Clear_Flag (Device.all, Data_Timeout);
            SD_Status := Timeout_Error;

         elsif Get_Flag (Device.all, RX_Overrun) then
            Clear_Flag (Device.all, RX_Overrun);
            SD_Status := Rx_Overrun;

         elsif Get_Flag (Device.all, TX_Underrun) then
            Clear_Flag (Device.all, TX_Underrun);
            SD_Status := Tx_Underrun;
         end if;

         for Int in SDMMC_Interrupts loop
            Disable_Interrupt (Device.all, Int);
         end loop;
      end Interrupt;

   end SDMMC_Interrupt_Handler;

   -----------
   -- Write --
   -----------

   overriding function Write
     (This         : in out SDCard_Controller;
      Block_Number : UInt64;
      Data         : Block) return Boolean
   is
      Ret     : SD_Error;
      DMA_Err : DMA_Error_Code;
   begin
      Ensure_Card_Informations (This);

      --  Flush the data cache
      Cortex_M.Cache.Clean_DCache
        (Start => Data (Data'First)'Address,
         Len   => Data'Length);

      DMA_Interrupt_Handler.Set_Transfer_State;
      SDMMC_Interrupt_Handler.Set_Transfer_State (This);

      Clear_All_Status (SD_DMA, SD_DMA_Tx_Stream);
      Ret := Write_Blocks_DMA
        (This.Device.all,
         Block_Number * UInt64 (This.Info.Card_Block_Size),
         SD_DMA,
         SD_DMA_Tx_Stream,
         SD_Data (Data));
      --  this always leaves the last 12 byte standing. Why?
      --  also...NDTR is not what it should be.

      if Ret /= OK then
         DMA_Interrupt_Handler.Clear_Transfer_State;
         SDMMC_Interrupt_Handler.Clear_Transfer_State;
         Abort_Transfer (SD_DMA, SD_DMA_Tx_Stream, DMA_Err);

         return False;
      end if;

      DMA_Interrupt_Handler.Wait_Transfer (DMA_Err); -- this unblocks
      SDMMC_Interrupt_Handler.Wait_Transfer (Ret); -- TX underrun!

      --  this seems slow. Do we have to wait?
      loop
         --  FIXME: some people claim, that this goes wrong with multiblock, see
         --  http://blog.frankvh.com/2011/09/04/stm32f2xx-sdio-sd-card-interface/
         exit when not Get_Flag (This.Device.all, TX_Active);
      end loop;

      Clear_All_Status (SD_DMA, SD_DMA_Tx_Stream);
      Disable (SD_DMA, SD_DMA_Tx_Stream);

      declare
         Data_Incomplete : constant Boolean :=
                             DMA_Interrupt_Handler.Buffer_Error and then
                                 Items_Transferred (SD_DMA, SD_DMA_Tx_Stream)
                                 /= Data'Length / 4;
      begin
         return Ret = OK
           and then DMA_Err = DMA_No_Error
           and then not Data_Incomplete;
      end;
   end Write;

   ----------
   -- Read --
   ----------

   overriding function Read
     (This         : in out SDCard_Controller;
      Block_Number : UInt64;
      Data         : out Block) return Boolean
   is
      Ret     : Boolean;
      SD_Err  : SD_Error;
      DMA_Err : DMA_Error_Code;
   begin
      Ensure_Card_Informations (This);

      DMA_Interrupt_Handler.Set_Transfer_State;
      SDMMC_Interrupt_Handler.Set_Transfer_State (This);

      SD_Err := Read_Blocks_DMA
        (This.Device.all,
         Block_Number * UInt64 (This.Info.Card_Block_Size),
         SD_DMA,
         SD_DMA_Rx_Stream,
         SD_Data (Data));

      if SD_Err /= OK then
         DMA_Interrupt_Handler.Clear_Transfer_State;
         SDMMC_Interrupt_Handler.Clear_Transfer_State;
         Abort_Transfer (SD_DMA, SD_DMA_Rx_Stream, DMA_Err);

         return False;
      end if;

      SDMMC_Interrupt_Handler.Wait_Transfer (SD_Err);

      if SD_Err /= OK then
         DMA_Interrupt_Handler.Clear_Transfer_State;
      else
         DMA_Interrupt_Handler.Wait_Transfer (DMA_Err);

         loop
            exit when not Get_Flag (This.Device.all, RX_Active);
         end loop;
      end if;

      Ret := SD_Err = OK and then DMA_Err = DMA_No_Error;

      if Last_Operation (This.Device.all) =
        Read_Multiple_Blocks_Operation
      then
         SD_Err := Stop_Transfer (This.Device.all);
         Ret := Ret and then SD_Err = OK;
      end if;

      Clear_All_Status (SD_DMA, SD_DMA_Rx_Stream);
      Disable (SD_DMA, SD_DMA_Rx_Stream);
      Disable_Data (This.Device.all);
      Clear_Static_Flags (This.Device.all);

      Cortex_M.Cache.Invalidate_DCache
        (Start => Data'Address,
         Len   => Data'Length);

      return Ret;
   end Read;

end SDCard;
