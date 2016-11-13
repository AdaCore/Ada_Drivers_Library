------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Real_Time;           use Ada.Real_Time;

with STM32.Device;            use STM32.Device;
with STM32.DMA;               use STM32.DMA;
with STM32.GPIO;              use STM32.GPIO;
with STM32.SDMMC;             use STM32.SDMMC;
with Cortex_M.Cache;

package body SDCard is

   SD_Detect_Pin     : STM32.GPIO.GPIO_Point renames PG2;

   SD_DMA            : DMA_Controller renames DMA_2;
   SD_DMA_Rx_Stream  : DMA_Stream_Selector renames Stream_3;
   SD_DMA_Rx_Channel : DMA_Channel_Selector renames Channel_4;
   SD_DMA_Tx_Stream  : DMA_Stream_Selector renames Stream_6;
   SD_DMA_Tx_Channel : DMA_Channel_Selector renames Channel_4;

   procedure Ensure_Card_Informations
     (Controller : in out SDCard_Controller) with Inline_Always;
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

      procedure Set_Transfer_State (Controller : SDCard_Controller);
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
     (Controller : in out SDCard_Controller)
   is
      SD_Pins : constant STM32.GPIO.GPIO_Points :=
                  (PC8, PC9, PC10, PC11, PC12, PD2);

   begin
      --  Enable the SDIO clock
      Enable_Clock (SDMMC_Controller (Controller.Device.all));
      Reset (SDMMC_Controller (Controller.Device.all));

      --  Enable the DMA2 clock
      Enable_Clock (SD_DMA);

      --  Enable the GPIOs
      Enable_Clock (SD_Pins & SD_Detect_Pin);

      --  GPIO configuration for the SDIO pins
      Configure_IO
        (SD_Pins,
         (Mode        => Mode_AF,
          Output_Type => Push_Pull,
          Speed       => Speed_High,
          Resistors   => Pull_Up));
      Configure_Alternate_Function (SD_Pins, GPIO_AF_12_SDIO);

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
   end Initialize;

   ------------------
   -- Card_Present --
   ------------------

   function Card_Present
     (Controller : in out SDCard_Controller) return Boolean
   is
   begin
      if STM32.GPIO.Set (SD_Detect_Pin) then
         --  No card
         Controller.Has_Info      := False;
         Controller.Card_Detected := False;
      else
         --  Card detected. Just wait a bit to unbounce the signal from the
         --  detect pin
         if not Controller.Card_Detected then
            delay until Clock + Milliseconds (50);
         end if;

         Controller.Card_Detected := not STM32.GPIO.Set (SD_Detect_Pin);
      end if;

      return Controller.Card_Detected;
   end Card_Present;

   ------------------------------
   -- Ensure_Card_Informations --
   ------------------------------

   procedure Ensure_Card_Informations
     (Controller : in out SDCard_Controller)
   is
      Ret : SD_Error;
   begin
      if Controller.Has_Info then
         return;
      end if;

      Ret := STM32.SDMMC.Initialize
        (Controller.Device.all, Controller.Info);

      if Ret = OK then
         Controller.Has_Info := True;
      else
         Controller.Has_Info := False;
      end if;
   end Ensure_Card_Informations;

   --------------------------
   -- Get_Card_information --
   --------------------------

   function Get_Card_Information
     (Controller : in out SDCard_Controller)
      return STM32.SDMMC.Card_Information
   is
   begin
      Ensure_Card_Informations (Controller);

      if not Controller.Has_Info then
         --  Issue reading the SD-card information
         Ensure_Card_Informations (Controller);
      end if;

      if not Controller.Has_Info then
         raise Device_Error;
      end if;

      return Controller.Info;
   end Get_Card_Information;

   ----------------
   -- Block_Size --
   ----------------

   function Block_Size
     (Controller : in out SDCard_Controller)
      return Unsigned_32
   is
   begin
      Ensure_Card_Informations (Controller);

      return Controller.Info.Card_Block_Size;
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

      procedure Set_Transfer_State (Controller : SDCard_Controller)
      is
      begin
         Finished  := False;
         Device    := Controller.Device.all'Unchecked_Access;
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
     (Controller   : in out SDCard_Controller;
      Block_Number : Unsigned_64;
      Data         : Block) return Boolean
   is
      Ret     : SD_Error;
      DMA_Err : DMA_Error_Code;
   begin
      Ensure_Card_Informations (Controller);

      --  Flush the data cache
      Cortex_M.Cache.Clean_DCache
        (Start => Data (Data'First)'Address,
         Len   => Data'Length);

      DMA_Interrupt_Handler.Set_Transfer_State;
      SDMMC_Interrupt_Handler.Set_Transfer_State (Controller);

      Clear_All_Status (SD_DMA, SD_DMA_Tx_Stream);
      Ret := Write_Blocks_DMA
        (Controller.Device.all,
         Block_Number * Unsigned_64 (Controller.Info.Card_Block_Size),
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
         exit when not Get_Flag (Controller.Device.all, TX_Active);
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
     (Controller   : in out SDCard_Controller;
      Block_Number : Unsigned_64;
      Data         : out Block) return Boolean
   is
      Ret     : Boolean;
      SD_Err  : SD_Error;
      DMA_Err : DMA_Error_Code;
   begin
      Ensure_Card_Informations (Controller);

      DMA_Interrupt_Handler.Set_Transfer_State;
      SDMMC_Interrupt_Handler.Set_Transfer_State (Controller);

      Cortex_M.Cache.Clean_DCache
        (Start => Data'Address,
         Len   => Data'Length);

      SD_Err := Read_Blocks_DMA
        (Controller.Device.all,
         Block_Number * Unsigned_64 (Controller.Info.Card_Block_Size),
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
            exit when not Get_Flag (Controller.Device.all, RX_Active);
         end loop;
      end if;

      Ret := SD_Err = OK and then DMA_Err = DMA_No_Error;

      if Last_Operation (Controller.Device.all) =
        Read_Multiple_Blocks_Operation
      then
         SD_Err := Stop_Transfer (Controller.Device.all);
         Ret := Ret and then SD_Err = OK;
      end if;

      Clear_All_Status (SD_DMA, SD_DMA_Rx_Stream);
      Disable (SD_DMA, SD_DMA_Rx_Stream);
      Disable_Data (Controller.Device.all);
      Clear_Static_Flags (Controller.Device.all);

      Cortex_M.Cache.Invalidate_DCache
        (Start => Data'Address,
         Len   => Data'Length);

      return Ret;
   end Read;

end SDCard;
