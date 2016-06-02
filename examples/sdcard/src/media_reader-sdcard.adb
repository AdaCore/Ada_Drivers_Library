with Ada.Interrupts;
with Ada.Interrupts.Names;

with STM32.Device;         use STM32.Device;
with STM32.DMA;            use STM32.DMA;
with STM32.GPIO;           use STM32.GPIO;
with STM32.SDMMC;          use STM32.SDMMC;
with Cortex_M.Cache;

with STM32_SVD.RCC;
with STM32_SVD.SDMMC;

package body Media_Reader.SDCard is

   SD_Pins       : constant STM32.GPIO.GPIO_Points :=
                     (PC8, PC9, PC10, PC11, PC12, PD2);
   SD_Detect_Pin : constant STM32.GPIO.GPIO_Point :=
                     PC13;

   SD_DMA            : DMA_Controller renames DMA_2;
   SD_DMA_Rx_Channel : constant DMA_Channel_Selector :=
                         Channel_4;
   SD_DMA_Rx_Stream  : constant DMA_Stream_Selector :=
                         Stream_3;
   Rx_IRQ            : Ada.Interrupts.Interrupt_ID renames
                         Ada.Interrupts.Names.DMA2_Stream3_Interrupt;
--     SD_DMA_Tx_Channel : constant DMA_Channel_Selector :=
--                           Channel_4;
--     SD_DMA_Tx_Stream  : constant DMA_Stream_Selector :=
--                           Stream_6;
--     Tx_IRQ            : constant Interrupt_ID :=
--                           Ada.Interrupts.Names.DMA2_Stream6_Interrupt;

   procedure Ensure_Card_Informations
     (Controller : in out SDCard_Controller) with Inline_Always;

   ------------
   -- DMA_Rx --
   ------------

   protected DMA_Interrupt_Handler is
      pragma Interrupt_Priority;

      procedure Set_Transfer_State;

      procedure Clear_Transfer_State;

      entry Wait_Transfer (Status : out DMA_Error_Code);

   private

      procedure Interrupt
        with Attach_Handler => Rx_IRQ, Unreferenced;

      Finished   : Boolean := True;
      DMA_Status : DMA_Error_Code;
   end DMA_Interrupt_Handler;

   ------------------
   -- SDMMC_Status --
   ------------------

   protected SDMMC_Interrupt_Handler is

      procedure Set_Transfer_State (Controller : SDCard_Controller);
      procedure Clear_Transfer_State;
      entry Wait_Transfer (Status : out SD_Error);

   private
      procedure Interrupt
        with Attach_Handler => Ada.Interrupts.Names.SDMMC1_Interrupt,
             Unreferenced;
      Finished  : Boolean := True;
      SD_Status : SD_Error;
      Device    : SDMMC_Controller;
   end SDMMC_Interrupt_Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Controller : in out SDCard_Controller)
   is
   begin
      --  Enable the SDIO clock
      STM32_SVD.RCC.RCC_Periph.APB2ENR.SDMMC1EN := True;
      STM32_SVD.RCC.RCC_Periph.APB2RSTR.SDMMC1RST := True;
      STM32_SVD.RCC.RCC_Periph.APB2RSTR.SDMMC1RST := False;

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
      Configure_Alternate_Function (SD_Pins, GPIO_AF_SDIO);

      --  GPIO configuration for the SD-Detect pin
      Configure_IO
        (SD_Detect_Pin,
         (Mode        => Mode_In,
          Output_Type => Open_Drain,
          Speed       => Speed_High,
          Resistors   => Pull_Up));

      Controller.Device :=
        STM32.SDMMC.As_Controller (STM32_SVD.SDMMC.SDMMC1_Periph'Access);

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

--        Disable (SD_DMA, SD_DMA_Tx_Stream);
--        Configure
--          (SD_DMA,
--           SD_DMA_Tx_Stream,
--           (Channel                      => SD_DMA_Tx_Channel,
--            Direction                    => Memory_To_Peripheral,
--            Increment_Peripheral_Address => False,
--            Increment_Memory_Address     => True,
--            Peripheral_Data_Format       => Words,
--            Memory_Data_Format           => Words,
--            Operation_Mode               => Peripheral_Flow_Control_Mode,
--            Priority                     => Priority_Very_High,
--            FIFO_Enabled                 => True,
--            FIFO_Threshold               => FIFO_Threshold_Full_Configuration,
--            Memory_Burst_Size            => Memory_Burst_Inc4,
--            Peripheral_Burst_Size        => Peripheral_Burst_Inc4));
--        Clear_All_Status (SD_DMA, SD_DMA_Tx_Stream);
   end Initialize;

   ------------------
   -- Card_Present --
   ------------------

   function Card_Present
     (Controller : in out SDCard_Controller) return Boolean
   is
      pragma Unreferenced (Controller);
   begin
      if STM32.GPIO.Set (SD_Detect_Pin) then
         --  No card
         Controller.Has_Info := False;
         return False;
      else
         return True;
      end if;
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
        (Controller.Device, Controller.Info);

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

   overriding function Block_Size
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

      procedure Interrupt is
      begin
         Finished := True;

         if Status (SD_DMA, SD_DMA_Rx_Stream, FIFO_Error_Indicated) then
            Disable_Interrupt (SD_DMA, SD_DMA_Rx_Stream, FIFO_Error_Interrupt);
            Clear_Status (SD_DMA, SD_DMA_Rx_Stream, FIFO_Error_Indicated);

            DMA_Status := DMA_FIFO_Error;
         end if;

         if Status (SD_DMA, SD_DMA_Rx_Stream, Transfer_Error_Indicated) then
            Disable_Interrupt
              (SD_DMA, SD_DMA_Rx_Stream, Transfer_Error_Interrupt);
            Clear_Status (SD_DMA, SD_DMA_Rx_Stream, Transfer_Error_Indicated);

            DMA_Status := DMA_Transfer_Error;
         end if;

         if Status (SD_DMA, SD_DMA_Rx_Stream, Transfer_Complete_Indicated) then
            Disable_Interrupt
              (SD_DMA, SD_DMA_Rx_Stream, Transfer_Complete_Interrupt);
            Clear_Status
              (SD_DMA, SD_DMA_Rx_Stream, Transfer_Complete_Indicated);

            DMA_Status := DMA_No_Error;
         end if;
      end Interrupt;

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
         Device    := Controller.Device;
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

         if Get_Flag (Device, Data_End) then
            Clear_Flag (Device, Data_End);
            SD_Status := OK;

         elsif Get_Flag (Device, Data_CRC_Fail) then
            Clear_Flag (Device, Data_CRC_Fail);
            SD_Status := CRC_Check_Fail;

         elsif Get_Flag (Device, Data_Timeout) then
            Clear_Flag (Device, Data_Timeout);
            SD_Status := Timeout_Error;

         elsif Get_Flag (Device, RX_Overrun) then
            Clear_Flag (Device, RX_Overrun);
            SD_Status := Rx_Overrun;

         elsif Get_Flag (Device, TX_Underrun) then
            Clear_Flag (Device, TX_Underrun);
            SD_Status := Tx_Underrun;
         end if;

         for Int in SDMMC_Interrupts loop
            Disable_Interrupt (Device, Int);
         end loop;
      end Interrupt;

   end SDMMC_Interrupt_Handler;

   ----------------
   -- Read_Block --
   ----------------

   overriding function Read_Block
     (Controller   : in out SDCard_Controller;
      Block_Number : Unsigned_32;
      Data         : out Block) return Boolean
   is
      Ret     : SD_Error;
      DMA_Err : DMA_Error_Code;
   begin
      Ensure_Card_Informations (Controller);

      DMA_Interrupt_Handler.Set_Transfer_State;
      SDMMC_Interrupt_Handler.Set_Transfer_State (Controller);

      Ret := Read_Blocks_DMA
        (Controller.Device,
         Unsigned_64 (Block_Number) *
             Unsigned_64 (Controller.Info.Card_Block_Size),
         SD_DMA,
         SD_DMA_Rx_Stream,
         SD_Data (Data));

      if Ret /= OK then
         DMA_Interrupt_Handler.Clear_Transfer_State;
         SDMMC_Interrupt_Handler.Clear_Transfer_State;
         Abort_Transfer (SD_DMA, SD_DMA_Rx_Stream, DMA_Err);

         return False;
      end if;

      DMA_Interrupt_Handler.Wait_Transfer (DMA_Err);
      SDMMC_Interrupt_Handler.Wait_Transfer (Ret);

      loop
         exit when not Get_Flag (Controller.Device, RX_Active);
      end loop;

--        if Data'Length > 512 then
--           Ret := Stop_Transfer (Controller.Device);
--        end if;

      Clear_All_Status (SD_DMA, SD_DMA_Rx_Stream);
      Disable (SD_DMA, SD_DMA_Rx_Stream);

      Cortex_M.Cache.Invalidate_DCache
        (Start => Data (Data'First)'Address,
         Len   => Data'Length);

      return Ret = OK
        and then DMA_Err = DMA_No_Error;
   end Read_Block;

end Media_Reader.SDCard;
