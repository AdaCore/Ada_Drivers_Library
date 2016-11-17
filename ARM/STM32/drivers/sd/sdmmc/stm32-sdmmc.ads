--  Initially based on stm32f7xx_hal_sd.h
--  V1.0.4
--  09-December-2015
--
--  SDCard driver. Controls the SDMMC peripheral.

with System;
with STM32_SVD.SDMMC; use STM32_SVD.SDMMC;
with HAL.SDCard;      use HAL.SDCard;

with STM32.DMA;

package STM32.SDMMC is

   type SDMMC_Controller (Periph : access STM32_SVD.SDMMC.SDMMC_Peripheral) is
   limited new HAL.SDCard.SDCard_Driver with private;

   function Initialize
     (This      : in out SDMMC_Controller;
      SDMMC_CLK : Natural;
      Info      : out Card_Information) return SD_Error;

   function Get_Card_Type
     (This : SDMMC_Controller) return Supported_SD_Memory_Cards;

   type SD_Data is array (Unsigned_16 range <>) of Byte
   with Pack;

   function Read_Blocks
     (This : in out SDMMC_Controller;
      Addr : Unsigned_64;
      Data : out SD_Data) return SD_Error
     with Pre => Data'Length mod 512 = 0;

   function Read_Blocks_DMA
     (This   : in out SDMMC_Controller;
      Addr   : Unsigned_64;
      DMA    : STM32.DMA.DMA_Controller;
      Stream : STM32.DMA.DMA_Stream_Selector;
      Data   : out SD_Data) return SD_Error;

   function Write_Blocks_DMA
     (This   : in out SDMMC_Controller;
      Addr   : Unsigned_64;
      DMA    : STM32.DMA.DMA_Controller;
      Stream : STM32.DMA.DMA_Stream_Selector;
      Data   : SD_Data) return SD_Error;

   function Stop_Transfer
     (This : in out SDMMC_Controller) return SD_Error;

   function Get_FIFO_Address
     (This : SDMMC_Controller) return System.Address;

   function Get_Transfer_Status
     (This : in out SDMMC_Controller) return SD_Error;

   type SDMMC_Flags is
     (Data_End,
      Data_CRC_Fail,
      Data_Timeout,
      RX_Overrun,
      TX_Underrun,
      RX_Active,
      TX_Active);

   subtype SDMMC_Clearable_Flags is SDMMC_Flags range Data_End .. TX_Underrun;

   function Get_Flag
     (This : SDMMC_Controller;
      Flag : SDMMC_Flags) return Boolean;

   procedure Clear_Flag
     (This : in out SDMMC_Controller;
      Flag : SDMMC_Clearable_Flags);

   procedure Clear_Static_Flags (This : in out SDMMC_Controller);

   type SDMMC_Interrupts is
     (Data_End_Interrupt,
      Data_CRC_Fail_Interrupt,
      Data_Timeout_Interrupt,
      TX_FIFO_Empty_Interrupt,
      RX_FIFO_Full_Interrupt,
      TX_Underrun_Interrupt,
      RX_Overrun_Interrupt);

   procedure Enable_Interrupt
     (This      : in out SDMMC_Controller;
      Interrupt : SDMMC_Interrupts);

   procedure Disable_Interrupt
     (This      : in out SDMMC_Controller;
      Interrupt : SDMMC_Interrupts);

   procedure Disable_Data
     (This : in out SDMMC_Controller);

   type SDMMC_Operation is
     (No_Operation,
      Read_Single_Block_Operation,
      Read_Multiple_Blocks_Operation,
      Write_Single_Block_Operation,
      Write_Multiple_Blocks_Operation);

   function Last_Operation
     (This : SDMMC_Controller) return SDMMC_Operation;

private

   type Card_Data_Table is array (0 .. 3) of UInt32;

   type SDMMC_Controller (Periph : access STM32_SVD.SDMMC.SDMMC_Peripheral) is
   limited new SDCard_Driver with record
      CLK_In    : Natural;
      RCA       : Unsigned_16;
      Card_Type : Supported_SD_Memory_Cards := STD_Capacity_SD_Card_V1_1;
      Operation : SDMMC_Operation := No_Operation;
   end record;

   overriding procedure Reset
     (This   : in out SDMMC_Controller;
      Status : out SD_Error);

   overriding procedure Set_Clock
     (This   : in out SDMMC_Controller;
      Freq   : Natural);

   overriding procedure Set_Bus_Size
     (This : in out SDMMC_Controller;
      Mode : Wide_Bus_Mode);

   overriding procedure Send_Cmd
     (This   : in out SDMMC_Controller;
      Cmd    : Cmd_Desc_Type;
      Arg    : Unsigned_32;
      Status : out SD_Error);

   overriding procedure Read_Cmd
     (This   : in out SDMMC_Controller;
      Cmd    : Cmd_Desc_Type;
      Arg    : Unsigned_32;
      Buf    : System.Address;
      Len    : Unsigned_32;
      Status : out SD_Error);

   overriding procedure Read_Rsp48
     (This : in out SDMMC_Controller;
      Rsp  : out Unsigned_32);

   overriding procedure Read_Rsp136
     (This           : in out SDMMC_Controller;
      W0, W1, W2, W3 : out Unsigned_32);

   function Get_Card_Type
     (This : SDMMC_Controller) return Supported_SD_Memory_Cards
   is (This.Card_Type);

   type Data_Direction is (Read, Write);

   function Get_FIFO_Address
     (This : SDMMC_Controller) return System.Address
   is (This.Periph.FIFO'Address);

   function Get_Flag
     (This : SDMMC_Controller;
      Flag : SDMMC_Flags) return Boolean
   is (case Flag is
          when Data_End      => This.Periph.STA.DATAEND,
          when Data_CRC_Fail => This.Periph.STA.DCRCFAIL,
          when Data_Timeout  => This.Periph.STA.DTIMEOUT,
          when RX_Overrun    => This.Periph.STA.RXOVERR,
          when TX_Underrun   => This.Periph.STA.TXUNDERR,
          when RX_Active     => This.Periph.STA.RXACT,
          when TX_Active     => This.Periph.STA.TXACT);

   function Last_Operation
     (This : SDMMC_Controller) return SDMMC_Operation
   is (This.Operation);

end STM32.SDMMC;
