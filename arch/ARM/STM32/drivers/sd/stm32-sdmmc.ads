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

--  Initially based on stm32f7xx_hal_sd.h
--  V1.0.4
--  09-December-2015
--
--  SDCard driver. Controls the SDMMC peripheral.

with System;
with SDMMC_SVD_Periph;

with HAL.SDMMC;             use HAL.SDMMC;
with HAL.Block_Drivers;
with STM32.DMA;
with STM32.DMA.Interrupts;

with STM32.SDMMC_Interrupt;

package STM32.SDMMC is

   type SDMMC_Controller
     (Periph : not null access SDMMC_SVD_Periph.Peripheral)
   is limited new HAL.Block_Drivers.Block_Driver
     and
       SDMMC_Driver
   with private;

   procedure Ensure_Card_Informations (This : in out SDMMC_Controller);
   --  Make sure the sdcard information is read and stored in the Controller
   --  structure

   procedure Set_Clk_Src_Speed
     (This : in out SDMMC_Controller;
      CLK  : UInt32);

   function Initialize
     (This      : in out SDMMC_Controller) return SD_Error;

   overriding function Read
     (This         : in out SDMMC_Controller;
      Block_Number : UInt64;
      Data         : out HAL.Block_Drivers.Block) return Boolean
     with Pre => Data'Length <= 16#10000#;
   --  Reads Data.
   --  Data size needs to be a multiple of the card's block size and maximum
   --  length is 2**16

   overriding function Write
     (This         : in out SDMMC_Controller;
      Block_Number : UInt64;
      Data         : HAL.Block_Drivers.Block) return Boolean
     with Pre => Data'Length <= 16#10000#;
   --  Writes Data.
   --  Data size needs to be a multiple of the card's block size and maximum
   --  length is 2**16

   function Read_Blocks
     (This : in out SDMMC_Controller;
      Addr : UInt64;
      Data : out HAL.Block_Drivers.Block) return SD_Error
     with Pre => Data'Length mod 512 = 0;

   function Read_Blocks_DMA
     (This   : in out SDMMC_Controller;
      Addr   :        UInt64;
      Data   :    out HAL.Block_Drivers.Block) return SD_Error
     with Pre => Data'Length <= 65536;

   function Write_Blocks_DMA
     (This   : in out SDMMC_Controller;
      Addr   :        UInt64;
      Data   :        HAL.Block_Drivers.Block) return SD_Error
     with Pre => Data'Length <= 65535;

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
     (This       : in out SDMMC_Controller;
      Interrupt  : SDMMC_Interrupts);

   type SDMMC_Operation is
     (No_Operation,
      Read_Single_Block_Operation,
      Read_Multiple_Blocks_Operation,
      Write_Single_Block_Operation,
      Write_Multiple_Blocks_Operation);

   function Last_Operation
     (This : SDMMC_Controller) return SDMMC_Operation;

   procedure Disable_Data
     (This : in out SDMMC_Controller);
   --  Sets DCFGR.DTEN to False to stop the data transfer mode.

   procedure Enable_DMA_Transfers
     (This   : in out SDMMC_Controller;
      RX_Int : not null STM32.DMA.Interrupts.DMA_Interrupt_Controller_Access;
      TX_Int : not null STM32.DMA.Interrupts.DMA_Interrupt_Controller_Access;
      SD_Int : not null STM32.SDMMC_Interrupt.SDMMC_Interrupt_Handler_Access);
   --  Enable DMA for SDMMC tranfers by setting the required interrupt
   --  controllers. See the examples for more info on how to initialize DMA for
   --  SDMMC.

   function Has_Card_Information
     (This : SDMMC_Controller)
      return Boolean;

   function Card_Information
     (This : SDMMC_Controller)
      return HAL.SDMMC.Card_Information
   with Pre => This.Has_Card_Information;

   procedure Clear_Card_Information
     (This : in out SDMMC_Controller);
private

   type SDMMC_Controller
     (Periph : not null access SDMMC_SVD_Periph.Peripheral)
   is limited new HAL.Block_Drivers.Block_Driver
     and
       SDMMC_Driver
   with record
      CLK_In     : UInt32 := 48_000_000; --  By default at hardware reset
      RCA        : UInt16;
      Card_Type  : Supported_SD_Memory_Cards :=
        STD_Capacity_SD_Card_V1_1;
      Operation  : SDMMC_Operation := No_Operation;

      Has_Info   : Boolean := False;
      Info       : HAL.SDMMC.Card_Information;

      TX_DMA_Int : STM32.DMA.Interrupts.DMA_Interrupt_Controller_Access := null;
      RX_DMA_Int : STM32.DMA.Interrupts.DMA_Interrupt_Controller_Access := null;
      SD_Int     : STM32.SDMMC_Interrupt.SDMMC_Interrupt_Handler_Access := null;
   end record;

   overriding procedure Delay_Milliseconds
     (This   : SDMMC_Controller;
      Amount : Natural);

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
      Arg    : UInt32;
      Status : out SD_Error);

   overriding procedure Read_Cmd
     (This   : in out SDMMC_Controller;
      Cmd    : Cmd_Desc_Type;
      Arg    : UInt32;
      Buf    : out UInt32_Array;
      Status : out SD_Error);

   overriding procedure Read_Rsp48
     (This : in out SDMMC_Controller;
      Rsp  : out UInt32);

   overriding procedure Read_Rsp136
     (This           : in out SDMMC_Controller;
      W0, W1, W2, W3 : out UInt32);

   function Command_Error
     (Controller : in out SDMMC_Controller) return SD_Error;

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
