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
with STM32_SVD.SDMMC;      use STM32_SVD.SDMMC;
with HAL.SDMMC;            use HAL.SDMMC;
with HAL.Block_Drivers;
with STM32.DMA;
with STM32.DMA.Interrupts;

package STM32.SDMMC is

   type SDMMC_Controller (Periph : access STM32_SVD.SDMMC.SDMMC_Peripheral)
      is limited private;

   procedure Set_Clk_Src_Speed
     (This : in out SDMMC_Controller;
      CLK  : UInt32);

   function Initialize
     (This      : in out SDMMC_Controller;
      Info      : out Card_Information) return SD_Error;

   function Read_Blocks
     (This : in out SDMMC_Controller;
      Addr : UInt64;
      Data : out HAL.Block_Drivers.Block) return SD_Error
     with Pre => Data'Length mod 512 = 0;

   function Read_Blocks_DMA
     (This   : in out SDMMC_Controller;
      Addr   :        UInt64;
      DMA    : in out STM32.DMA.Interrupts.DMA_Interrupt_Controller;
      Data   :    out HAL.Block_Drivers.Block) return SD_Error;

   function Write_Blocks_DMA
     (This   : in out SDMMC_Controller;
      Addr   :        UInt64;
      DMA    : in out STM32.DMA.Interrupts.DMA_Interrupt_Controller;
      Data   :        HAL.Block_Drivers.Block) return SD_Error;

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
   limited new SDMMC_Driver with record
      CLK_In    : UInt32 := 48_000_000; --  48 MHz clock by default
      RCA       : UInt16;
      Card_Type : Supported_SD_Memory_Cards := STD_Capacity_SD_Card_V1_1;
      Operation : SDMMC_Operation := No_Operation;
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

   function Command_Error
     (Controller : in out SDMMC_Controller) return SD_Error;

   overriding procedure Read_Rsp48
     (This : in out SDMMC_Controller;
      Rsp  : out UInt32);

   overriding procedure Read_Rsp136
     (This           : in out SDMMC_Controller;
      W0, W1, W2, W3 : out UInt32);

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
