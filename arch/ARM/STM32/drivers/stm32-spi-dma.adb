------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016-2017, AdaCore                      --
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

with STM32.DMA; use STM32.DMA;
with HAL.SPI;

package body STM32.SPI.DMA is

   procedure Transmit_Common
     (This        : in out SPI_Port_DMA;
      Source      : System.Address;
      Data_Count  : UInt16;
      Status      : out HAL.SPI.SPI_Status;
      Timeout     : Natural := 1000);

   ---------------------
   -- Transmit_Common --
   ---------------------

   procedure Transmit_Common
     (This        : in out SPI_Port_DMA;
      Source      : System.Address;
      Data_Count  : UInt16;
      Status      : out HAL.SPI.SPI_Status;
      Timeout     : Natural := 1000)
   is
      pragma Unreferenced (Timeout);
      DMA_Status : DMA_Error_Code;
   begin
      if not Compatible_Alignments (This.TX_DMA.Controller.all,
                                    This.TX_DMA.Stream,
                                    Source,
                                    This.Data_Register_Address)
      then
         raise Program_Error with "Incompatible alignments";
      end if;

      --  Enable TX DMA
      This.Periph.CR2.TXDMAEN := True;

      This.TX_DMA.Start_Transfer (Source      => Source,
                                  Destination => This.Data_Register_Address,
                                  Data_Count  => Data_Count);

      This.TX_DMA.Wait_For_Completion (DMA_Status);

      if DMA_Status = DMA_No_Error then
         Status := HAL.SPI.Ok;
      else
         Status := HAL.SPI.Err_Error;
      end if;
   end Transmit_Common;


   ------------------------
   -- Set_TX_DMA_Handler --
   ------------------------

   procedure Set_TX_DMA_Handler
     (This : in out SPI_Port_DMA;
      DMA  : DMA_Interrupt_Controller_Access)
   is
   begin
      This.TX_DMA := DMA;
   end Set_TX_DMA_Handler;

   ---------------------------
   -- Set_Polling_Threshold --
   ---------------------------

   procedure Set_Polling_Threshold
     (This      : in out SPI_Port_DMA;
      Threshold : Natural)
   is
   begin
      This.Threshold := Threshold;
   end Set_Polling_Threshold;

   ---------------
   -- Configure --
   ---------------

   overriding procedure Configure
     (This : in out SPI_Port_DMA;
      Conf : SPI_Configuration)
   is
   begin
      Configure (Parent (This), Conf);
   end Configure;

   --------------
   -- Transmit --
   --------------

   overriding procedure Transmit
     (This   : in out SPI_Port_DMA;
      Data   : HAL.SPI.SPI_Data_8b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000)
   is
   begin
      if This.TX_DMA = null or else Data'Length < This.Threshold then
         --  Fallback to polling implementation
         Transmit (Parent (This), Data, Status, Timeout);
      else
         Transmit_Common (This,
                          Data (Data'First)'Address,
                          Data'Length,
                          Status,
                          Timeout);
      end if;
   end Transmit;

   --------------
   -- Transmit --
   --------------

   overriding procedure Transmit
     (This   : in out SPI_Port_DMA;
      Data   : HAL.SPI.SPI_Data_16b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000)
   is
   begin
      if This.TX_DMA = null or else Data'Length < This.Threshold then
         --  Fallback to polling implementation
         Transmit (Parent (This), Data, Status, Timeout);
      else
         Transmit_Common (This,
                          Data (Data'First)'Address,
                          Data'Length,
                          Status,
                          Timeout);
      end if;
   end Transmit;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (This    : in out SPI_Port_DMA;
      Data    : out HAL.SPI.SPI_Data_8b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000)
   is
   begin
      --  Not implemented, fallback to polling implementation
      Receive (Parent (This), Data, Status, Timeout);
   end Receive;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (This    : in out SPI_Port_DMA;
      Data    : out HAL.SPI.SPI_Data_16b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000)
   is
   begin
      --  Not implemented, fallback to polling implementation
      Receive (Parent (This), Data, Status, Timeout);
   end Receive;

end STM32.SPI.DMA;
