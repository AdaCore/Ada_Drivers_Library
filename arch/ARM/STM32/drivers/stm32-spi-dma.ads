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

with STM32.DMA.Interrupts; use STM32.DMA.Interrupts;

package STM32.SPI.DMA is

   subtype Parent is SPI_Port;
   type SPI_Port_DMA is limited new Parent with private;

   procedure Set_TX_DMA_Handler (This : in out SPI_Port_DMA;
                                 DMA  : DMA_Interrupt_Controller_Access);

   procedure Set_Polling_Threshold (This      : in out SPI_Port_DMA;
                                    Threshold : Natural);

   overriding
   procedure Configure (This : in out SPI_Port_DMA;
                        Conf : SPI_Configuration);

   overriding
   procedure Transmit
     (This   : in out SPI_Port_DMA;
      Data   : HAL.SPI.SPI_Data_8b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Transmit
     (This   : in out SPI_Port_DMA;
      Data   : HAL.SPI.SPI_Data_16b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Receive
     (This    : in out SPI_Port_DMA;
      Data    : out HAL.SPI.SPI_Data_8b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Receive
     (This    : in out SPI_Port_DMA;
      Data    : out HAL.SPI.SPI_Data_16b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000);

private

   type SPI_Port_DMA is limited new Parent with record
      TX_DMA    : DMA_Interrupt_Controller_Access := null;
      Threshold : Natural := 5;
   end record;

end STM32.SPI.DMA;
