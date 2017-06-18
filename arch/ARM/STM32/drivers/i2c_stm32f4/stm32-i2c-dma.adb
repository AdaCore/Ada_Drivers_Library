------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with STM32_SVD.I2C; use STM32_SVD.I2C;
with STM32.Device;  use STM32.Device;
with STM32.DMA;     use STM32.DMA;

package body STM32.I2C.DMA is

   use type HAL.I2C.I2C_Status;

   ------------------------
   -- Set_TX_DMA_Handler --
   ------------------------

   procedure Set_TX_DMA_Handler
     (This : in out I2C_Port_DMA;
      DMA  : DMA_Interrupt_Controller_Access)
   is
   begin
      This.TX_DMA := DMA;
   end Set_TX_DMA_Handler;

   ------------------------
   -- Set_RX_DMA_Handler --
   ------------------------

   procedure Set_RX_DMA_Handler
     (This : in out I2C_Port_DMA;
      DMA  : DMA_Interrupt_Controller_Access)
   is
   begin
      This.RX_DMA := DMA;
   end Set_RX_DMA_Handler;

   ---------------------------
   -- Set_Polling_Threshold --
   ---------------------------

   procedure Set_Polling_Threshold (This      : in out I2C_Port_DMA;
                                    Threshold : Natural)
   is
   begin
      This.Threshold := Threshold;
   end Set_Polling_Threshold;

   ---------------
   -- Data_Send --
   ---------------

   overriding
   procedure Data_Send
     (This    : in out I2C_Port_DMA;
      Data    :        HAL.I2C.I2C_Data;
      Timeout :        Natural;
      Status  : out    HAL.I2C.I2C_Status)
   is
      DMA_Status : DMA_Error_Code;
   begin

      if This.TX_DMA = null or else Data'Length < This.Threshold then
         Parent (This).Data_Send (Data, Timeout, Status);
         return;
      end if;

      if not Compatible_Alignments (This.TX_DMA.Controller.all,
                                    This.TX_DMA.Stream,
                                    Data'Address,
                                    This.Data_Register_Address)
      then
         raise Program_Error with "Incompatible alignments";
      end if;

      This.TX_DMA.Start_Transfer (Source      => Data'Address,
                                  Destination => This.Data_Register_Address,
                                  Data_Count  => Data'Length);

      This.TX_DMA.Wait_For_Completion (DMA_Status);

      --  Disable DMA
      This.Periph.CR2.DMAEN := False;

      if DMA_Status = DMA_No_Error then
         Status := HAL.I2C.Ok;
      else
         Status := HAL.I2C.Err_Error;
      end if;
   end Data_Send;

   ------------------
   -- Data_Receive --
   ------------------

   overriding
   procedure Data_Receive
     (This    : in out I2C_Port_DMA;
      Data    :    out HAL.I2C.I2C_Data;
      Timeout :        Natural;
      Status  :    out HAL.I2C.I2C_Status)
   is
      DMA_Status : DMA_Error_Code;
   begin

      if This.RX_DMA = null or else Data'Length < This.Threshold then
         Parent (This).Data_Receive (Data, Timeout, Status);
         return;
      end if;

      if not Compatible_Alignments (This.RX_DMA.Controller.all,
                                    This.RX_DMA.Stream,
                                    This.Data_Register_Address,
                                    Data'Address)
      then
         raise Program_Error with "Incompatible alignments";
      end if;

      This.RX_DMA.Start_Transfer (Source      => This.Data_Register_Address,
                                  Destination => Data'Address,
                                  Data_Count  => Data'Length);

      This.RX_DMA.Wait_For_Completion (DMA_Status);

      --  Disable DMA
      This.Periph.CR2.DMAEN := False;

      if DMA_Status = DMA_No_Error then
         Status := HAL.I2C.Ok;
      else
         Status := HAL.I2C.Err_Error;
      end if;
   end Data_Receive;

end STM32.I2C.DMA;
