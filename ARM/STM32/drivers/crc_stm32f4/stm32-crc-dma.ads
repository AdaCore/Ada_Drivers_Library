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

--  A driver for the Cyclic Redundancy Check CRC-32 calculation processor,
--  using DMA to transfer the data to the CRC unit (instead of the CPU).

--  Note this API is for the STM32 F4x family. Other STM MCUs have additional
--  CRC capabilities.

--  See app note AN4187 "Using CRC through DMA"

with STM32.DMA; use STM32.DMA;

package STM32.CRC.DMA is
   pragma Elaborate_Body;

   --  These routines use the specified controller and stream to transfer
   --  all of the Input data components to This CRC unit, updating the
   --  CRC value accordingly. At the end of the transfer the DMA interrupt
   --  Transfer_Complete_Indicated is triggered. Clients are expected to have
   --  an application-defined handler for that interrupt, in order to await
   --  completion of the transfer.

   --  Note that you can use a slice if the entire array is not intended for
   --  transfer, but beware alignment boundaries to prevent copying of the
   --  actual parameter.

   procedure Update_CRC
     (This       : in out CRC_32;
      Controller : access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Input      : Block_32);
   --  Update the calculated CRC value based on all of the 32-bit components
   --  of Input. Triggers DMA.Transfer_Complete_Indicated on completion.

   procedure Update_CRC
     (This       : in out CRC_32;
      Controller : access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Input      : Block_16);
   --  Update the calculated CRC value based on all of the 16-bit components
   --  of Input. Triggers DMA.Transfer_Complete_Indicated on completion.

   procedure Update_CRC
     (This       : in out CRC_32;
      Controller : access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      Input      : Block_8);
   --  Update the calculated CRC value based on all of the 8-bit components
   --  of Input. Triggers DMA.Transfer_Complete_Indicated on completion.

end STM32.CRC.DMA;
