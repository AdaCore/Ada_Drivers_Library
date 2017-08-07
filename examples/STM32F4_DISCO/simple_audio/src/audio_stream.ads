------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

with System;               use System;
with HAL;                  use HAL;
with STM32.DMA;            use STM32.DMA;
with STM32.Device;         use STM32.Device;
with Ada.Interrupts;
with Ada.Interrupts.Names;

package Audio_Stream is

   protected type Double_Buffer_Controller
     (Controller : not null access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      ID         : Ada.Interrupts.Interrupt_ID)
   is

      procedure Start
        (Destination : Address;
         Source_0    : Address;
         Source_1    : Address;
         Data_Count  : UInt16);

      entry Wait_For_Transfer_Complete;

      function Not_In_Transfer return Address;

   private
      procedure Interrupt_Handler;
      pragma Attach_Handler (Interrupt_Handler, ID);

      Interrupt_Triggered : Boolean := False;
      Buffer_0            : Address := Null_Address;
      Buffer_1            : Address := Null_Address;
   end Double_Buffer_Controller;

   Audio_TX_DMA        : STM32.DMA.DMA_Controller renames DMA_1;
   Audio_TX_DMA_Chan   : STM32.DMA.DMA_Channel_Selector renames STM32.DMA.Channel_0;
   Audio_TX_DMA_Stream : STM32.DMA.DMA_Stream_Selector renames STM32.DMA.Stream_5;
   Audio_TX_DMA_Int    : Double_Buffer_Controller (Audio_TX_DMA'Access,
                                                   Audio_TX_DMA_Stream,
                                                   Ada.Interrupts.Names.DMA1_Stream5_Interrupt);

end Audio_Stream;
