------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016-2018, AdaCore                      --
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

with Ada.Interrupts;

package STM32.DMA.Interrupts is

   protected type DMA_Interrupt_Controller
     (Controller : not null access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      ID         : Ada.Interrupts.Interrupt_ID;
      Priority   : System.Interrupt_Priority)
   is
      pragma Interrupt_Priority (Priority);

      procedure Start_Transfer (Source      : Address;
                                Destination : Address;
                                Data_Count  : UInt16);

      procedure Abort_Transfer (Result : out DMA_Error_Code);

      procedure Clear_Transfer_State;

      function Buffer_Error return Boolean;

      entry Wait_For_Completion (Status : out DMA_Error_Code);

   private

      procedure Interrupt_Handler;
      pragma Attach_Handler (Interrupt_Handler, ID);

      No_Transfer_In_Progess : Boolean := True;
      Last_Status            : DMA_Error_Code := DMA_No_Error;
      Had_Buffer_Error       : Boolean := False;
   end DMA_Interrupt_Controller;

   type DMA_Interrupt_Controller_Access is access all DMA_Interrupt_Controller;

end STM32.DMA.Interrupts;
