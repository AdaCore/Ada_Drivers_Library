------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017, AdaCore                           --
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

--  This package provides a protected interrupt handler that knows how to
--  handle all the DMA interrupts. It is used by clients to await a specific
--  interrupt occurrence. It is a reusable package and PO type.

--  For example:
--
--     Controller : DMA_Controller renames DMA_2;
--
--     Stream : constant DMA_Stream_Selector := Stream_0;
--
--     IRQ : constant Ada.Interrupts.Interrupt_ID := DMA2_Stream0_Interrupt;
--     --  must match that of the selected controller and stream number!
--
--     DMA_IRQ_Handler : DMA_Unit_IRQ_Handler (Controller'Access, Stream, IRQ);


with Ada.Interrupts;  use Ada.Interrupts;
with STM32.DMA;       use STM32.DMA;

package DMA_Interrupt_Handling is

   protected type DMA_Unit_IRQ_Handler
     (Controller : access DMA_Controller;
      Stream     : DMA_Stream_Selector;
      IRQ        : Interrupt_ID)
   is
      pragma Interrupt_Priority;

      entry Await_Event (Occurrence : out DMA_Interrupt);
      --  Blocks the caller until the next interrupt. Returns that interrupt in
      --  Occurrence.

   private

      Event_Occurred : Boolean := False;
      Event_Kind     : DMA_Interrupt;

      procedure IRQ_Handler;
      pragma Attach_Handler (IRQ_Handler, IRQ);
      --  Handles the interrupts and signals the entry, passing the interrupt
      --  identifier via Event_Kind.

   end DMA_Unit_IRQ_Handler;

end DMA_Interrupt_Handling;
