------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2016, AdaCore                         --
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

with Ada.Interrupts;                use Ada.Interrupts;
with Ada.Synchronous_Task_Control;  use Ada.Synchronous_Task_Control;
with STM32.USARTs;                  use STM32.USARTs;

with HAL; use HAL;

package Serial_Port is

   type Message (Physical_Size : Positive) is
      record
         Content               : String (1 .. Physical_Size);
         Logical_Size          : Natural := 0;
         Reception_Complete    : Suspension_Object;
         Transmission_Complete : Suspension_Object;
         Terminator            : Character := ASCII.NUL;  -- ASCII.EOM ?
      end record;

   procedure Set (This : in out Message;  To : String) with
     Pre => To'Length <= This.Physical_Size;
   --  convenience routine

   function As_String (This : Message) return String;
   --  convenience routine


   type Error_Conditions is new UInt8;

   No_Error_Detected      : constant Error_Conditions := 2#0000_0000#;
   Parity_Error_Detected  : constant Error_Conditions := 2#0000_0001#;
   Noise_Error_Detected   : constant Error_Conditions := 2#0000_0010#;
   Frame_Error_Detected   : constant Error_Conditions := 2#0000_0100#;
   Overrun_Error_Detected : constant Error_Conditions := 2#0000_1000#;
   DMA_Error_Detected     : constant Error_Conditions := 2#0001_0000#;


   protected type Controller (Device : access USART;  IRQ : Interrupt_ID) is

      pragma Interrupt_Priority;

      function Errors_Detected return Error_Conditions;

      procedure Start_Sending (Msg : not null access Message);

      procedure Start_Receiving (Msg : not null access Message);

   private

      procedure Handle_Transmission with Inline;

      procedure Handle_Reception with Inline;

      procedure Detect_Errors with Inline;

      Next_Out          : Positive;
      Awaiting_Transfer : Natural;
      Outgoing_Msg      : access Message;

      Next_In           : Positive;
      Incoming_Msg      : access Message;

      Errors            : Error_Conditions := No_Error_Detected;

      procedure IRQ_Handler;
      pragma Attach_Handler (IRQ_Handler, IRQ);

   end Controller;

end Serial_Port;
