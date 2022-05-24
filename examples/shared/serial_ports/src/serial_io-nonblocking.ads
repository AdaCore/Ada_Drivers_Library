------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015-2022, AdaCore                      --
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

--  This package defines an abstract data type for a "serial port" providing
--  non-blocking input (Receive) and output (Send) procedures. The procedures
--  are considered non-blocking because they return to the caller (potentially)
--  before the entire message is received or sent.
--
--  The serial port abstraction is a wrapper around a USART peripheral,
--  described by a value of type Peripheral_Descriptor.
--
--  Interrupts are used to send and receive characters.
--
--  NB: clients must not send or receive messages until any prior sending or
--  receiving is completed.

with Message_Buffers; use Message_Buffers;
with Ada.Interrupts;  use Ada.Interrupts;
with System;          use System;

package Serial_IO.Nonblocking is
   pragma Elaborate_Body;

   type Serial_Port
     (Device       : not null access Peripheral_Descriptor;
      IRQ          : Interrupt_ID;
      IRQ_Priority : Interrupt_Priority)
   is limited private;

   procedure Initialize_Hardware (This : in out Serial_Port);
   --  A convenience wrapper for Serial_IO.Initialize_Hardware

   procedure Configure
     (This      : in out Serial_Port;
      Baud_Rate : Baud_Rates;
      Parity    : Parities     := No_Parity;
      Data_Bits : Word_Lengths := Word_Length_8;
      End_Bits  : Stop_Bits    := Stopbits_1;
      Control   : Flow_Control := No_Flow_Control);
   --  A convenience wrapper for Serial_IO.Configure

   procedure Send
     (This : in out Serial_Port;
      Msg  : not null access Message)
   with Inline;
   --  Start sending the content of Msg.all, returning potentially
   --  prior to the completion of the message transmission

   procedure Receive
     (This : in out Serial_Port;
      Msg  : not null access Message)
   with Inline;
   --  Start receiving Msg.all content, ending when the specified
   --  Msg.Terminator character is received (it is not stored), or
   --  the physical capacity of Msg.all is reached

private

   protected type Serial_Port
     (Device       : not null access Peripheral_Descriptor;
      IRQ          : Interrupt_ID;
      IRQ_Priority : Interrupt_Priority)
   --  with
   --     Interrupt_Priority => IRQ_Priority
   is

      pragma Interrupt_Priority (IRQ_Priority);
      --  use pragma as workaround for bug in CE_2021 frontend (V523-041)

      procedure Start_Sending (Msg : not null access Message);

      procedure Start_Receiving (Msg : not null access Message);

   private

      Next_Out     : Positive;
      Outgoing_Msg : access Message;
      Incoming_Msg : access Message;

      procedure Handle_Transmission with Inline;
      procedure Handle_Reception    with Inline;
      procedure Detect_Errors (Is_Xmit_IRQ : Boolean) with Inline;

      procedure ISR with Attach_Handler => IRQ;

   end Serial_Port;

end Serial_IO.Nonblocking;
