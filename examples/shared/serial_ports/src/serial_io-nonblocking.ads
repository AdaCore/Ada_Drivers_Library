------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015-2016, AdaCore                      --
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
--  non-blocking input (Get) and output (Put) procedures. The procedures are
--  considered non-blocking because they return to the caller (potentially)
--  before the entire message is received or sent.
--
--  The serial port abstraction is a wrapper around a USART peripheral,
--  described by a value of type Peripheral_Descriptor.
--
--  Interrupts are used to send and receive characters.
--
--  NB: clients must not send or receive messages until any prior sending or
--  receiving is completed. See the two functions Sending and Receiving and
--  the preconditions on Put and Get.

with Message_Buffers; use Message_Buffers;
with Ada.Interrupts;  use Ada.Interrupts;

package Serial_IO.Nonblocking is
   pragma Elaborate_Body;

   type Serial_Port
     (IRQ    : Interrupt_ID;
      Device : not null access Peripheral_Descriptor)
   is tagged limited private;

   procedure Initialize (This : in out Serial_Port) with
     Post => Initialized (This);

   function Initialized (This : Serial_Port) return Boolean with Inline;

   Serial_Port_Uninitialized : exception;

   procedure Configure
     (This      : in out Serial_Port;
      Baud_Rate : Baud_Rates;
      Parity    : Parities     := No_Parity;
      Data_Bits : Word_Lengths := Word_Length_8;
      End_Bits  : Stop_Bits    := Stopbits_1;
      Control   : Flow_Control := No_Flow_Control)
     with
       Pre => (Initialized (This) or else raise Serial_Port_Uninitialized);

   procedure Put (This : in out Serial_Port; Msg : not null access Message) with
     Pre => (Initialized (This) or else raise Serial_Port_Uninitialized)
            and then
            not Sending (This),
     Inline;

   procedure Get (This : in out Serial_Port;  Msg : not null access Message) with
     Pre => (Initialized (This) or else raise Serial_Port_Uninitialized)
            and then
            not Receiving (This),
     Inline;

   function Sending (This : in out Serial_Port) return Boolean;
   --  Returns whether This is currently sending a message.

   function Receiving (This : in out Serial_Port) return Boolean;
   --  Returns whether This is currently receiving a message.

private

   --  The protected type defining the interrupt handling for sending and
   --  receiving characters via the USART attached to the serial port. Each
   --  serial port type a component of this protected type.
   protected type Controller (IRQ : Interrupt_ID;  Port : access Serial_Port) is

      pragma Interrupt_Priority;

      procedure Start_Sending (Msg : not null access Message);
      --  error: internal call cannot appear in precondition of protected operation
      --  with Pre => not Sending;

      procedure Start_Receiving (Msg : not null access Message);
      --  error: internal call cannot appear in precondition of protected operation
      --  with Pre => not Receiving;

      function Sending   return Boolean;
      function Receiving return Boolean;

   private

      Next_Out          : Positive;
      Awaiting_Transfer : Natural;
      Outgoing_Msg      : access Message;
      Incoming_Msg      : access Message;

      procedure Handle_Transmission with Inline;
      procedure Handle_Reception with Inline;
      procedure Detect_Errors (Is_Xmit_IRQ : Boolean) with Inline;

      procedure IRQ_Handler with Attach_Handler => IRQ;

   end Controller;


   type Serial_Port
     (IRQ    : Interrupt_ID;
      Device : not null access Peripheral_Descriptor)
   is tagged limited record
      Initialized : Boolean := False;
      Control     : Controller (IRQ, Serial_Port'Access);
   end record;

end Serial_IO.Nonblocking;
