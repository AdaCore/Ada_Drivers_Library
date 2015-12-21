------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

with STM32;          use STM32;
with STM32.GPIO;     use STM32.GPIO;
with STM32.USARTs;   use STM32.USARTs;
with Ada.Interrupts; use Ada.Interrupts;

package Serial_IO.Nonblocking is
   pragma Elaborate_Body;

   type Peripheral_Descriptor is record
      Transceiver        : not null access USART;
      Transceiver_AF     : GPIO_Alternate_Function;
      Tx_Pin             : GPIO_Point;
      Rx_Pin             : GPIO_Point;
   end record;

   type Serial_Port
     (IRQ    : Interrupt_Id;
      Device : not null access Peripheral_Descriptor)
   is limited private;

   procedure Initialize (This : in out Serial_Port)
     with
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

   function Receiving (This : in out Serial_Port) return Boolean;

private

   protected type Controller (IRQ : Interrupt_Id;  Port : access Serial_Port) is

      pragma Interrupt_Priority;

      procedure Start_Sending (Msg : not null access Message) with
        Pre => not Sending;

      procedure Start_Receiving (Msg : not null access Message) with
        Pre => not Receiving;

      function Sending   return Boolean;
      function Receiving return Boolean;

   private

      Next_Out          : Positive;
      Awaiting_Transfer : Natural;
      Outgoing_Msg      : access Message;
      Next_In           : Positive;
      Incoming_Msg      : access Message;

      procedure Handle_Transmission with Inline;
      procedure Handle_Reception with Inline;
      procedure Detect_Errors (Is_Xmit_IRQ : Boolean) with Inline;

      procedure IRQ_Handler;
      pragma Attach_Handler (IRQ_Handler, IRQ);

   end Controller;


   type Serial_Port
     (IRQ    : Interrupt_Id;
      Device : not null access Peripheral_Descriptor)
   is limited record
      Initialized : Boolean := False;
      Control     : Controller (IRQ, Serial_Port'Access);
   end record;

end Serial_IO.Nonblocking;
