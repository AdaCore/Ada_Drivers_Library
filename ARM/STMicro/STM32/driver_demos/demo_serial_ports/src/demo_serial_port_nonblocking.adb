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

--  A demonstration of a higher-level USART interface, using non-blocking I/O.
--  The file declares the main procedure for the demonstration.

with Peripherals_Nonblocking;    use Peripherals_Nonblocking;
with Serial_IO.Nonblocking;      use Serial_IO.Nonblocking;
with Serial_IO;                  use Serial_IO;

procedure Demo_Serial_Port_Nonblocking is

   Outgoing : aliased Message (Physical_Size => 1024);  -- arbitrary size

   procedure Interact is
      Incoming : aliased Message (Physical_Size => 1024);  -- arbitrary size
   begin
      Set_Terminator (Incoming, To => ASCII.CR);
      loop
         Get (COM, Incoming'Unchecked_Access);
         Await_Reception_Complete (Incoming);

         Set (Outgoing, To => "Received : " & Content (Incoming));
         Put (COM, Outgoing'Unchecked_Access);
         Await_Transmission_Complete (Outgoing);
      end loop;
   end Interact;

begin
   Initialize (COM);

   Configure (COM, Baud_Rate => 115_200);

   Set (Outgoing, To => "Enter text, terminated by CR.");
   Put (COM, Outgoing'Unchecked_Access);
   Await_Transmission_Complete (Outgoing);

   Interact;
end Demo_Serial_Port_Nonblocking;

