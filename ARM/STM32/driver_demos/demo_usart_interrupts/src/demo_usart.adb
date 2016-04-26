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

--  A demonstration of using the USART device.  Note it requires a program on
--  the host to see the output and interact with the program on the board.

--  The file declares the main procedure for the demonstration.

with STM32;                        use STM32;
with STM32.GPIO;                   use STM32.GPIO;
with STM32.USARTs;                 use STM32.USARTs;

with STM32.Device;                 use STM32.Device;

with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

with Peripherals;                  use Peripherals;
with Serial_Port;                  use Serial_Port;

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

procedure Demo_USART is

   Outgoing : aliased Message (Physical_Size => 1024);  -- arbitrary size

   -----------------------------
   -- Initialize_STMicro_UART --
   -----------------------------

   procedure Initialize_STMicro_UART is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (Transceiver);
      Enable_Clock (IO_Port);

      Configuration.Mode := Mode_AF;
      Configuration.Speed := Speed_50MHz;
      Configuration.Output_Type := Push_Pull;
      Configuration.Resistors := Pull_Up;

      Configure_IO
        (Port => IO_Port,
         Pins => Rx_Pin & Tx_Pin,
         Config => Configuration);

      Configure_Alternate_Function
        (Port => IO_Port,
         Pins => Rx_Pin & Tx_Pin,
         AF   => Transceiver_AF);
   end Initialize_STMicro_UART;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Initialize_STMicro_UART;

      Disable (Transceiver);

      Set_Baud_Rate    (Transceiver, 115_200);
      Set_Mode         (Transceiver, Tx_Rx_Mode);
      Set_Stop_Bits    (Transceiver, Stopbits_1);
      Set_Word_Length  (Transceiver, Word_Length_8);
      Set_Parity       (Transceiver, No_Parity);
      Set_Flow_Control (Transceiver, No_Flow_Control);

      Enable (Transceiver);
   end Initialize;

   --------------
   -- Interact --
   --------------

   procedure Interact is
      Received : aliased Message (Physical_Size => 1024);  -- arbitrary size
   begin
      Received.Terminator := ASCII.CR;
      loop
         Peripherals.COM.Start_Receiving (Received'Unchecked_Access);
         Suspend_Until_True (Received.Reception_Complete);

         Set (Outgoing, To => "Received : " & As_String (Received));
         Peripherals.COM.Start_Sending (Outgoing'Unchecked_Access);
         Suspend_Until_True (Outgoing.Transmission_Complete);
      end loop;
   end Interact;

begin
   Initialize;

   Set (Outgoing, To => "Enter text, terminated by CR.");
   Peripherals.COM.Start_Sending (Outgoing'Unchecked_Access);
   Suspend_Until_True (Outgoing.Transmission_Complete);

   Interact;
end Demo_USART;


