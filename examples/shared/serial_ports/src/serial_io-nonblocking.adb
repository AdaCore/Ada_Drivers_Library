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

with STM32.Device; use STM32.Device;

package body Serial_IO.Nonblocking is

   -------------------------
   -- Initialize_Hardware -- 
   -------------------------
   
   procedure Initialize_Hardware (This : in out Serial_Port) is
   begin
      Serial_IO.Initialize_Hardware (This.Device);
   end Initialize_Hardware;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This      : in out Serial_Port;
      Baud_Rate : Baud_Rates;
      Parity    : Parities     := No_Parity;
      Data_Bits : Word_Lengths := Word_Length_8;
      End_Bits  : Stop_Bits    := Stopbits_1;
      Control   : Flow_Control := No_Flow_Control)
   is
   begin
      Serial_IO.Configure (This.Device, Baud_Rate, Parity, Data_Bits, End_Bits, Control);
   end Configure;
   
   ----------
   -- Send --
   ----------

   procedure Send (This : in out Serial_Port;  Msg : not null access Message) is
   begin
      This.Start_Sending (Msg);
   end Send;

   -------------
   -- Receive --
   -------------

   procedure Receive (This : in out Serial_Port;  Msg : not null access Message) is
   begin
      This.Start_Receiving (Msg);
   end Receive;

   -----------------
   -- Serial_Port --
   -----------------

   protected body Serial_Port is

      -------------------------
      -- Handle_Transmission --
      -------------------------

      procedure Handle_Transmission is
      begin
         --  if Word_Lenth = 9 then
         --    -- handle the extra byte required for the 9th bit
         --  else  -- 8 data bits so no extra byte involved
         Transmit
           (Device.Transceiver.all,
            Character'Pos (Outgoing_Msg.Content_At (Next_Out)));
         Next_Out := Next_Out + 1;
         --  end if;
         if Next_Out > Outgoing_Msg.Length then
            Disable_Interrupts (Device.Transceiver.all, Source => Transmission_Complete);
            Outgoing_Msg.Signal_Transmission_Complete;
            Outgoing_Msg := null;
         end if;
      end Handle_Transmission;

      ----------------------
      -- Handle_Reception --
      ----------------------

      procedure Handle_Reception is
         Received_Char : constant Character := Character'Val (Current_Input (Device.Transceiver.all));
      begin
         if Received_Char /= Terminator (Incoming_Msg.all) then
            Incoming_Msg.Append (Received_Char);
         end if;

         if Received_Char = Incoming_Msg.Terminator or
            Incoming_Msg.Length = Incoming_Msg.Physical_Size
         then -- reception complete
            loop
               --  wait for device to clear the status
               exit when not Status (Device.Transceiver.all, Read_Data_Register_Not_Empty);
            end loop;
            Disable_Interrupts (Device.Transceiver.all, Source => Received_Data_Not_Empty);
            Incoming_Msg.Signal_Reception_Complete;
            Incoming_Msg := null;
         end if;
      end Handle_Reception;

      ---------
      -- ISR --
      ---------

      procedure ISR is
      begin
         --  check for data arrival
         if Status (Device.Transceiver.all, Read_Data_Register_Not_Empty) and
           Interrupt_Enabled (Device.Transceiver.all, Received_Data_Not_Empty)
         then
            Detect_Errors (Is_Xmit_IRQ => False);
            Handle_Reception;
            Clear_Status (Device.Transceiver.all, Read_Data_Register_Not_Empty);
         end if;

         --  check for transmission ready
         if Status (Device.Transceiver.all, Transmission_Complete_Indicated) and
           Interrupt_Enabled (Device.Transceiver.all, Transmission_Complete)
         then
            Detect_Errors (Is_Xmit_IRQ => True);
            Handle_Transmission;
            Clear_Status (Device.Transceiver.all, Transmission_Complete_Indicated);
         end if;
      end ISR;

      -------------------
      -- Start_Sending --
      -------------------

      procedure Start_Sending (Msg : not null access Message) is
      begin
         Outgoing_Msg := Msg;
         Next_Out := 1;

         Enable_Interrupts (Device.Transceiver.all, Parity_Error);
         Enable_Interrupts (Device.Transceiver.all, Error);
         Enable_Interrupts (Device.Transceiver.all, Transmission_Complete);
      end Start_Sending;

      ---------------------
      -- Start_Receiving --
      ---------------------

      procedure Start_Receiving (Msg : not null access Message) is
      begin
         Incoming_Msg := Msg;
         Incoming_Msg.Clear;

         Enable_Interrupts (Device.Transceiver.all, Parity_Error);
         Enable_Interrupts (Device.Transceiver.all, Error);
         Enable_Interrupts (Device.Transceiver.all, Received_Data_Not_Empty);
      end Start_Receiving;

      -------------------
      -- Detect_Errors --
      -------------------

      procedure Detect_Errors (Is_Xmit_IRQ : Boolean) is
      begin
         if Status (Device.Transceiver.all, Parity_Error_Indicated) and
           Interrupt_Enabled (Device.Transceiver.all, Parity_Error)
         then
            Clear_Status (Device.Transceiver.all, Parity_Error_Indicated);
            if Is_Xmit_IRQ then
               Outgoing_Msg.Note_Error (Parity_Error_Detected);
            else
               Incoming_Msg.Note_Error (Parity_Error_Detected);
            end if;
         end if;

         if Status (Device.Transceiver.all, Framing_Error_Indicated) and
           Interrupt_Enabled (Device.Transceiver.all, Error)
         then
            Clear_Status (Device.Transceiver.all, Framing_Error_Indicated);
            if Is_Xmit_IRQ then
               Outgoing_Msg.Note_Error (Frame_Error_Detected);
            else
               Incoming_Msg.Note_Error (Frame_Error_Detected);
            end if;
         end if;

         if Status (Device.Transceiver.all, USART_Noise_Error_Indicated) and
           Interrupt_Enabled (Device.Transceiver.all, Error)
         then
            Clear_Status (Device.Transceiver.all, USART_Noise_Error_Indicated);
            if Is_Xmit_IRQ then
               Outgoing_Msg.Note_Error (Noise_Error_Detected);
            else
               Incoming_Msg.Note_Error (Noise_Error_Detected);
            end if;
         end if;

         if Status (Device.Transceiver.all, Overrun_Error_Indicated) and
           Interrupt_Enabled (Device.Transceiver.all, Error)
         then
            Clear_Status (Device.Transceiver.all, Overrun_Error_Indicated);
            if Is_Xmit_IRQ then
               Outgoing_Msg.Note_Error (Overrun_Error_Detected);
            else
               Incoming_Msg.Note_Error (Overrun_Error_Detected);
            end if;
         end if;
      end Detect_Errors;
      
   end Serial_Port;

end Serial_IO.Nonblocking;
