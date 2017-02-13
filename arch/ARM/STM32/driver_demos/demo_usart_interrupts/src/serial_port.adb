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

package body Serial_Port is

   ---------------
   -- As_String --
   ---------------

   function As_String (This : Message) return String is
   begin
      return This.Content (1 .. This.Logical_Size);
   end As_String;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Message;  To : String) is
   begin
      This.Content (1 .. To'Length) := To;
      This.Logical_Size := To'Length;
   end Set;

   ----------------
   -- Controller --
   ----------------

   protected body Controller is

      ---------------------
      -- Errors_Detected --
      ---------------------

      function Errors_Detected return Error_Conditions is
      begin
         return Errors;
      end Errors_Detected;

      -------------------
      -- Detect_Errors --
      -------------------

      procedure Detect_Errors is
      begin
         --  check for parity error
         if Status (Device.all, Parity_Error_Indicated) and
           Interrupt_Enabled (Device.all, Parity_Error)
         then
            Clear_Status (Device.all, Parity_Error_Indicated);
            Errors := Errors or Parity_Error_Detected;
         end if;

         --  check for framing error
         if Status (Device.all, Framing_Error_Indicated) and
           Interrupt_Enabled (Device.all, Error)
         then
            Clear_Status (Device.all, Framing_Error_Indicated);
            Errors := Errors or Frame_Error_Detected;
         end if;

         --  check for noise error
         if Status (Device.all, USART_Noise_Error_Indicated) and
           Interrupt_Enabled (Device.all, Error)
         then
            Clear_Status (Device.all, USART_Noise_Error_Indicated);
            Errors := Errors or Noise_Error_Detected;
         end if;

         --  check for overrun error
         if Status (Device.all, Overrun_Error_Indicated) and
           Interrupt_Enabled (Device.all, Error)
         then
            Clear_Status (Device.all, Overrun_Error_Indicated);
            Errors := Errors or Overrun_Error_Detected;
         end if;
      end Detect_Errors;

      -------------------------
      -- Handle_Transmission --
      -------------------------

      procedure Handle_Transmission is
      begin
         --  if Word_Lenth = 9 then
         --    -- handle the extra byte required for the 9th bit
         --  else  -- 8 data bits so no extra byte involved
         Transmit (Device.all, Character'Pos (Outgoing_Msg.Content (Next_Out)));
         Next_Out := Next_Out + 1;
         --  end if;
         Awaiting_Transfer := Awaiting_Transfer - 1;
         if Awaiting_Transfer = 0 then
            Disable_Interrupts (Device.all, Source => Transmit_Data_Register_Empty);
            Set_True (Outgoing_Msg.Transmission_Complete);
         end if;
      end Handle_Transmission;

      ----------------------
      -- Handle_Reception --
      ----------------------

      procedure Handle_Reception is
         Received_Char : constant Character := Character'Val (Current_Input (Device.all));
      begin
         Incoming_Msg.Content (Next_In) := Received_Char;
         if Received_Char = Incoming_Msg.Terminator or
            Next_In = Incoming_Msg.Physical_Size
         then --  reception complete
            Incoming_Msg.Logical_Size := Next_In;
            loop
               exit when not Status (Device.all, Read_Data_Register_Not_Empty);
            end loop;
            Disable_Interrupts (Device.all, Source => Received_Data_Not_Empty);
            Set_True (Incoming_Msg.Reception_Complete);
         else
            Next_In := Next_In + 1;
         end if;
      end Handle_Reception;

      -----------------
      -- IRQ_Handler --
      -----------------

      procedure IRQ_Handler is
      begin
         Detect_Errors;

         --  check for data arrival
         if Status (Device.all, Read_Data_Register_Not_Empty) and
           Interrupt_Enabled (Device.all, Received_Data_Not_Empty)
         then
            Handle_Reception;
            Clear_Status (Device.all, Read_Data_Register_Not_Empty);
         end if;

         --  check for transmission ready
         if Status (Device.all, Transmit_Data_Register_Empty) and
           Interrupt_Enabled (Device.all, Transmit_Data_Register_Empty)
         then
            Handle_Transmission;
            Clear_Status (Device.all, Transmit_Data_Register_Empty);
         end if;
      end IRQ_Handler;

      -------------------
      -- Start_Sending --
      -------------------

      procedure Start_Sending (Msg : not null access Message) is
      begin
         Outgoing_Msg := Msg;
         Awaiting_Transfer := Msg.Logical_Size;
         Next_Out := Msg.Content'First;

         Enable_Interrupts (Device.all, Source => Parity_Error);
         Enable_Interrupts (Device.all, Source => Error);
         Enable_Interrupts (Device.all, Source => Transmit_Data_Register_Empty);
      end Start_Sending;

      ---------------------
      -- Start_Receiving --
      ---------------------

      procedure Start_Receiving (Msg : not null access Message) is
      begin
         Incoming_Msg := Msg;
         Next_In := Incoming_Msg.Content'First;

         Enable_Interrupts (Device.all, Source => Parity_Error);
         Enable_Interrupts (Device.all, Source => Error);
         Enable_Interrupts (Device.all, Source => Received_Data_Not_Empty);
      end Start_Receiving;

   end Controller;


end Serial_Port;
