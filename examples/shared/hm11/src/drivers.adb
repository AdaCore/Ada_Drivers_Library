------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2025, AdaCore                             --
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

with Ada.Real_Time;                 use Ada.Real_Time;

package body Drivers is

   Received_Address : System.Address    with Volatile, Atomic;
   Is_Stream        : Boolean  := False with Volatile, Atomic;
   Maximum          : Positive := 1     with Volatile, Atomic;

   Position         : Positive := 1     with Volatile, Atomic;
   Until_Time       : Time;

   --------------
   -- Watchdog --
   --------------

   protected body Watchdog is

      -----------------
      -- Await_Event --
      -----------------

      entry Await_Event (Status : out UART_Status) when Event_Occurred is
      begin
         Status         := Read_Status;
         Event_Occurred := False;
      end Await_Event;

      -------------
      -- Release --
      -------------

      procedure Release (Status : UART_Status) is
      begin
         if not Event_Occurred then
            Read_Status    := Status;
            Event_Occurred := True;
            Stop;
         end if;
      end Release;

      -----------
      -- Start --
      -----------

      procedure Start
        (Received : System.Address;
         Length   : Natural;
         Timeout  : Ada.Real_Time.Time_Span)
      is
         Memory_Block : UART_Data_8b (1 .. Length) with Import,
           Address => Received;

      begin
         if Started then
            return;
         end if;

         Started    := True;
         Until_Time := Clock + Timeout;

         if not Is_Stream then
            Memory_Block := (others => 0);
            Clear_All_Status (Controller, Rx_Stream);

            Start_Transfer_with_Interrupts
              (This               => Controller,
               Stream             => Rx_Stream,
               Source             => Data_Register_Address (Drivers.UART),
               Destination        => Received,
               Data_Count         => HAL.UInt16 (Length),
               Enabled_Interrupts =>
                 (Half_Transfer_Complete_Interrupt => False, others => True));

            Resume_DMA_Reception (Drivers.UART);

         else
            Received_Address := Received;
            Position         := 1;
            Maximum          := Positive (Length);
            if Is_Stream then
               Memory_Block (1) := 0;

            else
               Memory_Block := (others => 0);
            end if;

            Clear_Status (UART, Read_Data_Register_Not_Empty);
            Enable_Interrupts (UART, Source => Received_Data_Not_Empty);
         end if;

         Clear_Status (UART, Parity_Error_Indicated);
         Clear_Status (UART, Framing_Error_Indicated);
         Clear_Status (UART, USART_Noise_Error_Indicated);
         Clear_Status (UART, Overrun_Error_Indicated);
         Clear_Status (UART, Line_Break_Detection_Indicated);
         Clear_Status (UART, Clear_To_Send_Indicated);
         Clear_Status (UART, Idle_Line_Detection_Indicated);

         Enable_Interrupts (UART, Source => Parity_Error);
         Enable_Interrupts (UART, Source => Line_Break_Detection);
         Enable_Interrupts (UART, Source => Clear_To_Send);
         Enable_Interrupts (UART, Source => Error);
         Enable_Interrupts (UART, Source => Idle_Line_Detection);
      end Start;

      ----------
      -- Stop --
      ----------

      procedure Stop
      is
         Dummy : DMA_Error_Code;
      begin
         if not Started then
            return;
         end if;

         Started := False;

         if not Is_Stream then
            Clear_All_Status (Controller, Rx_Stream);
            Pause_DMA_Reception (Drivers.UART);

         else
            Disable_Interrupts (UART, Source => Received_Data_Not_Empty);
            Clear_Status (UART, Read_Data_Register_Not_Empty);
         end if;

         Disable_Interrupts (UART, Source => Parity_Error);
         Disable_Interrupts (UART, Source => Line_Break_Detection);
         Disable_Interrupts (UART, Source => Clear_To_Send);
         Disable_Interrupts (UART, Source => Error);
         Disable_Interrupts (UART, Source => Idle_Line_Detection);

         Clear_Status (UART, Parity_Error_Indicated);
         Clear_Status (UART, Framing_Error_Indicated);
         Clear_Status (UART, USART_Noise_Error_Indicated);
         Clear_Status (UART, Overrun_Error_Indicated);
         Clear_Status (UART, Line_Break_Detection_Indicated);
         Clear_Status (UART, Clear_To_Send_Indicated);
         Clear_Status (UART, Idle_Line_Detection_Indicated);
      end Stop;

      ------------
      -- Readed --
      ------------

      procedure Readed
        (Closed : out Boolean;
         Zero   : out Positive) is
      begin
         Closed := not Started;
         Zero   := Position;
      end Readed;

   end Watchdog;

   ---------------------
   -- IRQ_DMA_Handler --
   ---------------------

   protected body IRQ_DMA_Handler is

      ----------------
      -- On_DMA_IRQ --
      ----------------

      procedure On_DMA_IRQ is

         --  Check --
         procedure Check
           (Status    : DMA_Status_Flag;
            Interrupt : DMA_Interrupt;
            Result    : UART_Status) is
         begin
            if STM32.DMA.Status (Controller, Rx_Stream, Status) then
               if Interrupt_Enabled (Controller, Rx_Stream, Interrupt) then
                  Disable_Interrupt (Controller, Rx_Stream, Interrupt);
                  Clear_Status (Controller, Rx_Stream, Status);
                  Watchdog.Release (Result);
               end if;
            end if;

         exception
            when others =>
               null;
         end Check;

      begin
         Check (Transfer_Error_Indicated, Transfer_Error_Interrupt, Err_Error);
         Check (FIFO_Error_Indicated, FIFO_Error_Interrupt, Err_Error);
         Check
           (Direct_Mode_Error_Indicated,
            Direct_Mode_Error_Interrupt,
            Err_Error);
         Check (Transfer_Complete_Indicated, Transfer_Complete_Interrupt, Ok);

      exception
         when others =>
            null;
      end On_DMA_IRQ;

   end IRQ_DMA_Handler;

   ----------------------
   -- IRQ_UART_Handler --
   ----------------------

   protected body IRQ_UART_Handler is

      -----------------
      -- On_UART_IRQ --
      -----------------

      procedure On_UART_IRQ
      is
         --  use type HAL.UInt8;

         --  Check --
         procedure Check
           (Flag      : USART_Status_Flag;
            Interrupt : USART_Interrupt) is
         begin
            if Status (UART, Flag) and then
              Interrupt_Enabled (UART, Interrupt)
            then
               Disable_Interrupts (UART, Source => Interrupt);
               Clear_Status (UART, Flag);
               Watchdog.Release (Err_Error);
            end if;

         exception
            when others =>
               null;
         end Check;

         Memory_Block : UART_Data_8b (1 .. Maximum) with Import,
           Address => Received_Address;

      begin
         Check (Parity_Error_Indicated, Parity_Error);
         Check (Framing_Error_Indicated, Error);
         Check (USART_Noise_Error_Indicated, Error);
         Check (Overrun_Error_Indicated, Error);
         Check (Line_Break_Detection_Indicated, Line_Break_Detection);
         Check (Clear_To_Send_Indicated, Clear_To_Send);

         if Status (UART, Idle_Line_Detection_Indicated) and then
           Interrupt_Enabled (UART, Idle_Line_Detection)
         then
            if Clock > Until_Time then
               Disable_Interrupts (UART, Source => Idle_Line_Detection);
               Clear_Status (UART, Idle_Line_Detection_Indicated);
               Watchdog.Release (Ok);
            else
               Clear_Status (UART, Idle_Line_Detection_Indicated);
            end if;
         end if;

         if Status (UART, Read_Data_Register_Not_Empty) and then
           Interrupt_Enabled (UART, Received_Data_Not_Empty)
         then
            Memory_Block (Position) := HAL.UInt8 (Current_Input (UART));
            Clear_Status (UART, Read_Data_Register_Not_Empty);

            if Is_Stream then
               if Position < Maximum then
                  Position := Position + 1;
               else
                  Position := 1;
               end if;
               Memory_Block (Position) := 0;

            else
               if Position = Maximum then
                  Watchdog.Release (Ok);
               else
                  Position := Position + 1;
               end if;
            end if;
         end if;
      exception
         when others =>
            null;
      end On_UART_IRQ;

   end IRQ_UART_Handler;

   -------------------------
   -- DMA_Receive_Handler --
   -------------------------

   procedure DMA_Receive_Handler
     (Port      : HAL.UART.Any_UART_Port;
      Received  : System.Address;
      Length    : Natural;
      Status    : out UART_Status;
      Timeout   : Natural := 1000;
      As_Stream : Boolean := False)
   is
      pragma Unreferenced (Port);

      Memory_Block : UART_Data_8b (1 .. Length) with Import,
        Address => Received;

   begin
      if Is_Stream then
         --  Called second time after `stream` started, close the `stream`
         Watchdog.Stop;
         Status := Ok;
         return;
      end if;

      Is_Stream := As_Stream;

      Watchdog.Start (Received, Length, Ada.Real_Time.Milliseconds (Timeout));

      if Is_Stream then
         Status := Ok;

      else
         --  Do not wait if we want the data stream
         Watchdog.Await_Event (Status);
      end if;

   exception
      when others =>
         Watchdog.Stop;
         Status := Err_Error;
   end DMA_Receive_Handler;

   -----------------------
   -- Last_Read_Handler --
   -----------------------

   procedure Last_Read_Handler
     (Closed : out Boolean;
      Zero   : out Positive) is
   begin
      Watchdog.Readed (Closed, Zero);
   end Last_Read_Handler;

   ---------------
   -- Init_UART --
   ---------------

   procedure Init_UART
   is
      use STM32.Device;

   begin
      Enable_Clock (UART);
      Enable_Clock (TX & RX);

      Configure_IO
        (TX & RX,
         (Mode           => Mode_AF,
          Resistors      => Pull_Up,
          AF             => GPIO_AF_UART5_8,
          AF_Output_Type => Push_Pull,
          AF_Speed       => Speed_50MHz));

      Disable (UART);

      Set_Baud_Rate     (UART, 9600);
      Set_Mode          (UART, Tx_Rx_Mode);
      Set_Stop_Bits     (UART, Stopbits_1);
      Set_Word_Length   (UART, Word_Length_8);
      Set_Parity        (UART, No_Parity);
      Set_Flow_Control  (UART, No_Flow_Control);

      Enable_Interrupts (UART, Error);
      Enable_Interrupts (UART, Parity_Error);
      Enable_Interrupts (UART, Received_Data_Not_Empty);

      Enable (UART);
   end Init_UART;

   --------------------
   -- Initialize_DMA --
   --------------------

   procedure Initialize_DMA is
      use STM32.Device;

      Configuration : DMA_Stream_Configuration;
   begin
      Enable_Clock (Controller);

      Reset (Controller, Rx_Stream);

      Configuration.Channel                      := Rx_Channel;
      Configuration.Direction                    := Peripheral_To_Memory;
      Configuration.Increment_Peripheral_Address := False;
      Configuration.Increment_Memory_Address     := True;
      Configuration.Peripheral_Data_Format       := Bytes;
      Configuration.Memory_Data_Format           := Bytes;
      Configuration.Operation_Mode               := Normal_Mode;
      Configuration.Priority                     := Priority_Very_High;
      Configuration.FIFO_Enabled                 := False;
      Configuration.Memory_Burst_Size            := Memory_Burst_Single;
      Configuration.Peripheral_Burst_Size        := Peripheral_Burst_Single;

      Configure (Controller, Rx_Stream, Configuration);
      Enable_DMA_Receive_Requests (Drivers.UART);
      Pause_DMA_Reception (Drivers.UART);
   end Initialize_DMA;

end Drivers;
