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

--  The main program uses the DMA controller to send a block of characters
--  to a USART. The characters are sent once per execution of the program.
--  Successful execution will be indicated by a slowly blinking green LED, in
--  addition to the characters appearing on a display conected to the USART.
--  Errors are indicated by flashing colors at higher rates (see the source
--  code). Note that all the LEDs flash before the characters are sent, as a
--  confirmation of overall execution.

--  The file declares the main procedure for the demonstration.

with STM32;                  use STM32;
with STM32.DMA;              use STM32.DMA;
with STM32.GPIO;             use STM32.GPIO;
with STM32.USARTs;           use STM32.USARTs;

with STM32.Device;           use STM32.Device;
with STM32.Board;            use STM32.Board;

with Ada.Real_Time;          use Ada.Real_Time;

with Peripherals;            use Peripherals;
with STM32F4_DMA_Interrupts; use STM32F4_DMA_Interrupts;

procedure Demo_USART_DMA is

   type Data is array (1 .. 26) of Character; -- arbitrary size and component
   for Data'Component_Size use 8;

   Bytes_To_Transfer : constant := Data'Length;

   Source_Block  : constant Data := "abcdefghijklmnopqrstuvwxyz";

   Event_Kind : DMA_Interrupt;


   procedure Initialize_GPIO_Port_Pins is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (IO_Port);

      Configuration.Mode := Mode_AF;
      Configuration.Speed := Speed_50MHz;
      Configuration.Output_Type := Push_Pull;
      Configuration.Resistors := Pull_Up;

      Configure_IO
        (Port   => IO_Port,
         Pins   => Rx_Pin & Tx_Pin,
         Config => Configuration);

      Configure_Alternate_Function
        (Port => IO_Port,
         Pins => Rx_Pin & Tx_Pin,
         AF   => Transceiver_AF);
   end Initialize_GPIO_Port_Pins;


   procedure Initialize_USART is
   begin
      Enable_Clock (Transceiver);

      Enable (Transceiver);

      Set_Baud_Rate    (Transceiver, 115_200);
      Set_Mode         (Transceiver, Tx_Mode);
      Set_Stop_Bits    (Transceiver, Stopbits_1);
      Set_Word_Length  (Transceiver, Word_Length_8);
      Set_Parity       (Transceiver, No_Parity);
      Set_Flow_Control (Transceiver, No_Flow_Control);
   end Initialize_USART;


   procedure Initialize_DMA is
      Configuration : DMA_Stream_Configuration;
   begin
      Enable_Clock (Controller);

      Configuration.Channel                      := Tx_Channel;
      Configuration.Direction                    := Memory_To_Peripheral;
      Configuration.Increment_Peripheral_Address := False;
      Configuration.Increment_Memory_Address     := True;
      Configuration.Peripheral_Data_Format       := Bytes;
      Configuration.Memory_Data_Format           := Bytes;
      Configuration.Operation_Mode               := Normal_Mode;
      Configuration.Priority                     := Priority_Very_High;
      Configuration.FIFO_Enabled                 := True;
      Configuration.FIFO_Threshold               := FIFO_Threshold_Full_Configuration;
      Configuration.Memory_Burst_Size            := Memory_Burst_Inc4;
      Configuration.Peripheral_Burst_Size        := Peripheral_Burst_Inc4;

      Configure (Controller, Tx_Stream, Configuration);
      --  note the controller is disabled by the call to Configure
   end Initialize_DMA;


   procedure Blink_LEDs is
   begin
      for K in 1 .. 3 loop
         All_LEDs_On;
         delay until Clock + Milliseconds (200);
         All_LEDs_Off;
         delay until Clock + Milliseconds (200);
      end loop;
   end Blink_LEDs;

begin
   Initialize_LEDs;

   Blink_LEDs; --  just to signal that we are indeed running...

   Initialize_GPIO_Port_Pins;
   Initialize_USART;
   Initialize_DMA;

   Enable (Transceiver);

   Start_Transfer_With_Interrupts
     (Controller,
      Tx_Stream,
      Source      => Source_Block'Address,
      Destination => Data_Register_Address (Transceiver),
      Data_Count  => Bytes_To_Transfer);
   --  also enables the stream

-- TODO: clear the flags esp the overrun flag   ???

   Enable_DMA_Transmit_Requests (Transceiver);

   Handler.Await_Event (Event_Kind);

   declare
      Status_LED : User_LED;
      Interval   : Integer := 200;  -- milliseconds between blinks, arbitrary
   begin
      case Event_Kind is
         when Direct_Mode_Error_Interrupt      => Status_LED := Blue;
         when FIFO_Error_Interrupt             => Status_LED := Orange;
         when Transfer_Error_Interrupt         => Status_LED := Red;
         when Half_Transfer_Complete_Interrupt => Status_LED := Green;
         when Transfer_Complete_Interrupt      => Status_LED := Green;
            Interval := 800;
            --  also change the blink rate, to distinguish from the HTCI
      end case;
      loop
         Turn_On (Status_LED);
         delay until Clock + Milliseconds (Interval);
         Turn_Off (Status_LED);
         delay until Clock + Milliseconds (Interval);
      end loop;
   end;
end Demo_USART_DMA;
