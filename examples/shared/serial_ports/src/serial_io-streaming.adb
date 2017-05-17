------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
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

with STM32.Device;  use STM32.Device;
with HAL;           use HAL;

package body Serial_IO.Streaming is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : out Serial_Port) is
   begin
      Serial_IO.Initialize_Peripheral (This.Device);
      This.Initialized := True;
   end Initialize;

   -----------------
   -- Initialized --
   -----------------

   function Initialized (This : Serial_Port) return Boolean is
     (This.Initialized);

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

   ----------------------
   -- Await_Send_Ready --
   ----------------------

   procedure Await_Send_Ready (This : USART) is
   begin
      loop
         exit when Tx_Ready (This);
      end loop;
   end Await_Send_Ready;

   ----------------------
   -- Set_Read_Timeout --
   ----------------------

   procedure Set_Read_Timeout
     (This : in out Serial_Port;
      Wait : Time_Span := Time_Span_Last)
   is
   begin
      This.Timeout := Wait;
   end Set_Read_Timeout;

   --------------------------
   -- Await_Data_Available --
   --------------------------

   procedure Await_Data_Available
     (This      : USART;
      Timeout   : Time_Span := Time_Span_Last;
      Timed_Out : out Boolean)
   is
      Deadline : constant Time := Clock + Timeout;
   begin
      Timed_Out := True;
      while Clock < Deadline loop
         if Rx_Ready (This) then
            Timed_Out := False;
            exit;
         end if;
      end loop;
   end Await_Data_Available;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index
     (First : Stream_Element_Offset;
      Count : Long_Integer)
      return Stream_Element_Offset
   is
   begin
      if First = Stream_Element_Offset'First and then Count = 0 then
         --  we need to return First - 1, but cannot
         raise Constraint_Error;  --  per RM
      else
         return First + Stream_Element_Offset (Count) - 1;
      end if;
   end Last_Index;

   ----------
   -- Read --
   ----------

   overriding
   procedure Read
     (This   : in out Serial_Port;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      Raw       : UInt9;
      Timed_Out : Boolean;
      Count     : Long_Integer := 0;
   begin
      Receiving : for K in Buffer'Range loop
         Await_Data_Available (This.Device.Transceiver.all, This.Timeout, Timed_Out);
         exit Receiving when Timed_Out;
         Receive (This.Device.Transceiver.all, Raw);
         Buffer (K) := Stream_Element (Raw);
         Count := Count + 1;
      end loop Receiving;
      Last := Last_Index (Buffer'First, Count);
   end Read;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write
     (This   : in out Serial_Port;
      Buffer : Ada.Streams.Stream_Element_Array)
   is
   begin
      for Next of Buffer loop
         Await_Send_Ready (This.Device.Transceiver.all);
         Transmit (This.Device.Transceiver.all, Stream_Element'Pos (Next));
      end loop;
   end Write;

end Serial_IO.Streaming;
