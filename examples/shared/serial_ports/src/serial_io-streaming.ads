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

--  A demonstration of a higher-level USART interface using streams. In
--  particular, the serial port is presented as a stream type, so these ports
--  can be used with stream attributes to send values or arbitrary types, not
--  just characters or Strings.

--  Polling is used within the procedures to determine when characters are sent
--  and received.

with Ada.Streams;
with Ada.Real_Time; use Ada.Real_Time;

package Serial_IO.Streaming is
   pragma Elaborate_Body;

   type Serial_Port (Device : not null access Peripheral_Descriptor) is
     new Ada.Streams.Root_Stream_Type with private;

   procedure Initialize (This : out Serial_Port)
     with Post => Initialized (This);

   function Initialized (This : Serial_Port) return Boolean with Inline;

   procedure Configure
     (This      : in out Serial_Port;
      Baud_Rate : Baud_Rates;
      Parity    : Parities     := No_Parity;
      Data_Bits : Word_Lengths := Word_Length_8;
      End_Bits  : Stop_Bits    := Stopbits_1;
      Control   : Flow_Control := No_Flow_Control)
     with
       Pre => Initialized (This);

   procedure Set_Read_Timeout
     (This : in out Serial_Port;
      Wait : Time_Span := Time_Span_Last);
   --  Stream attributes that call Read (below) can either wait indefinitely or
   --  can be set to return any current values received after a given interval.
   --  If the default value of Time_Span_Last is taken on a call, the effect is
   --  essentially to wait forever, i.e., blocking. That is also the effect if
   --  this routine is never called.

   overriding
   procedure Read
     (This   : in out Serial_Port;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding
   procedure Write
     (This   : in out Serial_Port;
      Buffer : Ada.Streams.Stream_Element_Array);

private

   type Serial_Port (Device : access Peripheral_Descriptor) is
     new Ada.Streams.Root_Stream_Type with
     record
         Initialized : Boolean := False;
         Timeout     : Time_Span := Time_Span_Last;
     end record;

   procedure Await_Send_Ready (This : USART) with Inline;

   procedure Await_Data_Available
     (This      : USART;
      Timeout   : Time_Span := Time_Span_Last;
      Timed_Out : out Boolean)
     with Inline;

   use Ada.Streams;

   function Last_Index
     (First : Stream_Element_Offset;
      Count : Long_Integer)
      return Stream_Element_Offset
   with Inline;

end Serial_IO.Streaming;
