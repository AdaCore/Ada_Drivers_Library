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

--  **************************************************************************
--  NOTE: THIS PROGRAM REQUIRES THE RAVENSCAR-FULL-* RUNTIME LIBRARIES.
--  Set the scenario variable accordingly.
--  **************************************************************************

--  A demonstration of a higher-level USART interface using streams. In
--  particular, the serial port is presented as a stream type, so these ports
--  can be used with stream attributes to send values of arbitrary types, not
--  just characters or Strings. For this demonstration, however, we simply
--  read an incoming string from the stream (the serial port) and echo it back,
--  surrounding it with single quotes.

--  HOST COMPUTER SIDE:

--  The incoming strings are intended to come from another program, presumably
--  running on the host computer, connected to the target board with a
--  serial cable that presents a serial port to the host operating system. The
--  "README.md" file associated with this project describes such a cable. A
--  sample host-side program is described below.

--  Note that, because it uses the stream attributes String'Output and
--  String'Input, which write and read the bounds as well as the characters,
--  you will need to use a program on the host that uses streams to send
--  and receive these String values. Here is a sample interactive program,
--  hardcoded arbitrarily to use COM3 on Windows. Note that the source code for
--  this program is included in the root of this project, not in the source dir
--  for the project because it is not intended to be run on the target board.

--  with GNAT.IO;                    use GNAT.IO;
--  with GNAT.Serial_Communications; use GNAT.Serial_Communications;
--
--  procedure Host is
--     COM  : aliased Serial_Port;
--     COM3 : constant Port_Name := Name (3);
--
--     Outgoing : String (1 .. 1024); -- arbitrary
--     Last     : Natural;
--  begin
--     COM.Open (COM3);
--     COM.Set (Rate => B115200, Block => False);
--
--     loop
--        Put ("> ");
--        Get_Line (Outgoing, Last);
--        exit when Last = Outgoing'First - 1;
--
--        Put_Line ("Sending: '" & Outgoing (1 .. Last) & "'");
--        String'Output (COM'Access, Outgoing (1 .. Last));
--
--        declare
--           Incoming : constant String := String'Input (COM'Access);
--        begin
--           Put_Line ("From board: " & Incoming);
--        end;
--     end loop;
--
--     COM.Close;
--  end Host;

--  You can change the COM port number, or even get it from the command line
--  as an argument, but it must match what the host OS sees from the USB-COM
--  cable.

--  When running it, enter a string at the prompt (">") or just hit carriage
--  return if you are ready to quit. If you enter a string, it will be sent to
--  the target board, along with the bounds. The program (in this file) running
--  on the target, echos it back so the host app will show that response from
--  the board.

--  TARGET BOARD SIDE:

--  This file declares the main procedure for the program running on the target
--  board. It simply echos the incoming strings, forever.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with Peripherals_Streaming; use Peripherals_Streaming;
with Serial_IO.Streaming;   use Serial_IO.Streaming;

procedure Demo_Serial_Port_Streaming is
begin
   Initialize (COM);
   Configure (COM, Baud_Rate => 115_200);

   loop
      declare
         Incoming : constant String := String'Input (COM'Access);
      begin
         String'Output (COM'Access, "'" & Incoming & "'");
      end;
   end loop;
end Demo_Serial_Port_Streaming;
