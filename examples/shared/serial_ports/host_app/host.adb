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

--  This file contains the source code for an interactive program to be run on
--  a host computer, to communicate via serial port with a program running on
--  a target board. It uses streams to send and receive strings to/from the
--  target board program.

--  The program can be connected to the target board with a serial cable that
--  presents a serial port to the host operating system. The "README.md" file
--  associated with this project describes such a cable.

--  You can change the COM port number, or even get it from the command line
--  as an argument, but it must match what the host OS sees from the USB-COM
--  cable. This program has been run successfully on Windows 7 but should run
--  on Linux as well.

--  When running it, enter a string at the prompt (">") or just hit carriage
--  return if you are ready to quit. If you do enter a string, it will be sent
--  to the target board, along with the bounds. The program running on the
--  target echos it back so this host app will show that response from the
--  board.

with GNAT.IO;                    use GNAT.IO;
with GNAT.Serial_Communications; use GNAT.Serial_Communications;

procedure Host is
   COM  : aliased Serial_Port;
   COM3 : constant Port_Name := Name (3);

   Outgoing : String (1 .. 1024); -- arbitrary
   Last     : Natural;
begin
   COM.Open (COM3);
   COM.Set (Rate => B115200, Block => False);

   loop
      Put ("> ");
      Get_Line (Outgoing, Last);
      exit when Last = Outgoing'First - 1;

      Put_Line ("Sending: '" & Outgoing (1 .. Last) & "'");

      String'Output (COM'Access, Outgoing (1 .. Last));

      declare
         Incoming : constant String := String'Input (COM'Access);
      begin
         Put_Line ("From board: " & Incoming);
      end;
   end loop;

   COM.Close;
end Host;
