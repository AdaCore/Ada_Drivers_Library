------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

--  Turns on the LED and waits for the SD card. Then, it mounts the first FAT
--  partition as '/card/', opens the `/file.txt` file and reads 512 bytes from
--  it. If everything is okay, the program blinks the green LED slowly
--  at a rate of 1 Hz. Otherwise, it blinks the LED faster at 5 Hz.

pragma Ada_2022;

with Ada.Real_Time;

with File_IO;
with HAL;
with SDCard;
with STM32.Board;

procedure Main is
   use type Ada.Real_Time.Time;
   use all type File_IO.Status_Code;

   Next : Ada.Real_Time.Time := Ada.Real_Time.Clock;

   SD_Card renames STM32.Board.SDCard_Device;
   --  Typeless renaming works since Ada 2022

   Success : Boolean;
begin
   STM32.Board.Initialize_LEDs;
   SD_Card.Initialize;
   STM32.Board.Green_LED.Set;

   --  Wait for card
   while not SD_Card.Card_Present loop
      Next := Next + Ada.Real_Time.Milliseconds (200);
      delay until Next;
   end loop;

   Success := File_IO.Mount_Drive ("card", SD_Card'Unchecked_Access) = OK;
   --  Mounting the SD card as '/card/'

   declare
      use type File_IO.File_Size;

      FD   : File_IO.File_Descriptor;
      Data : HAL.UInt8_Array (1 .. 512);
      Last : File_IO.File_Size;
   begin
      if Success then
         Success := File_IO.Open
           (FD, "/card/file.txt", File_IO.Read_Only) = OK;
      end if;

      if Success then
         Last := File_IO.Read (FD, Data'Address, Data'Length);

         Success := Last = Data'Length;
      end if;

      loop
         STM32.Board.Toggle (STM32.Board.Green_LED);

         Next := Next + Ada.Real_Time.Milliseconds
           (if Success then 500 else 100);

         delay until Next;
      end loop;
   end;
end Main;
