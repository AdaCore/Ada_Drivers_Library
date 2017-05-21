----------------------------------------------------------------------------
--                                                                        --
--  Bluetooth.HCI                                                         --
--                                                                        --
--  Copyright (C) 2017, John Leimon                                       --
--                                                                        --
-- Permission to use, copy, modify, and/or distribute                     --
-- this software for any purpose with or without fee                      --
-- is hereby granted, provided that the above copyright                   --
-- notice and this permission notice appear in all copies.                --
--                                                                        --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR                        --
-- DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE                  --
-- INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY                    --
-- AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE                    --
-- FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL                    --
-- DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS                  --
-- OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF                       --
-- CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING                 --
-- OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF                 --
-- THIS SOFTWARE.                                                         --
--                                                                        --
----------------------------------------------------------------------------
with Ada.Real_Time; use Ada.Real_Time;
with HAL;           use HAL;
with Interfaces;    use Interfaces;
with STM32;         use STM32;
with STM32.GPIO;    use STM32.GPIO;
with STM32.SPI;     use STM32.SPI;

package body Bluetooth.HCI is

   ----------
   -- Poll --
   ----------
   
   procedure Poll
     (Device            : in out Tranceiver;
      Ready             :    out Boolean;
      Write_Buffer_Size :    out Byte;
      Read_Buffer_Size  :    out Byte)
   is
      SPI        : SPI_Port renames Device.Port.all;
      Ready_Byte : Byte;
      Dummy      : Byte;
   begin
      Device.Chip_Select_Pin.Clear;
      SPI.Transmit_Receive(Read_Operation, Ready_Byte);

      if Ready_Byte /= Device_Ready then
         Device.Chip_Select_Pin.Set;
         Ready             := False;
         Write_Buffer_Size := 0;
         Read_Buffer_Size  := 0;
         return;
      end if;

      SPI.Transmit_Receive(16#00#, Write_Buffer_Size);
      SPI.Transmit_Receive(16#00#, Dummy);
      SPI.Transmit_Receive(16#00#, Read_Buffer_Size);
      SPI.Transmit_Receive(16#00#, Dummy);
      Device.Chip_Select_Pin.Set;

      if Ready_Byte = Device_Ready then
         Ready := True;
      else
         Ready := False;
      end if;
   end Poll;

   ----------
   -- Read --
   ----------

   function Read
     (Device : in out Tranceiver) 
      return Byte_Array
   is
      SPI              : SPI_Port renames Device.Port.all;
      No_Data          : Byte_Array (1 .. 0);
      Read_Buffer_Size : Byte;
      Ready            : Byte;
      Dummy            : Byte;
   begin
      Device.Chip_Select_Pin.Clear;
      SPI.Transmit_Receive(Read_Operation, Ready);

      if Ready /= Device_Ready then
         Device.Chip_Select_Pin.Set;
         return No_Data;
      end if;

      SPI.Transmit_Receive(16#00#, Dummy);
      SPI.Transmit_Receive(16#00#, Dummy);
      SPI.Transmit_Receive(16#00#, Read_Buffer_Size);
      SPI.Transmit_Receive(16#00#, Dummy);

      if Read_Buffer_Size = 0 then
         Device.Chip_Select_Pin.Set;
         return No_Data;
      end if;

      declare
         Data : Byte_Array(1 .. Integer(Read_Buffer_Size));
      begin
         for Index in Integer range 1 .. Integer(Read_Buffer_Size) loop
            SPI.Transmit_Receive(16#00#, Data(Index));
         end loop;
         Device.Chip_Select_Pin.Set;

         return Data;
      end;
   end Read;

   ----------
   -- Read --
   ----------

   function Read
     (Device : in out Tranceiver;
      Count  : in out Natural)
      return Byte_Array
   is
      SPI              : SPI_Port renames Device.Port.all;
      No_Data          : Byte_Array (1 .. 0);
      Read_Buffer_Size : Byte;
      Ready            : Byte;
      Dummy            : Byte;
   begin
      Device.Chip_Select_Pin.Clear;
      SPI.Transmit_Receive(Read_Operation, Ready);

      if Ready /= Device_Ready then
         Device.Chip_Select_Pin.Set;
         return No_Data;
      end if;

      SPI.Transmit_Receive(16#00#, Dummy);
      SPI.Transmit_Receive(16#00#, Dummy);
      SPI.Transmit_Receive(16#00#, Read_Buffer_Size);
      SPI.Transmit_Receive(16#00#, Dummy);

      if Read_Buffer_Size = 0 then
         Device.Chip_Select_Pin.Set;
         return No_Data;
      end if;

      if Count > Natural(Read_Buffer_Size) then
         Count := Natural(Read_Buffer_Size);
      end if;

      declare
         Data : Byte_Array(1 .. Integer(Count));
      begin
         for Index in Natural range 1 .. Count loop
            SPI.Transmit_Receive(16#00#, Data(Index));
         end loop;
         Device.Chip_Select_Pin.Set;

         return Data;
      end;
   end Read;

   -----------
   -- Write --
   -----------

   function Write
     (Device : in out Tranceiver;
      Data   : in     Byte_Array)
      return Boolean
   is
      SPI               : SPI_Port renames Device.Port.all;
      Write_Buffer_Size : Byte;
      Ready             : Byte;
      Dummy             : Byte;
   begin
      Device.Chip_Select_Pin.Clear;
      SPI.Transmit_Receive(Write_Operation, Ready);
      
      if Ready /= Device_Ready then
         Device.Chip_Select_Pin.Set;
         return False;
      end if;

      SPI.Transmit_Receive(16#00#, Write_Buffer_Size);
      SPI.Transmit_Receive(16#00#, Dummy);
      SPI.Transmit_Receive(16#00#, Dummy);
      SPI.Transmit_Receive(16#00#, Dummy);

      if Write_Buffer_Size < Data'Length then
         Device.Chip_Select_Pin.Set;
         return False;
      end if;
      
      for Index in Integer range Data'First .. Data'Last loop
        SPI.Transmit_Receive(Data(Index), Dummy);
      end loop;
         
      Device.Chip_Select_Pin.Set;

      return True;
   end Write;

end Bluetooth.HCI;
