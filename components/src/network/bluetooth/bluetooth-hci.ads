----------------------------------------------------------------------------
--                                                                        --
--  Bluetooth.HCI Specification                                           --
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
with HAL;        use HAL;
with Interfaces; use Interfaces;
with STM32;      use STM32;
with STM32.GPIO; use STM32.GPIO;
with STM32.SPI;  use STM32.SPI;

package Bluetooth.HCI is

   type Any_SPI_Port is access all SPI_Port;

   type Tranceiver is tagged limited record
      Port            : not null Any_SPI_Port;
      SPI_AF          :          GPIO_Alternate_Function;
      Chip_Select_Pin :          GPIO_Point;
      SCK_Pin         :          GPIO_Point;
      MISO_Pin        :          GPIO_Point;
      MOSI_Pin        :          GPIO_Point;
      Reset_Pin       :          GPIO_Point;
   end record;

   Command_Packet_Type : constant Byte := 16#01#;
   Event_Packet_Type   : constant Byte := 16#04#;

   type Event is
     (Event_Disconn_Complete,
      Event_Encrypt_Change,
      Event_Read_Remote_Version_Complete,
      Event_Cmd_Complete,
      Event_Cmd_Status,
      Event_Hardware_Error,
      Event_Num_Comp_Pkts,
      Event_Data_Buffer_Overflow,
      Event_Encryption_Key_Refresh_Complete,
      Event_LE);

   for Event use
     (Event_Disconn_Complete                => 16#05#,
      Event_Encrypt_Change                  => 16#08#,
      Event_Read_Remote_Version_Complete    => 16#0C#,
      Event_Cmd_Complete                    => 16#0E#,
      Event_Cmd_Status                      => 16#0F#,
      Event_Hardware_Error                  => 16#10#,
      Event_Num_Comp_Pkts                   => 16#13#,
      Event_Data_Buffer_Overflow            => 16#1A#,
      Event_Encryption_Key_Refresh_Complete => 16#30#,
      Event_LE                              => 16#3E#);

   procedure Poll
     (Device            : in out Tranceiver;
      Ready             :    out Boolean;
      Write_Buffer_Size :    out Byte;
      Read_Buffer_Size  :    out Byte);
   -- Sends a SPI header to the BlueNRG-MS. If Ready is true then
   -- Write_Buffer_Size and Read_Buffer_Size are valid.

   function Read
     (Device : in out Tranceiver)
      return Byte_Array;
   -- Reads all bytes that are available in the read buffer

   function Read
      (Device : in out Tranceiver;
       Count  : in out Natural)
       return Byte_Array;
   -- Reads a maximum of Count bytes from the read buffer

   function Write
     (Device : in out Tranceiver;
      Data   : in     Byte_Array)
      return Boolean;
   -- Returns true if data was successfully written

end Bluetooth.HCI;
