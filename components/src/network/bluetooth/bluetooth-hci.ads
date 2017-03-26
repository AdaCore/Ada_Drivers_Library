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
with HAL.GPIO;   use HAL.GPIO;

package Bluetooth.HCI is

   Command_Packet_Type : constant UInt8 := 16#01#;
   Event_Packet_Type   : constant UInt8 := 16#04#;

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

   function Build_HCI_Command
     (Command    : OpCode;
      Parameters : UInt8_Array)
      return UInt8_Array
     with
      Pre => Parameters'Length < 256;
   --  Returns a Bluetooth HCI packet using the OpCode
   --  and parameters.

   function Build_HCI_Command
     (Command    : OpCode)
      return UInt8_Array;
   --  Returns a Bluetooth HCI packet using the OpCode
   --  and no parameters.

end Bluetooth.HCI;
