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

package body Bluetooth.HCI is

   -----------------------
   -- Build_HCI_Command --
   -----------------------

   function Build_HCI_Command
     (Command    : OpCode;
      Parameters : UInt8_Array)
      return UInt8_Array
   is
   begin
      return (Command_Packet_Type &
              Command &
              Parameters'Length &
              Parameters);
   end Build_HCI_Command;

   -----------------------
   -- Build_HCI_Command --
   -----------------------

   function Build_HCI_Command
     (Command    : OpCode)
      return UInt8_Array
   is
   begin
      return (Command_Packet_Type &
              Command &
              16#00#);
   end Build_HCI_Command;

end Bluetooth.HCI;
