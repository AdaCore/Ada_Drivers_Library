----------------------------------------------------------------------------
--                                                                        --
--  Bluetooth Specification                                               --
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
with HAL; use HAL;

package Bluetooth is

   Write_Operation : constant := 16#0A#;
   Read_Operation  : constant := 16#0B#;
   Device_Ready    : constant := 16#02#;

   subtype OpCode  is UInt8_Array (1 .. 2);
   subtype Handle  is UInt8_Array (1 .. 2);
   subtype Address is UInt8_Array (1 .. 6);

   type UUID_16  is new UInt8_Array (1 .. 2);
   type UUID_128 is new UInt8_Array (1 .. 16);

   type Service_Type is
     (Primary,
      Secondary);

   for Service_Type use
     (Primary   => 16#01#,
      Secondary => 16#02#);

end Bluetooth;
