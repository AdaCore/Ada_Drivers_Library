------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2026, AdaCore                        --
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

--  Embedded Flash memory for STM32F4xx with two banks.
--
--  Embedded STM32 Flash memory has two 1M-byte banks. Each bank organized into
--  12 sectors (4 x 16, 1 x 64, 7 x 128 kilo bytes).

with HAL.Flash;
with System;

package STM32.Flash is
   pragma Preelaborate;

   subtype Flash_Bank is Natural range 1 .. 2;

   type Flash_Memory (Bank : Flash_Bank) is
       limited new HAL.Flash.Flash_Memory with private;

   Bank_Size   : constant := 2 ** 20;  --  1 MB per bank
   First_Bank  : constant System.Address := System'To_Address (16#0800_0000#);
   Second_Bank : constant System.Address := System'To_Address (16#0810_0000#);

   overriding function Size (This : Flash_Memory) return Natural
     is (Bank_Size);

   overriding function Is_Busy (This : Flash_Memory) return Boolean;

   overriding function Erasable_Region
     (This   : in out Flash_Memory;
      Region : HAL.Flash.Memory_Region)
       return HAL.Flash.Memory_Region;

   use type HAL.Flash.Memory_Region;

   overriding procedure Erase
     (This    : in out Flash_Memory;
      Region  : HAL.Flash.Memory_Region;
      Success : out Boolean)
        with Pre => Region = Erasable_Region (This, Region);

   overriding procedure Read
     (This    : in out Flash_Memory;
      Offset  : Natural;
      Data    : out HAL.UInt8_Array;
      Success : out Boolean);

   overriding procedure Write
     (This    : in out Flash_Memory;
      Offset  : Natural;
      Data    : HAL.UInt8_Array;
      Success : out Boolean);
   --  The chip is able to write up to 256 bytes in one go if they are fitted
   --  in aligned 256 bytes blocks. Otherwise, Write is split in several
   --  commands and execution is blocked until all except the last one is
   --  finished.

   procedure Lock;
   procedure Unlock;
   procedure Programming (By_Word : Boolean);

private

   type Flash_Memory (Bank : Flash_Bank)
     is limited new HAL.Flash.Flash_Memory with null record;

end STM32.Flash;
