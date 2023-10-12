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

--  Winbond W25Q16 is 2M-byte flash memory organized into 8,192 programmable
--  pages of 256 bytes each. Up to 256 bytes can be programmed at a time.
--  Pages can be erased in groups of 16 (4KB), groups of 128 (32KB), groups
--  of 256 (64KB) or the entire chip (chip erase).

with HAL.Flash;
with HAL.GPIO;
with HAL.SPI;

package W25Q16 is

   type Flash_Memory
     (SPI : not null HAL.SPI.Any_SPI_Port;
      CS  : not null HAL.GPIO.Any_GPIO_Point) is
       limited new HAL.Flash.Flash_Memory with private;

   Chip_Size : constant := 2 ** 21;  --  2 MB

   overriding function Size (This : Flash_Memory) return Natural
     is (Chip_Size);

   overriding function Is_Busy (This : Flash_Memory) return Boolean;

   overriding function Erasable_Region
     (This   : in out Flash_Memory;
      Region : HAL.Flash.Memory_Region)
       return HAL.Flash.Memory_Region;

   overriding procedure Erase
     (This    : in out Flash_Memory;
      Region  : HAL.Flash.Memory_Region;
      Success : out Boolean);

   procedure Check_JEDEC_ID
     (This : in out Flash_Memory;
      Ok   : out Boolean);

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

private

   type Flash_Memory
     (SPI : not null HAL.SPI.Any_SPI_Port;
      CS  : not null HAL.GPIO.Any_GPIO_Point)
   is limited new HAL.Flash.Flash_Memory with null record;

end W25Q16;
