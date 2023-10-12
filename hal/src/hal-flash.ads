------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2023, AdaCore                     --
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

package HAL.Flash is
   pragma Preelaborate;

   type Flash_Memory is limited interface;
   --  Interface representing a flash memory device

   type Any_Flash_Memory is access all Flash_Memory'Class;

   function Size (This : Flash_Memory) return Natural is abstract;
   --  Return the size of the flash memory in bytes

   function Is_Busy (This : Flash_Memory) return Boolean is abstract;
   --  Return True if an erase or write operation is in progress.
   --  This function can be used to check if the device is busy before
   --  invoking erase, write or read operations.

   type Memory_Region is record
      From : Natural;
      To   : Natural;
   end record;
   --  Type representing a region in memory for erase operation

   function Erasable_Region
    (This   : in out Flash_Memory;
     Region : Memory_Region) return Memory_Region is abstract;
   --  Calculate the minimum enclosing region that could be erased

   procedure Erase
     (This    : in out Flash_Memory;
      Region  : Memory_Region;
      Success : out Boolean) is abstract
       with Pre'Class => Erasable_Region (This, Region) = Region;
   --  Erase a region in the flash memory.
   --  This subroutine transitions the memory into a busy state. Do not invoke
   --  when the device is already busy. If called when the device is busy, the
   --  call will fail without performing the operation.

   procedure Write
     (This    : in out Flash_Memory;
      Offset  : Natural;
      Data    : in UInt8_Array;
      Success : out Boolean) is null;
   --  Write Data to the flash memory at given Offset.
   --  Specific memory models may have limitations depending on Data size and
   --  Offset that affect the execution time of this subroutine. This
   --  subroutine transitions the memory into a busy state. Do not invoke when
   --  the device is already busy. If called when the device is busy, the call
   --  will fail without performing the operation.

   procedure Read
     (This    : in out Flash_Memory;
      Offset  : Natural;
      Data    : out UInt8_Array;
      Success : out Boolean) is abstract;
   --  Read bytes from the flash memory into Data starting at given Offset.
   --  Do not invoke when the device is busy. If called when the device is
   --  busy, the call will fail without performing the operation.

end HAL.Flash;
