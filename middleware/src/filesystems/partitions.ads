------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

with HAL; use HAL;
with HAL.Block_Drivers; use HAL.Block_Drivers;

package Partitions is

   type Partition_Kind is new UInt8;

   Empty_Partition       : constant Partition_Kind := 16#00#;
   Fat12_Parition        : constant Partition_Kind := 16#01#;
   Fat16_Parition        : constant Partition_Kind := 16#04#;
   Extended_Parition     : constant Partition_Kind := 16#05#;
   Fat16B_Parition       : constant Partition_Kind := 16#06#;
   NTFS_Partition        : constant Partition_Kind := 16#07#;
   Fat32_CHS_Parition    : constant Partition_Kind := 16#0B#;
   Fat32_LBA_Parition    : constant Partition_Kind := 16#0C#;
   Fat16B_LBA_Parition   : constant Partition_Kind := 16#0E#;
   Extended_LBA_Parition : constant Partition_Kind := 16#0F#;
   Linux_Swap_Partition  : constant Partition_Kind := 16#82#;
   Linux_Partition       : constant Partition_Kind := 16#83#;

   subtype Logical_Block_Address is UInt32;

   type CHS_Address is record
      C : UInt10;
      H : UInt8;
      S : UInt6;
   end record with Pack, Size => 24;

   type Partition_Entry is record
      Status            : UInt8;
      First_Sector_CHS  : CHS_Address;
      Kind              : Partition_Kind;
      Last_Sector_CHS   : CHS_Address;
      First_Sector_LBA  : Logical_Block_Address;
      Number_Of_Sectors : UInt32;
   end record with Pack, Size => 16 * 8;

   type Status_Code is (Status_Ok, Disk_Error, Invalid_Parition);

   function Get_Partition_Entry (Disk         : not null Any_Block_Driver;
                                 Entry_Number : Positive;
                                 P_Entry      : out Partition_Entry)
                                 return Status_Code;

   function Number_Of_Partitions (Disk : Any_Block_Driver) return Natural;

   function Is_Valid (P_Entry : Partition_Entry) return Boolean is
     (P_Entry.Status = 16#00# or else P_Entry.Status = 16#80#);
end Partitions;
