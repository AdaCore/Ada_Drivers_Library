------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
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

with Interfaces;
with HAL.Block_Drivers;
with File_IO;

package Filesystem.MBR is

   type Master_Boot_Record is private;
   type Extended_Boot_Record is private;

   type Partition_Number is range 1 .. 4;

   type Partition_Type is new Interfaces.Unsigned_8;

   function Read
     (Controller  : HAL.Block_Drivers.Any_Block_Driver;
      MBR         : out Master_Boot_Record) return File_IO.Status_Code;

   function Active  (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Boolean;
   function Valid   (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Boolean;
   function Get_Type (MBR : Master_Boot_Record;
                      P   : Partition_Number) return Partition_Type;
   function LBA     (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Block_Number;
   function Sectors (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Interfaces.Unsigned_32;

   function Is_Extended
     (MBR : Master_Boot_Record;
      P   : Partition_Number) return Boolean;

   function Read_Extended
     (Controller  : HAL.Block_Drivers.Any_Block_Driver;
      MBR         : Master_Boot_Record;
      P           : Partition_Number;
      EBR         : out Extended_Boot_Record) return File_IO.Status_Code;

   function Get_Type (EBR : Extended_Boot_Record) return Partition_Type;
   function LBA     (EBR : Extended_Boot_Record) return Block_Number;
   function Sectors (EBR : Extended_Boot_Record) return Interfaces.Unsigned_32;

   function Has_Next (EBR : Extended_Boot_Record) return Boolean;

   function Read_Next
     (Controller : HAL.Block_Drivers.Any_Block_Driver;
      EBR        : in out Extended_Boot_Record) return File_IO.Status_Code;

private

   type CHS_Address is record
      Head     : Interfaces.Unsigned_8;
      Sector   : Interfaces.Unsigned_8;
      Cylinder : Interfaces.Unsigned_8;
   end record with Size => 24;

   for CHS_Address use record
      Head     at 0 range 0 .. 7;
      Sector   at 1 range 0 .. 7;
      Cylinder at 2 range 0 .. 7;
   end record;

   type Partition_Entry is record
      Status       : Interfaces.Unsigned_8;
      First_Sector : CHS_Address;
      P_Type       : Partition_Type;
      Last_Sector  : CHS_Address;
      LBA          : Interfaces.Unsigned_32;
      Num_Sectors  : Interfaces.Unsigned_32;
   end record with Size => 16 * 8;

   for Partition_Entry use record
      Status       at 0 range 0 .. 7;
      First_Sector at 1 range 0 .. 23;
      P_Type       at 4 range 0 .. 7;
      Last_Sector  at 5 range 0 .. 23;
      LBA          at 8 range 0 .. 31;
      Num_Sectors  at 12 range 0 .. 31;
   end record;

   Zeroed_Entry : constant Partition_Entry :=
                    (Status       => 0,
                     First_Sector => (0, 0, 0),
                     P_Type       => 0,
                     Last_Sector  => (0, 0, 0),
                     LBA          => 0,
                     Num_Sectors  => 0);

   type Partition_Array is array (Partition_Number) of Partition_Entry;

   type Master_Boot_Record is record
      P_Entries : Partition_Array;
      Signature : Interfaces.Unsigned_16;
   end record with Size => 512 * 8;

   for Master_Boot_Record use record
      P_Entries at 16#1BE# range 0 .. 4 * 16 * 8 - 1;
      Signature at 16#1FE# range 0 .. 15;
   end record;

   type Extended_Boot_Record is new Master_Boot_Record;

   function Is_Extended
     (MBR : Master_Boot_Record;
      P   : Partition_Number) return Boolean
   is (Get_Type (MBR, P) = 16#0F#);

end Filesystem.MBR;
