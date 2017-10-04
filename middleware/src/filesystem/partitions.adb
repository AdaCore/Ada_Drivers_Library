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

package body Partitions is

   type Partition_Entry_Block_Mapping (Kind : Boolean := True) is record
      case Kind is
         when True => Data : Block (1 .. 16);
         when False => P_Entry : Partition_Entry;
      end case;
   end record with Pack, Unchecked_Union, Size => 16 * 8;

   procedure Read_Entry_In_MBR (MBR     : Block;
                                Index   : Integer;
                                P_Entry : out Partition_Entry)
     with Pre => Index <= 3;

   function Number_Of_Logical_Partitions (Disk        : not null Any_Block_Driver;
                                          EBR_Address : Logical_Block_Address)
                                          return Natural;

   function Get_Logical_Partition_Entry (Disk         : not null Any_Block_Driver;
                                         EBR_Address  : Logical_Block_Address;
                                         Entry_Number : Positive;
                                         Entry_Cnt    : in out Natural;
                                         P_Entry      : out Partition_Entry)
                                         return Status_Code;

   -----------------------
   -- Read_Entry_In_MBR --
   -----------------------

   procedure Read_Entry_In_MBR (MBR     : Block;
                                Index   : Integer;
                                P_Entry : out Partition_Entry)
   is
      Entry_Block_Conv : Partition_Entry_Block_Mapping;
      First            : constant Integer := MBR'First + 446 + Index * 16;
      Last             : constant Integer := First + 15;
   begin
      if MBR'Length /= 512 then
         P_Entry.Status := 16#FF#;
      end if;

      Entry_Block_Conv.Data := MBR (First .. Last);
      P_Entry := Entry_Block_Conv.P_Entry;
   end Read_Entry_In_MBR;

   ----------------------------------
   -- Number_Of_Logical_Partitions --
   ----------------------------------

   function Number_Of_Logical_Partitions (Disk        : not null Any_Block_Driver;
                                          EBR_Address : Logical_Block_Address)
                                          return Natural
   is
      EBR       : Block (0 .. 511);
      Entry_Cnt : Natural := 0;
      Address   : Logical_Block_Address := EBR_Address;
      P_Entry   : Partition_Entry;
   begin
      loop
         if not Disk.Read (UInt64 (Address), EBR) or else EBR (510 .. 511) /= (16#55#, 16#AA#) then
            return Entry_Cnt;
         end if;

         Read_Entry_In_MBR (EBR, 0, P_Entry);

         if Is_Valid (P_Entry) then
            Entry_Cnt := Entry_Cnt + 1;
         end if;

         Read_Entry_In_MBR (EBR, 1, P_Entry);

         exit when P_Entry.First_Sector_LBA = 0;
         Address := EBR_Address + P_Entry.First_Sector_LBA;
      end loop;

      return Entry_Cnt;
   end Number_Of_Logical_Partitions;

   ---------------------------------
   -- Get_Logical_Partition_Entry --
   ---------------------------------

   function Get_Logical_Partition_Entry (Disk         : not null Any_Block_Driver;
                                         EBR_Address  : Logical_Block_Address;
                                         Entry_Number : Positive;
                                         Entry_Cnt    : in out Natural;
                                         P_Entry      : out Partition_Entry)
                                         return Status_Code
   is
      EBR     : Block (0 .. 511);
      Address : Logical_Block_Address := EBR_Address;
   begin
      loop
         if not Disk.Read (UInt64 (Address), EBR)
           or else
             EBR (510 .. 511) /= (16#55#, 16#AA#)
         then
            return Invalid_Parition;
         end if;

         Read_Entry_In_MBR (EBR, 0, P_Entry);

         if Is_Valid (P_Entry) then
            Entry_Cnt := Entry_Cnt + 1;

            if Entry_Cnt = Entry_Number then
               return Status_Ok;
            end if;
         end if;

         Read_Entry_In_MBR (EBR, 1, P_Entry);

         exit when P_Entry.First_Sector_LBA = 0;
         Address := EBR_Address + P_Entry.First_Sector_LBA;
      end loop;

      return Invalid_Parition;
   end Get_Logical_Partition_Entry;

   -------------------------
   -- Get_Partition_Entry --
   -------------------------

   function Get_Partition_Entry (Disk         : not null Any_Block_Driver;
                                 Entry_Number : Positive;
                                 P_Entry      : out Partition_Entry)
                                 return Status_Code
   is
      MBR         : Block (0 .. 511);
      Entry_Cnt   : Natural := 0;
      EBR_Address : Logical_Block_Address;
   begin
      if not Disk.Read (0, MBR) then
         return Disk_Error;
      end if;

      if MBR (510 .. 511) /= (16#55#, 16#AA#) then
         return Disk_Error;
      end if;

      for P_Index in 0 .. 3 loop
         Read_Entry_In_MBR (MBR, P_Index, P_Entry);

         if Is_Valid (P_Entry) then
            Entry_Cnt := Entry_Cnt + 1;

            --  Is is the entry we are looking for?
            if Entry_Cnt = Entry_Number then
               return Status_Ok;
            elsif P_Entry.Kind = Extended_Parition then

               EBR_Address := P_Entry.First_Sector_LBA;

               --  Look in the list of logical partitions
               if Get_Logical_Partition_Entry (Disk,
                                               EBR_Address,
                                               Entry_Number,
                                               Entry_Cnt,
                                               P_Entry) = Status_Ok
               then
                  return Status_Ok;
               end if;
            end if;
         end if;
      end loop;

      return Invalid_Parition;
   end Get_Partition_Entry;

   --------------------------
   -- Number_Of_Partitions --
   --------------------------

   function Number_Of_Partitions (Disk : Any_Block_Driver) return Natural is
      MBR       : Block (0 .. 511);
      Entry_Cnt : Natural := 0;
      P_Entry   : Partition_Entry;
   begin
      if not Disk.Read (0, MBR) then
         return 0;
      end if;

      if MBR (510 .. 511) /= (16#55#, 16#AA#) then
         return 0;
      end if;

      for P_Index in 0 .. 3 loop
         Read_Entry_In_MBR (MBR, P_Index, P_Entry);

         if Is_Valid (P_Entry) then
            Entry_Cnt := Entry_Cnt + 1;
         end if;

         if P_Entry.Kind = Extended_Parition then
            Entry_Cnt := Entry_Cnt +
              Number_Of_Logical_Partitions (Disk, P_Entry.First_Sector_LBA);
         end if;
      end loop;

      return Entry_Cnt;
   end Number_Of_Partitions;

end Partitions;
