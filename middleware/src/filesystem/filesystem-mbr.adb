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

with Interfaces; use Interfaces;

package body Filesystem.MBR is

   ----------
   -- Read --
   ----------

   function Read
     (Controller  : HAL.Block_Drivers.Any_Block_Driver;
      MBR         : out Master_Boot_Record)
      return File_IO.Status_Code
   is
      Tmp  : aliased Master_Boot_Record;
      Data : aliased HAL.UInt8_Array (1 .. 512) with Address => Tmp'Address;
   begin
      --  Let's read the MBR: located in the first block
      if not Controller.Read (0, Data) then
         return File_IO.Disk_Error;
      end if;

      MBR := Tmp;

      if MBR.Signature /= 16#AA55# then
         return File_IO.No_MBR_Found;
      end if;

      return File_IO.OK;
   end Read;

   -------------------
   -- Read_Extended --
   -------------------

   function Read_Extended
     (Controller  : HAL.Block_Drivers.Any_Block_Driver;
      MBR         : Master_Boot_Record;
      P           : Partition_Number;
      EBR         : out Extended_Boot_Record)
      return File_IO.Status_Code
   is
      BA : constant Block_Number := LBA (MBR, P);
      Tmp  : aliased Extended_Boot_Record;
      Data : aliased HAL.UInt8_Array (1 .. 512) with Address => Tmp'Address;
   begin
      --  Let's read the MBR: located in the first block
      if not Controller.Read (HAL.UInt64 (BA), Data) then
         return File_IO.Disk_Error;
      end if;

      EBR := Tmp;

      if EBR.Signature /= 16#AA55# then
         return File_IO.No_MBR_Found;
      end if;

      return File_IO.OK;
   end Read_Extended;

   ------------
   -- Active --
   ------------

   function Active  (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Boolean
   is (MBR.P_Entries (P).Status = 16#80#);

   -----------
   -- Valid --
   -----------

   function Valid   (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Boolean
   is ((MBR.P_Entries (P).Status and not 16#80#) = 0);

   --------------
   -- Get_Type --
   --------------

   function Get_Type (MBR : Master_Boot_Record;
                      P   : Partition_Number) return Partition_Type
   is (MBR.P_Entries (P).P_Type);

   ---------
   -- LBA --
   ---------

   function LBA     (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Block_Number
   is (
       --  MBR only supports 32-bit LBA. But as we want a generic FS interface
       --  here, LBA is defined as a 64-bit number, hence the explicit cast
       --  below.
       Block_Number (MBR.P_Entries (P).LBA));

   -------------
   -- Sectors --
   -------------

   function Sectors (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Interfaces.Unsigned_32
   is (MBR.P_Entries (P).Num_Sectors);

   --------------
   -- Get_Type --
   --------------

   function Get_Type (EBR : Extended_Boot_Record) return Partition_Type
   is
   begin
      return EBR.P_Entries (1).P_Type;
   end Get_Type;

   ---------
   -- LBA --
   ---------

   function LBA (EBR : Extended_Boot_Record) return Block_Number
   is
   begin
      return Block_Number (EBR.P_Entries (1).LBA);
   end LBA;

   -------------
   -- Sectors --
   -------------

   function Sectors (EBR : Extended_Boot_Record) return Interfaces.Unsigned_32
   is
   begin
      return EBR.P_Entries (1).Num_Sectors;
   end Sectors;

   --------------
   -- Has_Next --
   --------------

   function Has_Next (EBR : Extended_Boot_Record) return Boolean
   is
   begin
      return EBR.P_Entries (2) /= Zeroed_Entry;
   end Has_Next;

   ---------------
   -- Read_Next --
   ---------------

   function Read_Next
     (Controller : HAL.Block_Drivers.Any_Block_Driver;
      EBR        : in out Extended_Boot_Record)
      return File_IO.Status_Code
   is
      BA   : constant Block_Number := Block_Number (EBR.P_Entries (2).LBA);
      Tmp  : aliased Extended_Boot_Record;
      Data : aliased HAL.UInt8_Array (1 .. 512) with Address => Tmp'Address;
   begin
      --  Let's read the MBR: located in the first block
      if not Controller.Read (BA, Data) then
         return File_IO.Disk_Error;
      end if;

      EBR := Tmp;

      if EBR.Signature /= 16#AA55# then
         return File_IO.No_MBR_Found;
      end if;

      return File_IO.OK;
   end Read_Next;

end Filesystem.MBR;
