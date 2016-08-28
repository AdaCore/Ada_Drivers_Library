
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

   procedure Read_Entry_In_MBR (MBR     : Block;
                                Index   : Integer;
                                P_Entry : out Partition_Entry)
   is
      Entry_Block_Conv : Partition_Entry_Block_Mapping;
      First : constant Integer := MBR'First + 446 + Index * 16;
      Last  : constant Integer := First + 15;
   begin
      if MBR'Length /= 512 then
         P_Entry.Status := 16#FF#;
      end if;

      Entry_Block_Conv.Data := MBR (First .. Last);
      P_Entry := Entry_Block_Conv.P_Entry;
   end Read_Entry_In_MBR;

   -------------------------
   -- Get_Partition_Entry --
   -------------------------

   function Get_Partition_Entry (Disk         : not null Block_Driver_Ref;
                                 Entry_Number : Positive;
                                 P_Entry      : out Partition_Entry)
                                 return Status_Code
   is
      MBR : Block (0 .. 511);
      Entry_Cnt : Natural := 0;
   begin
      if not Disk.Read (0, MBR) then
         return Disk_Error;
      end if;

      if MBR (510 .. 511) /= (16#55#, 16#AA#) then
         return Disk_Error;
      end if;

      for P_Index in 0 .. 3 loop
         Read_Entry_In_MBR (MBR, P_Index, P_Entry);

         --  Is it a valid entry?
         if P_Entry.Status = 16#80# or else P_Entry.Status = 16#00# then
            Entry_Cnt := Entry_Cnt + 1;

            --  Is is the entry we are looking for?
            if Entry_Cnt = Entry_Number then
               return Status_Ok;
            end if;
         end if;
      end loop;

      return Invalid_Parition;
   end Get_Partition_Entry;

   --------------------------
   -- Number_Of_Partitions --
   --------------------------

   function Number_Of_Partitions (Disk : Block_Driver_Ref) return Natural is
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

         --  Is it a valid entry?
         if P_Entry.Status = 16#80# or else P_Entry.Status = 16#00# then
            Entry_Cnt := Entry_Cnt + 1;
         end if;
      end loop;

      return Entry_Cnt;
   end Number_Of_Partitions;

end Partitions;
