with Ada.Unchecked_Conversion;

package body FAT_Filesystem is

   Volumes : array (1 .. MAX_VOLUMES) of aliased FAT_Filesystem;
   --  Mounted volumes

   procedure Initialize_FS
     (FS     : in out FAT_Filesystem;
      Status : out Status_Code);

   ----------
   -- Trim --
   ----------

   function Trim (S : String) return String
   is
   begin
      for J in reverse S'Range loop
         if S (J) /= ' ' then
            return S (S'First .. J);
         end if;
      end loop;

      return "";
   end Trim;

   ----------
   -- Open --
   ----------

   function Open
     (Controller : Media_Controller_Access;
      Status     : out Status_Code) return FAT_Filesystem_Access
   is
      subtype Word_Data is Media_Reader.Block (0 .. 3);
      function To_Word is new
        Ada.Unchecked_Conversion (Word_Data, Unsigned_32);
      Ret      : FAT_Filesystem_Access;
      P_Idx    : Unsigned_16;
      P_Data   : Media_Reader.Block (0 .. 15);
      N_Blocks : Unsigned_32;

   begin
      --  Find a free Volume handle
      Ret := null;

      for J in Volumes'Range loop
         if not Volumes (J).Mounted then
            Ret := Volumes (J)'Access;
            exit;
         end if;
      end loop;

      if Ret = null then
         Status := Too_Many_Open_Files;
         return Null_FAT_Volume;
      end if;

      Ret.Mounted := True;
      Ret.Controller := Controller;

      --  Let's read the MBR
      Status := Ret.Ensure_Block (0);

      if Status /= OK then
         Ret.Mounted := False;

         return null;
      end if;

      --  Check for the MBR magic number
      if Ret.Window (510 .. 511) /= (16#55#, 16#AA#) then
         Status := No_MBR_Found;
         Ret.Mounted := False;

         return null;
      end if;

      --  Now check the partition entries: 4 partitions for the MBR
      for P in 1 .. 4 loop
         --  Partitions are defined as an array of 16 bytes from
         --  base MBR + 446 (16#1BE#)
         P_Idx  := 446 + Unsigned_16 (P - 1) * 16;
         P_Data := Ret.Window (P_Idx .. P_Idx + 15);

         --  Retrieve the number of blocks in the partition.
         N_Blocks := To_Word (P_Data (12 .. 15));

         if N_Blocks > 0 then
            --  The partition is valid
            Ret.LBA := To_Word (P_Data (8 .. 11));

            exit;

         elsif P = 4 then
            --  Last of the partition is not valid: there's no valid partition
            Status := No_Partition_Found;
            Ret.Mounted := False;

            return null;
         end if;
      end loop;

      Initialize_FS (Ret.all, Status);

      if Status /= OK then
         Ret.Mounted := False;

         return null;
      end if;

      return Ret;
   end Open;

   ----------
   -- Open --
   ----------

   function Open
     (Controller : Media_Controller_Access;
      LBA        : Unsigned_32;
      Status     : out Status_Code) return FAT_Filesystem_Access
   is
      Ret    : FAT_Filesystem_Access;

   begin
      --  Find a free Volume handle
      Ret := null;

      for J in Volumes'Range loop
         if not Volumes (J).Mounted then
            Ret := Volumes (J)'Access;
            exit;
         end if;
      end loop;

      if Ret = null then
         Status := Too_Many_Open_Files;
         return Null_FAT_Volume;
      end if;

      Ret.Mounted := True;
      Ret.Controller := Controller;
      Ret.LBA := LBA;

      Initialize_FS (Ret.all, Status);

      if Status /= OK then
         Ret.Mounted := False;

         return null;
      end if;

      return Ret;
   end Open;

   -------------------
   -- Initialize_FS --
   -------------------

   procedure Initialize_FS
     (FS     : in out FAT_Filesystem;
      Status : out Status_Code)
   is
      subtype Disk_Parameter_Block is Block (0 .. 91);
      function To_Disk_Parameter is new Ada.Unchecked_Conversion
        (Disk_Parameter_Block, FAT_Disk_Parameter);

      subtype FSInfo_Block is Block (0 .. 11);
      function To_FSInfo is new Ada.Unchecked_Conversion
        (FSInfo_Block, FAT_FS_Info);

   begin
      FS.Window_Block := 16#FFFF_FFFF#;
      Status := FS.Ensure_Block (FS.LBA);

      if Status /= OK then
         return;
      end if;

      if FS.Window (510 .. 511) /= (16#55#, 16#AA#) then
         Status := No_Filesystem;
         return;
      end if;

      FS.Disk_Parameters :=
        To_Disk_Parameter (FS.Window (0 .. 91));

      if FS.Version = FAT32 then
         Status :=
           FS.Ensure_Block (FS.LBA + Unsigned_32 (FS.FSInfo_Block_Number));

         if Status /= OK then
            return;
         end if;

         --  Check the generic FAT block signature
         if FS.Window (510 .. 511) /= (16#55#, 16#AA#) then
            Status := No_Filesystem;
            return;
         end if;

         FS.FSInfo :=
           To_FSInfo (FS.Window (16#1E4# .. 16#1EF#));
      end if;

      declare
         Data_Offset_In_Block : constant Unsigned_32 :=
                                  Unsigned_32 (FS.Reserved_Blocks) +
                                  FS.FAT_Table_Size_In_Blocks *
                                    Unsigned_32 (FS.Number_Of_FATs);
         Root_Dir_Size        : Unsigned_32 := 0;
      begin
         if FS.Version = FAT16 then
            --  Each directory entry is 32 Bytes.
            Root_Dir_Size :=
              Unsigned_32
                (FS.Number_Of_Entries_In_Root_Dir) * 32;

            if (Root_Dir_Size mod FS.Block_Size_In_Bytes) = 0 then
               Root_Dir_Size := Root_Dir_Size / FS.Block_Size_In_Bytes;
            else
               Root_Dir_Size := 1 + Root_Dir_Size / FS.Block_Size_In_Bytes;
            end if;
         end if;

         FS.FAT_Addr  := FS.LBA + Unsigned_32 (FS.Reserved_Blocks);
         FS.Data_Area := FS.LBA + Data_Offset_In_Block + Root_Dir_Size;
         FS.Num_Clusters :=
           (FS.Total_Number_Of_Blocks - Data_Offset_In_Block) /
           Unsigned_32 (FS.Number_Of_Blocks_Per_Cluster);
      end;
   end Initialize_FS;

   -----------
   -- Close --
   -----------

   procedure Close (FS : FAT_Filesystem_Access)
   is
   begin
      FS.Mounted := False;
   end Close;

   ------------------
   -- Ensure_Block --
   ------------------

   function Ensure_Block
     (FS    : in out FAT_Filesystem;
      Block : Unsigned_32) return Status_Code
   is
   begin
      --  ??? Should use re-entrance protection here
      if FS.Window_Block = Block then
         return OK;
      end if;

      if not FS.Controller.Read_Block (Block, FS.Window) then
         FS.Window_Block  := 16#FFFF_FFFF#;

         return Disk_Error;
      end if;

      FS.Window_Block := Block;

      return OK;
   end Ensure_Block;

   -------------
   -- Get_FAT --
   -------------

   function Get_FAT
     (FS      : in out FAT_Filesystem;
      Cluster : Unsigned_32) return Unsigned_32
   is
      Status    : Status_Code;
      Idx       : Unsigned_16;
      subtype B2 is Block (1 .. 2);
      subtype B4 is Block (1 .. 4);
      function To_Int16 is new Ada.Unchecked_Conversion
        (B2, Unsigned_16);
      function To_Int32 is new Ada.Unchecked_Conversion
        (B4, Unsigned_32);

   begin
      if Cluster < 2 or else Cluster >= FS.Num_Clusters then
         return 1;
      end if;

      case FS.Version is
         when FAT16 =>
            Status := Ensure_Block
              (FS,
               FS.FAT_Addr + Cluster * 2 / FS.Block_Size_In_Bytes);

            if Status /= OK then
               return 1;
            end if;

            Idx := Unsigned_16 ((Cluster * 2) mod FS.Block_Size_In_Bytes);

            return Unsigned_32 (To_Int16 (FS.Window (Idx .. Idx + 1)));

         when FAT32 =>
            Status := Ensure_Block
              (FS,
               FS.FAT_Addr + Cluster * 4 / FS.Block_Size_In_Bytes);

            if Status /= OK then
               return 1;
            end if;

            Idx := Unsigned_16 ((Cluster * 4) mod FS.Block_Size_In_Bytes);

            return To_Int32 (FS.Window (Idx .. Idx + 3)) and 16#0FFF_FFFF#;
      end case;
   end Get_FAT;

end FAT_Filesystem;
