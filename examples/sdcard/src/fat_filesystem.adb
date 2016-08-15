--  XXX! Nothing here is thread-safe!
with Ada.Unchecked_Conversion;

package body FAT_Filesystem is

   Volumes : array (1 .. MAX_VOLUMES) of aliased FAT_Filesystem;
   --  Mounted volumes

   procedure Initialize_FS
     (FS     : in out FAT_Filesystem;
      Status : out Status_Code);

   function Allocate_Cluster (FS : in out FAT_Filesystem;
                              cluster : Unsigned_32) return Boolean
   is
      LAST_CLUSTER : constant Unsigned_32 := 16#FFFF_FFFF#;
   begin
      if FS.Set_FAT (cluster, LAST_CLUSTER) then
         FS.FSInfo.Free_Clusters := FS.FSInfo.Free_Clusters - 1;
         FS.FSInfo.Last_Allocated_Cluster := cluster;
         FS.Writeback_FsInfo;
         return True;
      else
         return False;
      end if;
   end Allocate_Cluster;

   function Append_Cluster
     (FS : in out FAT_Filesystem;
      Last_Cluster : Unsigned_32;
      New_Cluster  : out Unsigned_32) return Status_Code
   is
   begin
      New_Cluster := FS.Get_Free_Cluster;
      if New_Cluster = INVALID_CLUSTER then
         return Device_Full;
      end if;
      if not FS.Allocate_Cluster (New_Cluster) then
         return Allocation_Error;
      end if;
      --  chain it to the formerly last cluster
      if not FS.Set_FAT (Last_Cluster, New_Cluster) then
         return Allocation_Error;
      end if;
      return OK;
   end Append_Cluster;

   procedure Writeback_FsInfo
     (FS : in out FAT_Filesystem)
   is
      subtype FSInfo_Block is Block (0 .. 11);
      function From_FSInfo is new Ada.Unchecked_Conversion
        (FAT_FS_Info, FSInfo_Block);

      Status : Status_Code;
      fat_begin_lba : constant Unsigned_32 :=
        FS.LBA + Unsigned_32 (FS.FSInfo_Block_Number);
      ret : Status_Code;
      pragma Unreferenced (ret);
   begin
      FS.Window_Block := 16#FFFF_FFFF#;
      Status := FS.Ensure_Block (fat_begin_lba);

      if Status /= OK then
         return;
      end if;

      --  again, check the generic FAT block signature
      if FS.Window (510 .. 511) /= (16#55#, 16#AA#) then
         return;
      end if;

      --  good. now we got the entire FSinfo in our window.
      --  modify that part of the window and writeback.
      FS.Window (16#1E4# .. 16#1EF#) := From_FSInfo (FS.FSInfo);
      ret := FS.Write_Window (Block_Arg => fat_begin_lba);
   end Writeback_FsInfo;

   function Get_Free_Cluster (FS : in out FAT_Filesystem) return Unsigned_32 is
      cand : Unsigned_32 := FS.Most_Recently_Allocated_Cluster;
   begin
      if FS.Version = FAT32 then
         --  try shortcut: last allocated + 1 could be free
         if cand /= INVALID_CLUSTER and then cand < FS.Num_Clusters then
            cand := cand + 1;
            if FS.Is_Free_Cluster (FS.Get_FAT (cand)) then
               return cand;
            end if;
         end if;
      end if;

      --  otherwise exhaustively search for first free cluster
      declare
         c : Unsigned_32 := FIRST_CLUSTER;
      begin
         Scan_Loop : loop
            if FS.Is_Free_Cluster (FS.Get_FAT (c)) then
               return c;
            end if;
            exit Scan_Loop when c = FS.Num_Clusters;
            c := c + 1;
         end loop Scan_Loop;
      end;

      return INVALID_CLUSTER;
   end Get_Free_Cluster;

   function Is_FAT_Partition (typecode : Unsigned_8) return Boolean is
     (typecode = 16#0b#) or (typecode = 16#0c#) or --  FAT32
     (typecode = 16#0e#) or (typecode = 16#0f#) or --  FAT32
     (typecode = 16#04#); -- FAT16

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

      BOOTCODE_SIZE : constant := 446;

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

      --  Let's read the MBR (first 512B)
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

      --  Now check the partition entries: 4 partitions for the MBR,
      --  right after the boot code, which occupies the first 446B
      --  of the MBR. Then open the first FAT partition that we find.
      Partition_Loop : for P in 1 .. 4 loop
         --  Partitions are defined as an array of 16 bytes from
         --  base MBR + 446 (16#1BE#)
         P_Idx  := BOOTCODE_SIZE + Unsigned_16 (P - 1) * 16;
         P_Data := Ret.Window (P_Idx .. P_Idx + 15);

         --  Retrieve the number of blocks/sectors in the partition.
         N_Blocks := To_Word (P_Data (12 .. 15));

         if N_Blocks > 0 then
            --  The partition is valid
            declare
               typecode : constant Unsigned_8 := P_Data (4);
            begin
               if Is_FAT_Partition (typecode) then
                  Ret.LBA := To_Word (P_Data (8 .. 11)); -- start address/LBA
                  exit Partition_Loop;
               end if;
            end;

         elsif P = 4 then
            --  Last of the partition is also not valid.
            --  Thus, there's no valid partition at all.
            Status := No_Partition_Found;
            Ret.Mounted := False;

            return null;
         end if;
      end loop Partition_Loop;

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

   --  @summary read FAT volume ID (first 512 B)
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
      Status := FS.Ensure_Block (FS.LBA); -- read partition's first block ("volume id")

      if Status /= OK then
         return;
      end if;

      --  check volume signature: must be 55,AA
      if FS.Window (510 .. 511) /= (16#55#, 16#AA#) then
         Status := No_Filesystem;
         return;
      end if;

      FS.Disk_Parameters :=
        To_Disk_Parameter (FS.Window (0 .. 91));

      --  read the first file allocation table (FAT) and get FS info (#free, etc.)
      if FS.Version = FAT32 then
         declare
            fat_begin_lba : constant Unsigned_32 :=
              FS.LBA + Unsigned_32 (FS.FSInfo_Block_Number);
         begin
            Status := FS.Ensure_Block (fat_begin_lba);
         end;

         if Status /= OK then
            return;
         end if;

         --  again, check the generic FAT block signature
         if FS.Window (510 .. 511) /= (16#55#, 16#AA#) then
            Status := No_Filesystem;
            return;
         end if;

         FS.FSInfo :=
           To_FSInfo (FS.Window (16#1E4# .. 16#1EF#));
         --  read #free clusters, last alloc'd cluster, signature
      end if;

      --  compute LBA for FAT and data clusters
      declare
         --  where the clusters with data start relative to Volume ID LBA
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

   function Write_Window
     (FS : in out FAT_Filesystem;
      Block_Arg : Unsigned_32) return Status_Code
   is
   begin
      FS.Window_Block := Block_Arg; -- now we hold this block in the window
      if not FS.Controller.Write_Block (FS.Window_Block, FS.Window) then
         return Disk_Error;
      end if;
      return OK;
   end Write_Window;

   ------------------
   -- Ensure_Block --
   ------------------

   function Ensure_Block
     (FS    : in out FAT_Filesystem;
      Block_Arg : Unsigned_32) return Status_Code
   is
   begin
      --  ??? Should use re-entrance protection here
      if FS.Window_Block = Block_Arg then
         return OK;
      end if;

      if not FS.Controller.Read_Block (Block_Arg, FS.Window) then
         FS.Window_Block  := 16#FFFF_FFFF#;
         return Disk_Error;
      end if;

      FS.Window_Block := Block_Arg;

      return OK;
   end Ensure_Block;

   function Set_FAT
     (FS      : in out FAT_Filesystem;
      Cluster : Unsigned_32;
      Value   : Unsigned_32) return Boolean
   is
      Status    : Status_Code;
      Idx       : Unsigned_16;
      subtype B2 is Block (1 .. 2);
      subtype B4 is Block (1 .. 4);
      function From_Int16 is new Ada.Unchecked_Conversion
        (Unsigned_16, B2);
      function From_Int32 is new Ada.Unchecked_Conversion
        (Unsigned_32, B4);

      fat_block : Unsigned_32;
   begin

      case FS.Version is
         when FAT16 =>
            fat_block := FS.FAT_Addr + Cluster * 2 / FS.Block_Size_In_Bytes;
            Status := Ensure_Block (FS, fat_block);

            if Status /= OK then
               return False;
            end if;

            Idx := Unsigned_16 ((Cluster * 2) mod FS.Block_Size_In_Bytes);
            FS.Window (Idx .. Idx + 1) := From_Int16 (Unsigned_16 (Value and 16#FFFF#));

         when FAT32 =>
            fat_block := FS.FAT_Addr + Cluster * 4 / FS.Block_Size_In_Bytes;
            Status := Ensure_Block (FS, fat_block);

            if Status /= OK then
               return False;
            end if;

            Idx := Unsigned_16 ((Cluster * 4) mod FS.Block_Size_In_Bytes);
            FS.Window (Idx .. Idx + 3) := From_Int32 (Value);
      end case;

      --  write FAT back to disk
      return OK = FS.Write_Window (fat_block);
   end Set_FAT;

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
      if Cluster < FIRST_CLUSTER or else Cluster >= FS.Num_Clusters then
         return INVALID_CLUSTER;
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
               return INVALID_CLUSTER;
            end if;

            Idx := Unsigned_16 ((Cluster * 4) mod FS.Block_Size_In_Bytes);
            return To_Int32 (FS.Window (Idx .. Idx + 3)) and 16#0FFF_FFFF#;
      end case;
   end Get_FAT;

   function Image (s : Status_Code) return String is
   begin
      case s is
      when OK => return "OK";
      when Disk_Error => return "DiskErr";
      when Internal_Error => return "IntErr";
      when No_Such_File => return "NoSuchFile";
      when Invalid_Name => return "InvName";
      when Already_Exists => return "Exists";
      when Invalid_Object_Entry => return "InvEnt";
      when No_MBR_Found => return "NoMBR";
      when Device_Full => return "DevFull";
      when Allocation_Error => return "AllocErr";
      when No_Partition_Found => return "NoPart";
      when others => return "unknown";
      end case;
   end Image;

   function Is_Reserved_Cluster
     (FS  : FAT_Filesystem;
      ent : FAT_Entry) return Boolean is
   begin
      case Version (FS) is
         when FAT16 =>
            return ent > FS.Num_Clusters and ent <= 16#FFF6#;
         when FAT32 =>
            return ent > FS.Num_Clusters and ent <= 16#FFFF_FFF6#;
      end case;
   end Is_Reserved_Cluster;

end FAT_Filesystem;
