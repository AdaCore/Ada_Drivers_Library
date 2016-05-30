with Ada.Unchecked_Conversion;

package body FAT_Filesystem is

   Volumes : array (1 .. MAX_VOLUMES) of aliased FAT_Filesystem;
   --  Mounted volumes

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
      LBA        : Unsigned_32;
      Status     : out Status_Code) return FAT_Filesystem_Access
   is
      Ret    : FAT_Filesystem_Access;

      subtype Disk_Parameter_Block is Block (0 .. 91);
      function To_Disk_Parameter is new Ada.Unchecked_Conversion
        (Disk_Parameter_Block, FAT_Disk_Parameter);

      subtype FSInfo_Block is Block (0 .. 11);
      function To_FSInfo is new Ada.Unchecked_Conversion
        (FSInfo_Block, FAT_FS_Info);

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

      Status := Ret.Ensure_Block (LBA);

      if Status /= OK then
         return Null_FAT_Volume;
      end if;

      if Ret.Window (510 .. 511) /= (16#55#, 16#AA#) then
         Status := No_Filesystem;
         return Null_FAT_Volume;
      end if;

      Ret.Disk_Parameters :=
        To_Disk_Parameter (Ret.Window (0 .. 91));

      if Ret.Version = FAT32 then
         Status :=
           Ret.Ensure_Block (LBA + Unsigned_32 (Ret.FSInfo_Block_Number));

         if Status /= OK then
            return Null_FAT_Volume;
         end if;

         --  Check the generic FAT block signature
         if Ret.Window (510 .. 511) /= (16#55#, 16#AA#) then
            Status := No_Filesystem;
            return Null_FAT_Volume;
         end if;

         Ret.FSInfo :=
           To_FSInfo (Ret.Window (16#1E4# .. 16#1EF#));
      end if;

      declare
         Data_Offset_In_Block : constant Unsigned_32 :=
                                  Unsigned_32 (Ret.Reserved_Blocks) +
                                  Ret.FAT_Table_Size_In_Blocks *
                                    Unsigned_32 (Ret.Number_Of_FATs);
         Root_Dir_Size        : Unsigned_32 := 0;
      begin
         if Ret.Version = FAT16 then
            --  Each directory entry is 32 Bytes.
            Root_Dir_Size :=
              Unsigned_32
                (Ret.Number_Of_Entries_In_Root_Dir) * 32;

            if (Root_Dir_Size mod Ret.Block_Size_In_Bytes) = 0 then
               Root_Dir_Size := Root_Dir_Size / Ret.Block_Size_In_Bytes;
            else
               Root_Dir_Size := 1 + Root_Dir_Size / Ret.Block_Size_In_Bytes;
            end if;
         end if;

         Ret.FAT_Addr  := LBA + Unsigned_32 (Ret.Reserved_Blocks);
         Ret.Data_Area := LBA + Data_Offset_In_Block + Root_Dir_Size;
         Ret.Num_Clusters :=
           (Ret.Total_Number_Of_Blocks - Data_Offset_In_Block) /
           Unsigned_32 (Ret.Number_Of_Blocks_Per_Cluster);
      end;

      return Ret;
   end Open;

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
