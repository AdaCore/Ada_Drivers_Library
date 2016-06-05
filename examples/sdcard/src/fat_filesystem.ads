with Interfaces;   use Interfaces;
with Media_Reader; use Media_Reader;

package FAT_Filesystem is

   MAX_VOLUMES           : constant := 1;
   --  Maximum number of mounted volumes

   Max_Handles_Reached   : exception;

   type Status_Code is
     (OK,
      Disk_Error, --  A hardware error occurred in the low level disk I/O
      Internal_Error,
      Drive_Not_Ready,
      No_Such_File,
      No_Such_Path,
      Invalid_Name,
      Access_Denied,
      Already_Exists,
      Invalid_Object_Entry,
      Write_Protected,
      Invalid_Drive,
      Not_Enabled, --  The volume has no work area
      No_Filesystem, --  The volume is not a FAT volume
      Locked,
      Too_Many_Open_Files, --  Number of opened files > Max
      Invalid_Parameter,
      No_MBR_Found,
      No_Partition_Found);

   type FAT_Filesystem is limited private;
   type FAT_Filesystem_Access is access all FAT_Filesystem;

   Null_FAT_Volume : constant FAT_Filesystem_Access;

   function Open
     (Controller : Media_Controller_Access;
      Status     : out Status_Code) return FAT_Filesystem_Access;
   --  Search the media for a valid FAT partition and opens it.
   --  If the media contains several partitions, the first one is used

   function Open
     (Controller : Media_Controller_Access;
      LBA        : Unsigned_32;
      Status     : out Status_Code) return FAT_Filesystem_Access;
   --  Opens a FAT partition at the given LBA

   procedure Close (FS : FAT_Filesystem_Access);

   type FAT_Version is
     (FAT16,
      FAT32);

   function Version (FS : FAT_Filesystem) return FAT_Version;
   --  The FAT version of the volume

   function OEM_Name (FS : FAT_Filesystem) return String;
   --  The OEM Name of the Volume. Different from the Volume Label.

   function Block_Size_In_Bytes
     (FS : FAT_Filesystem) return Unsigned_32;
   function Number_Of_Blocks_Per_Cluster
     (FS : FAT_Filesystem) return Unsigned_8;
   function Reserved_Blocks
     (FS : FAT_Filesystem) return Unsigned_16;
   function Number_Of_FATs
     (FS : FAT_Filesystem) return Unsigned_8;
   function Total_Number_Of_Blocks
     (FS : FAT_Filesystem) return Unsigned_32;
   function Removable_Drive
     (FS : FAT_Filesystem) return Boolean;
   function FAT_Table_Size_In_Blocks
     (FS : FAT_Filesystem) return Unsigned_32;
   function Number_Of_Hidden_Blocks
     (FS : FAT_Filesystem) return Unsigned_32;
   function Drive_Number
     (FS : FAT_Filesystem) return Unsigned_8;

   function Is_Volume
     (FS : FAT_Filesystem) return Boolean;
   function Volume_ID
     (FS : FAT_Filesystem) return Unsigned_32;
   function Volume_Label
     (FS : FAT_Filesystem) return String;
   function File_System_Type
     (FS : FAT_Filesystem) return String;
   function Root_Dir_Cluster
     (FS : FAT_Filesystem) return Unsigned_32;

   --------------------
   -- FAT16 specific --
   --------------------

   function Number_Of_Entries_In_Root_Dir
     (FS : FAT_Filesystem) return Unsigned_16
     with Pre => Version (FS) = FAT16;

   --------------------
   -- FAT32 specific --
   --------------------

   function Flags_For_FAT_Mirroring
     (FS : FAT_Filesystem) return Unsigned_16
     with Pre => Version (FS) = FAT32;
   function FS_Version_Number
     (FS : FAT_Filesystem) return Unsigned_16
     with Pre => Version (FS) = FAT32;
   function FSInfo_Block_Number
     (FS : FAT_Filesystem) return Unsigned_16
     with Pre => Version (FS) = FAT32;
   function Boot_Block_Backup_Block_Number
     (FS : FAT_Filesystem) return Unsigned_16
     with Pre => Version (FS) = FAT32;
   function Last_Known_Free_Data_Clusters_Number
     (FS : FAT_Filesystem) return Unsigned_32
     with Pre => Version (FS) = FAT32;
   function Most_Recently_Allocated_Cluster
     (FS : FAT_Filesystem) return Unsigned_32
     with Pre => Version (FS) = FAT32;

private

   type FAT_Disk_Parameter (Version : FAT_Version := FAT16) is record
      OEM_Name                : String (1 .. 8);
      Block_Size_In_Bytes     : Unsigned_16;
      Blocks_Per_Cluster      : Unsigned_8;
      Reserved_Blocks         : Unsigned_16;
      Number_Of_FATs          : Unsigned_8;
      Root_Dir_Entries_Fat16  : Unsigned_16;
      Number_Of_Blocks_Fat16  : Unsigned_16;
      Removable_Drive         : Boolean;
      Table_Size_Fat16        : Unsigned_16;
      Blocks_Per_Cylinder     : Unsigned_16;
      Number_Of_Heads         : Unsigned_16;
      Hidden_Blocks           : Unsigned_32;
      Number_Of_Blocks_Fat32  : Unsigned_32;

      case Version is
         when FAT32 =>
            Table_Size_Fat32        : Unsigned_32;
            Fat_Mirroring_Flags     : Unsigned_16;
            FS_Version_Number       : Unsigned_16;
            Root_Directory_Cluster  : Unsigned_32;
            FSInfo_Block_Number     : Unsigned_16;
            Boot_Block_Backup_Block : Unsigned_16;
            Drive_Number_Fat32      : Unsigned_8;
            Current_Head_Fat32      : Unsigned_8;
            Boot_Signature_Fat32    : Unsigned_8;
            Volume_Id_Fat32         : Unsigned_32;
            Volume_Label_Fat32      : String (1 .. 11);
            FS_Type_Fat32           : String (1 .. 8);
         when FAT16 =>
            Drive_Number_Fat16      : Unsigned_8;
            Current_Head_Fat16      : Unsigned_8;
            Boot_Signature_Fat16    : Unsigned_8;
            Volume_Id_Fat16         : Unsigned_32;
            Volume_Label_Fat16      : String (1 .. 11);
            FS_Type_Fat16           : String (1 .. 8);
      end case;
   end record with Unchecked_Union, Size => 92 * 8;

   for FAT_Disk_Parameter use record
      OEM_Name                at 16#03# range 0 .. 63;
      Block_Size_In_Bytes     at 16#0B# range 0 .. 15;
      Blocks_Per_Cluster      at 16#0D# range 0 .. 7;
      Reserved_Blocks         at 16#0E# range 0 .. 15;
      Number_Of_FATs          at 16#10# range 0 .. 7;
      Root_Dir_Entries_Fat16  at 16#11# range 0 .. 15;
      Number_Of_Blocks_Fat16  at 16#13# range 0 .. 15;
      Removable_Drive         at 16#15# range 2 .. 2;
      Table_Size_Fat16        at 16#16# range 0 .. 15;
      Blocks_Per_Cylinder     at 16#18# range 0 .. 15;
      Number_Of_Heads         at 16#1A# range 0 .. 15;
      Hidden_Blocks           at 16#1C# range 0 .. 31;
      Number_Of_Blocks_Fat32  at 16#20# range 0 .. 31;
      Table_Size_Fat32        at 16#24# range 0 .. 31;
      Fat_Mirroring_Flags     at 16#28# range 0 .. 15;
      FS_Version_Number       at 16#2A# range 0 .. 15;
      Root_Directory_Cluster  at 16#2C# range 0 .. 31;
      FSInfo_Block_Number     at 16#30# range 0 .. 15;
      Boot_Block_Backup_Block at 16#32# range 0 .. 15;
      Drive_Number_Fat32      at 16#40# range 0 .. 7;
      Current_Head_Fat32      at 16#41# range 0 .. 7;
      Boot_Signature_Fat32    at 16#42# range 0 .. 7;
      Volume_Id_Fat32         at 16#43# range 0 .. 31;
      Volume_Label_Fat32      at 16#47# range 0 .. 87;
      FS_Type_Fat32           at 16#52# range 0 .. 63;
      Drive_Number_Fat16      at 16#24# range 0 .. 7;
      Current_Head_Fat16      at 16#25# range 0 .. 7;
      Boot_Signature_Fat16    at 16#26# range 0 .. 7;
      Volume_Id_Fat16         at 16#27# range 0 .. 31;
      Volume_Label_Fat16      at 16#2B# range 0 .. 87;
      FS_Type_Fat16           at 16#36# range 0 .. 63;
   end record;

   function Trim (S : String) return String;

   type FAT_FS_Info is record
      Signature              : String (1 .. 4);
      Free_Clusters          : Unsigned_32;
      Last_Allocated_Cluster : Unsigned_32;
   end record;

   for FAT_FS_Info use record
      Signature              at 0 range 0 .. 31;
      Free_Clusters          at 4 range 0 .. 31;
      Last_Allocated_Cluster at 8 range 0 .. 31;
   end record;

   type FAT_Filesystem is tagged limited record
      Mounted         : Boolean := False;
      Disk_Parameters : FAT_Disk_Parameter;
      LBA             : Unsigned_32;
      Controller      : Media_Controller_Access;
      FSInfo          : FAT_FS_Info;
      Data_Area       : Unsigned_32;
      FAT_Addr        : Unsigned_32;
      Num_Clusters    : Unsigned_32;
      Window_Block    : Unsigned_32 := 16#FFFF_FFFF#;
      Window          : Block (0 .. 511);
   end record;

   function Ensure_Block
     (FS    : in out FAT_Filesystem;
      Block : Unsigned_32) return Status_Code;

   function Cluster_To_Block
     (FS      : FAT_Filesystem;
      Cluster : Unsigned_32) return Unsigned_32
   is (FS.Data_Area +
       (Cluster - 2) * Unsigned_32 (FS.Number_Of_Blocks_Per_Cluster));

   function Get_FAT
     (FS      : in out FAT_Filesystem;
      Cluster : Unsigned_32) return Unsigned_32;

   function EOC
     (FS : FAT_Filesystem;
      Cluster : Unsigned_32) return Boolean
   is (case Version (FS) is
          when FAT16 => (Cluster and 16#FFF8#) = 16#FFF8#,
          when FAT32 => (Cluster and 16#0FFF_FFF8#) = 16#0FFF_FFF8#);

   function Version
     (FS : FAT_Filesystem) return FAT_Version
   is (if FS.Disk_Parameters.Root_Dir_Entries_Fat16 /= 0
       then FAT16 else FAT32);

   function OEM_Name (FS : FAT_Filesystem) return String
   is (FS.Disk_Parameters.OEM_Name);

   function Block_Size_In_Bytes
     (FS : FAT_Filesystem) return Unsigned_32
   is (Unsigned_32 (FS.Disk_Parameters.Block_Size_In_Bytes));

   function Number_Of_Blocks_Per_Cluster
     (FS : FAT_Filesystem) return Unsigned_8
   is (FS.Disk_Parameters.Blocks_Per_Cluster);

   function Reserved_Blocks
     (FS : FAT_Filesystem) return Unsigned_16
   is (FS.Disk_Parameters.Reserved_Blocks);

   function Number_Of_FATs
     (FS : FAT_Filesystem) return Unsigned_8
   is (FS.Disk_Parameters.Number_Of_FATs);

   function Total_Number_Of_Blocks
     (FS : FAT_Filesystem) return Unsigned_32
   is (case Version (FS) is
          when FAT16 =>
             Unsigned_32 (FS.Disk_Parameters.Number_Of_Blocks_Fat16),
          when FAT32 => FS.Disk_Parameters.Number_Of_Blocks_Fat32);

   function Removable_Drive
     (FS : FAT_Filesystem) return Boolean
   is (FS.Disk_Parameters.Removable_Drive);

   function FAT_Table_Size_In_Blocks
     (FS : FAT_Filesystem) return Unsigned_32
   is (case Version (FS) is
          when FAT16 => Unsigned_32 (FS.Disk_Parameters.Table_Size_Fat16),
          when FAT32 => FS.Disk_Parameters.Table_Size_Fat32);

   function Number_Of_Hidden_Blocks
     (FS : FAT_Filesystem) return Unsigned_32
   is (FS.Disk_Parameters.Hidden_Blocks);

   function Drive_Number
     (FS : FAT_Filesystem) return Unsigned_8
   is (case Version (FS) is
          when FAT16 => FS.Disk_Parameters.Drive_Number_Fat16,
          when FAT32 => FS.Disk_Parameters.Drive_Number_Fat32);

   function Is_Volume
     (FS : FAT_Filesystem) return Boolean
   is (case Version (FS) is
          when FAT16 => FS.Disk_Parameters.Boot_Signature_Fat16 = 16#29#,
          when FAT32 => FS.Disk_Parameters.Boot_Signature_Fat32 = 16#29#);

   function Volume_ID
     (FS : FAT_Filesystem) return Unsigned_32
   is (if not Is_Volume (FS)
       then 0
       else
         (case Version (FS) is
             when FAT16 => FS.Disk_Parameters.Volume_Id_Fat16,
             when FAT32 => FS.Disk_Parameters.Volume_Id_Fat32));

   function Volume_Label
     (FS : FAT_Filesystem) return String
   is (if not Is_Volume (FS)
       then "UNKNOWN"
       else
         (case Version (FS) is
             when FAT16 => Trim (FS.Disk_Parameters.Volume_Label_Fat16),
             when FAT32 => Trim (FS.Disk_Parameters.Volume_Label_Fat32)));

   function File_System_Type
     (FS : FAT_Filesystem) return String
   is (if not Is_Volume (FS)
       then "FAT16"
       else
         (case Version (FS) is
             when FAT16 => Trim (FS.Disk_Parameters.FS_Type_Fat16),
             when FAT32 => Trim (FS.Disk_Parameters.FS_Type_Fat32)));

   function Number_Of_Entries_In_Root_Dir
     (FS : FAT_Filesystem) return Unsigned_16
   is (FS.Disk_Parameters.Root_Dir_Entries_Fat16);

   function Flags_For_FAT_Mirroring
     (FS : FAT_Filesystem) return Unsigned_16
   is (FS.Disk_Parameters.Fat_Mirroring_Flags);

   function FS_Version_Number
     (FS : FAT_Filesystem) return Unsigned_16
   is (FS.Disk_Parameters.FS_Version_Number);

   function Root_Dir_Cluster
     (FS : FAT_Filesystem) return Unsigned_32
   is (case Version (FS) is
          when FAT16 => Unsigned_32 (FS.Number_Of_FATs) *
                          FS.FAT_Table_Size_In_Blocks + 1,
          when FAT32 => FS.Disk_Parameters.Root_Directory_Cluster);

   function FSInfo_Block_Number
     (FS : FAT_Filesystem) return Unsigned_16
   is (FS.Disk_Parameters.FSInfo_Block_Number);

   function Boot_Block_Backup_Block_Number
     (FS : FAT_Filesystem) return Unsigned_16
   is (FS.Disk_Parameters.Boot_Block_Backup_Block);

   function Last_Known_Free_Data_Clusters_Number
     (FS : FAT_Filesystem) return Unsigned_32
   is (FS.FSInfo.Free_Clusters);

   function Most_Recently_Allocated_Cluster
     (FS : FAT_Filesystem) return Unsigned_32
   is (FS.FSInfo.Last_Allocated_Cluster);

   Null_FAT_Volume : constant FAT_Filesystem_Access := null;

end FAT_Filesystem;
