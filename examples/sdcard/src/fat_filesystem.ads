with Interfaces;   use Interfaces;
with Media_Reader; use Media_Reader;

package FAT_Filesystem is

   type Status_Code is
     (OK, Media_Error, FAT_Error);

   type FAT_Volume is abstract tagged limited private;
   type FAT_Volume_Access is access all FAT_Volume'Class;

   Null_FAT_Volume : constant FAT_Volume_Access;

   function Open
     (Controller : Media_Controller_Access;
      LBA        : Unsigned_32;
      Status     : out Status_Code) return FAT_Volume_Access;

   type FAT_Version is (FAT16, FAT32);

   function Version (Descr : FAT_Volume) return FAT_Version;
   --  The FAT version of the volume

   function OEM_Name (Descr : FAT_Volume) return String;
   --  The OEM Name of the Volume. Different from the Volume Label.

   function Block_Size_In_Bytes
     (Descr : FAT_Volume) return Unsigned_16;
   function Number_Of_Blocks_Per_Cluster
     (Descr : FAT_Volume) return Unsigned_8;
   function Reserved_Blocks
     (Descr : FAT_Volume) return Unsigned_16;
   function Number_Of_FATs
     (Descr : FAT_Volume) return Unsigned_8;
   function Total_Number_Of_Blocks
     (Descr : FAT_Volume) return Unsigned_32 is abstract;
   function Removable_Drive
     (Descr : FAT_Volume) return Boolean;
   function FAT_Table_Size_In_Blocks
     (Descr : FAT_Volume) return Unsigned_32 is abstract;
   function Number_Of_Hidden_Blocks
     (Descr : FAT_Volume) return Unsigned_32;
   function Drive_Number
     (Descr : FAT_Volume) return Unsigned_8 is abstract;

   function Is_Volume
     (Descr : FAT_Volume) return Boolean is abstract;
   function Volume_ID
     (Descr : FAT_Volume) return Unsigned_32 is abstract;
   function Volume_Label
     (Descr : FAT_Volume) return String is abstract;
   function File_System_Type
     (Descr : FAT_Volume) return String is abstract;
   function Root_Dir_Cluster
     (Descr : FAT_Volume) return Unsigned_32 is abstract;

   --------------------
   -- FAT16 specific --
   --------------------

   type FAT16_Volume is new FAT_Volume with private;

   overriding function Total_Number_Of_Blocks
     (Descr : FAT16_Volume) return Unsigned_32;
   overriding function FAT_Table_Size_In_Blocks
     (Descr : FAT16_Volume) return Unsigned_32;
   overriding function Drive_Number
     (Descr : FAT16_Volume) return Unsigned_8;
   overriding function Is_Volume
     (Descr : FAT16_Volume) return Boolean;
   overriding function Volume_ID
     (Descr : FAT16_Volume) return Unsigned_32;
   overriding function Volume_Label
     (Descr : FAT16_Volume) return String;
   overriding function File_System_Type
     (Descr : FAT16_Volume) return String;
   overriding function Root_Dir_Cluster
     (Descr : FAT16_Volume) return Unsigned_32;
   function Number_Of_Entries_In_Root_Dir
     (Descr : FAT16_Volume) return Unsigned_16;

   --------------------
   -- FAT32 specific --
   --------------------

   type FAT32_Volume is new FAT_Volume with private;

   overriding function Total_Number_Of_Blocks
     (Descr : FAT32_Volume) return Unsigned_32;
   overriding function FAT_Table_Size_In_Blocks
     (Descr : FAT32_Volume) return Unsigned_32;
   overriding function Drive_Number
     (Descr : FAT32_Volume) return Unsigned_8;
   overriding function Is_Volume
     (Descr : FAT32_Volume) return Boolean;
   overriding function Volume_ID
     (Descr : FAT32_Volume) return Unsigned_32;
   overriding function Volume_Label
     (Descr : FAT32_Volume) return String;
   overriding function File_System_Type
     (Descr : FAT32_Volume) return String;
   function Flags_For_FAT_Mirroring
     (Descr : FAT32_Volume) return Unsigned_16;
   function FS_Version_Number
     (Descr : FAT32_Volume) return Unsigned_16;
   overriding function Root_Dir_Cluster
     (Descr : FAT32_Volume) return Unsigned_32;
   function FSInfo_Block_Number
     (Descr : FAT32_Volume) return Unsigned_16;
   function Boot_Block_Backup_Block_Number
     (Descr : FAT32_Volume) return Unsigned_16;
   function Last_Known_Free_Data_Clusters_Number
     (Descr : FAT32_Volume) return Unsigned_32;
   function Most_Recently_Allocated_Cluster
     (Descr : FAT32_Volume) return Unsigned_32;

   type Directory_Handle is private;

   function Open_Root_Directory
     (Volume : FAT_Volume_Access) return Directory_Handle;

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

   type FAT_Volume is abstract tagged limited record
      Disk_Parameters : FAT_Disk_Parameter;
      LBA             : Unsigned_32;
      Controller      : Media_Controller_Access;
   end record;

   type FAT16_Volume is new FAT_Volume with null record;

   type FAT32_Volume is new FAT_Volume with record
      FSInfo : FAT_FS_Info;
   end record;

   function Version
     (Descr : FAT_Volume) return FAT_Version
   is (if Descr.Disk_Parameters.Root_Dir_Entries_Fat16 /= 0
       then FAT16 else FAT32);

   function OEM_Name (Descr : FAT_Volume) return String
   is (Descr.Disk_Parameters.OEM_Name);

   function Block_Size_In_Bytes
     (Descr : FAT_Volume) return Unsigned_16
   is (Descr.Disk_Parameters.Block_Size_In_Bytes);

   function Number_Of_Blocks_Per_Cluster
     (Descr : FAT_Volume) return Unsigned_8
   is (Descr.Disk_Parameters.Blocks_Per_Cluster);

   function Reserved_Blocks
     (Descr : FAT_Volume) return Unsigned_16
   is (Descr.Disk_Parameters.Reserved_Blocks);

   function Number_Of_FATs
     (Descr : FAT_Volume) return Unsigned_8
   is (Descr.Disk_Parameters.Number_Of_FATs);

   overriding function Total_Number_Of_Blocks
     (Descr : FAT16_Volume) return Unsigned_32
   is (Unsigned_32 (Descr.Disk_Parameters.Number_Of_Blocks_Fat16));

   overriding function Total_Number_Of_Blocks
     (Descr : FAT32_Volume) return Unsigned_32
   is (Descr.Disk_Parameters.Number_Of_Blocks_Fat32);

   function Removable_Drive
     (Descr : FAT_Volume) return Boolean
   is (Descr.Disk_Parameters.Removable_Drive);

   overriding function FAT_Table_Size_In_Blocks
     (Descr : FAT16_Volume) return Unsigned_32
   is (Unsigned_32 (Descr.Disk_Parameters.Table_Size_Fat16));

   overriding function FAT_Table_Size_In_Blocks
     (Descr : FAT32_Volume) return Unsigned_32
     is (Descr.Disk_Parameters.Table_Size_Fat32);

   function Number_Of_Hidden_Blocks
     (Descr : FAT_Volume) return Unsigned_32
   is (Descr.Disk_Parameters.Hidden_Blocks);

   overriding function Drive_Number
     (Descr : FAT16_Volume) return Unsigned_8
   is (Descr.Disk_Parameters.Drive_Number_Fat16);

   overriding function Drive_Number
     (Descr : FAT32_Volume) return Unsigned_8
   is (Descr.Disk_Parameters.Drive_Number_Fat32);

   overriding function Is_Volume
     (Descr : FAT16_Volume) return Boolean
   is (Descr.Disk_Parameters.Boot_Signature_Fat16 = 16#29#);

   overriding function Is_Volume
     (Descr : FAT32_Volume) return Boolean
   is (Descr.Disk_Parameters.Boot_Signature_Fat32 = 16#29#);

   overriding function Volume_ID
     (Descr : FAT16_Volume) return Unsigned_32
   is (if not Is_Volume (Descr)
       then 0 else Descr.Disk_Parameters.Volume_Id_Fat16);

   overriding function Volume_ID
     (Descr : FAT32_Volume) return Unsigned_32
   is (if not Is_Volume (Descr)
       then 0 else Descr.Disk_Parameters.Volume_Id_Fat32);

   overriding function Volume_Label
     (Descr : FAT16_Volume) return String
   is (if not Is_Volume (Descr)
       then "UNKNOWN"
       else Trim (Descr.Disk_Parameters.Volume_Label_Fat16));

   overriding function Volume_Label
     (Descr : FAT32_Volume) return String
   is (if not Is_Volume (Descr)
       then "UNKNOWN"
       else Trim (Descr.Disk_Parameters.Volume_Label_Fat32));

   overriding function File_System_Type
     (Descr : FAT16_Volume) return String
   is (if not Is_Volume (Descr)
       then "FAT16"
       else Trim (Descr.Disk_Parameters.FS_Type_Fat16));

   overriding function File_System_Type
     (Descr : FAT32_Volume) return String
   is (if not Is_Volume (Descr)
       then "FAT32"
       else Trim (Descr.Disk_Parameters.FS_Type_Fat32));

   function Number_Of_Entries_In_Root_Dir
     (Descr : FAT16_Volume) return Unsigned_16
   is (Descr.Disk_Parameters.Root_Dir_Entries_Fat16);

   function Flags_For_FAT_Mirroring
     (Descr : FAT32_Volume) return Unsigned_16
   is (Descr.Disk_Parameters.Fat_Mirroring_Flags);

   function FS_Version_Number
     (Descr : FAT32_Volume) return Unsigned_16
   is (Descr.Disk_Parameters.FS_Version_Number);

   overriding function Root_Dir_Cluster
     (Descr : FAT16_Volume) return Unsigned_32
   is (Unsigned_32 (Descr.Number_Of_FATs) *
         Descr.FAT_Table_Size_In_Blocks + 1);

   overriding function Root_Dir_Cluster
     (Descr : FAT32_Volume) return Unsigned_32
   is (Descr.Disk_Parameters.Root_Directory_Cluster);

   function FSInfo_Block_Number
     (Descr : FAT32_Volume) return Unsigned_16
   is (Descr.Disk_Parameters.FSInfo_Block_Number);

   function Boot_Block_Backup_Block_Number
     (Descr : FAT32_Volume) return Unsigned_16
   is (Descr.Disk_Parameters.Boot_Block_Backup_Block);

   function Last_Known_Free_Data_Clusters_Number
     (Descr : FAT32_Volume) return Unsigned_32
   is (Descr.FSInfo.Free_Clusters);

   function Most_Recently_Allocated_Cluster
     (Descr : FAT32_Volume) return Unsigned_32
   is (Descr.FSInfo.Last_Allocated_Cluster);

   Null_FAT_Volume : constant FAT_Volume_Access := null;

   type Directory_Handle is null record;

end FAT_Filesystem;
