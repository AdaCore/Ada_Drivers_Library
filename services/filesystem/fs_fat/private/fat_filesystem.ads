------------------------------------------------------------------------------
--                            Ada FAT FS Library                            --
--                                                                          --
--                   Copyright (C) 2016, Jerome Lambourg                    --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces;        use Interfaces;
with HAL.Block_Drivers; use HAL.Block_Drivers;

limited with FAT_Filesystem.Directories;

package FAT_Filesystem is

   Max_Handles_Reached : exception;

   MAX_VOLUMES         : constant := 1;
   --  Maximum number of mounted volumes

   MAX_FILENAME_LENGTH : constant := 255;
   --  Maximum size of a file or directory name

   MAX_PATH_LENGTH     : constant := 1024;
   --  Maximum size of a path name length

   type Cluster_Type is mod 2 ** 32 with Size => 32;
   subtype Valid_Cluster is Cluster_Type range 2 .. 16#0FFF_FFFF#;

   FREE_CLUSTER_VALUE : constant Cluster_Type := 16#0000_0000#;
   LAST_CLUSTER_VALUE : constant Cluster_Type := 16#0FFF_FFFF#;
   BAD_CLUSTER_VALUE  : constant Cluster_Type := 16#FFFF_FFF7#;

   type Block_Num is mod 2 ** 32 with Size => 32;

   type Status_Code is
     (OK,
      Non_Empty_Directory,
      Disk_Error, --  A hardware error occurred in the low level disk I/O
      Disk_Full,
      Internal_Error,
      Drive_Not_Ready,
      No_Such_File,
      No_Such_Path,
      Not_Mounted, --  The mount point is invalid
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
      No_Partition_Found,
      No_More_Entries);

   -----------------------
   -- PATH MANIPULATION --
   -----------------------

   type FAT_Name is private;

   function "-" (Name : FAT_Name) return String;

   function "-" (Name : String) return FAT_Name
     with Pre => Name'Length < MAX_FILENAME_LENGTH;

   overriding function "=" (Name1, Name2 : FAT_Name) return Boolean;

   type FAT_Path is private;

   Empty_Path : constant FAT_Path;

   function "-" (Path : FAT_Path) return String;

   function "-" (Path : String) return FAT_Path
     with Pre => Path'Length < MAX_PATH_LENGTH;

   overriding function "=" (Name1, Name2 : FAT_Path) return Boolean;

   procedure Append
     (Path : in out FAT_Path;
      Name : FAT_Name);

   procedure Append
     (Path     : in out FAT_Path;
      Sub_Path : FAT_Path);

   function "&" (Path : FAT_Path; Name : FAT_Name) return FAT_Path;

   function "&" (Path : FAT_Path; Sub_Path : FAT_Path) return FAT_Path;

   function Is_Root (Path : FAT_Path) return Boolean with Inline_Always;

   procedure To_Parent (Path : in out FAT_Path);
   function Parent (Path : FAT_Path) return FAT_Path;

   procedure Normalize (Path       : in out FAT_Path;
                        Ensure_Dir : Boolean := False);

   function Mount_Point (Path : FAT_Path) return FAT_Name;
   function FS_Path (Path : FAT_Path) return FAT_Path;

   ------------------------
   -- DIRECTORY HANDLING --
   ------------------------

   function Change_Dir (Dir_Name : FAT_Path) return Boolean;
   function Change_Dir (Dir_Name : FAT_Name) return Boolean;
   procedure Change_Dir (Dir_Name : FAT_Path);
   procedure Change_Dir (Dir_Name : FAT_Name);

   function Current_Directory return FAT_Path;

   type Directory_Handle is private;

   function Open
     (Path   : FAT_Path;
      Handle : out Directory_Handle) return Status_Code;
   function Read (Dir    : in out Directory_Handle;
                  DEntry : out Directories.Directory_Entry)
                  return Status_Code;
   procedure Reset (Dir : in out Directory_Handle);
   procedure Close (Dir : in out Directory_Handle);

--     function Is_Directory (Path : FAT_Path) return Boolean;
--     function Is_File (Path : FAT_Path) return Boolean;
--     function Is_File_Or_Directory (Path : FAT_Path) return Boolean;

   --------------------
   -- FAT FILESYSTEM --
   --------------------

   type FAT_Filesystem is limited private;
   type FAT_Filesystem_Access is access all FAT_Filesystem;

   Null_FAT_Volume : constant FAT_Filesystem_Access;

   function Open
     (Controller  : HAL.Block_Drivers.Block_Driver_Ref;
      Mount_Point : FAT_Name;
      Status      : out Status_Code) return FAT_Filesystem_Access;
   --  Search the media for a valid FAT partition and opens it.
   --  If the media contains several partitions, the first one is used

   function Open
     (Controller  : HAL.Block_Drivers.Block_Driver_Ref;
      LBA         : Unsigned_32;
      Mount_Point : FAT_Name;
      Status      : out Status_Code) return FAT_Filesystem_Access;
   --  Opens a FAT partition at the given LBA

   function Get_Mount_Point
     (Mount_Point : FAT_Name;
      FS          : out FAT_Filesystem_Access) return Status_Code;

   function Get_Mount_Point
     (FS : not null FAT_Filesystem_Access) return FAT_Name;

   procedure Close (FS : FAT_Filesystem_Access);

   type FAT_Directory_Entry_Attribute is record
      Read_Only    : Boolean;
      Hidden       : Boolean;
      System_File  : Boolean;
      Volume_Label : Boolean;
      Subdirectory : Boolean;
      Archive      : Boolean;
   end record with Size => 8, Pack;

   -----------------------
   -- FAT FS PROPERTIES --
   -----------------------

   type FAT_Version is
     (FAT16,
      FAT32);

   function Version (FS : FAT_Filesystem) return FAT_Version;
   --  The FAT version of the volume

   function OEM_Name (FS : FAT_Filesystem) return String;
   --  The OEM Name of the Volume. Different from the Volume Label.

   function Bytes_Per_Block
     (FS : FAT_Filesystem) return Unsigned_32;
   function Blocks_Per_Cluster
     (FS : FAT_Filesystem) return Unsigned_8;
   function Bytes_Per_Cluster
     (FS : FAT_Filesystem) return Unsigned_32;
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
     (FS : FAT_Filesystem) return Cluster_Type;

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
     (FS : FAT_Filesystem) return Cluster_Type
     with Pre => Version (FS) = FAT32;

private

   INVALID_CLUSTER : constant := 0;

   type FAT_Name is record
      Name : String (1 .. MAX_FILENAME_LENGTH);
      Len  : Natural := 0;
   end record;

   type FAT_Path is record
      Name : String (1 .. MAX_PATH_LENGTH);
      Len  : Natural := 0;
   end record;

   Empty_Path : constant FAT_Path :=
                  (Name => (others => ' '),
                   Len  => 0);

   type FAT_Disk_Parameter is record
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

      Table_Size_Fat32        : Unsigned_32;
      Fat_Mirroring_Flags     : Unsigned_16;
      FS_Version_Number       : Unsigned_16;
      Root_Directory_Cluster  : Cluster_Type;
      FSInfo_Block_Number     : Unsigned_16;
      Boot_Block_Backup_Block : Unsigned_16;
      Drive_Number_Fat32      : Unsigned_8;
      Current_Head_Fat32      : Unsigned_8;
      Boot_Signature_Fat32    : Unsigned_8;
      Volume_Id_Fat32         : Unsigned_32;
      Volume_Label_Fat32      : String (1 .. 11);
      FS_Type_Fat32           : String (1 .. 8);
   end record with Size => 92 * 8;

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
   end record;

   function Trim (S : String) return String;

   type FAT_FS_Info is record
      Signature              : String (1 .. 4);
      Free_Clusters          : Unsigned_32;
      Last_Allocated_Cluster : Cluster_Type;
   end record;

   for FAT_FS_Info use record
      Signature              at 0 range 0 .. 31;
      Free_Clusters          at 4 range 0 .. 31;
      Last_Allocated_Cluster at 8 range 0 .. 31;
   end record;

   type FAT_Filesystem is tagged limited record
      Mounted         : Boolean := False;
      Mount_Point     : FAT_Name;
      Disk_Parameters : FAT_Disk_Parameter;
      LBA             : Unsigned_32;
      Controller      : Block_Driver_Ref;
      FSInfo          : FAT_FS_Info;
      Data_Area       : Unsigned_32;
      FAT_Addr        : Unsigned_32;
      Num_Clusters    : Cluster_Type;
      Window_Block    : Unsigned_32 := 16#FFFF_FFFF#;
      Window          : Block (0 .. 511);
      FAT_Block       : Unsigned_32 := 16#FFFF_FFFF#;
      FAT_Window      : Block (0 .. 511);
   end record;

   function Ensure_Block
     (FS                : in out FAT_Filesystem;
      Block             : Unsigned_32) return Status_Code;
   --  Ensures the block is visible within the FS window.
   --  Block_Base_OFfset returns the index within the FS window of the block

   function Write_Window (FS : in out FAT_Filesystem) return Status_Code;

   function Cluster_To_Block
     (FS      : FAT_Filesystem;
      Cluster : Cluster_Type) return Unsigned_32
   is (FS.Data_Area +
       Unsigned_32 (Cluster - 2) * Unsigned_32 (FS.Blocks_Per_Cluster));

   function Get_FAT
     (FS      : in out FAT_Filesystem;
      Cluster : Cluster_Type) return Cluster_Type;

   function Set_FAT
     (FS      : in out FAT_Filesystem;
      Cluster : Cluster_Type;
      Value   : Cluster_Type) return Status_Code;

   function Get_Free_Cluster
     (FS       : in out FAT_Filesystem;
      Previous : Cluster_Type := INVALID_CLUSTER) return Cluster_Type;
   --  Retrieve a free cluster from the filesystem.
   --  Returns INVALID_CLUSTER in case the filesystem is full.

   procedure Write_FSInfo
     (FS : in out FAT_Filesystem);
   --  Writes back the FSInfo structure on the Filesystem

   function Is_Last_Cluster
     (FS  : FAT_Filesystem;
      Ent : Cluster_Type) return Boolean
   is (case Version (FS) is
          when FAT16 => (Ent and 16#FFF8#) = 16#FFF8#,
          when FAT32 => (Ent and 16#0FFF_FFF8#) = 16#0FFF_FFF8#);
   --  return true if this is the last cluster for an entry

   function Is_Reserved_Cluster
     (FS  : FAT_Filesystem;
      Ent : Cluster_Type) return Boolean
   is (case Version (FS) is
          when FAT16 => Ent > FS.Num_Clusters and Ent <= 16#FFF6#,
          when FAT32 => Ent > FS.Num_Clusters and Ent <= 16#0FFF_FFF6#);
   --  return true if this cluster is reserved

   function Is_Bad_Cluster
     (FS  : FAT_Filesystem;
      Ent : Cluster_Type) return Boolean
   is (case Version (FS) is
          when FAT16 => (Ent and 16#FFF7#) = 16#FFF7#,
          when FAT32 => (Ent and 16#FFFF_FFF7#) = 16#FFFF_FFF7#);
   --  return true if this cluster is defective

   function Is_Free_Cluster
     (FS  : FAT_Filesystem;
      Ent : Cluster_Type) return Boolean
   is ((Ent and 16#0FFF_FFFF#) = FREE_CLUSTER_VALUE);
   --  return true if the FAT entry indicates the cluster being unused

   function New_Cluster
     (FS : in out FAT_Filesystem) return Cluster_Type;

   function New_Cluster
     (FS       : in out FAT_Filesystem;
      Previous : Cluster_Type) return Cluster_Type;

   function Get_Mount_Point
     (FS : not null FAT_Filesystem_Access) return FAT_Name
   is (FS.Mount_Point);

   --------------------------------------------------
   -- Inlined implementations of utility functions --
   --------------------------------------------------

   function Version
     (FS : FAT_Filesystem) return FAT_Version
   is (if FS.Disk_Parameters.Root_Dir_Entries_Fat16 /= 0
       then FAT16 else FAT32);

   function OEM_Name (FS : FAT_Filesystem) return String
   is (FS.Disk_Parameters.OEM_Name);

   function Bytes_Per_Block
     (FS : FAT_Filesystem) return Unsigned_32
   is (Unsigned_32 (FS.Disk_Parameters.Block_Size_In_Bytes));

   function Blocks_Per_Cluster
     (FS : FAT_Filesystem) return Unsigned_8
   is (FS.Disk_Parameters.Blocks_Per_Cluster);

   function Bytes_Per_Cluster
     (FS : FAT_Filesystem) return Unsigned_32
   is (Unsigned_32 (FS.Blocks_Per_Cluster) * FS.Bytes_Per_Block);

   function Reserved_Blocks
     (FS : FAT_Filesystem) return Unsigned_16
   is (FS.Disk_Parameters.Reserved_Blocks);

   function Number_Of_FATs
     (FS : FAT_Filesystem) return Unsigned_8
   is (FS.Disk_Parameters.Number_Of_FATs);

   function Total_Number_Of_Blocks
     (FS : FAT_Filesystem) return Unsigned_32
   is (FS.Disk_Parameters.Number_Of_Blocks_Fat32);

   function Removable_Drive
     (FS : FAT_Filesystem) return Boolean
   is (FS.Disk_Parameters.Removable_Drive);

   function FAT_Table_Size_In_Blocks
     (FS : FAT_Filesystem) return Unsigned_32
   is (FS.Disk_Parameters.Table_Size_Fat32);

   function Number_Of_Hidden_Blocks
     (FS : FAT_Filesystem) return Unsigned_32
   is (FS.Disk_Parameters.Hidden_Blocks);

   function Drive_Number
     (FS : FAT_Filesystem) return Unsigned_8
   is (FS.Disk_Parameters.Drive_Number_Fat32);

   function Is_Volume
     (FS : FAT_Filesystem) return Boolean
   is (FS.Disk_Parameters.Boot_Signature_Fat32 = 16#29#);

   function Volume_ID
     (FS : FAT_Filesystem) return Unsigned_32
   is (if not Is_Volume (FS)
       then 0
       else FS.Disk_Parameters.Volume_Id_Fat32);

   function Volume_Label
     (FS : FAT_Filesystem) return String
   is (if not Is_Volume (FS)
       then "UNKNOWN"
       else Trim (FS.Disk_Parameters.Volume_Label_Fat32));

   function File_System_Type
     (FS : FAT_Filesystem) return String
   is (if not Is_Volume (FS)
       then "FAT32"
       else Trim (FS.Disk_Parameters.FS_Type_Fat32));

   function Flags_For_FAT_Mirroring
     (FS : FAT_Filesystem) return Unsigned_16
   is (FS.Disk_Parameters.Fat_Mirroring_Flags);

   function FS_Version_Number
     (FS : FAT_Filesystem) return Unsigned_16
   is (FS.Disk_Parameters.FS_Version_Number);

   function Root_Dir_Cluster
     (FS : FAT_Filesystem) return Cluster_Type
   is (FS.Disk_Parameters.Root_Directory_Cluster);

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
     (FS : FAT_Filesystem) return Cluster_Type
   is (FS.FSInfo.Last_Allocated_Cluster);

   Null_FAT_Volume : constant FAT_Filesystem_Access := null;

   type Entry_Index is new Unsigned_16;
   Null_Index : Entry_Index := 16#FFFF#;

   type Handle_Kind is
     (Handle_Mount_Point,
      Handle_FAT);

   type Directory_Handle (Kind : Handle_Kind := Handle_FAT) is record
      case Kind is
         when Handle_Mount_Point =>
            Current_Volume_Index : Integer := 0;
         when Handle_FAT =>
            FS                   : FAT_Filesystem_Access;
            Current_Index        : Entry_Index;
            Start_Cluster        : Cluster_Type;
            Current_Cluster      : Cluster_Type;
            Current_Block        : Unsigned_32;
      end case;
   end record;

   --  Type definition for implementation details, make them visible to all
   --  children of the package

   type FAT_Directory_Entry is record
      Filename   : String (1 .. 8);
      Extension  : String (1 .. 3);
      Attributes : FAT_Directory_Entry_Attribute;
      Reserved   : String (1 .. 8);
      Cluster_H  : Unsigned_16;
      Time       : Unsigned_16;
      Date       : Unsigned_16;
      Cluster_L  : Unsigned_16;
      Size       : Unsigned_32;
   end record with Size => 32 * 8;

   for FAT_Directory_Entry use record
      Filename   at 16#00# range 0 .. 63;
      Extension  at 16#08# range 0 .. 23;
      Attributes at 16#0B# range 0 .. 7;
      Reserved   at 16#0C# range 0 .. 63;
      Cluster_H  at 16#14# range 0 .. 15;
      Time       at 16#16# range 0 .. 15;
      Date       at 16#18# range 0 .. 15;
      Cluster_L  at 16#1A# range 0 .. 15;
      Size       at 16#1C# range 0 .. 31;
   end record;

   VFAT_Directory_Entry_Attribute : constant FAT_Directory_Entry_Attribute :=
                                      (Subdirectory => False,
                                       Archive      => False,
                                       others       => True);
   --  Attrite value 16#F0# defined at offset 16#0B# and identifying a VFAT
   --  entry rather than a regular directory entry

   type VFAT_Sequence_Number is mod 2 ** 6
     with Size => 6;

   type VFAT_Sequence is record
      Sequence : VFAT_Sequence_Number;
      Stop_Bit : Boolean;
   end record with Size => 8;

   for VFAT_Sequence use record
      Sequence at 0 range 0 .. 5;
      Stop_Bit at 0 range 6 .. 7;
   end record;

   type VFAT_Directory_Entry is record
      VFAT_Attr : VFAT_Sequence;
      Name_1    : Wide_String (1 .. 5);
      Attribute : FAT_Directory_Entry_Attribute;
      Reserved  : Unsigned_8 := 0;
      Checksum  : Unsigned_8;
      Name_2    : Wide_String (1 .. 6);
      Cluster   : Unsigned_16 := 0;
      Name_3    : Wide_String (1 .. 2);
   end record with Pack, Size => 32 * 8;

   function Get_Num_VFAT_Entries (Name : FAT_Name) return Natural
   is ((Name.Len + 12) / 13);
   --  Returns the number of VFAT Entries needed to encode 'Name'
   --  There's 13 characters in each entry, and we need Name.Len + 2 characters
   --  for the trailing ASCII.NUL + 0xFFFF sequence.

end FAT_Filesystem;
