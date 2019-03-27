------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2019, AdaCore                     --
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

--  This packages provide a low level driver for the FAT file system
--  architecture. It is recommended to _not_ use this interface directly but to
--  access the file system using the File_IO package. For more info, see the
--  file system chapter of the documentation.

with System;
with Interfaces;        use Interfaces;
with HAL;               use HAL;
with HAL.Block_Drivers; use HAL.Block_Drivers;
with HAL.Filesystem;    use HAL.Filesystem;

package Filesystem.FAT is

   MAX_VOLUMES         : constant := 1;
   --  Maximum number of mounted volumes

   MAX_FILENAME_LENGTH : constant := 255;
   --  Maximum size of a file or directory name

   MAX_FILE_HANDLES    : constant := 10;
   --  Maximum number of handles opened simultaneously.

   MAX_DIR_HANDLES     : constant := 10;
   --  Maximum number of handles opened simultaneously.

   type FAT_Name is private;

   type FAT_Filesystem is limited new Filesystem_Driver with private;

   type FAT_Filesystem_Access is access all FAT_Filesystem;

   type FAT_Node is new Node_Handle with private;

   -----------------------
   -- PATH MANIPULATION --
   -----------------------

   function "-" (Name : FAT_Name) return String;

   function "-" (Name : String) return FAT_Name
     with Pre => Name'Length < MAX_FILENAME_LENGTH;

   overriding function "=" (Name1, Name2 : FAT_Name) return Boolean;

   function Is_Root (Path : String) return Boolean with Inline_Always;

   function Parent (Path : String) return String;
   function Basename (Path : String) return String;

   function Normalize (Path       : String;
                       Ensure_Dir : Boolean := False) return String;

   ------------------------
   -- DIRECTORY HANDLING --
   ------------------------

   overriding function Open
     (FS     : in out FAT_Filesystem;
      Path   : String;
      Handle : out Any_Directory_Handle) return Status_Code;

   overriding function Root_Node
     (FS     : in out FAT_Filesystem;
      As     : String;
      Handle : out Any_Node_Handle)
      return Status_Code;
   function Long_Name (E : FAT_Node) return FAT_Name;
   function Short_Name (E : FAT_Node) return FAT_Name;
   overriding function Basename (E : FAT_Node) return String;
   overriding function Is_Read_Only (E : FAT_Node) return Boolean;
   overriding function Is_Hidden (E : FAT_Node) return Boolean;
   function Is_System_File (E : FAT_Node) return Boolean;
   overriding function Is_Symlink (E : FAT_Node) return Boolean;
   overriding function Is_Subdirectory
     (E : FAT_Node) return Boolean;
   function Is_Archive (E : FAT_Node) return Boolean;
   overriding function Size (E : FAT_Node) return File_Size;
   overriding procedure Close (E : in out FAT_Node);
   overriding function Get_FS
     (E : FAT_Node) return Any_Filesystem_Driver;

   -------------------
   -- FILE HANDLING --
   -------------------

   overriding function Open
     (FS     : in out FAT_Filesystem;
      Path   : String;
      Mode   : File_Mode;
      Handle : out Any_File_Handle)
      return Status_Code;

   overriding function Open
     (Parent : FAT_Node;
      Name   : String;
      Mode   : File_Mode;
      Handle : out Any_File_Handle)
      return Status_Code
   with Pre => Name'Length <= MAX_FILENAME_LENGTH;

   --------------------
   -- FAT FILESYSTEM --
   --------------------

   function Open
     (Controller  : HAL.Block_Drivers.Any_Block_Driver;
      LBA         : Block_Number;
      FS          : in out FAT_Filesystem)
      return Status_Code;
   --  Opens a FAT partition at the given LBA

   overriding procedure Close (FS : in out FAT_Filesystem);

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

   function Is_Volume
     (FS : FAT_Filesystem) return Boolean;
   function Volume_ID
     (FS : FAT_Filesystem) return Unsigned_32;
   function Volume_Label
     (FS : FAT_Filesystem) return String;
   function File_System_Type
     (FS : FAT_Filesystem) return String;

private

   type Cluster_Type is new Interfaces.Unsigned_32;
   subtype Valid_Cluster is Cluster_Type range 2 .. 16#0FFF_FFFF#;
   type Block_Offset is new Interfaces.Unsigned_32;
   type FAT_File_Size is new Interfaces.Unsigned_32;
   --  FAT Filesystem does not support files >= 4GB (e.g. 2**32)

   INVALID_CLUSTER    : constant Cluster_Type := 0;
   FREE_CLUSTER_VALUE : constant Cluster_Type := 16#0000_0000#;
   LAST_CLUSTER_VALUE : constant Cluster_Type := 16#0FFF_FFFF#;
   BAD_CLUSTER_VALUE  : constant Cluster_Type := 16#0FFF_FFF7#;

   function Get_Start_Cluster
     (E : FAT_Node) return Cluster_Type;
   function Size (E : FAT_Node) return FAT_File_Size;

   function Block_Size
     (FS : FAT_Filesystem) return FAT_File_Size;
   function Blocks_Per_Cluster
     (FS : FAT_Filesystem) return Block_Offset;
   function Cluster_Size
     (FS : FAT_Filesystem) return FAT_File_Size;
   function Reserved_Blocks
     (FS : FAT_Filesystem) return Unsigned_16;
   function Number_Of_FATs
     (FS : FAT_Filesystem) return Unsigned_8;
   function Total_Number_Of_Blocks
     (FS : FAT_Filesystem) return Unsigned_32;
   function FAT_Table_Size_In_Blocks
     (FS : FAT_Filesystem) return Unsigned_32;
   function Number_Of_Hidden_Blocks
     (FS : FAT_Filesystem) return Unsigned_32;
   function Root_Dir_Cluster
     (FS : FAT_Filesystem) return Cluster_Type;
   function FAT16_Root_Dir_Num_Entries
     (FS : FAT_Filesystem) return Unsigned_16;
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

   type FAT_Name is record
      Name : String (1 .. MAX_FILENAME_LENGTH);
      Len  : Natural := 0;
   end record;

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

   type FAT_Filesystem is limited new Filesystem_Driver with record
      Initialized     : Boolean := False;
      Disk_Parameters : FAT_Disk_Parameter;
      LBA             : Block_Number;
      Controller      : Any_Block_Driver;
      FSInfo          : FAT_FS_Info;
      FSInfo_Changed  : Boolean := False;
      Root_Dir_Area   : Block_Offset := 0;
      Data_Area       : Block_Offset; --  address to the data area, rel. to LBA
      FAT_Addr        : Block_Offset; --  address to the FAT table, rel. to LBA
      Num_Clusters    : Cluster_Type;
      Window_Block    : Block_Offset := Block_Offset'Last;
      Window          : Block (0 .. 511);
      FAT_Block       : Block_Offset := Block_Offset'Last;
      FAT_Window      : Block (0 .. 511);
      Root_Entry      : aliased FAT_Node;
   end record;

   function Ensure_Block
     (FS                : in out FAT_Filesystem;
      Block             : Block_Offset)
      return Status_Code;
   --  Ensures the block is visible within the FS window.
   --  Block_Base_OFfset returns the index within the FS window of the block

   function Write_Window (FS : in out FAT_Filesystem)
                          return Status_Code;

   function Cluster_To_Block
     (FS      : FAT_Filesystem;
      Cluster : Cluster_Type) return Block_Offset
   is (FS.Data_Area + Block_Offset (Cluster - 2) * FS.Blocks_Per_Cluster);

   function "+" (Base : Block_Number;
                 Off  : Block_Offset) return Block_Number
   is (Base + Block_Number (Off));

   function Get_FAT
     (FS      : in out FAT_Filesystem;
      Cluster : Cluster_Type) return Cluster_Type;

   function Set_FAT
     (FS      : in out FAT_Filesystem;
      Cluster : Cluster_Type;
      Value   : Cluster_Type)
      return Status_Code;

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

   type Entry_Index is new Unsigned_16;
   Null_Index : Entry_Index := 16#FFFF#;

   type FAT_Directory_Handle is limited new Directory_Handle with record
      Is_Free         : Boolean := True;
      FS              : FAT_Filesystem_Access;
      Current_Index   : Entry_Index;
      Start_Cluster   : Cluster_Type;
      Current_Cluster : Cluster_Type;
      Current_Block   : Block_Offset;
      Current_Node    : aliased FAT_Node;
   end record;

   type FAT_Directory_Handle_Access is access all FAT_Directory_Handle;
   type Any_FAT_Directory_Handle is access all FAT_Directory_Handle'Class;

   overriding function Get_FS
     (Dir : FAT_Directory_Handle) return Any_Filesystem_Driver;

   overriding function Read
     (Dir    : in out FAT_Directory_Handle;
      Handle : out Any_Node_Handle)
      return Status_Code;

   overriding procedure Reset (Dir : in out FAT_Directory_Handle);

   overriding procedure Close (Dir : in out FAT_Directory_Handle);

   overriding
   function Create_File (This : in out FAT_Filesystem;
                         Path : String)
                         return Status_Code;

   overriding
   function Unlink (This : in out FAT_Filesystem;
                    Path : String)
                    return Status_Code;

   overriding
   function Remove_Directory (This : in out FAT_Filesystem;
                              Path : String)
                              return Status_Code;
   function FAT_Open
     (FS     : in out FAT_Filesystem;
      Path   : String;
      Handle : out FAT_Directory_Handle_Access)
      return Status_Code;

   function FAT_Open
     (D_Entry : FAT_Node;
      Handle  : out FAT_Directory_Handle_Access)
      return Status_Code;

   type FAT_Directory_Entry_Attribute is record
      Read_Only    : Boolean;
      Hidden       : Boolean;
      System_File  : Boolean;
      Volume_Label : Boolean;
      Subdirectory : Boolean;
      Archive      : Boolean;
   end record with Size => 8, Pack;

   type FAT_Node is new Node_Handle with record
      FS            : FAT_Filesystem_Access;
      L_Name        : FAT_Name;
      S_Name        : String (1 .. 8);
      S_Name_Ext    : String (1 .. 3);
      Attributes    : FAT_Directory_Entry_Attribute;
      Start_Cluster : Cluster_Type; --  The content of this entry
      Size          : FAT_File_Size;

      Index         : Entry_Index;
      --  Index of the FAT_Directory_Intry within Parent's content

      Is_Root       : Boolean := False;
      --  Is it the root directory ?

      Is_Dirty      : Boolean := False;
      --  Whether changes need to be written on disk
   end record;

   type FAT_File_Handle is limited new File_Handle with record
      Is_Free         : Boolean := True;
      FS              : FAT_Filesystem_Access;
      Mode            : File_Mode;
      --  The current cluster from which we read or write
      Current_Cluster : Cluster_Type := 0;
      --  The current block from which we read or write, offset from
      --  current_cluster base block
      Current_Block   : Block_Offset := 0;
      --  Buffer with the content of the current block
      Buffer          : Block (0 .. 511);
      --  How much data in Buffer is meaningful
      Buffer_Filled   : Boolean := False;
      --  Whether there's a discrepency between the disk data and the buffer
      Buffer_Dirty    : Boolean := False;
      --  The actual file index
      File_Index      : FAT_File_Size := 0;
      --  The associated directory entry
      D_Entry         : FAT_Node;
      --  The parent's directory directory entry
      Parent          : FAT_Node;
   end record;

   type FAT_File_Handle_Access is access all FAT_File_Handle;

   overriding function Get_FS
     (File : in out FAT_File_Handle) return Any_Filesystem_Driver;

   overriding function Size (File : FAT_File_Handle)
                             return File_Size;

   overriding function Mode (File : FAT_File_Handle)
                             return File_Mode;

   overriding function Read
     (File   : in out FAT_File_Handle;
      Addr   : System.Address;
      Length : in out File_Size)
      return Status_Code;
   --  read data from file.
   --  @return number of bytes read (at most Data'Length), or -1 on error.

   overriding function Offset
     (File : FAT_File_Handle)
      return File_Size;
   --  Current index within the file

   overriding function Write
     (File   : in out FAT_File_Handle;
      Addr   : System.Address;
      Length : File_Size)
      return Status_Code;
   --  write to file
   --  @return number of bytes written (at most Data'Length), or -1 on error.

   overriding function Flush
     (File : in out FAT_File_Handle)
      return Status_Code;
   --  force writing file to disk at this very moment (slow!)

   overriding function Seek
     (File   : in out FAT_File_Handle;
      Origin : Seek_Mode;
      Amount : in out File_Size)
      return Status_Code;
   --  Moves the current file position to "Amount", according to the Origin
   --  parameter. If the command makes the file pointer move outside of the
   --  file, it stops at the file boundary and returns the actual amount of
   --  bytes moved.

   overriding procedure Close (File : in out FAT_File_Handle);
   --  invalidates the handle, and ensures that
   --  everything is flushed to the disk

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
      Size       : FAT_File_Size;
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

   --------------------------------------------------
   -- Inlined implementations of utility functions --
   --------------------------------------------------

   function Version
     (FS : FAT_Filesystem) return FAT_Version
   is (if FS.Disk_Parameters.Root_Dir_Entries_Fat16 /= 0
       then FAT16 else FAT32);

   function OEM_Name (FS : FAT_Filesystem) return String
   is (FS.Disk_Parameters.OEM_Name);

   function Block_Size
     (FS : FAT_Filesystem) return FAT_File_Size
   is (FAT_File_Size (FS.Disk_Parameters.Block_Size_In_Bytes));

   function Blocks_Per_Cluster
     (FS : FAT_Filesystem) return Block_Offset
   is (Block_Offset (FS.Disk_Parameters.Blocks_Per_Cluster));

   function Cluster_Size
     (FS : FAT_Filesystem) return FAT_File_Size
   is (FAT_File_Size (FS.Blocks_Per_Cluster) * FS.Block_Size);

   function Reserved_Blocks
     (FS : FAT_Filesystem) return Unsigned_16
   is (FS.Disk_Parameters.Reserved_Blocks);

   function Number_Of_FATs
     (FS : FAT_Filesystem) return Unsigned_8
   is (FS.Disk_Parameters.Number_Of_FATs);

   function Total_Number_Of_Blocks
     (FS : FAT_Filesystem) return Unsigned_32
   is (FS.Disk_Parameters.Number_Of_Blocks_Fat32);

   function FAT_Table_Size_In_Blocks
     (FS : FAT_Filesystem) return Unsigned_32
   is ((if FS.Version = FAT16
        then Unsigned_32 (FS.Disk_Parameters.Table_Size_Fat16)
        else FS.Disk_Parameters.Table_Size_Fat32));

   function Number_Of_Hidden_Blocks
     (FS : FAT_Filesystem) return Unsigned_32
   is (FS.Disk_Parameters.Hidden_Blocks);

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
   is (if FS.Version = FAT16 then "UNKNOWN"
       elsif not Is_Volume (FS)
       then "UNKNOWN"
       else Trim (FS.Disk_Parameters.Volume_Label_Fat32));

   function File_System_Type
     (FS : FAT_Filesystem) return String
   is (if FS.Version = FAT16 then "FAT16"
       elsif not Is_Volume (FS) then "FAT32"
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

   function FAT16_Root_Dir_Num_Entries
     (FS : FAT_Filesystem) return Unsigned_16
   is (FS.Disk_Parameters.Root_Dir_Entries_Fat16);

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

   function Get_Num_VFAT_Entries (Name : FAT_Name) return Natural
   is ((Name.Len + 12) / 13);
   --  Returns the number of VFAT Entries needed to encode 'Name'
   --  There's 13 characters in each entry, and we need Name.Len + 2 characters
   --  for the trailing ASCII.NUL + 0xFFFF sequence.

   overriding function Get_FS
     (Dir : FAT_Directory_Handle) return Any_Filesystem_Driver
   is (Any_Filesystem_Driver (Dir.FS));

   overriding function Get_FS
     (File : in out FAT_File_Handle) return Any_Filesystem_Driver
   is (Any_Filesystem_Driver (File.FS));

   overriding function Get_FS (E : FAT_Node) return Any_Filesystem_Driver
   is (Any_Filesystem_Driver (E.FS));

   function Long_Name (E : FAT_Node) return FAT_Name
   is (if E.L_Name.Len > 0 then E.L_Name else Short_Name (E));

   function Short_Name (E : FAT_Node) return FAT_Name
   is (-(Trim (E.S_Name) &
         (if E.S_Name_Ext /= "   "
          then "." & E.S_Name_Ext
          else "")));

   overriding function Basename (E : FAT_Node) return String
   is (-E.Long_Name);

   overriding function Is_Read_Only (E : FAT_Node) return Boolean
   is (E.Attributes.Read_Only);

   overriding function Is_Hidden (E : FAT_Node) return Boolean
   is (E.Attributes.Hidden);

   function Is_System_File (E : FAT_Node) return Boolean
   is (E.Attributes.System_File);

   overriding function Is_Subdirectory (E : FAT_Node) return Boolean
   is (E.Attributes.Subdirectory);

   function Is_Archive (E : FAT_Node) return Boolean
   is (E.Attributes.Archive);

   function Get_Start_Cluster (E : FAT_Node) return Cluster_Type
   is (E.Start_Cluster);

   function Size (E : FAT_Node) return FAT_File_Size
   is (E.Size);

   overriding function Is_Symlink (E : FAT_Node) return Boolean
   is (False);

end Filesystem.FAT;
