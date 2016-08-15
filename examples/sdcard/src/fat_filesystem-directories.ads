--  Project: StratoX
--  System:  Stratosphere Balloon Flight Controller
--  Author: Martin Becker (becker@rcs.ei.tum.de)
--  based on AdaCore's Ada_Driver_Library
--  XXX! Nothing here is thread-safe!

--  @summary Directory (end directory entries) handling for FAT FS
package FAT_Filesystem.Directories with SPARK_Mode => Off is

   type Directory_Handle is private; -- used to read directories

   function Open_Root_Directory
     (FS  : FAT_Filesystem_Access;
      Dir : out Directory_Handle) return Status_Code;

   type Directory_Entry is private; -- used to represent one item in directory

   function Open
     (E   : Directory_Entry;
      Dir : out Directory_Handle) return Status_Code
     with Pre => Is_Subdirectory (E);
   --  get handle of given item. Handle can be used with Read().

   function Make_Directory
     (Parent  : in out Directory_Handle;
      newname : String;
      D_Entry : out Directory_Entry) return Status_Code
     with Pre => newname'Length < 12;
   --  create a new directory within the given one
   --  we only allow short names for now.
   --  if directory already exists, returns its entry.

   procedure Close (Dir : in out Directory_Handle);

   function Read (Dir     : in out Directory_Handle;
                  DEntry  : out Directory_Entry;
                  Deleted : Boolean := False) return Status_Code;
   --  @summary get the next entry in the given directory
   --  after calling this, Dir.Dir_Current are invalid iff return /= OK.
   --  However, the Dir_Begin and Dir_End components are always valid.
   --  if Deleted is true, then deleted entries are also returned and
   --  marked with FS=null

   function Get_Name (E : Directory_Entry) return String;

   function Is_Read_Only (E : Directory_Entry) return Boolean;

   function Is_Hidden (E : Directory_Entry) return Boolean;

   function Is_System_File (E : Directory_Entry) return Boolean;

   function Is_Subdirectory (E : Directory_Entry) return Boolean;

   function Is_Archive (E : Directory_Entry) return Boolean;

private
   pragma SPARK_Mode (Off);

   type FAT_Directory_Entry_Attribute is record
      Read_Only : Boolean;
      Hidden    : Boolean;
      System_File : Boolean;
      Volume_Label : Boolean;
      Subdirectory : Boolean;
      Archive      : Boolean;
   end record with Size => 8, Pack;

   type FAT_Directory_Entry is record
      Filename   : String (1 .. 8);
      Extension  : String (1 .. 3);
      Attributes : FAT_Directory_Entry_Attribute;
      Reserved   : String (1 .. 8);
      Cluster_H  : Unsigned_16;
      Time       : Unsigned_16;
      Date       : Unsigned_16;
      Cluster_L  : Unsigned_16;
      Size       : Unsigned_32; -- TODO: what is this?
   end record with Size => 32 * 8; --  32 Byte per entry

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

   ENTRY_SIZE : constant := 32;

   VFAT_Directory_Entry_Attribute : constant FAT_Directory_Entry_Attribute :=
                                      (Subdirectory => False,
                                       Archive      => False,
                                       others       => True);
   --  Attrite value 16#F0# defined at offset 16#0B# and identifying a VFAT
   --  entry rather than a regular directory entry

   type VFAT_Sequence_Number is mod 2 ** 5
     with Size => 5;

   type VFAT_Sequence is record
      Sequence : VFAT_Sequence_Number;
      Stop_Bit : Boolean;
   end record with Size => 8, Pack;

   type VFAT_Directory_Entry is record
      VFAT_Attr : VFAT_Sequence;
      Name_1    : Wide_String (1 .. 5);
      Attribute : FAT_Directory_Entry_Attribute;
      E_Type    : Unsigned_8;
      Checksum  : Unsigned_8;
      Name_2    : Wide_String (1 .. 6);
      Cluster   : Unsigned_16;
      Name_3    : Wide_String (1 .. 2);
   end record with Pack, Size => 32 * 8;

--     type File_Object_Structure is record
--        FS              : FAT_Filesystem_Access;
--        Flags           : Unsigned_8;
--        Err             : Unsigned_8;
--        File_Ptr        : Unsigned_32 := 0;
--        File_Size       : Unsigned_32;
--        Start_Cluster   : Unsigned_32;
--        Current_Cluster : Unsigned_32;
--     end record;

   --  FIXME: encapsulate this, and provide function "move_forward" or something.
   type Directory_Handle_Pointer is record
      Index   : Unsigned_16;
      Cluster : Unsigned_32;
      Block   : Unsigned_32;
   end record;

   procedure Invalidate_Handle_Pointer (h : in out Directory_Handle_Pointer);
   function  Valid_Handle_Pointer (h : Directory_Handle_Pointer) return Boolean;

   type Directory_Handle is record
      FS          : FAT_Filesystem_Access;
      Dir_Begin   : Directory_Handle_Pointer; -- points to the first entry of the directory
      Dir_Current : Directory_Handle_Pointer; -- points to the current, valid entry
      Dir_End     : Directory_Handle_Pointer; -- points past the last valid entry
   end record;
   --  used to read directories

   type Directory_Entry is record
      FS               : FAT_Filesystem_Access;

      Name             : String (1 .. 128);
      Name_First       : Natural := 129; -- where the string starts within 'Name'
      Name_Last        : Natural := 0; -- where the string ends within 'Name'

      CRC              : Unsigned_8 := 0;

      Attributes       : FAT_Directory_Entry_Attribute;
      Start_Cluster    : Unsigned_32; -- the address of the data/contents of the entry
      Size             : Unsigned_32;
      Entry_Address    : FAT_Address; -- the address of the entry itself
      --  FIXME: add time stamps, attributes, etc.
   end record;
   --  each item in a directory is described by this in high-level view

   procedure Rewind (Dir : in out Directory_Handle);

   function Get_Entry_Or_Deleted
     (Parent   : in out Directory_Handle;
      E_Name   : String;
      Ent      : out Directory_Entry;
      Deleted  : out Boolean) return Boolean;
   --  search for entry with the given name.
   --  if Deleted is true, then the 'Ent' points
   --  to an empty directory entry that can be
   --  re-used. If Deleted is false, then an entry
   --  with the given name was actually found

   function Is_Deleted (F_Entry : FAT_Directory_Entry) return Boolean;

   function Get_Entry
     (Parent   : in out Directory_Handle;
      E_Name   : String;
      Ent      : out Directory_Entry) return Boolean;
   --  search for entry with the given name.

   procedure Goto_Last_Entry (Parent   : in out Directory_Handle);
   --  proceed to last entry in given directory

   function Allocate_Entry
     (Parent   : in out Directory_Handle;
      New_Name : String;
      Ent_Addr : out FAT_Address) return Status_Code;
   --  find a location for a new entry within Parent_Ent
   --  and make sure that the directory stays terminated

   procedure Set_Name (newname : String; D : in out Directory_Entry)
     with Pre => newname'Length > 0;

   procedure Set_Name (newname : String; E : in out FAT_Directory_Entry)
     with Pre => newname'Length > 0;

   function Directory_To_FAT_Entry
     (D_Entry : in Directory_Entry;
      F_Entry : out FAT_Directory_Entry) return Status_Code;

   function FAT_To_Directory_Entry
     (FS : FAT_Filesystem_Access;
      F_Entry : in FAT_Directory_Entry;
      D_Entry : in out Directory_Entry;
      Last_Seq : in out VFAT_Sequence_Number) return Status_Code;
   --  @summary decypher the raw entry (file/dir name, etc) and write
   --           to record.
   --  @return OK when decipher is complete, otherwise needs to be
   --          called again with the next entry (VFAT entries have
   --          multiple parts)

   function Is_Read_Only (E : Directory_Entry) return Boolean
   is (E.Attributes.Read_Only);

   function Is_Hidden (E : Directory_Entry) return Boolean
   is (E.Attributes.Hidden);

   function Is_System_File (E : Directory_Entry) return Boolean
   is (E.Attributes.System_File);

   function Is_Subdirectory (E : Directory_Entry) return Boolean
   is (E.Attributes.Subdirectory);

   function Is_Archive (E : Directory_Entry) return Boolean
   is (E.Attributes.Archive);

end FAT_Filesystem.Directories;
