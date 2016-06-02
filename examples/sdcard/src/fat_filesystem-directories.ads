package FAT_Filesystem.Directories is

   type Directory_Handle is private;

   function Open_Root_Directory
     (FS  : FAT_Filesystem_Access;
      Dir : out Directory_Handle) return Status_Code;

   type Directory_Entry is private;

   function Open
     (E   : Directory_Entry;
      Dir : out Directory_Handle) return Status_Code
   with Pre => Is_Subdirectory (E);

   procedure Close (Dir : in out Directory_Handle);

   function Read (Dir : in out Directory_Handle;
                  DEntry : out Directory_Entry) return Status_Code;

   function Name (E : Directory_Entry) return String;

   function Is_Read_Only (E : Directory_Entry) return Boolean;

   function Is_Hidden (E : Directory_Entry) return Boolean;

   function Is_System_File (E : Directory_Entry) return Boolean;

   function Is_Subdirectory (E : Directory_Entry) return Boolean;

   function Is_Archive (E : Directory_Entry) return Boolean;

private


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

   type Directory_Handle is record
      FS              : FAT_Filesystem_Access;
      Current_Index   : Unsigned_16;
      Start_Cluster   : Unsigned_32;
      Current_Cluster : Unsigned_32;
      Current_Block   : Unsigned_32;
   end record;

   type Directory_Entry is record
      FS            : FAT_Filesystem_Access;
      L_Name        : String (1 .. 128);
      L_Name_First  : Natural := 129;
      S_Name        : String (1 .. 12);
      S_Name_Last   : Natural := 0;
      Attributes    : FAT_Directory_Entry_Attribute;
      Start_Cluster : Unsigned_32;
      Size          : Unsigned_32;
   end record;

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
