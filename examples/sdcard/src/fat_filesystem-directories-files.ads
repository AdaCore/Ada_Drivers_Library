--  Institution: Technische Universitaet Muenchen
--  Department:  Real-Time Computer Systems (RCS)
--  Project:     StratoX
--  Authors:     Martin Becker (becker@rcs.ei.tum.de)
--
--  XXX! Nothing here is proven thread-safe!
with FAT_Filesystem;
with FAT_Filesystem.Directories; use FAT_Filesystem.Directories;

--  @summary File handling for FAT FS
package FAT_Filesystem.Directories.Files with SPARK_Mode => Off is

   type File_Handle is private;

   type File_Mode is (Read_Mode, Write_Mode);

   subtype File_Data is Media_Reader.Block;

   function File_Create
     (Parent    : in out Directory_Handle;
      newname   : String;
      Overwrite : Boolean := False;
      File      : out File_Handle) return Status_Code;
   --  create a new file in the given directory, and return
   --  handle for write access

   function File_Write
     (File   : in out File_Handle;
      Data   : File_Data;
      Status : out Status_Code) return Integer;
   --  write to file
   --  @return number of bytes written (at most Data'Length), or -1 on error.

   function File_Flush
     (File : in out File_Handle) return Status_Code;
   --  force writing file to disk at this very moment (slow!)

   function File_Size (File : File_Handle) return Unsigned_32;

   function File_Open_Readonly
     (Ent  : in out Directory_Entry;
      File : in out File_Data) return Status_Code
     with Pre => Is_System_File (Ent);
   --  open the file given by the directory entry and return
   --  handle for read access;

   function File_Read
     (File : in out File_Handle;
      Data : out File_Data) return Integer;
   --  read data from file.
   --  @return number of bytes read (at most Data'Length), or -1 on error.

   procedure File_Close (File : in out File_Handle);
   --  invalidates the handle, and ensures that
   --  everything is flushed to the disk

   function To_File_Data (S : String) return File_Data;
   --  convenience function to turn plain-text into file data

private
   pragma SPARK_Mode (Off);

   type File_Handle is record
      Is_Open             : Boolean := False;
      FS                  : FAT_Filesystem_Access;
      Mode                : File_Mode := Read_Mode;
      Start_Cluster       : Unsigned_32 := INVALID_CLUSTER; -- first cluster; file begin
      Current_Cluster     : Unsigned_32 := 0; -- where we are currently reading/writing
      Current_Block       : Unsigned_32 := 0; -- same, but block
      Buffer              : File_Data (1 .. 512); --  size of one block in FAT/SD card
      Buffer_Level        : Natural := 0; -- how much data in Buffer is meaningful
      Bytes_Total         : Unsigned_32 := 0; -- how many bytes were read/written
      D_Entry             : Directory_Entry; -- the associated directory entry
   end record;
   --  used to access files

   function Update_Entry (File : in out File_Handle) return Status_Code;
   --  maintain the directory entry (file size)

end FAT_Filesystem.Directories.Files;
