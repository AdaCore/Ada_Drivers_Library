private with Ada.Containers.Vectors;
private with Ada.Direct_IO;
private with Ada.Strings.Unbounded;

with HAL;            use HAL;
with HAL.Filesystem; use HAL.Filesystem;

--  Simple wrappers around the Ada standard library to provide implementations
--  for HAL.Filesystem interfaces.

package Native.Filesystem is

   ----------------------
   -- Native_FS_Driver --
   ----------------------

   type Native_FS_Driver is limited new FS_Driver with private;
   type Native_FS_Driver_Ref is access Native_FS_Driver;

   procedure Destroy (This : in out Native_FS_Driver_Ref);

   function Create
     (FS       : out Native_FS_Driver;
      Root_Dir : Pathname)
      return Status_Kind;
   --  Create a Native_FS_Driver considering Root_Dir as its root directory.
   --  All other pathnames in this API are processed as relative to it.
   --
   --  Note that this does not provide real isolation: trying to access the
   --  ".." directory will access the parent of the root directory, not the
   --  root directory itself.

   overriding function Create_Node
     (This : in out Native_FS_Driver;
      Path : Pathname;
      Kind : File_Kind)
      return Status_Kind;

   overriding function Create_Directory
     (This : in out Native_FS_Driver;
      Path : Pathname)
      return Status_Kind;

   overriding function Unlink
     (This : in out Native_FS_Driver;
      Path : Pathname)
      return Status_Kind;

   overriding function Remove_Directory
     (This : in out Native_FS_Driver;
      Path : Pathname)
      return Status_Kind;

   overriding function Rename
     (This     : in out Native_FS_Driver;
      Old_Path : Pathname;
      New_Path : Pathname)
      return Status_Kind;

   overriding function Truncate_File
     (This   : in out Native_FS_Driver;
      Path   : Pathname;
      Length : IO_Count)
      return Status_Kind;

   overriding function Open
     (This   : in out Native_FS_Driver;
      Path   : Pathname;
      Mode   : File_Mode;
      Handle : out File_Handle_Ref)
      return Status_Kind;

   overriding function Open_Directory
     (This   : in out Native_FS_Driver;
      Path   : Pathname;
      Handle : out Directory_Handle_Ref)
      return Status_Kind;

   ------------------------
   -- Native_File_Handle --
   ------------------------

   type Native_File_Handle is limited new File_Handle with private;
   type Native_File_Handle_Ref is access Native_File_Handle;

   overriding function Read
     (This : in out Native_File_Handle;
      Data : out Byte_Array)
      return Status_Kind;

   overriding function Write
     (This : in out Native_File_Handle;
      Data : Byte_Array)
      return Status_Kind;

   overriding function Seek
     (This   : in out Native_File_Handle;
      Offset : IO_Count)
      return Status_Kind;

   overriding function Close
     (This : in out Native_File_Handle)
      return Status_Kind;

   -----------------------------
   -- Native_Directory_Handle --
   -----------------------------

   type Native_Directory_Handle is limited new Directory_Handle with private;
   type Native_Directory_Handle_Ref is access Native_Directory_Handle;

   overriding function Read_Entry
     (This         : in out Native_Directory_Handle;
      Entry_Number : Positive;
      Dir_Entry    : out Directory_Entry)
      return Status_Kind;

   overriding function Entry_Name
     (This         : in out Native_Directory_Handle;
      Entry_Number : Positive)
      return Pathname;

   overriding function Close
     (This : in out Native_Directory_Handle)
      return Status_Kind;

   -------------
   -- Helpers --
   -------------

   function Join
     (Prefix, Suffix           : Pathname;
      Ignore_Absolute_Suffixes : Boolean)
      return Pathname;
   --  Like Ada.Directories.Compose, but also accepts a full path as Suffix.
   --  For instance:
   --
   --     Join ("/a", "b")   => "/a/b"
   --     Join ("/a", "b/c") => "/a/b/c"
   --
   --  If Ignore_Absolute_Suffixes is True, if Suffix is an absolute path, do
   --  as if it was a relative path instead:
   --
   --     Join ("/a", "/b")  => "/a/b"
   --
   --  Otherwise, if sufifx is an absolute path, just return Suffix.

private

   package Byte_IO is new Ada.Direct_IO (Byte);

   type Native_FS_Driver is limited new FS_Driver with record
      Root_Dir          : Ada.Strings.Unbounded.Unbounded_String;
      --  Path on the host file system to be used as root directory for this FS

      Free_File_Handles : Native_File_Handle_Ref;
      --  Linked list of file handles available to use

      Free_Dir_Handles : Native_Directory_Handle_Ref;
      --  Likend list of directory handles available to use
   end record;

   function Get_Handle
     (FS : in out Native_FS_Driver)
      return Native_File_Handle_Ref;
   function Get_Handle
     (FS : in out Native_FS_Driver)
      return Native_Directory_Handle_Ref;
   --  Return an existing free handle or create one if none is available

   procedure Add_Free_Handle
     (FS     : in out Native_FS_Driver;
      Handle : in out Native_File_Handle_Ref)
     with Pre => Handle /= null,
     Post => Handle = null;
   procedure Add_Free_Handle
     (FS     : in out Native_FS_Driver;
      Handle : in out Native_Directory_Handle_Ref)
     with Pre => Handle /= null,
     Post => Handle = null;
   --  Add Handle to the list of available handles

   type Native_File_Handle is limited new File_Handle with record
      FS   : Native_FS_Driver_Ref;
      --  The filesystem that owns this handle

      Next : Native_File_Handle_Ref;
      --  If this handle is used, this is undefined. Otherwise,
      --  this is the next free handle in the list (see
      --  Native_FS_Driver.Free_File_Handles).

      File : Byte_IO.File_Type;
   end record;

   type Directory_Data_Entry is record
      Kind : File_Kind;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   --  Set of information we handle for a directory entry

   package Directory_Data_Vectors is new Ada.Containers.Vectors
     (Positive, Directory_Data_Entry);

   type Native_Directory_Handle is limited new Directory_Handle with record
      FS   : Native_FS_Driver_Ref;
      --  The filesystem that owns this handle

      Next      : Native_Directory_Handle_Ref;
      --  If this handle is used, this is undefined. Otherwise,
      --  this is the next free handle in the list (see
      --  Native_FS_Driver.Free_Dir_Handles).

      Full_Name : Ada.Strings.Unbounded.Unbounded_String;
      --  Absolute path for this directory

      Data : Directory_Data_Vectors.Vector;
      --  Vector of entries for this directory.
      --
      --  On one hand, HAL.Filesystem exposes an index-based API to access
      --  directory entries. On the other hand, Ada.Directories exposes a
      --  kind of single-linked list. What we do here is that when we open a
      --  directory, we immediately get all entries and build a vector out of
      --  it for convenient random access.
   end record;

end Native.Filesystem;
