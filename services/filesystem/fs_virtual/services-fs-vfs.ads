package Services.FS.VFS is

   MAX_VOLUMES         : constant := 1;
   --  Maximum number of mounted volumes

   MAX_PATH_LENGTH     : constant := 1024;
   --  Maximum size of a path name length

   type VFS is new Services.FS.FS_Driver with private;
   type VFS_Ref is access all VFS'Class;

   function Mount (This       : in out VFS;
                   Path       : Pathname;
                   Filesystem : not null FS_Driver_Ref)
                   return Status_Kind;

   function Umount (This       : in out VFS;
                    Path       : Pathname)
                    return Status_Kind;

   -------------------------------
   --  FS_Driver implementation --
   -------------------------------

   overriding
   function Create_Node (This : in out VFS;
                         Path : Pathname;
                         Kind : File_Kind)
                         return Status_Kind;

   overriding
   function Create_Directory (This : in out VFS;
                              Path : Pathname)
                              return Status_Kind;

   overriding
   function Unlink (This : in out VFS;
                    Path : Pathname)
                    return Status_Kind;

   overriding
   function Remove_Directory (This : in out VFS;
                              Path : Pathname)
                              return Status_Kind;

   overriding
   function Rename (This : in out VFS;
                    Old_Path : Pathname;
                    New_Path : Pathname)
                    return Status_Kind;

--     overriding
--     function Truncate_File (This   : in out VFS;
--                             Path   : Pathname;
--                             Lenght : IO_Count)
--                             return Status_Kind;

   overriding
   function Open (This    : in out VFS;
                  Path    : Pathname;
                  Mode    : File_Mode;
                  Handler : out File_Handle_Ref)
                  return Status_Kind;

   overriding
   function Open_Directory (This   : in out VFS;
                            Path   : Pathname;
                            Handle : out Directory_Handle_Ref)
                            return Status_Kind;

private

   type Mount_Point;
   type Mount_Point_Access is access all Mount_Point;

   type Mount_Point is record
      Directory : not null access String;
      FS        : not null FS_Driver_Ref;
   end record;

   type Mount_Point_Array is array (1 .. MAX_VOLUMES) of Mount_Point;

   type VFS_Directory_Handle is new Services.FS.Directory_Handle with record
      FS : access VFS;
   end record;

   type VFS_Private_Cursor is new Directory_Private_Cursor with record
      Current : Mount_Point_Access;
   end record;

   overriding function Directory_Iter_First
     (This : VFS_Directory_Handle)
      return Directory_Private_Cursor'Class;

   overriding function Directory_Iter_Next
     (This : VFS_Directory_Handle;
      C    : Directory_Private_Cursor'Class)
      return Directory_Private_Cursor'Class;

   overriding function Directory_Iter_Has_Element
     (This : VFS_Directory_Handle;
      C    : Directory_Private_Cursor'Class) return Boolean;

   overriding function Directory_Iter_Element
     (This : VFS_Directory_Handle;
      C    : Directory_Private_Cursor'Class)
      return Directory_Entry'Class;

   overriding function Close
     (This : in out VFS_Directory_Handle)
      return Status_Kind;

   type VFS_Directory_Handle_Access is access all VFS_Directory_Handle;

   type Mount_Point;
   type Mount_Point_Access is access all Mount_Point;

   type Mount_Point is record
      Directory : not null access String;
      FS        : not null FS_Driver_Ref;
   end record;

   type VFS is new Services.FS.FS_Driver with record
      Mount_points : Mount_Point_Array;
      Dir_Handle   : aliased VFS_Directory_Handle;
   end record;

   function Find_FS (This                : in out VFS;
                     Path                : Pathname;
                     Path_Reminder_Start : out Integer)
                     return FS_Driver_Ref;
   --  Find the mount point for a given path and return the index of the first
   --  character of the remaining path. Path_Reminder_Start will be out of
   --  Path'Range is there is no remaining characters.


end Services.FS.VFS;
