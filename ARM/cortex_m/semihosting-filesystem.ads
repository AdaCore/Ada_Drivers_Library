with HAL.Filesystem; use HAL.Filesystem;
with HAL; use HAL;

package Semihosting.Filesystem is

   type SHFS is new HAL.Filesystem.FS_Driver with private;
   type SHFS_Ref is access all SHFS'Class;

   -------------------------------
   --  FS_Driver implementation --
   -------------------------------

   overriding
   function Create_Node (This : in out SHFS;
                         Path : Pathname;
                         Kind : File_Kind)
                         return Status_Kind;

   overriding
   function Create_Directory (This : in out SHFS;
                              Path : Pathname)
                              return Status_Kind;

   overriding
   function Unlink (This : in out SHFS;
                    Path : Pathname)
                    return Status_Kind;

   overriding
   function Remove_Directory (This : in out SHFS;
                              Path : Pathname)
                              return Status_Kind;

   overriding
   function Rename (This : in out SHFS;
                    Old_Path : Pathname;
                    New_Path : Pathname)
                    return Status_Kind;

   overriding
   function Change_Permissions (This        : in out SHFS;
                                Path        : Pathname;
                                Permissions : Permission_Set)
                                return Status_Kind;

   overriding
   function Change_Owner_And_Group (This  : in out SHFS;
                                    Path  : Pathname;
                                    Owner : User_ID;
                                    Group : Group_ID)
                                    return Status_Kind;

   overriding
   function Change_Permissions (This   : in out SHFS;
                                Path   : Pathname;
                                Lenght : IO_Count)
                                return Status_Kind;

   overriding
   function Open (This    : in out SHFS;
                  Path    : Pathname;
                  Mode    : File_Mode;
                  Handler : out File_Handle_Ref)
                  return Status_Kind;

   overriding
   function Open_Directory (This   : in out SHFS;
                            Path   : Pathname;
                            Handle : out Directory_Handle_Ref)
                            return Status_Kind;
private

   type SHFS_File_Handle is new File_Handle with record
      FD : SH_Word;
   end record;

   type SHFS_File_Handle_Access is access all SHFS_File_Handle;

   overriding
   function Read (This : in out SHFS_File_Handle;
                  Data : out Byte_Array)
                  return Status_Kind;

   overriding
   function Write (This : in out SHFS_File_Handle;
                   Data : Byte_Array)
                   return Status_Kind;

   overriding
   function Seek (This   : in out SHFS_File_Handle;
                  Offset : IO_Count)
                  return Status_Kind;

   overriding
   function Close (This   : in out SHFS_File_Handle)
                  return Status_Kind;

   type SHFS is new HAL.Filesystem.FS_Driver with null record;

end Semihosting.Filesystem;
