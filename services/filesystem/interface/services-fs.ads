with Interfaces; use Interfaces;
with HAL;        use HAL;

package Services.FS is

   subtype Pathname is String;

   type File_Kind is (Regular_File, Directory);

   type File_Mode is (Read_Only, Write_Only, Read_Write);

   type Status_Kind is (Status_Ok,
                        Symbolic_Links_Loop,
                        Permission_Denied,
                        Input_Output_Error,
                        No_Such_File_Or_Directory,
                        Filename_Is_Too_Long,
                        Not_A_Directory,
                        Representation_Overflow,
                        Invalid_Argument,
                        Not_Enough_Space,
                        Not_Enough_Memory,
                        Bad_Address,
                        File_Exists,
                        Read_Only_File_System,
                        Operation_Not_Permitted,
                        No_Space_Left_On_Device,
                        Too_Many_Links,
                        Resource_Busy,
                        Buffer_Is_Too_Small,
                        Read_Would_Block,
                        Call_Was_Interrupted);

   type IO_Count is new Unsigned_64;

   type FS_Driver is limited interface;
   type FS_Driver_Ref is access all FS_Driver'Class;

   type File_Handle is interface;
   type Directory_Handle is interface;

   type Directory_Iterator is limited private with
     Iterable => (First       => Directory_Iter_First,
                  Has_Element => Directory_Iter_Has_Element,
                  Next        => Directory_Iter_Next,
                  Element     => Directory_Iter_Element);

   function Iter (Handle : Directory_Handle) return Directory_Iterator;

   type Directory_Entry is interface;

   ---------------
   -- FS_Driver --
   ---------------

   function Create_Node (This : in out FS_Driver;
                         Path : Pathname;
                         Kind : File_Kind)
                         return Status_Kind is abstract;

   function Create_Directory (This : in out FS_Driver;
                              Path : Pathname)
                              return Status_Kind is abstract;

   function Unlink (This : in out FS_Driver;
                    Path : Pathname)
                    return Status_Kind is abstract;

   function Remove_Directory (This : in out FS_Driver;
                              Path : Pathname)
                              return Status_Kind is abstract;

   function Rename (This     : in out FS_Driver;
                    Old_Path : Pathname;
                    New_Path : Pathname)
                    return Status_Kind is abstract;

   function Open (This   : in out FS_Driver;
                  Path   : Pathname;
                  Mode   : File_Mode;
                  Handle : out File_Handle_Ref)
                  return Status_Kind is abstract;

   function Open_Directory (This   : in out FS_Driver;
                            Path   : Pathname;
                            Handle : out Directory_Handle_Ref)
                            return Status_Kind is abstract;

   ------------------
   --  File_Handle --
   ------------------

   function Read (This : in out File_Handle;
                  Data : out Byte_Array)
                  return Status_Kind is abstract;

   function Write (This : in out File_Handle;
                   Data : Byte_Array)
                   return Status_Kind is abstract;

   type File_Offset_Origin is
     (Relative,
      From_Start,
      From_End);

   function Seek (This   : in out File_Handle;
                  Origin : File_Offset_Origin;
                  Offset : IO_Count)
                  return Status_Kind is abstract;

   function Position (This : in out File_Handle) return IO_Count is abstract;

   function Truncate (This   : in out File_Handle;
                      Path   : Pathname;
                      Lenght : IO_Count)
                      return Status_Kind is abstract;

   function Close (This : in out File_Handle)
                   return Status_Kind is abstract;

   ----------------------
   -- Directory_Handle --
   ----------------------

   type Directory_Private_Cursor is interface;

   function Directory_Iter_First
     (This : Directory_Handle)
      return Directory_Private_Cursor'Class is abstract;

   function Directory_Iter_Next
     (This : Directory_Handle;
      C    : Directory_Private_Cursor'Class)
      return Directory_Private_Cursor'Class is abstract;

   function Directory_Iter_Has_Element
     (This : Directory_Handle;
      C    : Directory_Private_Cursor'Class) return Boolean is abstract;

   function Directory_Iter_Element
     (This : Directory_Handle;
      C    : Directory_Private_Cursor'Class)
      return Directory_Entry'Class is abstract;

   function Close (This : in out Directory_Handle)
                   return Status_Kind is abstract;

   ---------------------
   -- Directory_Entry --
   ---------------------

   function Name (This : Directory_Entry) return String is abstract;
   function Kind (This : Directory_Entry) return File_Kind is abstract;

end Services.FS;
