with Ada.Unchecked_Deallocation;

with HAL;                 use HAL;
with HAL.Filesystem;      use HAL.Filesystem;
with Native.Filesystem;
with Virtual_File_System; use Virtual_File_System;

--  Helpers for Virtual_File_System testcases

package Helpers is

   procedure Test (Status : Status_Kind);
   --  Check that status is Status_Ok

   function Create
     (Root_Dir          : Pathname;
      Create_If_Missing : Boolean := False)
      return Native.Filesystem.Native_FS_Driver_Ref;
   --  Create a native FS driver rooted at Root_Dir, in the Material_Name
   --  material directory.

   function Read_File (File : in out File_Handle'Class) return Byte_Array;
   --  Read the whole content of File and return it. Raise a Program_Error if
   --  anything goes wrong.

   function Quote_Bytes (Bytes : Byte_Array) return String;
   --  Return a human-readable representation of Bytes, considered as ASCII

   procedure Dump (FS : in out FS_Driver'Class; Dir : Pathname);
   --  Dump the content of the Dir directory in FS to the standard output

   procedure Destroy is new Ada.Unchecked_Deallocation
     (FS_Driver'Class, FS_Driver_Ref);
   procedure Destroy is new Ada.Unchecked_Deallocation
     (Virtual_File_System.VFS'Class, Virtual_File_System.VFS_Ref);

end Helpers;
