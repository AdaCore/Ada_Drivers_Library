------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
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

with Interfaces; use Interfaces;

--  Provide common interfaces to deal with filesystems

package HAL.Filesystem is

   subtype Pathname is String;

   type File_Kind is (Regular_File, Directory);

   type File_Mode is (Read_Only, Write_Only, Read_Write);

   type Status_Kind is (Status_Ok,
                        Invalid_FD,
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

   type User_ID is new Natural;
   type Group_ID is new Natural;
   type IO_Count is new Unsigned_64;

   type FS_Driver is limited interface;
   type Any_FS_Driver is access all FS_Driver'Class;
   --  Interface to provide access a filesystem

   type File_Handle is limited interface;
   type Any_File_Handle is access all File_Handle'Class;
   --  Interface to provide access to a regular file

   type Directory_Handle is limited interface;
   type Any_Directory_Handle is access all Directory_Handle'Class;
   --  Interface to provide access to a directory

   --  ??? Document error cases for all primitives below

   type Delimiter is record
      From, To : Integer;
   end record;

   type Path_Delimiters is array (Natural range <>) of Delimiter;

   ---------------
   -- FS_Driver --
   ---------------

   function Create_Node (This       : in out FS_Driver;
                         Path       : Pathname;
                         Delimiters : Path_Delimiters;
                         Kind       : File_Kind)
                         return Status_Kind is abstract;
   --  Create a Kind node in the This filesystem at the Path location

   function Create_Directory (This       : in out FS_Driver;
                              Path       : Pathname;
                              Delimiters : Path_Delimiters)
                              return Status_Kind is abstract;
   --  Shortcut for Create_Node (This, Path, Directory)

   function Unlink (This       : in out FS_Driver;
                    Path       : Pathname;
                    Delimiters : Path_Delimiters)
                    return Status_Kind is abstract;
   --  Remove the regular file located at Path in the This filesystem

   function Remove_Directory (This       : in out FS_Driver;
                              Path       : Pathname;
                              Delimiters : Path_Delimiters)
                              return Status_Kind is abstract;
   --  Remove the directory located at Path in the This filesystem

   function Rename (This     : in out FS_Driver;
                    Old_Path : Pathname;
                    New_Path : Pathname)
                    return Status_Kind is abstract;
   --  Move a node in the This filesystem

   function Truncate_File (This       : in out FS_Driver;
                           Path       : Pathname;
                           Delimiters : Path_Delimiters;
                           Length     : IO_Count)
                           return Status_Kind is abstract;
   --  Assuming Path designates a regular file, set its size to be Length. This
   --  operation preserves the first Length UInt8s and leaves the other with
   --  undefined values.

   function Open (This       : in out FS_Driver;
                  Path       : Pathname;
                  Delimiters : Path_Delimiters;
                  Mode       : File_Mode;
                  Handle     : out Any_File_Handle)
                  return Status_Kind is abstract;
   --  Assuming Path designates a regular file, open it with the given Mode and
   --  create a Handle for it.
   --
   --  The This filesystem owns the resulting handle. Callers are simply
   --  expected to call Close on it when done with the file.

   function Open_Directory (This       : in out FS_Driver;
                            Path       : Pathname;
                            Delimiters : Path_Delimiters;
                            Handle     : out Any_Directory_Handle)
                            return Status_Kind is abstract;
   --  Assuming Path designates a directory, open it and create a Handle for
   --  it.
   --
   --  The This filesystem owns the resulting handle. Callers are simply
   --  expected to call Close on it when done with the directory.
   --
   --  ??? Does this handler also contains entry for the current directory and
   --  the parent one?

   ------------------
   --  File_Handle --
   ------------------

   function Read (This : in out File_Handle;
                  Data : out UInt8_Array)
                  return Status_Kind is abstract;
   --  Read the next Data'Length UInt8s in This and put to in Data. If there
   --  isn't enough UInt8 to read to fill Data, return Input_Output_Error.

   function Write (This : in out File_Handle;
                   Data : UInt8_Array)
                   return Status_Kind is abstract;
   --  Write UInt8s in Data to This. This overrides the next Data'Length UInt8s in
   --  this file, if they exist, it extends the file otherwise.

   function Seek (This   : in out File_Handle;
                  Offset : IO_Count)
                  return Status_Kind is abstract;
   --  Set the read/write cursor of This to point to its UInt8 at the absolute
   --  Offset.
   --
   --  ??? What should happen if this offset is beyond the end of the file?

   function Close (This : in out File_Handle)
                   return Status_Kind is abstract;
   --  Close This. If this succeeds, This should not be used again afterwards.

   ----------------------
   -- Directory_Handle --
   ----------------------

   type Directory_Entry is record
      Entry_Type  : File_Kind;
   end record;

   function Read_Entry (This         : in out Directory_Handle;
                        Entry_Number : Positive;
                        Dir_Entry    : out Directory_Entry)
                        return Status_Kind is abstract;
   --  Read the Entry_Number'th entry in This. If it exists, store its
   --  details in Dir_Entry and return Status_Ok. Otherwise, return
   --  No_Such_File_Or_Directory.

   function Entry_Name (This         : in out Directory_Handle;
                        Entry_Number : Positive)
                        return Pathname is abstract;
   --  Return the name for the Entry_Number directory entry. This is a simple
   --  name (not a path). Return an empty string if this entry does not exist.

   function Close (This : in out Directory_Handle)
                   return Status_Kind is abstract;
   --  Close This. If this succeeds, This should not be used again afterwards.

end HAL.Filesystem;
