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

with System;

package HAL.Filesystem is
   type Status_Code is
     (OK,
      Non_Empty_Directory,
      Disk_Error, --  A hardware error occurred in the low level disk I/O
      Disk_Full,
      Internal_Error,
      Drive_Not_Ready,
      No_Such_File,
      No_Such_Path,
      Not_Mounted, --  The mount point is invalid
      Invalid_Name,
      Access_Denied,
      Already_Exists,
      Invalid_Object_Entry,
      Write_Protected,
      Invalid_Drive,
      No_Filesystem, --  The volume is not a FAT volume
      Locked,
      Too_Many_Open_Files, --  All available handles are used
      Invalid_Parameter,
      Input_Output_Error,
      No_MBR_Found,
      No_Partition_Found,
      No_More_Entries,
      Read_Only_File_System,
      Operation_Not_Permitted);

   type File_Mode is (Read_Only, Write_Only, Read_Write);
   type Seek_Mode is
     (
      --  Seek from the beginning of the file, forward
      From_Start,
      --  Seek from the end of the file, backward
      From_End,
      --  Seek from the current position, forward
      Forward,
      --  Seek from the current position, backward
      Backward);

   type File_Size is new HAL.UInt64;
   --  Modern fs all support 64-bit file size. Only old or limited ones support
   --  max 32-bit (FAT in particular). So let's see big and not limit ourselves
   --  in this API with 32-bit only.

   type Filesystem_Driver is limited interface;
   type Any_Filesystem_Driver is access all Filesystem_Driver'Class;

   type Directory_Handle is limited interface;
   type Any_Directory_Handle is access all Directory_Handle'Class;

   type File_Handle is limited interface;
   type Any_File_Handle is access all File_Handle'Class;

   type Node_Handle is interface;
   type Any_Node_Handle is access all Node_Handle'Class;

   ---------------------------
   --  Directory operations --
   ---------------------------

   function Open
     (This   : in out Filesystem_Driver;
      Path   : String;
      Handle : out Any_Directory_Handle)
      return Status_Code is abstract;
   --  Open a new Directory Handle at the given Filesystem_Driver Path

   function Create_File (This : in out Filesystem_Driver;
                         Path : String)
                         return Status_Code is abstract;

   function Unlink (This : in out Filesystem_Driver;
                    Path : String)
                    return Status_Code is abstract;
   --  Remove the regular file located at Path in the This filesystem_Driver

   function Remove_Directory (This : in out Filesystem_Driver;
                              Path : String)
                              return Status_Code is abstract;
   --  Remove the directory located at Path in the This filesystem_Driver

   function Get_FS
     (This : Directory_Handle) return Any_Filesystem_Driver
      is abstract;
   --  Return the filesystem_Driver the handle belongs to.

   function Root_Node
     (This   : in out Filesystem_Driver;
      As     : String;
      Handle : out Any_Node_Handle)
      return Status_Code is abstract;
   --  Open a new Directory Handle at the given Filesystem_Driver Path

   function Read
     (This   : in out Directory_Handle;
      Handle : out Any_Node_Handle)
      return Status_Code is abstract;
   --  Reads the next directory entry. If no such entry is there, an error
   --  code is returned in Status.

   procedure Reset (This : in out Directory_Handle) is abstract;
   --  Resets the handle to the first node

   procedure Close (This : in out Directory_Handle) is abstract;
   --  Closes the handle, and free the associated resources.

   ---------------------
   -- Node operations --
   ---------------------

   function Get_FS (This : Node_Handle) return Any_Filesystem_Driver is abstract;

   function Basename (This : Node_Handle) return String is abstract;

   function Is_Read_Only (This : Node_Handle) return Boolean is abstract;

   function Is_Hidden (This : Node_Handle) return Boolean is abstract;

   function Is_Subdirectory (This : Node_Handle) return Boolean is abstract;

   function Is_Symlink (This : Node_Handle) return Boolean is abstract;

   function Size (This : Node_Handle) return File_Size is abstract;

   procedure Close (This : in out Node_Handle) is abstract;

   ---------------------
   -- File operations --
   ---------------------

   function Open
     (This   : in out Filesystem_Driver;
      Path   : String;
      Mode   : File_Mode;
      Handle : out Any_File_Handle)
      return Status_Code is abstract;
   --  Open a new File Handle at the given Filesystem_Driver Path

   function Open
     (This   : Node_Handle;
      Name   : String;
      Mode   : File_Mode;
      Handle : out Any_File_Handle)
      return Status_Code is abstract
     with Pre'Class => Is_Subdirectory (This);

   function Get_FS
     (This : in out File_Handle) return Any_Filesystem_Driver is abstract;

   function Size
     (This : File_Handle) return File_Size is abstract;

   function Mode
     (This : File_Handle) return File_Mode is abstract;

   function Read
     (This   : in out File_Handle;
      Addr   : System.Address;
      Length : in out File_Size)
      return Status_Code is abstract
     with Pre'Class => Mode (This) in Read_Only | Read_Write;

   function Write
     (This   : in out File_Handle;
      Addr   : System.Address;
      Length : File_Size)
      return Status_Code is abstract
     with
       Pre'Class => Mode (This) in Write_Only | Read_Write;

   function Offset
     (This : File_Handle)
      return File_Size is abstract;

   function Flush
     (This : in out File_Handle)
      return Status_Code is abstract;

   function Seek
     (This   : in out File_Handle;
      Origin : Seek_Mode;
      Amount : in out File_Size)
      return Status_Code is abstract;

   procedure Close (This : in out File_Handle) is abstract;

   -------------------
   -- FS operations --
   -------------------

   procedure Close (This : in out Filesystem_Driver) is abstract;

end HAL.Filesystem;
