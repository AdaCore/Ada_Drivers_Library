------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

--  This package use the ARM semihosting feature to provide a low level driver
--  to access the host file system. It is recommended to _not_ use this
--  interface directly but to access the file system using the File_IO package.
--  For more info, see the file system chapter of the documentation.

with HAL.Filesystem; use HAL.Filesystem;
with HAL; use HAL;

package Semihosting.Filesystem is

   type SHFS is new HAL.Filesystem.Filesystem_Driver with private;
   type Any_SHFS is access all SHFS'Class;

   type File_Kind is (Regular_File, Directory);

   -------------------------------
   --  FS_Driver implementation --
   -------------------------------

   overriding
   function Create_File (This : in out SHFS;
                         Path : String)
                         return Status_Code;

   function Create_Node (This : in out SHFS;
                         Path : String;
                         Kind : File_Kind)
                         return Status_Code;

   function Create_Directory (This : in out SHFS;
                              Path : String)
                              return Status_Code;

   overriding
   function Unlink (This : in out SHFS;
                    Path : String)
                    return Status_Code;

   overriding
   function Remove_Directory (This : in out SHFS;
                              Path : String)
                              return Status_Code;

   function Rename (This : in out SHFS;
                    Old_Path : String;
                    New_Path : String)
                    return Status_Code;

   function Truncate_File (This   : in out SHFS;
                           Path   : String;
                           Length : File_Size)
                           return Status_Code;

   overriding
   function Open (This    : in out SHFS;
                  Path    : String;
                  Mode    : File_Mode;
                  Handler : out Any_File_Handle)
                  return Status_Code;

   overriding
   function Open (This   : in out SHFS;
                  Path   : String;
                  Handle : out Any_Directory_Handle)
                  return Status_Code;

   overriding
   function Root_Node
     (This   : in out SHFS;
      As     : String;
      Handle : out Any_Node_Handle)
      return Status_Code;

   overriding
   procedure Close (This : in out SHFS);

private

   type SHFS_File_Handle;
   type SHFS_File_Handle_Access is access all SHFS_File_Handle;

   type SHFS_File_Handle is new File_Handle with record
      FS                : Any_SHFS;
      FD                : SH_Word;
      Absolute_Position : File_Size;
      Is_Open           : Boolean := False;
      Next              : SHFS_File_Handle_Access := null;
   end record;

   overriding
   function Get_FS
     (This : in out SHFS_File_Handle)
      return Any_Filesystem_Driver;

   overriding
   function Size
     (This : SHFS_File_Handle) return File_Size;

   overriding
   function Mode
     (This : SHFS_File_Handle) return File_Mode;

   overriding
   function Read (This   : in out SHFS_File_Handle;
                  Addr   : System.Address;
                  Length : in out File_Size)
                  return Status_Code;

   overriding
   function Write (This   : in out SHFS_File_Handle;
                   Addr   : System.Address;
                   Length : File_Size)
                   return Status_Code;

   overriding
   function Offset
     (This : SHFS_File_Handle)
      return File_Size;

   overriding
   function Flush
     (This : in out SHFS_File_Handle)
      return Status_Code;

   overriding
   function Seek (This   : in out SHFS_File_Handle;
                  Origin : Seek_Mode;
                  Amount : in out File_Size)
                  return Status_Code;

   overriding
   procedure Close (This : in out SHFS_File_Handle);

   type SHFS is new HAL.Filesystem.Filesystem_Driver with record
      File_Handles : SHFS_File_Handle_Access := null;
   end record;

   function Get_File_Handle (This : in out SHFS)
                             return not null SHFS_File_Handle_Access;
   --  This function will find an existing free file handle or allocate a new
   --  one if necessary.
end Semihosting.Filesystem;
