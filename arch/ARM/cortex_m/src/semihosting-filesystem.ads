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

with HAL.Filesystem; use HAL.Filesystem;
with HAL; use HAL;

package Semihosting.Filesystem is

   type SHFS is new HAL.Filesystem.FS_Driver with private;
   type Any_SHFS is access all SHFS'Class;

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
   function Truncate_File (This   : in out SHFS;
                           Path   : Pathname;
                           Length : IO_Count)
                           return Status_Kind;

   overriding
   function Open (This    : in out SHFS;
                  Path    : Pathname;
                  Mode    : File_Mode;
                  Handler : out Any_File_Handle)
                  return Status_Kind;

   overriding
   function Open_Directory (This   : in out SHFS;
                            Path   : Pathname;
                            Handle : out Any_Directory_Handle)
                            return Status_Kind;
private

   type SHFS_File_Handle;
   type SHFS_File_Handle_Access is access all SHFS_File_Handle;

   type SHFS_File_Handle is new File_Handle with record
      FD      : SH_Word;
      Is_Open : Boolean := False;
      Next    : SHFS_File_Handle_Access := null;
   end record;

   overriding
   function Read (This : in out SHFS_File_Handle;
                  Data : out UInt8_Array)
                  return Status_Kind;

   overriding
   function Write (This : in out SHFS_File_Handle;
                   Data : UInt8_Array)
                   return Status_Kind;

   overriding
   function Seek (This   : in out SHFS_File_Handle;
                  Offset : IO_Count)
                  return Status_Kind;

   overriding
   function Close (This   : in out SHFS_File_Handle)
                  return Status_Kind;

   type SHFS is new HAL.Filesystem.FS_Driver with record
      File_Handles : SHFS_File_Handle_Access := null;
   end record;

   function Get_File_Handle (This : in out SHFS)
                             return not null SHFS_File_Handle_Access;
   --  This function will find an existing free file handle or allocate a new
   --  one if necessary.
end Semihosting.Filesystem;
