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

package Virtual_File_System is

   type VFS is new HAL.Filesystem.FS_Driver with private;
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

   overriding
   function Truncate_File (This   : in out VFS;
                           Path   : Pathname;
                           Length : IO_Count)
                           return Status_Kind;

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

   type VFS_Directory_Handle is new HAL.Filesystem.Directory_Handle with record
      FS : access VFS;
   end record;

   overriding
   function Read_Entry (This         : in out VFS_Directory_Handle;
                        Entry_Number : Positive;
                        Dir_Entry    : out Directory_Entry)
                        return Status_Kind;

   overriding
   function Entry_Name (This         : in out VFS_Directory_Handle;
                        Entry_Number : Positive)
                        return Pathname;

   overriding
   function Close (This : in out VFS_Directory_Handle)
                   return Status_Kind;

   type VFS_Directory_Handle_Access is access all VFS_Directory_Handle;

   type Mount_Point;
   type Mount_Point_Access is access all Mount_Point;

   type Mount_Point is record
      Directory : not null access String;
      FS        : not null FS_Driver_Ref;
      Next      : Mount_Point_Access := null;
   end record;

   type VFS is new HAL.Filesystem.FS_Driver with record
      Mount_points : Mount_Point_Access;
      Dir_Handle   : aliased VFS_Directory_Handle;
   end record;

   function Find_FS (This                : in out VFS;
                     Path                : Pathname;
                     Path_Reminder_Start : out Integer)
                     return FS_Driver_Ref;
   --  Find the mount point for a given path and return the index of the first
   --  character of the remaining path. Path_Reminder_Start will be out of
   --  Path'Range is there is no remaining characters.


end Virtual_File_System;
