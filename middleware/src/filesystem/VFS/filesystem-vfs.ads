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

--  Simple virtual file system allowing to mount actual fs into a virtual
--  fs environment composed of 1 level of virtual directories.

with HAL.Filesystem; use HAL.Filesystem;
with HAL.Block_Drivers;

package Filesystem.VFS is

   MAX_MOUNT_POINTS : constant := 2;
   MAX_MOUNT_NAME_LENGTH : constant := 128;

   subtype Mount_Path is String
     with Dynamic_Predicate => Mount_Path'Length <= MAX_MOUNT_NAME_LENGTH;

   function Mount_Volume
     (Mount_Point : Mount_Path;
      FS          : Any_Filesystem) return Status_Code;

   function Mount_Drive
     (Mount_Point : Mount_Path;
      Device      : HAL.Block_Drivers.Any_Block_Driver) return Status_Code;

   function Unmount (Mount_Point : Mount_Path) return Status_Code;

   function Open
     (Path   : String;
      Handle : out Any_Directory_Handle)
      return Status_Code;

   function Open
     (Path   : String;
      Mode   : File_Mode;
      Handle : out Any_File_Handle)
      return Status_Code;

   function Unlink (Path : String) return Status_Code;
   function Remove_Directory (Path : String) return Status_Code;

private

   type Mount_Record is record
      Is_Free  : Boolean := True;
      Name     : String (1 .. MAX_MOUNT_NAME_LENGTH);
      Name_Len : Positive;
      FS       : Any_Filesystem;
   end record;

   subtype Mount_Index is Integer range 0 .. MAX_MOUNT_POINTS;
   subtype Valid_Mount_Index is Mount_Index range 1 .. MAX_MOUNT_POINTS;
   type Mount_Array is array (Valid_Mount_Index) of Mount_Record;

   type VFS_Directory_Handle is new Directory_Handle with record
      Is_Free  : Boolean := True;
      Mount_Id : Mount_Index;
   end record;

   overriding function Get_FS
     (Dir : VFS_Directory_Handle) return Any_Filesystem;
   --  Return the filesystem the handle belongs to.

   overriding function Read
     (Dir    : in out VFS_Directory_Handle;
      Handle : out Any_Node_Handle) return Status_Code;
   --  Reads the next directory entry. If no such entry is there, an error
   --  code is returned in Status.

   overriding procedure Reset (Dir : in out VFS_Directory_Handle);
   --  Resets the handle to the first node

   overriding procedure Close (Dir : in out VFS_Directory_Handle);
   --  Closes the handle, and free the associated resources.

end Filesystem.VFS;
