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


--  Initial contribution by:
--
--  Institution: Technische Universitaet Muenchen
--  Department:  Real-Time Computer Systems (RCS)
--  Project:     StratoX
--  Authors:     Martin Becker (becker@rcs.ei.tum.de)
--
--  XXX! Nothing here is proven thread-safe!

with System;
with FAT_Filesystem;
with FAT_Filesystem.Directories; use FAT_Filesystem.Directories;

--  @summary File handling for FAT FS
package FAT_Filesystem.Files with SPARK_Mode => Off is

   type File_Handle is private;

   type File_Mode is (Read_Mode, Write_Mode, Read_Write_Mode);

   subtype File_Data is HAL.Block_Drivers.Block;

   function File_Open
     (Parent : Directory_Entry;
      Name   : FAT_Name;
      Mode   : File_Mode;
      File   : out File_Handle) return Status_Code
     with Pre => Is_Subdirectory (Parent);
   --  open the file given by the directory entry and return
   --  handle.
   --  if Mode is Read_Mode, then the file is returned with read-only access
   --  if Mode is Write_Mode, then if the file is created: if a file already
   --   exists it's content is wiped out
   --  if Mode is Read_Write_Mode, then the file is created if not exist, else
   --   its content is preserved but can be overwritten by calls to File_Write.

   function File_Size (Handle : File_Handle) return Unsigned_32;

   function Mode (Handle : File_Handle) return File_Mode;

   function File_Read
     (Handle : in out File_Handle;
      Addr   : System.Address;
      Length : Natural) return Integer
     with Pre => Mode (Handle) /= Write_Mode;
   --  read data from file.
   --  @return number of bytes read (at most Data'Length), or -1 on error.

   function File_Read
     (Handle : in out File_Handle;
      Data   : out File_Data) return Integer
     with Pre => Mode (Handle) /= Write_Mode;
   --  read data from file.
   --  @return number of bytes read (at most Data'Length), or -1 on error.

   function File_Offset
     (Handle : in out File_Handle) return Unsigned_32;
   --  Current index within the file

   generic
      type T is private;
   procedure Generic_Read
     (File : in out File_Handle;
      Data : out T);

   function File_Write
     (File   : in out File_Handle;
      Data   : File_Data) return Status_Code
     with
       Pre => Mode (File) = Write_Mode or else Mode (File) = Read_Write_Mode;
   --  write to file
   --  @return number of bytes written (at most Data'Length), or -1 on error.

   function File_Flush
     (File : in out File_Handle) return Status_Code;
   --  force writing file to disk at this very moment (slow!)

--     function File_Seek
--       (File     : in out File_Handle;
--        Position : Unsigned_32) return Status_Code;
   --  Moves the current file position to "Position"

   procedure File_Close (File : in out File_Handle);
   --  invalidates the handle, and ensures that
   --  everything is flushed to the disk

   function To_File_Data (S : String) return File_Data;
   --  convenience function to turn plain-text into file data

private
   pragma SPARK_Mode (Off);

   type File_Handle is record
      Is_Open             : Boolean := False;
      FS                  : FAT_Filesystem_Access;
      Mode                : File_Mode := Read_Mode;
      Current_Cluster     : Cluster_Type := 0; -- where we are currently reading/writing
      Current_Block       : Unsigned_32 := 0; -- same, but block
      Buffer              : Block (0 .. 511); --  size of one block in FAT/SD card
      Buffer_Level        : Natural := 0; -- how much data in Buffer is meaningful
      Bytes_Total         : Unsigned_32 := 0; -- how many bytes were read/written
      D_Entry             : Directory_Entry; -- the associated directory entry
      Parent              : Directory_Entry;
   end record;
   --  used to access files

   function File_Size (Handle : File_Handle) return Unsigned_32
   is (Get_Size (Handle.D_Entry));

   function Mode (Handle : File_Handle) return File_Mode
     is (Handle.Mode);

   function File_Offset
     (Handle : in out File_Handle) return Unsigned_32
     is (Handle.Bytes_Total);
end FAT_Filesystem.Files;
