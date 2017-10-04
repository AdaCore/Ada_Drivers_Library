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

--  This packages provide a low level driver for the FAT file system
--  architecture. It is recommended to _not_ use this interface directly but to
--  access the file system using the File_IO package. For more info, see the
--  file system chapter of the documentation.

--  Initial contribution by:
--
--  Institution: Technische Universitaet Muenchen
--  Department:  Real-Time Computer Systems (RCS)
--  Project:     StratoX
--  Authors:     Martin Becker (becker@rcs.ei.tum.de)
--
--  XXX! Nothing here is proven thread-safe!

with System;

--  @summary File handling for FAT FS
private package Filesystem.FAT.Files is

   type File_Data is array (FAT_File_Size range <>) of HAL.UInt8;

   function Open
     (Parent : FAT_Node;
      Name   : FAT_Name;
      Mode   : File_Mode;
      File   : FAT_File_Handle_Access) return Status_Code;
   --  open the file given by the directory entry and return
   --  handle.
   --  if Mode is Read_Mode, then the file is returned with read-only access
   --  if Mode is Write_Mode, then if the file is created: if a file already
   --   exists it's content is wiped out
   --  if Mode is Read_Write_Mode, then the file is created if not exist, else
   --   its content is preserved but can be overwritten by calls to File_Write.

--     function Size (File : access FAT_File_Handle) return FAT_File_Size;
--
--     function Mode (File : access FAT_File_Handle) return File_Mode;

   function Read
     (File   : in out FAT_File_Handle;
      Addr   : System.Address;
      Length : in out FAT_File_Size) return Status_Code;
   --  read data from file.
   --  @return number of bytes read (at most Data'Length), or -1 on error.

--     function Offset
--       (File : access FAT_File_Handle) return FAT_File_Size;
--     --  Current index within the file
--
   function Write
     (File   : in out FAT_File_Handle;
      Addr   : System.Address;
      Length : FAT_File_Size) return Status_Code;
   --  write to file
   --  @return number of bytes written (at most Data'Length), or -1 on error.

   function Flush
     (File : in out FAT_File_Handle) return Status_Code;
   --  force writing file to disk at this very moment (slow!)

   function Seek
     (File   : in out FAT_File_Handle;
      Amount : in out FAT_File_Size;
      Origin : Seek_Mode) return Status_Code;
   --  Moves the current file position from "Origin" of "Amount" bytes.

   procedure Close (File : in out FAT_File_Handle);
   --  invalidates the handle, and ensures that
   --  everything is flushed to the disk

end Filesystem.FAT.Files;
