------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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

   subtype File_Data is Media_Reader.Block;

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
     (File   : in out File_Handle;
      Addr   : System.Address;
      Length : Unsigned_16) return Integer
     with Pre => Mode (File) = Read_Mode or else Mode (File) = Read_Write_Mode;
   --  read data from file.
   --  @return number of bytes read (at most Data'Length), or -1 on error.

   function File_Read
     (File : in out File_Handle;
      Data : out File_Data) return Integer
     with Pre => Mode (File) = Read_Mode or else Mode (File) = Read_Write_Mode;
   --  read data from file.
   --  @return number of bytes read (at most Data'Length), or -1 on error.

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
      Buffer_Level        : Unsigned_16 := 0; -- how much data in Buffer is meaningful
      Bytes_Total         : Unsigned_32 := 0; -- how many bytes were read/written
      D_Entry             : Directory_Entry; -- the associated directory entry
      Parent              : Directory_Entry;
   end record;
   --  used to access files

   function File_Size (Handle : File_Handle) return Unsigned_32
   is (Get_Size (Handle.D_Entry));

   function Mode (Handle : File_Handle) return File_Mode
     is (Handle.Mode);

end FAT_Filesystem.Files;
