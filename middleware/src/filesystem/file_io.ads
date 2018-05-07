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

--  This package provides a user friendly interface to the file system. For
--  more info, see the file system chapter of the documentation.

with System;
with HAL.Block_Drivers;

with HAL.Filesystem;

with ADL_Config;

package File_IO is

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


   type File_Descriptor is limited private;

   function Open
     (File : in out File_Descriptor;
      Name : String;
      Mode : File_Mode)
      return Status_Code;

   procedure Close  (File : in out File_Descriptor);

   function Is_Open (File : File_Descriptor)
                     return Boolean;

   function Flush (File : File_Descriptor)
                   return Status_Code;

   function Size (File : File_Descriptor)
                  return File_Size;

   function Read (File   : File_Descriptor;
                  Addr   : System.Address;
                  Length : File_Size)
                  return File_Size;

   function Write (File   : File_Descriptor;
                   Addr   : System.Address;
                   Length : File_Size)
                   return File_Size;

   function Offset (File : File_Descriptor)
                    return File_Size;

   function Seek
     (File   : in out File_Descriptor;
      Origin : Seek_Mode;
      Amount : in out File_Size) return Status_Code;

   generic
      type T is private;
   function Generic_Write
     (File  : File_Descriptor;
      Value : T) return Status_Code;

   generic
      type T is private;
   function Generic_Read
     (File  : File_Descriptor;
      Value : out T) return Status_Code;

   type Directory_Descriptor is limited private;

   function Open
     (Dir  : in out Directory_Descriptor;
      Name : String)
      return Status_Code;

   procedure Close (Dir : in out Directory_Descriptor);

   type Directory_Entry (Name_Length : Natural) is record
      Name         : String (1 .. Name_Length);
      Subdirectory : Boolean;
      Read_Only    : Boolean;
      Hidden       : Boolean;
      Symlink      : Boolean;
      Size         : File_Size;
   end record;

   Invalid_Dir_Entry : constant Directory_Entry;

   function Read (Dir : in out Directory_Descriptor)
                  return Directory_Entry;

   procedure Reset (Dir : in out Directory_Descriptor);

   function Create_File (Path : String) return Status_Code;

   function Unlink (Path : String) return Status_Code;

   function Remove_Directory (Path : String) return Status_Code;

   function Copy_File (Source_Path, Destination_Path : String;
                       Buffer_Size : Positive := 512)
                       return Status_Code;

   --------------
   -- Mounting --
   --------------

   Max_Mount_Points : constant := ADL_Config.Max_Mount_Points;
   Max_Mount_Name_Length : constant := ADL_Config.Max_Mount_Name_Length;

   subtype Mount_Path is String
     with Dynamic_Predicate => Mount_Path'Length <= Max_Mount_Name_Length;

   function Mount_Volume
     (Mount_Point : Mount_Path;
      FS          : HAL.Filesystem.Any_Filesystem_Driver) return Status_Code;

   function Mount_Drive
     (Mount_Point : Mount_Path;
      Device      : HAL.Block_Drivers.Any_Block_Driver) return Status_Code;

   function Unmount (Mount_Point : Mount_Path) return Status_Code;

private

   type File_Descriptor is limited record
      Handle : HAL.Filesystem.Any_File_Handle := null;
   end record;

   type Directory_Descriptor is limited record
      Handle : HAL.Filesystem.Any_Directory_Handle := null;
   end record;

   Invalid_Dir_Entry : constant Directory_Entry (Name_Length => 0) :=
     (Name_Length  => 0,
      Name         => "",
      Subdirectory => False,
      Read_Only    => False,
      Hidden       => False,
      Symlink      => False,
      Size         => 0);

   for Status_Code use
     (OK                      => HAL.Filesystem.OK'Enum_Rep,
      Non_Empty_Directory     => HAL.Filesystem.Non_Empty_Directory'Enum_Rep,
      Disk_Error              => HAL.Filesystem.Disk_Error'Enum_Rep,
      Disk_Full               => HAL.Filesystem.Disk_Full'Enum_Rep,
      Internal_Error          => HAL.Filesystem.Internal_Error'Enum_Rep,
      Drive_Not_Ready         => HAL.Filesystem.Drive_Not_Ready'Enum_Rep,
      No_Such_File            => HAL.Filesystem.No_Such_File'Enum_Rep,
      No_Such_Path            => HAL.Filesystem.No_Such_Path'Enum_Rep,
      Not_Mounted             => HAL.Filesystem.Not_Mounted'Enum_Rep,
      Invalid_Name            => HAL.Filesystem.Invalid_Name'Enum_Rep,
      Access_Denied           => HAL.Filesystem.Access_Denied'Enum_Rep,
      Already_Exists          => HAL.Filesystem.Already_Exists'Enum_Rep,
      Invalid_Object_Entry    => HAL.Filesystem.Invalid_Object_Entry'Enum_Rep,
      Write_Protected         => HAL.Filesystem.Write_Protected'Enum_Rep,
      Invalid_Drive           => HAL.Filesystem.Invalid_Drive'Enum_Rep,
      No_Filesystem           => HAL.Filesystem.No_Filesystem'Enum_Rep,
      Locked                  => HAL.Filesystem.Locked'Enum_Rep,
      Too_Many_Open_Files     => HAL.Filesystem.Too_Many_Open_Files'Enum_Rep,
      Invalid_Parameter       => HAL.Filesystem.Invalid_Parameter'Enum_Rep,
      Input_Output_Error      => HAL.Filesystem.Input_Output_Error'Enum_Rep,
      No_MBR_Found            => HAL.Filesystem.No_MBR_Found'Enum_Rep,
      No_Partition_Found      => HAL.Filesystem.No_Partition_Found'Enum_Rep,
      No_More_Entries         => HAL.Filesystem.No_More_Entries'Enum_Rep,
      Read_Only_File_System   => HAL.Filesystem.Read_Only_File_System'Enum_Rep,
      Operation_Not_Permitted => HAL.Filesystem.Operation_Not_Permitted'Enum_Rep);

   for File_Mode use
     (Read_Only  => HAL.Filesystem.Read_Only'Enum_Rep,
      Write_Only => HAL.Filesystem.Write_Only'Enum_Rep,
      Read_Write => HAL.Filesystem.Read_Write'Enum_Rep);

   for Seek_Mode use
     (From_Start => HAL.Filesystem.From_Start'Enum_Rep,
      From_End   => HAL.Filesystem.From_End'Enum_Rep,
      Forward    => HAL.Filesystem.Forward'Enum_Rep,
      Backward   => HAL.Filesystem.Backward'Enum_Rep);

end File_IO;
