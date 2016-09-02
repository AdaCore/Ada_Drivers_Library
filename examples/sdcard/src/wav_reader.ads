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

with Interfaces; use Interfaces;

with FAT_Filesystem.Files;

package Wav_Reader is

   type WAV_Status_Code is
     (OK,
      Not_A_WAV_File,
      Internal_Error,
      Wrong_WAV_Format,
      Unexpected_Section,
      Cannot_Read);

   type Header_Block is record
      ID   : String (1 .. 4);
      Size : Unsigned_32;
   end record with Pack;

   type RIFF_Block is record
      Format_ID : String (1 .. 4);
   end record with Pack;

   type Audio_Format is
     (Unknown,
      PCM) with Size => 16;

   type Audio_Description_Block is record
      Format          : Audio_Format;
      Channels        : Unsigned_16;
      Frequency       : Unsigned_32;
      Byte_Per_Sec    : Unsigned_32;
      Byte_Per_Block  : Unsigned_16;
      Bits_Per_Sample : Unsigned_16;
   end record with Pack;

   type Metadata_Info is record
      Artist    : String (1 .. 32) := (others => ' ');
      Title     : String (1 .. 32) := (others => ' ');
      Album     : String (1 .. 32) := (others => ' ');
      Track_Num : Natural := 0;
      Year      : Natural := 0;
      Genre     : String (1 .. 32) := (others => ' ');
   end record;

   type WAV_Info is record
      Audio_Description : Audio_Description_Block;
      Metadata          : Metadata_Info;
   end record;

   function Read_Header
     (F    : in out FAT_Filesystem.Files.File_Handle;
      Info : out WAV_Info) return WAV_Status_Code;

end Wav_Reader;
