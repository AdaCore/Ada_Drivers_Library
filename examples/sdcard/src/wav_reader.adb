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

with FAT_Filesystem.Files; use FAT_Filesystem.Files;
with Ada.Unchecked_Conversion;

package body Wav_Reader is

   generic
      type T is private;
   function Read
     (Block : File_Data;
      Idx   : in out Unsigned_16) return T;

   function Read
     (Block : File_Data;
      Idx   : in out Unsigned_16) return T
   is
      subtype Buffer is File_Data (1 .. T'Size / 8);
      function Convert is new Ada.Unchecked_Conversion
        (Buffer, T);
      Ret : T;
   begin
      Ret := Convert (Block (Idx .. Idx + Buffer'Length - 1));
      Idx := Idx + Buffer'Length;

      return Ret;
   end Read;

   ---------------
   -- To_String --
   ---------------

   function To_String (Block : File_Data) return String
   is
      subtype B is File_Data (1 .. Block'Length);
      subtype S is String (1 .. Block'Length);
      function To_S is new Ada.Unchecked_Conversion (B, S);
   begin
      return To_S (Block);
   end To_String;

   -----------------
   -- Read_Header --
   -----------------

   function Read_Header
     (F : in out FAT_Filesystem.Files.File_Handle;
      Info : out WAV_Info) return WAV_Status_Code
   is
      subtype ID is String (1 .. 4);
      function Read_Header is new Read (Header_Block);
      function Read_RIFF is new Read (RIFF_Block);
      function Read_Audio is new Read (Audio_Description_Block);
      function Read_ID is new Read (ID);

      procedure Read_String
        (H : Header_Block;
         S : in out String);

      Header            : Header_Block;
      RIFF_Header       : RIFF_Block;
      Buffer            : File_Data (1 .. 512)
        with Alignment => 32;
      Index             : Unsigned_16 := Buffer'First;
      Index_Info        : Unsigned_16;
      Num               : Integer;

      procedure Read_String
        (H : Header_Block;
         S : in out String)
      is
      begin
         if H.Size - 1 > S'Length then
            S := To_String (Buffer (Index_Info .. Index_Info + S'Length - 1));
         else
            S (S'First .. S'First + Integer (H.Size - 2)) :=
              To_String
                (Buffer (Index_Info .. Index_Info + Unsigned_16 (H.Size) - 2));
         end if;
      end Read_String;

   begin
      Num := File_Read (F, Buffer);
      if Num /= Buffer'Length then
         return Cannot_Read;
      end if;

      Header := Read_Header (Buffer, Index);

      if Header.ID /= "RIFF" then
         return Not_A_WAV_File;
      end if;

      RIFF_Header := Read_RIFF (Buffer, Index);
      if RIFF_Header.Format_ID /= "WAVE" then
         return Wrong_WAV_Format;
      end if;

      loop
         Header := Read_Header (Buffer, Index);

         if Header.ID = "fmt " then
            Info.Audio_Description := Read_Audio (Buffer, Index);
         elsif Header.ID = "LIST" then
            Index_Info := Index;
            Index := Index + Unsigned_16 (Header.Size);

            if Read_ID (Buffer, Index_Info) /= "INFO" then
               return Unexpected_Section;
            end if;

            loop
               Header := Read_Header (Buffer, Index_Info);

               if Header.ID = "IART" then
                  Read_String (Header, Info.Metadata.Artist);
               elsif Header.ID = "INAM" then
                  Read_String (Header, Info.Metadata.Title);
               elsif Header.ID = "IPRD" then
                  Read_String (Header, Info.Metadata.Album);
               elsif Header.ID = "ICRD" then
                  declare
                     Y_String : String (1 .. 4);
                     Year     : Natural := 0;
                  begin
                     Read_String (Header, Y_String);
                     for J in Y_String'Range loop
                        Year := Year * 10 +
                          Character'Pos (Y_String (J)) - Character'Pos ('0');
                     end loop;

                     Info.Metadata.Year := Year;
                  end;
               elsif Header.ID = "IGNR" then
                  Read_String (Header, Info.Metadata.Genre);
               end if;

               --  Aligned on 16bit
               Index_Info := Index_Info +
                 Unsigned_16 ((Header.Size + 1) / 2 * 2);
               exit when Index_Info = Index;
            end loop;

         elsif Header.ID = "data" then
            exit;
         else
            return Unexpected_Section;
         end if;

         while Index > Buffer'Last loop
            Num := File_Read (F, Buffer);
            Index := Index - Buffer'Length;
         end loop;
      end loop;

      return OK;
   end Read_Header;

end Wav_Reader;
