
with Interfaces; use Interfaces;

with HAL.Audio;
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
      Data_Size         : Unsigned_32;
   end record;

   procedure Initialize (Volume : HAL.Audio.Audio_Volume);

   function Read_Header
     (F    : in out FAT_Filesystem.Files.File_Handle;
      Info : out WAV_Info) return WAV_Status_Code;

   procedure Play
     (F    : in out FAT_Filesystem.Files.File_Handle;
      Info : WAV_Info);

end Wav_Reader;
