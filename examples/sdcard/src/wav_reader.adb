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


with FAT_Filesystem.Files; use FAT_Filesystem.Files;
with Ada.Unchecked_Conversion;

with HAL.Audio;    use HAL.Audio;

with STM32.Board;  use STM32.Board;
with STM32.DMA;
with STM32.Button; use STM32.Button;

package body Wav_Reader is

   function To_String (Block : File_Data) return String;

   subtype Buffer_Type is Audio_Buffer (1 .. 32 * 1024);
   Buffer : aliased Buffer_Type := (others => 0);
   for Buffer'Alignment use 32;

   protected Buffer_Scheduler is
      entry Next_Index (Idx  : out Natural;
                        Len  : out Natural);

   private
      procedure Interrupt;
      pragma Attach_Handler
        (Interrupt, STM32.Board.Audio_Out_DMA_Interrupt);
      Current_Idx  : Natural := Buffer'First + Buffer'Length / 2;
      Available    : Boolean := True;
   end Buffer_Scheduler;

   ----------------------
   -- Buffer_Scheduler --
   ----------------------

   protected body Buffer_Scheduler is

      ----------------
      -- Next_Index --
      ----------------

      entry Next_Index
         (Idx  : out Natural;
          Len  : out Natural) when Available
      is
      begin
         Idx := Current_Idx;
         Len := Buffer'Length / 2;

         --  Update the internal state
         Available := False;
      end Next_Index;

      ---------------
      -- Interrupt --
      ---------------

      procedure Interrupt is
      begin
         if STM32.DMA.Status
           (Audio_DMA, Audio_DMA_Out_Stream,
            STM32.DMA.Half_Transfer_Complete_Indicated)
         then
--              STM32.Board.Turn_On (STM32.Board.Red);
            STM32.DMA.Clear_Status
              (Audio_DMA, Audio_DMA_Out_Stream,
               STM32.DMA.Half_Transfer_Complete_Indicated);
            Current_Idx := Buffer'First;
            Available := True;
         end if;

         if STM32.DMA.Status
           (Audio_DMA, Audio_DMA_Out_Stream,
            STM32.DMA.Transfer_Complete_Indicated)
         then
--              STM32.Board.Turn_Off (STM32.Board.Red);
            STM32.DMA.Clear_Status
              (Audio_DMA, Audio_DMA_Out_Stream,
               STM32.DMA.Transfer_Complete_Indicated);
            Current_Idx := Buffer'First + Buffer'Length / 2;
            Available := True;
         end if;
      end Interrupt;

   end Buffer_Scheduler;

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Volume : HAL.Audio.Audio_Volume)
   is
   begin
      if not STM32.Button.Initialized then
         STM32.Button.Initialize;
      end if;

      STM32.Board.Audio_Device.Initialize_Audio_Out
        (Volume    => Volume,
         Frequency => Audio_Freq_48kHz);
      STM32.Board.Audio_Device.Play (Buffer);
      STM32.Board.Audio_Device.Pause;
   end Initialize;

   -----------------
   -- Read_Header --
   -----------------

   function Read_Header
     (F : in out FAT_Filesystem.Files.File_Handle;
      Info : out WAV_Info) return WAV_Status_Code
   is
      subtype ID is String (1 .. 4);
      procedure Read_Header is new Generic_Read (Header_Block);
      procedure Read_RIFF is new Generic_Read (RIFF_Block);
      procedure Read_Audio is new Generic_Read (Audio_Description_Block);
      procedure Read_ID is new Generic_Read (ID);

      procedure Read_String
        (H : Header_Block;
         S : in out String);

      Header            : Header_Block;
      RIFF_Header       : RIFF_Block;
      Buffer            : File_Data (1 .. 512)
        with Alignment => 32;
      Index             : Natural := Buffer'First;
      Index_Info        : Natural;
      Num               : Integer with Unreferenced;

      -----------------
      -- Read_String --
      -----------------

      procedure Read_String
        (H : Header_Block;
         S : in out String)
      is
         Num : Integer with Unreferenced;
      begin
         Num := File_Read (F, Buffer (1 .. Natural (H.Size)));

         if H.Size - 1 > S'Length then
            S := To_String (Buffer (1 .. S'Length));
         else
            S (S'First .. S'First + Integer (H.Size - 2)) :=
              To_String (Buffer (1 .. Natural (H.Size) - 1));
         end if;
      end Read_String;

   begin
      Read_Header (F, Header);

      if Header.ID /= "RIFF" then
         return Not_A_WAV_File;
      end if;

      Read_RIFF (F, RIFF_Header);
      if RIFF_Header.Format_ID /= "WAVE" then
         return Wrong_WAV_Format;
      end if;

      loop
         Read_Header (F, Header);

         if Header.ID = "fmt " then
            Read_Audio (F, Info.Audio_Description);
         elsif Header.ID = "LIST" then
            Index := Natural (Header.Size);
            Index_Info := 4; --  to account for the INFO ID after the header

            Read_ID (F, Header.ID);

            if Header.ID /= "INFO" then
               return Unexpected_Section;
            end if;

            loop
               Read_Header (F, Header);
               Index_Info := Index_Info + 8 + Natural (Header.Size);

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
               else
                  Num :=
                    File_Read (F, Buffer (1 .. Natural (Header.Size)));
               end if;

               --  Aligned on 16bit
               if Header.Size mod 2 = 1 then
                  Num :=
                    File_Read (F, Buffer (1 .. 1));
                  Index_Info := Index_Info + 1;
               end if;

               exit when Index_Info = Index;
            end loop;

         elsif Header.ID = "data" then
            Info.Data_Size := Header.Size;
            exit;

         else
            return Unexpected_Section;
         end if;
      end loop;

      return OK;
   end Read_Header;

   ----------
   -- Play --
   ----------

   procedure Play
     (F    : in out FAT_Filesystem.Files.File_Handle;
      Info : WAV_Info)
   is
      Idx        : Natural;
      Len        : Natural;
      Frq        : Audio_Frequency := Audio_Frequency'First;
      Total      : Unsigned_32 := 0;

   begin
      for F in Audio_Frequency'Range loop
         exit when Info.Audio_Description.Frequency < F'Enum_Rep;
         Frq := F;
      end loop;

      STM32.Board.Audio_Device.Set_Frequency (Frq);

      --  Init the buffer with zeros (silent)
      Buffer := (others => 0);

      --  Start playing it
      STM32.Board.Audio_Device.Resume;

      --  Read a few data to make sure that next read operations are aligned on
      --  blocks.
      Buffer_Scheduler.Next_Index (Idx, Len);
      declare
         Initial_Length : constant Natural :=
                            512 - Natural (File_Offset (F) mod 512);
         Cnt            : Integer;
      begin
         Cnt :=
           File_Read (F,
                      Buffer
                        (Idx + Len - Integer (Initial_Length / 2) - 1)'Address,
                      Initial_Length);
         Total := Total + Unsigned_32 (Cnt / 2);
      end;

      loop
         declare
            Cnt : Integer;
         begin
            Buffer_Scheduler.Next_Index (Idx, Len);

            STM32.Board.Turn_On (STM32.Board.Green);
            Cnt := File_Read (F, Buffer (Idx)'Address, Len * 2);
            STM32.Board.Turn_Off (STM32.Board.Green);
            Total := Total + Unsigned_32 (Cnt / 2);

            exit when Total >= Info.Data_Size or else Cnt <= 0;
            exit when STM32.Button.Has_Been_Pressed;
         end;
      end loop;

      Buffer := (others => 0);
      STM32.Board.Audio_Device.Pause;
   end Play;

end Wav_Reader;
