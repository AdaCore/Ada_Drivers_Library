------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
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

with System;
with HAL;

package Semihosting is
   pragma Preelaborate;

   type SH_Word is new HAL.UInt32;

   subtype Flag is SH_Word;

   OPEN_FLAG_R         : constant Flag := 0;
   OPEN_FLAG_RB        : constant Flag := 1;
   OPEN_FLAG_R_PLUS    : constant Flag := 2;
   OPEN_FLAG_R_PLUS_B  : constant Flag := 3;
   OPEN_FLAG_W         : constant Flag := 4;
   OPEN_FLAG_WB        : constant Flag := 5;
   OPEN_FLAG_W_PLUS    : constant Flag := 6;
   OPEN_FLAG_W_PLUS_B  : constant Flag := 7;
   OPEN_FLAG_A         : constant Flag := 8;
   OPEN_FLAG_AB        : constant Flag := 9;
   OPEN_FLAG_A_PLUS    : constant Flag := 10;
   OPEN_FLAG_A_PLUS_B  : constant Flag := 11;

   procedure Write_C (C : Character);
   procedure Write_0 (Str : String);
   function Close (File_Handle : SH_Word) return SH_Word;
   function Open (Filename : String; Mode : Flag) return SH_Word;
   function Read (File_Handle     : SH_Word;
                  Buffer_Address  : System.Address;
                  Buffer_Size     : SH_Word) return SH_Word;
   function Write (File_Handle     : SH_Word;
                   Buffer_Address  : System.Address;
                   Buffer_Size     : SH_Word) return SH_Word;
   function Remove (Filename : String) return SH_Word;
   function Seek (File_Handle : SH_Word;
                  Absolute_Position : SH_Word) return SH_Word;
   function Errno return SH_Word;

   procedure Log (C : Character) renames Write_C;
   procedure Log (Str : String) renames Write_0;
   procedure Log_Line (Str : String);
   procedure Log_New_Line;

end Semihosting;
