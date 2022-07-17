------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2016-2020, AdaCore                        --
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

with Cortex_M.Debug;
with System.Machine_Code; use System.Machine_Code;
with HAL; use HAL;
with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;

package body Semihosting is

   type SH_u32_Array is array (Integer range <>) of SH_Word
     with Pack, Convention => C, Volatile_Components;

   function To_SH_u32 is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => SH_Word);
   function To_SH_u32 is new Ada.Unchecked_Conversion
     (Source => Integer, Target => SH_Word);

   subtype Syscall is SH_Word;

   SYS_OPEN     : constant Syscall := 16#01#;
   SYS_CLOSE    : constant Syscall := 16#02#;
   SYS_WRITEC   : constant Syscall := 16#03#;
   SYS_WRITE0   : constant Syscall := 16#04#;
   SYS_WRITE    : constant Syscall := 16#05#;
   SYS_READ     : constant Syscall := 16#06#;
   --  SYS_READC    : constant Syscall := 16#07#;
   --  SYS_ISERROR  : constant Syscall := 16#08#;
   --  SYS_ISTTY    : constant Syscall := 16#09#;
   SYS_SEEK     : constant Syscall := 16#0A#;
   --  SYS_FLEN     : constant Syscall := 16#0C#;
   --  SYS_TMPNAM   : constant Syscall := 16#0D#;
   SYS_REMOVE   : constant Syscall := 16#0E#;
   --  SYS_RENAME   : constant Syscall := 16#0E#;
   --  SYS_CLOCK    : constant Syscall := 16#10#;
   --  SYS_TIME     : constant Syscall := 16#11#;
   SYS_ERRNO    : constant Syscall := 16#13#;
   --  SYS_GET_CMD  : constant Syscall := 16#15#;
   --  SYS_HEAPINFO : constant Syscall := 16#16#;
   --  SYS_ELAPSED  : constant Syscall := 16#30#;
   --  SYS_TICKFREQ : constant Syscall := 16#31#;

   function Semihosting_Enabled return Boolean is
     (Cortex_M.Debug.Halting_Debug_Enabled);
   function Generic_SH_Call (R0, R1 : SH_Word) return SH_Word;
   function Generic_SH_Call (R0 : SH_Word; R1 : System.Address) return SH_Word;

   ---------------------
   -- Generic_SH_Call --
   ---------------------

   function Generic_SH_Call (R0, R1 : SH_Word) return SH_Word is
      Ret : SH_Word;
   begin
      Asm ("mov r0, %1" & ASCII.LF & ASCII.HT &
           "mov r1, %2" & ASCII.LF & ASCII.HT &
           "bkpt #0xAB" & ASCII.LF & ASCII.HT &
           "mov %0, r0",
           Outputs  => (SH_Word'Asm_Output ("=r", Ret)),
           Inputs   => (SH_Word'Asm_Input ("r", R0),
                        SH_Word'Asm_Input ("r", R1)),
           Volatile => True,
           Clobber => ("r1, r0"));
      return Ret;
   end Generic_SH_Call;

   ---------------------
   -- Generic_SH_Call --
   ---------------------

   function Generic_SH_Call (R0 : SH_Word; R1 : System.Address)
      return SH_Word is
   begin
      return Generic_SH_Call (R0, To_SH_u32 (R1));
   end Generic_SH_Call;

   -----------
   -- Close --
   -----------

   function Close (File_Handle : SH_Word) return SH_Word is
      Block : SH_u32_Array (0 .. 0);
   begin
      if not Semihosting_Enabled then
         --  No debugger attached
         return SH_Word'Last;
      end if;

      Block (0) := File_Handle;
      return Generic_SH_Call (SYS_CLOSE, Block'Address);
   end Close;

   ----------
   -- Open --
   ----------

   function Open (Filename : String; Mode : Flag) return SH_Word is
      Block  : SH_u32_Array (0 .. 2);
      C_Name : char_array (0 .. Filename'Length) with Volatile;
   begin
      if not Semihosting_Enabled then
         --  No debugger attached
         return SH_Word'Last;
      end if;

      for J in Filename'Range loop
         C_Name (size_t (J - Filename'First)) :=
           char'Val (Character'Pos (Filename (J)));
      end loop;
      C_Name (C_Name'Last) := nul;

      Block (0) := To_SH_u32 (C_Name'Address);
      Block (1) := Mode;
      Block (2) := Filename'Length;

      return Generic_SH_Call (SYS_OPEN, Block'Address);
   end Open;

   ----------
   -- Read --
   ----------

   function Read (File_Handle     : SH_Word;
                  Buffer_Address  : System.Address;
                  Buffer_Size     : SH_Word) return SH_Word
   is
      Block  : SH_u32_Array (0 .. 2);
   begin
      if not Semihosting_Enabled then
         --  No debugger attached
         return Buffer_Size;
      end if;

      Block (0) := File_Handle;
      Block (1) := To_SH_u32 (Buffer_Address);
      Block (2) := Buffer_Size;

      return Generic_SH_Call (SYS_READ, Block'Address);
   end Read;

   -----------
   -- Write --
   -----------

   function Write (File_Handle     : SH_Word;
                   Buffer_Address  : System.Address;
                   Buffer_Size     : SH_Word) return SH_Word
   is
      Block  : SH_u32_Array (0 .. 3);
   begin
      if not Semihosting_Enabled then
         --  No debugger attached
         return Buffer_Size;
      end if;

      Block (0) := File_Handle;
      Block (1) := To_SH_u32 (Buffer_Address);
      Block (2) := Buffer_Size;

      return Generic_SH_Call (SYS_WRITE, Block'Address);
   end Write;

   ------------
   -- Remove --
   ------------

   function Remove (Filename : String) return SH_Word is
      Block  : SH_u32_Array (0 .. 1);
      C_Name : char_array (0 .. Filename'Length) with Volatile;
   begin
      if not Semihosting_Enabled then
         --  No debugger attached
         return SH_Word'Last;
      end if;

      for J in Filename'Range loop
         C_Name (size_t (J - Filename'First)) :=
           char'Val (Character'Pos (Filename (J)));
      end loop;
      C_Name (C_Name'Last) := nul;

      Block (0) := To_SH_u32 (C_Name'Address);
      Block (1) := To_SH_u32 (Filename'Length);

      return Generic_SH_Call (SYS_REMOVE, Block'Address);
   end Remove;

   ----------
   -- Seek --
   ----------

   function Seek (File_Handle       : SH_Word;
                  Absolute_Position : SH_Word) return SH_Word
   is
      Block  : SH_u32_Array (0 .. 1);
   begin
      if not Semihosting_Enabled then
         --  No debugger attached
         return SH_Word'Last;
      end if;

      Block (0) := File_Handle;
      Block (1) := Absolute_Position;

      return Generic_SH_Call (SYS_SEEK, Block'Address);
   end Seek;

   -----------
   -- Errno --
   -----------

   function Errno return SH_Word is
   begin
      return Generic_SH_Call (SYS_ERRNO, 0);
   end Errno;

   -------------
   -- Write_C --
   -------------

   procedure Write_C (C : Character) is
      Ret : SH_Word with Unreferenced;
   begin
      if not Semihosting_Enabled then
         --  No debugger attached
         return;
      end if;

      Ret := Generic_SH_Call (SYS_WRITEC, C'Address);
   end Write_C;

   -------------
   -- Write_0 --
   -------------

   procedure Write_0 (Str : String) is
      type Byte_Array is new UInt8_Array with Volatile_Components;
      Data : Byte_Array (Str'First .. Str'Last + 1);
      Ret  : SH_Word with Unreferenced;
   begin
      if not Semihosting_Enabled then
         --  No debugger attached
         return;
      end if;

      for Index in Str'Range loop
         Data (Index) := Character'Pos (Str (Index));
      end loop;

      --  Add trailing zero
      Data (Str'Last + 1) := 0;

      Ret := Generic_SH_Call (SYS_WRITE0, Data'Address);
   end Write_0;

   --------------
   -- Log_Line --
   --------------

   procedure Log_Line (Str : String) is
   begin
      Log (Str);
      Log_New_Line;
   end Log_Line;

   ------------------
   -- Log_New_Line --
   ------------------

   procedure Log_New_Line is
   begin
      Write_C (ASCII.LF);
   end Log_New_Line;

end Semihosting;
