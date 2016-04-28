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
--     3. Neither the name of AdaCore nor the names of its contributors may --
--        be used to endorse or promote products derived from this software --
--        without specific prior written permission.
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

with System.Machine_Code;
with System.Storage_Elements;
with Ada.Unchecked_Conversion;

with Interfaces; use Interfaces;
with HAL;        use HAL;

package body Cortex_M.Cache is

   type CCSIDR_Register is record
      Line_Size        : UInt3;
      Associativity    : UInt10;
      Num_Sets         : UInt15;
      Write_Allocation : Boolean;
      Read_Allocation  : Boolean;
      Write_Back       : Boolean;
      Write_Through    : Boolean;
   end record with Volatile_Full_Access, Size => 32;
   for CCSIDR_Register use record
      Line_Size        at 0 range  0 .. 2;
      Associativity    at 0 range  3 .. 12;
      Num_Sets         at 0 range 13 .. 27;
      Write_Allocation at 0 range 28 .. 28;
      Read_Allocation  at 0 range 29 .. 29;
      Write_Back       at 0 range 30 .. 30;
      Write_Through    at 0 range 31 .. 31;
   end record;

   CCSIDR : CCSIDR_Register
     with Address => System'To_Address (16#E000ED80#);
   DCCSW  : Word
     with Volatile, Address => System'To_Address (16#E000EF6C#);

   function CLZ (Ways : Word) return Word;
   pragma Import (Intrinsic, CLZ, "__builtin_clz");

   ------------------
   -- Clean_DCache --
   ------------------

   procedure Clean_DCache (Start, Stop : System.Address)
   is
      function To_Word is new Ada.Unchecked_Conversion
        (System.Address, Word);
      use System.Machine_Code;

      Start_W  : Word;
      S_Mask   : Word;
      S_Shift  : Natural;
      Ways     : Word;
      W_Shift  : Natural;
      S_Size   : Word;
      Tmp_Ways : Word;
      Set      : Word;

   begin
      S_Mask  := Word (CCSIDR.Num_Sets);
      S_Shift := Natural (CCSIDR.Line_Size) + 4;

      Ways    := Word (CCSIDR.Associativity);
      W_Shift := Natural (CLZ (Ways) and 16#1F#);

      S_Size := Shift_Left (1, S_Shift);
      Start_W := To_Word (Start) and not (S_Size - 1);

      Asm ("dsb", Volatile => True);

      while Start_W < To_Word (Stop) loop
         Tmp_Ways := Ways;
         Set := Shift_Right (Start_W, S_Shift) and S_Mask;

         loop
            DCCSW :=
              Shift_Left (Tmp_Ways, W_Shift) or Shift_Left (Set, S_Shift);
            exit when Tmp_Ways = 0;
            Tmp_Ways := Tmp_Ways - 1;
         end loop;

         Start_W := Start_W + S_Size;
      end loop;

      Asm ("dsb", Volatile => True);
      Asm ("isb", Volatile => True);
   end Clean_DCache;

   ------------------
   -- Clean_DCache --
   ------------------

   procedure Clean_DCache (Start : System.Address;
                           Len   : Natural)
   is
      use System.Storage_Elements;
   begin
      Clean_DCache
        (Start,
         Start + System.Storage_Elements.Storage_Offset (Len - 1));
   end Clean_DCache;
end Cortex_M.Cache;
