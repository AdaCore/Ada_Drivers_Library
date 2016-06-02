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

with System.Machine_Code;      use System.Machine_Code;
with Ada.Unchecked_Conversion;

with Interfaces;               use Interfaces;
with HAL;                      use HAL;

package body Cortex_M.Cache is

   Data_Cache_Line_Size : constant := 32; --  Fixed to 8 words

   SCS_Base : constant := 16#E000_E000#; --  System Control Space base addr.
   SCB_Base : constant := SCS_Base + 16#0D00#; --  System Control Block

   --  D-Cache invalidate by MVA to PoC
   DCIMVAC  : Word
     with Volatile, Address => System'To_Address (SCB_Base + 16#25C#);

   --  D-Cache clean by MVA to PoC
   DCCMVAC  : Word
     with Volatile, Address => System'To_Address (SCB_Base + 16#268#);

   --  D-Cache clean and invalidate by MVA to PoC
   DCCIMVAC : Word
     with Volatile, Address => System'To_Address (SCB_Base + 16#270#);

   function To_U32 is new Ada.Unchecked_Conversion
     (System.Address, Unsigned_32);

   ------------------
   -- Clean_DCache --
   ------------------

   procedure Clean_DCache
     (Start : System.Address;
      Len   : Natural)
   is
      Op_Size   : Integer_32 := Integer_32 (Len);
      Op_Addr   : Unsigned_32 := To_U32 (Start);

   begin
      Asm ("dsb", Volatile => True);

      while Op_Size > 0 loop
         DCCMVAC := Op_Addr;
         Op_Addr := Op_Addr + Data_Cache_Line_Size;
         Op_Size := Op_Size - Data_Cache_Line_Size;
      end loop;

      Asm ("dsb", Volatile => True);
      Asm ("isb", Volatile => True);
   end Clean_DCache;

   -----------------------
   -- Invalidate_DCache --
   -----------------------

   procedure Invalidate_DCache
     (Start : System.Address;
      Len   : Natural)
   is
      Op_Size   : Integer_32 := Integer_32 (Len);
      Op_Addr   : Unsigned_32 := To_U32 (Start);

   begin
      Asm ("dsb", Volatile => True);

      while Op_Size > 0 loop
         DCIMVAC := Op_Addr;
         Op_Addr := Op_Addr + Data_Cache_Line_Size;
         Op_Size := Op_Size - Data_Cache_Line_Size;
      end loop;

      Asm ("dsb", Volatile => True);
      Asm ("isb", Volatile => True);
   end Invalidate_DCache;

   -----------------------------
   -- Clean_Invalidate_DCache --
   -----------------------------

   procedure Clean_Invalidate_DCache
     (Start : System.Address;
      Len   : Natural)
   is
      Op_Size   : Integer_32 := Integer_32 (Len);
      Op_Addr   : Unsigned_32 := To_U32 (Start);

   begin
      Asm ("dsb", Volatile => True);

      while Op_Size > 0 loop
         DCCIMVAC := Op_Addr;
         Op_Addr := Op_Addr + Data_Cache_Line_Size;
         Op_Size := Op_Size - Data_Cache_Line_Size;
      end loop;

      Asm ("dsb", Volatile => True);
      Asm ("isb", Volatile => True);
   end Clean_Invalidate_DCache;

end Cortex_M.Cache;
