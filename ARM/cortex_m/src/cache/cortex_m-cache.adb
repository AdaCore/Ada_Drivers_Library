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

   SCS_Base : constant := 16#E000_E000#; --  System Control Space base addr.
   SCB_Base : constant := SCS_Base + 16#0D00#; --  System Control Block

   type CCSIDR_Register is record
      Line_Size     : UInt3;
      Associativity : UInt10;
      Num_Sets      : UInt15;
      WA            : Boolean;
      RA            : Boolean;
      WB            : Boolean;
      WT            : Boolean;
   end record with Volatile, Size => 32;

   for CCSIDR_Register use record
      Line_Size     at 0 range  0 ..  2;
      Associativity at 0 range  3 .. 12;
      Num_Sets      at 0 range 13 .. 27;
      WA            at 0 range 28 .. 28;
      RA            at 0 range 29 .. 29;
      WB            at 0 range 30 .. 30;
      WT            at 0 range 31 .. 31;
   end record;

   CCSIDR               : CCSIDR_Register
     with Volatile, Address => System'To_Address (SCB_Base + 16#080#);

   Data_Cache_Line_Size : constant Word := Word (CCSIDR.Line_Size) * 32;

   --  D-Cache clean by MVA to PoC
   DCCMVAC  : constant System.Address :=
                System'To_Address (SCB_Base + 16#268#);

   --  D-Cache invalidate by MVA to PoC
   DCIMVAC  : constant System.Address :=
                System'To_Address (SCB_Base + 16#25C#);

   --  D-Cache clean and invalidate by MVA to PoC
   DCCIMVAC : constant System.Address :=
                System'To_Address (SCB_Base + 16#270#);

   procedure DSB with Inline_Always;
   --  Data Stored Barrier

   procedure ISB with Inline_Always;
   --  Instruction Stored Barrier

   generic
      Reg_Address : System.Address;
   procedure Cache_Maintenance
     (Start : System.Address;
      Len   : Natural) with Inline_Always;

   ---------
   -- DSB --
   ---------

   procedure DSB is
   begin
      Asm ("dsb", Volatile => True);
   end DSB;

   ---------
   -- ISB --
   ---------

   procedure ISB is
   begin
      Asm ("isb", Volatile => True);
   end ISB;

   -----------------------
   -- Cache_Maintenance --
   -----------------------

   procedure Cache_Maintenance
     (Start : System.Address;
      Len   : Natural)
   is
      function To_U32 is new Ada.Unchecked_Conversion
        (System.Address, Unsigned_32);

      Op_Size   : Integer_32 := Integer_32 (Len);
      Op_Addr   : Unsigned_32 := To_U32 (Start);
      Reg       : Word with Volatile, Address => Reg_Address;

   begin
      DSB;

      while Op_Size > 0 loop
         Reg     := Op_Addr;
         Op_Addr := Op_Addr + Data_Cache_Line_Size;
         Op_Size := Op_Size - Integer_32 (Data_Cache_Line_Size);
      end loop;

      DSB;
      ISB;
   end Cache_Maintenance;

   ------------------
   -- Clean_DCache --
   ------------------

   procedure Int_Clean_DCache is new Cache_Maintenance (DCCMVAC);

   procedure Clean_DCache
     (Start : System.Address;
      Len   : Natural)
      renames Int_Clean_DCache;

   -----------------------
   -- Invalidate_DCache --
   -----------------------

   procedure Int_Invalidate_DCache is new Cache_Maintenance (DCIMVAC);

   procedure Invalidate_DCache
     (Start : System.Address;
      Len   : Natural)
      renames Int_Invalidate_DCache;

   -----------------------------
   -- Clean_Invalidate_DCache --
   -----------------------------

   procedure Int_Clean_Invalidate_DCache is new Cache_Maintenance (DCCIMVAC);

   procedure Clean_Invalidate_DCache
     (Start : System.Address;
      Len   : Natural)
      renames Int_Clean_Invalidate_DCache;

end Cortex_M.Cache;
