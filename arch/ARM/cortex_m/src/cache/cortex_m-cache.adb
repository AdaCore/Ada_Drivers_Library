
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

with System.Machine_Code;      use System.Machine_Code;
with Ada.Unchecked_Conversion;

with Interfaces;               use Interfaces;
with HAL;                      use HAL;

with Cortex_M_SVD.SCB;         use Cortex_M_SVD.SCB;
with Cortex_M_SVD.PF;          use Cortex_M_SVD.PF;
with Cortex_M_SVD.Cache;       use Cortex_M_SVD.Cache;

package body Cortex_M.Cache is

   Data_Cache_Line_Size : constant UInt32 :=
                            2 ** Natural (PF_Periph.CCSIDR.LineSize + 2);

   procedure DSB with Inline_Always;
   --  Data Stored Barrier

   procedure ISB with Inline_Always;
   --  Instruction Stored Barrier

   generic
      Reg_Address : System.Address;
   procedure Cache_Maintenance
     (Start : System.Address;
      Len   : Natural);
   --  FIXME: The Inline_Always is removed as workaround GNAT Community 2019
   --  bug.
   --    with Inline_Always;

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
   begin
      if not D_Cache_Enabled then
         return;
      end if;

      declare
         function To_U32 is new Ada.Unchecked_Conversion
           (System.Address, UInt32);

         Op_Size   : Integer_32 := Integer_32 (Len);
         Op_Addr   : UInt32 := To_U32 (Start);
         Reg       : UInt32 with Volatile, Address => Reg_Address;

      begin
         DSB;

         while Op_Size > 0 loop
            Reg     := Op_Addr;
            Op_Addr := Op_Addr + Data_Cache_Line_Size;
            Op_Size := Op_Size - Integer_32 (Data_Cache_Line_Size);
         end loop;

         DSB;
         ISB;
      end;
   end Cache_Maintenance;

   --------------------
   -- Enable_I_Cache --
   --------------------

   procedure Enable_I_Cache
   is
   begin
      DSB;
      ISB;
      Cortex_M_SVD.Cache.Cache_Periph.ICIALLU := 0; --  Invalidate I-Cache
      Cortex_M_SVD.SCB.SCB_Periph.CCR.IC := True;       --  Enable I-Cache
      DSB;
      ISB;
   end Enable_I_Cache;

   ---------------------
   -- Disable_I_Cache --
   ---------------------

   procedure Disable_I_Cache
   is
   begin
      DSB;
      ISB;

      Cortex_M_SVD.SCB.SCB_Periph.CCR.IC := False;  --  Disable I-Cache
      Cortex_M_SVD.Cache.Cache_Periph.ICIALLU := 0; --  Invalidate I-Cache

      DSB;
      ISB;
   end Disable_I_Cache;

   --------------------
   -- Enable_D_Cache --
   --------------------

   procedure Enable_D_Cache
   is
      CCSIDR : CCSIDR_Register;
   begin
      PF_Periph.CSSELR := (InD    => Data_Cache,
                           Level  => Level_1,
                           others => <>);

      DSB;

      CCSIDR := PF_Periph.CCSIDR;

      for S in reverse 0 .. CCSIDR.NumSets loop
         for W in reverse 0 .. CCSIDR.Associativity loop
            Cache_Periph.DCISW :=
              (Set => UInt9 (S),
               Way => UInt2 (W),
               others => <>);
         end loop;
      end loop;

      DSB;

      SCB_Periph.CCR.DC := True;       --  Enable D-Cache

      DSB;
      ISB;
   end Enable_D_Cache;

   ---------------------
   -- Disable_D_Cache --
   ---------------------

   procedure Disable_D_Cache
   is
      Sets   : DCISW_Set_Field;
      Ways   : DCISW_Way_Field;

   begin
      PF_Periph.CSSELR := (InD    => Data_Cache,
                           Level  => Level_1,
                           others => <>);

      DSB;

      --  Clean & Invalidate D-Cache
      Sets := DCISW_Set_Field (PF_Periph.CCSIDR.NumSets);
      Ways := DCISW_Way_Field (PF_Periph.CCSIDR.Associativity);

      for S in 0 .. Sets loop
         for W in 0 .. Ways loop
            Cache_Periph.DCCISW :=
              (Set => S,
               Way => W,
               others => <>);
         end loop;
      end loop;

      SCB_Periph.CCR.DC := False; --  Disable D-Cache

      DSB;
      ISB;
   end Disable_D_Cache;

   ---------------------
   -- I_Cache_Enabled --
   ---------------------

   function I_Cache_Enabled return Boolean
   is
   begin
      return SCB_Periph.CCR.IC;
   end I_Cache_Enabled;

   ---------------------
   -- D_Cache_Enabled --
   ---------------------

   function D_Cache_Enabled return Boolean
   is
   begin
      return SCB_Periph.CCR.DC;
   end D_Cache_Enabled;

   ------------------
   -- Clean_DCache --
   ------------------

   procedure Int_Clean_DCache is
     new Cache_Maintenance (Cache_Periph.DCCMVAC'Address);

   procedure Clean_DCache
     (Start : System.Address;
      Len   : Natural)
      renames Int_Clean_DCache;

   -----------------------
   -- Invalidate_DCache --
   -----------------------

   procedure Int_Invalidate_DCache is
     new Cache_Maintenance (Cache_Periph.DCIMVAC'Address);

   procedure Invalidate_DCache
     (Start : System.Address;
      Len   : Natural)
      renames Int_Invalidate_DCache;

   -----------------------------
   -- Clean_Invalidate_DCache --
   -----------------------------

   procedure Int_Clean_Invalidate_DCache is
     new Cache_Maintenance (Cache_Periph.DCCIMVAC'Address);

   procedure Clean_Invalidate_DCache
     (Start : System.Address;
      Len   : Natural)
      renames Int_Clean_Invalidate_DCache;

end Cortex_M.Cache;
