------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2026, AdaCore                       --
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

with System.Machine_Code;   use System.Machine_Code;
with Ada.Unchecked_Conversion;

with Cortex_M_SVD.MPU;      use Cortex_M_SVD.MPU;

package body Cortex_M.MPU is

   procedure DSB with Inline_Always;
   procedure ISB with Inline_Always;

   procedure DSB is
   begin
      Asm ("dsb", Volatile => True);
   end DSB;

   procedure ISB is
   begin
      Asm ("isb", Volatile => True);
   end ISB;

   ------------
   -- Enable --
   ------------

   procedure Enable (Control : MPU_Control) is
   begin
      MPU_Periph.CTRL := (ENABLE     => True,
                          HFNMIENA   => Control.Hard_Fault_NMI_Enable,
                          PRIVDEFENA => Control.Privileged_Default,
                          others     => <>);
      DSB;
      ISB;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable is
   begin
      DSB;
      MPU_Periph.CTRL := (ENABLE     => False,
                          HFNMIENA   => False,
                          PRIVDEFENA => False,
                          others     => <>);
      ISB;
   end Disable;

   ----------------------
   -- Configure_Region --
   ----------------------

   procedure Configure_Region (Config : Region_Configuration) is

      function To_UInt32 is new Ada.Unchecked_Conversion
        (System.Address, HAL.UInt32);

      use HAL;

      Addr : constant UInt32 := To_UInt32 (Config.Base_Address);

      Size_Val : constant UInt5 := Region_Size'Enum_Rep (Config.Size);
      AP_Val   : constant UInt3 := Access_Permission'Enum_Rep (Config.Access_Permission);
   begin
      --  Select the region
      MPU_Periph.RNR := (REGION => HAL.UInt8 (Config.Number), others => <>);

      --  Set base address
      MPU_Periph.RBAR := (ADDR => UInt27 (Addr / 2**5), VALID => False, REGION => 0);

      --  Set attributes and enable
      MPU_Periph.RASR := (ENABLE => True,
                          SIZE   => Size_Val,
                          SRD    => (As_Array => False, Val => Config.Subregion_Disable),
                          B      => Config.Bufferable,
                          C      => Config.Cacheable,
                          S      => Config.Shareable,
                          TEX    => Config.TEX,
                          AP     => Cortex_M_SVD.MPU.RASR_AP_Field'Val (AP_Val),
                          XN     => (if Config.Execute_Never
                                     then Cortex_M_SVD.MPU.I_Disabled
                                     else Cortex_M_SVD.MPU.I_Enabled),
                          others => <>);
   end Configure_Region;

   --------------------
   -- Disable_Region --
   --------------------

   procedure Disable_Region (Number : Region_Number) is
   begin
      MPU_Periph.RNR := (REGION => HAL.UInt8 (Number), others => <>);
      MPU_Periph.RASR := (ENABLE => False, others => <>);
   end Disable_Region;

end Cortex_M.MPU;
