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

--  This package provides subprograms to configure the Memory Protection
--  Unit (MPU) on the Cortex-M7 family of CPU.
--
--  On Cortex-M7, the MPU is required when the D-cache is enabled in order
--  to prevent speculative reads from causing bus contention on external
--  memory interfaces (e.g., FMC/SDRAM). Without MPU configuration, the
--  default memory map treats all external memory as Normal, allowing the
--  CPU to issue speculative reads that can starve bus masters such as the
--  LTDC display controller.

with HAL;
with System;

package Cortex_M.MPU is

   type Region_Number is range 0 .. 7;

   type Region_Size is
     (Size_32B,   Size_64B,   Size_128B,  Size_256B,
      Size_512B,  Size_1KB,   Size_2KB,   Size_4KB,
      Size_8KB,   Size_16KB,  Size_32KB,  Size_64KB,
      Size_128KB, Size_256KB, Size_512KB, Size_1MB,
      Size_2MB,   Size_4MB,   Size_8MB,   Size_16MB,
      Size_32MB,  Size_64MB,  Size_128MB, Size_256MB,
      Size_512MB, Size_1GB,   Size_2GB,   Size_4GB);

   for Region_Size use
     (Size_32B   => 16#04#, Size_64B   => 16#05#,
      Size_128B  => 16#06#, Size_256B  => 16#07#,
      Size_512B  => 16#08#, Size_1KB   => 16#09#,
      Size_2KB   => 16#0A#, Size_4KB   => 16#0B#,
      Size_8KB   => 16#0C#, Size_16KB  => 16#0D#,
      Size_32KB  => 16#0E#, Size_64KB  => 16#0F#,
      Size_128KB => 16#10#, Size_256KB => 16#11#,
      Size_512KB => 16#12#, Size_1MB   => 16#13#,
      Size_2MB   => 16#14#, Size_4MB   => 16#15#,
      Size_8MB   => 16#16#, Size_16MB  => 16#17#,
      Size_32MB  => 16#18#, Size_64MB  => 16#19#,
      Size_128MB => 16#1A#, Size_256MB => 16#1B#,
      Size_512MB => 16#1C#, Size_1GB   => 16#1D#,
      Size_2GB   => 16#1E#, Size_4GB   => 16#1F#);

   type Access_Permission is
     (No_Access,
      Privileged_RW,
      Privileged_RW_Unprivileged_RO,
      Full_Access,
      Privileged_RO,
      Privileged_RO_Unprivileged_RO);

   for Access_Permission use
     (No_Access                      => 0,
      Privileged_RW                  => 1,
      Privileged_RW_Unprivileged_RO  => 2,
      Full_Access                    => 3,
      Privileged_RO                  => 5,
      Privileged_RO_Unprivileged_RO  => 6);

   subtype TEX_Level is HAL.UInt3;

   subtype Subregion_Disable is HAL.UInt8;

   type Region_Configuration is record
      Number            : Region_Number;
      Base_Address      : System.Address;
      Size              : Region_Size;
      Subregion_Disable : MPU.Subregion_Disable := 16#00#;
      TEX               : TEX_Level             := 0;
      Access_Permission : MPU.Access_Permission := Full_Access;
      Execute_Never     : Boolean               := False;
      Shareable         : Boolean               := False;
      Cacheable         : Boolean               := False;
      Bufferable        : Boolean               := False;
   end record;

   --  Enable/disable control bits for HAL_MPU_Enable
   type MPU_Control is record
      Hard_Fault_NMI_Enable   : Boolean;
      --  When True, the MPU is enabled during hard fault, NMI, and FAULTMASK
      Privileged_Default      : Boolean;
      --  When True, enables the default memory map as a background region
      --  for privileged access
   end record;

   procedure Enable (Control : MPU_Control);
   --  Enable the MPU with the specified control options.
   --  DSB and ISB are issued after enabling.

   procedure Disable;
   --  Disable the MPU. DSB and ISB are issued after disabling.

   procedure Configure_Region (Config : Region_Configuration);
   --  Configure a single MPU region. The region is enabled upon return.

   procedure Disable_Region (Number : Region_Number);
   --  Disable a single MPU region.

end Cortex_M.MPU;
