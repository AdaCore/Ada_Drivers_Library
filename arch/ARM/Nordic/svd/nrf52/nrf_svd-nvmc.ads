--  Copyright (c) 2010 - 2018, Nordic Semiconductor ASA
--
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without modification,
--  are permitted provided that the following conditions are met:
--
--  1. Redistributions of source code must retain the above copyright notice, this
--  list of conditions and the following disclaimer.
--
--  2. Redistributions in binary form, except as embedded into a Nordic
--  Semiconductor ASA integrated circuit in a product or a software update for
--  such product, must reproduce the above copyright notice, this list of
--  conditions and the following disclaimer in the documentation and/or other
--  materials provided with the distribution.
--
--  3. Neither the name of Nordic Semiconductor ASA nor the names of its
--  contributors may be used to endorse or promote products derived from this
--  software without specific prior written permission.
--
--  4. This software, with or without modification, must only be used with a
--  Nordic Semiconductor ASA integrated circuit.
--
--  5. Any software provided in binary form under this license must not be reverse
--  engineered, decompiled, modified and/or disassembled.
--
--  THIS SOFTWARE IS PROVIDED BY NORDIC SEMICONDUCTOR ASA "AS IS" AND ANY EXPRESS
--  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY, NONINFRINGEMENT, AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED. IN NO EVENT SHALL NORDIC SEMICONDUCTOR ASA OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
--  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
--  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
--  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--  This spec has been automatically generated from nrf52.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NRF_SVD.NVMC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  NVMC is ready or busy
   type READY_READY_Field is
     (--  NVMC is busy (on-going write or erase operation)
      Busy,
      --  NVMC is ready
      Ready)
     with Size => 1;
   for READY_READY_Field use
     (Busy => 0,
      Ready => 1);

   --  Ready flag
   type READY_Register is record
      --  Read-only. NVMC is ready or busy
      READY         : READY_READY_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for READY_Register use record
      READY         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Program memory access mode. It is strongly recommended to only activate
   --  erase and write modes when they are actively used. Enabling write or
   --  erase will invalidate the cache and keep it invalidated.
   type CONFIG_WEN_Field is
     (--  Read only access
      Ren,
      --  Write Enabled
      Wen,
      --  Erase enabled
      Een)
     with Size => 2;
   for CONFIG_WEN_Field use
     (Ren => 0,
      Wen => 1,
      Een => 2);

   --  Configuration register
   type CONFIG_Register is record
      --  Program memory access mode. It is strongly recommended to only
      --  activate erase and write modes when they are actively used. Enabling
      --  write or erase will invalidate the cache and keep it invalidated.
      WEN           : CONFIG_WEN_Field := NRF_SVD.NVMC.Ren;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      WEN           at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Erase all non-volatile memory including UICR registers. Note that code
   --  erase has to be enabled by CONFIG.EEN before the UICR can be erased.
   type ERASEALL_ERASEALL_Field is
     (--  No operation
      Nooperation,
      --  Start chip erase
      Erase)
     with Size => 1;
   for ERASEALL_ERASEALL_Field use
     (Nooperation => 0,
      Erase => 1);

   --  Register for erasing all non-volatile user memory
   type ERASEALL_Register is record
      --  Erase all non-volatile memory including UICR registers. Note that
      --  code erase has to be enabled by CONFIG.EEN before the UICR can be
      --  erased.
      ERASEALL      : ERASEALL_ERASEALL_Field := NRF_SVD.NVMC.Nooperation;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ERASEALL_Register use record
      ERASEALL      at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Register starting erase of all User Information Configuration Registers.
   --  Note that code erase has to be enabled by CONFIG.EEN before the UICR can
   --  be erased.
   type ERASEUICR_ERASEUICR_Field is
     (--  No operation
      Nooperation,
      --  Start erase of UICR
      Erase)
     with Size => 1;
   for ERASEUICR_ERASEUICR_Field use
     (Nooperation => 0,
      Erase => 1);

   --  Register for erasing User Information Configuration Registers
   type ERASEUICR_Register is record
      --  Register starting erase of all User Information Configuration
      --  Registers. Note that code erase has to be enabled by CONFIG.EEN
      --  before the UICR can be erased.
      ERASEUICR     : ERASEUICR_ERASEUICR_Field := NRF_SVD.NVMC.Nooperation;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ERASEUICR_Register use record
      ERASEUICR     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Cache enable
   type ICACHECNF_CACHEEN_Field is
     (--  Disable cache. Invalidates all cache entries.
      Disabled,
      --  Enable cache
      Enabled)
     with Size => 1;
   for ICACHECNF_CACHEEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Cache profiling enable
   type ICACHECNF_CACHEPROFEN_Field is
     (--  Disable cache profiling
      Disabled,
      --  Enable cache profiling
      Enabled)
     with Size => 1;
   for ICACHECNF_CACHEPROFEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  I-Code cache configuration register.
   type ICACHECNF_Register is record
      --  Cache enable
      CACHEEN       : ICACHECNF_CACHEEN_Field := NRF_SVD.NVMC.Disabled;
      --  unspecified
      Reserved_1_7  : HAL.UInt7 := 16#0#;
      --  Cache profiling enable
      CACHEPROFEN   : ICACHECNF_CACHEPROFEN_Field := NRF_SVD.NVMC.Disabled;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICACHECNF_Register use record
      CACHEEN       at 0 range 0 .. 0;
      Reserved_1_7  at 0 range 1 .. 7;
      CACHEPROFEN   at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type NVMC_Disc is
     (Age,
      Cr1);

   --  Non Volatile Memory Controller
   type NVMC_Peripheral
     (Discriminent : NVMC_Disc := Age)
   is record
      --  Ready flag
      READY     : aliased READY_Register;
      --  Configuration register
      CONFIG    : aliased CONFIG_Register;
      --  Register for erasing all non-volatile user memory
      ERASEALL  : aliased ERASEALL_Register;
      --  Deprecated register - Register for erasing a page in Code area.
      --  Equivalent to ERASEPAGE.
      ERASEPCR0 : aliased HAL.UInt32;
      --  Register for erasing User Information Configuration Registers
      ERASEUICR : aliased ERASEUICR_Register;
      --  I-Code cache configuration register.
      ICACHECNF : aliased ICACHECNF_Register;
      --  I-Code cache hit counter.
      IHIT      : aliased HAL.UInt32;
      --  I-Code cache miss counter.
      IMISS     : aliased HAL.UInt32;
      case Discriminent is
         when Age =>
            --  Register for erasing a page in Code area
            ERASEPAGE : aliased HAL.UInt32;
         when Cr1 =>
            --  Deprecated register - Register for erasing a page in Code area.
            --  Equivalent to ERASEPAGE.
            ERASEPCR1 : aliased HAL.UInt32;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for NVMC_Peripheral use record
      READY     at 16#400# range 0 .. 31;
      CONFIG    at 16#504# range 0 .. 31;
      ERASEALL  at 16#50C# range 0 .. 31;
      ERASEPCR0 at 16#510# range 0 .. 31;
      ERASEUICR at 16#514# range 0 .. 31;
      ICACHECNF at 16#540# range 0 .. 31;
      IHIT      at 16#548# range 0 .. 31;
      IMISS     at 16#54C# range 0 .. 31;
      ERASEPAGE at 16#508# range 0 .. 31;
      ERASEPCR1 at 16#508# range 0 .. 31;
   end record;

   --  Non Volatile Memory Controller
   NVMC_Periph : aliased NVMC_Peripheral
     with Import, Address => NVMC_Base;

end NRF_SVD.NVMC;
