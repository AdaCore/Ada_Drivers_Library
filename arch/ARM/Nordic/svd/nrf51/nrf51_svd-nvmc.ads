--  Copyright (c) 2013, Nordic Semiconductor ASA
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  * Redistributions of source code must retain the above copyright notice, this
--  list of conditions and the following disclaimer.
--
--  * Redistributions in binary form must reproduce the above copyright notice,
--  this list of conditions and the following disclaimer in the documentation
--  and/or other materials provided with the distribution.
--
--  * Neither the name of Nordic Semiconductor ASA nor the names of its
--  contributors may be used to endorse or promote products derived from
--  this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
--  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
--  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
--  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
--  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
--  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--  This spec has been automatically generated from nrf51.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NRF51_SVD.NVMC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  NVMC ready.
   type READY_READY_Field is
     (
      --  NVMC is busy (on-going write or erase operation).
      Busy,
      --  NVMC is ready.
      Ready)
     with Size => 1;
   for READY_READY_Field use
     (Busy => 0,
      Ready => 1);

   --  Ready flag.
   type READY_Register is record
      --  Read-only. NVMC ready.
      READY         : READY_READY_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for READY_Register use record
      READY         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Program write enable.
   type CONFIG_WEN_Field is
     (
      --  Read only access.
      Ren,
      --  Write enabled.
      Wen,
      --  Erase enabled.
      Een)
     with Size => 2;
   for CONFIG_WEN_Field use
     (Ren => 0,
      Wen => 1,
      Een => 2);

   --  Configuration register.
   type CONFIG_Register is record
      --  Program write enable.
      WEN           : CONFIG_WEN_Field := NRF51_SVD.NVMC.Ren;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      WEN           at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Starts the erasing of all user NVM (code region 0/1 and UICR registers).
   type ERASEALL_ERASEALL_Field is
     (
      --  No operation.
      Nooperation,
      --  Start chip erase.
      Erase)
     with Size => 1;
   for ERASEALL_ERASEALL_Field use
     (Nooperation => 0,
      Erase => 1);

   --  Register for erasing all non-volatile user memory.
   type ERASEALL_Register is record
      --  Starts the erasing of all user NVM (code region 0/1 and UICR
      --  registers).
      ERASEALL      : ERASEALL_ERASEALL_Field := NRF51_SVD.NVMC.Nooperation;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ERASEALL_Register use record
      ERASEALL      at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  It can only be used when all contents of code region 1 are erased.
   type ERASEUICR_ERASEUICR_Field is
     (
      --  No operation.
      Nooperation,
      --  Start UICR erase.
      Erase)
     with Size => 1;
   for ERASEUICR_ERASEUICR_Field use
     (Nooperation => 0,
      Erase => 1);

   --  Register for start erasing User Information Congfiguration Registers.
   type ERASEUICR_Register is record
      --  It can only be used when all contents of code region 1 are erased.
      ERASEUICR     : ERASEUICR_ERASEUICR_Field := NRF51_SVD.NVMC.Nooperation;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ERASEUICR_Register use record
      ERASEUICR     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type NVMC_Disc is
     (
      Age,
      Cr1);

   --  Non Volatile Memory Controller.
   type NVMC_Peripheral
     (Discriminent : NVMC_Disc := Age)
   is record
      --  Ready flag.
      READY     : aliased READY_Register;
      --  Configuration register.
      CONFIG    : aliased CONFIG_Register;
      --  Register for erasing all non-volatile user memory.
      ERASEALL  : aliased ERASEALL_Register;
      --  Register for erasing a protected non-volatile memory page.
      ERASEPCR0 : aliased HAL.UInt32;
      --  Register for start erasing User Information Congfiguration Registers.
      ERASEUICR : aliased ERASEUICR_Register;
      case Discriminent is
         when Age =>
            --  Register for erasing a non-protected non-volatile memory page.
            ERASEPAGE : aliased HAL.UInt32;
         when Cr1 =>
            --  Register for erasing a non-protected non-volatile memory page.
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
      ERASEPAGE at 16#508# range 0 .. 31;
      ERASEPCR1 at 16#508# range 0 .. 31;
   end record;

   --  Non Volatile Memory Controller.
   NVMC_Periph : aliased NVMC_Peripheral
     with Import, Address => System'To_Address (16#4001E000#);

end NRF51_SVD.NVMC;
