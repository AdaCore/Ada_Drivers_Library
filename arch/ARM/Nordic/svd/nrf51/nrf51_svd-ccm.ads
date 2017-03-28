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

package NRF51_SVD.CCM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Shortcut between ENDKSGEN event and CRYPT task.
   type SHORTS_ENDKSGEN_CRYPT_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_ENDKSGEN_CRYPT_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcuts for the CCM.
   type SHORTS_Register is record
      --  Shortcut between ENDKSGEN event and CRYPT task.
      ENDKSGEN_CRYPT : SHORTS_ENDKSGEN_CRYPT_Field := NRF51_SVD.CCM.Disabled;
      --  unspecified
      Reserved_1_31  : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      ENDKSGEN_CRYPT at 0 range 0 .. 0;
      Reserved_1_31  at 0 range 1 .. 31;
   end record;

   --  Enable interrupt on ENDKSGEN event.
   type INTENSET_ENDKSGEN_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_ENDKSGEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on ENDKSGEN event.
   type INTENSET_ENDKSGEN_Field_1 is
     (
      --  Reset value for the field
      Intenset_Endksgen_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_ENDKSGEN_Field_1 use
     (Intenset_Endksgen_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on ENDCRYPT event.
   type INTENSET_ENDCRYPT_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_ENDCRYPT_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on ENDCRYPT event.
   type INTENSET_ENDCRYPT_Field_1 is
     (
      --  Reset value for the field
      Intenset_Endcrypt_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_ENDCRYPT_Field_1 use
     (Intenset_Endcrypt_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on ERROR event.
   type INTENSET_ERROR_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_ERROR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on ERROR event.
   type INTENSET_ERROR_Field_1 is
     (
      --  Reset value for the field
      Intenset_Error_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_ERROR_Field_1 use
     (Intenset_Error_Field_Reset => 0,
      Set => 1);

   --  Interrupt enable set register.
   type INTENSET_Register is record
      --  Enable interrupt on ENDKSGEN event.
      ENDKSGEN      : INTENSET_ENDKSGEN_Field_1 :=
                       Intenset_Endksgen_Field_Reset;
      --  Enable interrupt on ENDCRYPT event.
      ENDCRYPT      : INTENSET_ENDCRYPT_Field_1 :=
                       Intenset_Endcrypt_Field_Reset;
      --  Enable interrupt on ERROR event.
      ERROR         : INTENSET_ERROR_Field_1 := Intenset_Error_Field_Reset;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      ENDKSGEN      at 0 range 0 .. 0;
      ENDCRYPT      at 0 range 1 .. 1;
      ERROR         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Disable interrupt on ENDKSGEN event.
   type INTENCLR_ENDKSGEN_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_ENDKSGEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on ENDKSGEN event.
   type INTENCLR_ENDKSGEN_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Endksgen_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_ENDKSGEN_Field_1 use
     (Intenclr_Endksgen_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on ENDCRYPT event.
   type INTENCLR_ENDCRYPT_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_ENDCRYPT_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on ENDCRYPT event.
   type INTENCLR_ENDCRYPT_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Endcrypt_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_ENDCRYPT_Field_1 use
     (Intenclr_Endcrypt_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on ERROR event.
   type INTENCLR_ERROR_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_ERROR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on ERROR event.
   type INTENCLR_ERROR_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Error_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_ERROR_Field_1 use
     (Intenclr_Error_Field_Reset => 0,
      Clear => 1);

   --  Interrupt enable clear register.
   type INTENCLR_Register is record
      --  Disable interrupt on ENDKSGEN event.
      ENDKSGEN      : INTENCLR_ENDKSGEN_Field_1 :=
                       Intenclr_Endksgen_Field_Reset;
      --  Disable interrupt on ENDCRYPT event.
      ENDCRYPT      : INTENCLR_ENDCRYPT_Field_1 :=
                       Intenclr_Endcrypt_Field_Reset;
      --  Disable interrupt on ERROR event.
      ERROR         : INTENCLR_ERROR_Field_1 := Intenclr_Error_Field_Reset;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      ENDKSGEN      at 0 range 0 .. 0;
      ENDCRYPT      at 0 range 1 .. 1;
      ERROR         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Result of the MIC check performed during the previous CCM RX STARTCRYPT
   type MICSTATUS_MICSTATUS_Field is
     (
      --  MIC check failed.
      Checkfailed,
      --  MIC check passed.
      Checkpassed)
     with Size => 1;
   for MICSTATUS_MICSTATUS_Field use
     (Checkfailed => 0,
      Checkpassed => 1);

   --  CCM RX MIC check result.
   type MICSTATUS_Register is record
      --  Read-only. Result of the MIC check performed during the previous CCM
      --  RX STARTCRYPT
      MICSTATUS     : MICSTATUS_MICSTATUS_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MICSTATUS_Register use record
      MICSTATUS     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  CCM enable.
   type ENABLE_ENABLE_Field is
     (
      --  CCM is disabled.
      Disabled,
      --  CCM is enabled.
      Enabled)
     with Size => 2;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 2);

   --  CCM enable.
   type ENABLE_Register is record
      --  CCM enable.
      ENABLE        : ENABLE_ENABLE_Field := NRF51_SVD.CCM.Disabled;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  CCM mode operation.
   type MODE_MODE_Field is
     (
      --  CCM mode TX
      Encryption,
      --  CCM mode TX
      Decryption)
     with Size => 1;
   for MODE_MODE_Field use
     (Encryption => 0,
      Decryption => 1);

   --  Operation mode.
   type MODE_Register is record
      --  CCM mode operation.
      MODE          : MODE_MODE_Field := NRF51_SVD.CCM.Decryption;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MODE_Register use record
      MODE          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Peripheral power control.
   type POWER_POWER_Field is
     (
      --  Module power disabled.
      Disabled,
      --  Module power enabled.
      Enabled)
     with Size => 1;
   for POWER_POWER_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Peripheral power control.
   type POWER_Register is record
      --  Peripheral power control.
      POWER         : POWER_POWER_Field := NRF51_SVD.CCM.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for POWER_Register use record
      POWER         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  AES CCM Mode Encryption.
   type CCM_Peripheral is record
      --  Start generation of key-stream. This operation will stop by itself
      --  when completed.
      TASKS_KSGEN     : aliased HAL.UInt32;
      --  Start encrypt/decrypt. This operation will stop by itself when
      --  completed.
      TASKS_CRYPT     : aliased HAL.UInt32;
      --  Stop encrypt/decrypt.
      TASKS_STOP      : aliased HAL.UInt32;
      --  Keystream generation completed.
      EVENTS_ENDKSGEN : aliased HAL.UInt32;
      --  Encrypt/decrypt completed.
      EVENTS_ENDCRYPT : aliased HAL.UInt32;
      --  Error happened.
      EVENTS_ERROR    : aliased HAL.UInt32;
      --  Shortcuts for the CCM.
      SHORTS          : aliased SHORTS_Register;
      --  Interrupt enable set register.
      INTENSET        : aliased INTENSET_Register;
      --  Interrupt enable clear register.
      INTENCLR        : aliased INTENCLR_Register;
      --  CCM RX MIC check result.
      MICSTATUS       : aliased MICSTATUS_Register;
      --  CCM enable.
      ENABLE          : aliased ENABLE_Register;
      --  Operation mode.
      MODE            : aliased MODE_Register;
      --  Pointer to a data structure holding AES key and NONCE vector.
      CNFPTR          : aliased HAL.UInt32;
      --  Pointer to the input packet.
      INPTR           : aliased HAL.UInt32;
      --  Pointer to the output packet.
      OUTPTR          : aliased HAL.UInt32;
      --  Pointer to a "scratch" data area used for temporary storage during
      --  resolution. A minimum of 43 bytes must be reserved.
      SCRATCHPTR      : aliased HAL.UInt32;
      --  Peripheral power control.
      POWER           : aliased POWER_Register;
   end record
     with Volatile;

   for CCM_Peripheral use record
      TASKS_KSGEN     at 16#0# range 0 .. 31;
      TASKS_CRYPT     at 16#4# range 0 .. 31;
      TASKS_STOP      at 16#8# range 0 .. 31;
      EVENTS_ENDKSGEN at 16#100# range 0 .. 31;
      EVENTS_ENDCRYPT at 16#104# range 0 .. 31;
      EVENTS_ERROR    at 16#108# range 0 .. 31;
      SHORTS          at 16#200# range 0 .. 31;
      INTENSET        at 16#304# range 0 .. 31;
      INTENCLR        at 16#308# range 0 .. 31;
      MICSTATUS       at 16#400# range 0 .. 31;
      ENABLE          at 16#500# range 0 .. 31;
      MODE            at 16#504# range 0 .. 31;
      CNFPTR          at 16#508# range 0 .. 31;
      INPTR           at 16#50C# range 0 .. 31;
      OUTPTR          at 16#510# range 0 .. 31;
      SCRATCHPTR      at 16#514# range 0 .. 31;
      POWER           at 16#FFC# range 0 .. 31;
   end record;

   --  AES CCM Mode Encryption.
   CCM_Periph : aliased CCM_Peripheral
     with Import, Address => System'To_Address (16#4000F000#);

end NRF51_SVD.CCM;
