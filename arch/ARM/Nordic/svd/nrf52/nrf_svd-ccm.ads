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

package NRF_SVD.CCM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Shortcut between ENDKSGEN event and CRYPT task
   type SHORTS_ENDKSGEN_CRYPT_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_ENDKSGEN_CRYPT_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut register
   type SHORTS_Register is record
      --  Shortcut between ENDKSGEN event and CRYPT task
      ENDKSGEN_CRYPT : SHORTS_ENDKSGEN_CRYPT_Field := NRF_SVD.CCM.Disabled;
      --  unspecified
      Reserved_1_31  : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      ENDKSGEN_CRYPT at 0 range 0 .. 0;
      Reserved_1_31  at 0 range 1 .. 31;
   end record;

   --  Write '1' to Enable interrupt for ENDKSGEN event
   type INTENSET_ENDKSGEN_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_ENDKSGEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for ENDKSGEN event
   type INTENSET_ENDKSGEN_Field_1 is
     (--  Reset value for the field
      Intenset_Endksgen_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_ENDKSGEN_Field_1 use
     (Intenset_Endksgen_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for ENDCRYPT event
   type INTENSET_ENDCRYPT_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_ENDCRYPT_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for ENDCRYPT event
   type INTENSET_ENDCRYPT_Field_1 is
     (--  Reset value for the field
      Intenset_Endcrypt_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_ENDCRYPT_Field_1 use
     (Intenset_Endcrypt_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for ERROR event
   type INTENSET_ERROR_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_ERROR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for ERROR event
   type INTENSET_ERROR_Field_1 is
     (--  Reset value for the field
      Intenset_Error_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_ERROR_Field_1 use
     (Intenset_Error_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  Write '1' to Enable interrupt for ENDKSGEN event
      ENDKSGEN      : INTENSET_ENDKSGEN_Field_1 :=
                       Intenset_Endksgen_Field_Reset;
      --  Write '1' to Enable interrupt for ENDCRYPT event
      ENDCRYPT      : INTENSET_ENDCRYPT_Field_1 :=
                       Intenset_Endcrypt_Field_Reset;
      --  Write '1' to Enable interrupt for ERROR event
      ERROR         : INTENSET_ERROR_Field_1 := Intenset_Error_Field_Reset;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      ENDKSGEN      at 0 range 0 .. 0;
      ENDCRYPT      at 0 range 1 .. 1;
      ERROR         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Write '1' to Disable interrupt for ENDKSGEN event
   type INTENCLR_ENDKSGEN_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_ENDKSGEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for ENDKSGEN event
   type INTENCLR_ENDKSGEN_Field_1 is
     (--  Reset value for the field
      Intenclr_Endksgen_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_ENDKSGEN_Field_1 use
     (Intenclr_Endksgen_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for ENDCRYPT event
   type INTENCLR_ENDCRYPT_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_ENDCRYPT_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for ENDCRYPT event
   type INTENCLR_ENDCRYPT_Field_1 is
     (--  Reset value for the field
      Intenclr_Endcrypt_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_ENDCRYPT_Field_1 use
     (Intenclr_Endcrypt_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for ERROR event
   type INTENCLR_ERROR_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_ERROR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for ERROR event
   type INTENCLR_ERROR_Field_1 is
     (--  Reset value for the field
      Intenclr_Error_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_ERROR_Field_1 use
     (Intenclr_Error_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  Write '1' to Disable interrupt for ENDKSGEN event
      ENDKSGEN      : INTENCLR_ENDKSGEN_Field_1 :=
                       Intenclr_Endksgen_Field_Reset;
      --  Write '1' to Disable interrupt for ENDCRYPT event
      ENDCRYPT      : INTENCLR_ENDCRYPT_Field_1 :=
                       Intenclr_Endcrypt_Field_Reset;
      --  Write '1' to Disable interrupt for ERROR event
      ERROR         : INTENCLR_ERROR_Field_1 := Intenclr_Error_Field_Reset;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      ENDKSGEN      at 0 range 0 .. 0;
      ENDCRYPT      at 0 range 1 .. 1;
      ERROR         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  The result of the MIC check performed during the previous decryption
   --  operation
   type MICSTATUS_MICSTATUS_Field is
     (--  MIC check failed
      Checkfailed,
      --  MIC check passed
      Checkpassed)
     with Size => 1;
   for MICSTATUS_MICSTATUS_Field use
     (Checkfailed => 0,
      Checkpassed => 1);

   --  MIC check result
   type MICSTATUS_Register is record
      --  Read-only. The result of the MIC check performed during the previous
      --  decryption operation
      MICSTATUS     : MICSTATUS_MICSTATUS_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MICSTATUS_Register use record
      MICSTATUS     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Enable or disable CCM
   type ENABLE_ENABLE_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 2;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 2);

   --  Enable
   type ENABLE_Register is record
      --  Enable or disable CCM
      ENABLE        : ENABLE_ENABLE_Field := NRF_SVD.CCM.Disabled;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  The mode of operation to be used
   type MODE_MODE_Field is
     (--  AES CCM packet encryption mode
      Encryption,
      --  AES CCM packet decryption mode
      Decryption)
     with Size => 1;
   for MODE_MODE_Field use
     (Encryption => 0,
      Decryption => 1);

   --  Data rate that the CCM shall run in synch with
   type MODE_DATARATE_Field is
     (--  In synch with 1 Mbit data rate
      Val_1Mbit,
      --  In synch with 2 Mbit data rate
      Val_2Mbit)
     with Size => 1;
   for MODE_DATARATE_Field use
     (Val_1Mbit => 0,
      Val_2Mbit => 1);

   --  Packet length configuration
   type MODE_LENGTH_Field is
     (--  Default length. Effective length of LENGTH field is 5-bit
      Default,
      --  Extended length. Effective length of LENGTH field is 8-bit
      Extended)
     with Size => 1;
   for MODE_LENGTH_Field use
     (Default => 0,
      Extended => 1);

   --  Operation mode
   type MODE_Register is record
      --  The mode of operation to be used
      MODE           : MODE_MODE_Field := NRF_SVD.CCM.Decryption;
      --  unspecified
      Reserved_1_15  : HAL.UInt15 := 16#0#;
      --  Data rate that the CCM shall run in synch with
      DATARATE       : MODE_DATARATE_Field := NRF_SVD.CCM.Val_1Mbit;
      --  unspecified
      Reserved_17_23 : HAL.UInt7 := 16#0#;
      --  Packet length configuration
      LENGTH         : MODE_LENGTH_Field := NRF_SVD.CCM.Default;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MODE_Register use record
      MODE           at 0 range 0 .. 0;
      Reserved_1_15  at 0 range 1 .. 15;
      DATARATE       at 0 range 16 .. 16;
      Reserved_17_23 at 0 range 17 .. 23;
      LENGTH         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  AES CCM Mode Encryption
   type CCM_Peripheral is record
      --  Start generation of key-stream. This operation will stop by itself
      --  when completed.
      TASKS_KSGEN     : aliased HAL.UInt32;
      --  Start encryption/decryption. This operation will stop by itself when
      --  completed.
      TASKS_CRYPT     : aliased HAL.UInt32;
      --  Stop encryption/decryption
      TASKS_STOP      : aliased HAL.UInt32;
      --  Key-stream generation complete
      EVENTS_ENDKSGEN : aliased HAL.UInt32;
      --  Encrypt/decrypt complete
      EVENTS_ENDCRYPT : aliased HAL.UInt32;
      --  CCM error event
      EVENTS_ERROR    : aliased HAL.UInt32;
      --  Shortcut register
      SHORTS          : aliased SHORTS_Register;
      --  Enable interrupt
      INTENSET        : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR        : aliased INTENCLR_Register;
      --  MIC check result
      MICSTATUS       : aliased MICSTATUS_Register;
      --  Enable
      ENABLE          : aliased ENABLE_Register;
      --  Operation mode
      MODE            : aliased MODE_Register;
      --  Pointer to data structure holding AES key and NONCE vector
      CNFPTR          : aliased HAL.UInt32;
      --  Input pointer
      INPTR           : aliased HAL.UInt32;
      --  Output pointer
      OUTPTR          : aliased HAL.UInt32;
      --  Pointer to data area used for temporary storage
      SCRATCHPTR      : aliased HAL.UInt32;
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
   end record;

   --  AES CCM Mode Encryption
   CCM_Periph : aliased CCM_Peripheral
     with Import, Address => CCM_Base;

end NRF_SVD.CCM;
