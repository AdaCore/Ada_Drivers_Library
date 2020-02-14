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

package NRF_SVD.AAR is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Write '1' to Enable interrupt for END event
   type INTENSET_END_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_END_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for END event
   type INTENSET_END_Field_1 is
     (--  Reset value for the field
      Intenset_End_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_END_Field_1 use
     (Intenset_End_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for RESOLVED event
   type INTENSET_RESOLVED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_RESOLVED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for RESOLVED event
   type INTENSET_RESOLVED_Field_1 is
     (--  Reset value for the field
      Intenset_Resolved_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_RESOLVED_Field_1 use
     (Intenset_Resolved_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for NOTRESOLVED event
   type INTENSET_NOTRESOLVED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_NOTRESOLVED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for NOTRESOLVED event
   type INTENSET_NOTRESOLVED_Field_1 is
     (--  Reset value for the field
      Intenset_Notresolved_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_NOTRESOLVED_Field_1 use
     (Intenset_Notresolved_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  Write '1' to Enable interrupt for END event
      END_k         : INTENSET_END_Field_1 := Intenset_End_Field_Reset;
      --  Write '1' to Enable interrupt for RESOLVED event
      RESOLVED      : INTENSET_RESOLVED_Field_1 :=
                       Intenset_Resolved_Field_Reset;
      --  Write '1' to Enable interrupt for NOTRESOLVED event
      NOTRESOLVED   : INTENSET_NOTRESOLVED_Field_1 :=
                       Intenset_Notresolved_Field_Reset;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      END_k         at 0 range 0 .. 0;
      RESOLVED      at 0 range 1 .. 1;
      NOTRESOLVED   at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Write '1' to Disable interrupt for END event
   type INTENCLR_END_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_END_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for END event
   type INTENCLR_END_Field_1 is
     (--  Reset value for the field
      Intenclr_End_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_END_Field_1 use
     (Intenclr_End_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for RESOLVED event
   type INTENCLR_RESOLVED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_RESOLVED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for RESOLVED event
   type INTENCLR_RESOLVED_Field_1 is
     (--  Reset value for the field
      Intenclr_Resolved_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_RESOLVED_Field_1 use
     (Intenclr_Resolved_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for NOTRESOLVED event
   type INTENCLR_NOTRESOLVED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_NOTRESOLVED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for NOTRESOLVED event
   type INTENCLR_NOTRESOLVED_Field_1 is
     (--  Reset value for the field
      Intenclr_Notresolved_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_NOTRESOLVED_Field_1 use
     (Intenclr_Notresolved_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  Write '1' to Disable interrupt for END event
      END_k         : INTENCLR_END_Field_1 := Intenclr_End_Field_Reset;
      --  Write '1' to Disable interrupt for RESOLVED event
      RESOLVED      : INTENCLR_RESOLVED_Field_1 :=
                       Intenclr_Resolved_Field_Reset;
      --  Write '1' to Disable interrupt for NOTRESOLVED event
      NOTRESOLVED   : INTENCLR_NOTRESOLVED_Field_1 :=
                       Intenclr_Notresolved_Field_Reset;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      END_k         at 0 range 0 .. 0;
      RESOLVED      at 0 range 1 .. 1;
      NOTRESOLVED   at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype STATUS_STATUS_Field is HAL.UInt4;

   --  Resolution status
   type STATUS_Register is record
      --  Read-only. The IRK that was used last time an address was resolved
      STATUS        : STATUS_STATUS_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for STATUS_Register use record
      STATUS        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Enable or disable AAR
   type ENABLE_ENABLE_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 2;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 3);

   --  Enable AAR
   type ENABLE_Register is record
      --  Enable or disable AAR
      ENABLE        : ENABLE_ENABLE_Field := NRF_SVD.AAR.Disabled;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype NIRK_NIRK_Field is HAL.UInt5;

   --  Number of IRKs
   type NIRK_Register is record
      --  Number of Identity root keys available in the IRK data structure
      NIRK          : NIRK_NIRK_Field := 16#1#;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NIRK_Register use record
      NIRK          at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Accelerated Address Resolver
   type AAR_Peripheral is record
      --  Start resolving addresses based on IRKs specified in the IRK data
      --  structure
      TASKS_START        : aliased HAL.UInt32;
      --  Stop resolving addresses
      TASKS_STOP         : aliased HAL.UInt32;
      --  Address resolution procedure complete
      EVENTS_END         : aliased HAL.UInt32;
      --  Address resolved
      EVENTS_RESOLVED    : aliased HAL.UInt32;
      --  Address not resolved
      EVENTS_NOTRESOLVED : aliased HAL.UInt32;
      --  Enable interrupt
      INTENSET           : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR           : aliased INTENCLR_Register;
      --  Resolution status
      STATUS             : aliased STATUS_Register;
      --  Enable AAR
      ENABLE             : aliased ENABLE_Register;
      --  Number of IRKs
      NIRK               : aliased NIRK_Register;
      --  Pointer to IRK data structure
      IRKPTR             : aliased HAL.UInt32;
      --  Pointer to the resolvable address
      ADDRPTR            : aliased HAL.UInt32;
      --  Pointer to data area used for temporary storage
      SCRATCHPTR         : aliased HAL.UInt32;
   end record
     with Volatile;

   for AAR_Peripheral use record
      TASKS_START        at 16#0# range 0 .. 31;
      TASKS_STOP         at 16#8# range 0 .. 31;
      EVENTS_END         at 16#100# range 0 .. 31;
      EVENTS_RESOLVED    at 16#104# range 0 .. 31;
      EVENTS_NOTRESOLVED at 16#108# range 0 .. 31;
      INTENSET           at 16#304# range 0 .. 31;
      INTENCLR           at 16#308# range 0 .. 31;
      STATUS             at 16#400# range 0 .. 31;
      ENABLE             at 16#500# range 0 .. 31;
      NIRK               at 16#504# range 0 .. 31;
      IRKPTR             at 16#508# range 0 .. 31;
      ADDRPTR            at 16#510# range 0 .. 31;
      SCRATCHPTR         at 16#514# range 0 .. 31;
   end record;

   --  Accelerated Address Resolver
   AAR_Periph : aliased AAR_Peripheral
     with Import, Address => AAR_Base;

end NRF_SVD.AAR;
