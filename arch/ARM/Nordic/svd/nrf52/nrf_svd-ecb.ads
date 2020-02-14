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

package NRF_SVD.ECB is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Write '1' to Enable interrupt for ENDECB event
   type INTENSET_ENDECB_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_ENDECB_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for ENDECB event
   type INTENSET_ENDECB_Field_1 is
     (--  Reset value for the field
      Intenset_Endecb_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_ENDECB_Field_1 use
     (Intenset_Endecb_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for ERRORECB event
   type INTENSET_ERRORECB_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_ERRORECB_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for ERRORECB event
   type INTENSET_ERRORECB_Field_1 is
     (--  Reset value for the field
      Intenset_Errorecb_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_ERRORECB_Field_1 use
     (Intenset_Errorecb_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  Write '1' to Enable interrupt for ENDECB event
      ENDECB        : INTENSET_ENDECB_Field_1 := Intenset_Endecb_Field_Reset;
      --  Write '1' to Enable interrupt for ERRORECB event
      ERRORECB      : INTENSET_ERRORECB_Field_1 :=
                       Intenset_Errorecb_Field_Reset;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      ENDECB        at 0 range 0 .. 0;
      ERRORECB      at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Write '1' to Disable interrupt for ENDECB event
   type INTENCLR_ENDECB_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_ENDECB_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for ENDECB event
   type INTENCLR_ENDECB_Field_1 is
     (--  Reset value for the field
      Intenclr_Endecb_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_ENDECB_Field_1 use
     (Intenclr_Endecb_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for ERRORECB event
   type INTENCLR_ERRORECB_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_ERRORECB_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for ERRORECB event
   type INTENCLR_ERRORECB_Field_1 is
     (--  Reset value for the field
      Intenclr_Errorecb_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_ERRORECB_Field_1 use
     (Intenclr_Errorecb_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  Write '1' to Disable interrupt for ENDECB event
      ENDECB        : INTENCLR_ENDECB_Field_1 := Intenclr_Endecb_Field_Reset;
      --  Write '1' to Disable interrupt for ERRORECB event
      ERRORECB      : INTENCLR_ERRORECB_Field_1 :=
                       Intenclr_Errorecb_Field_Reset;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      ENDECB        at 0 range 0 .. 0;
      ERRORECB      at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  AES ECB Mode Encryption
   type ECB_Peripheral is record
      --  Start ECB block encrypt
      TASKS_STARTECB  : aliased HAL.UInt32;
      --  Abort a possible executing ECB operation
      TASKS_STOPECB   : aliased HAL.UInt32;
      --  ECB block encrypt complete
      EVENTS_ENDECB   : aliased HAL.UInt32;
      --  ECB block encrypt aborted because of a STOPECB task or due to an
      --  error
      EVENTS_ERRORECB : aliased HAL.UInt32;
      --  Enable interrupt
      INTENSET        : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR        : aliased INTENCLR_Register;
      --  ECB block encrypt memory pointers
      ECBDATAPTR      : aliased HAL.UInt32;
   end record
     with Volatile;

   for ECB_Peripheral use record
      TASKS_STARTECB  at 16#0# range 0 .. 31;
      TASKS_STOPECB   at 16#4# range 0 .. 31;
      EVENTS_ENDECB   at 16#100# range 0 .. 31;
      EVENTS_ERRORECB at 16#104# range 0 .. 31;
      INTENSET        at 16#304# range 0 .. 31;
      INTENCLR        at 16#308# range 0 .. 31;
      ECBDATAPTR      at 16#504# range 0 .. 31;
   end record;

   --  AES ECB Mode Encryption
   ECB_Periph : aliased ECB_Peripheral
     with Import, Address => ECB_Base;

end NRF_SVD.ECB;
