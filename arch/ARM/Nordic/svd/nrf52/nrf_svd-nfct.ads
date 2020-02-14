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

package NRF_SVD.NFCT is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Shortcut between FIELDDETECTED event and ACTIVATE task
   type SHORTS_FIELDDETECTED_ACTIVATE_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_FIELDDETECTED_ACTIVATE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between FIELDLOST event and SENSE task
   type SHORTS_FIELDLOST_SENSE_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_FIELDLOST_SENSE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut register
   type SHORTS_Register is record
      --  Shortcut between FIELDDETECTED event and ACTIVATE task
      FIELDDETECTED_ACTIVATE : SHORTS_FIELDDETECTED_ACTIVATE_Field :=
                                NRF_SVD.NFCT.Disabled;
      --  Shortcut between FIELDLOST event and SENSE task
      FIELDLOST_SENSE        : SHORTS_FIELDLOST_SENSE_Field :=
                                NRF_SVD.NFCT.Disabled;
      --  unspecified
      Reserved_2_31          : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      FIELDDETECTED_ACTIVATE at 0 range 0 .. 0;
      FIELDLOST_SENSE        at 0 range 1 .. 1;
      Reserved_2_31          at 0 range 2 .. 31;
   end record;

   --  Enable or disable interrupt for READY event
   type INTEN_READY_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_READY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for FIELDDETECTED event
   type INTEN_FIELDDETECTED_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_FIELDDETECTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for FIELDLOST event
   type INTEN_FIELDLOST_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_FIELDLOST_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for TXFRAMESTART event
   type INTEN_TXFRAMESTART_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_TXFRAMESTART_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for TXFRAMEEND event
   type INTEN_TXFRAMEEND_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_TXFRAMEEND_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for RXFRAMESTART event
   type INTEN_RXFRAMESTART_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_RXFRAMESTART_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for RXFRAMEEND event
   type INTEN_RXFRAMEEND_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_RXFRAMEEND_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for ERROR event
   type INTEN_ERROR_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_ERROR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for RXERROR event
   type INTEN_RXERROR_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_RXERROR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for ENDRX event
   type INTEN_ENDRX_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_ENDRX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for ENDTX event
   type INTEN_ENDTX_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_ENDTX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for AUTOCOLRESSTARTED event
   type INTEN_AUTOCOLRESSTARTED_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_AUTOCOLRESSTARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for COLLISION event
   type INTEN_COLLISION_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_COLLISION_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for SELECTED event
   type INTEN_SELECTED_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_SELECTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for STARTED event
   type INTEN_STARTED_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_STARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt
   type INTEN_Register is record
      --  Enable or disable interrupt for READY event
      READY             : INTEN_READY_Field := NRF_SVD.NFCT.Disabled;
      --  Enable or disable interrupt for FIELDDETECTED event
      FIELDDETECTED     : INTEN_FIELDDETECTED_Field := NRF_SVD.NFCT.Disabled;
      --  Enable or disable interrupt for FIELDLOST event
      FIELDLOST         : INTEN_FIELDLOST_Field := NRF_SVD.NFCT.Disabled;
      --  Enable or disable interrupt for TXFRAMESTART event
      TXFRAMESTART      : INTEN_TXFRAMESTART_Field := NRF_SVD.NFCT.Disabled;
      --  Enable or disable interrupt for TXFRAMEEND event
      TXFRAMEEND        : INTEN_TXFRAMEEND_Field := NRF_SVD.NFCT.Disabled;
      --  Enable or disable interrupt for RXFRAMESTART event
      RXFRAMESTART      : INTEN_RXFRAMESTART_Field := NRF_SVD.NFCT.Disabled;
      --  Enable or disable interrupt for RXFRAMEEND event
      RXFRAMEEND        : INTEN_RXFRAMEEND_Field := NRF_SVD.NFCT.Disabled;
      --  Enable or disable interrupt for ERROR event
      ERROR             : INTEN_ERROR_Field := NRF_SVD.NFCT.Disabled;
      --  unspecified
      Reserved_8_9      : HAL.UInt2 := 16#0#;
      --  Enable or disable interrupt for RXERROR event
      RXERROR           : INTEN_RXERROR_Field := NRF_SVD.NFCT.Disabled;
      --  Enable or disable interrupt for ENDRX event
      ENDRX             : INTEN_ENDRX_Field := NRF_SVD.NFCT.Disabled;
      --  Enable or disable interrupt for ENDTX event
      ENDTX             : INTEN_ENDTX_Field := NRF_SVD.NFCT.Disabled;
      --  unspecified
      Reserved_13_13    : HAL.Bit := 16#0#;
      --  Enable or disable interrupt for AUTOCOLRESSTARTED event
      AUTOCOLRESSTARTED : INTEN_AUTOCOLRESSTARTED_Field :=
                           NRF_SVD.NFCT.Disabled;
      --  unspecified
      Reserved_15_17    : HAL.UInt3 := 16#0#;
      --  Enable or disable interrupt for COLLISION event
      COLLISION         : INTEN_COLLISION_Field := NRF_SVD.NFCT.Disabled;
      --  Enable or disable interrupt for SELECTED event
      SELECTED          : INTEN_SELECTED_Field := NRF_SVD.NFCT.Disabled;
      --  Enable or disable interrupt for STARTED event
      STARTED           : INTEN_STARTED_Field := NRF_SVD.NFCT.Disabled;
      --  unspecified
      Reserved_21_31    : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTEN_Register use record
      READY             at 0 range 0 .. 0;
      FIELDDETECTED     at 0 range 1 .. 1;
      FIELDLOST         at 0 range 2 .. 2;
      TXFRAMESTART      at 0 range 3 .. 3;
      TXFRAMEEND        at 0 range 4 .. 4;
      RXFRAMESTART      at 0 range 5 .. 5;
      RXFRAMEEND        at 0 range 6 .. 6;
      ERROR             at 0 range 7 .. 7;
      Reserved_8_9      at 0 range 8 .. 9;
      RXERROR           at 0 range 10 .. 10;
      ENDRX             at 0 range 11 .. 11;
      ENDTX             at 0 range 12 .. 12;
      Reserved_13_13    at 0 range 13 .. 13;
      AUTOCOLRESSTARTED at 0 range 14 .. 14;
      Reserved_15_17    at 0 range 15 .. 17;
      COLLISION         at 0 range 18 .. 18;
      SELECTED          at 0 range 19 .. 19;
      STARTED           at 0 range 20 .. 20;
      Reserved_21_31    at 0 range 21 .. 31;
   end record;

   --  Write '1' to Enable interrupt for READY event
   type INTENSET_READY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_READY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for READY event
   type INTENSET_READY_Field_1 is
     (--  Reset value for the field
      Intenset_Ready_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_READY_Field_1 use
     (Intenset_Ready_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for FIELDDETECTED event
   type INTENSET_FIELDDETECTED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_FIELDDETECTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for FIELDDETECTED event
   type INTENSET_FIELDDETECTED_Field_1 is
     (--  Reset value for the field
      Intenset_Fielddetected_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_FIELDDETECTED_Field_1 use
     (Intenset_Fielddetected_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for FIELDLOST event
   type INTENSET_FIELDLOST_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_FIELDLOST_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for FIELDLOST event
   type INTENSET_FIELDLOST_Field_1 is
     (--  Reset value for the field
      Intenset_Fieldlost_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_FIELDLOST_Field_1 use
     (Intenset_Fieldlost_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for TXFRAMESTART event
   type INTENSET_TXFRAMESTART_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_TXFRAMESTART_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for TXFRAMESTART event
   type INTENSET_TXFRAMESTART_Field_1 is
     (--  Reset value for the field
      Intenset_Txframestart_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_TXFRAMESTART_Field_1 use
     (Intenset_Txframestart_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for TXFRAMEEND event
   type INTENSET_TXFRAMEEND_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_TXFRAMEEND_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for TXFRAMEEND event
   type INTENSET_TXFRAMEEND_Field_1 is
     (--  Reset value for the field
      Intenset_Txframeend_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_TXFRAMEEND_Field_1 use
     (Intenset_Txframeend_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for RXFRAMESTART event
   type INTENSET_RXFRAMESTART_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_RXFRAMESTART_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for RXFRAMESTART event
   type INTENSET_RXFRAMESTART_Field_1 is
     (--  Reset value for the field
      Intenset_Rxframestart_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_RXFRAMESTART_Field_1 use
     (Intenset_Rxframestart_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for RXFRAMEEND event
   type INTENSET_RXFRAMEEND_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_RXFRAMEEND_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for RXFRAMEEND event
   type INTENSET_RXFRAMEEND_Field_1 is
     (--  Reset value for the field
      Intenset_Rxframeend_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_RXFRAMEEND_Field_1 use
     (Intenset_Rxframeend_Field_Reset => 0,
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

   --  Write '1' to Enable interrupt for RXERROR event
   type INTENSET_RXERROR_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_RXERROR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for RXERROR event
   type INTENSET_RXERROR_Field_1 is
     (--  Reset value for the field
      Intenset_Rxerror_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_RXERROR_Field_1 use
     (Intenset_Rxerror_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for ENDRX event
   type INTENSET_ENDRX_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_ENDRX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for ENDRX event
   type INTENSET_ENDRX_Field_1 is
     (--  Reset value for the field
      Intenset_Endrx_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_ENDRX_Field_1 use
     (Intenset_Endrx_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for ENDTX event
   type INTENSET_ENDTX_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_ENDTX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for ENDTX event
   type INTENSET_ENDTX_Field_1 is
     (--  Reset value for the field
      Intenset_Endtx_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_ENDTX_Field_1 use
     (Intenset_Endtx_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for AUTOCOLRESSTARTED event
   type INTENSET_AUTOCOLRESSTARTED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_AUTOCOLRESSTARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for AUTOCOLRESSTARTED event
   type INTENSET_AUTOCOLRESSTARTED_Field_1 is
     (--  Reset value for the field
      Intenset_Autocolresstarted_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_AUTOCOLRESSTARTED_Field_1 use
     (Intenset_Autocolresstarted_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for COLLISION event
   type INTENSET_COLLISION_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_COLLISION_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for COLLISION event
   type INTENSET_COLLISION_Field_1 is
     (--  Reset value for the field
      Intenset_Collision_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_COLLISION_Field_1 use
     (Intenset_Collision_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for SELECTED event
   type INTENSET_SELECTED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_SELECTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for SELECTED event
   type INTENSET_SELECTED_Field_1 is
     (--  Reset value for the field
      Intenset_Selected_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_SELECTED_Field_1 use
     (Intenset_Selected_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for STARTED event
   type INTENSET_STARTED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_STARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for STARTED event
   type INTENSET_STARTED_Field_1 is
     (--  Reset value for the field
      Intenset_Started_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_STARTED_Field_1 use
     (Intenset_Started_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  Write '1' to Enable interrupt for READY event
      READY             : INTENSET_READY_Field_1 :=
                           Intenset_Ready_Field_Reset;
      --  Write '1' to Enable interrupt for FIELDDETECTED event
      FIELDDETECTED     : INTENSET_FIELDDETECTED_Field_1 :=
                           Intenset_Fielddetected_Field_Reset;
      --  Write '1' to Enable interrupt for FIELDLOST event
      FIELDLOST         : INTENSET_FIELDLOST_Field_1 :=
                           Intenset_Fieldlost_Field_Reset;
      --  Write '1' to Enable interrupt for TXFRAMESTART event
      TXFRAMESTART      : INTENSET_TXFRAMESTART_Field_1 :=
                           Intenset_Txframestart_Field_Reset;
      --  Write '1' to Enable interrupt for TXFRAMEEND event
      TXFRAMEEND        : INTENSET_TXFRAMEEND_Field_1 :=
                           Intenset_Txframeend_Field_Reset;
      --  Write '1' to Enable interrupt for RXFRAMESTART event
      RXFRAMESTART      : INTENSET_RXFRAMESTART_Field_1 :=
                           Intenset_Rxframestart_Field_Reset;
      --  Write '1' to Enable interrupt for RXFRAMEEND event
      RXFRAMEEND        : INTENSET_RXFRAMEEND_Field_1 :=
                           Intenset_Rxframeend_Field_Reset;
      --  Write '1' to Enable interrupt for ERROR event
      ERROR             : INTENSET_ERROR_Field_1 :=
                           Intenset_Error_Field_Reset;
      --  unspecified
      Reserved_8_9      : HAL.UInt2 := 16#0#;
      --  Write '1' to Enable interrupt for RXERROR event
      RXERROR           : INTENSET_RXERROR_Field_1 :=
                           Intenset_Rxerror_Field_Reset;
      --  Write '1' to Enable interrupt for ENDRX event
      ENDRX             : INTENSET_ENDRX_Field_1 :=
                           Intenset_Endrx_Field_Reset;
      --  Write '1' to Enable interrupt for ENDTX event
      ENDTX             : INTENSET_ENDTX_Field_1 :=
                           Intenset_Endtx_Field_Reset;
      --  unspecified
      Reserved_13_13    : HAL.Bit := 16#0#;
      --  Write '1' to Enable interrupt for AUTOCOLRESSTARTED event
      AUTOCOLRESSTARTED : INTENSET_AUTOCOLRESSTARTED_Field_1 :=
                           Intenset_Autocolresstarted_Field_Reset;
      --  unspecified
      Reserved_15_17    : HAL.UInt3 := 16#0#;
      --  Write '1' to Enable interrupt for COLLISION event
      COLLISION         : INTENSET_COLLISION_Field_1 :=
                           Intenset_Collision_Field_Reset;
      --  Write '1' to Enable interrupt for SELECTED event
      SELECTED          : INTENSET_SELECTED_Field_1 :=
                           Intenset_Selected_Field_Reset;
      --  Write '1' to Enable interrupt for STARTED event
      STARTED           : INTENSET_STARTED_Field_1 :=
                           Intenset_Started_Field_Reset;
      --  unspecified
      Reserved_21_31    : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      READY             at 0 range 0 .. 0;
      FIELDDETECTED     at 0 range 1 .. 1;
      FIELDLOST         at 0 range 2 .. 2;
      TXFRAMESTART      at 0 range 3 .. 3;
      TXFRAMEEND        at 0 range 4 .. 4;
      RXFRAMESTART      at 0 range 5 .. 5;
      RXFRAMEEND        at 0 range 6 .. 6;
      ERROR             at 0 range 7 .. 7;
      Reserved_8_9      at 0 range 8 .. 9;
      RXERROR           at 0 range 10 .. 10;
      ENDRX             at 0 range 11 .. 11;
      ENDTX             at 0 range 12 .. 12;
      Reserved_13_13    at 0 range 13 .. 13;
      AUTOCOLRESSTARTED at 0 range 14 .. 14;
      Reserved_15_17    at 0 range 15 .. 17;
      COLLISION         at 0 range 18 .. 18;
      SELECTED          at 0 range 19 .. 19;
      STARTED           at 0 range 20 .. 20;
      Reserved_21_31    at 0 range 21 .. 31;
   end record;

   --  Write '1' to Disable interrupt for READY event
   type INTENCLR_READY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_READY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for READY event
   type INTENCLR_READY_Field_1 is
     (--  Reset value for the field
      Intenclr_Ready_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_READY_Field_1 use
     (Intenclr_Ready_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for FIELDDETECTED event
   type INTENCLR_FIELDDETECTED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_FIELDDETECTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for FIELDDETECTED event
   type INTENCLR_FIELDDETECTED_Field_1 is
     (--  Reset value for the field
      Intenclr_Fielddetected_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_FIELDDETECTED_Field_1 use
     (Intenclr_Fielddetected_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for FIELDLOST event
   type INTENCLR_FIELDLOST_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_FIELDLOST_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for FIELDLOST event
   type INTENCLR_FIELDLOST_Field_1 is
     (--  Reset value for the field
      Intenclr_Fieldlost_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_FIELDLOST_Field_1 use
     (Intenclr_Fieldlost_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for TXFRAMESTART event
   type INTENCLR_TXFRAMESTART_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_TXFRAMESTART_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for TXFRAMESTART event
   type INTENCLR_TXFRAMESTART_Field_1 is
     (--  Reset value for the field
      Intenclr_Txframestart_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_TXFRAMESTART_Field_1 use
     (Intenclr_Txframestart_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for TXFRAMEEND event
   type INTENCLR_TXFRAMEEND_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_TXFRAMEEND_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for TXFRAMEEND event
   type INTENCLR_TXFRAMEEND_Field_1 is
     (--  Reset value for the field
      Intenclr_Txframeend_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_TXFRAMEEND_Field_1 use
     (Intenclr_Txframeend_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for RXFRAMESTART event
   type INTENCLR_RXFRAMESTART_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_RXFRAMESTART_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for RXFRAMESTART event
   type INTENCLR_RXFRAMESTART_Field_1 is
     (--  Reset value for the field
      Intenclr_Rxframestart_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_RXFRAMESTART_Field_1 use
     (Intenclr_Rxframestart_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for RXFRAMEEND event
   type INTENCLR_RXFRAMEEND_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_RXFRAMEEND_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for RXFRAMEEND event
   type INTENCLR_RXFRAMEEND_Field_1 is
     (--  Reset value for the field
      Intenclr_Rxframeend_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_RXFRAMEEND_Field_1 use
     (Intenclr_Rxframeend_Field_Reset => 0,
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

   --  Write '1' to Disable interrupt for RXERROR event
   type INTENCLR_RXERROR_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_RXERROR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for RXERROR event
   type INTENCLR_RXERROR_Field_1 is
     (--  Reset value for the field
      Intenclr_Rxerror_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_RXERROR_Field_1 use
     (Intenclr_Rxerror_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for ENDRX event
   type INTENCLR_ENDRX_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_ENDRX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for ENDRX event
   type INTENCLR_ENDRX_Field_1 is
     (--  Reset value for the field
      Intenclr_Endrx_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_ENDRX_Field_1 use
     (Intenclr_Endrx_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for ENDTX event
   type INTENCLR_ENDTX_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_ENDTX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for ENDTX event
   type INTENCLR_ENDTX_Field_1 is
     (--  Reset value for the field
      Intenclr_Endtx_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_ENDTX_Field_1 use
     (Intenclr_Endtx_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for AUTOCOLRESSTARTED event
   type INTENCLR_AUTOCOLRESSTARTED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_AUTOCOLRESSTARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for AUTOCOLRESSTARTED event
   type INTENCLR_AUTOCOLRESSTARTED_Field_1 is
     (--  Reset value for the field
      Intenclr_Autocolresstarted_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_AUTOCOLRESSTARTED_Field_1 use
     (Intenclr_Autocolresstarted_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for COLLISION event
   type INTENCLR_COLLISION_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_COLLISION_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for COLLISION event
   type INTENCLR_COLLISION_Field_1 is
     (--  Reset value for the field
      Intenclr_Collision_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_COLLISION_Field_1 use
     (Intenclr_Collision_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for SELECTED event
   type INTENCLR_SELECTED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_SELECTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for SELECTED event
   type INTENCLR_SELECTED_Field_1 is
     (--  Reset value for the field
      Intenclr_Selected_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_SELECTED_Field_1 use
     (Intenclr_Selected_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for STARTED event
   type INTENCLR_STARTED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_STARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for STARTED event
   type INTENCLR_STARTED_Field_1 is
     (--  Reset value for the field
      Intenclr_Started_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_STARTED_Field_1 use
     (Intenclr_Started_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  Write '1' to Disable interrupt for READY event
      READY             : INTENCLR_READY_Field_1 :=
                           Intenclr_Ready_Field_Reset;
      --  Write '1' to Disable interrupt for FIELDDETECTED event
      FIELDDETECTED     : INTENCLR_FIELDDETECTED_Field_1 :=
                           Intenclr_Fielddetected_Field_Reset;
      --  Write '1' to Disable interrupt for FIELDLOST event
      FIELDLOST         : INTENCLR_FIELDLOST_Field_1 :=
                           Intenclr_Fieldlost_Field_Reset;
      --  Write '1' to Disable interrupt for TXFRAMESTART event
      TXFRAMESTART      : INTENCLR_TXFRAMESTART_Field_1 :=
                           Intenclr_Txframestart_Field_Reset;
      --  Write '1' to Disable interrupt for TXFRAMEEND event
      TXFRAMEEND        : INTENCLR_TXFRAMEEND_Field_1 :=
                           Intenclr_Txframeend_Field_Reset;
      --  Write '1' to Disable interrupt for RXFRAMESTART event
      RXFRAMESTART      : INTENCLR_RXFRAMESTART_Field_1 :=
                           Intenclr_Rxframestart_Field_Reset;
      --  Write '1' to Disable interrupt for RXFRAMEEND event
      RXFRAMEEND        : INTENCLR_RXFRAMEEND_Field_1 :=
                           Intenclr_Rxframeend_Field_Reset;
      --  Write '1' to Disable interrupt for ERROR event
      ERROR             : INTENCLR_ERROR_Field_1 :=
                           Intenclr_Error_Field_Reset;
      --  unspecified
      Reserved_8_9      : HAL.UInt2 := 16#0#;
      --  Write '1' to Disable interrupt for RXERROR event
      RXERROR           : INTENCLR_RXERROR_Field_1 :=
                           Intenclr_Rxerror_Field_Reset;
      --  Write '1' to Disable interrupt for ENDRX event
      ENDRX             : INTENCLR_ENDRX_Field_1 :=
                           Intenclr_Endrx_Field_Reset;
      --  Write '1' to Disable interrupt for ENDTX event
      ENDTX             : INTENCLR_ENDTX_Field_1 :=
                           Intenclr_Endtx_Field_Reset;
      --  unspecified
      Reserved_13_13    : HAL.Bit := 16#0#;
      --  Write '1' to Disable interrupt for AUTOCOLRESSTARTED event
      AUTOCOLRESSTARTED : INTENCLR_AUTOCOLRESSTARTED_Field_1 :=
                           Intenclr_Autocolresstarted_Field_Reset;
      --  unspecified
      Reserved_15_17    : HAL.UInt3 := 16#0#;
      --  Write '1' to Disable interrupt for COLLISION event
      COLLISION         : INTENCLR_COLLISION_Field_1 :=
                           Intenclr_Collision_Field_Reset;
      --  Write '1' to Disable interrupt for SELECTED event
      SELECTED          : INTENCLR_SELECTED_Field_1 :=
                           Intenclr_Selected_Field_Reset;
      --  Write '1' to Disable interrupt for STARTED event
      STARTED           : INTENCLR_STARTED_Field_1 :=
                           Intenclr_Started_Field_Reset;
      --  unspecified
      Reserved_21_31    : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      READY             at 0 range 0 .. 0;
      FIELDDETECTED     at 0 range 1 .. 1;
      FIELDLOST         at 0 range 2 .. 2;
      TXFRAMESTART      at 0 range 3 .. 3;
      TXFRAMEEND        at 0 range 4 .. 4;
      RXFRAMESTART      at 0 range 5 .. 5;
      RXFRAMEEND        at 0 range 6 .. 6;
      ERROR             at 0 range 7 .. 7;
      Reserved_8_9      at 0 range 8 .. 9;
      RXERROR           at 0 range 10 .. 10;
      ENDRX             at 0 range 11 .. 11;
      ENDTX             at 0 range 12 .. 12;
      Reserved_13_13    at 0 range 13 .. 13;
      AUTOCOLRESSTARTED at 0 range 14 .. 14;
      Reserved_15_17    at 0 range 15 .. 17;
      COLLISION         at 0 range 18 .. 18;
      SELECTED          at 0 range 19 .. 19;
      STARTED           at 0 range 20 .. 20;
      Reserved_21_31    at 0 range 21 .. 31;
   end record;

   --  NFC Error Status register
   type ERRORSTATUS_Register is record
      --  No STARTTX task triggered before expiration of the time set in
      --  FRAMEDELAYMAX
      FRAMEDELAYTIMEOUT : Boolean := False;
      --  unspecified
      Reserved_1_1      : HAL.Bit := 16#0#;
      --  Field level is too high at max load resistance
      NFCFIELDTOOSTRONG : Boolean := False;
      --  Field level is too low at min load resistance
      NFCFIELDTOOWEAK   : Boolean := False;
      --  unspecified
      Reserved_4_31     : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ERRORSTATUS_Register use record
      FRAMEDELAYTIMEOUT at 0 range 0 .. 0;
      Reserved_1_1      at 0 range 1 .. 1;
      NFCFIELDTOOSTRONG at 0 range 2 .. 2;
      NFCFIELDTOOWEAK   at 0 range 3 .. 3;
      Reserved_4_31     at 0 range 4 .. 31;
   end record;

   -------------------------------------
   -- FRAMESTATUS cluster's Registers --
   -------------------------------------

   --  No valid End of Frame detected
   type RX_CRCERROR_Field is
     (--  Valid CRC detected
      Crccorrect,
      --  CRC received does not match local check
      Crcerror)
     with Size => 1;
   for RX_CRCERROR_Field use
     (Crccorrect => 0,
      Crcerror => 1);

   --  Parity status of received frame
   type RX_PARITYSTATUS_Field is
     (--  Frame received with parity OK
      Parityok,
      --  Frame received with parity error
      Parityerror)
     with Size => 1;
   for RX_PARITYSTATUS_Field use
     (Parityok => 0,
      Parityerror => 1);

   --  Overrun detected
   type RX_OVERRUN_Field is
     (--  No overrun detected
      Nooverrun,
      --  Overrun error
      Overrun)
     with Size => 1;
   for RX_OVERRUN_Field use
     (Nooverrun => 0,
      Overrun => 1);

   --  Result of last incoming frames
   type RX_FRAMESTATUS_Register is record
      --  No valid End of Frame detected
      CRCERROR      : RX_CRCERROR_Field := NRF_SVD.NFCT.Crccorrect;
      --  unspecified
      Reserved_1_1  : HAL.Bit := 16#0#;
      --  Parity status of received frame
      PARITYSTATUS  : RX_PARITYSTATUS_Field := NRF_SVD.NFCT.Parityok;
      --  Overrun detected
      OVERRUN       : RX_OVERRUN_Field := NRF_SVD.NFCT.Nooverrun;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RX_FRAMESTATUS_Register use record
      CRCERROR      at 0 range 0 .. 0;
      Reserved_1_1  at 0 range 1 .. 1;
      PARITYSTATUS  at 0 range 2 .. 2;
      OVERRUN       at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Unspecified
   type FRAMESTATUS_Cluster is record
      --  Result of last incoming frames
      RX : aliased RX_FRAMESTATUS_Register;
   end record
     with Size => 32;

   for FRAMESTATUS_Cluster use record
      RX at 0 range 0 .. 31;
   end record;

   subtype CURRENTLOADCTRL_CURRENTLOADCTRL_Field is HAL.UInt6;

   --  Current value driven to the NFC Load Control
   type CURRENTLOADCTRL_Register is record
      --  Read-only. Current value driven to the NFC Load Control
      CURRENTLOADCTRL : CURRENTLOADCTRL_CURRENTLOADCTRL_Field;
      --  unspecified
      Reserved_6_31   : HAL.UInt26;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CURRENTLOADCTRL_Register use record
      CURRENTLOADCTRL at 0 range 0 .. 5;
      Reserved_6_31   at 0 range 6 .. 31;
   end record;

   --  Indicates the presence or not of a valid field. Available only in the
   --  activated state.
   type FIELDPRESENT_FIELDPRESENT_Field is
     (--  No valid field detected
      Nofield,
      --  Valid field detected
      Fieldpresent)
     with Size => 1;
   for FIELDPRESENT_FIELDPRESENT_Field use
     (Nofield => 0,
      Fieldpresent => 1);

   --  Indicates if the low level has locked to the field
   type FIELDPRESENT_LOCKDETECT_Field is
     (--  Not locked to field
      Notlocked,
      --  Locked to field
      Locked)
     with Size => 1;
   for FIELDPRESENT_LOCKDETECT_Field use
     (Notlocked => 0,
      Locked => 1);

   --  Indicates the presence or not of a valid field
   type FIELDPRESENT_Register is record
      --  Read-only. Indicates the presence or not of a valid field. Available
      --  only in the activated state.
      FIELDPRESENT  : FIELDPRESENT_FIELDPRESENT_Field;
      --  Read-only. Indicates if the low level has locked to the field
      LOCKDETECT    : FIELDPRESENT_LOCKDETECT_Field;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIELDPRESENT_Register use record
      FIELDPRESENT  at 0 range 0 .. 0;
      LOCKDETECT    at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype FRAMEDELAYMIN_FRAMEDELAYMIN_Field is HAL.UInt16;

   --  Minimum frame delay
   type FRAMEDELAYMIN_Register is record
      --  Minimum frame delay in number of 13.56 MHz clocks
      FRAMEDELAYMIN  : FRAMEDELAYMIN_FRAMEDELAYMIN_Field := 16#480#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FRAMEDELAYMIN_Register use record
      FRAMEDELAYMIN  at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FRAMEDELAYMAX_FRAMEDELAYMAX_Field is HAL.UInt16;

   --  Maximum frame delay
   type FRAMEDELAYMAX_Register is record
      --  Maximum frame delay in number of 13.56 MHz clocks
      FRAMEDELAYMAX  : FRAMEDELAYMAX_FRAMEDELAYMAX_Field := 16#1000#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FRAMEDELAYMAX_Register use record
      FRAMEDELAYMAX  at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Configuration register for the Frame Delay Timer
   type FRAMEDELAYMODE_FRAMEDELAYMODE_Field is
     (--  Transmission is independent of frame timer and will start when the STARTTX
--  task is triggered. No timeout.
      Freerun,
      --  Frame is transmitted between FRAMEDELAYMIN and FRAMEDELAYMAX
      Window,
      --  Frame is transmitted exactly at FRAMEDELAYMAX
      Exactval,
      --  Frame is transmitted on a bit grid between FRAMEDELAYMIN and FRAMEDELAYMAX
      Windowgrid)
     with Size => 2;
   for FRAMEDELAYMODE_FRAMEDELAYMODE_Field use
     (Freerun => 0,
      Window => 1,
      Exactval => 2,
      Windowgrid => 3);

   --  Configuration register for the Frame Delay Timer
   type FRAMEDELAYMODE_Register is record
      --  Configuration register for the Frame Delay Timer
      FRAMEDELAYMODE : FRAMEDELAYMODE_FRAMEDELAYMODE_Field :=
                        NRF_SVD.NFCT.Window;
      --  unspecified
      Reserved_2_31  : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FRAMEDELAYMODE_Register use record
      FRAMEDELAYMODE at 0 range 0 .. 1;
      Reserved_2_31  at 0 range 2 .. 31;
   end record;

   subtype MAXLEN_MAXLEN_Field is HAL.UInt9;

   --  Size of allocated for TXD and RXD data storage buffer in Data RAM
   type MAXLEN_Register is record
      --  Size of allocated for TXD and RXD data storage buffer in Data RAM
      MAXLEN        : MAXLEN_MAXLEN_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAXLEN_Register use record
      MAXLEN        at 0 range 0 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   -----------------------------
   -- TXD cluster's Registers --
   -----------------------------

   --  Adding parity or not in the frame
   type FRAMECONFIG_PARITY_Field is
     (--  Parity is not added in TX frames
      Noparity,
      --  Parity is added TX frames
      Parity)
     with Size => 1;
   for FRAMECONFIG_PARITY_Field use
     (Noparity => 0,
      Parity => 1);

   --  Discarding unused bits in start or at end of a Frame
   type FRAMECONFIG_DISCARDMODE_Field is
     (--  Unused bits is discarded at end of frame
      Discardend,
      --  Unused bits is discarded at start of frame
      Discardstart)
     with Size => 1;
   for FRAMECONFIG_DISCARDMODE_Field use
     (Discardend => 0,
      Discardstart => 1);

   --  Adding SoF or not in TX frames
   type FRAMECONFIG_SOF_Field is
     (--  Start of Frame symbol not added
      Nosof,
      --  Start of Frame symbol added
      Sof)
     with Size => 1;
   for FRAMECONFIG_SOF_Field use
     (Nosof => 0,
      Sof => 1);

   --  CRC mode for outgoing frames
   type FRAMECONFIG_CRCMODETX_Field is
     (--  CRC is not added to the frame
      Nocrctx,
      --  16 bit CRC added to the frame based on all the data read from RAM that is
--  used in the frame
      Crc16Tx)
     with Size => 1;
   for FRAMECONFIG_CRCMODETX_Field use
     (Nocrctx => 0,
      Crc16Tx => 1);

   --  Configuration of outgoing frames
   type FRAMECONFIG_TXD_Register is record
      --  Adding parity or not in the frame
      PARITY        : FRAMECONFIG_PARITY_Field := NRF_SVD.NFCT.Parity;
      --  Discarding unused bits in start or at end of a Frame
      DISCARDMODE   : FRAMECONFIG_DISCARDMODE_Field :=
                       NRF_SVD.NFCT.Discardstart;
      --  Adding SoF or not in TX frames
      SOF           : FRAMECONFIG_SOF_Field := NRF_SVD.NFCT.Sof;
      --  unspecified
      Reserved_3_3  : HAL.Bit := 16#0#;
      --  CRC mode for outgoing frames
      CRCMODETX     : FRAMECONFIG_CRCMODETX_Field := NRF_SVD.NFCT.Crc16Tx;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FRAMECONFIG_TXD_Register use record
      PARITY        at 0 range 0 .. 0;
      DISCARDMODE   at 0 range 1 .. 1;
      SOF           at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      CRCMODETX     at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype AMOUNT_TXD_TXDATABITS_Field is HAL.UInt3;
   subtype AMOUNT_TXD_TXDATABYTES_Field is HAL.UInt9;

   --  Size of outgoing frame
   type AMOUNT_TXD_Register is record
      --  Number of bits in the last or first byte read from RAM that shall be
      --  included in the frame (excluding parity bit).
      TXDATABITS     : AMOUNT_TXD_TXDATABITS_Field := 16#0#;
      --  Number of complete bytes that shall be included in the frame,
      --  excluding CRC, parity and framing
      TXDATABYTES    : AMOUNT_TXD_TXDATABYTES_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AMOUNT_TXD_Register use record
      TXDATABITS     at 0 range 0 .. 2;
      TXDATABYTES    at 0 range 3 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Unspecified
   type TXD_Cluster is record
      --  Configuration of outgoing frames
      FRAMECONFIG : aliased FRAMECONFIG_TXD_Register;
      --  Size of outgoing frame
      AMOUNT      : aliased AMOUNT_TXD_Register;
   end record
     with Size => 64;

   for TXD_Cluster use record
      FRAMECONFIG at 16#0# range 0 .. 31;
      AMOUNT      at 16#4# range 0 .. 31;
   end record;

   -----------------------------
   -- RXD cluster's Registers --
   -----------------------------

   --  CRC mode for incoming frames
   type FRAMECONFIG_CRCMODERX_Field is
     (--  CRC is not expected in RX frames
      Nocrcrx,
      --  Last 16 bits in RX frame is CRC, CRC is checked and CRCSTATUS updated
      Crc16Rx)
     with Size => 1;
   for FRAMECONFIG_CRCMODERX_Field use
     (Nocrcrx => 0,
      Crc16Rx => 1);

   --  Configuration of incoming frames
   type FRAMECONFIG_RXD_Register is record
      --  Parity expected or not in RX frame
      PARITY        : FRAMECONFIG_PARITY_Field := NRF_SVD.NFCT.Parity;
      --  unspecified
      Reserved_1_1  : HAL.Bit := 16#0#;
      --  SoF expected or not in RX frames
      SOF           : FRAMECONFIG_SOF_Field := NRF_SVD.NFCT.Sof;
      --  unspecified
      Reserved_3_3  : HAL.Bit := 16#0#;
      --  CRC mode for incoming frames
      CRCMODERX     : FRAMECONFIG_CRCMODERX_Field := NRF_SVD.NFCT.Crc16Rx;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FRAMECONFIG_RXD_Register use record
      PARITY        at 0 range 0 .. 0;
      Reserved_1_1  at 0 range 1 .. 1;
      SOF           at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      CRCMODERX     at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype AMOUNT_RXD_RXDATABITS_Field is HAL.UInt3;
   subtype AMOUNT_RXD_RXDATABYTES_Field is HAL.UInt9;

   --  Size of last incoming frame
   type AMOUNT_RXD_Register is record
      --  Read-only. Number of bits in the last byte in the frame, if less than
      --  8 (including CRC, but excluding parity and SoF/EoF framing).
      RXDATABITS     : AMOUNT_RXD_RXDATABITS_Field;
      --  Read-only. Number of complete bytes received in the frame (including
      --  CRC, but excluding parity and SoF/EoF framing)
      RXDATABYTES    : AMOUNT_RXD_RXDATABYTES_Field;
      --  unspecified
      Reserved_12_31 : HAL.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AMOUNT_RXD_Register use record
      RXDATABITS     at 0 range 0 .. 2;
      RXDATABYTES    at 0 range 3 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Unspecified
   type RXD_Cluster is record
      --  Configuration of incoming frames
      FRAMECONFIG : aliased FRAMECONFIG_RXD_Register;
      --  Size of last incoming frame
      AMOUNT      : aliased AMOUNT_RXD_Register;
   end record
     with Size => 64;

   for RXD_Cluster use record
      FRAMECONFIG at 16#0# range 0 .. 31;
      AMOUNT      at 16#4# range 0 .. 31;
   end record;

   subtype NFCID1_LAST_NFCID1_Z_Field is HAL.UInt8;
   subtype NFCID1_LAST_NFCID1_Y_Field is HAL.UInt8;
   subtype NFCID1_LAST_NFCID1_X_Field is HAL.UInt8;
   subtype NFCID1_LAST_NFCID1_W_Field is HAL.UInt8;

   --  Last NFCID1 part (4, 7 or 10 bytes ID)
   type NFCID1_LAST_Register is record
      --  NFCID1 byte Z (very last byte sent)
      NFCID1_Z : NFCID1_LAST_NFCID1_Z_Field := 16#63#;
      --  NFCID1 byte Y
      NFCID1_Y : NFCID1_LAST_NFCID1_Y_Field := 16#63#;
      --  NFCID1 byte X
      NFCID1_X : NFCID1_LAST_NFCID1_X_Field := 16#0#;
      --  NFCID1 byte W
      NFCID1_W : NFCID1_LAST_NFCID1_W_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NFCID1_LAST_Register use record
      NFCID1_Z at 0 range 0 .. 7;
      NFCID1_Y at 0 range 8 .. 15;
      NFCID1_X at 0 range 16 .. 23;
      NFCID1_W at 0 range 24 .. 31;
   end record;

   subtype NFCID1_2ND_LAST_NFCID1_V_Field is HAL.UInt8;
   subtype NFCID1_2ND_LAST_NFCID1_U_Field is HAL.UInt8;
   subtype NFCID1_2ND_LAST_NFCID1_T_Field is HAL.UInt8;

   --  Second last NFCID1 part (7 or 10 bytes ID)
   type NFCID1_2ND_LAST_Register is record
      --  NFCID1 byte V
      NFCID1_V       : NFCID1_2ND_LAST_NFCID1_V_Field := 16#0#;
      --  NFCID1 byte U
      NFCID1_U       : NFCID1_2ND_LAST_NFCID1_U_Field := 16#0#;
      --  NFCID1 byte T
      NFCID1_T       : NFCID1_2ND_LAST_NFCID1_T_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NFCID1_2ND_LAST_Register use record
      NFCID1_V       at 0 range 0 .. 7;
      NFCID1_U       at 0 range 8 .. 15;
      NFCID1_T       at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype NFCID1_3RD_LAST_NFCID1_S_Field is HAL.UInt8;
   subtype NFCID1_3RD_LAST_NFCID1_R_Field is HAL.UInt8;
   subtype NFCID1_3RD_LAST_NFCID1_Q_Field is HAL.UInt8;

   --  Third last NFCID1 part (10 bytes ID)
   type NFCID1_3RD_LAST_Register is record
      --  NFCID1 byte S
      NFCID1_S       : NFCID1_3RD_LAST_NFCID1_S_Field := 16#0#;
      --  NFCID1 byte R
      NFCID1_R       : NFCID1_3RD_LAST_NFCID1_R_Field := 16#0#;
      --  NFCID1 byte Q
      NFCID1_Q       : NFCID1_3RD_LAST_NFCID1_Q_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NFCID1_3RD_LAST_Register use record
      NFCID1_S       at 0 range 0 .. 7;
      NFCID1_R       at 0 range 8 .. 15;
      NFCID1_Q       at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Bit frame SDD as defined by the b5:b1 of byte 1 in SENS_RES response in
   --  the NFC Forum, NFC Digital Protocol Technical Specification
   type SENSRES_BITFRAMESDD_Field is
     (--  SDD pattern 00000
      Sdd00000,
      --  SDD pattern 00001
      Sdd00001,
      --  SDD pattern 00010
      Sdd00010,
      --  SDD pattern 00100
      Sdd00100,
      --  SDD pattern 01000
      Sdd01000,
      --  SDD pattern 10000
      Sdd10000)
     with Size => 5;
   for SENSRES_BITFRAMESDD_Field use
     (Sdd00000 => 0,
      Sdd00001 => 1,
      Sdd00010 => 2,
      Sdd00100 => 4,
      Sdd01000 => 8,
      Sdd10000 => 16);

   --  NFCID1 size. This value is used by the Auto collision resolution engine.
   type SENSRES_NFCIDSIZE_Field is
     (--  NFCID1 size: single (4 bytes)
      Nfcid1Single,
      --  NFCID1 size: double (7 bytes)
      Nfcid1Double,
      --  NFCID1 size: triple (10 bytes)
      Nfcid1Triple)
     with Size => 2;
   for SENSRES_NFCIDSIZE_Field use
     (Nfcid1Single => 0,
      Nfcid1Double => 1,
      Nfcid1Triple => 2);

   subtype SENSRES_PLATFCONFIG_Field is HAL.UInt4;
   subtype SENSRES_RFU74_Field is HAL.UInt4;

   --  NFC-A SENS_RES auto-response settings
   type SENSRES_Register is record
      --  Bit frame SDD as defined by the b5:b1 of byte 1 in SENS_RES response
      --  in the NFC Forum, NFC Digital Protocol Technical Specification
      BITFRAMESDD    : SENSRES_BITFRAMESDD_Field := NRF_SVD.NFCT.Sdd00001;
      --  Reserved for future use. Shall be 0.
      RFU5           : Boolean := False;
      --  NFCID1 size. This value is used by the Auto collision resolution
      --  engine.
      NFCIDSIZE      : SENSRES_NFCIDSIZE_Field := NRF_SVD.NFCT.Nfcid1Single;
      --  Tag platform configuration as defined by the b4:b1 of byte 2 in
      --  SENS_RES response in the NFC Forum, NFC Digital Protocol Technical
      --  Specification
      PLATFCONFIG    : SENSRES_PLATFCONFIG_Field := 16#0#;
      --  Reserved for future use. Shall be 0.
      RFU74          : SENSRES_RFU74_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SENSRES_Register use record
      BITFRAMESDD    at 0 range 0 .. 4;
      RFU5           at 0 range 5 .. 5;
      NFCIDSIZE      at 0 range 6 .. 7;
      PLATFCONFIG    at 0 range 8 .. 11;
      RFU74          at 0 range 12 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype SELRES_RFU10_Field is HAL.UInt2;

   --  Cascade bit (controlled by hardware, write has no effect)
   type SELRES_CASCADE_Field is
     (--  NFCID1 complete
      Complete,
      --  NFCID1 not complete
      Notcomplete)
     with Size => 1;
   for SELRES_CASCADE_Field use
     (Complete => 0,
      Notcomplete => 1);

   subtype SELRES_RFU43_Field is HAL.UInt2;
   subtype SELRES_PROTOCOL_Field is HAL.UInt2;

   --  NFC-A SEL_RES auto-response settings
   type SELRES_Register is record
      --  Reserved for future use. Shall be 0.
      RFU10         : SELRES_RFU10_Field := 16#0#;
      --  Cascade bit (controlled by hardware, write has no effect)
      CASCADE       : SELRES_CASCADE_Field := NRF_SVD.NFCT.Complete;
      --  Reserved for future use. Shall be 0.
      RFU43         : SELRES_RFU43_Field := 16#0#;
      --  Protocol as defined by the b7:b6 of SEL_RES response in the NFC
      --  Forum, NFC Digital Protocol Technical Specification
      PROTOCOL      : SELRES_PROTOCOL_Field := 16#0#;
      --  Reserved for future use. Shall be 0.
      RFU7          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SELRES_Register use record
      RFU10         at 0 range 0 .. 1;
      CASCADE       at 0 range 2 .. 2;
      RFU43         at 0 range 3 .. 4;
      PROTOCOL      at 0 range 5 .. 6;
      RFU7          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  NFC-A compatible radio
   type NFCT_Peripheral is record
      --  Activate NFC peripheral for incoming and outgoing frames, change
      --  state to activated
      TASKS_ACTIVATE           : aliased HAL.UInt32;
      --  Disable NFC peripheral
      TASKS_DISABLE            : aliased HAL.UInt32;
      --  Enable NFC sense field mode, change state to sense mode
      TASKS_SENSE              : aliased HAL.UInt32;
      --  Start transmission of a outgoing frame, change state to transmit
      TASKS_STARTTX            : aliased HAL.UInt32;
      --  Initializes the EasyDMA for receive.
      TASKS_ENABLERXDATA       : aliased HAL.UInt32;
      --  Force state machine to IDLE state
      TASKS_GOIDLE             : aliased HAL.UInt32;
      --  Force state machine to SLEEP_A state
      TASKS_GOSLEEP            : aliased HAL.UInt32;
      --  The NFC peripheral is ready to receive and send frames
      EVENTS_READY             : aliased HAL.UInt32;
      --  Remote NFC field detected
      EVENTS_FIELDDETECTED     : aliased HAL.UInt32;
      --  Remote NFC field lost
      EVENTS_FIELDLOST         : aliased HAL.UInt32;
      --  Marks the start of the first symbol of a transmitted frame
      EVENTS_TXFRAMESTART      : aliased HAL.UInt32;
      --  Marks the end of the last transmitted on-air symbol of a frame
      EVENTS_TXFRAMEEND        : aliased HAL.UInt32;
      --  Marks the end of the first symbol of a received frame
      EVENTS_RXFRAMESTART      : aliased HAL.UInt32;
      --  Received data have been checked (CRC, parity) and transferred to RAM,
      --  and EasyDMA has ended accessing the RX buffer
      EVENTS_RXFRAMEEND        : aliased HAL.UInt32;
      --  NFC error reported. The ERRORSTATUS register contains details on the
      --  source of the error.
      EVENTS_ERROR             : aliased HAL.UInt32;
      --  NFC RX frame error reported. The FRAMESTATUS.RX register contains
      --  details on the source of the error.
      EVENTS_RXERROR           : aliased HAL.UInt32;
      --  RX buffer (as defined by PACKETPTR and MAXLEN) in Data RAM full.
      EVENTS_ENDRX             : aliased HAL.UInt32;
      --  Transmission of data in RAM has ended, and EasyDMA has ended
      --  accessing the TX buffer
      EVENTS_ENDTX             : aliased HAL.UInt32;
      --  Auto collision resolution process has started
      EVENTS_AUTOCOLRESSTARTED : aliased HAL.UInt32;
      --  NFC Auto collision resolution error reported.
      EVENTS_COLLISION         : aliased HAL.UInt32;
      --  NFC Auto collision resolution successfully completed
      EVENTS_SELECTED          : aliased HAL.UInt32;
      --  EasyDMA is ready to receive or send frames.
      EVENTS_STARTED           : aliased HAL.UInt32;
      --  Shortcut register
      SHORTS                   : aliased SHORTS_Register;
      --  Enable or disable interrupt
      INTEN                    : aliased INTEN_Register;
      --  Enable interrupt
      INTENSET                 : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR                 : aliased INTENCLR_Register;
      --  NFC Error Status register
      ERRORSTATUS              : aliased ERRORSTATUS_Register;
      --  Unspecified
      FRAMESTATUS              : aliased FRAMESTATUS_Cluster;
      --  Current value driven to the NFC Load Control
      CURRENTLOADCTRL          : aliased CURRENTLOADCTRL_Register;
      --  Indicates the presence or not of a valid field
      FIELDPRESENT             : aliased FIELDPRESENT_Register;
      --  Minimum frame delay
      FRAMEDELAYMIN            : aliased FRAMEDELAYMIN_Register;
      --  Maximum frame delay
      FRAMEDELAYMAX            : aliased FRAMEDELAYMAX_Register;
      --  Configuration register for the Frame Delay Timer
      FRAMEDELAYMODE           : aliased FRAMEDELAYMODE_Register;
      --  Packet pointer for TXD and RXD data storage in Data RAM
      PACKETPTR                : aliased HAL.UInt32;
      --  Size of allocated for TXD and RXD data storage buffer in Data RAM
      MAXLEN                   : aliased MAXLEN_Register;
      --  Unspecified
      TXD                      : aliased TXD_Cluster;
      --  Unspecified
      RXD                      : aliased RXD_Cluster;
      --  Last NFCID1 part (4, 7 or 10 bytes ID)
      NFCID1_LAST              : aliased NFCID1_LAST_Register;
      --  Second last NFCID1 part (7 or 10 bytes ID)
      NFCID1_2ND_LAST          : aliased NFCID1_2ND_LAST_Register;
      --  Third last NFCID1 part (10 bytes ID)
      NFCID1_3RD_LAST          : aliased NFCID1_3RD_LAST_Register;
      --  NFC-A SENS_RES auto-response settings
      SENSRES                  : aliased SENSRES_Register;
      --  NFC-A SEL_RES auto-response settings
      SELRES                   : aliased SELRES_Register;
   end record
     with Volatile;

   for NFCT_Peripheral use record
      TASKS_ACTIVATE           at 16#0# range 0 .. 31;
      TASKS_DISABLE            at 16#4# range 0 .. 31;
      TASKS_SENSE              at 16#8# range 0 .. 31;
      TASKS_STARTTX            at 16#C# range 0 .. 31;
      TASKS_ENABLERXDATA       at 16#1C# range 0 .. 31;
      TASKS_GOIDLE             at 16#24# range 0 .. 31;
      TASKS_GOSLEEP            at 16#28# range 0 .. 31;
      EVENTS_READY             at 16#100# range 0 .. 31;
      EVENTS_FIELDDETECTED     at 16#104# range 0 .. 31;
      EVENTS_FIELDLOST         at 16#108# range 0 .. 31;
      EVENTS_TXFRAMESTART      at 16#10C# range 0 .. 31;
      EVENTS_TXFRAMEEND        at 16#110# range 0 .. 31;
      EVENTS_RXFRAMESTART      at 16#114# range 0 .. 31;
      EVENTS_RXFRAMEEND        at 16#118# range 0 .. 31;
      EVENTS_ERROR             at 16#11C# range 0 .. 31;
      EVENTS_RXERROR           at 16#128# range 0 .. 31;
      EVENTS_ENDRX             at 16#12C# range 0 .. 31;
      EVENTS_ENDTX             at 16#130# range 0 .. 31;
      EVENTS_AUTOCOLRESSTARTED at 16#138# range 0 .. 31;
      EVENTS_COLLISION         at 16#148# range 0 .. 31;
      EVENTS_SELECTED          at 16#14C# range 0 .. 31;
      EVENTS_STARTED           at 16#150# range 0 .. 31;
      SHORTS                   at 16#200# range 0 .. 31;
      INTEN                    at 16#300# range 0 .. 31;
      INTENSET                 at 16#304# range 0 .. 31;
      INTENCLR                 at 16#308# range 0 .. 31;
      ERRORSTATUS              at 16#404# range 0 .. 31;
      FRAMESTATUS              at 16#40C# range 0 .. 31;
      CURRENTLOADCTRL          at 16#430# range 0 .. 31;
      FIELDPRESENT             at 16#43C# range 0 .. 31;
      FRAMEDELAYMIN            at 16#504# range 0 .. 31;
      FRAMEDELAYMAX            at 16#508# range 0 .. 31;
      FRAMEDELAYMODE           at 16#50C# range 0 .. 31;
      PACKETPTR                at 16#510# range 0 .. 31;
      MAXLEN                   at 16#514# range 0 .. 31;
      TXD                      at 16#518# range 0 .. 63;
      RXD                      at 16#520# range 0 .. 63;
      NFCID1_LAST              at 16#590# range 0 .. 31;
      NFCID1_2ND_LAST          at 16#594# range 0 .. 31;
      NFCID1_3RD_LAST          at 16#598# range 0 .. 31;
      SENSRES                  at 16#5A0# range 0 .. 31;
      SELRES                   at 16#5A4# range 0 .. 31;
   end record;

   --  NFC-A compatible radio
   NFCT_Periph : aliased NFCT_Peripheral
     with Import, Address => NFCT_Base;

end NRF_SVD.NFCT;
