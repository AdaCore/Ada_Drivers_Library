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

package NRF_SVD.RADIO is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Shortcut between READY event and START task
   type SHORTS_READY_START_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_READY_START_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between END event and DISABLE task
   type SHORTS_END_DISABLE_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_END_DISABLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between DISABLED event and TXEN task
   type SHORTS_DISABLED_TXEN_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_DISABLED_TXEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between DISABLED event and RXEN task
   type SHORTS_DISABLED_RXEN_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_DISABLED_RXEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between ADDRESS event and RSSISTART task
   type SHORTS_ADDRESS_RSSISTART_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_ADDRESS_RSSISTART_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between END event and START task
   type SHORTS_END_START_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_END_START_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between ADDRESS event and BCSTART task
   type SHORTS_ADDRESS_BCSTART_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_ADDRESS_BCSTART_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between DISABLED event and RSSISTOP task
   type SHORTS_DISABLED_RSSISTOP_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_DISABLED_RSSISTOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut register
   type SHORTS_Register is record
      --  Shortcut between READY event and START task
      READY_START       : SHORTS_READY_START_Field := NRF_SVD.RADIO.Disabled;
      --  Shortcut between END event and DISABLE task
      END_DISABLE       : SHORTS_END_DISABLE_Field := NRF_SVD.RADIO.Disabled;
      --  Shortcut between DISABLED event and TXEN task
      DISABLED_TXEN     : SHORTS_DISABLED_TXEN_Field :=
                           NRF_SVD.RADIO.Disabled;
      --  Shortcut between DISABLED event and RXEN task
      DISABLED_RXEN     : SHORTS_DISABLED_RXEN_Field :=
                           NRF_SVD.RADIO.Disabled;
      --  Shortcut between ADDRESS event and RSSISTART task
      ADDRESS_RSSISTART : SHORTS_ADDRESS_RSSISTART_Field :=
                           NRF_SVD.RADIO.Disabled;
      --  Shortcut between END event and START task
      END_START         : SHORTS_END_START_Field := NRF_SVD.RADIO.Disabled;
      --  Shortcut between ADDRESS event and BCSTART task
      ADDRESS_BCSTART   : SHORTS_ADDRESS_BCSTART_Field :=
                           NRF_SVD.RADIO.Disabled;
      --  unspecified
      Reserved_7_7      : HAL.Bit := 16#0#;
      --  Shortcut between DISABLED event and RSSISTOP task
      DISABLED_RSSISTOP : SHORTS_DISABLED_RSSISTOP_Field :=
                           NRF_SVD.RADIO.Disabled;
      --  unspecified
      Reserved_9_31     : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      READY_START       at 0 range 0 .. 0;
      END_DISABLE       at 0 range 1 .. 1;
      DISABLED_TXEN     at 0 range 2 .. 2;
      DISABLED_RXEN     at 0 range 3 .. 3;
      ADDRESS_RSSISTART at 0 range 4 .. 4;
      END_START         at 0 range 5 .. 5;
      ADDRESS_BCSTART   at 0 range 6 .. 6;
      Reserved_7_7      at 0 range 7 .. 7;
      DISABLED_RSSISTOP at 0 range 8 .. 8;
      Reserved_9_31     at 0 range 9 .. 31;
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

   --  Write '1' to Enable interrupt for ADDRESS event
   type INTENSET_ADDRESS_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_ADDRESS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for ADDRESS event
   type INTENSET_ADDRESS_Field_1 is
     (--  Reset value for the field
      Intenset_Address_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_ADDRESS_Field_1 use
     (Intenset_Address_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for PAYLOAD event
   type INTENSET_PAYLOAD_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_PAYLOAD_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for PAYLOAD event
   type INTENSET_PAYLOAD_Field_1 is
     (--  Reset value for the field
      Intenset_Payload_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_PAYLOAD_Field_1 use
     (Intenset_Payload_Field_Reset => 0,
      Set => 1);

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

   --  Write '1' to Enable interrupt for DISABLED event
   type INTENSET_DISABLED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_DISABLED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for DISABLED event
   type INTENSET_DISABLED_Field_1 is
     (--  Reset value for the field
      Intenset_Disabled_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_DISABLED_Field_1 use
     (Intenset_Disabled_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for DEVMATCH event
   type INTENSET_DEVMATCH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_DEVMATCH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for DEVMATCH event
   type INTENSET_DEVMATCH_Field_1 is
     (--  Reset value for the field
      Intenset_Devmatch_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_DEVMATCH_Field_1 use
     (Intenset_Devmatch_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for DEVMISS event
   type INTENSET_DEVMISS_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_DEVMISS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for DEVMISS event
   type INTENSET_DEVMISS_Field_1 is
     (--  Reset value for the field
      Intenset_Devmiss_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_DEVMISS_Field_1 use
     (Intenset_Devmiss_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for RSSIEND event
   type INTENSET_RSSIEND_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_RSSIEND_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for RSSIEND event
   type INTENSET_RSSIEND_Field_1 is
     (--  Reset value for the field
      Intenset_Rssiend_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_RSSIEND_Field_1 use
     (Intenset_Rssiend_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for BCMATCH event
   type INTENSET_BCMATCH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_BCMATCH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for BCMATCH event
   type INTENSET_BCMATCH_Field_1 is
     (--  Reset value for the field
      Intenset_Bcmatch_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_BCMATCH_Field_1 use
     (Intenset_Bcmatch_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CRCOK event
   type INTENSET_CRCOK_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CRCOK_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CRCOK event
   type INTENSET_CRCOK_Field_1 is
     (--  Reset value for the field
      Intenset_Crcok_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CRCOK_Field_1 use
     (Intenset_Crcok_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CRCERROR event
   type INTENSET_CRCERROR_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CRCERROR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CRCERROR event
   type INTENSET_CRCERROR_Field_1 is
     (--  Reset value for the field
      Intenset_Crcerror_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CRCERROR_Field_1 use
     (Intenset_Crcerror_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  Write '1' to Enable interrupt for READY event
      READY          : INTENSET_READY_Field_1 := Intenset_Ready_Field_Reset;
      --  Write '1' to Enable interrupt for ADDRESS event
      ADDRESS        : INTENSET_ADDRESS_Field_1 :=
                        Intenset_Address_Field_Reset;
      --  Write '1' to Enable interrupt for PAYLOAD event
      PAYLOAD        : INTENSET_PAYLOAD_Field_1 :=
                        Intenset_Payload_Field_Reset;
      --  Write '1' to Enable interrupt for END event
      END_k          : INTENSET_END_Field_1 := Intenset_End_Field_Reset;
      --  Write '1' to Enable interrupt for DISABLED event
      DISABLED       : INTENSET_DISABLED_Field_1 :=
                        Intenset_Disabled_Field_Reset;
      --  Write '1' to Enable interrupt for DEVMATCH event
      DEVMATCH       : INTENSET_DEVMATCH_Field_1 :=
                        Intenset_Devmatch_Field_Reset;
      --  Write '1' to Enable interrupt for DEVMISS event
      DEVMISS        : INTENSET_DEVMISS_Field_1 :=
                        Intenset_Devmiss_Field_Reset;
      --  Write '1' to Enable interrupt for RSSIEND event
      RSSIEND        : INTENSET_RSSIEND_Field_1 :=
                        Intenset_Rssiend_Field_Reset;
      --  unspecified
      Reserved_8_9   : HAL.UInt2 := 16#0#;
      --  Write '1' to Enable interrupt for BCMATCH event
      BCMATCH        : INTENSET_BCMATCH_Field_1 :=
                        Intenset_Bcmatch_Field_Reset;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  Write '1' to Enable interrupt for CRCOK event
      CRCOK          : INTENSET_CRCOK_Field_1 := Intenset_Crcok_Field_Reset;
      --  Write '1' to Enable interrupt for CRCERROR event
      CRCERROR       : INTENSET_CRCERROR_Field_1 :=
                        Intenset_Crcerror_Field_Reset;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      READY          at 0 range 0 .. 0;
      ADDRESS        at 0 range 1 .. 1;
      PAYLOAD        at 0 range 2 .. 2;
      END_k          at 0 range 3 .. 3;
      DISABLED       at 0 range 4 .. 4;
      DEVMATCH       at 0 range 5 .. 5;
      DEVMISS        at 0 range 6 .. 6;
      RSSIEND        at 0 range 7 .. 7;
      Reserved_8_9   at 0 range 8 .. 9;
      BCMATCH        at 0 range 10 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      CRCOK          at 0 range 12 .. 12;
      CRCERROR       at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
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

   --  Write '1' to Disable interrupt for ADDRESS event
   type INTENCLR_ADDRESS_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_ADDRESS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for ADDRESS event
   type INTENCLR_ADDRESS_Field_1 is
     (--  Reset value for the field
      Intenclr_Address_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_ADDRESS_Field_1 use
     (Intenclr_Address_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for PAYLOAD event
   type INTENCLR_PAYLOAD_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_PAYLOAD_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for PAYLOAD event
   type INTENCLR_PAYLOAD_Field_1 is
     (--  Reset value for the field
      Intenclr_Payload_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_PAYLOAD_Field_1 use
     (Intenclr_Payload_Field_Reset => 0,
      Clear => 1);

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

   --  Write '1' to Disable interrupt for DISABLED event
   type INTENCLR_DISABLED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_DISABLED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for DISABLED event
   type INTENCLR_DISABLED_Field_1 is
     (--  Reset value for the field
      Intenclr_Disabled_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_DISABLED_Field_1 use
     (Intenclr_Disabled_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for DEVMATCH event
   type INTENCLR_DEVMATCH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_DEVMATCH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for DEVMATCH event
   type INTENCLR_DEVMATCH_Field_1 is
     (--  Reset value for the field
      Intenclr_Devmatch_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_DEVMATCH_Field_1 use
     (Intenclr_Devmatch_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for DEVMISS event
   type INTENCLR_DEVMISS_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_DEVMISS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for DEVMISS event
   type INTENCLR_DEVMISS_Field_1 is
     (--  Reset value for the field
      Intenclr_Devmiss_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_DEVMISS_Field_1 use
     (Intenclr_Devmiss_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for RSSIEND event
   type INTENCLR_RSSIEND_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_RSSIEND_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for RSSIEND event
   type INTENCLR_RSSIEND_Field_1 is
     (--  Reset value for the field
      Intenclr_Rssiend_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_RSSIEND_Field_1 use
     (Intenclr_Rssiend_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for BCMATCH event
   type INTENCLR_BCMATCH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_BCMATCH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for BCMATCH event
   type INTENCLR_BCMATCH_Field_1 is
     (--  Reset value for the field
      Intenclr_Bcmatch_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_BCMATCH_Field_1 use
     (Intenclr_Bcmatch_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CRCOK event
   type INTENCLR_CRCOK_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CRCOK_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CRCOK event
   type INTENCLR_CRCOK_Field_1 is
     (--  Reset value for the field
      Intenclr_Crcok_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CRCOK_Field_1 use
     (Intenclr_Crcok_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CRCERROR event
   type INTENCLR_CRCERROR_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CRCERROR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CRCERROR event
   type INTENCLR_CRCERROR_Field_1 is
     (--  Reset value for the field
      Intenclr_Crcerror_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CRCERROR_Field_1 use
     (Intenclr_Crcerror_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  Write '1' to Disable interrupt for READY event
      READY          : INTENCLR_READY_Field_1 := Intenclr_Ready_Field_Reset;
      --  Write '1' to Disable interrupt for ADDRESS event
      ADDRESS        : INTENCLR_ADDRESS_Field_1 :=
                        Intenclr_Address_Field_Reset;
      --  Write '1' to Disable interrupt for PAYLOAD event
      PAYLOAD        : INTENCLR_PAYLOAD_Field_1 :=
                        Intenclr_Payload_Field_Reset;
      --  Write '1' to Disable interrupt for END event
      END_k          : INTENCLR_END_Field_1 := Intenclr_End_Field_Reset;
      --  Write '1' to Disable interrupt for DISABLED event
      DISABLED       : INTENCLR_DISABLED_Field_1 :=
                        Intenclr_Disabled_Field_Reset;
      --  Write '1' to Disable interrupt for DEVMATCH event
      DEVMATCH       : INTENCLR_DEVMATCH_Field_1 :=
                        Intenclr_Devmatch_Field_Reset;
      --  Write '1' to Disable interrupt for DEVMISS event
      DEVMISS        : INTENCLR_DEVMISS_Field_1 :=
                        Intenclr_Devmiss_Field_Reset;
      --  Write '1' to Disable interrupt for RSSIEND event
      RSSIEND        : INTENCLR_RSSIEND_Field_1 :=
                        Intenclr_Rssiend_Field_Reset;
      --  unspecified
      Reserved_8_9   : HAL.UInt2 := 16#0#;
      --  Write '1' to Disable interrupt for BCMATCH event
      BCMATCH        : INTENCLR_BCMATCH_Field_1 :=
                        Intenclr_Bcmatch_Field_Reset;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  Write '1' to Disable interrupt for CRCOK event
      CRCOK          : INTENCLR_CRCOK_Field_1 := Intenclr_Crcok_Field_Reset;
      --  Write '1' to Disable interrupt for CRCERROR event
      CRCERROR       : INTENCLR_CRCERROR_Field_1 :=
                        Intenclr_Crcerror_Field_Reset;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      READY          at 0 range 0 .. 0;
      ADDRESS        at 0 range 1 .. 1;
      PAYLOAD        at 0 range 2 .. 2;
      END_k          at 0 range 3 .. 3;
      DISABLED       at 0 range 4 .. 4;
      DEVMATCH       at 0 range 5 .. 5;
      DEVMISS        at 0 range 6 .. 6;
      RSSIEND        at 0 range 7 .. 7;
      Reserved_8_9   at 0 range 8 .. 9;
      BCMATCH        at 0 range 10 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      CRCOK          at 0 range 12 .. 12;
      CRCERROR       at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  CRC status of packet received
   type CRCSTATUS_CRCSTATUS_Field is
     (--  Packet received with CRC error
      Crcerror,
      --  Packet received with CRC ok
      Crcok)
     with Size => 1;
   for CRCSTATUS_CRCSTATUS_Field use
     (Crcerror => 0,
      Crcok => 1);

   --  CRC status
   type CRCSTATUS_Register is record
      --  Read-only. CRC status of packet received
      CRCSTATUS     : CRCSTATUS_CRCSTATUS_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CRCSTATUS_Register use record
      CRCSTATUS     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype RXMATCH_RXMATCH_Field is HAL.UInt3;

   --  Received address
   type RXMATCH_Register is record
      --  Read-only. Received address
      RXMATCH       : RXMATCH_RXMATCH_Field;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXMATCH_Register use record
      RXMATCH       at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype RXCRC_RXCRC_Field is HAL.UInt24;

   --  CRC field of previously received packet
   type RXCRC_Register is record
      --  Read-only. CRC field of previously received packet
      RXCRC          : RXCRC_RXCRC_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXCRC_Register use record
      RXCRC          at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DAI_DAI_Field is HAL.UInt3;

   --  Device address match index
   type DAI_Register is record
      --  Read-only. Device address match index
      DAI           : DAI_DAI_Field;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAI_Register use record
      DAI           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype FREQUENCY_FREQUENCY_Field is HAL.UInt7;

   --  Channel map selection.
   type FREQUENCY_MAP_Field is
     (--  Channel map between 2400 MHZ .. 2500 MHz
      Default,
      --  Channel map between 2360 MHZ .. 2460 MHz
      Low)
     with Size => 1;
   for FREQUENCY_MAP_Field use
     (Default => 0,
      Low => 1);

   --  Frequency
   type FREQUENCY_Register is record
      --  Radio channel frequency
      FREQUENCY     : FREQUENCY_FREQUENCY_Field := 16#2#;
      --  unspecified
      Reserved_7_7  : HAL.Bit := 16#0#;
      --  Channel map selection.
      MAP           : FREQUENCY_MAP_Field := NRF_SVD.RADIO.Default;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FREQUENCY_Register use record
      FREQUENCY     at 0 range 0 .. 6;
      Reserved_7_7  at 0 range 7 .. 7;
      MAP           at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  RADIO output power.
   type TXPOWER_TXPOWER_Field is
     (--  0 dBm
      Val_0DBm,
      --  +3 dBm
      Pos3DBm,
      --  +4 dBm
      Pos4DBm,
      --  -40 dBm
      Neg40DBm,
      --  -20 dBm
      Neg20DBm,
      --  -16 dBm
      Neg16DBm,
      --  -12 dBm
      Neg12DBm,
      --  -8 dBm
      Neg8DBm,
      --  -4 dBm
      Neg4DBm,
      --  Deprecated enumerator - -40 dBm
      Neg30DBm)
     with Size => 8;
   for TXPOWER_TXPOWER_Field use
     (Val_0DBm => 0,
      Pos3DBm => 3,
      Pos4DBm => 4,
      Neg40DBm => 216,
      Neg20DBm => 236,
      Neg16DBm => 240,
      Neg12DBm => 244,
      Neg8DBm => 248,
      Neg4DBm => 252,
      Neg30DBm => 255);

   --  Output power
   type TXPOWER_Register is record
      --  RADIO output power.
      TXPOWER       : TXPOWER_TXPOWER_Field := NRF_SVD.RADIO.Val_0DBm;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXPOWER_Register use record
      TXPOWER       at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Radio data rate and modulation setting. The radio supports
   --  Frequency-shift Keying (FSK) modulation.
   type MODE_MODE_Field is
     (--  1 Mbit/s Nordic proprietary radio mode
      Nrf_1Mbit,
      --  2 Mbit/s Nordic proprietary radio mode
      Nrf_2Mbit,
      --  Deprecated enumerator - 250 kbit/s Nordic proprietary radio mode
      Nrf_250Kbit,
      --  1 Mbit/s Bluetooth Low Energy
      Ble_1Mbit,
      --  2 Mbit/s Bluetooth Low Energy
      Ble_2Mbit)
     with Size => 4;
   for MODE_MODE_Field use
     (Nrf_1Mbit => 0,
      Nrf_2Mbit => 1,
      Nrf_250Kbit => 2,
      Ble_1Mbit => 3,
      Ble_2Mbit => 4);

   --  Data rate and modulation
   type MODE_Register is record
      --  Radio data rate and modulation setting. The radio supports
      --  Frequency-shift Keying (FSK) modulation.
      MODE          : MODE_MODE_Field := NRF_SVD.RADIO.Nrf_1Mbit;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MODE_Register use record
      MODE          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype PCNF0_LFLEN_Field is HAL.UInt4;
   subtype PCNF0_S1LEN_Field is HAL.UInt4;

   --  Include or exclude S1 field in RAM
   type PCNF0_S1INCL_Field is
     (--  Include S1 field in RAM only if S1LEN &gt; 0
      Automatic,
      --  Always include S1 field in RAM independent of S1LEN
      Include)
     with Size => 1;
   for PCNF0_S1INCL_Field use
     (Automatic => 0,
      Include => 1);

   --  Length of preamble on air. Decision point: TASKS_START task
   type PCNF0_PLEN_Field is
     (--  8-bit preamble
      Val_8BIT,
      --  16-bit preamble
      Val_16BIT)
     with Size => 1;
   for PCNF0_PLEN_Field use
     (Val_8BIT => 0,
      Val_16BIT => 1);

   --  Packet configuration register 0
   type PCNF0_Register is record
      --  Length on air of LENGTH field in number of bits.
      LFLEN          : PCNF0_LFLEN_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Length on air of S0 field in number of bytes.
      S0LEN          : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  Length on air of S1 field in number of bits.
      S1LEN          : PCNF0_S1LEN_Field := 16#0#;
      --  Include or exclude S1 field in RAM
      S1INCL         : PCNF0_S1INCL_Field := NRF_SVD.RADIO.Automatic;
      --  unspecified
      Reserved_21_23 : HAL.UInt3 := 16#0#;
      --  Length of preamble on air. Decision point: TASKS_START task
      PLEN           : PCNF0_PLEN_Field := NRF_SVD.RADIO.Val_8BIT;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PCNF0_Register use record
      LFLEN          at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      S0LEN          at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      S1LEN          at 0 range 16 .. 19;
      S1INCL         at 0 range 20 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      PLEN           at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype PCNF1_MAXLEN_Field is HAL.UInt8;
   subtype PCNF1_STATLEN_Field is HAL.UInt8;
   subtype PCNF1_BALEN_Field is HAL.UInt3;

   --  On air endianness of packet, this applies to the S0, LENGTH, S1 and the
   --  PAYLOAD fields.
   type PCNF1_ENDIAN_Field is
     (--  Least Significant bit on air first
      Little,
      --  Most significant bit on air first
      Big)
     with Size => 1;
   for PCNF1_ENDIAN_Field use
     (Little => 0,
      Big => 1);

   --  Enable or disable packet whitening
   type PCNF1_WHITEEN_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for PCNF1_WHITEEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Packet configuration register 1
   type PCNF1_Register is record
      --  Maximum length of packet payload. If the packet payload is larger
      --  than MAXLEN, the radio will truncate the payload to MAXLEN.
      MAXLEN         : PCNF1_MAXLEN_Field := 16#0#;
      --  Static length in number of bytes
      STATLEN        : PCNF1_STATLEN_Field := 16#0#;
      --  Base address length in number of bytes
      BALEN          : PCNF1_BALEN_Field := 16#0#;
      --  unspecified
      Reserved_19_23 : HAL.UInt5 := 16#0#;
      --  On air endianness of packet, this applies to the S0, LENGTH, S1 and
      --  the PAYLOAD fields.
      ENDIAN         : PCNF1_ENDIAN_Field := NRF_SVD.RADIO.Little;
      --  Enable or disable packet whitening
      WHITEEN        : PCNF1_WHITEEN_Field := NRF_SVD.RADIO.Disabled;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PCNF1_Register use record
      MAXLEN         at 0 range 0 .. 7;
      STATLEN        at 0 range 8 .. 15;
      BALEN          at 0 range 16 .. 18;
      Reserved_19_23 at 0 range 19 .. 23;
      ENDIAN         at 0 range 24 .. 24;
      WHITEEN        at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  PREFIX0_AP array element
   subtype PREFIX0_AP_Element is HAL.UInt8;

   --  PREFIX0_AP array
   type PREFIX0_AP_Field_Array is array (0 .. 3) of PREFIX0_AP_Element
     with Component_Size => 8, Size => 32;

   --  Prefixes bytes for logical addresses 0-3
   type PREFIX0_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AP as a value
            Val : HAL.UInt32;
         when True =>
            --  AP as an array
            Arr : PREFIX0_AP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PREFIX0_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  PREFIX1_AP array element
   subtype PREFIX1_AP_Element is HAL.UInt8;

   --  PREFIX1_AP array
   type PREFIX1_AP_Field_Array is array (4 .. 7) of PREFIX1_AP_Element
     with Component_Size => 8, Size => 32;

   --  Prefixes bytes for logical addresses 4-7
   type PREFIX1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AP as a value
            Val : HAL.UInt32;
         when True =>
            --  AP as an array
            Arr : PREFIX1_AP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PREFIX1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype TXADDRESS_TXADDRESS_Field is HAL.UInt3;

   --  Transmit address select
   type TXADDRESS_Register is record
      --  Transmit address select
      TXADDRESS     : TXADDRESS_TXADDRESS_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXADDRESS_Register use record
      TXADDRESS     at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Enable or disable reception on logical address 0.
   type RXADDRESSES_ADDR0_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for RXADDRESSES_ADDR0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  RXADDRESSES_ADDR array
   type RXADDRESSES_ADDR_Field_Array is array (0 .. 7)
     of RXADDRESSES_ADDR0_Field
     with Component_Size => 1, Size => 8;

   --  Type definition for RXADDRESSES_ADDR
   type RXADDRESSES_ADDR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ADDR as a value
            Val : HAL.UInt8;
         when True =>
            --  ADDR as an array
            Arr : RXADDRESSES_ADDR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for RXADDRESSES_ADDR_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  Receive address select
   type RXADDRESSES_Register is record
      --  Enable or disable reception on logical address 0.
      ADDR          : RXADDRESSES_ADDR_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXADDRESSES_Register use record
      ADDR          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  CRC length in number of bytes.
   type CRCCNF_LEN_Field is
     (--  CRC length is zero and CRC calculation is disabled
      Disabled,
      --  CRC length is one byte and CRC calculation is enabled
      One,
      --  CRC length is two bytes and CRC calculation is enabled
      Two,
      --  CRC length is three bytes and CRC calculation is enabled
      Three)
     with Size => 2;
   for CRCCNF_LEN_Field use
     (Disabled => 0,
      One => 1,
      Two => 2,
      Three => 3);

   --  Include or exclude packet address field out of CRC calculation.
   type CRCCNF_SKIPADDR_Field is
     (--  CRC calculation includes address field
      Include,
      --  CRC calculation does not include address field. The CRC calculation will
--  start at the first byte after the address.
      Skip)
     with Size => 1;
   for CRCCNF_SKIPADDR_Field use
     (Include => 0,
      Skip => 1);

   --  CRC configuration
   type CRCCNF_Register is record
      --  CRC length in number of bytes.
      LEN           : CRCCNF_LEN_Field := NRF_SVD.RADIO.Disabled;
      --  unspecified
      Reserved_2_7  : HAL.UInt6 := 16#0#;
      --  Include or exclude packet address field out of CRC calculation.
      SKIPADDR      : CRCCNF_SKIPADDR_Field := NRF_SVD.RADIO.Include;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CRCCNF_Register use record
      LEN           at 0 range 0 .. 1;
      Reserved_2_7  at 0 range 2 .. 7;
      SKIPADDR      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype CRCPOLY_CRCPOLY_Field is HAL.UInt24;

   --  CRC polynomial
   type CRCPOLY_Register is record
      --  CRC polynomial
      CRCPOLY        : CRCPOLY_CRCPOLY_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CRCPOLY_Register use record
      CRCPOLY        at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CRCINIT_CRCINIT_Field is HAL.UInt24;

   --  CRC initial value
   type CRCINIT_Register is record
      --  CRC initial value
      CRCINIT        : CRCINIT_CRCINIT_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CRCINIT_Register use record
      CRCINIT        at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype TIFS_TIFS_Field is HAL.UInt8;

   --  Inter Frame Spacing in us
   type TIFS_Register is record
      --  Inter Frame Spacing in us
      TIFS          : TIFS_TIFS_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIFS_Register use record
      TIFS          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype RSSISAMPLE_RSSISAMPLE_Field is HAL.UInt7;

   --  RSSI sample
   type RSSISAMPLE_Register is record
      --  Read-only. RSSI sample
      RSSISAMPLE    : RSSISAMPLE_RSSISAMPLE_Field;
      --  unspecified
      Reserved_7_31 : HAL.UInt25;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSSISAMPLE_Register use record
      RSSISAMPLE    at 0 range 0 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  Current radio state
   type STATE_STATE_Field is
     (--  RADIO is in the Disabled state
      Disabled,
      --  RADIO is in the RXRU state
      Rxru,
      --  RADIO is in the RXIDLE state
      Rxidle,
      --  RADIO is in the RX state
      Rx,
      --  RADIO is in the RXDISABLED state
      Rxdisable,
      --  RADIO is in the TXRU state
      Txru,
      --  RADIO is in the TXIDLE state
      Txidle,
      --  RADIO is in the TX state
      Tx,
      --  RADIO is in the TXDISABLED state
      Txdisable)
     with Size => 4;
   for STATE_STATE_Field use
     (Disabled => 0,
      Rxru => 1,
      Rxidle => 2,
      Rx => 3,
      Rxdisable => 4,
      Txru => 9,
      Txidle => 10,
      Tx => 11,
      Txdisable => 12);

   --  Current radio state
   type STATE_Register is record
      --  Read-only. Current radio state
      STATE         : STATE_STATE_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for STATE_Register use record
      STATE         at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype DATAWHITEIV_DATAWHITEIV_Field is HAL.UInt7;

   --  Data whitening initial value
   type DATAWHITEIV_Register is record
      --  Data whitening initial value. Bit 6 is hard-wired to '1', writing '0'
      --  to it has no effect, and it will always be read back and used by the
      --  device as '1'.
      DATAWHITEIV   : DATAWHITEIV_DATAWHITEIV_Field := 16#40#;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DATAWHITEIV_Register use record
      DATAWHITEIV   at 0 range 0 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  Description collection[0]: Device address base segment 0

   --  Description collection[0]: Device address base segment 0
   type DAB_Registers is array (0 .. 7) of HAL.UInt32;

   subtype DAP_DAP_Field is HAL.UInt16;

   --  Description collection[0]: Device address prefix 0
   type DAP_Register is record
      --  Device address prefix 0
      DAP            : DAP_DAP_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DAP_Register use record
      DAP            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Description collection[0]: Device address prefix 0
   type DAP_Registers is array (0 .. 7) of DAP_Register;

   --  Enable or disable device address matching using device address 0
   type DACNF_ENA0_Field is
     (--  Disabled
      Disabled,
      --  Enabled
      Enabled)
     with Size => 1;
   for DACNF_ENA0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  DACNF_ENA array
   type DACNF_ENA_Field_Array is array (0 .. 7) of DACNF_ENA0_Field
     with Component_Size => 1, Size => 8;

   --  Type definition for DACNF_ENA
   type DACNF_ENA_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ENA as a value
            Val : HAL.UInt8;
         when True =>
            --  ENA as an array
            Arr : DACNF_ENA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for DACNF_ENA_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  DACNF_TXADD array
   type DACNF_TXADD_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for DACNF_TXADD
   type DACNF_TXADD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TXADD as a value
            Val : HAL.UInt8;
         when True =>
            --  TXADD as an array
            Arr : DACNF_TXADD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for DACNF_TXADD_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  Device address match configuration
   type DACNF_Register is record
      --  Enable or disable device address matching using device address 0
      ENA            : DACNF_ENA_Field := (As_Array => False, Val => 16#0#);
      --  TxAdd for device address 0
      TXADD          : DACNF_TXADD_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DACNF_Register use record
      ENA            at 0 range 0 .. 7;
      TXADD          at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Radio ramp-up time
   type MODECNF0_RU_Field is
     (--  Default ramp-up time (tRXEN), compatible with firmware written for nRF51
      Default,
      --  Fast ramp-up (tRXEN,FAST), see electrical specification for more
--  information
      Fast)
     with Size => 1;
   for MODECNF0_RU_Field use
     (Default => 0,
      Fast => 1);

   --  Default TX value
   type MODECNF0_DTX_Field is
     (--  Transmit '1'
      B1,
      --  Transmit '0'
      B0,
      --  Transmit center frequency
      Center)
     with Size => 2;
   for MODECNF0_DTX_Field use
     (B1 => 0,
      B0 => 1,
      Center => 2);

   --  Radio mode configuration register 0
   type MODECNF0_Register is record
      --  Radio ramp-up time
      RU             : MODECNF0_RU_Field := NRF_SVD.RADIO.Default;
      --  unspecified
      Reserved_1_7   : HAL.UInt7 := 16#0#;
      --  Default TX value
      DTX            : MODECNF0_DTX_Field := NRF_SVD.RADIO.Center;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MODECNF0_Register use record
      RU             at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      DTX            at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  Peripheral power control. The peripheral and its registers will be reset
   --  to its initial state by switching the peripheral off and then back on
   --  again.
   type POWER_POWER_Field is
     (--  Peripheral is powered off
      Disabled,
      --  Peripheral is powered on
      Enabled)
     with Size => 1;
   for POWER_POWER_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Peripheral power control
   type POWER_Register is record
      --  Peripheral power control. The peripheral and its registers will be
      --  reset to its initial state by switching the peripheral off and then
      --  back on again.
      POWER         : POWER_POWER_Field := NRF_SVD.RADIO.Enabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for POWER_Register use record
      POWER         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  2.4 GHz Radio
   type RADIO_Peripheral is record
      --  Enable RADIO in TX mode
      TASKS_TXEN      : aliased HAL.UInt32;
      --  Enable RADIO in RX mode
      TASKS_RXEN      : aliased HAL.UInt32;
      --  Start RADIO
      TASKS_START     : aliased HAL.UInt32;
      --  Stop RADIO
      TASKS_STOP      : aliased HAL.UInt32;
      --  Disable RADIO
      TASKS_DISABLE   : aliased HAL.UInt32;
      --  Start the RSSI and take one single sample of the receive signal
      --  strength.
      TASKS_RSSISTART : aliased HAL.UInt32;
      --  Stop the RSSI measurement
      TASKS_RSSISTOP  : aliased HAL.UInt32;
      --  Start the bit counter
      TASKS_BCSTART   : aliased HAL.UInt32;
      --  Stop the bit counter
      TASKS_BCSTOP    : aliased HAL.UInt32;
      --  RADIO has ramped up and is ready to be started
      EVENTS_READY    : aliased HAL.UInt32;
      --  Address sent or received
      EVENTS_ADDRESS  : aliased HAL.UInt32;
      --  Packet payload sent or received
      EVENTS_PAYLOAD  : aliased HAL.UInt32;
      --  Packet sent or received
      EVENTS_END      : aliased HAL.UInt32;
      --  RADIO has been disabled
      EVENTS_DISABLED : aliased HAL.UInt32;
      --  A device address match occurred on the last received packet
      EVENTS_DEVMATCH : aliased HAL.UInt32;
      --  No device address match occurred on the last received packet
      EVENTS_DEVMISS  : aliased HAL.UInt32;
      --  Sampling of receive signal strength complete.
      EVENTS_RSSIEND  : aliased HAL.UInt32;
      --  Bit counter reached bit count value.
      EVENTS_BCMATCH  : aliased HAL.UInt32;
      --  Packet received with CRC ok
      EVENTS_CRCOK    : aliased HAL.UInt32;
      --  Packet received with CRC error
      EVENTS_CRCERROR : aliased HAL.UInt32;
      --  Shortcut register
      SHORTS          : aliased SHORTS_Register;
      --  Enable interrupt
      INTENSET        : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR        : aliased INTENCLR_Register;
      --  CRC status
      CRCSTATUS       : aliased CRCSTATUS_Register;
      --  Received address
      RXMATCH         : aliased RXMATCH_Register;
      --  CRC field of previously received packet
      RXCRC           : aliased RXCRC_Register;
      --  Device address match index
      DAI             : aliased DAI_Register;
      --  Packet pointer
      PACKETPTR       : aliased HAL.UInt32;
      --  Frequency
      FREQUENCY       : aliased FREQUENCY_Register;
      --  Output power
      TXPOWER         : aliased TXPOWER_Register;
      --  Data rate and modulation
      MODE            : aliased MODE_Register;
      --  Packet configuration register 0
      PCNF0           : aliased PCNF0_Register;
      --  Packet configuration register 1
      PCNF1           : aliased PCNF1_Register;
      --  Base address 0
      BASE0           : aliased HAL.UInt32;
      --  Base address 1
      BASE1           : aliased HAL.UInt32;
      --  Prefixes bytes for logical addresses 0-3
      PREFIX0         : aliased PREFIX0_Register;
      --  Prefixes bytes for logical addresses 4-7
      PREFIX1         : aliased PREFIX1_Register;
      --  Transmit address select
      TXADDRESS       : aliased TXADDRESS_Register;
      --  Receive address select
      RXADDRESSES     : aliased RXADDRESSES_Register;
      --  CRC configuration
      CRCCNF          : aliased CRCCNF_Register;
      --  CRC polynomial
      CRCPOLY         : aliased CRCPOLY_Register;
      --  CRC initial value
      CRCINIT         : aliased CRCINIT_Register;
      --  Unspecified
      UNUSED0         : aliased HAL.UInt32;
      --  Inter Frame Spacing in us
      TIFS            : aliased TIFS_Register;
      --  RSSI sample
      RSSISAMPLE      : aliased RSSISAMPLE_Register;
      --  Current radio state
      STATE           : aliased STATE_Register;
      --  Data whitening initial value
      DATAWHITEIV     : aliased DATAWHITEIV_Register;
      --  Bit counter compare
      BCC             : aliased HAL.UInt32;
      --  Description collection[0]: Device address base segment 0
      DAB             : aliased DAB_Registers;
      --  Description collection[0]: Device address prefix 0
      DAP             : aliased DAP_Registers;
      --  Device address match configuration
      DACNF           : aliased DACNF_Register;
      --  Radio mode configuration register 0
      MODECNF0        : aliased MODECNF0_Register;
      --  Peripheral power control
      POWER           : aliased POWER_Register;
   end record
     with Volatile;

   for RADIO_Peripheral use record
      TASKS_TXEN      at 16#0# range 0 .. 31;
      TASKS_RXEN      at 16#4# range 0 .. 31;
      TASKS_START     at 16#8# range 0 .. 31;
      TASKS_STOP      at 16#C# range 0 .. 31;
      TASKS_DISABLE   at 16#10# range 0 .. 31;
      TASKS_RSSISTART at 16#14# range 0 .. 31;
      TASKS_RSSISTOP  at 16#18# range 0 .. 31;
      TASKS_BCSTART   at 16#1C# range 0 .. 31;
      TASKS_BCSTOP    at 16#20# range 0 .. 31;
      EVENTS_READY    at 16#100# range 0 .. 31;
      EVENTS_ADDRESS  at 16#104# range 0 .. 31;
      EVENTS_PAYLOAD  at 16#108# range 0 .. 31;
      EVENTS_END      at 16#10C# range 0 .. 31;
      EVENTS_DISABLED at 16#110# range 0 .. 31;
      EVENTS_DEVMATCH at 16#114# range 0 .. 31;
      EVENTS_DEVMISS  at 16#118# range 0 .. 31;
      EVENTS_RSSIEND  at 16#11C# range 0 .. 31;
      EVENTS_BCMATCH  at 16#128# range 0 .. 31;
      EVENTS_CRCOK    at 16#130# range 0 .. 31;
      EVENTS_CRCERROR at 16#134# range 0 .. 31;
      SHORTS          at 16#200# range 0 .. 31;
      INTENSET        at 16#304# range 0 .. 31;
      INTENCLR        at 16#308# range 0 .. 31;
      CRCSTATUS       at 16#400# range 0 .. 31;
      RXMATCH         at 16#408# range 0 .. 31;
      RXCRC           at 16#40C# range 0 .. 31;
      DAI             at 16#410# range 0 .. 31;
      PACKETPTR       at 16#504# range 0 .. 31;
      FREQUENCY       at 16#508# range 0 .. 31;
      TXPOWER         at 16#50C# range 0 .. 31;
      MODE            at 16#510# range 0 .. 31;
      PCNF0           at 16#514# range 0 .. 31;
      PCNF1           at 16#518# range 0 .. 31;
      BASE0           at 16#51C# range 0 .. 31;
      BASE1           at 16#520# range 0 .. 31;
      PREFIX0         at 16#524# range 0 .. 31;
      PREFIX1         at 16#528# range 0 .. 31;
      TXADDRESS       at 16#52C# range 0 .. 31;
      RXADDRESSES     at 16#530# range 0 .. 31;
      CRCCNF          at 16#534# range 0 .. 31;
      CRCPOLY         at 16#538# range 0 .. 31;
      CRCINIT         at 16#53C# range 0 .. 31;
      UNUSED0         at 16#540# range 0 .. 31;
      TIFS            at 16#544# range 0 .. 31;
      RSSISAMPLE      at 16#548# range 0 .. 31;
      STATE           at 16#550# range 0 .. 31;
      DATAWHITEIV     at 16#554# range 0 .. 31;
      BCC             at 16#560# range 0 .. 31;
      DAB             at 16#600# range 0 .. 255;
      DAP             at 16#620# range 0 .. 255;
      DACNF           at 16#640# range 0 .. 31;
      MODECNF0        at 16#650# range 0 .. 31;
      POWER           at 16#FFC# range 0 .. 31;
   end record;

   --  2.4 GHz Radio
   RADIO_Periph : aliased RADIO_Peripheral
     with Import, Address => RADIO_Base;

end NRF_SVD.RADIO;
