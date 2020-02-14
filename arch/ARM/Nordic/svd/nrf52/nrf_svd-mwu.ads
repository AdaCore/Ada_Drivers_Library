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

package NRF_SVD.MWU is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ---------------------------------------
   -- EVENTS_REGION cluster's Registers --
   ---------------------------------------

   --  Unspecified
   type EVENTS_REGION_Cluster is record
      --  Description cluster[0]: Write access to region 0 detected
      WA : aliased HAL.UInt32;
      --  Description cluster[0]: Read access to region 0 detected
      RA : aliased HAL.UInt32;
   end record
     with Size => 64;

   for EVENTS_REGION_Cluster use record
      WA at 16#0# range 0 .. 31;
      RA at 16#4# range 0 .. 31;
   end record;

   --  Unspecified
   type EVENTS_REGION_Clusters is array (0 .. 3) of EVENTS_REGION_Cluster;

   ----------------------------------------
   -- EVENTS_PREGION cluster's Registers --
   ----------------------------------------

   --  Unspecified
   type EVENTS_PREGION_Cluster is record
      --  Description cluster[0]: Write access to peripheral region 0 detected
      WA : aliased HAL.UInt32;
      --  Description cluster[0]: Read access to peripheral region 0 detected
      RA : aliased HAL.UInt32;
   end record
     with Size => 64;

   for EVENTS_PREGION_Cluster use record
      WA at 16#0# range 0 .. 31;
      RA at 16#4# range 0 .. 31;
   end record;

   --  Unspecified
   type EVENTS_PREGION_Clusters is array (0 .. 1) of EVENTS_PREGION_Cluster;

   --  Enable or disable interrupt for REGION[0].WA event
   type INTEN_REGION0WA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_REGION0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for REGION[0].RA event
   type INTEN_REGION0RA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_REGION0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for REGION[1].WA event
   type INTEN_REGION1WA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_REGION1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for REGION[1].RA event
   type INTEN_REGION1RA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_REGION1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for REGION[2].WA event
   type INTEN_REGION2WA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_REGION2WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for REGION[2].RA event
   type INTEN_REGION2RA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_REGION2RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for REGION[3].WA event
   type INTEN_REGION3WA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_REGION3WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for REGION[3].RA event
   type INTEN_REGION3RA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_REGION3RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for PREGION[0].WA event
   type INTEN_PREGION0WA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_PREGION0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for PREGION[0].RA event
   type INTEN_PREGION0RA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_PREGION0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for PREGION[1].WA event
   type INTEN_PREGION1WA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_PREGION1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for PREGION[1].RA event
   type INTEN_PREGION1RA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_PREGION1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt
   type INTEN_Register is record
      --  Enable or disable interrupt for REGION[0].WA event
      REGION0WA      : INTEN_REGION0WA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable interrupt for REGION[0].RA event
      REGION0RA      : INTEN_REGION0RA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable interrupt for REGION[1].WA event
      REGION1WA      : INTEN_REGION1WA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable interrupt for REGION[1].RA event
      REGION1RA      : INTEN_REGION1RA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable interrupt for REGION[2].WA event
      REGION2WA      : INTEN_REGION2WA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable interrupt for REGION[2].RA event
      REGION2RA      : INTEN_REGION2RA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable interrupt for REGION[3].WA event
      REGION3WA      : INTEN_REGION3WA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable interrupt for REGION[3].RA event
      REGION3RA      : INTEN_REGION3RA_Field := NRF_SVD.MWU.Disabled;
      --  unspecified
      Reserved_8_23  : HAL.UInt16 := 16#0#;
      --  Enable or disable interrupt for PREGION[0].WA event
      PREGION0WA     : INTEN_PREGION0WA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable interrupt for PREGION[0].RA event
      PREGION0RA     : INTEN_PREGION0RA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable interrupt for PREGION[1].WA event
      PREGION1WA     : INTEN_PREGION1WA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable interrupt for PREGION[1].RA event
      PREGION1RA     : INTEN_PREGION1RA_Field := NRF_SVD.MWU.Disabled;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTEN_Register use record
      REGION0WA      at 0 range 0 .. 0;
      REGION0RA      at 0 range 1 .. 1;
      REGION1WA      at 0 range 2 .. 2;
      REGION1RA      at 0 range 3 .. 3;
      REGION2WA      at 0 range 4 .. 4;
      REGION2RA      at 0 range 5 .. 5;
      REGION3WA      at 0 range 6 .. 6;
      REGION3RA      at 0 range 7 .. 7;
      Reserved_8_23  at 0 range 8 .. 23;
      PREGION0WA     at 0 range 24 .. 24;
      PREGION0RA     at 0 range 25 .. 25;
      PREGION1WA     at 0 range 26 .. 26;
      PREGION1RA     at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  Write '1' to Enable interrupt for REGION[0].WA event
   type INTENSET_REGION0WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_REGION0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for REGION[0].WA event
   type INTENSET_REGION0WA_Field_1 is
     (--  Reset value for the field
      Intenset_Region0Wa_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_REGION0WA_Field_1 use
     (Intenset_Region0Wa_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for REGION[0].RA event
   type INTENSET_REGION0RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_REGION0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for REGION[0].RA event
   type INTENSET_REGION0RA_Field_1 is
     (--  Reset value for the field
      Intenset_Region0Ra_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_REGION0RA_Field_1 use
     (Intenset_Region0Ra_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for REGION[1].WA event
   type INTENSET_REGION1WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_REGION1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for REGION[1].WA event
   type INTENSET_REGION1WA_Field_1 is
     (--  Reset value for the field
      Intenset_Region1Wa_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_REGION1WA_Field_1 use
     (Intenset_Region1Wa_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for REGION[1].RA event
   type INTENSET_REGION1RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_REGION1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for REGION[1].RA event
   type INTENSET_REGION1RA_Field_1 is
     (--  Reset value for the field
      Intenset_Region1Ra_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_REGION1RA_Field_1 use
     (Intenset_Region1Ra_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for REGION[2].WA event
   type INTENSET_REGION2WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_REGION2WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for REGION[2].WA event
   type INTENSET_REGION2WA_Field_1 is
     (--  Reset value for the field
      Intenset_Region2Wa_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_REGION2WA_Field_1 use
     (Intenset_Region2Wa_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for REGION[2].RA event
   type INTENSET_REGION2RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_REGION2RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for REGION[2].RA event
   type INTENSET_REGION2RA_Field_1 is
     (--  Reset value for the field
      Intenset_Region2Ra_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_REGION2RA_Field_1 use
     (Intenset_Region2Ra_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for REGION[3].WA event
   type INTENSET_REGION3WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_REGION3WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for REGION[3].WA event
   type INTENSET_REGION3WA_Field_1 is
     (--  Reset value for the field
      Intenset_Region3Wa_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_REGION3WA_Field_1 use
     (Intenset_Region3Wa_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for REGION[3].RA event
   type INTENSET_REGION3RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_REGION3RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for REGION[3].RA event
   type INTENSET_REGION3RA_Field_1 is
     (--  Reset value for the field
      Intenset_Region3Ra_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_REGION3RA_Field_1 use
     (Intenset_Region3Ra_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for PREGION[0].WA event
   type INTENSET_PREGION0WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_PREGION0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for PREGION[0].WA event
   type INTENSET_PREGION0WA_Field_1 is
     (--  Reset value for the field
      Intenset_Pregion0Wa_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_PREGION0WA_Field_1 use
     (Intenset_Pregion0Wa_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for PREGION[0].RA event
   type INTENSET_PREGION0RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_PREGION0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for PREGION[0].RA event
   type INTENSET_PREGION0RA_Field_1 is
     (--  Reset value for the field
      Intenset_Pregion0Ra_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_PREGION0RA_Field_1 use
     (Intenset_Pregion0Ra_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for PREGION[1].WA event
   type INTENSET_PREGION1WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_PREGION1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for PREGION[1].WA event
   type INTENSET_PREGION1WA_Field_1 is
     (--  Reset value for the field
      Intenset_Pregion1Wa_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_PREGION1WA_Field_1 use
     (Intenset_Pregion1Wa_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for PREGION[1].RA event
   type INTENSET_PREGION1RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_PREGION1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for PREGION[1].RA event
   type INTENSET_PREGION1RA_Field_1 is
     (--  Reset value for the field
      Intenset_Pregion1Ra_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_PREGION1RA_Field_1 use
     (Intenset_Pregion1Ra_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  Write '1' to Enable interrupt for REGION[0].WA event
      REGION0WA      : INTENSET_REGION0WA_Field_1 :=
                        Intenset_Region0Wa_Field_Reset;
      --  Write '1' to Enable interrupt for REGION[0].RA event
      REGION0RA      : INTENSET_REGION0RA_Field_1 :=
                        Intenset_Region0Ra_Field_Reset;
      --  Write '1' to Enable interrupt for REGION[1].WA event
      REGION1WA      : INTENSET_REGION1WA_Field_1 :=
                        Intenset_Region1Wa_Field_Reset;
      --  Write '1' to Enable interrupt for REGION[1].RA event
      REGION1RA      : INTENSET_REGION1RA_Field_1 :=
                        Intenset_Region1Ra_Field_Reset;
      --  Write '1' to Enable interrupt for REGION[2].WA event
      REGION2WA      : INTENSET_REGION2WA_Field_1 :=
                        Intenset_Region2Wa_Field_Reset;
      --  Write '1' to Enable interrupt for REGION[2].RA event
      REGION2RA      : INTENSET_REGION2RA_Field_1 :=
                        Intenset_Region2Ra_Field_Reset;
      --  Write '1' to Enable interrupt for REGION[3].WA event
      REGION3WA      : INTENSET_REGION3WA_Field_1 :=
                        Intenset_Region3Wa_Field_Reset;
      --  Write '1' to Enable interrupt for REGION[3].RA event
      REGION3RA      : INTENSET_REGION3RA_Field_1 :=
                        Intenset_Region3Ra_Field_Reset;
      --  unspecified
      Reserved_8_23  : HAL.UInt16 := 16#0#;
      --  Write '1' to Enable interrupt for PREGION[0].WA event
      PREGION0WA     : INTENSET_PREGION0WA_Field_1 :=
                        Intenset_Pregion0Wa_Field_Reset;
      --  Write '1' to Enable interrupt for PREGION[0].RA event
      PREGION0RA     : INTENSET_PREGION0RA_Field_1 :=
                        Intenset_Pregion0Ra_Field_Reset;
      --  Write '1' to Enable interrupt for PREGION[1].WA event
      PREGION1WA     : INTENSET_PREGION1WA_Field_1 :=
                        Intenset_Pregion1Wa_Field_Reset;
      --  Write '1' to Enable interrupt for PREGION[1].RA event
      PREGION1RA     : INTENSET_PREGION1RA_Field_1 :=
                        Intenset_Pregion1Ra_Field_Reset;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      REGION0WA      at 0 range 0 .. 0;
      REGION0RA      at 0 range 1 .. 1;
      REGION1WA      at 0 range 2 .. 2;
      REGION1RA      at 0 range 3 .. 3;
      REGION2WA      at 0 range 4 .. 4;
      REGION2RA      at 0 range 5 .. 5;
      REGION3WA      at 0 range 6 .. 6;
      REGION3RA      at 0 range 7 .. 7;
      Reserved_8_23  at 0 range 8 .. 23;
      PREGION0WA     at 0 range 24 .. 24;
      PREGION0RA     at 0 range 25 .. 25;
      PREGION1WA     at 0 range 26 .. 26;
      PREGION1RA     at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  Write '1' to Disable interrupt for REGION[0].WA event
   type INTENCLR_REGION0WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_REGION0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for REGION[0].WA event
   type INTENCLR_REGION0WA_Field_1 is
     (--  Reset value for the field
      Intenclr_Region0Wa_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_REGION0WA_Field_1 use
     (Intenclr_Region0Wa_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for REGION[0].RA event
   type INTENCLR_REGION0RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_REGION0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for REGION[0].RA event
   type INTENCLR_REGION0RA_Field_1 is
     (--  Reset value for the field
      Intenclr_Region0Ra_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_REGION0RA_Field_1 use
     (Intenclr_Region0Ra_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for REGION[1].WA event
   type INTENCLR_REGION1WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_REGION1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for REGION[1].WA event
   type INTENCLR_REGION1WA_Field_1 is
     (--  Reset value for the field
      Intenclr_Region1Wa_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_REGION1WA_Field_1 use
     (Intenclr_Region1Wa_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for REGION[1].RA event
   type INTENCLR_REGION1RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_REGION1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for REGION[1].RA event
   type INTENCLR_REGION1RA_Field_1 is
     (--  Reset value for the field
      Intenclr_Region1Ra_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_REGION1RA_Field_1 use
     (Intenclr_Region1Ra_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for REGION[2].WA event
   type INTENCLR_REGION2WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_REGION2WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for REGION[2].WA event
   type INTENCLR_REGION2WA_Field_1 is
     (--  Reset value for the field
      Intenclr_Region2Wa_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_REGION2WA_Field_1 use
     (Intenclr_Region2Wa_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for REGION[2].RA event
   type INTENCLR_REGION2RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_REGION2RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for REGION[2].RA event
   type INTENCLR_REGION2RA_Field_1 is
     (--  Reset value for the field
      Intenclr_Region2Ra_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_REGION2RA_Field_1 use
     (Intenclr_Region2Ra_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for REGION[3].WA event
   type INTENCLR_REGION3WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_REGION3WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for REGION[3].WA event
   type INTENCLR_REGION3WA_Field_1 is
     (--  Reset value for the field
      Intenclr_Region3Wa_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_REGION3WA_Field_1 use
     (Intenclr_Region3Wa_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for REGION[3].RA event
   type INTENCLR_REGION3RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_REGION3RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for REGION[3].RA event
   type INTENCLR_REGION3RA_Field_1 is
     (--  Reset value for the field
      Intenclr_Region3Ra_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_REGION3RA_Field_1 use
     (Intenclr_Region3Ra_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for PREGION[0].WA event
   type INTENCLR_PREGION0WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_PREGION0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for PREGION[0].WA event
   type INTENCLR_PREGION0WA_Field_1 is
     (--  Reset value for the field
      Intenclr_Pregion0Wa_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_PREGION0WA_Field_1 use
     (Intenclr_Pregion0Wa_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for PREGION[0].RA event
   type INTENCLR_PREGION0RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_PREGION0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for PREGION[0].RA event
   type INTENCLR_PREGION0RA_Field_1 is
     (--  Reset value for the field
      Intenclr_Pregion0Ra_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_PREGION0RA_Field_1 use
     (Intenclr_Pregion0Ra_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for PREGION[1].WA event
   type INTENCLR_PREGION1WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_PREGION1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for PREGION[1].WA event
   type INTENCLR_PREGION1WA_Field_1 is
     (--  Reset value for the field
      Intenclr_Pregion1Wa_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_PREGION1WA_Field_1 use
     (Intenclr_Pregion1Wa_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for PREGION[1].RA event
   type INTENCLR_PREGION1RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_PREGION1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for PREGION[1].RA event
   type INTENCLR_PREGION1RA_Field_1 is
     (--  Reset value for the field
      Intenclr_Pregion1Ra_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_PREGION1RA_Field_1 use
     (Intenclr_Pregion1Ra_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  Write '1' to Disable interrupt for REGION[0].WA event
      REGION0WA      : INTENCLR_REGION0WA_Field_1 :=
                        Intenclr_Region0Wa_Field_Reset;
      --  Write '1' to Disable interrupt for REGION[0].RA event
      REGION0RA      : INTENCLR_REGION0RA_Field_1 :=
                        Intenclr_Region0Ra_Field_Reset;
      --  Write '1' to Disable interrupt for REGION[1].WA event
      REGION1WA      : INTENCLR_REGION1WA_Field_1 :=
                        Intenclr_Region1Wa_Field_Reset;
      --  Write '1' to Disable interrupt for REGION[1].RA event
      REGION1RA      : INTENCLR_REGION1RA_Field_1 :=
                        Intenclr_Region1Ra_Field_Reset;
      --  Write '1' to Disable interrupt for REGION[2].WA event
      REGION2WA      : INTENCLR_REGION2WA_Field_1 :=
                        Intenclr_Region2Wa_Field_Reset;
      --  Write '1' to Disable interrupt for REGION[2].RA event
      REGION2RA      : INTENCLR_REGION2RA_Field_1 :=
                        Intenclr_Region2Ra_Field_Reset;
      --  Write '1' to Disable interrupt for REGION[3].WA event
      REGION3WA      : INTENCLR_REGION3WA_Field_1 :=
                        Intenclr_Region3Wa_Field_Reset;
      --  Write '1' to Disable interrupt for REGION[3].RA event
      REGION3RA      : INTENCLR_REGION3RA_Field_1 :=
                        Intenclr_Region3Ra_Field_Reset;
      --  unspecified
      Reserved_8_23  : HAL.UInt16 := 16#0#;
      --  Write '1' to Disable interrupt for PREGION[0].WA event
      PREGION0WA     : INTENCLR_PREGION0WA_Field_1 :=
                        Intenclr_Pregion0Wa_Field_Reset;
      --  Write '1' to Disable interrupt for PREGION[0].RA event
      PREGION0RA     : INTENCLR_PREGION0RA_Field_1 :=
                        Intenclr_Pregion0Ra_Field_Reset;
      --  Write '1' to Disable interrupt for PREGION[1].WA event
      PREGION1WA     : INTENCLR_PREGION1WA_Field_1 :=
                        Intenclr_Pregion1Wa_Field_Reset;
      --  Write '1' to Disable interrupt for PREGION[1].RA event
      PREGION1RA     : INTENCLR_PREGION1RA_Field_1 :=
                        Intenclr_Pregion1Ra_Field_Reset;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      REGION0WA      at 0 range 0 .. 0;
      REGION0RA      at 0 range 1 .. 1;
      REGION1WA      at 0 range 2 .. 2;
      REGION1RA      at 0 range 3 .. 3;
      REGION2WA      at 0 range 4 .. 4;
      REGION2RA      at 0 range 5 .. 5;
      REGION3WA      at 0 range 6 .. 6;
      REGION3RA      at 0 range 7 .. 7;
      Reserved_8_23  at 0 range 8 .. 23;
      PREGION0WA     at 0 range 24 .. 24;
      PREGION0RA     at 0 range 25 .. 25;
      PREGION1WA     at 0 range 26 .. 26;
      PREGION1RA     at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  Enable or disable non-maskable interrupt for REGION[0].WA event
   type NMIEN_REGION0WA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for NMIEN_REGION0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable non-maskable interrupt for REGION[0].RA event
   type NMIEN_REGION0RA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for NMIEN_REGION0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable non-maskable interrupt for REGION[1].WA event
   type NMIEN_REGION1WA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for NMIEN_REGION1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable non-maskable interrupt for REGION[1].RA event
   type NMIEN_REGION1RA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for NMIEN_REGION1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable non-maskable interrupt for REGION[2].WA event
   type NMIEN_REGION2WA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for NMIEN_REGION2WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable non-maskable interrupt for REGION[2].RA event
   type NMIEN_REGION2RA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for NMIEN_REGION2RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable non-maskable interrupt for REGION[3].WA event
   type NMIEN_REGION3WA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for NMIEN_REGION3WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable non-maskable interrupt for REGION[3].RA event
   type NMIEN_REGION3RA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for NMIEN_REGION3RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable non-maskable interrupt for PREGION[0].WA event
   type NMIEN_PREGION0WA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for NMIEN_PREGION0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable non-maskable interrupt for PREGION[0].RA event
   type NMIEN_PREGION0RA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for NMIEN_PREGION0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable non-maskable interrupt for PREGION[1].WA event
   type NMIEN_PREGION1WA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for NMIEN_PREGION1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable non-maskable interrupt for PREGION[1].RA event
   type NMIEN_PREGION1RA_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for NMIEN_PREGION1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable non-maskable interrupt
   type NMIEN_Register is record
      --  Enable or disable non-maskable interrupt for REGION[0].WA event
      REGION0WA      : NMIEN_REGION0WA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable non-maskable interrupt for REGION[0].RA event
      REGION0RA      : NMIEN_REGION0RA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable non-maskable interrupt for REGION[1].WA event
      REGION1WA      : NMIEN_REGION1WA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable non-maskable interrupt for REGION[1].RA event
      REGION1RA      : NMIEN_REGION1RA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable non-maskable interrupt for REGION[2].WA event
      REGION2WA      : NMIEN_REGION2WA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable non-maskable interrupt for REGION[2].RA event
      REGION2RA      : NMIEN_REGION2RA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable non-maskable interrupt for REGION[3].WA event
      REGION3WA      : NMIEN_REGION3WA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable non-maskable interrupt for REGION[3].RA event
      REGION3RA      : NMIEN_REGION3RA_Field := NRF_SVD.MWU.Disabled;
      --  unspecified
      Reserved_8_23  : HAL.UInt16 := 16#0#;
      --  Enable or disable non-maskable interrupt for PREGION[0].WA event
      PREGION0WA     : NMIEN_PREGION0WA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable non-maskable interrupt for PREGION[0].RA event
      PREGION0RA     : NMIEN_PREGION0RA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable non-maskable interrupt for PREGION[1].WA event
      PREGION1WA     : NMIEN_PREGION1WA_Field := NRF_SVD.MWU.Disabled;
      --  Enable or disable non-maskable interrupt for PREGION[1].RA event
      PREGION1RA     : NMIEN_PREGION1RA_Field := NRF_SVD.MWU.Disabled;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NMIEN_Register use record
      REGION0WA      at 0 range 0 .. 0;
      REGION0RA      at 0 range 1 .. 1;
      REGION1WA      at 0 range 2 .. 2;
      REGION1RA      at 0 range 3 .. 3;
      REGION2WA      at 0 range 4 .. 4;
      REGION2RA      at 0 range 5 .. 5;
      REGION3WA      at 0 range 6 .. 6;
      REGION3RA      at 0 range 7 .. 7;
      Reserved_8_23  at 0 range 8 .. 23;
      PREGION0WA     at 0 range 24 .. 24;
      PREGION0RA     at 0 range 25 .. 25;
      PREGION1WA     at 0 range 26 .. 26;
      PREGION1RA     at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  Write '1' to Enable non-maskable interrupt for REGION[0].WA event
   type NMIENSET_REGION0WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENSET_REGION0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable non-maskable interrupt for REGION[0].WA event
   type NMIENSET_REGION0WA_Field_1 is
     (--  Reset value for the field
      Nmienset_Region0Wa_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for NMIENSET_REGION0WA_Field_1 use
     (Nmienset_Region0Wa_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable non-maskable interrupt for REGION[0].RA event
   type NMIENSET_REGION0RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENSET_REGION0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable non-maskable interrupt for REGION[0].RA event
   type NMIENSET_REGION0RA_Field_1 is
     (--  Reset value for the field
      Nmienset_Region0Ra_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for NMIENSET_REGION0RA_Field_1 use
     (Nmienset_Region0Ra_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable non-maskable interrupt for REGION[1].WA event
   type NMIENSET_REGION1WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENSET_REGION1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable non-maskable interrupt for REGION[1].WA event
   type NMIENSET_REGION1WA_Field_1 is
     (--  Reset value for the field
      Nmienset_Region1Wa_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for NMIENSET_REGION1WA_Field_1 use
     (Nmienset_Region1Wa_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable non-maskable interrupt for REGION[1].RA event
   type NMIENSET_REGION1RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENSET_REGION1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable non-maskable interrupt for REGION[1].RA event
   type NMIENSET_REGION1RA_Field_1 is
     (--  Reset value for the field
      Nmienset_Region1Ra_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for NMIENSET_REGION1RA_Field_1 use
     (Nmienset_Region1Ra_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable non-maskable interrupt for REGION[2].WA event
   type NMIENSET_REGION2WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENSET_REGION2WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable non-maskable interrupt for REGION[2].WA event
   type NMIENSET_REGION2WA_Field_1 is
     (--  Reset value for the field
      Nmienset_Region2Wa_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for NMIENSET_REGION2WA_Field_1 use
     (Nmienset_Region2Wa_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable non-maskable interrupt for REGION[2].RA event
   type NMIENSET_REGION2RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENSET_REGION2RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable non-maskable interrupt for REGION[2].RA event
   type NMIENSET_REGION2RA_Field_1 is
     (--  Reset value for the field
      Nmienset_Region2Ra_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for NMIENSET_REGION2RA_Field_1 use
     (Nmienset_Region2Ra_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable non-maskable interrupt for REGION[3].WA event
   type NMIENSET_REGION3WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENSET_REGION3WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable non-maskable interrupt for REGION[3].WA event
   type NMIENSET_REGION3WA_Field_1 is
     (--  Reset value for the field
      Nmienset_Region3Wa_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for NMIENSET_REGION3WA_Field_1 use
     (Nmienset_Region3Wa_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable non-maskable interrupt for REGION[3].RA event
   type NMIENSET_REGION3RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENSET_REGION3RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable non-maskable interrupt for REGION[3].RA event
   type NMIENSET_REGION3RA_Field_1 is
     (--  Reset value for the field
      Nmienset_Region3Ra_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for NMIENSET_REGION3RA_Field_1 use
     (Nmienset_Region3Ra_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable non-maskable interrupt for PREGION[0].WA event
   type NMIENSET_PREGION0WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENSET_PREGION0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable non-maskable interrupt for PREGION[0].WA event
   type NMIENSET_PREGION0WA_Field_1 is
     (--  Reset value for the field
      Nmienset_Pregion0Wa_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for NMIENSET_PREGION0WA_Field_1 use
     (Nmienset_Pregion0Wa_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable non-maskable interrupt for PREGION[0].RA event
   type NMIENSET_PREGION0RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENSET_PREGION0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable non-maskable interrupt for PREGION[0].RA event
   type NMIENSET_PREGION0RA_Field_1 is
     (--  Reset value for the field
      Nmienset_Pregion0Ra_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for NMIENSET_PREGION0RA_Field_1 use
     (Nmienset_Pregion0Ra_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable non-maskable interrupt for PREGION[1].WA event
   type NMIENSET_PREGION1WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENSET_PREGION1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable non-maskable interrupt for PREGION[1].WA event
   type NMIENSET_PREGION1WA_Field_1 is
     (--  Reset value for the field
      Nmienset_Pregion1Wa_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for NMIENSET_PREGION1WA_Field_1 use
     (Nmienset_Pregion1Wa_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable non-maskable interrupt for PREGION[1].RA event
   type NMIENSET_PREGION1RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENSET_PREGION1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable non-maskable interrupt for PREGION[1].RA event
   type NMIENSET_PREGION1RA_Field_1 is
     (--  Reset value for the field
      Nmienset_Pregion1Ra_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for NMIENSET_PREGION1RA_Field_1 use
     (Nmienset_Pregion1Ra_Field_Reset => 0,
      Set => 1);

   --  Enable non-maskable interrupt
   type NMIENSET_Register is record
      --  Write '1' to Enable non-maskable interrupt for REGION[0].WA event
      REGION0WA      : NMIENSET_REGION0WA_Field_1 :=
                        Nmienset_Region0Wa_Field_Reset;
      --  Write '1' to Enable non-maskable interrupt for REGION[0].RA event
      REGION0RA      : NMIENSET_REGION0RA_Field_1 :=
                        Nmienset_Region0Ra_Field_Reset;
      --  Write '1' to Enable non-maskable interrupt for REGION[1].WA event
      REGION1WA      : NMIENSET_REGION1WA_Field_1 :=
                        Nmienset_Region1Wa_Field_Reset;
      --  Write '1' to Enable non-maskable interrupt for REGION[1].RA event
      REGION1RA      : NMIENSET_REGION1RA_Field_1 :=
                        Nmienset_Region1Ra_Field_Reset;
      --  Write '1' to Enable non-maskable interrupt for REGION[2].WA event
      REGION2WA      : NMIENSET_REGION2WA_Field_1 :=
                        Nmienset_Region2Wa_Field_Reset;
      --  Write '1' to Enable non-maskable interrupt for REGION[2].RA event
      REGION2RA      : NMIENSET_REGION2RA_Field_1 :=
                        Nmienset_Region2Ra_Field_Reset;
      --  Write '1' to Enable non-maskable interrupt for REGION[3].WA event
      REGION3WA      : NMIENSET_REGION3WA_Field_1 :=
                        Nmienset_Region3Wa_Field_Reset;
      --  Write '1' to Enable non-maskable interrupt for REGION[3].RA event
      REGION3RA      : NMIENSET_REGION3RA_Field_1 :=
                        Nmienset_Region3Ra_Field_Reset;
      --  unspecified
      Reserved_8_23  : HAL.UInt16 := 16#0#;
      --  Write '1' to Enable non-maskable interrupt for PREGION[0].WA event
      PREGION0WA     : NMIENSET_PREGION0WA_Field_1 :=
                        Nmienset_Pregion0Wa_Field_Reset;
      --  Write '1' to Enable non-maskable interrupt for PREGION[0].RA event
      PREGION0RA     : NMIENSET_PREGION0RA_Field_1 :=
                        Nmienset_Pregion0Ra_Field_Reset;
      --  Write '1' to Enable non-maskable interrupt for PREGION[1].WA event
      PREGION1WA     : NMIENSET_PREGION1WA_Field_1 :=
                        Nmienset_Pregion1Wa_Field_Reset;
      --  Write '1' to Enable non-maskable interrupt for PREGION[1].RA event
      PREGION1RA     : NMIENSET_PREGION1RA_Field_1 :=
                        Nmienset_Pregion1Ra_Field_Reset;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NMIENSET_Register use record
      REGION0WA      at 0 range 0 .. 0;
      REGION0RA      at 0 range 1 .. 1;
      REGION1WA      at 0 range 2 .. 2;
      REGION1RA      at 0 range 3 .. 3;
      REGION2WA      at 0 range 4 .. 4;
      REGION2RA      at 0 range 5 .. 5;
      REGION3WA      at 0 range 6 .. 6;
      REGION3RA      at 0 range 7 .. 7;
      Reserved_8_23  at 0 range 8 .. 23;
      PREGION0WA     at 0 range 24 .. 24;
      PREGION0RA     at 0 range 25 .. 25;
      PREGION1WA     at 0 range 26 .. 26;
      PREGION1RA     at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  Write '1' to Disable non-maskable interrupt for REGION[0].WA event
   type NMIENCLR_REGION0WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENCLR_REGION0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable non-maskable interrupt for REGION[0].WA event
   type NMIENCLR_REGION0WA_Field_1 is
     (--  Reset value for the field
      Nmienclr_Region0Wa_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for NMIENCLR_REGION0WA_Field_1 use
     (Nmienclr_Region0Wa_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable non-maskable interrupt for REGION[0].RA event
   type NMIENCLR_REGION0RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENCLR_REGION0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable non-maskable interrupt for REGION[0].RA event
   type NMIENCLR_REGION0RA_Field_1 is
     (--  Reset value for the field
      Nmienclr_Region0Ra_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for NMIENCLR_REGION0RA_Field_1 use
     (Nmienclr_Region0Ra_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable non-maskable interrupt for REGION[1].WA event
   type NMIENCLR_REGION1WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENCLR_REGION1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable non-maskable interrupt for REGION[1].WA event
   type NMIENCLR_REGION1WA_Field_1 is
     (--  Reset value for the field
      Nmienclr_Region1Wa_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for NMIENCLR_REGION1WA_Field_1 use
     (Nmienclr_Region1Wa_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable non-maskable interrupt for REGION[1].RA event
   type NMIENCLR_REGION1RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENCLR_REGION1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable non-maskable interrupt for REGION[1].RA event
   type NMIENCLR_REGION1RA_Field_1 is
     (--  Reset value for the field
      Nmienclr_Region1Ra_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for NMIENCLR_REGION1RA_Field_1 use
     (Nmienclr_Region1Ra_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable non-maskable interrupt for REGION[2].WA event
   type NMIENCLR_REGION2WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENCLR_REGION2WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable non-maskable interrupt for REGION[2].WA event
   type NMIENCLR_REGION2WA_Field_1 is
     (--  Reset value for the field
      Nmienclr_Region2Wa_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for NMIENCLR_REGION2WA_Field_1 use
     (Nmienclr_Region2Wa_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable non-maskable interrupt for REGION[2].RA event
   type NMIENCLR_REGION2RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENCLR_REGION2RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable non-maskable interrupt for REGION[2].RA event
   type NMIENCLR_REGION2RA_Field_1 is
     (--  Reset value for the field
      Nmienclr_Region2Ra_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for NMIENCLR_REGION2RA_Field_1 use
     (Nmienclr_Region2Ra_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable non-maskable interrupt for REGION[3].WA event
   type NMIENCLR_REGION3WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENCLR_REGION3WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable non-maskable interrupt for REGION[3].WA event
   type NMIENCLR_REGION3WA_Field_1 is
     (--  Reset value for the field
      Nmienclr_Region3Wa_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for NMIENCLR_REGION3WA_Field_1 use
     (Nmienclr_Region3Wa_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable non-maskable interrupt for REGION[3].RA event
   type NMIENCLR_REGION3RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENCLR_REGION3RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable non-maskable interrupt for REGION[3].RA event
   type NMIENCLR_REGION3RA_Field_1 is
     (--  Reset value for the field
      Nmienclr_Region3Ra_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for NMIENCLR_REGION3RA_Field_1 use
     (Nmienclr_Region3Ra_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable non-maskable interrupt for PREGION[0].WA event
   type NMIENCLR_PREGION0WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENCLR_PREGION0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable non-maskable interrupt for PREGION[0].WA event
   type NMIENCLR_PREGION0WA_Field_1 is
     (--  Reset value for the field
      Nmienclr_Pregion0Wa_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for NMIENCLR_PREGION0WA_Field_1 use
     (Nmienclr_Pregion0Wa_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable non-maskable interrupt for PREGION[0].RA event
   type NMIENCLR_PREGION0RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENCLR_PREGION0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable non-maskable interrupt for PREGION[0].RA event
   type NMIENCLR_PREGION0RA_Field_1 is
     (--  Reset value for the field
      Nmienclr_Pregion0Ra_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for NMIENCLR_PREGION0RA_Field_1 use
     (Nmienclr_Pregion0Ra_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable non-maskable interrupt for PREGION[1].WA event
   type NMIENCLR_PREGION1WA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENCLR_PREGION1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable non-maskable interrupt for PREGION[1].WA event
   type NMIENCLR_PREGION1WA_Field_1 is
     (--  Reset value for the field
      Nmienclr_Pregion1Wa_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for NMIENCLR_PREGION1WA_Field_1 use
     (Nmienclr_Pregion1Wa_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable non-maskable interrupt for PREGION[1].RA event
   type NMIENCLR_PREGION1RA_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for NMIENCLR_PREGION1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable non-maskable interrupt for PREGION[1].RA event
   type NMIENCLR_PREGION1RA_Field_1 is
     (--  Reset value for the field
      Nmienclr_Pregion1Ra_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for NMIENCLR_PREGION1RA_Field_1 use
     (Nmienclr_Pregion1Ra_Field_Reset => 0,
      Clear => 1);

   --  Disable non-maskable interrupt
   type NMIENCLR_Register is record
      --  Write '1' to Disable non-maskable interrupt for REGION[0].WA event
      REGION0WA      : NMIENCLR_REGION0WA_Field_1 :=
                        Nmienclr_Region0Wa_Field_Reset;
      --  Write '1' to Disable non-maskable interrupt for REGION[0].RA event
      REGION0RA      : NMIENCLR_REGION0RA_Field_1 :=
                        Nmienclr_Region0Ra_Field_Reset;
      --  Write '1' to Disable non-maskable interrupt for REGION[1].WA event
      REGION1WA      : NMIENCLR_REGION1WA_Field_1 :=
                        Nmienclr_Region1Wa_Field_Reset;
      --  Write '1' to Disable non-maskable interrupt for REGION[1].RA event
      REGION1RA      : NMIENCLR_REGION1RA_Field_1 :=
                        Nmienclr_Region1Ra_Field_Reset;
      --  Write '1' to Disable non-maskable interrupt for REGION[2].WA event
      REGION2WA      : NMIENCLR_REGION2WA_Field_1 :=
                        Nmienclr_Region2Wa_Field_Reset;
      --  Write '1' to Disable non-maskable interrupt for REGION[2].RA event
      REGION2RA      : NMIENCLR_REGION2RA_Field_1 :=
                        Nmienclr_Region2Ra_Field_Reset;
      --  Write '1' to Disable non-maskable interrupt for REGION[3].WA event
      REGION3WA      : NMIENCLR_REGION3WA_Field_1 :=
                        Nmienclr_Region3Wa_Field_Reset;
      --  Write '1' to Disable non-maskable interrupt for REGION[3].RA event
      REGION3RA      : NMIENCLR_REGION3RA_Field_1 :=
                        Nmienclr_Region3Ra_Field_Reset;
      --  unspecified
      Reserved_8_23  : HAL.UInt16 := 16#0#;
      --  Write '1' to Disable non-maskable interrupt for PREGION[0].WA event
      PREGION0WA     : NMIENCLR_PREGION0WA_Field_1 :=
                        Nmienclr_Pregion0Wa_Field_Reset;
      --  Write '1' to Disable non-maskable interrupt for PREGION[0].RA event
      PREGION0RA     : NMIENCLR_PREGION0RA_Field_1 :=
                        Nmienclr_Pregion0Ra_Field_Reset;
      --  Write '1' to Disable non-maskable interrupt for PREGION[1].WA event
      PREGION1WA     : NMIENCLR_PREGION1WA_Field_1 :=
                        Nmienclr_Pregion1Wa_Field_Reset;
      --  Write '1' to Disable non-maskable interrupt for PREGION[1].RA event
      PREGION1RA     : NMIENCLR_PREGION1RA_Field_1 :=
                        Nmienclr_Pregion1Ra_Field_Reset;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NMIENCLR_Register use record
      REGION0WA      at 0 range 0 .. 0;
      REGION0RA      at 0 range 1 .. 1;
      REGION1WA      at 0 range 2 .. 2;
      REGION1RA      at 0 range 3 .. 3;
      REGION2WA      at 0 range 4 .. 4;
      REGION2RA      at 0 range 5 .. 5;
      REGION3WA      at 0 range 6 .. 6;
      REGION3RA      at 0 range 7 .. 7;
      Reserved_8_23  at 0 range 8 .. 23;
      PREGION0WA     at 0 range 24 .. 24;
      PREGION0RA     at 0 range 25 .. 25;
      PREGION1WA     at 0 range 26 .. 26;
      PREGION1RA     at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   -----------------------------------
   -- PERREGION cluster's Registers --
   -----------------------------------

   --  Subregion 0 in region 0 (write '1' to clear)
   type SUBSTATWA_SR0_Field is
     (--  No write access occurred in this subregion
      Noaccess,
      --  Write access(es) occurred in this subregion
      Access_k)
     with Size => 1;
   for SUBSTATWA_SR0_Field use
     (Noaccess => 0,
      Access_k => 1);

   --  SUBSTATWA_PERREGION_SR array
   type SUBSTATWA_PERREGION_SR_Field_Array is array (0 .. 31)
     of SUBSTATWA_SR0_Field
     with Component_Size => 1, Size => 32;

   --  Description cluster[0]: Source of event/interrupt in region 0, write
   --  access detected while corresponding subregion was enabled for watching
   type SUBSTATWA_PERREGION_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SR as a value
            Val : HAL.UInt32;
         when True =>
            --  SR as an array
            Arr : SUBSTATWA_PERREGION_SR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUBSTATWA_PERREGION_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Subregion 0 in region 0 (write '1' to clear)
   type SUBSTATRA_SR0_Field is
     (--  No read access occurred in this subregion
      Noaccess,
      --  Read access(es) occurred in this subregion
      Access_k)
     with Size => 1;
   for SUBSTATRA_SR0_Field use
     (Noaccess => 0,
      Access_k => 1);

   --  SUBSTATRA_PERREGION_SR array
   type SUBSTATRA_PERREGION_SR_Field_Array is array (0 .. 31)
     of SUBSTATRA_SR0_Field
     with Component_Size => 1, Size => 32;

   --  Description cluster[0]: Source of event/interrupt in region 0, read
   --  access detected while corresponding subregion was enabled for watching
   type SUBSTATRA_PERREGION_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SR as a value
            Val : HAL.UInt32;
         when True =>
            --  SR as an array
            Arr : SUBSTATRA_PERREGION_SR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUBSTATRA_PERREGION_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Unspecified
   type PERREGION_Cluster is record
      --  Description cluster[0]: Source of event/interrupt in region 0, write
      --  access detected while corresponding subregion was enabled for
      --  watching
      SUBSTATWA : aliased SUBSTATWA_PERREGION_Register;
      --  Description cluster[0]: Source of event/interrupt in region 0, read
      --  access detected while corresponding subregion was enabled for
      --  watching
      SUBSTATRA : aliased SUBSTATRA_PERREGION_Register;
   end record
     with Size => 64;

   for PERREGION_Cluster use record
      SUBSTATWA at 16#0# range 0 .. 31;
      SUBSTATRA at 16#4# range 0 .. 31;
   end record;

   --  Unspecified
   type PERREGION_Clusters is array (0 .. 1) of PERREGION_Cluster;

   --  Enable/disable write access watch in region[0]
   type REGIONEN_RGN0WA_Field is
     (--  Disable write access watch in this region
      Disable,
      --  Enable write access watch in this region
      Enable)
     with Size => 1;
   for REGIONEN_RGN0WA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable/disable read access watch in region[0]
   type REGIONEN_RGN0RA_Field is
     (--  Disable read access watch in this region
      Disable,
      --  Enable read access watch in this region
      Enable)
     with Size => 1;
   for REGIONEN_RGN0RA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable/disable write access watch in region[1]
   type REGIONEN_RGN1WA_Field is
     (--  Disable write access watch in this region
      Disable,
      --  Enable write access watch in this region
      Enable)
     with Size => 1;
   for REGIONEN_RGN1WA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable/disable read access watch in region[1]
   type REGIONEN_RGN1RA_Field is
     (--  Disable read access watch in this region
      Disable,
      --  Enable read access watch in this region
      Enable)
     with Size => 1;
   for REGIONEN_RGN1RA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable/disable write access watch in region[2]
   type REGIONEN_RGN2WA_Field is
     (--  Disable write access watch in this region
      Disable,
      --  Enable write access watch in this region
      Enable)
     with Size => 1;
   for REGIONEN_RGN2WA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable/disable read access watch in region[2]
   type REGIONEN_RGN2RA_Field is
     (--  Disable read access watch in this region
      Disable,
      --  Enable read access watch in this region
      Enable)
     with Size => 1;
   for REGIONEN_RGN2RA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable/disable write access watch in region[3]
   type REGIONEN_RGN3WA_Field is
     (--  Disable write access watch in this region
      Disable,
      --  Enable write access watch in this region
      Enable)
     with Size => 1;
   for REGIONEN_RGN3WA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable/disable read access watch in region[3]
   type REGIONEN_RGN3RA_Field is
     (--  Disable read access watch in this region
      Disable,
      --  Enable read access watch in this region
      Enable)
     with Size => 1;
   for REGIONEN_RGN3RA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable/disable write access watch in PREGION[0]
   type REGIONEN_PRGN0WA_Field is
     (--  Disable write access watch in this PREGION
      Disable,
      --  Enable write access watch in this PREGION
      Enable)
     with Size => 1;
   for REGIONEN_PRGN0WA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable/disable read access watch in PREGION[0]
   type REGIONEN_PRGN0RA_Field is
     (--  Disable read access watch in this PREGION
      Disable,
      --  Enable read access watch in this PREGION
      Enable)
     with Size => 1;
   for REGIONEN_PRGN0RA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable/disable write access watch in PREGION[1]
   type REGIONEN_PRGN1WA_Field is
     (--  Disable write access watch in this PREGION
      Disable,
      --  Enable write access watch in this PREGION
      Enable)
     with Size => 1;
   for REGIONEN_PRGN1WA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable/disable read access watch in PREGION[1]
   type REGIONEN_PRGN1RA_Field is
     (--  Disable read access watch in this PREGION
      Disable,
      --  Enable read access watch in this PREGION
      Enable)
     with Size => 1;
   for REGIONEN_PRGN1RA_Field use
     (Disable => 0,
      Enable => 1);

   --  Enable/disable regions watch
   type REGIONEN_Register is record
      --  Enable/disable write access watch in region[0]
      RGN0WA         : REGIONEN_RGN0WA_Field := NRF_SVD.MWU.Disable;
      --  Enable/disable read access watch in region[0]
      RGN0RA         : REGIONEN_RGN0RA_Field := NRF_SVD.MWU.Disable;
      --  Enable/disable write access watch in region[1]
      RGN1WA         : REGIONEN_RGN1WA_Field := NRF_SVD.MWU.Disable;
      --  Enable/disable read access watch in region[1]
      RGN1RA         : REGIONEN_RGN1RA_Field := NRF_SVD.MWU.Disable;
      --  Enable/disable write access watch in region[2]
      RGN2WA         : REGIONEN_RGN2WA_Field := NRF_SVD.MWU.Disable;
      --  Enable/disable read access watch in region[2]
      RGN2RA         : REGIONEN_RGN2RA_Field := NRF_SVD.MWU.Disable;
      --  Enable/disable write access watch in region[3]
      RGN3WA         : REGIONEN_RGN3WA_Field := NRF_SVD.MWU.Disable;
      --  Enable/disable read access watch in region[3]
      RGN3RA         : REGIONEN_RGN3RA_Field := NRF_SVD.MWU.Disable;
      --  unspecified
      Reserved_8_23  : HAL.UInt16 := 16#0#;
      --  Enable/disable write access watch in PREGION[0]
      PRGN0WA        : REGIONEN_PRGN0WA_Field := NRF_SVD.MWU.Disable;
      --  Enable/disable read access watch in PREGION[0]
      PRGN0RA        : REGIONEN_PRGN0RA_Field := NRF_SVD.MWU.Disable;
      --  Enable/disable write access watch in PREGION[1]
      PRGN1WA        : REGIONEN_PRGN1WA_Field := NRF_SVD.MWU.Disable;
      --  Enable/disable read access watch in PREGION[1]
      PRGN1RA        : REGIONEN_PRGN1RA_Field := NRF_SVD.MWU.Disable;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for REGIONEN_Register use record
      RGN0WA         at 0 range 0 .. 0;
      RGN0RA         at 0 range 1 .. 1;
      RGN1WA         at 0 range 2 .. 2;
      RGN1RA         at 0 range 3 .. 3;
      RGN2WA         at 0 range 4 .. 4;
      RGN2RA         at 0 range 5 .. 5;
      RGN3WA         at 0 range 6 .. 6;
      RGN3RA         at 0 range 7 .. 7;
      Reserved_8_23  at 0 range 8 .. 23;
      PRGN0WA        at 0 range 24 .. 24;
      PRGN0RA        at 0 range 25 .. 25;
      PRGN1WA        at 0 range 26 .. 26;
      PRGN1RA        at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  Enable write access watch in region[0]
   type REGIONENSET_RGN0WA_Field is
     (--  Write access watch in this region is disabled
      Disabled,
      --  Write access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENSET_RGN0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable write access watch in region[0]
   type REGIONENSET_RGN0WA_Field_1 is
     (--  Reset value for the field
      Regionenset_Rgn0Wa_Field_Reset,
      --  Enable write access watch in this region
      Set)
     with Size => 1;
   for REGIONENSET_RGN0WA_Field_1 use
     (Regionenset_Rgn0Wa_Field_Reset => 0,
      Set => 1);

   --  Enable read access watch in region[0]
   type REGIONENSET_RGN0RA_Field is
     (--  Read access watch in this region is disabled
      Disabled,
      --  Read access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENSET_RGN0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable read access watch in region[0]
   type REGIONENSET_RGN0RA_Field_1 is
     (--  Reset value for the field
      Regionenset_Rgn0Ra_Field_Reset,
      --  Enable read access watch in this region
      Set)
     with Size => 1;
   for REGIONENSET_RGN0RA_Field_1 use
     (Regionenset_Rgn0Ra_Field_Reset => 0,
      Set => 1);

   --  Enable write access watch in region[1]
   type REGIONENSET_RGN1WA_Field is
     (--  Write access watch in this region is disabled
      Disabled,
      --  Write access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENSET_RGN1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable write access watch in region[1]
   type REGIONENSET_RGN1WA_Field_1 is
     (--  Reset value for the field
      Regionenset_Rgn1Wa_Field_Reset,
      --  Enable write access watch in this region
      Set)
     with Size => 1;
   for REGIONENSET_RGN1WA_Field_1 use
     (Regionenset_Rgn1Wa_Field_Reset => 0,
      Set => 1);

   --  Enable read access watch in region[1]
   type REGIONENSET_RGN1RA_Field is
     (--  Read access watch in this region is disabled
      Disabled,
      --  Read access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENSET_RGN1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable read access watch in region[1]
   type REGIONENSET_RGN1RA_Field_1 is
     (--  Reset value for the field
      Regionenset_Rgn1Ra_Field_Reset,
      --  Enable read access watch in this region
      Set)
     with Size => 1;
   for REGIONENSET_RGN1RA_Field_1 use
     (Regionenset_Rgn1Ra_Field_Reset => 0,
      Set => 1);

   --  Enable write access watch in region[2]
   type REGIONENSET_RGN2WA_Field is
     (--  Write access watch in this region is disabled
      Disabled,
      --  Write access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENSET_RGN2WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable write access watch in region[2]
   type REGIONENSET_RGN2WA_Field_1 is
     (--  Reset value for the field
      Regionenset_Rgn2Wa_Field_Reset,
      --  Enable write access watch in this region
      Set)
     with Size => 1;
   for REGIONENSET_RGN2WA_Field_1 use
     (Regionenset_Rgn2Wa_Field_Reset => 0,
      Set => 1);

   --  Enable read access watch in region[2]
   type REGIONENSET_RGN2RA_Field is
     (--  Read access watch in this region is disabled
      Disabled,
      --  Read access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENSET_RGN2RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable read access watch in region[2]
   type REGIONENSET_RGN2RA_Field_1 is
     (--  Reset value for the field
      Regionenset_Rgn2Ra_Field_Reset,
      --  Enable read access watch in this region
      Set)
     with Size => 1;
   for REGIONENSET_RGN2RA_Field_1 use
     (Regionenset_Rgn2Ra_Field_Reset => 0,
      Set => 1);

   --  Enable write access watch in region[3]
   type REGIONENSET_RGN3WA_Field is
     (--  Write access watch in this region is disabled
      Disabled,
      --  Write access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENSET_RGN3WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable write access watch in region[3]
   type REGIONENSET_RGN3WA_Field_1 is
     (--  Reset value for the field
      Regionenset_Rgn3Wa_Field_Reset,
      --  Enable write access watch in this region
      Set)
     with Size => 1;
   for REGIONENSET_RGN3WA_Field_1 use
     (Regionenset_Rgn3Wa_Field_Reset => 0,
      Set => 1);

   --  Enable read access watch in region[3]
   type REGIONENSET_RGN3RA_Field is
     (--  Read access watch in this region is disabled
      Disabled,
      --  Read access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENSET_RGN3RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable read access watch in region[3]
   type REGIONENSET_RGN3RA_Field_1 is
     (--  Reset value for the field
      Regionenset_Rgn3Ra_Field_Reset,
      --  Enable read access watch in this region
      Set)
     with Size => 1;
   for REGIONENSET_RGN3RA_Field_1 use
     (Regionenset_Rgn3Ra_Field_Reset => 0,
      Set => 1);

   --  Enable write access watch in PREGION[0]
   type REGIONENSET_PRGN0WA_Field is
     (--  Write access watch in this PREGION is disabled
      Disabled,
      --  Write access watch in this PREGION is enabled
      Enabled)
     with Size => 1;
   for REGIONENSET_PRGN0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable write access watch in PREGION[0]
   type REGIONENSET_PRGN0WA_Field_1 is
     (--  Reset value for the field
      Regionenset_Prgn0Wa_Field_Reset,
      --  Enable write access watch in this PREGION
      Set)
     with Size => 1;
   for REGIONENSET_PRGN0WA_Field_1 use
     (Regionenset_Prgn0Wa_Field_Reset => 0,
      Set => 1);

   --  Enable read access watch in PREGION[0]
   type REGIONENSET_PRGN0RA_Field is
     (--  Read access watch in this PREGION is disabled
      Disabled,
      --  Read access watch in this PREGION is enabled
      Enabled)
     with Size => 1;
   for REGIONENSET_PRGN0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable read access watch in PREGION[0]
   type REGIONENSET_PRGN0RA_Field_1 is
     (--  Reset value for the field
      Regionenset_Prgn0Ra_Field_Reset,
      --  Enable read access watch in this PREGION
      Set)
     with Size => 1;
   for REGIONENSET_PRGN0RA_Field_1 use
     (Regionenset_Prgn0Ra_Field_Reset => 0,
      Set => 1);

   --  Enable write access watch in PREGION[1]
   type REGIONENSET_PRGN1WA_Field is
     (--  Write access watch in this PREGION is disabled
      Disabled,
      --  Write access watch in this PREGION is enabled
      Enabled)
     with Size => 1;
   for REGIONENSET_PRGN1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable write access watch in PREGION[1]
   type REGIONENSET_PRGN1WA_Field_1 is
     (--  Reset value for the field
      Regionenset_Prgn1Wa_Field_Reset,
      --  Enable write access watch in this PREGION
      Set)
     with Size => 1;
   for REGIONENSET_PRGN1WA_Field_1 use
     (Regionenset_Prgn1Wa_Field_Reset => 0,
      Set => 1);

   --  Enable read access watch in PREGION[1]
   type REGIONENSET_PRGN1RA_Field is
     (--  Read access watch in this PREGION is disabled
      Disabled,
      --  Read access watch in this PREGION is enabled
      Enabled)
     with Size => 1;
   for REGIONENSET_PRGN1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable read access watch in PREGION[1]
   type REGIONENSET_PRGN1RA_Field_1 is
     (--  Reset value for the field
      Regionenset_Prgn1Ra_Field_Reset,
      --  Enable read access watch in this PREGION
      Set)
     with Size => 1;
   for REGIONENSET_PRGN1RA_Field_1 use
     (Regionenset_Prgn1Ra_Field_Reset => 0,
      Set => 1);

   --  Enable regions watch
   type REGIONENSET_Register is record
      --  Enable write access watch in region[0]
      RGN0WA         : REGIONENSET_RGN0WA_Field_1 :=
                        Regionenset_Rgn0Wa_Field_Reset;
      --  Enable read access watch in region[0]
      RGN0RA         : REGIONENSET_RGN0RA_Field_1 :=
                        Regionenset_Rgn0Ra_Field_Reset;
      --  Enable write access watch in region[1]
      RGN1WA         : REGIONENSET_RGN1WA_Field_1 :=
                        Regionenset_Rgn1Wa_Field_Reset;
      --  Enable read access watch in region[1]
      RGN1RA         : REGIONENSET_RGN1RA_Field_1 :=
                        Regionenset_Rgn1Ra_Field_Reset;
      --  Enable write access watch in region[2]
      RGN2WA         : REGIONENSET_RGN2WA_Field_1 :=
                        Regionenset_Rgn2Wa_Field_Reset;
      --  Enable read access watch in region[2]
      RGN2RA         : REGIONENSET_RGN2RA_Field_1 :=
                        Regionenset_Rgn2Ra_Field_Reset;
      --  Enable write access watch in region[3]
      RGN3WA         : REGIONENSET_RGN3WA_Field_1 :=
                        Regionenset_Rgn3Wa_Field_Reset;
      --  Enable read access watch in region[3]
      RGN3RA         : REGIONENSET_RGN3RA_Field_1 :=
                        Regionenset_Rgn3Ra_Field_Reset;
      --  unspecified
      Reserved_8_23  : HAL.UInt16 := 16#0#;
      --  Enable write access watch in PREGION[0]
      PRGN0WA        : REGIONENSET_PRGN0WA_Field_1 :=
                        Regionenset_Prgn0Wa_Field_Reset;
      --  Enable read access watch in PREGION[0]
      PRGN0RA        : REGIONENSET_PRGN0RA_Field_1 :=
                        Regionenset_Prgn0Ra_Field_Reset;
      --  Enable write access watch in PREGION[1]
      PRGN1WA        : REGIONENSET_PRGN1WA_Field_1 :=
                        Regionenset_Prgn1Wa_Field_Reset;
      --  Enable read access watch in PREGION[1]
      PRGN1RA        : REGIONENSET_PRGN1RA_Field_1 :=
                        Regionenset_Prgn1Ra_Field_Reset;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for REGIONENSET_Register use record
      RGN0WA         at 0 range 0 .. 0;
      RGN0RA         at 0 range 1 .. 1;
      RGN1WA         at 0 range 2 .. 2;
      RGN1RA         at 0 range 3 .. 3;
      RGN2WA         at 0 range 4 .. 4;
      RGN2RA         at 0 range 5 .. 5;
      RGN3WA         at 0 range 6 .. 6;
      RGN3RA         at 0 range 7 .. 7;
      Reserved_8_23  at 0 range 8 .. 23;
      PRGN0WA        at 0 range 24 .. 24;
      PRGN0RA        at 0 range 25 .. 25;
      PRGN1WA        at 0 range 26 .. 26;
      PRGN1RA        at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  Disable write access watch in region[0]
   type REGIONENCLR_RGN0WA_Field is
     (--  Write access watch in this region is disabled
      Disabled,
      --  Write access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENCLR_RGN0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable write access watch in region[0]
   type REGIONENCLR_RGN0WA_Field_1 is
     (--  Reset value for the field
      Regionenclr_Rgn0Wa_Field_Reset,
      --  Disable write access watch in this region
      Clear)
     with Size => 1;
   for REGIONENCLR_RGN0WA_Field_1 use
     (Regionenclr_Rgn0Wa_Field_Reset => 0,
      Clear => 1);

   --  Disable read access watch in region[0]
   type REGIONENCLR_RGN0RA_Field is
     (--  Read access watch in this region is disabled
      Disabled,
      --  Read access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENCLR_RGN0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable read access watch in region[0]
   type REGIONENCLR_RGN0RA_Field_1 is
     (--  Reset value for the field
      Regionenclr_Rgn0Ra_Field_Reset,
      --  Disable read access watch in this region
      Clear)
     with Size => 1;
   for REGIONENCLR_RGN0RA_Field_1 use
     (Regionenclr_Rgn0Ra_Field_Reset => 0,
      Clear => 1);

   --  Disable write access watch in region[1]
   type REGIONENCLR_RGN1WA_Field is
     (--  Write access watch in this region is disabled
      Disabled,
      --  Write access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENCLR_RGN1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable write access watch in region[1]
   type REGIONENCLR_RGN1WA_Field_1 is
     (--  Reset value for the field
      Regionenclr_Rgn1Wa_Field_Reset,
      --  Disable write access watch in this region
      Clear)
     with Size => 1;
   for REGIONENCLR_RGN1WA_Field_1 use
     (Regionenclr_Rgn1Wa_Field_Reset => 0,
      Clear => 1);

   --  Disable read access watch in region[1]
   type REGIONENCLR_RGN1RA_Field is
     (--  Read access watch in this region is disabled
      Disabled,
      --  Read access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENCLR_RGN1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable read access watch in region[1]
   type REGIONENCLR_RGN1RA_Field_1 is
     (--  Reset value for the field
      Regionenclr_Rgn1Ra_Field_Reset,
      --  Disable read access watch in this region
      Clear)
     with Size => 1;
   for REGIONENCLR_RGN1RA_Field_1 use
     (Regionenclr_Rgn1Ra_Field_Reset => 0,
      Clear => 1);

   --  Disable write access watch in region[2]
   type REGIONENCLR_RGN2WA_Field is
     (--  Write access watch in this region is disabled
      Disabled,
      --  Write access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENCLR_RGN2WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable write access watch in region[2]
   type REGIONENCLR_RGN2WA_Field_1 is
     (--  Reset value for the field
      Regionenclr_Rgn2Wa_Field_Reset,
      --  Disable write access watch in this region
      Clear)
     with Size => 1;
   for REGIONENCLR_RGN2WA_Field_1 use
     (Regionenclr_Rgn2Wa_Field_Reset => 0,
      Clear => 1);

   --  Disable read access watch in region[2]
   type REGIONENCLR_RGN2RA_Field is
     (--  Read access watch in this region is disabled
      Disabled,
      --  Read access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENCLR_RGN2RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable read access watch in region[2]
   type REGIONENCLR_RGN2RA_Field_1 is
     (--  Reset value for the field
      Regionenclr_Rgn2Ra_Field_Reset,
      --  Disable read access watch in this region
      Clear)
     with Size => 1;
   for REGIONENCLR_RGN2RA_Field_1 use
     (Regionenclr_Rgn2Ra_Field_Reset => 0,
      Clear => 1);

   --  Disable write access watch in region[3]
   type REGIONENCLR_RGN3WA_Field is
     (--  Write access watch in this region is disabled
      Disabled,
      --  Write access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENCLR_RGN3WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable write access watch in region[3]
   type REGIONENCLR_RGN3WA_Field_1 is
     (--  Reset value for the field
      Regionenclr_Rgn3Wa_Field_Reset,
      --  Disable write access watch in this region
      Clear)
     with Size => 1;
   for REGIONENCLR_RGN3WA_Field_1 use
     (Regionenclr_Rgn3Wa_Field_Reset => 0,
      Clear => 1);

   --  Disable read access watch in region[3]
   type REGIONENCLR_RGN3RA_Field is
     (--  Read access watch in this region is disabled
      Disabled,
      --  Read access watch in this region is enabled
      Enabled)
     with Size => 1;
   for REGIONENCLR_RGN3RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable read access watch in region[3]
   type REGIONENCLR_RGN3RA_Field_1 is
     (--  Reset value for the field
      Regionenclr_Rgn3Ra_Field_Reset,
      --  Disable read access watch in this region
      Clear)
     with Size => 1;
   for REGIONENCLR_RGN3RA_Field_1 use
     (Regionenclr_Rgn3Ra_Field_Reset => 0,
      Clear => 1);

   --  Disable write access watch in PREGION[0]
   type REGIONENCLR_PRGN0WA_Field is
     (--  Write access watch in this PREGION is disabled
      Disabled,
      --  Write access watch in this PREGION is enabled
      Enabled)
     with Size => 1;
   for REGIONENCLR_PRGN0WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable write access watch in PREGION[0]
   type REGIONENCLR_PRGN0WA_Field_1 is
     (--  Reset value for the field
      Regionenclr_Prgn0Wa_Field_Reset,
      --  Disable write access watch in this PREGION
      Clear)
     with Size => 1;
   for REGIONENCLR_PRGN0WA_Field_1 use
     (Regionenclr_Prgn0Wa_Field_Reset => 0,
      Clear => 1);

   --  Disable read access watch in PREGION[0]
   type REGIONENCLR_PRGN0RA_Field is
     (--  Read access watch in this PREGION is disabled
      Disabled,
      --  Read access watch in this PREGION is enabled
      Enabled)
     with Size => 1;
   for REGIONENCLR_PRGN0RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable read access watch in PREGION[0]
   type REGIONENCLR_PRGN0RA_Field_1 is
     (--  Reset value for the field
      Regionenclr_Prgn0Ra_Field_Reset,
      --  Disable read access watch in this PREGION
      Clear)
     with Size => 1;
   for REGIONENCLR_PRGN0RA_Field_1 use
     (Regionenclr_Prgn0Ra_Field_Reset => 0,
      Clear => 1);

   --  Disable write access watch in PREGION[1]
   type REGIONENCLR_PRGN1WA_Field is
     (--  Write access watch in this PREGION is disabled
      Disabled,
      --  Write access watch in this PREGION is enabled
      Enabled)
     with Size => 1;
   for REGIONENCLR_PRGN1WA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable write access watch in PREGION[1]
   type REGIONENCLR_PRGN1WA_Field_1 is
     (--  Reset value for the field
      Regionenclr_Prgn1Wa_Field_Reset,
      --  Disable write access watch in this PREGION
      Clear)
     with Size => 1;
   for REGIONENCLR_PRGN1WA_Field_1 use
     (Regionenclr_Prgn1Wa_Field_Reset => 0,
      Clear => 1);

   --  Disable read access watch in PREGION[1]
   type REGIONENCLR_PRGN1RA_Field is
     (--  Read access watch in this PREGION is disabled
      Disabled,
      --  Read access watch in this PREGION is enabled
      Enabled)
     with Size => 1;
   for REGIONENCLR_PRGN1RA_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable read access watch in PREGION[1]
   type REGIONENCLR_PRGN1RA_Field_1 is
     (--  Reset value for the field
      Regionenclr_Prgn1Ra_Field_Reset,
      --  Disable read access watch in this PREGION
      Clear)
     with Size => 1;
   for REGIONENCLR_PRGN1RA_Field_1 use
     (Regionenclr_Prgn1Ra_Field_Reset => 0,
      Clear => 1);

   --  Disable regions watch
   type REGIONENCLR_Register is record
      --  Disable write access watch in region[0]
      RGN0WA         : REGIONENCLR_RGN0WA_Field_1 :=
                        Regionenclr_Rgn0Wa_Field_Reset;
      --  Disable read access watch in region[0]
      RGN0RA         : REGIONENCLR_RGN0RA_Field_1 :=
                        Regionenclr_Rgn0Ra_Field_Reset;
      --  Disable write access watch in region[1]
      RGN1WA         : REGIONENCLR_RGN1WA_Field_1 :=
                        Regionenclr_Rgn1Wa_Field_Reset;
      --  Disable read access watch in region[1]
      RGN1RA         : REGIONENCLR_RGN1RA_Field_1 :=
                        Regionenclr_Rgn1Ra_Field_Reset;
      --  Disable write access watch in region[2]
      RGN2WA         : REGIONENCLR_RGN2WA_Field_1 :=
                        Regionenclr_Rgn2Wa_Field_Reset;
      --  Disable read access watch in region[2]
      RGN2RA         : REGIONENCLR_RGN2RA_Field_1 :=
                        Regionenclr_Rgn2Ra_Field_Reset;
      --  Disable write access watch in region[3]
      RGN3WA         : REGIONENCLR_RGN3WA_Field_1 :=
                        Regionenclr_Rgn3Wa_Field_Reset;
      --  Disable read access watch in region[3]
      RGN3RA         : REGIONENCLR_RGN3RA_Field_1 :=
                        Regionenclr_Rgn3Ra_Field_Reset;
      --  unspecified
      Reserved_8_23  : HAL.UInt16 := 16#0#;
      --  Disable write access watch in PREGION[0]
      PRGN0WA        : REGIONENCLR_PRGN0WA_Field_1 :=
                        Regionenclr_Prgn0Wa_Field_Reset;
      --  Disable read access watch in PREGION[0]
      PRGN0RA        : REGIONENCLR_PRGN0RA_Field_1 :=
                        Regionenclr_Prgn0Ra_Field_Reset;
      --  Disable write access watch in PREGION[1]
      PRGN1WA        : REGIONENCLR_PRGN1WA_Field_1 :=
                        Regionenclr_Prgn1Wa_Field_Reset;
      --  Disable read access watch in PREGION[1]
      PRGN1RA        : REGIONENCLR_PRGN1RA_Field_1 :=
                        Regionenclr_Prgn1Ra_Field_Reset;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for REGIONENCLR_Register use record
      RGN0WA         at 0 range 0 .. 0;
      RGN0RA         at 0 range 1 .. 1;
      RGN1WA         at 0 range 2 .. 2;
      RGN1RA         at 0 range 3 .. 3;
      RGN2WA         at 0 range 4 .. 4;
      RGN2RA         at 0 range 5 .. 5;
      RGN3WA         at 0 range 6 .. 6;
      RGN3RA         at 0 range 7 .. 7;
      Reserved_8_23  at 0 range 8 .. 23;
      PRGN0WA        at 0 range 24 .. 24;
      PRGN0RA        at 0 range 25 .. 25;
      PRGN1WA        at 0 range 26 .. 26;
      PRGN1RA        at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --------------------------------
   -- REGION cluster's Registers --
   --------------------------------

   --  Unspecified
   type REGION_Cluster is record
      --  Description cluster[0]: Start address for region 0
      START : aliased HAL.UInt32;
      --  Description cluster[0]: End address of region 0
      END_k : aliased HAL.UInt32;
   end record
     with Size => 64;

   for REGION_Cluster use record
      START at 16#0# range 0 .. 31;
      END_k at 16#4# range 0 .. 31;
   end record;

   --  Unspecified
   type REGION_Clusters is array (0 .. 3) of REGION_Cluster;

   ---------------------------------
   -- PREGION cluster's Registers --
   ---------------------------------

   --  Include or exclude subregion 0 in region
   type SUBS_SR0_Field is
     (--  Exclude
      Exclude,
      --  Include
      Include)
     with Size => 1;
   for SUBS_SR0_Field use
     (Exclude => 0,
      Include => 1);

   --  SUBS_PREGION_SR array
   type SUBS_PREGION_SR_Field_Array is array (0 .. 31) of SUBS_SR0_Field
     with Component_Size => 1, Size => 32;

   --  Description cluster[0]: Subregions of region 0
   type SUBS_PREGION_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SR as a value
            Val : HAL.UInt32;
         when True =>
            --  SR as an array
            Arr : SUBS_PREGION_SR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUBS_PREGION_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Unspecified
   type PREGION_Cluster is record
      --  Description cluster[0]: Reserved for future use
      START : aliased HAL.UInt32;
      --  Description cluster[0]: Reserved for future use
      END_k : aliased HAL.UInt32;
      --  Description cluster[0]: Subregions of region 0
      SUBS  : aliased SUBS_PREGION_Register;
   end record
     with Size => 96;

   for PREGION_Cluster use record
      START at 16#0# range 0 .. 31;
      END_k at 16#4# range 0 .. 31;
      SUBS  at 16#8# range 0 .. 31;
   end record;

   --  Unspecified
   type PREGION_Clusters is array (0 .. 1) of PREGION_Cluster;

   -----------------
   -- Peripherals --
   -----------------

   --  Memory Watch Unit
   type MWU_Peripheral is record
      --  Unspecified
      EVENTS_REGION  : aliased EVENTS_REGION_Clusters;
      --  Unspecified
      EVENTS_PREGION : aliased EVENTS_PREGION_Clusters;
      --  Enable or disable interrupt
      INTEN          : aliased INTEN_Register;
      --  Enable interrupt
      INTENSET       : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR       : aliased INTENCLR_Register;
      --  Enable or disable non-maskable interrupt
      NMIEN          : aliased NMIEN_Register;
      --  Enable non-maskable interrupt
      NMIENSET       : aliased NMIENSET_Register;
      --  Disable non-maskable interrupt
      NMIENCLR       : aliased NMIENCLR_Register;
      --  Unspecified
      PERREGION      : aliased PERREGION_Clusters;
      --  Enable/disable regions watch
      REGIONEN       : aliased REGIONEN_Register;
      --  Enable regions watch
      REGIONENSET    : aliased REGIONENSET_Register;
      --  Disable regions watch
      REGIONENCLR    : aliased REGIONENCLR_Register;
      --  Unspecified
      REGION         : aliased REGION_Clusters;
      --  Unspecified
      PREGION        : aliased PREGION_Clusters;
   end record
     with Volatile;

   for MWU_Peripheral use record
      EVENTS_REGION  at 16#100# range 0 .. 255;
      EVENTS_PREGION at 16#160# range 0 .. 127;
      INTEN          at 16#300# range 0 .. 31;
      INTENSET       at 16#304# range 0 .. 31;
      INTENCLR       at 16#308# range 0 .. 31;
      NMIEN          at 16#320# range 0 .. 31;
      NMIENSET       at 16#324# range 0 .. 31;
      NMIENCLR       at 16#328# range 0 .. 31;
      PERREGION      at 16#400# range 0 .. 127;
      REGIONEN       at 16#510# range 0 .. 31;
      REGIONENSET    at 16#514# range 0 .. 31;
      REGIONENCLR    at 16#518# range 0 .. 31;
      REGION         at 16#600# range 0 .. 255;
      PREGION        at 16#6C0# range 0 .. 191;
   end record;

   --  Memory Watch Unit
   MWU_Periph : aliased MWU_Peripheral
     with Import, Address => MWU_Base;

end NRF_SVD.MWU;
