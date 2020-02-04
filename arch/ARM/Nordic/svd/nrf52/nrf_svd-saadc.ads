--  Copyright (c) 2010 - 2018, Nordic Semiconductor ASA
--
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  1. Redistributions of source code must retain the above copyright notice,
--  this list of conditions and the following disclaimer.
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
--  5. Any software provided in binary form under this license must not be
--  reverse engineered, decompiled, modified and/or disassembled.
--
--  THIS SOFTWARE IS PROVIDED BY NORDIC SEMICONDUCTOR ASA "AS IS" AND ANY
--  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
--  WARRANTIES OF MERCHANTABILITY, NONINFRINGEMENT, AND FITNESS FOR A
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL NORDIC SEMICONDUCTOR
--  ASA OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--  This spec has been automatically generated from nrf52.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NRF_SVD.SAADC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Start the ADC and prepare the result buffer in RAM
   type TASKS_START_Register is record
      --  Write-only.
      TASKS_START   : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TASKS_START_Register use record
      TASKS_START   at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Take one ADC sample, if scan is enabled all channels are sampled
   type TASKS_SAMPLE_Register is record
      --  Write-only.
      TASKS_SAMPLE  : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TASKS_SAMPLE_Register use record
      TASKS_SAMPLE  at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Stop the ADC and terminate any on-going conversion
   type TASKS_STOP_Register is record
      --  Write-only.
      TASKS_STOP    : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TASKS_STOP_Register use record
      TASKS_STOP    at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Starts offset auto-calibration
   type TASKS_CALIBRATEOFFSET_Register is record
      --  Write-only.
      TASKS_CALIBRATEOFFSET : Boolean := False;
      --  unspecified
      Reserved_1_31         : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TASKS_CALIBRATEOFFSET_Register use record
      TASKS_CALIBRATEOFFSET at 0 range 0 .. 0;
      Reserved_1_31         at 0 range 1 .. 31;
   end record;

   --  The ADC has started
   type EVENTS_STARTED_Register is record
      EVENTS_STARTED : Boolean := False;
      --  unspecified
      Reserved_1_31  : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVENTS_STARTED_Register use record
      EVENTS_STARTED at 0 range 0 .. 0;
      Reserved_1_31  at 0 range 1 .. 31;
   end record;

   --  The ADC has filled up the Result buffer
   type EVENTS_END_Register is record
      EVENTS_END    : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVENTS_END_Register use record
      EVENTS_END    at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  A conversion task has been completed. Depending on the mode, multiple
   --  conversions might be needed for a result to be transferred to RAM.
   type EVENTS_DONE_Register is record
      EVENTS_DONE   : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVENTS_DONE_Register use record
      EVENTS_DONE   at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  A result is ready to get transferred to RAM.
   type EVENTS_RESULTDONE_Register is record
      EVENTS_RESULTDONE : Boolean := False;
      --  unspecified
      Reserved_1_31     : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVENTS_RESULTDONE_Register use record
      EVENTS_RESULTDONE at 0 range 0 .. 0;
      Reserved_1_31     at 0 range 1 .. 31;
   end record;

   --  Calibration is complete
   type EVENTS_CALIBRATEDONE_Register is record
      EVENTS_CALIBRATEDONE : Boolean := False;
      --  unspecified
      Reserved_1_31        : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVENTS_CALIBRATEDONE_Register use record
      EVENTS_CALIBRATEDONE at 0 range 0 .. 0;
      Reserved_1_31        at 0 range 1 .. 31;
   end record;

   --  The ADC has stopped
   type EVENTS_STOPPED_Register is record
      EVENTS_STOPPED : Boolean := False;
      --  unspecified
      Reserved_1_31  : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVENTS_STOPPED_Register use record
      EVENTS_STOPPED at 0 range 0 .. 0;
      Reserved_1_31  at 0 range 1 .. 31;
   end record;

   -----------------------------------
   -- EVENTS_CH cluster's Registers --
   -----------------------------------

   --  Unspecified
   type EVENTS_CH_Cluster is record
      --  Description cluster[0]: Last results is equal or above
      --  CH[0].LIMIT.HIGH
      LIMITH : aliased HAL.UInt32;
      --  Description cluster[0]: Last results is equal or below
      --  CH[0].LIMIT.LOW
      LIMITL : aliased HAL.UInt32;
   end record
     with Size => 64;

   for EVENTS_CH_Cluster use record
      LIMITH at 16#0# range 0 .. 31;
      LIMITL at 16#4# range 0 .. 31;
   end record;

   --  Unspecified
   type EVENTS_CH_Clusters is array (0 .. 7) of EVENTS_CH_Cluster;

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

   --  Enable or disable interrupt for END event
   type INTEN_END_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_END_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for DONE event
   type INTEN_DONE_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_DONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for RESULTDONE event
   type INTEN_RESULTDONE_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_RESULTDONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CALIBRATEDONE event
   type INTEN_CALIBRATEDONE_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CALIBRATEDONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for STOPPED event
   type INTEN_STOPPED_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_STOPPED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[0].LIMITH event
   type INTEN_CH0LIMITH_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH0LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[0].LIMITL event
   type INTEN_CH0LIMITL_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH0LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[1].LIMITH event
   type INTEN_CH1LIMITH_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH1LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[1].LIMITL event
   type INTEN_CH1LIMITL_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH1LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[2].LIMITH event
   type INTEN_CH2LIMITH_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH2LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[2].LIMITL event
   type INTEN_CH2LIMITL_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH2LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[3].LIMITH event
   type INTEN_CH3LIMITH_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH3LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[3].LIMITL event
   type INTEN_CH3LIMITL_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH3LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[4].LIMITH event
   type INTEN_CH4LIMITH_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH4LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[4].LIMITL event
   type INTEN_CH4LIMITL_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH4LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[5].LIMITH event
   type INTEN_CH5LIMITH_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH5LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[5].LIMITL event
   type INTEN_CH5LIMITL_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH5LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[6].LIMITH event
   type INTEN_CH6LIMITH_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH6LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[6].LIMITL event
   type INTEN_CH6LIMITL_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH6LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[7].LIMITH event
   type INTEN_CH7LIMITH_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH7LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for CH[7].LIMITL event
   type INTEN_CH7LIMITL_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_CH7LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt
   type INTEN_Register is record
      --  Enable or disable interrupt for STARTED event
      STARTED        : INTEN_STARTED_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for END event
      END_k          : INTEN_END_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for DONE event
      DONE           : INTEN_DONE_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for RESULTDONE event
      RESULTDONE     : INTEN_RESULTDONE_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CALIBRATEDONE event
      CALIBRATEDONE  : INTEN_CALIBRATEDONE_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for STOPPED event
      STOPPED        : INTEN_STOPPED_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[0].LIMITH event
      CH0LIMITH      : INTEN_CH0LIMITH_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[0].LIMITL event
      CH0LIMITL      : INTEN_CH0LIMITL_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[1].LIMITH event
      CH1LIMITH      : INTEN_CH1LIMITH_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[1].LIMITL event
      CH1LIMITL      : INTEN_CH1LIMITL_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[2].LIMITH event
      CH2LIMITH      : INTEN_CH2LIMITH_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[2].LIMITL event
      CH2LIMITL      : INTEN_CH2LIMITL_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[3].LIMITH event
      CH3LIMITH      : INTEN_CH3LIMITH_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[3].LIMITL event
      CH3LIMITL      : INTEN_CH3LIMITL_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[4].LIMITH event
      CH4LIMITH      : INTEN_CH4LIMITH_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[4].LIMITL event
      CH4LIMITL      : INTEN_CH4LIMITL_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[5].LIMITH event
      CH5LIMITH      : INTEN_CH5LIMITH_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[5].LIMITL event
      CH5LIMITL      : INTEN_CH5LIMITL_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[6].LIMITH event
      CH6LIMITH      : INTEN_CH6LIMITH_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[6].LIMITL event
      CH6LIMITL      : INTEN_CH6LIMITL_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[7].LIMITH event
      CH7LIMITH      : INTEN_CH7LIMITH_Field := NRF_SVD.SAADC.Disabled;
      --  Enable or disable interrupt for CH[7].LIMITL event
      CH7LIMITL      : INTEN_CH7LIMITL_Field := NRF_SVD.SAADC.Disabled;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTEN_Register use record
      STARTED        at 0 range 0 .. 0;
      END_k          at 0 range 1 .. 1;
      DONE           at 0 range 2 .. 2;
      RESULTDONE     at 0 range 3 .. 3;
      CALIBRATEDONE  at 0 range 4 .. 4;
      STOPPED        at 0 range 5 .. 5;
      CH0LIMITH      at 0 range 6 .. 6;
      CH0LIMITL      at 0 range 7 .. 7;
      CH1LIMITH      at 0 range 8 .. 8;
      CH1LIMITL      at 0 range 9 .. 9;
      CH2LIMITH      at 0 range 10 .. 10;
      CH2LIMITL      at 0 range 11 .. 11;
      CH3LIMITH      at 0 range 12 .. 12;
      CH3LIMITL      at 0 range 13 .. 13;
      CH4LIMITH      at 0 range 14 .. 14;
      CH4LIMITL      at 0 range 15 .. 15;
      CH5LIMITH      at 0 range 16 .. 16;
      CH5LIMITL      at 0 range 17 .. 17;
      CH6LIMITH      at 0 range 18 .. 18;
      CH6LIMITL      at 0 range 19 .. 19;
      CH7LIMITH      at 0 range 20 .. 20;
      CH7LIMITL      at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

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

   --  Write '1' to Enable interrupt for DONE event
   type INTENSET_DONE_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_DONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for DONE event
   type INTENSET_DONE_Field_1 is
     (--  Reset value for the field
      Intenset_Done_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_DONE_Field_1 use
     (Intenset_Done_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for RESULTDONE event
   type INTENSET_RESULTDONE_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_RESULTDONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for RESULTDONE event
   type INTENSET_RESULTDONE_Field_1 is
     (--  Reset value for the field
      Intenset_Resultdone_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_RESULTDONE_Field_1 use
     (Intenset_Resultdone_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CALIBRATEDONE event
   type INTENSET_CALIBRATEDONE_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CALIBRATEDONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CALIBRATEDONE event
   type INTENSET_CALIBRATEDONE_Field_1 is
     (--  Reset value for the field
      Intenset_Calibratedone_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CALIBRATEDONE_Field_1 use
     (Intenset_Calibratedone_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for STOPPED event
   type INTENSET_STOPPED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_STOPPED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for STOPPED event
   type INTENSET_STOPPED_Field_1 is
     (--  Reset value for the field
      Intenset_Stopped_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_STOPPED_Field_1 use
     (Intenset_Stopped_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[0].LIMITH event
   type INTENSET_CH0LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH0LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[0].LIMITH event
   type INTENSET_CH0LIMITH_Field_1 is
     (--  Reset value for the field
      Intenset_Ch0Limith_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH0LIMITH_Field_1 use
     (Intenset_Ch0Limith_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[0].LIMITL event
   type INTENSET_CH0LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH0LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[0].LIMITL event
   type INTENSET_CH0LIMITL_Field_1 is
     (--  Reset value for the field
      Intenset_Ch0Limitl_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH0LIMITL_Field_1 use
     (Intenset_Ch0Limitl_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[1].LIMITH event
   type INTENSET_CH1LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH1LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[1].LIMITH event
   type INTENSET_CH1LIMITH_Field_1 is
     (--  Reset value for the field
      Intenset_Ch1Limith_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH1LIMITH_Field_1 use
     (Intenset_Ch1Limith_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[1].LIMITL event
   type INTENSET_CH1LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH1LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[1].LIMITL event
   type INTENSET_CH1LIMITL_Field_1 is
     (--  Reset value for the field
      Intenset_Ch1Limitl_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH1LIMITL_Field_1 use
     (Intenset_Ch1Limitl_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[2].LIMITH event
   type INTENSET_CH2LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH2LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[2].LIMITH event
   type INTENSET_CH2LIMITH_Field_1 is
     (--  Reset value for the field
      Intenset_Ch2Limith_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH2LIMITH_Field_1 use
     (Intenset_Ch2Limith_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[2].LIMITL event
   type INTENSET_CH2LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH2LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[2].LIMITL event
   type INTENSET_CH2LIMITL_Field_1 is
     (--  Reset value for the field
      Intenset_Ch2Limitl_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH2LIMITL_Field_1 use
     (Intenset_Ch2Limitl_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[3].LIMITH event
   type INTENSET_CH3LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH3LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[3].LIMITH event
   type INTENSET_CH3LIMITH_Field_1 is
     (--  Reset value for the field
      Intenset_Ch3Limith_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH3LIMITH_Field_1 use
     (Intenset_Ch3Limith_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[3].LIMITL event
   type INTENSET_CH3LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH3LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[3].LIMITL event
   type INTENSET_CH3LIMITL_Field_1 is
     (--  Reset value for the field
      Intenset_Ch3Limitl_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH3LIMITL_Field_1 use
     (Intenset_Ch3Limitl_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[4].LIMITH event
   type INTENSET_CH4LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH4LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[4].LIMITH event
   type INTENSET_CH4LIMITH_Field_1 is
     (--  Reset value for the field
      Intenset_Ch4Limith_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH4LIMITH_Field_1 use
     (Intenset_Ch4Limith_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[4].LIMITL event
   type INTENSET_CH4LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH4LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[4].LIMITL event
   type INTENSET_CH4LIMITL_Field_1 is
     (--  Reset value for the field
      Intenset_Ch4Limitl_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH4LIMITL_Field_1 use
     (Intenset_Ch4Limitl_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[5].LIMITH event
   type INTENSET_CH5LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH5LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[5].LIMITH event
   type INTENSET_CH5LIMITH_Field_1 is
     (--  Reset value for the field
      Intenset_Ch5Limith_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH5LIMITH_Field_1 use
     (Intenset_Ch5Limith_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[5].LIMITL event
   type INTENSET_CH5LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH5LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[5].LIMITL event
   type INTENSET_CH5LIMITL_Field_1 is
     (--  Reset value for the field
      Intenset_Ch5Limitl_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH5LIMITL_Field_1 use
     (Intenset_Ch5Limitl_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[6].LIMITH event
   type INTENSET_CH6LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH6LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[6].LIMITH event
   type INTENSET_CH6LIMITH_Field_1 is
     (--  Reset value for the field
      Intenset_Ch6Limith_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH6LIMITH_Field_1 use
     (Intenset_Ch6Limith_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[6].LIMITL event
   type INTENSET_CH6LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH6LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[6].LIMITL event
   type INTENSET_CH6LIMITL_Field_1 is
     (--  Reset value for the field
      Intenset_Ch6Limitl_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH6LIMITL_Field_1 use
     (Intenset_Ch6Limitl_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[7].LIMITH event
   type INTENSET_CH7LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH7LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[7].LIMITH event
   type INTENSET_CH7LIMITH_Field_1 is
     (--  Reset value for the field
      Intenset_Ch7Limith_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH7LIMITH_Field_1 use
     (Intenset_Ch7Limith_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CH[7].LIMITL event
   type INTENSET_CH7LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CH7LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CH[7].LIMITL event
   type INTENSET_CH7LIMITL_Field_1 is
     (--  Reset value for the field
      Intenset_Ch7Limitl_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CH7LIMITL_Field_1 use
     (Intenset_Ch7Limitl_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  Write '1' to Enable interrupt for STARTED event
      STARTED        : INTENSET_STARTED_Field_1 :=
                        Intenset_Started_Field_Reset;
      --  Write '1' to Enable interrupt for END event
      END_k          : INTENSET_END_Field_1 := Intenset_End_Field_Reset;
      --  Write '1' to Enable interrupt for DONE event
      DONE           : INTENSET_DONE_Field_1 := Intenset_Done_Field_Reset;
      --  Write '1' to Enable interrupt for RESULTDONE event
      RESULTDONE     : INTENSET_RESULTDONE_Field_1 :=
                        Intenset_Resultdone_Field_Reset;
      --  Write '1' to Enable interrupt for CALIBRATEDONE event
      CALIBRATEDONE  : INTENSET_CALIBRATEDONE_Field_1 :=
                        Intenset_Calibratedone_Field_Reset;
      --  Write '1' to Enable interrupt for STOPPED event
      STOPPED        : INTENSET_STOPPED_Field_1 :=
                        Intenset_Stopped_Field_Reset;
      --  Write '1' to Enable interrupt for CH[0].LIMITH event
      CH0LIMITH      : INTENSET_CH0LIMITH_Field_1 :=
                        Intenset_Ch0Limith_Field_Reset;
      --  Write '1' to Enable interrupt for CH[0].LIMITL event
      CH0LIMITL      : INTENSET_CH0LIMITL_Field_1 :=
                        Intenset_Ch0Limitl_Field_Reset;
      --  Write '1' to Enable interrupt for CH[1].LIMITH event
      CH1LIMITH      : INTENSET_CH1LIMITH_Field_1 :=
                        Intenset_Ch1Limith_Field_Reset;
      --  Write '1' to Enable interrupt for CH[1].LIMITL event
      CH1LIMITL      : INTENSET_CH1LIMITL_Field_1 :=
                        Intenset_Ch1Limitl_Field_Reset;
      --  Write '1' to Enable interrupt for CH[2].LIMITH event
      CH2LIMITH      : INTENSET_CH2LIMITH_Field_1 :=
                        Intenset_Ch2Limith_Field_Reset;
      --  Write '1' to Enable interrupt for CH[2].LIMITL event
      CH2LIMITL      : INTENSET_CH2LIMITL_Field_1 :=
                        Intenset_Ch2Limitl_Field_Reset;
      --  Write '1' to Enable interrupt for CH[3].LIMITH event
      CH3LIMITH      : INTENSET_CH3LIMITH_Field_1 :=
                        Intenset_Ch3Limith_Field_Reset;
      --  Write '1' to Enable interrupt for CH[3].LIMITL event
      CH3LIMITL      : INTENSET_CH3LIMITL_Field_1 :=
                        Intenset_Ch3Limitl_Field_Reset;
      --  Write '1' to Enable interrupt for CH[4].LIMITH event
      CH4LIMITH      : INTENSET_CH4LIMITH_Field_1 :=
                        Intenset_Ch4Limith_Field_Reset;
      --  Write '1' to Enable interrupt for CH[4].LIMITL event
      CH4LIMITL      : INTENSET_CH4LIMITL_Field_1 :=
                        Intenset_Ch4Limitl_Field_Reset;
      --  Write '1' to Enable interrupt for CH[5].LIMITH event
      CH5LIMITH      : INTENSET_CH5LIMITH_Field_1 :=
                        Intenset_Ch5Limith_Field_Reset;
      --  Write '1' to Enable interrupt for CH[5].LIMITL event
      CH5LIMITL      : INTENSET_CH5LIMITL_Field_1 :=
                        Intenset_Ch5Limitl_Field_Reset;
      --  Write '1' to Enable interrupt for CH[6].LIMITH event
      CH6LIMITH      : INTENSET_CH6LIMITH_Field_1 :=
                        Intenset_Ch6Limith_Field_Reset;
      --  Write '1' to Enable interrupt for CH[6].LIMITL event
      CH6LIMITL      : INTENSET_CH6LIMITL_Field_1 :=
                        Intenset_Ch6Limitl_Field_Reset;
      --  Write '1' to Enable interrupt for CH[7].LIMITH event
      CH7LIMITH      : INTENSET_CH7LIMITH_Field_1 :=
                        Intenset_Ch7Limith_Field_Reset;
      --  Write '1' to Enable interrupt for CH[7].LIMITL event
      CH7LIMITL      : INTENSET_CH7LIMITL_Field_1 :=
                        Intenset_Ch7Limitl_Field_Reset;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      STARTED        at 0 range 0 .. 0;
      END_k          at 0 range 1 .. 1;
      DONE           at 0 range 2 .. 2;
      RESULTDONE     at 0 range 3 .. 3;
      CALIBRATEDONE  at 0 range 4 .. 4;
      STOPPED        at 0 range 5 .. 5;
      CH0LIMITH      at 0 range 6 .. 6;
      CH0LIMITL      at 0 range 7 .. 7;
      CH1LIMITH      at 0 range 8 .. 8;
      CH1LIMITL      at 0 range 9 .. 9;
      CH2LIMITH      at 0 range 10 .. 10;
      CH2LIMITL      at 0 range 11 .. 11;
      CH3LIMITH      at 0 range 12 .. 12;
      CH3LIMITL      at 0 range 13 .. 13;
      CH4LIMITH      at 0 range 14 .. 14;
      CH4LIMITL      at 0 range 15 .. 15;
      CH5LIMITH      at 0 range 16 .. 16;
      CH5LIMITL      at 0 range 17 .. 17;
      CH6LIMITH      at 0 range 18 .. 18;
      CH6LIMITL      at 0 range 19 .. 19;
      CH7LIMITH      at 0 range 20 .. 20;
      CH7LIMITL      at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

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

   --  Write '1' to Disable interrupt for DONE event
   type INTENCLR_DONE_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_DONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for DONE event
   type INTENCLR_DONE_Field_1 is
     (--  Reset value for the field
      Intenclr_Done_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_DONE_Field_1 use
     (Intenclr_Done_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for RESULTDONE event
   type INTENCLR_RESULTDONE_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_RESULTDONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for RESULTDONE event
   type INTENCLR_RESULTDONE_Field_1 is
     (--  Reset value for the field
      Intenclr_Resultdone_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_RESULTDONE_Field_1 use
     (Intenclr_Resultdone_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CALIBRATEDONE event
   type INTENCLR_CALIBRATEDONE_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CALIBRATEDONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CALIBRATEDONE event
   type INTENCLR_CALIBRATEDONE_Field_1 is
     (--  Reset value for the field
      Intenclr_Calibratedone_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CALIBRATEDONE_Field_1 use
     (Intenclr_Calibratedone_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for STOPPED event
   type INTENCLR_STOPPED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_STOPPED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for STOPPED event
   type INTENCLR_STOPPED_Field_1 is
     (--  Reset value for the field
      Intenclr_Stopped_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_STOPPED_Field_1 use
     (Intenclr_Stopped_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[0].LIMITH event
   type INTENCLR_CH0LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH0LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[0].LIMITH event
   type INTENCLR_CH0LIMITH_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch0Limith_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH0LIMITH_Field_1 use
     (Intenclr_Ch0Limith_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[0].LIMITL event
   type INTENCLR_CH0LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH0LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[0].LIMITL event
   type INTENCLR_CH0LIMITL_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch0Limitl_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH0LIMITL_Field_1 use
     (Intenclr_Ch0Limitl_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[1].LIMITH event
   type INTENCLR_CH1LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH1LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[1].LIMITH event
   type INTENCLR_CH1LIMITH_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch1Limith_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH1LIMITH_Field_1 use
     (Intenclr_Ch1Limith_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[1].LIMITL event
   type INTENCLR_CH1LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH1LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[1].LIMITL event
   type INTENCLR_CH1LIMITL_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch1Limitl_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH1LIMITL_Field_1 use
     (Intenclr_Ch1Limitl_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[2].LIMITH event
   type INTENCLR_CH2LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH2LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[2].LIMITH event
   type INTENCLR_CH2LIMITH_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch2Limith_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH2LIMITH_Field_1 use
     (Intenclr_Ch2Limith_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[2].LIMITL event
   type INTENCLR_CH2LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH2LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[2].LIMITL event
   type INTENCLR_CH2LIMITL_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch2Limitl_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH2LIMITL_Field_1 use
     (Intenclr_Ch2Limitl_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[3].LIMITH event
   type INTENCLR_CH3LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH3LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[3].LIMITH event
   type INTENCLR_CH3LIMITH_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch3Limith_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH3LIMITH_Field_1 use
     (Intenclr_Ch3Limith_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[3].LIMITL event
   type INTENCLR_CH3LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH3LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[3].LIMITL event
   type INTENCLR_CH3LIMITL_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch3Limitl_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH3LIMITL_Field_1 use
     (Intenclr_Ch3Limitl_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[4].LIMITH event
   type INTENCLR_CH4LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH4LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[4].LIMITH event
   type INTENCLR_CH4LIMITH_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch4Limith_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH4LIMITH_Field_1 use
     (Intenclr_Ch4Limith_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[4].LIMITL event
   type INTENCLR_CH4LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH4LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[4].LIMITL event
   type INTENCLR_CH4LIMITL_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch4Limitl_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH4LIMITL_Field_1 use
     (Intenclr_Ch4Limitl_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[5].LIMITH event
   type INTENCLR_CH5LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH5LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[5].LIMITH event
   type INTENCLR_CH5LIMITH_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch5Limith_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH5LIMITH_Field_1 use
     (Intenclr_Ch5Limith_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[5].LIMITL event
   type INTENCLR_CH5LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH5LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[5].LIMITL event
   type INTENCLR_CH5LIMITL_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch5Limitl_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH5LIMITL_Field_1 use
     (Intenclr_Ch5Limitl_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[6].LIMITH event
   type INTENCLR_CH6LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH6LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[6].LIMITH event
   type INTENCLR_CH6LIMITH_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch6Limith_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH6LIMITH_Field_1 use
     (Intenclr_Ch6Limith_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[6].LIMITL event
   type INTENCLR_CH6LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH6LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[6].LIMITL event
   type INTENCLR_CH6LIMITL_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch6Limitl_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH6LIMITL_Field_1 use
     (Intenclr_Ch6Limitl_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[7].LIMITH event
   type INTENCLR_CH7LIMITH_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH7LIMITH_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[7].LIMITH event
   type INTENCLR_CH7LIMITH_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch7Limith_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH7LIMITH_Field_1 use
     (Intenclr_Ch7Limith_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CH[7].LIMITL event
   type INTENCLR_CH7LIMITL_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CH7LIMITL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CH[7].LIMITL event
   type INTENCLR_CH7LIMITL_Field_1 is
     (--  Reset value for the field
      Intenclr_Ch7Limitl_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CH7LIMITL_Field_1 use
     (Intenclr_Ch7Limitl_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  Write '1' to Disable interrupt for STARTED event
      STARTED        : INTENCLR_STARTED_Field_1 :=
                        Intenclr_Started_Field_Reset;
      --  Write '1' to Disable interrupt for END event
      END_k          : INTENCLR_END_Field_1 := Intenclr_End_Field_Reset;
      --  Write '1' to Disable interrupt for DONE event
      DONE           : INTENCLR_DONE_Field_1 := Intenclr_Done_Field_Reset;
      --  Write '1' to Disable interrupt for RESULTDONE event
      RESULTDONE     : INTENCLR_RESULTDONE_Field_1 :=
                        Intenclr_Resultdone_Field_Reset;
      --  Write '1' to Disable interrupt for CALIBRATEDONE event
      CALIBRATEDONE  : INTENCLR_CALIBRATEDONE_Field_1 :=
                        Intenclr_Calibratedone_Field_Reset;
      --  Write '1' to Disable interrupt for STOPPED event
      STOPPED        : INTENCLR_STOPPED_Field_1 :=
                        Intenclr_Stopped_Field_Reset;
      --  Write '1' to Disable interrupt for CH[0].LIMITH event
      CH0LIMITH      : INTENCLR_CH0LIMITH_Field_1 :=
                        Intenclr_Ch0Limith_Field_Reset;
      --  Write '1' to Disable interrupt for CH[0].LIMITL event
      CH0LIMITL      : INTENCLR_CH0LIMITL_Field_1 :=
                        Intenclr_Ch0Limitl_Field_Reset;
      --  Write '1' to Disable interrupt for CH[1].LIMITH event
      CH1LIMITH      : INTENCLR_CH1LIMITH_Field_1 :=
                        Intenclr_Ch1Limith_Field_Reset;
      --  Write '1' to Disable interrupt for CH[1].LIMITL event
      CH1LIMITL      : INTENCLR_CH1LIMITL_Field_1 :=
                        Intenclr_Ch1Limitl_Field_Reset;
      --  Write '1' to Disable interrupt for CH[2].LIMITH event
      CH2LIMITH      : INTENCLR_CH2LIMITH_Field_1 :=
                        Intenclr_Ch2Limith_Field_Reset;
      --  Write '1' to Disable interrupt for CH[2].LIMITL event
      CH2LIMITL      : INTENCLR_CH2LIMITL_Field_1 :=
                        Intenclr_Ch2Limitl_Field_Reset;
      --  Write '1' to Disable interrupt for CH[3].LIMITH event
      CH3LIMITH      : INTENCLR_CH3LIMITH_Field_1 :=
                        Intenclr_Ch3Limith_Field_Reset;
      --  Write '1' to Disable interrupt for CH[3].LIMITL event
      CH3LIMITL      : INTENCLR_CH3LIMITL_Field_1 :=
                        Intenclr_Ch3Limitl_Field_Reset;
      --  Write '1' to Disable interrupt for CH[4].LIMITH event
      CH4LIMITH      : INTENCLR_CH4LIMITH_Field_1 :=
                        Intenclr_Ch4Limith_Field_Reset;
      --  Write '1' to Disable interrupt for CH[4].LIMITL event
      CH4LIMITL      : INTENCLR_CH4LIMITL_Field_1 :=
                        Intenclr_Ch4Limitl_Field_Reset;
      --  Write '1' to Disable interrupt for CH[5].LIMITH event
      CH5LIMITH      : INTENCLR_CH5LIMITH_Field_1 :=
                        Intenclr_Ch5Limith_Field_Reset;
      --  Write '1' to Disable interrupt for CH[5].LIMITL event
      CH5LIMITL      : INTENCLR_CH5LIMITL_Field_1 :=
                        Intenclr_Ch5Limitl_Field_Reset;
      --  Write '1' to Disable interrupt for CH[6].LIMITH event
      CH6LIMITH      : INTENCLR_CH6LIMITH_Field_1 :=
                        Intenclr_Ch6Limith_Field_Reset;
      --  Write '1' to Disable interrupt for CH[6].LIMITL event
      CH6LIMITL      : INTENCLR_CH6LIMITL_Field_1 :=
                        Intenclr_Ch6Limitl_Field_Reset;
      --  Write '1' to Disable interrupt for CH[7].LIMITH event
      CH7LIMITH      : INTENCLR_CH7LIMITH_Field_1 :=
                        Intenclr_Ch7Limith_Field_Reset;
      --  Write '1' to Disable interrupt for CH[7].LIMITL event
      CH7LIMITL      : INTENCLR_CH7LIMITL_Field_1 :=
                        Intenclr_Ch7Limitl_Field_Reset;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      STARTED        at 0 range 0 .. 0;
      END_k          at 0 range 1 .. 1;
      DONE           at 0 range 2 .. 2;
      RESULTDONE     at 0 range 3 .. 3;
      CALIBRATEDONE  at 0 range 4 .. 4;
      STOPPED        at 0 range 5 .. 5;
      CH0LIMITH      at 0 range 6 .. 6;
      CH0LIMITL      at 0 range 7 .. 7;
      CH1LIMITH      at 0 range 8 .. 8;
      CH1LIMITL      at 0 range 9 .. 9;
      CH2LIMITH      at 0 range 10 .. 10;
      CH2LIMITL      at 0 range 11 .. 11;
      CH3LIMITH      at 0 range 12 .. 12;
      CH3LIMITL      at 0 range 13 .. 13;
      CH4LIMITH      at 0 range 14 .. 14;
      CH4LIMITL      at 0 range 15 .. 15;
      CH5LIMITH      at 0 range 16 .. 16;
      CH5LIMITL      at 0 range 17 .. 17;
      CH6LIMITH      at 0 range 18 .. 18;
      CH6LIMITL      at 0 range 19 .. 19;
      CH7LIMITH      at 0 range 20 .. 20;
      CH7LIMITL      at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  Status
   type STATUS_STATUS_Field is
     (--  ADC is ready. No on-going conversion.
      Ready,
      --  ADC is busy. Conversion in progress.
      Busy)
     with Size => 1;
   for STATUS_STATUS_Field use
     (Ready => 0,
      Busy => 1);

   --  Status
   type STATUS_Register is record
      --  Read-only. Status
      STATUS        : STATUS_STATUS_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for STATUS_Register use record
      STATUS        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Enable or disable ADC
   type ENABLE_ENABLE_Field is
     (--  Disable ADC
      Disabled,
      --  Enable ADC
      Enabled)
     with Size => 1;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable ADC
   type ENABLE_Register is record
      --  Enable or disable ADC
      ENABLE        : ENABLE_ENABLE_Field := NRF_SVD.SAADC.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   ----------------------------
   -- CH cluster's Registers --
   ----------------------------

   --  Analog positive input channel
   type PSELP_PSELP_Field is
     (--  Not connected
      Nc,
      --  AIN0
      Analoginput0,
      --  AIN1
      Analoginput1,
      --  AIN2
      Analoginput2,
      --  AIN3
      Analoginput3,
      --  AIN4
      Analoginput4,
      --  AIN5
      Analoginput5,
      --  AIN6
      Analoginput6,
      --  AIN7
      Analoginput7,
      --  VDD
      Vdd)
     with Size => 5;
   for PSELP_PSELP_Field use
     (Nc => 0,
      Analoginput0 => 1,
      Analoginput1 => 2,
      Analoginput2 => 3,
      Analoginput3 => 4,
      Analoginput4 => 5,
      Analoginput5 => 6,
      Analoginput6 => 7,
      Analoginput7 => 8,
      Vdd => 9);

   --  Description cluster[0]: Input positive pin selection for CH[0]
   type PSELP_CH_Register is record
      --  Analog positive input channel
      PSELP         : PSELP_PSELP_Field := NRF_SVD.SAADC.Nc;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PSELP_CH_Register use record
      PSELP         at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Analog negative input, enables differential channel
   type PSELN_PSELN_Field is
     (--  Not connected
      Nc,
      --  AIN0
      Analoginput0,
      --  AIN1
      Analoginput1,
      --  AIN2
      Analoginput2,
      --  AIN3
      Analoginput3,
      --  AIN4
      Analoginput4,
      --  AIN5
      Analoginput5,
      --  AIN6
      Analoginput6,
      --  AIN7
      Analoginput7,
      --  VDD
      Vdd)
     with Size => 5;
   for PSELN_PSELN_Field use
     (Nc => 0,
      Analoginput0 => 1,
      Analoginput1 => 2,
      Analoginput2 => 3,
      Analoginput3 => 4,
      Analoginput4 => 5,
      Analoginput5 => 6,
      Analoginput6 => 7,
      Analoginput7 => 8,
      Vdd => 9);

   --  Description cluster[0]: Input negative pin selection for CH[0]
   type PSELN_CH_Register is record
      --  Analog negative input, enables differential channel
      PSELN         : PSELN_PSELN_Field := NRF_SVD.SAADC.Nc;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PSELN_CH_Register use record
      PSELN         at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Positive channel resistor control
   type CONFIG_RESP_Field is
     (--  Bypass resistor ladder
      Bypass,
      --  Pull-down to GND
      Pulldown,
      --  Pull-up to VDD
      Pullup,
      --  Set input at VDD/2
      Vdd1_2)
     with Size => 2;
   for CONFIG_RESP_Field use
     (Bypass => 0,
      Pulldown => 1,
      Pullup => 2,
      Vdd1_2 => 3);

   --  Negative channel resistor control
   type CONFIG_RESN_Field is
     (--  Bypass resistor ladder
      Bypass,
      --  Pull-down to GND
      Pulldown,
      --  Pull-up to VDD
      Pullup,
      --  Set input at VDD/2
      Vdd1_2)
     with Size => 2;
   for CONFIG_RESN_Field use
     (Bypass => 0,
      Pulldown => 1,
      Pullup => 2,
      Vdd1_2 => 3);

   --  Gain control
   type CONFIG_GAIN_Field is
     (--  1/6
      Gain1_6,
      --  1/5
      Gain1_5,
      --  1/4
      Gain1_4,
      --  1/3
      Gain1_3,
      --  1/2
      Gain1_2,
      --  1
      Gain1,
      --  2
      Gain2,
      --  4
      Gain4)
     with Size => 3;
   for CONFIG_GAIN_Field use
     (Gain1_6 => 0,
      Gain1_5 => 1,
      Gain1_4 => 2,
      Gain1_3 => 3,
      Gain1_2 => 4,
      Gain1 => 5,
      Gain2 => 6,
      Gain4 => 7);

   --  Reference control
   type CONFIG_REFSEL_Field is
     (--  Internal reference (0.6 V)
      Internal,
      --  VDD/4 as reference
      Vdd1_4)
     with Size => 1;
   for CONFIG_REFSEL_Field use
     (Internal => 0,
      Vdd1_4 => 1);

   --  Acquisition time, the time the ADC uses to sample the input voltage
   type CONFIG_TACQ_Field is
     (--  3 us
      Val_3US,
      --  5 us
      Val_5US,
      --  10 us
      Val_10US,
      --  15 us
      Val_15US,
      --  20 us
      Val_20US,
      --  40 us
      Val_40US)
     with Size => 3;
   for CONFIG_TACQ_Field use
     (Val_3US => 0,
      Val_5US => 1,
      Val_10US => 2,
      Val_15US => 3,
      Val_20US => 4,
      Val_40US => 5);

   --  Enable differential mode
   type CONFIG_MODE_Field is
     (--  Single ended, PSELN will be ignored, negative input to ADC shorted to GND
      Se,
      --  Differential
      Diff)
     with Size => 1;
   for CONFIG_MODE_Field use
     (Se => 0,
      Diff => 1);

   --  Enable burst mode
   type CONFIG_BURST_Field is
     (--  Burst mode is disabled (normal operation)
      Disabled,
      --  Burst mode is enabled. SAADC takes 2^OVERSAMPLE number of samples as fast
--  as it can, and sends the average to Data RAM.
      Enabled)
     with Size => 1;
   for CONFIG_BURST_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Description cluster[0]: Input configuration for CH[0]
   type CONFIG_CH_Register is record
      --  Positive channel resistor control
      RESP           : CONFIG_RESP_Field := NRF_SVD.SAADC.Bypass;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Negative channel resistor control
      RESN           : CONFIG_RESN_Field := NRF_SVD.SAADC.Bypass;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Gain control
      GAIN           : CONFIG_GAIN_Field := NRF_SVD.SAADC.Gain1_6;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  Reference control
      REFSEL         : CONFIG_REFSEL_Field := NRF_SVD.SAADC.Internal;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  Acquisition time, the time the ADC uses to sample the input voltage
      TACQ           : CONFIG_TACQ_Field := NRF_SVD.SAADC.Val_10US;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  Enable differential mode
      MODE           : CONFIG_MODE_Field := NRF_SVD.SAADC.Se;
      --  unspecified
      Reserved_21_23 : HAL.UInt3 := 16#0#;
      --  Enable burst mode
      BURST          : CONFIG_BURST_Field := NRF_SVD.SAADC.Disabled;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_CH_Register use record
      RESP           at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      RESN           at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      GAIN           at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      REFSEL         at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      TACQ           at 0 range 16 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      MODE           at 0 range 20 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      BURST          at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype LIMIT_CH_LOW_Field is HAL.UInt16;
   subtype LIMIT_CH_HIGH_Field is HAL.UInt16;

   --  Description cluster[0]: High/low limits for event monitoring a channel
   type LIMIT_CH_Register is record
      --  Low level limit
      LOW  : LIMIT_CH_LOW_Field := 16#8000#;
      --  High level limit
      HIGH : LIMIT_CH_HIGH_Field := 16#7FFF#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LIMIT_CH_Register use record
      LOW  at 0 range 0 .. 15;
      HIGH at 0 range 16 .. 31;
   end record;

   --  Unspecified
   type CH_Cluster is record
      --  Description cluster[0]: Input positive pin selection for CH[0]
      PSELP  : aliased PSELP_CH_Register;
      --  Description cluster[0]: Input negative pin selection for CH[0]
      PSELN  : aliased PSELN_CH_Register;
      --  Description cluster[0]: Input configuration for CH[0]
      CONFIG : aliased CONFIG_CH_Register;
      --  Description cluster[0]: High/low limits for event monitoring a
      --  channel
      LIMIT  : aliased LIMIT_CH_Register;
   end record
     with Size => 128;

   for CH_Cluster use record
      PSELP  at 16#0# range 0 .. 31;
      PSELN  at 16#4# range 0 .. 31;
      CONFIG at 16#8# range 0 .. 31;
      LIMIT  at 16#C# range 0 .. 31;
   end record;

   --  Unspecified
   type CH_Clusters is array (0 .. 7) of CH_Cluster;

   --  Set the resolution
   type RESOLUTION_VAL_Field is
     (--  8 bit
      Val_8BIT,
      --  10 bit
      Val_10BIT,
      --  12 bit
      Val_12BIT,
      --  14 bit
      Val_14BIT)
     with Size => 3;
   for RESOLUTION_VAL_Field use
     (Val_8BIT => 0,
      Val_10BIT => 1,
      Val_12BIT => 2,
      Val_14BIT => 3);

   --  Resolution configuration
   type RESOLUTION_Register is record
      --  Set the resolution
      VAL           : RESOLUTION_VAL_Field := NRF_SVD.SAADC.Val_10BIT;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RESOLUTION_Register use record
      VAL           at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Oversample control
   type OVERSAMPLE_OVERSAMPLE_Field is
     (--  Bypass oversampling
      Bypass,
      --  Oversample 2x
      Over2X,
      --  Oversample 4x
      Over4X,
      --  Oversample 8x
      Over8X,
      --  Oversample 16x
      Over16X,
      --  Oversample 32x
      Over32X,
      --  Oversample 64x
      Over64X,
      --  Oversample 128x
      Over128X,
      --  Oversample 256x
      Over256X)
     with Size => 4;
   for OVERSAMPLE_OVERSAMPLE_Field use
     (Bypass => 0,
      Over2X => 1,
      Over4X => 2,
      Over8X => 3,
      Over16X => 4,
      Over32X => 5,
      Over64X => 6,
      Over128X => 7,
      Over256X => 8);

   --  Oversampling configuration. OVERSAMPLE should not be combined with SCAN.
   --  The RESOLUTION is applied before averaging, thus for high OVERSAMPLE a
   --  higher RESOLUTION should be used.
   type OVERSAMPLE_Register is record
      --  Oversample control
      OVERSAMPLE    : OVERSAMPLE_OVERSAMPLE_Field := NRF_SVD.SAADC.Bypass;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OVERSAMPLE_Register use record
      OVERSAMPLE    at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype SAMPLERATE_CC_Field is HAL.UInt11;

   --  Select mode for sample rate control
   type SAMPLERATE_MODE_Field is
     (--  Rate is controlled from SAMPLE task
      Task_k,
      --  Rate is controlled from local timer (use CC to control the rate)
      Timers)
     with Size => 1;
   for SAMPLERATE_MODE_Field use
     (Task_k => 0,
      Timers => 1);

   --  Controls normal or continuous sample rate
   type SAMPLERATE_Register is record
      --  Capture and compare value. Sample rate is 16 MHz/CC
      CC             : SAMPLERATE_CC_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  Select mode for sample rate control
      MODE           : SAMPLERATE_MODE_Field := NRF_SVD.SAADC.Task_k;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SAMPLERATE_Register use record
      CC             at 0 range 0 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      MODE           at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --------------------------------
   -- RESULT cluster's Registers --
   --------------------------------

   subtype MAXCNT_RESULT_MAXCNT_Field is HAL.UInt15;

   --  Maximum number of buffer words to transfer
   type MAXCNT_RESULT_Register is record
      --  Maximum number of buffer words to transfer
      MAXCNT         : MAXCNT_RESULT_MAXCNT_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAXCNT_RESULT_Register use record
      MAXCNT         at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype AMOUNT_RESULT_AMOUNT_Field is HAL.UInt15;

   --  Number of buffer words transferred since last START
   type AMOUNT_RESULT_Register is record
      --  Read-only. Number of buffer words transferred since last START. This
      --  register can be read after an END or STOPPED event.
      AMOUNT         : AMOUNT_RESULT_AMOUNT_Field;
      --  unspecified
      Reserved_15_31 : HAL.UInt17;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AMOUNT_RESULT_Register use record
      AMOUNT         at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  RESULT EasyDMA channel
   type RESULT_Cluster is record
      --  Data pointer
      PTR    : aliased HAL.UInt32;
      --  Maximum number of buffer words to transfer
      MAXCNT : aliased MAXCNT_RESULT_Register;
      --  Number of buffer words transferred since last START
      AMOUNT : aliased AMOUNT_RESULT_Register;
   end record
     with Size => 96;

   for RESULT_Cluster use record
      PTR    at 16#0# range 0 .. 31;
      MAXCNT at 16#4# range 0 .. 31;
      AMOUNT at 16#8# range 0 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Analog to Digital Converter
   type SAADC_Peripheral is record
      --  Start the ADC and prepare the result buffer in RAM
      TASKS_START           : aliased TASKS_START_Register;
      --  Take one ADC sample, if scan is enabled all channels are sampled
      TASKS_SAMPLE          : aliased TASKS_SAMPLE_Register;
      --  Stop the ADC and terminate any on-going conversion
      TASKS_STOP            : aliased TASKS_STOP_Register;
      --  Starts offset auto-calibration
      TASKS_CALIBRATEOFFSET : aliased TASKS_CALIBRATEOFFSET_Register;
      --  The ADC has started
      EVENTS_STARTED        : aliased EVENTS_STARTED_Register;
      --  The ADC has filled up the Result buffer
      EVENTS_END            : aliased EVENTS_END_Register;
      --  A conversion task has been completed. Depending on the mode, multiple
      --  conversions might be needed for a result to be transferred to RAM.
      EVENTS_DONE           : aliased EVENTS_DONE_Register;
      --  A result is ready to get transferred to RAM.
      EVENTS_RESULTDONE     : aliased EVENTS_RESULTDONE_Register;
      --  Calibration is complete
      EVENTS_CALIBRATEDONE  : aliased EVENTS_CALIBRATEDONE_Register;
      --  The ADC has stopped
      EVENTS_STOPPED        : aliased EVENTS_STOPPED_Register;
      --  Unspecified
      EVENTS_CH             : aliased EVENTS_CH_Clusters;
      --  Enable or disable interrupt
      INTEN                 : aliased INTEN_Register;
      --  Enable interrupt
      INTENSET              : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR              : aliased INTENCLR_Register;
      --  Status
      STATUS                : aliased STATUS_Register;
      --  Enable or disable ADC
      ENABLE                : aliased ENABLE_Register;
      --  Unspecified
      CH                    : aliased CH_Clusters;
      --  Resolution configuration
      RESOLUTION            : aliased RESOLUTION_Register;
      --  Oversampling configuration. OVERSAMPLE should not be combined with
      --  SCAN. The RESOLUTION is applied before averaging, thus for high
      --  OVERSAMPLE a higher RESOLUTION should be used.
      OVERSAMPLE            : aliased OVERSAMPLE_Register;
      --  Controls normal or continuous sample rate
      SAMPLERATE            : aliased SAMPLERATE_Register;
      --  RESULT EasyDMA channel
      RESULT                : aliased RESULT_Cluster;
   end record
     with Volatile;

   for SAADC_Peripheral use record
      TASKS_START           at 16#0# range 0 .. 31;
      TASKS_SAMPLE          at 16#4# range 0 .. 31;
      TASKS_STOP            at 16#8# range 0 .. 31;
      TASKS_CALIBRATEOFFSET at 16#C# range 0 .. 31;
      EVENTS_STARTED        at 16#100# range 0 .. 31;
      EVENTS_END            at 16#104# range 0 .. 31;
      EVENTS_DONE           at 16#108# range 0 .. 31;
      EVENTS_RESULTDONE     at 16#10C# range 0 .. 31;
      EVENTS_CALIBRATEDONE  at 16#110# range 0 .. 31;
      EVENTS_STOPPED        at 16#114# range 0 .. 31;
      EVENTS_CH             at 16#118# range 0 .. 511;
      INTEN                 at 16#300# range 0 .. 31;
      INTENSET              at 16#304# range 0 .. 31;
      INTENCLR              at 16#308# range 0 .. 31;
      STATUS                at 16#400# range 0 .. 31;
      ENABLE                at 16#500# range 0 .. 31;
      CH                    at 16#510# range 0 .. 1023;
      RESOLUTION            at 16#5F0# range 0 .. 31;
      OVERSAMPLE            at 16#5F4# range 0 .. 31;
      SAMPLERATE            at 16#5F8# range 0 .. 31;
      RESULT                at 16#62C# range 0 .. 95;
   end record;

   --  Analog to Digital Converter
   SAADC_Periph : aliased SAADC_Peripheral
     with Import, Address => SAADC_Base;

end NRF_SVD.SAADC;
