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

package NRF_SVD.QDEC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Shortcut between REPORTRDY event and READCLRACC task
   type SHORTS_REPORTRDY_READCLRACC_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_REPORTRDY_READCLRACC_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between SAMPLERDY event and STOP task
   type SHORTS_SAMPLERDY_STOP_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_SAMPLERDY_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between REPORTRDY event and RDCLRACC task
   type SHORTS_REPORTRDY_RDCLRACC_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_REPORTRDY_RDCLRACC_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between REPORTRDY event and STOP task
   type SHORTS_REPORTRDY_STOP_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_REPORTRDY_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between DBLRDY event and RDCLRDBL task
   type SHORTS_DBLRDY_RDCLRDBL_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_DBLRDY_RDCLRDBL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between DBLRDY event and STOP task
   type SHORTS_DBLRDY_STOP_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_DBLRDY_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between SAMPLERDY event and READCLRACC task
   type SHORTS_SAMPLERDY_READCLRACC_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_SAMPLERDY_READCLRACC_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut register
   type SHORTS_Register is record
      --  Shortcut between REPORTRDY event and READCLRACC task
      REPORTRDY_READCLRACC : SHORTS_REPORTRDY_READCLRACC_Field :=
                              NRF_SVD.QDEC.Disabled;
      --  Shortcut between SAMPLERDY event and STOP task
      SAMPLERDY_STOP       : SHORTS_SAMPLERDY_STOP_Field :=
                              NRF_SVD.QDEC.Disabled;
      --  Shortcut between REPORTRDY event and RDCLRACC task
      REPORTRDY_RDCLRACC   : SHORTS_REPORTRDY_RDCLRACC_Field :=
                              NRF_SVD.QDEC.Disabled;
      --  Shortcut between REPORTRDY event and STOP task
      REPORTRDY_STOP       : SHORTS_REPORTRDY_STOP_Field :=
                              NRF_SVD.QDEC.Disabled;
      --  Shortcut between DBLRDY event and RDCLRDBL task
      DBLRDY_RDCLRDBL      : SHORTS_DBLRDY_RDCLRDBL_Field :=
                              NRF_SVD.QDEC.Disabled;
      --  Shortcut between DBLRDY event and STOP task
      DBLRDY_STOP          : SHORTS_DBLRDY_STOP_Field :=
                              NRF_SVD.QDEC.Disabled;
      --  Shortcut between SAMPLERDY event and READCLRACC task
      SAMPLERDY_READCLRACC : SHORTS_SAMPLERDY_READCLRACC_Field :=
                              NRF_SVD.QDEC.Disabled;
      --  unspecified
      Reserved_7_31        : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      REPORTRDY_READCLRACC at 0 range 0 .. 0;
      SAMPLERDY_STOP       at 0 range 1 .. 1;
      REPORTRDY_RDCLRACC   at 0 range 2 .. 2;
      REPORTRDY_STOP       at 0 range 3 .. 3;
      DBLRDY_RDCLRDBL      at 0 range 4 .. 4;
      DBLRDY_STOP          at 0 range 5 .. 5;
      SAMPLERDY_READCLRACC at 0 range 6 .. 6;
      Reserved_7_31        at 0 range 7 .. 31;
   end record;

   --  Write '1' to Enable interrupt for SAMPLERDY event
   type INTENSET_SAMPLERDY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_SAMPLERDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for SAMPLERDY event
   type INTENSET_SAMPLERDY_Field_1 is
     (--  Reset value for the field
      Intenset_Samplerdy_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_SAMPLERDY_Field_1 use
     (Intenset_Samplerdy_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for REPORTRDY event
   type INTENSET_REPORTRDY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_REPORTRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for REPORTRDY event
   type INTENSET_REPORTRDY_Field_1 is
     (--  Reset value for the field
      Intenset_Reportrdy_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_REPORTRDY_Field_1 use
     (Intenset_Reportrdy_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for ACCOF event
   type INTENSET_ACCOF_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_ACCOF_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for ACCOF event
   type INTENSET_ACCOF_Field_1 is
     (--  Reset value for the field
      Intenset_Accof_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_ACCOF_Field_1 use
     (Intenset_Accof_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for DBLRDY event
   type INTENSET_DBLRDY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_DBLRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for DBLRDY event
   type INTENSET_DBLRDY_Field_1 is
     (--  Reset value for the field
      Intenset_Dblrdy_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_DBLRDY_Field_1 use
     (Intenset_Dblrdy_Field_Reset => 0,
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

   --  Enable interrupt
   type INTENSET_Register is record
      --  Write '1' to Enable interrupt for SAMPLERDY event
      SAMPLERDY     : INTENSET_SAMPLERDY_Field_1 :=
                       Intenset_Samplerdy_Field_Reset;
      --  Write '1' to Enable interrupt for REPORTRDY event
      REPORTRDY     : INTENSET_REPORTRDY_Field_1 :=
                       Intenset_Reportrdy_Field_Reset;
      --  Write '1' to Enable interrupt for ACCOF event
      ACCOF         : INTENSET_ACCOF_Field_1 := Intenset_Accof_Field_Reset;
      --  Write '1' to Enable interrupt for DBLRDY event
      DBLRDY        : INTENSET_DBLRDY_Field_1 := Intenset_Dblrdy_Field_Reset;
      --  Write '1' to Enable interrupt for STOPPED event
      STOPPED       : INTENSET_STOPPED_Field_1 :=
                       Intenset_Stopped_Field_Reset;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      SAMPLERDY     at 0 range 0 .. 0;
      REPORTRDY     at 0 range 1 .. 1;
      ACCOF         at 0 range 2 .. 2;
      DBLRDY        at 0 range 3 .. 3;
      STOPPED       at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Write '1' to Disable interrupt for SAMPLERDY event
   type INTENCLR_SAMPLERDY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_SAMPLERDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for SAMPLERDY event
   type INTENCLR_SAMPLERDY_Field_1 is
     (--  Reset value for the field
      Intenclr_Samplerdy_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_SAMPLERDY_Field_1 use
     (Intenclr_Samplerdy_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for REPORTRDY event
   type INTENCLR_REPORTRDY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_REPORTRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for REPORTRDY event
   type INTENCLR_REPORTRDY_Field_1 is
     (--  Reset value for the field
      Intenclr_Reportrdy_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_REPORTRDY_Field_1 use
     (Intenclr_Reportrdy_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for ACCOF event
   type INTENCLR_ACCOF_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_ACCOF_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for ACCOF event
   type INTENCLR_ACCOF_Field_1 is
     (--  Reset value for the field
      Intenclr_Accof_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_ACCOF_Field_1 use
     (Intenclr_Accof_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for DBLRDY event
   type INTENCLR_DBLRDY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_DBLRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for DBLRDY event
   type INTENCLR_DBLRDY_Field_1 is
     (--  Reset value for the field
      Intenclr_Dblrdy_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_DBLRDY_Field_1 use
     (Intenclr_Dblrdy_Field_Reset => 0,
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

   --  Disable interrupt
   type INTENCLR_Register is record
      --  Write '1' to Disable interrupt for SAMPLERDY event
      SAMPLERDY     : INTENCLR_SAMPLERDY_Field_1 :=
                       Intenclr_Samplerdy_Field_Reset;
      --  Write '1' to Disable interrupt for REPORTRDY event
      REPORTRDY     : INTENCLR_REPORTRDY_Field_1 :=
                       Intenclr_Reportrdy_Field_Reset;
      --  Write '1' to Disable interrupt for ACCOF event
      ACCOF         : INTENCLR_ACCOF_Field_1 := Intenclr_Accof_Field_Reset;
      --  Write '1' to Disable interrupt for DBLRDY event
      DBLRDY        : INTENCLR_DBLRDY_Field_1 := Intenclr_Dblrdy_Field_Reset;
      --  Write '1' to Disable interrupt for STOPPED event
      STOPPED       : INTENCLR_STOPPED_Field_1 :=
                       Intenclr_Stopped_Field_Reset;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      SAMPLERDY     at 0 range 0 .. 0;
      REPORTRDY     at 0 range 1 .. 1;
      ACCOF         at 0 range 2 .. 2;
      DBLRDY        at 0 range 3 .. 3;
      STOPPED       at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Enable or disable the quadrature decoder
   type ENABLE_ENABLE_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable the quadrature decoder
   type ENABLE_Register is record
      --  Enable or disable the quadrature decoder
      ENABLE        : ENABLE_ENABLE_Field := NRF_SVD.QDEC.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  LED output pin polarity
   type LEDPOL_LEDPOL_Field is
     (--  Led active on output pin low
      Activelow,
      --  Led active on output pin high
      Activehigh)
     with Size => 1;
   for LEDPOL_LEDPOL_Field use
     (Activelow => 0,
      Activehigh => 1);

   --  LED output pin polarity
   type LEDPOL_Register is record
      --  LED output pin polarity
      LEDPOL        : LEDPOL_LEDPOL_Field := NRF_SVD.QDEC.Activelow;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LEDPOL_Register use record
      LEDPOL        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Sample period. The SAMPLE register will be updated for every new sample
   type SAMPLEPER_SAMPLEPER_Field is
     (--  128 us
      Val_128US,
      --  256 us
      Val_256US,
      --  512 us
      Val_512US,
      --  1024 us
      Val_1024US,
      --  2048 us
      Val_2048US,
      --  4096 us
      Val_4096US,
      --  8192 us
      Val_8192US,
      --  16384 us
      Val_16384US,
      --  32768 us
      Val_32MS,
      --  65536 us
      Val_65MS,
      --  131072 us
      Val_131MS)
     with Size => 4;
   for SAMPLEPER_SAMPLEPER_Field use
     (Val_128US => 0,
      Val_256US => 1,
      Val_512US => 2,
      Val_1024US => 3,
      Val_2048US => 4,
      Val_4096US => 5,
      Val_8192US => 6,
      Val_16384US => 7,
      Val_32MS => 8,
      Val_65MS => 9,
      Val_131MS => 10);

   --  Sample period
   type SAMPLEPER_Register is record
      --  Sample period. The SAMPLE register will be updated for every new
      --  sample
      SAMPLEPER     : SAMPLEPER_SAMPLEPER_Field := NRF_SVD.QDEC.Val_128US;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SAMPLEPER_Register use record
      SAMPLEPER     at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Specifies the number of samples to be accumulated in the ACC register
   --  before the REPORTRDY and DBLRDY events can be generated
   type REPORTPER_REPORTPER_Field is
     (--  10 samples / report
      Val_10Smpl,
      --  40 samples / report
      Val_40Smpl,
      --  80 samples / report
      Val_80Smpl,
      --  120 samples / report
      Val_120Smpl,
      --  160 samples / report
      Val_160Smpl,
      --  200 samples / report
      Val_200Smpl,
      --  240 samples / report
      Val_240Smpl,
      --  280 samples / report
      Val_280Smpl,
      --  1 sample / report
      Val_1Smpl)
     with Size => 4;
   for REPORTPER_REPORTPER_Field use
     (Val_10Smpl => 0,
      Val_40Smpl => 1,
      Val_80Smpl => 2,
      Val_120Smpl => 3,
      Val_160Smpl => 4,
      Val_200Smpl => 5,
      Val_240Smpl => 6,
      Val_280Smpl => 7,
      Val_1Smpl => 8);

   --  Number of samples to be taken before REPORTRDY and DBLRDY events can be
   --  generated
   type REPORTPER_Register is record
      --  Specifies the number of samples to be accumulated in the ACC register
      --  before the REPORTRDY and DBLRDY events can be generated
      REPORTPER     : REPORTPER_REPORTPER_Field := NRF_SVD.QDEC.Val_10Smpl;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for REPORTPER_Register use record
      REPORTPER     at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   ------------------------------
   -- PSEL cluster's Registers --
   ------------------------------

   subtype LED_PSEL_PIN_Field is HAL.UInt5;

   --  Connection
   type LED_CONNECT_Field is
     (--  Connect
      Connected,
      --  Disconnect
      Disconnected)
     with Size => 1;
   for LED_CONNECT_Field use
     (Connected => 0,
      Disconnected => 1);

   --  Pin select for LED signal
   type LED_PSEL_Register is record
      --  Pin number
      PIN           : LED_PSEL_PIN_Field := 16#1F#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#3FFFFFF#;
      --  Connection
      CONNECT       : LED_CONNECT_Field := NRF_SVD.QDEC.Disconnected;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LED_PSEL_Register use record
      PIN           at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   subtype A_PSEL_PIN_Field is HAL.UInt5;

   --  Connection
   type A_CONNECT_Field is
     (--  Connect
      Connected,
      --  Disconnect
      Disconnected)
     with Size => 1;
   for A_CONNECT_Field use
     (Connected => 0,
      Disconnected => 1);

   --  Pin select for A signal
   type A_PSEL_Register is record
      --  Pin number
      PIN           : A_PSEL_PIN_Field := 16#1F#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#3FFFFFF#;
      --  Connection
      CONNECT       : A_CONNECT_Field := NRF_SVD.QDEC.Disconnected;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for A_PSEL_Register use record
      PIN           at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   subtype B_PSEL_PIN_Field is HAL.UInt5;

   --  Connection
   type B_CONNECT_Field is
     (--  Connect
      Connected,
      --  Disconnect
      Disconnected)
     with Size => 1;
   for B_CONNECT_Field use
     (Connected => 0,
      Disconnected => 1);

   --  Pin select for B signal
   type B_PSEL_Register is record
      --  Pin number
      PIN           : B_PSEL_PIN_Field := 16#1F#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#3FFFFFF#;
      --  Connection
      CONNECT       : B_CONNECT_Field := NRF_SVD.QDEC.Disconnected;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for B_PSEL_Register use record
      PIN           at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   --  Unspecified
   type PSEL_Cluster is record
      --  Pin select for LED signal
      LED : aliased LED_PSEL_Register;
      --  Pin select for A signal
      A   : aliased A_PSEL_Register;
      --  Pin select for B signal
      B   : aliased B_PSEL_Register;
   end record
     with Size => 96;

   for PSEL_Cluster use record
      LED at 16#0# range 0 .. 31;
      A   at 16#4# range 0 .. 31;
      B   at 16#8# range 0 .. 31;
   end record;

   --  Enable input debounce filters
   type DBFEN_DBFEN_Field is
     (--  Debounce input filters disabled
      Disabled,
      --  Debounce input filters enabled
      Enabled)
     with Size => 1;
   for DBFEN_DBFEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable input debounce filters
   type DBFEN_Register is record
      --  Enable input debounce filters
      DBFEN         : DBFEN_DBFEN_Field := NRF_SVD.QDEC.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBFEN_Register use record
      DBFEN         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype LEDPRE_LEDPRE_Field is HAL.UInt9;

   --  Time period the LED is switched ON prior to sampling
   type LEDPRE_Register is record
      --  Period in us the LED is switched on prior to sampling
      LEDPRE        : LEDPRE_LEDPRE_Field := 16#10#;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LEDPRE_Register use record
      LEDPRE        at 0 range 0 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype ACCDBL_ACCDBL_Field is HAL.UInt4;

   --  Register accumulating the number of detected double transitions
   type ACCDBL_Register is record
      --  Read-only. Register accumulating the number of detected double or
      --  illegal transitions. ( SAMPLE = 2 ).
      ACCDBL        : ACCDBL_ACCDBL_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ACCDBL_Register use record
      ACCDBL        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype ACCDBLREAD_ACCDBLREAD_Field is HAL.UInt4;

   --  Snapshot of the ACCDBL, updated by the READCLRACC or RDCLRDBL task
   type ACCDBLREAD_Register is record
      --  Read-only. Snapshot of the ACCDBL register. This field is updated
      --  when the READCLRACC or RDCLRDBL task is triggered.
      ACCDBLREAD    : ACCDBLREAD_ACCDBLREAD_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ACCDBLREAD_Register use record
      ACCDBLREAD    at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Quadrature Decoder
   type QDEC_Peripheral is record
      --  Task starting the quadrature decoder
      TASKS_START      : aliased HAL.UInt32;
      --  Task stopping the quadrature decoder
      TASKS_STOP       : aliased HAL.UInt32;
      --  Read and clear ACC and ACCDBL
      TASKS_READCLRACC : aliased HAL.UInt32;
      --  Read and clear ACC
      TASKS_RDCLRACC   : aliased HAL.UInt32;
      --  Read and clear ACCDBL
      TASKS_RDCLRDBL   : aliased HAL.UInt32;
      --  Event being generated for every new sample value written to the
      --  SAMPLE register
      EVENTS_SAMPLERDY : aliased HAL.UInt32;
      --  Non-null report ready
      EVENTS_REPORTRDY : aliased HAL.UInt32;
      --  ACC or ACCDBL register overflow
      EVENTS_ACCOF     : aliased HAL.UInt32;
      --  Double displacement(s) detected
      EVENTS_DBLRDY    : aliased HAL.UInt32;
      --  QDEC has been stopped
      EVENTS_STOPPED   : aliased HAL.UInt32;
      --  Shortcut register
      SHORTS           : aliased SHORTS_Register;
      --  Enable interrupt
      INTENSET         : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR         : aliased INTENCLR_Register;
      --  Enable the quadrature decoder
      ENABLE           : aliased ENABLE_Register;
      --  LED output pin polarity
      LEDPOL           : aliased LEDPOL_Register;
      --  Sample period
      SAMPLEPER        : aliased SAMPLEPER_Register;
      --  Motion sample value
      SAMPLE           : aliased HAL.UInt32;
      --  Number of samples to be taken before REPORTRDY and DBLRDY events can
      --  be generated
      REPORTPER        : aliased REPORTPER_Register;
      --  Register accumulating the valid transitions
      ACC              : aliased HAL.UInt32;
      --  Snapshot of the ACC register, updated by the READCLRACC or RDCLRACC
      --  task
      ACCREAD          : aliased HAL.UInt32;
      --  Unspecified
      PSEL             : aliased PSEL_Cluster;
      --  Enable input debounce filters
      DBFEN            : aliased DBFEN_Register;
      --  Time period the LED is switched ON prior to sampling
      LEDPRE           : aliased LEDPRE_Register;
      --  Register accumulating the number of detected double transitions
      ACCDBL           : aliased ACCDBL_Register;
      --  Snapshot of the ACCDBL, updated by the READCLRACC or RDCLRDBL task
      ACCDBLREAD       : aliased ACCDBLREAD_Register;
   end record
     with Volatile;

   for QDEC_Peripheral use record
      TASKS_START      at 16#0# range 0 .. 31;
      TASKS_STOP       at 16#4# range 0 .. 31;
      TASKS_READCLRACC at 16#8# range 0 .. 31;
      TASKS_RDCLRACC   at 16#C# range 0 .. 31;
      TASKS_RDCLRDBL   at 16#10# range 0 .. 31;
      EVENTS_SAMPLERDY at 16#100# range 0 .. 31;
      EVENTS_REPORTRDY at 16#104# range 0 .. 31;
      EVENTS_ACCOF     at 16#108# range 0 .. 31;
      EVENTS_DBLRDY    at 16#10C# range 0 .. 31;
      EVENTS_STOPPED   at 16#110# range 0 .. 31;
      SHORTS           at 16#200# range 0 .. 31;
      INTENSET         at 16#304# range 0 .. 31;
      INTENCLR         at 16#308# range 0 .. 31;
      ENABLE           at 16#500# range 0 .. 31;
      LEDPOL           at 16#504# range 0 .. 31;
      SAMPLEPER        at 16#508# range 0 .. 31;
      SAMPLE           at 16#50C# range 0 .. 31;
      REPORTPER        at 16#510# range 0 .. 31;
      ACC              at 16#514# range 0 .. 31;
      ACCREAD          at 16#518# range 0 .. 31;
      PSEL             at 16#51C# range 0 .. 95;
      DBFEN            at 16#528# range 0 .. 31;
      LEDPRE           at 16#540# range 0 .. 31;
      ACCDBL           at 16#544# range 0 .. 31;
      ACCDBLREAD       at 16#548# range 0 .. 31;
   end record;

   --  Quadrature Decoder
   QDEC_Periph : aliased QDEC_Peripheral
     with Import, Address => QDEC_Base;

end NRF_SVD.QDEC;
