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

package NRF_SVD.I2S is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Enable or disable interrupt for RXPTRUPD event
   type INTEN_RXPTRUPD_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_RXPTRUPD_Field use
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

   --  Enable or disable interrupt for TXPTRUPD event
   type INTEN_TXPTRUPD_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_TXPTRUPD_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt
   type INTEN_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  Enable or disable interrupt for RXPTRUPD event
      RXPTRUPD      : INTEN_RXPTRUPD_Field := NRF_SVD.I2S.Disabled;
      --  Enable or disable interrupt for STOPPED event
      STOPPED       : INTEN_STOPPED_Field := NRF_SVD.I2S.Disabled;
      --  unspecified
      Reserved_3_4  : HAL.UInt2 := 16#0#;
      --  Enable or disable interrupt for TXPTRUPD event
      TXPTRUPD      : INTEN_TXPTRUPD_Field := NRF_SVD.I2S.Disabled;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTEN_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      RXPTRUPD      at 0 range 1 .. 1;
      STOPPED       at 0 range 2 .. 2;
      Reserved_3_4  at 0 range 3 .. 4;
      TXPTRUPD      at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  Write '1' to Enable interrupt for RXPTRUPD event
   type INTENSET_RXPTRUPD_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_RXPTRUPD_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for RXPTRUPD event
   type INTENSET_RXPTRUPD_Field_1 is
     (--  Reset value for the field
      Intenset_Rxptrupd_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_RXPTRUPD_Field_1 use
     (Intenset_Rxptrupd_Field_Reset => 0,
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

   --  Write '1' to Enable interrupt for TXPTRUPD event
   type INTENSET_TXPTRUPD_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_TXPTRUPD_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for TXPTRUPD event
   type INTENSET_TXPTRUPD_Field_1 is
     (--  Reset value for the field
      Intenset_Txptrupd_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_TXPTRUPD_Field_1 use
     (Intenset_Txptrupd_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  Write '1' to Enable interrupt for RXPTRUPD event
      RXPTRUPD      : INTENSET_RXPTRUPD_Field_1 :=
                       Intenset_Rxptrupd_Field_Reset;
      --  Write '1' to Enable interrupt for STOPPED event
      STOPPED       : INTENSET_STOPPED_Field_1 :=
                       Intenset_Stopped_Field_Reset;
      --  unspecified
      Reserved_3_4  : HAL.UInt2 := 16#0#;
      --  Write '1' to Enable interrupt for TXPTRUPD event
      TXPTRUPD      : INTENSET_TXPTRUPD_Field_1 :=
                       Intenset_Txptrupd_Field_Reset;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      RXPTRUPD      at 0 range 1 .. 1;
      STOPPED       at 0 range 2 .. 2;
      Reserved_3_4  at 0 range 3 .. 4;
      TXPTRUPD      at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  Write '1' to Disable interrupt for RXPTRUPD event
   type INTENCLR_RXPTRUPD_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_RXPTRUPD_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for RXPTRUPD event
   type INTENCLR_RXPTRUPD_Field_1 is
     (--  Reset value for the field
      Intenclr_Rxptrupd_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_RXPTRUPD_Field_1 use
     (Intenclr_Rxptrupd_Field_Reset => 0,
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

   --  Write '1' to Disable interrupt for TXPTRUPD event
   type INTENCLR_TXPTRUPD_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_TXPTRUPD_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for TXPTRUPD event
   type INTENCLR_TXPTRUPD_Field_1 is
     (--  Reset value for the field
      Intenclr_Txptrupd_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_TXPTRUPD_Field_1 use
     (Intenclr_Txptrupd_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  Write '1' to Disable interrupt for RXPTRUPD event
      RXPTRUPD      : INTENCLR_RXPTRUPD_Field_1 :=
                       Intenclr_Rxptrupd_Field_Reset;
      --  Write '1' to Disable interrupt for STOPPED event
      STOPPED       : INTENCLR_STOPPED_Field_1 :=
                       Intenclr_Stopped_Field_Reset;
      --  unspecified
      Reserved_3_4  : HAL.UInt2 := 16#0#;
      --  Write '1' to Disable interrupt for TXPTRUPD event
      TXPTRUPD      : INTENCLR_TXPTRUPD_Field_1 :=
                       Intenclr_Txptrupd_Field_Reset;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      RXPTRUPD      at 0 range 1 .. 1;
      STOPPED       at 0 range 2 .. 2;
      Reserved_3_4  at 0 range 3 .. 4;
      TXPTRUPD      at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  Enable I2S module.
   type ENABLE_ENABLE_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable I2S module.
   type ENABLE_Register is record
      --  Enable I2S module.
      ENABLE        : ENABLE_ENABLE_Field := NRF_SVD.I2S.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --------------------------------
   -- CONFIG cluster's Registers --
   --------------------------------

   --  I2S mode.
   type MODE_MODE_Field is
     (--  Master mode. SCK and LRCK generated from internal master clcok (MCK) and
--  output on pins defined by PSEL.xxx.
      Master,
      --  Slave mode. SCK and LRCK generated by external master and received on pins
--  defined by PSEL.xxx
      Slave)
     with Size => 1;
   for MODE_MODE_Field use
     (Master => 0,
      Slave => 1);

   --  I2S mode.
   type MODE_CONFIG_Register is record
      --  I2S mode.
      MODE          : MODE_MODE_Field := NRF_SVD.I2S.Master;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MODE_CONFIG_Register use record
      MODE          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Reception (RX) enable.
   type RXEN_RXEN_Field is
     (--  Reception disabled and now data will be written to the RXD.PTR address.
      Disabled,
      --  Reception enabled.
      Enabled)
     with Size => 1;
   for RXEN_RXEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Reception (RX) enable.
   type RXEN_CONFIG_Register is record
      --  Reception (RX) enable.
      RXEN          : RXEN_RXEN_Field := NRF_SVD.I2S.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXEN_CONFIG_Register use record
      RXEN          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Transmission (TX) enable.
   type TXEN_TXEN_Field is
     (--  Transmission disabled and now data will be read from the RXD.TXD address.
      Disabled,
      --  Transmission enabled.
      Enabled)
     with Size => 1;
   for TXEN_TXEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Transmission (TX) enable.
   type TXEN_CONFIG_Register is record
      --  Transmission (TX) enable.
      TXEN          : TXEN_TXEN_Field := NRF_SVD.I2S.Enabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXEN_CONFIG_Register use record
      TXEN          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Master clock generator enable.
   type MCKEN_MCKEN_Field is
     (--  Master clock generator disabled and PSEL.MCK not connected(available as
--  GPIO).
      Disabled,
      --  Master clock generator running and MCK output on PSEL.MCK.
      Enabled)
     with Size => 1;
   for MCKEN_MCKEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Master clock generator enable.
   type MCKEN_CONFIG_Register is record
      --  Master clock generator enable.
      MCKEN         : MCKEN_MCKEN_Field := NRF_SVD.I2S.Enabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MCKEN_CONFIG_Register use record
      MCKEN         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  MCK / LRCK ratio.
   type RATIO_RATIO_Field is
     (--  LRCK = MCK / 32
      Val_32X,
      --  LRCK = MCK / 48
      Val_48X,
      --  LRCK = MCK / 64
      Val_64X,
      --  LRCK = MCK / 96
      Val_96X,
      --  LRCK = MCK / 128
      Val_128X,
      --  LRCK = MCK / 192
      Val_192X,
      --  LRCK = MCK / 256
      Val_256X,
      --  LRCK = MCK / 384
      Val_384X,
      --  LRCK = MCK / 512
      Val_512X)
     with Size => 4;
   for RATIO_RATIO_Field use
     (Val_32X => 0,
      Val_48X => 1,
      Val_64X => 2,
      Val_96X => 3,
      Val_128X => 4,
      Val_192X => 5,
      Val_256X => 6,
      Val_384X => 7,
      Val_512X => 8);

   --  MCK / LRCK ratio.
   type RATIO_CONFIG_Register is record
      --  MCK / LRCK ratio.
      RATIO         : RATIO_RATIO_Field := NRF_SVD.I2S.Val_256X;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RATIO_CONFIG_Register use record
      RATIO         at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Sample width.
   type SWIDTH_SWIDTH_Field is
     (--  8 bit.
      Val_8Bit,
      --  16 bit.
      Val_16Bit,
      --  24 bit.
      Val_24Bit)
     with Size => 2;
   for SWIDTH_SWIDTH_Field use
     (Val_8Bit => 0,
      Val_16Bit => 1,
      Val_24Bit => 2);

   --  Sample width.
   type SWIDTH_CONFIG_Register is record
      --  Sample width.
      SWIDTH        : SWIDTH_SWIDTH_Field := NRF_SVD.I2S.Val_16Bit;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWIDTH_CONFIG_Register use record
      SWIDTH        at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Alignment of sample within a frame.
   type ALIGN_ALIGN_Field is
     (--  Left-aligned.
      Left,
      --  Right-aligned.
      Right)
     with Size => 1;
   for ALIGN_ALIGN_Field use
     (Left => 0,
      Right => 1);

   --  Alignment of sample within a frame.
   type ALIGN_CONFIG_Register is record
      --  Alignment of sample within a frame.
      ALIGN         : ALIGN_ALIGN_Field := NRF_SVD.I2S.Left;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ALIGN_CONFIG_Register use record
      ALIGN         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Frame format.
   type FORMAT_FORMAT_Field is
     (--  Original I2S format.
      I2S,
      --  Alternate (left- or right-aligned) format.
      Aligned)
     with Size => 1;
   for FORMAT_FORMAT_Field use
     (I2S => 0,
      Aligned => 1);

   --  Frame format.
   type FORMAT_CONFIG_Register is record
      --  Frame format.
      FORMAT        : FORMAT_FORMAT_Field := NRF_SVD.I2S.I2S;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FORMAT_CONFIG_Register use record
      FORMAT        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Enable channels.
   type CHANNELS_CHANNELS_Field is
     (--  Stereo.
      Stereo,
      --  Left only.
      Left,
      --  Right only.
      Right)
     with Size => 2;
   for CHANNELS_CHANNELS_Field use
     (Stereo => 0,
      Left => 1,
      Right => 2);

   --  Enable channels.
   type CHANNELS_CONFIG_Register is record
      --  Enable channels.
      CHANNELS      : CHANNELS_CHANNELS_Field := NRF_SVD.I2S.Stereo;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CHANNELS_CONFIG_Register use record
      CHANNELS      at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Unspecified
   type CONFIG_Cluster is record
      --  I2S mode.
      MODE     : aliased MODE_CONFIG_Register;
      --  Reception (RX) enable.
      RXEN     : aliased RXEN_CONFIG_Register;
      --  Transmission (TX) enable.
      TXEN     : aliased TXEN_CONFIG_Register;
      --  Master clock generator enable.
      MCKEN    : aliased MCKEN_CONFIG_Register;
      --  Master clock generator frequency.
      MCKFREQ  : aliased HAL.UInt32;
      --  MCK / LRCK ratio.
      RATIO    : aliased RATIO_CONFIG_Register;
      --  Sample width.
      SWIDTH   : aliased SWIDTH_CONFIG_Register;
      --  Alignment of sample within a frame.
      ALIGN    : aliased ALIGN_CONFIG_Register;
      --  Frame format.
      FORMAT   : aliased FORMAT_CONFIG_Register;
      --  Enable channels.
      CHANNELS : aliased CHANNELS_CONFIG_Register;
   end record
     with Size => 320;

   for CONFIG_Cluster use record
      MODE     at 16#0# range 0 .. 31;
      RXEN     at 16#4# range 0 .. 31;
      TXEN     at 16#8# range 0 .. 31;
      MCKEN    at 16#C# range 0 .. 31;
      MCKFREQ  at 16#10# range 0 .. 31;
      RATIO    at 16#14# range 0 .. 31;
      SWIDTH   at 16#18# range 0 .. 31;
      ALIGN    at 16#1C# range 0 .. 31;
      FORMAT   at 16#20# range 0 .. 31;
      CHANNELS at 16#24# range 0 .. 31;
   end record;

   -----------------------------
   -- RXD cluster's Registers --
   -----------------------------

   --  Unspecified
   type RXD_Cluster is record
      --  Receive buffer RAM start address.
      PTR : aliased HAL.UInt32;
   end record
     with Size => 32;

   for RXD_Cluster use record
      PTR at 0 range 0 .. 31;
   end record;

   -----------------------------
   -- TXD cluster's Registers --
   -----------------------------

   --  Unspecified
   type TXD_Cluster is record
      --  Transmit buffer RAM start address.
      PTR : aliased HAL.UInt32;
   end record
     with Size => 32;

   for TXD_Cluster use record
      PTR at 0 range 0 .. 31;
   end record;

   -------------------------------
   -- RXTXD cluster's Registers --
   -------------------------------

   subtype MAXCNT_RXTXD_MAXCNT_Field is HAL.UInt14;

   --  Size of RXD and TXD buffers.
   type MAXCNT_RXTXD_Register is record
      --  Size of RXD and TXD buffers in number of 32 bit words.
      MAXCNT         : MAXCNT_RXTXD_MAXCNT_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAXCNT_RXTXD_Register use record
      MAXCNT         at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  Unspecified
   type RXTXD_Cluster is record
      --  Size of RXD and TXD buffers.
      MAXCNT : aliased MAXCNT_RXTXD_Register;
   end record
     with Size => 32;

   for RXTXD_Cluster use record
      MAXCNT at 0 range 0 .. 31;
   end record;

   ------------------------------
   -- PSEL cluster's Registers --
   ------------------------------

   subtype MCK_PSEL_PIN_Field is HAL.UInt5;

   --  Connection
   type MCK_CONNECT_Field is
     (--  Connect
      Connected,
      --  Disconnect
      Disconnected)
     with Size => 1;
   for MCK_CONNECT_Field use
     (Connected => 0,
      Disconnected => 1);

   --  Pin select for MCK signal.
   type MCK_PSEL_Register is record
      --  Pin number
      PIN           : MCK_PSEL_PIN_Field := 16#1F#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#3FFFFFF#;
      --  Connection
      CONNECT       : MCK_CONNECT_Field := NRF_SVD.I2S.Disconnected;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MCK_PSEL_Register use record
      PIN           at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   subtype SCK_PSEL_PIN_Field is HAL.UInt5;

   --  Connection
   type SCK_CONNECT_Field is
     (--  Connect
      Connected,
      --  Disconnect
      Disconnected)
     with Size => 1;
   for SCK_CONNECT_Field use
     (Connected => 0,
      Disconnected => 1);

   --  Pin select for SCK signal.
   type SCK_PSEL_Register is record
      --  Pin number
      PIN           : SCK_PSEL_PIN_Field := 16#1F#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#3FFFFFF#;
      --  Connection
      CONNECT       : SCK_CONNECT_Field := NRF_SVD.I2S.Disconnected;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCK_PSEL_Register use record
      PIN           at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   subtype LRCK_PSEL_PIN_Field is HAL.UInt5;

   --  Connection
   type LRCK_CONNECT_Field is
     (--  Connect
      Connected,
      --  Disconnect
      Disconnected)
     with Size => 1;
   for LRCK_CONNECT_Field use
     (Connected => 0,
      Disconnected => 1);

   --  Pin select for LRCK signal.
   type LRCK_PSEL_Register is record
      --  Pin number
      PIN           : LRCK_PSEL_PIN_Field := 16#1F#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#3FFFFFF#;
      --  Connection
      CONNECT       : LRCK_CONNECT_Field := NRF_SVD.I2S.Disconnected;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LRCK_PSEL_Register use record
      PIN           at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   subtype SDIN_PSEL_PIN_Field is HAL.UInt5;

   --  Connection
   type SDIN_CONNECT_Field is
     (--  Connect
      Connected,
      --  Disconnect
      Disconnected)
     with Size => 1;
   for SDIN_CONNECT_Field use
     (Connected => 0,
      Disconnected => 1);

   --  Pin select for SDIN signal.
   type SDIN_PSEL_Register is record
      --  Pin number
      PIN           : SDIN_PSEL_PIN_Field := 16#1F#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#3FFFFFF#;
      --  Connection
      CONNECT       : SDIN_CONNECT_Field := NRF_SVD.I2S.Disconnected;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDIN_PSEL_Register use record
      PIN           at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   subtype SDOUT_PSEL_PIN_Field is HAL.UInt5;

   --  Connection
   type SDOUT_CONNECT_Field is
     (--  Connect
      Connected,
      --  Disconnect
      Disconnected)
     with Size => 1;
   for SDOUT_CONNECT_Field use
     (Connected => 0,
      Disconnected => 1);

   --  Pin select for SDOUT signal.
   type SDOUT_PSEL_Register is record
      --  Pin number
      PIN           : SDOUT_PSEL_PIN_Field := 16#1F#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#3FFFFFF#;
      --  Connection
      CONNECT       : SDOUT_CONNECT_Field := NRF_SVD.I2S.Disconnected;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDOUT_PSEL_Register use record
      PIN           at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   --  Unspecified
   type PSEL_Cluster is record
      --  Pin select for MCK signal.
      MCK   : aliased MCK_PSEL_Register;
      --  Pin select for SCK signal.
      SCK   : aliased SCK_PSEL_Register;
      --  Pin select for LRCK signal.
      LRCK  : aliased LRCK_PSEL_Register;
      --  Pin select for SDIN signal.
      SDIN  : aliased SDIN_PSEL_Register;
      --  Pin select for SDOUT signal.
      SDOUT : aliased SDOUT_PSEL_Register;
   end record
     with Size => 160;

   for PSEL_Cluster use record
      MCK   at 16#0# range 0 .. 31;
      SCK   at 16#4# range 0 .. 31;
      LRCK  at 16#8# range 0 .. 31;
      SDIN  at 16#C# range 0 .. 31;
      SDOUT at 16#10# range 0 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Inter-IC Sound
   type I2S_Peripheral is record
      --  Starts continuous I2S transfer. Also starts MCK generator when this
      --  is enabled.
      TASKS_START     : aliased HAL.UInt32;
      --  Stops I2S transfer. Also stops MCK generator. Triggering this task
      --  will cause the {event:STOPPED} event to be generated.
      TASKS_STOP      : aliased HAL.UInt32;
      --  The RXD.PTR register has been copied to internal double-buffers. When
      --  the I2S module is started and RX is enabled, this event will be
      --  generated for every RXTXD.MAXCNT words that are received on the SDIN
      --  pin.
      EVENTS_RXPTRUPD : aliased HAL.UInt32;
      --  I2S transfer stopped.
      EVENTS_STOPPED  : aliased HAL.UInt32;
      --  The TDX.PTR register has been copied to internal double-buffers. When
      --  the I2S module is started and TX is enabled, this event will be
      --  generated for every RXTXD.MAXCNT words that are sent on the SDOUT
      --  pin.
      EVENTS_TXPTRUPD : aliased HAL.UInt32;
      --  Enable or disable interrupt
      INTEN           : aliased INTEN_Register;
      --  Enable interrupt
      INTENSET        : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR        : aliased INTENCLR_Register;
      --  Enable I2S module.
      ENABLE          : aliased ENABLE_Register;
      --  Unspecified
      CONFIG          : aliased CONFIG_Cluster;
      --  Unspecified
      RXD             : aliased RXD_Cluster;
      --  Unspecified
      TXD             : aliased TXD_Cluster;
      --  Unspecified
      RXTXD           : aliased RXTXD_Cluster;
      --  Unspecified
      PSEL            : aliased PSEL_Cluster;
   end record
     with Volatile;

   for I2S_Peripheral use record
      TASKS_START     at 16#0# range 0 .. 31;
      TASKS_STOP      at 16#4# range 0 .. 31;
      EVENTS_RXPTRUPD at 16#104# range 0 .. 31;
      EVENTS_STOPPED  at 16#108# range 0 .. 31;
      EVENTS_TXPTRUPD at 16#114# range 0 .. 31;
      INTEN           at 16#300# range 0 .. 31;
      INTENSET        at 16#304# range 0 .. 31;
      INTENCLR        at 16#308# range 0 .. 31;
      ENABLE          at 16#500# range 0 .. 31;
      CONFIG          at 16#504# range 0 .. 319;
      RXD             at 16#538# range 0 .. 31;
      TXD             at 16#540# range 0 .. 31;
      RXTXD           at 16#550# range 0 .. 31;
      PSEL            at 16#560# range 0 .. 159;
   end record;

   --  Inter-IC Sound
   I2S_Periph : aliased I2S_Peripheral
     with Import, Address => I2S_Base;

end NRF_SVD.I2S;
