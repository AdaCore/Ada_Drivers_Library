--  ============================================================================
--  Atmel Microcontroller Software Support
--  ============================================================================
--  Copyright (c) 2017 Atmel Corporation,
--  a wholly owned subsidiary of Microchip Technology Inc.
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the Licence at
--
--  http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
--  ============================================================================

--  This spec has been automatically generated from ATSAMD51J19A.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.GCLK is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Control
   type GCLK_CTRLA_Register is record
      --  Software Reset
      SWRST        : Boolean := False;
      --  unspecified
      Reserved_1_7 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for GCLK_CTRLA_Register use record
      SWRST        at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
   end record;

   --  Generic Clock Generator Control 0 Synchronization Busy bits
   type SYNCBUSY_GENCTRL0Select is
     (--  Generic clock generator 0
      Gclk0,
      --  Generic clock generator 1
      Gclk1,
      --  Generic clock generator 2
      Gclk2,
      --  Generic clock generator 3
      Gclk3,
      --  Generic clock generator 4
      Gclk4,
      --  Generic clock generator 5
      Gclk5,
      --  Generic clock generator 6
      Gclk6,
      --  Generic clock generator 7
      Gclk7,
      --  Generic clock generator 8
      Gclk8,
      --  Generic clock generator 9
      Gclk9,
      --  Generic clock generator 10
      Gclk10,
      --  Generic clock generator 11
      Gclk11)
     with Size => 12;
   for SYNCBUSY_GENCTRL0Select use
     (Gclk0 => 1,
      Gclk1 => 2,
      Gclk2 => 4,
      Gclk3 => 8,
      Gclk4 => 16,
      Gclk5 => 32,
      Gclk6 => 64,
      Gclk7 => 128,
      Gclk8 => 256,
      Gclk9 => 512,
      Gclk10 => 1024,
      Gclk11 => 2048);

   --  Generic Clock Generator Control 1 Synchronization Busy bits
   type SYNCBUSY_GENCTRL1Select is
     (--  Generic clock generator 0
      Gclk0,
      --  Generic clock generator 1
      Gclk1,
      --  Generic clock generator 2
      Gclk2,
      --  Generic clock generator 3
      Gclk3,
      --  Generic clock generator 4
      Gclk4,
      --  Generic clock generator 5
      Gclk5,
      --  Generic clock generator 6
      Gclk6,
      --  Generic clock generator 7
      Gclk7,
      --  Generic clock generator 8
      Gclk8,
      --  Generic clock generator 9
      Gclk9,
      --  Generic clock generator 10
      Gclk10,
      --  Generic clock generator 11
      Gclk11)
     with Size => 12;
   for SYNCBUSY_GENCTRL1Select use
     (Gclk0 => 1,
      Gclk1 => 2,
      Gclk2 => 4,
      Gclk3 => 8,
      Gclk4 => 16,
      Gclk5 => 32,
      Gclk6 => 64,
      Gclk7 => 128,
      Gclk8 => 256,
      Gclk9 => 512,
      Gclk10 => 1024,
      Gclk11 => 2048);

   --  Generic Clock Generator Control 2 Synchronization Busy bits
   type SYNCBUSY_GENCTRL2Select is
     (--  Generic clock generator 0
      Gclk0,
      --  Generic clock generator 1
      Gclk1,
      --  Generic clock generator 2
      Gclk2,
      --  Generic clock generator 3
      Gclk3,
      --  Generic clock generator 4
      Gclk4,
      --  Generic clock generator 5
      Gclk5,
      --  Generic clock generator 6
      Gclk6,
      --  Generic clock generator 7
      Gclk7,
      --  Generic clock generator 8
      Gclk8,
      --  Generic clock generator 9
      Gclk9,
      --  Generic clock generator 10
      Gclk10,
      --  Generic clock generator 11
      Gclk11)
     with Size => 12;
   for SYNCBUSY_GENCTRL2Select use
     (Gclk0 => 1,
      Gclk1 => 2,
      Gclk2 => 4,
      Gclk3 => 8,
      Gclk4 => 16,
      Gclk5 => 32,
      Gclk6 => 64,
      Gclk7 => 128,
      Gclk8 => 256,
      Gclk9 => 512,
      Gclk10 => 1024,
      Gclk11 => 2048);

   --  Generic Clock Generator Control 3 Synchronization Busy bits
   type SYNCBUSY_GENCTRL3Select is
     (--  Generic clock generator 0
      Gclk0,
      --  Generic clock generator 1
      Gclk1,
      --  Generic clock generator 2
      Gclk2,
      --  Generic clock generator 3
      Gclk3,
      --  Generic clock generator 4
      Gclk4,
      --  Generic clock generator 5
      Gclk5,
      --  Generic clock generator 6
      Gclk6,
      --  Generic clock generator 7
      Gclk7,
      --  Generic clock generator 8
      Gclk8,
      --  Generic clock generator 9
      Gclk9,
      --  Generic clock generator 10
      Gclk10,
      --  Generic clock generator 11
      Gclk11)
     with Size => 12;
   for SYNCBUSY_GENCTRL3Select use
     (Gclk0 => 1,
      Gclk1 => 2,
      Gclk2 => 4,
      Gclk3 => 8,
      Gclk4 => 16,
      Gclk5 => 32,
      Gclk6 => 64,
      Gclk7 => 128,
      Gclk8 => 256,
      Gclk9 => 512,
      Gclk10 => 1024,
      Gclk11 => 2048);

   --  Generic Clock Generator Control 4 Synchronization Busy bits
   type SYNCBUSY_GENCTRL4Select is
     (--  Generic clock generator 0
      Gclk0,
      --  Generic clock generator 1
      Gclk1,
      --  Generic clock generator 2
      Gclk2,
      --  Generic clock generator 3
      Gclk3,
      --  Generic clock generator 4
      Gclk4,
      --  Generic clock generator 5
      Gclk5,
      --  Generic clock generator 6
      Gclk6,
      --  Generic clock generator 7
      Gclk7,
      --  Generic clock generator 8
      Gclk8,
      --  Generic clock generator 9
      Gclk9,
      --  Generic clock generator 10
      Gclk10,
      --  Generic clock generator 11
      Gclk11)
     with Size => 12;
   for SYNCBUSY_GENCTRL4Select use
     (Gclk0 => 1,
      Gclk1 => 2,
      Gclk2 => 4,
      Gclk3 => 8,
      Gclk4 => 16,
      Gclk5 => 32,
      Gclk6 => 64,
      Gclk7 => 128,
      Gclk8 => 256,
      Gclk9 => 512,
      Gclk10 => 1024,
      Gclk11 => 2048);

   --  Generic Clock Generator Control 5 Synchronization Busy bits
   type SYNCBUSY_GENCTRL5Select is
     (--  Generic clock generator 0
      Gclk0,
      --  Generic clock generator 1
      Gclk1,
      --  Generic clock generator 2
      Gclk2,
      --  Generic clock generator 3
      Gclk3,
      --  Generic clock generator 4
      Gclk4,
      --  Generic clock generator 5
      Gclk5,
      --  Generic clock generator 6
      Gclk6,
      --  Generic clock generator 7
      Gclk7,
      --  Generic clock generator 8
      Gclk8,
      --  Generic clock generator 9
      Gclk9,
      --  Generic clock generator 10
      Gclk10,
      --  Generic clock generator 11
      Gclk11)
     with Size => 12;
   for SYNCBUSY_GENCTRL5Select use
     (Gclk0 => 1,
      Gclk1 => 2,
      Gclk2 => 4,
      Gclk3 => 8,
      Gclk4 => 16,
      Gclk5 => 32,
      Gclk6 => 64,
      Gclk7 => 128,
      Gclk8 => 256,
      Gclk9 => 512,
      Gclk10 => 1024,
      Gclk11 => 2048);

   --  Generic Clock Generator Control 6 Synchronization Busy bits
   type SYNCBUSY_GENCTRL6Select is
     (--  Generic clock generator 0
      Gclk0,
      --  Generic clock generator 1
      Gclk1,
      --  Generic clock generator 2
      Gclk2,
      --  Generic clock generator 3
      Gclk3,
      --  Generic clock generator 4
      Gclk4,
      --  Generic clock generator 5
      Gclk5,
      --  Generic clock generator 6
      Gclk6,
      --  Generic clock generator 7
      Gclk7,
      --  Generic clock generator 8
      Gclk8,
      --  Generic clock generator 9
      Gclk9,
      --  Generic clock generator 10
      Gclk10,
      --  Generic clock generator 11
      Gclk11)
     with Size => 12;
   for SYNCBUSY_GENCTRL6Select use
     (Gclk0 => 1,
      Gclk1 => 2,
      Gclk2 => 4,
      Gclk3 => 8,
      Gclk4 => 16,
      Gclk5 => 32,
      Gclk6 => 64,
      Gclk7 => 128,
      Gclk8 => 256,
      Gclk9 => 512,
      Gclk10 => 1024,
      Gclk11 => 2048);

   --  Generic Clock Generator Control 7 Synchronization Busy bits
   type SYNCBUSY_GENCTRL7Select is
     (--  Generic clock generator 0
      Gclk0,
      --  Generic clock generator 1
      Gclk1,
      --  Generic clock generator 2
      Gclk2,
      --  Generic clock generator 3
      Gclk3,
      --  Generic clock generator 4
      Gclk4,
      --  Generic clock generator 5
      Gclk5,
      --  Generic clock generator 6
      Gclk6,
      --  Generic clock generator 7
      Gclk7,
      --  Generic clock generator 8
      Gclk8,
      --  Generic clock generator 9
      Gclk9,
      --  Generic clock generator 10
      Gclk10,
      --  Generic clock generator 11
      Gclk11)
     with Size => 12;
   for SYNCBUSY_GENCTRL7Select use
     (Gclk0 => 1,
      Gclk1 => 2,
      Gclk2 => 4,
      Gclk3 => 8,
      Gclk4 => 16,
      Gclk5 => 32,
      Gclk6 => 64,
      Gclk7 => 128,
      Gclk8 => 256,
      Gclk9 => 512,
      Gclk10 => 1024,
      Gclk11 => 2048);

   --  Generic Clock Generator Control 8 Synchronization Busy bits
   type SYNCBUSY_GENCTRL8Select is
     (--  Generic clock generator 0
      Gclk0,
      --  Generic clock generator 1
      Gclk1,
      --  Generic clock generator 2
      Gclk2,
      --  Generic clock generator 3
      Gclk3,
      --  Generic clock generator 4
      Gclk4,
      --  Generic clock generator 5
      Gclk5,
      --  Generic clock generator 6
      Gclk6,
      --  Generic clock generator 7
      Gclk7,
      --  Generic clock generator 8
      Gclk8,
      --  Generic clock generator 9
      Gclk9,
      --  Generic clock generator 10
      Gclk10,
      --  Generic clock generator 11
      Gclk11)
     with Size => 12;
   for SYNCBUSY_GENCTRL8Select use
     (Gclk0 => 1,
      Gclk1 => 2,
      Gclk2 => 4,
      Gclk3 => 8,
      Gclk4 => 16,
      Gclk5 => 32,
      Gclk6 => 64,
      Gclk7 => 128,
      Gclk8 => 256,
      Gclk9 => 512,
      Gclk10 => 1024,
      Gclk11 => 2048);

   --  Generic Clock Generator Control 9 Synchronization Busy bits
   type SYNCBUSY_GENCTRL9Select is
     (--  Generic clock generator 0
      Gclk0,
      --  Generic clock generator 1
      Gclk1,
      --  Generic clock generator 2
      Gclk2,
      --  Generic clock generator 3
      Gclk3,
      --  Generic clock generator 4
      Gclk4,
      --  Generic clock generator 5
      Gclk5,
      --  Generic clock generator 6
      Gclk6,
      --  Generic clock generator 7
      Gclk7,
      --  Generic clock generator 8
      Gclk8,
      --  Generic clock generator 9
      Gclk9,
      --  Generic clock generator 10
      Gclk10,
      --  Generic clock generator 11
      Gclk11)
     with Size => 12;
   for SYNCBUSY_GENCTRL9Select use
     (Gclk0 => 1,
      Gclk1 => 2,
      Gclk2 => 4,
      Gclk3 => 8,
      Gclk4 => 16,
      Gclk5 => 32,
      Gclk6 => 64,
      Gclk7 => 128,
      Gclk8 => 256,
      Gclk9 => 512,
      Gclk10 => 1024,
      Gclk11 => 2048);

   --  Generic Clock Generator Control 10 Synchronization Busy bits
   type SYNCBUSY_GENCTRL10Select is
     (--  Generic clock generator 0
      Gclk0,
      --  Generic clock generator 1
      Gclk1,
      --  Generic clock generator 2
      Gclk2,
      --  Generic clock generator 3
      Gclk3,
      --  Generic clock generator 4
      Gclk4,
      --  Generic clock generator 5
      Gclk5,
      --  Generic clock generator 6
      Gclk6,
      --  Generic clock generator 7
      Gclk7,
      --  Generic clock generator 8
      Gclk8,
      --  Generic clock generator 9
      Gclk9,
      --  Generic clock generator 10
      Gclk10,
      --  Generic clock generator 11
      Gclk11)
     with Size => 12;
   for SYNCBUSY_GENCTRL10Select use
     (Gclk0 => 1,
      Gclk1 => 2,
      Gclk2 => 4,
      Gclk3 => 8,
      Gclk4 => 16,
      Gclk5 => 32,
      Gclk6 => 64,
      Gclk7 => 128,
      Gclk8 => 256,
      Gclk9 => 512,
      Gclk10 => 1024,
      Gclk11 => 2048);

   --  Generic Clock Generator Control 11 Synchronization Busy bits
   type SYNCBUSY_GENCTRL11Select is
     (--  Generic clock generator 0
      Gclk0,
      --  Generic clock generator 1
      Gclk1,
      --  Generic clock generator 2
      Gclk2,
      --  Generic clock generator 3
      Gclk3,
      --  Generic clock generator 4
      Gclk4,
      --  Generic clock generator 5
      Gclk5,
      --  Generic clock generator 6
      Gclk6,
      --  Generic clock generator 7
      Gclk7,
      --  Generic clock generator 8
      Gclk8,
      --  Generic clock generator 9
      Gclk9,
      --  Generic clock generator 10
      Gclk10,
      --  Generic clock generator 11
      Gclk11)
     with Size => 12;
   for SYNCBUSY_GENCTRL11Select use
     (Gclk0 => 1,
      Gclk1 => 2,
      Gclk2 => 4,
      Gclk3 => 8,
      Gclk4 => 16,
      Gclk5 => 32,
      Gclk6 => 64,
      Gclk7 => 128,
      Gclk8 => 256,
      Gclk9 => 512,
      Gclk10 => 1024,
      Gclk11 => 2048);

   --  Synchronization Busy
   type GCLK_SYNCBUSY_Register is record
      --  Read-only. Software Reset Synchroniation Busy bit
      SWRST          : Boolean;
      --  unspecified
      Reserved_1_1   : HAL.Bit;
      --  Read-only. Generic Clock Generator Control 0 Synchronization Busy
      --  bits
      GENCTRL        : HAL.UInt12;
      Reserved_14_31 : HAL.UInt18;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for GCLK_SYNCBUSY_Register use record
      SWRST          at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      GENCTRL        at 0 range 2 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  Source Select
   type GENCTRL_SRCSelect is
     (--  XOSC0 oscillator output
      Xosc0,
      --  XOSC1 oscillator output
      Xosc1,
      --  Generator input pad
      Gclkin,
      --  Generic clock generator 1 output
      Gclkgen1,
      --  OSCULP32K oscillator output
      Osculp32K,
      --  XOSC32K oscillator output
      Xosc32K,
      --  DFLL output
      Dfll,
      --  DPLL0 output
      Dpll0,
      --  DPLL1 output
      Dpll1)
     with Size => 4;
   for GENCTRL_SRCSelect use
     (Xosc0 => 0,
      Xosc1 => 1,
      Gclkin => 2,
      Gclkgen1 => 3,
      Osculp32K => 4,
      Xosc32K => 5,
      Dfll => 6,
      Dpll0 => 7,
      Dpll1 => 8);

   subtype GCLK_GENCTRL_DIV_Field is HAL.UInt16;

   --  Generic Clock Generator Control
   type GCLK_GENCTRL_Register is record
      --  Source Select
      SRC            : GENCTRL_SRCSelect := SAM_SVD.GCLK.Xosc0;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Generic Clock Generator Enable
      GENEN          : Boolean := False;
      --  Improve Duty Cycle
      IDC            : Boolean := False;
      --  Output Off Value
      OOV            : Boolean := False;
      --  Output Enable
      OE             : Boolean := False;
      --  Divide Selection
      DIVSEL         : Boolean := False;
      --  Run in Standby
      RUNSTDBY       : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Division Factor
      DIV            : GCLK_GENCTRL_DIV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for GCLK_GENCTRL_Register use record
      SRC            at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      GENEN          at 0 range 8 .. 8;
      IDC            at 0 range 9 .. 9;
      OOV            at 0 range 10 .. 10;
      OE             at 0 range 11 .. 11;
      DIVSEL         at 0 range 12 .. 12;
      RUNSTDBY       at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      DIV            at 0 range 16 .. 31;
   end record;

   --  Generic Clock Generator Control
   type GCLK_GENCTRL_Registers is array (0 .. 11) of GCLK_GENCTRL_Register;

   --  Generic Clock Generator
   type PCHCTRL_GENSelect is new HAL.UInt4
     with Size => 4;

   --  Peripheral Clock Control
   type GCLK_PCHCTRL_Register is record
      --  Generic Clock Generator
      GEN           : PCHCTRL_GENSelect;
      --  unspecified
      Reserved_4_5  : HAL.UInt2 := 16#0#;
      --  Channel Enable
      CHEN          : Boolean := False;
      --  Write Lock
      WRTLOCK       : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for GCLK_PCHCTRL_Register use record
      GEN           at 0 range 0 .. 3;
      Reserved_4_5  at 0 range 4 .. 5;
      CHEN          at 0 range 6 .. 6;
      WRTLOCK       at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Peripheral Clock Control
   type GCLK_PCHCTRL_Registers is array (0 .. 47) of GCLK_PCHCTRL_Register;

   -----------------
   -- Peripherals --
   -----------------

   --  Generic Clock Generator
   type GCLK_Peripheral is record
      --  Control
      CTRLA    : aliased GCLK_CTRLA_Register;
      --  Synchronization Busy
      SYNCBUSY : aliased GCLK_SYNCBUSY_Register;
      --  Generic Clock Generator Control
      GENCTRL  : aliased GCLK_GENCTRL_Registers;
      --  Peripheral Clock Control
      PCHCTRL  : aliased GCLK_PCHCTRL_Registers;
   end record
     with Volatile;

   for GCLK_Peripheral use record
      CTRLA    at 16#0# range 0 .. 7;
      SYNCBUSY at 16#4# range 0 .. 31;
      GENCTRL  at 16#20# range 0 .. 383;
      PCHCTRL  at 16#80# range 0 .. 1535;
   end record;

   --  Generic Clock Generator
   GCLK_Periph : aliased GCLK_Peripheral
     with Import, Address => GCLK_Base;

end SAM_SVD.GCLK;
