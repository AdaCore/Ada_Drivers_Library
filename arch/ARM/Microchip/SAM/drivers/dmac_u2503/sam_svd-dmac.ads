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

package SAM_SVD.DMAC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  DMAC_CTRL_LVLEN array
   type DMAC_CTRL_LVLEN_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for DMAC_CTRL_LVLEN
   type DMAC_CTRL_LVLEN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LVLEN as a value
            Val : HAL.UInt4;
         when True =>
            --  LVLEN as an array
            Arr : DMAC_CTRL_LVLEN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for DMAC_CTRL_LVLEN_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Control
   type DMAC_CTRL_Register is record
      --  Software Reset
      SWRST          : Boolean := False;
      --  DMA Enable
      DMAENABLE      : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Priority Level 0 Enable
      LVLEN          : DMAC_CTRL_LVLEN_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for DMAC_CTRL_Register use record
      SWRST          at 0 range 0 .. 0;
      DMAENABLE      at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      LVLEN          at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
   end record;

   --  CRC Beat Size
   type CRCCTRL_CRCBEATSIZESelect is
     (--  8-bit bus transfer
      Byte,
      --  16-bit bus transfer
      Hword,
      --  32-bit bus transfer
      Word)
     with Size => 2;
   for CRCCTRL_CRCBEATSIZESelect use
     (Byte => 0,
      Hword => 1,
      Word => 2);

   --  CRC Polynomial Type
   type CRCCTRL_CRCPOLYSelect is
     (--  CRC-16 (CRC-CCITT)
      Crc16,
      --  CRC32 (IEEE 802.3)
      Crc32)
     with Size => 2;
   for CRCCTRL_CRCPOLYSelect use
     (Crc16 => 0,
      Crc32 => 1);

   --  CRC Input Source
   type CRCCTRL_CRCSRCSelect is
     (--  CRC Disabled
      Disable,
      --  I/O interface
      Io)
     with Size => 6;
   for CRCCTRL_CRCSRCSelect use
     (Disable => 0,
      Io => 1);

   --  CRC Operating Mode
   type CRCCTRL_CRCMODESelect is
     (--  Default operating mode
      Default,
      --  Memory CRC monitor operating mode
      Crcmon,
      --  Memory CRC generation operating mode
      Crcgen)
     with Size => 2;
   for CRCCTRL_CRCMODESelect use
     (Default => 0,
      Crcmon => 2,
      Crcgen => 3);

   --  CRC Control
   type DMAC_CRCCTRL_Register is record
      --  CRC Beat Size
      CRCBEATSIZE  : CRCCTRL_CRCBEATSIZESelect := SAM_SVD.DMAC.Byte;
      --  CRC Polynomial Type
      CRCPOLY      : CRCCTRL_CRCPOLYSelect := SAM_SVD.DMAC.Crc16;
      --  unspecified
      Reserved_4_7 : HAL.UInt4 := 16#0#;
      --  CRC Input Source
      CRCSRC       : CRCCTRL_CRCSRCSelect := SAM_SVD.DMAC.Disable;
      --  CRC Operating Mode
      CRCMODE      : CRCCTRL_CRCMODESelect := SAM_SVD.DMAC.Default;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for DMAC_CRCCTRL_Register use record
      CRCBEATSIZE  at 0 range 0 .. 1;
      CRCPOLY      at 0 range 2 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      CRCSRC       at 0 range 8 .. 13;
      CRCMODE      at 0 range 14 .. 15;
   end record;

   --  CRC Status
   type DMAC_CRCSTATUS_Register is record
      --  CRC Module Busy
      CRCBUSY      : Boolean := False;
      --  Read-only. CRC Zero
      CRCZERO      : Boolean := False;
      --  Read-only. CRC Error
      CRCERR       : Boolean := False;
      --  unspecified
      Reserved_3_7 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for DMAC_CRCSTATUS_Register use record
      CRCBUSY      at 0 range 0 .. 0;
      CRCZERO      at 0 range 1 .. 1;
      CRCERR       at 0 range 2 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
   end record;

   --  Debug Control
   type DMAC_DBGCTRL_Register is record
      --  Debug Run
      DBGRUN       : Boolean := False;
      --  unspecified
      Reserved_1_7 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for DMAC_DBGCTRL_Register use record
      DBGRUN       at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
   end record;

   --  DMAC_SWTRIGCTRL_SWTRIG array
   type DMAC_SWTRIGCTRL_SWTRIG_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Software Trigger Control
   type DMAC_SWTRIGCTRL_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWTRIG as a value
            Val : HAL.UInt32;
         when True =>
            --  SWTRIG as an array
            Arr : DMAC_SWTRIGCTRL_SWTRIG_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for DMAC_SWTRIGCTRL_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype DMAC_PRICTRL0_LVLPRI0_Field is HAL.UInt5;

   --  Level 0 Quality of Service
   type PRICTRL0_QOS0Select is
     (--  Regular delivery
      Regular,
      --  Bandwidth shortage
      Shortage,
      --  Latency sensitive
      Sensitive,
      --  Latency critical
      Critical)
     with Size => 2;
   for PRICTRL0_QOS0Select use
     (Regular => 0,
      Shortage => 1,
      Sensitive => 2,
      Critical => 3);

   subtype DMAC_PRICTRL0_LVLPRI1_Field is HAL.UInt5;

   --  Level 1 Quality of Service
   type PRICTRL0_QOS1Select is
     (--  Regular delivery
      Regular,
      --  Bandwidth shortage
      Shortage,
      --  Latency sensitive
      Sensitive,
      --  Latency critical
      Critical)
     with Size => 2;
   for PRICTRL0_QOS1Select use
     (Regular => 0,
      Shortage => 1,
      Sensitive => 2,
      Critical => 3);

   subtype DMAC_PRICTRL0_LVLPRI2_Field is HAL.UInt5;

   --  Level 2 Quality of Service
   type PRICTRL0_QOS2Select is
     (--  Regular delivery
      Regular,
      --  Bandwidth shortage
      Shortage,
      --  Latency sensitive
      Sensitive,
      --  Latency critical
      Critical)
     with Size => 2;
   for PRICTRL0_QOS2Select use
     (Regular => 0,
      Shortage => 1,
      Sensitive => 2,
      Critical => 3);

   subtype DMAC_PRICTRL0_LVLPRI3_Field is HAL.UInt5;

   --  Level 3 Quality of Service
   type PRICTRL0_QOS3Select is
     (--  Regular delivery
      Regular,
      --  Bandwidth shortage
      Shortage,
      --  Latency sensitive
      Sensitive,
      --  Latency critical
      Critical)
     with Size => 2;
   for PRICTRL0_QOS3Select use
     (Regular => 0,
      Shortage => 1,
      Sensitive => 2,
      Critical => 3);

   --  Priority Control 0
   type DMAC_PRICTRL0_Register is record
      --  Level 0 Channel Priority Number
      LVLPRI0  : DMAC_PRICTRL0_LVLPRI0_Field := 16#0#;
      --  Level 0 Quality of Service
      QOS0     : PRICTRL0_QOS0Select := SAM_SVD.DMAC.Sensitive;
      --  Level 0 Round-Robin Scheduling Enable
      RRLVLEN0 : Boolean := False;
      --  Level 1 Channel Priority Number
      LVLPRI1  : DMAC_PRICTRL0_LVLPRI1_Field := 16#0#;
      --  Level 1 Quality of Service
      QOS1     : PRICTRL0_QOS1Select := SAM_SVD.DMAC.Sensitive;
      --  Level 1 Round-Robin Scheduling Enable
      RRLVLEN1 : Boolean := False;
      --  Level 2 Channel Priority Number
      LVLPRI2  : DMAC_PRICTRL0_LVLPRI2_Field := 16#0#;
      --  Level 2 Quality of Service
      QOS2     : PRICTRL0_QOS2Select := SAM_SVD.DMAC.Sensitive;
      --  Level 2 Round-Robin Scheduling Enable
      RRLVLEN2 : Boolean := False;
      --  Level 3 Channel Priority Number
      LVLPRI3  : DMAC_PRICTRL0_LVLPRI3_Field := 16#0#;
      --  Level 3 Quality of Service
      QOS3     : PRICTRL0_QOS3Select := SAM_SVD.DMAC.Sensitive;
      --  Level 3 Round-Robin Scheduling Enable
      RRLVLEN3 : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAC_PRICTRL0_Register use record
      LVLPRI0  at 0 range 0 .. 4;
      QOS0     at 0 range 5 .. 6;
      RRLVLEN0 at 0 range 7 .. 7;
      LVLPRI1  at 0 range 8 .. 12;
      QOS1     at 0 range 13 .. 14;
      RRLVLEN1 at 0 range 15 .. 15;
      LVLPRI2  at 0 range 16 .. 20;
      QOS2     at 0 range 21 .. 22;
      RRLVLEN2 at 0 range 23 .. 23;
      LVLPRI3  at 0 range 24 .. 28;
      QOS3     at 0 range 29 .. 30;
      RRLVLEN3 at 0 range 31 .. 31;
   end record;

   subtype DMAC_INTPEND_ID_Field is HAL.UInt5;

   --  Interrupt Pending
   type DMAC_INTPEND_Register is record
      --  Channel ID
      ID             : DMAC_INTPEND_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Transfer Error
      TERR           : Boolean := False;
      --  Transfer Complete
      TCMPL          : Boolean := False;
      --  Channel Suspend
      SUSP           : Boolean := False;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  CRC Error
      CRCERR         : Boolean := False;
      --  Read-only. Fetch Error
      FERR           : Boolean := False;
      --  Read-only. Busy
      BUSY           : Boolean := False;
      --  Read-only. Pending
      PEND           : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for DMAC_INTPEND_Register use record
      ID             at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      TERR           at 0 range 8 .. 8;
      TCMPL          at 0 range 9 .. 9;
      SUSP           at 0 range 10 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      CRCERR         at 0 range 12 .. 12;
      FERR           at 0 range 13 .. 13;
      BUSY           at 0 range 14 .. 14;
      PEND           at 0 range 15 .. 15;
   end record;

   --  DMAC_INTSTATUS_CHINT array
   type DMAC_INTSTATUS_CHINT_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Interrupt Status
   type DMAC_INTSTATUS_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CHINT as a value
            Val : HAL.UInt32;
         when True =>
            --  CHINT as an array
            Arr : DMAC_INTSTATUS_CHINT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for DMAC_INTSTATUS_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  DMAC_BUSYCH_BUSYCH array
   type DMAC_BUSYCH_BUSYCH_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Busy Channels
   type DMAC_BUSYCH_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  BUSYCH as a value
            Val : HAL.UInt32;
         when True =>
            --  BUSYCH as an array
            Arr : DMAC_BUSYCH_BUSYCH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for DMAC_BUSYCH_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  DMAC_PENDCH_PENDCH array
   type DMAC_PENDCH_PENDCH_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Pending Channels
   type DMAC_PENDCH_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PENDCH as a value
            Val : HAL.UInt32;
         when True =>
            --  PENDCH as an array
            Arr : DMAC_PENDCH_PENDCH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for DMAC_PENDCH_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  DMAC_ACTIVE_LVLEX array
   type DMAC_ACTIVE_LVLEX_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for DMAC_ACTIVE_LVLEX
   type DMAC_ACTIVE_LVLEX_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LVLEX as a value
            Val : HAL.UInt4;
         when True =>
            --  LVLEX as an array
            Arr : DMAC_ACTIVE_LVLEX_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for DMAC_ACTIVE_LVLEX_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   subtype DMAC_ACTIVE_ID_Field is HAL.UInt5;
   subtype DMAC_ACTIVE_BTCNT_Field is HAL.UInt16;

   --  Active Channel and Levels
   type DMAC_ACTIVE_Register is record
      --  Read-only. Level 0 Channel Trigger Request Executing
      LVLEX          : DMAC_ACTIVE_LVLEX_Field;
      --  unspecified
      Reserved_4_7   : HAL.UInt4;
      --  Read-only. Active Channel ID
      ID             : DMAC_ACTIVE_ID_Field;
      --  unspecified
      Reserved_13_14 : HAL.UInt2;
      --  Read-only. Active Channel Busy
      ABUSY          : Boolean;
      --  Read-only. Active Channel Block Transfer Count
      BTCNT          : DMAC_ACTIVE_BTCNT_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAC_ACTIVE_Register use record
      LVLEX          at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      ID             at 0 range 8 .. 12;
      Reserved_13_14 at 0 range 13 .. 14;
      ABUSY          at 0 range 15 .. 15;
      BTCNT          at 0 range 16 .. 31;
   end record;

   --  Trigger Source
   subtype CHCTRLA0_TRIGSRCSelect is HAL.UInt7;

   --  Trigger Action
   type CHCTRLA0_TRIGACTSelect is
     (--  One trigger required for each block transfer
      Block,
      --  One trigger required for each burst transfer
      Burst,
      --  One trigger required for each transaction
      Transaction)
     with Size => 2;
   for CHCTRLA0_TRIGACTSelect use
     (Block => 0,
      Burst => 2,
      Transaction => 3);

   --  Burst Length
   type CHCTRLA0_BURSTLENSelect is
     (--  Single-beat burst length
      Single,
      --  2-beats burst length
      Val_2Beat,
      --  3-beats burst length
      Val_3Beat,
      --  4-beats burst length
      Val_4Beat,
      --  5-beats burst length
      Val_5Beat,
      --  6-beats burst length
      Val_6Beat,
      --  7-beats burst length
      Val_7Beat,
      --  8-beats burst length
      Val_8Beat,
      --  9-beats burst length
      Val_9Beat,
      --  10-beats burst length
      Val_10Beat,
      --  11-beats burst length
      Val_11Beat,
      --  12-beats burst length
      Val_12Beat,
      --  13-beats burst length
      Val_13Beat,
      --  14-beats burst length
      Val_14Beat,
      --  15-beats burst length
      Val_15Beat,
      --  16-beats burst length
      Val_16Beat)
     with Size => 4;
   for CHCTRLA0_BURSTLENSelect use
     (Single => 0,
      Val_2Beat => 1,
      Val_3Beat => 2,
      Val_4Beat => 3,
      Val_5Beat => 4,
      Val_6Beat => 5,
      Val_7Beat => 6,
      Val_8Beat => 7,
      Val_9Beat => 8,
      Val_10Beat => 9,
      Val_11Beat => 10,
      Val_12Beat => 11,
      Val_13Beat => 12,
      Val_14Beat => 13,
      Val_15Beat => 14,
      Val_16Beat => 15);

   --  FIFO Threshold
   type CHCTRLA0_THRESHOLDSelect is
     (--  Destination write starts after each beat source address read
      Val_1Beat,
      --  Destination write starts after 2-beats source address read
      Val_2Beats,
      --  Destination write starts after 4-beats source address read
      Val_4Beats,
      --  Destination write starts after 8-beats source address read
      Val_8Beats)
     with Size => 2;
   for CHCTRLA0_THRESHOLDSelect use
     (Val_1Beat => 0,
      Val_2Beats => 1,
      Val_4Beats => 2,
      Val_8Beats => 3);

   --  Channel n Control A
   type CHCTRLA_Register is record
      --  Channel Software Reset
      SWRST          : Boolean := False;
      --  Channel Enable
      ENABLE         : Boolean := False;
      --  unspecified
      Reserved_2_5   : HAL.UInt4 := 16#0#;
      --  Channel Run in Standby
      RUNSTDBY       : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Trigger Source
      TRIGSRC        : CHCTRLA0_TRIGSRCSelect := 0;
      --  unspecified
      Reserved_15_19 : HAL.UInt5 := 16#0#;
      --  Trigger Action
      TRIGACT        : CHCTRLA0_TRIGACTSelect := SAM_SVD.DMAC.Block;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Burst Length
      BURSTLEN       : CHCTRLA0_BURSTLENSelect := SAM_SVD.DMAC.Single;
      --  FIFO Threshold
      THRESHOLD      : CHCTRLA0_THRESHOLDSelect := SAM_SVD.DMAC.Val_1Beat;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CHCTRLA_Register use record
      SWRST          at 0 range 0 .. 0;
      ENABLE         at 0 range 1 .. 1;
      Reserved_2_5   at 0 range 2 .. 5;
      RUNSTDBY       at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      TRIGSRC        at 0 range 8 .. 14;
      Reserved_15_19 at 0 range 15 .. 19;
      TRIGACT        at 0 range 20 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      BURSTLEN       at 0 range 24 .. 27;
      THRESHOLD      at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  Software Command
   type CHCTRLB0_CMDSelect is
     (--  No action
      Noact,
      --  Channel suspend operation
      Suspend,
      --  Channel resume operation
      Resume)
     with Size => 2;
   for CHCTRLB0_CMDSelect use
     (Noact => 0,
      Suspend => 1,
      Resume => 2);

   --  Channel n Control B
   type CHCTRLB_Register is record
      --  Software Command
      CMD          : CHCTRLB0_CMDSelect := SAM_SVD.DMAC.Noact;
      --  unspecified
      Reserved_2_7 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for CHCTRLB_Register use record
      CMD          at 0 range 0 .. 1;
      Reserved_2_7 at 0 range 2 .. 7;
   end record;

   --  Channel Priority Level
   type CHPRILVL0_PRILVLSelect is
     (--  Channel Priority Level 0 (Lowest Level)
      Lvl0,
      --  Channel Priority Level 1
      Lvl1,
      --  Channel Priority Level 2
      Lvl2,
      --  Channel Priority Level 3  (Highest Level)
      Lvl3)
     with Size => 2;
   for CHPRILVL0_PRILVLSelect use
     (Lvl0 => 0,
      Lvl1 => 1,
      Lvl2 => 2,
      Lvl3 => 3);

   --  Channel n Priority Level
   type CHPRILVL_Register is record
      --  Channel Priority Level
      PRILVL       : CHPRILVL0_PRILVLSelect := SAM_SVD.DMAC.Lvl0;
      --  unspecified
      Reserved_2_7 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for CHPRILVL_Register use record
      PRILVL       at 0 range 0 .. 1;
      Reserved_2_7 at 0 range 2 .. 7;
   end record;

   --  Channel Event Input Action
   type CHEVCTRL0_EVACTSelect is
     (--  No action
      Noact,
      --  Transfer and periodic transfer trigger
      Trig,
      --  Conditional transfer trigger
      Ctrig,
      --  Conditional block transfer
      Cblock,
      --  Channel suspend operation
      Suspend,
      --  Channel resume operation
      Resume,
      --  Skip next block suspend action
      Sskip,
      --  Increase priority
      Incpri)
     with Size => 3;
   for CHEVCTRL0_EVACTSelect use
     (Noact => 0,
      Trig => 1,
      Ctrig => 2,
      Cblock => 3,
      Suspend => 4,
      Resume => 5,
      Sskip => 6,
      Incpri => 7);

   --  Channel Event Output Mode
   type CHEVCTRL0_EVOMODESelect is
     (--  Block event output selection. Refer to BTCTRL.EVOSEL for available
--  selections.
      Default,
      --  Ongoing trigger action
      Trigact)
     with Size => 2;
   for CHEVCTRL0_EVOMODESelect use
     (Default => 0,
      Trigact => 1);

   --  Channel n Event Control
   type CHEVCTRL_Register is record
      --  Channel Event Input Action
      EVACT        : CHEVCTRL0_EVACTSelect := SAM_SVD.DMAC.Noact;
      --  unspecified
      Reserved_3_3 : HAL.Bit := 16#0#;
      --  Channel Event Output Mode
      EVOMODE      : CHEVCTRL0_EVOMODESelect := SAM_SVD.DMAC.Default;
      --  Channel Event Input Enable
      EVIE         : Boolean := False;
      --  Channel Event Output Enable
      EVOE         : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for CHEVCTRL_Register use record
      EVACT        at 0 range 0 .. 2;
      Reserved_3_3 at 0 range 3 .. 3;
      EVOMODE      at 0 range 4 .. 5;
      EVIE         at 0 range 6 .. 6;
      EVOE         at 0 range 7 .. 7;
   end record;

   --  Channel n Interrupt Enable Clear
   type CHINTENCLR_Register is record
      --  Channel Transfer Error Interrupt Enable
      TERR         : Boolean := False;
      --  Channel Transfer Complete Interrupt Enable
      TCMPL        : Boolean := False;
      --  Channel Suspend Interrupt Enable
      SUSP         : Boolean := False;
      --  unspecified
      Reserved_3_7 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for CHINTENCLR_Register use record
      TERR         at 0 range 0 .. 0;
      TCMPL        at 0 range 1 .. 1;
      SUSP         at 0 range 2 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
   end record;

   --  Channel n Interrupt Enable Set
   type CHINTENSET_Register is record
      --  Channel Transfer Error Interrupt Enable
      TERR         : Boolean := False;
      --  Channel Transfer Complete Interrupt Enable
      TCMPL        : Boolean := False;
      --  Channel Suspend Interrupt Enable
      SUSP         : Boolean := False;
      --  unspecified
      Reserved_3_7 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for CHINTENSET_Register use record
      TERR         at 0 range 0 .. 0;
      TCMPL        at 0 range 1 .. 1;
      SUSP         at 0 range 2 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
   end record;

   --  Channel n Interrupt Flag Status and Clear
   type CHINTFLAG_Register is record
      --  Channel Transfer Error
      TERR         : Boolean := False;
      --  Channel Transfer Complete
      TCMPL        : Boolean := False;
      --  Channel Suspend
      SUSP         : Boolean := False;
      --  unspecified
      Reserved_3_7 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for CHINTFLAG_Register use record
      TERR         at 0 range 0 .. 0;
      TCMPL        at 0 range 1 .. 1;
      SUSP         at 0 range 2 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
   end record;

   --  Channel n Status
   type CHSTATUS_Register is record
      --  Read-only. Channel Pending
      PEND         : Boolean := False;
      --  Read-only. Channel Busy
      BUSY         : Boolean := False;
      --  Read-only. Channel Fetch Error
      FERR         : Boolean := False;
      --  Channel CRC Error
      CRCERR       : Boolean := False;
      --  unspecified
      Reserved_4_7 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for CHSTATUS_Register use record
      PEND         at 0 range 0 .. 0;
      BUSY         at 0 range 1 .. 1;
      FERR         at 0 range 2 .. 2;
      CRCERR       at 0 range 3 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
   end record;

   type Channel_Record is record
      --  Channel n Control A
      CHCTRLA     : aliased CHCTRLA_Register;
      --  Channel n Control B
      CHCTRLB     : aliased CHCTRLB_Register;
      --  Channel n Priority Level
      CHPRILVL    : aliased CHPRILVL_Register;
      --  Channel n Event Control
      CHEVCTRL    : aliased CHEVCTRL_Register;
      --  Channel n Interrupt Enable Clear
      CHINTENCLR  : aliased CHINTENCLR_Register;
      --  Channel n Interrupt Enable Set
      CHINTENSET  : aliased CHINTENSET_Register;
      --  Channel n Interrupt Flag Status and Clear
      CHINTFLAG   : aliased CHINTFLAG_Register;
      --  Channel n Status
      CHSTATUS    : aliased CHSTATUS_Register;
   end record;

   for Channel_Record use record
      CHCTRLA     at 16#00# range 0 .. 31;
      CHCTRLB     at 16#04# range 0 .. 7;
      CHPRILVL    at 16#05# range 0 .. 7;
      CHEVCTRL    at 16#06# range 0 .. 7;
      CHINTENCLR  at 16#0C# range 0 .. 7;
      CHINTENSET  at 16#0D# range 0 .. 7;
      CHINTFLAG   at 16#0E# range 0 .. 7;
      CHSTATUS    at 16#0F# range 0 .. 7;
   end record;

   type Channel_Array is array (0 .. 31) of Channel_Record;

   -----------------
   -- Peripherals --
   -----------------

   --  Direct Memory Access Controller
   type DMAC_Peripheral is record
      --  Control
      CTRL         : aliased DMAC_CTRL_Register;
      --  CRC Control
      CRCCTRL      : aliased DMAC_CRCCTRL_Register;
      --  CRC Data Input
      CRCDATAIN    : aliased HAL.UInt32;
      --  CRC Checksum
      CRCCHKSUM    : aliased HAL.UInt32;
      --  CRC Status
      CRCSTATUS    : aliased DMAC_CRCSTATUS_Register;
      --  Debug Control
      DBGCTRL      : aliased DMAC_DBGCTRL_Register;
      --  Software Trigger Control
      SWTRIGCTRL   : aliased DMAC_SWTRIGCTRL_Register;
      --  Priority Control 0
      PRICTRL0     : aliased DMAC_PRICTRL0_Register;
      --  Interrupt Pending
      INTPEND      : aliased DMAC_INTPEND_Register;
      --  Interrupt Status
      INTSTATUS    : aliased DMAC_INTSTATUS_Register;
      --  Busy Channels
      BUSYCH       : aliased DMAC_BUSYCH_Register;
      --  Pending Channels
      PENDCH       : aliased DMAC_PENDCH_Register;
      --  Active Channel and Levels
      ACTIVE       : aliased DMAC_ACTIVE_Register;
      --  Descriptor Memory Section Base Address
      BASEADDR     : aliased System.Address;
      --  Write-Back Memory Section Base Address
      WRBADDR      : aliased System.Address;

      Channels     : aliased Channel_Array;
   end record
     with Volatile;

   for DMAC_Peripheral use record
      CTRL         at 16#0# range 0 .. 15;
      CRCCTRL      at 16#2# range 0 .. 15;
      CRCDATAIN    at 16#4# range 0 .. 31;
      CRCCHKSUM    at 16#8# range 0 .. 31;
      CRCSTATUS    at 16#C# range 0 .. 7;
      DBGCTRL      at 16#D# range 0 .. 7;
      SWTRIGCTRL   at 16#10# range 0 .. 31;
      PRICTRL0     at 16#14# range 0 .. 31;
      INTPEND      at 16#20# range 0 .. 15;
      INTSTATUS    at 16#24# range 0 .. 31;
      BUSYCH       at 16#28# range 0 .. 31;
      PENDCH       at 16#2C# range 0 .. 31;
      ACTIVE       at 16#30# range 0 .. 31;
      BASEADDR     at 16#34# range 0 .. 31;
      WRBADDR      at 16#38# range 0 .. 31;
      Channels     at 16#40# range 0 .. 4095;
   end record;

   --  Direct Memory Access Controller
   DMAC_Periph : aliased DMAC_Peripheral
     with Import, Address => DMAC_Base;

end SAM_SVD.DMAC;
