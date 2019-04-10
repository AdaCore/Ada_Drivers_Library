--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.SMC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -------------------------------------------
   -- SMC_SMC_CS_NUMBER cluster's Registers --
   -------------------------------------------

   subtype SMC_SMC_SETUP_SMC_SMC_CS_NUMBER_NWE_SETUP_Field is HAL.UInt6;
   subtype SMC_SMC_SETUP_SMC_SMC_CS_NUMBER_NCS_WR_SETUP_Field is HAL.UInt6;
   subtype SMC_SMC_SETUP_SMC_SMC_CS_NUMBER_NRD_SETUP_Field is HAL.UInt6;
   subtype SMC_SMC_SETUP_SMC_SMC_CS_NUMBER_NCS_RD_SETUP_Field is HAL.UInt6;

   --  SMC Setup Register (CS_number = 0)
   type SMC_SMC_SETUP_SMC_SMC_CS_NUMBER_Register is record
      --  NWE Setup Length
      NWE_SETUP      : SMC_SMC_SETUP_SMC_SMC_CS_NUMBER_NWE_SETUP_Field :=
                        16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  NCS Setup Length in WRITE Access
      NCS_WR_SETUP   : SMC_SMC_SETUP_SMC_SMC_CS_NUMBER_NCS_WR_SETUP_Field :=
                        16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  NRD Setup Length
      NRD_SETUP      : SMC_SMC_SETUP_SMC_SMC_CS_NUMBER_NRD_SETUP_Field :=
                        16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  NCS Setup Length in READ Access
      NCS_RD_SETUP   : SMC_SMC_SETUP_SMC_SMC_CS_NUMBER_NCS_RD_SETUP_Field :=
                        16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMC_SMC_SETUP_SMC_SMC_CS_NUMBER_Register use record
      NWE_SETUP      at 0 range 0 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      NCS_WR_SETUP   at 0 range 8 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      NRD_SETUP      at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      NCS_RD_SETUP   at 0 range 24 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype SMC_SMC_PULSE_SMC_SMC_CS_NUMBER_NWE_PULSE_Field is HAL.UInt7;
   subtype SMC_SMC_PULSE_SMC_SMC_CS_NUMBER_NCS_WR_PULSE_Field is HAL.UInt7;
   subtype SMC_SMC_PULSE_SMC_SMC_CS_NUMBER_NRD_PULSE_Field is HAL.UInt7;
   subtype SMC_SMC_PULSE_SMC_SMC_CS_NUMBER_NCS_RD_PULSE_Field is HAL.UInt7;

   --  SMC Pulse Register (CS_number = 0)
   type SMC_SMC_PULSE_SMC_SMC_CS_NUMBER_Register is record
      --  NWE Pulse Length
      NWE_PULSE      : SMC_SMC_PULSE_SMC_SMC_CS_NUMBER_NWE_PULSE_Field :=
                        16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  NCS Pulse Length in WRITE Access
      NCS_WR_PULSE   : SMC_SMC_PULSE_SMC_SMC_CS_NUMBER_NCS_WR_PULSE_Field :=
                        16#0#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  NRD Pulse Length
      NRD_PULSE      : SMC_SMC_PULSE_SMC_SMC_CS_NUMBER_NRD_PULSE_Field :=
                        16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  NCS Pulse Length in READ Access
      NCS_RD_PULSE   : SMC_SMC_PULSE_SMC_SMC_CS_NUMBER_NCS_RD_PULSE_Field :=
                        16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMC_SMC_PULSE_SMC_SMC_CS_NUMBER_Register use record
      NWE_PULSE      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      NCS_WR_PULSE   at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      NRD_PULSE      at 0 range 16 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      NCS_RD_PULSE   at 0 range 24 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype SMC_SMC_CYCLE_SMC_SMC_CS_NUMBER_NWE_CYCLE_Field is HAL.UInt9;
   subtype SMC_SMC_CYCLE_SMC_SMC_CS_NUMBER_NRD_CYCLE_Field is HAL.UInt9;

   --  SMC Cycle Register (CS_number = 0)
   type SMC_SMC_CYCLE_SMC_SMC_CS_NUMBER_Register is record
      --  Total Write Cycle Length
      NWE_CYCLE      : SMC_SMC_CYCLE_SMC_SMC_CS_NUMBER_NWE_CYCLE_Field :=
                        16#0#;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  Total Read Cycle Length
      NRD_CYCLE      : SMC_SMC_CYCLE_SMC_SMC_CS_NUMBER_NRD_CYCLE_Field :=
                        16#0#;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMC_SMC_CYCLE_SMC_SMC_CS_NUMBER_Register use record
      NWE_CYCLE      at 0 range 0 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      NRD_CYCLE      at 0 range 16 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  NWAIT Mode
   type SMC_MODE_EXNW_MODE_Field is
     (
      --  Disabled-The NWAIT input signal is ignored on the corresponding chip
      --  select.
      Disabled,
      --  Frozen Mode-If asserted, the NWAIT signal freezes the current read or
      --  write cycle. After deassertion, the read/write cycle is resumed from
      --  the point where it was stopped.
      Frozen,
      --  Ready Mode-The NWAIT signal indicates the availability of the
      --  external device at the end of the pulse of the controlling read or
      --  write signal, to complete the access. If high, the access normally
      --  completes. If low, the access is extended until NWAIT returns high.
      Ready)
     with Size => 2;
   for SMC_MODE_EXNW_MODE_Field use
     (Disabled => 0,
      Frozen => 2,
      Ready => 3);

   --  Byte Access Type
   type SMC_MODE_BAT_Field is
     (
      --  Byte select access type:- Write operation is controlled using NCS,
      --  NWE, NBS0, NBS1.- Read operation is controlled using NCS, NRD, NBS0,
      --  NBS1.
      Byte_Select,
      --  Byte write access type:- Write operation is controlled using NCS,
      --  NWR0, NWR1.- Read operation is controlled using NCS and NRD.
      Byte_Write)
     with Size => 1;
   for SMC_MODE_BAT_Field use
     (Byte_Select => 0,
      Byte_Write => 1);

   --  Data Bus Width
   type SMC_MODE_DBW_Field is
     (
      --  8-bit Data Bus
      Val_8_Bit,
      --  16-bit Data Bus
      Val_16_Bit)
     with Size => 1;
   for SMC_MODE_DBW_Field use
     (Val_8_Bit => 0,
      Val_16_Bit => 1);

   subtype SMC_SMC_MODE_SMC_SMC_CS_NUMBER_TDF_CYCLES_Field is HAL.UInt4;

   --  Page Size
   type SMC_MODE_PS_Field is
     (
      --  4-byte page
      Val_4_Byte,
      --  8-byte page
      Val_8_Byte,
      --  16-byte page
      Val_16_Byte,
      --  32-byte page
      Val_32_Byte)
     with Size => 2;
   for SMC_MODE_PS_Field use
     (Val_4_Byte => 0,
      Val_8_Byte => 1,
      Val_16_Byte => 2,
      Val_32_Byte => 3);

   --  SMC Mode Register (CS_number = 0)
   type SMC_SMC_MODE_SMC_SMC_CS_NUMBER_Register is record
      --  Read Mode
      READ_MODE      : Boolean := False;
      --  Write Mode
      WRITE_MODE     : Boolean := False;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  NWAIT Mode
      EXNW_MODE      : SMC_MODE_EXNW_MODE_Field := SAM_SVD.SMC.Disabled;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Byte Access Type
      BAT            : SMC_MODE_BAT_Field := SAM_SVD.SMC.Byte_Select;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  Data Bus Width
      DBW            : SMC_MODE_DBW_Field := SAM_SVD.SMC.Val_8_Bit;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  Data Float Time
      TDF_CYCLES     : SMC_SMC_MODE_SMC_SMC_CS_NUMBER_TDF_CYCLES_Field :=
                        16#0#;
      --  TDF Optimization
      TDF_MODE       : Boolean := False;
      --  unspecified
      Reserved_21_23 : HAL.UInt3 := 16#0#;
      --  Page Mode Enabled
      PMEN           : Boolean := False;
      --  unspecified
      Reserved_25_27 : HAL.UInt3 := 16#0#;
      --  Page Size
      PS             : SMC_MODE_PS_Field := SAM_SVD.SMC.Val_4_Byte;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMC_SMC_MODE_SMC_SMC_CS_NUMBER_Register use record
      READ_MODE      at 0 range 0 .. 0;
      WRITE_MODE     at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      EXNW_MODE      at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      BAT            at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      DBW            at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      TDF_CYCLES     at 0 range 16 .. 19;
      TDF_MODE       at 0 range 20 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      PMEN           at 0 range 24 .. 24;
      Reserved_25_27 at 0 range 25 .. 27;
      PS             at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  SMC Setup Register (CS_number = 0)
   type SMC_SMC_CS_NUMBER_Cluster is record
      --  SMC Setup Register (CS_number = 0)
      SMC_SETUP : aliased SMC_SMC_SETUP_SMC_SMC_CS_NUMBER_Register;
      --  SMC Pulse Register (CS_number = 0)
      SMC_PULSE : aliased SMC_SMC_PULSE_SMC_SMC_CS_NUMBER_Register;
      --  SMC Cycle Register (CS_number = 0)
      SMC_CYCLE : aliased SMC_SMC_CYCLE_SMC_SMC_CS_NUMBER_Register;
      --  SMC Mode Register (CS_number = 0)
      SMC_MODE  : aliased SMC_SMC_MODE_SMC_SMC_CS_NUMBER_Register;
   end record
     with Volatile, Size => 128;

   for SMC_SMC_CS_NUMBER_Cluster use record
      SMC_SETUP at 16#0# range 0 .. 31;
      SMC_PULSE at 16#4# range 0 .. 31;
      SMC_CYCLE at 16#8# range 0 .. 31;
      SMC_MODE  at 16#C# range 0 .. 31;
   end record;

   --  SMC Setup Register (CS_number = 0)
   type SMC_SMC_CS_NUMBER_Clusters is array (0 .. 3)
     of SMC_SMC_CS_NUMBER_Cluster;

   --  SMC Off-Chip Memory Scrambling Register
   type SMC_SMC_OCMS_Register is record
      --  Static Memory Controller Scrambling Enable
      SMSE           : Boolean := False;
      --  unspecified
      Reserved_1_7   : HAL.UInt7 := 16#0#;
      --  Chip Select (x = 0 to 3) Scrambling Enable
      CS0SE          : Boolean := False;
      --  Chip Select (x = 0 to 3) Scrambling Enable
      CS1SE          : Boolean := False;
      --  Chip Select (x = 0 to 3) Scrambling Enable
      CS2SE          : Boolean := False;
      --  Chip Select (x = 0 to 3) Scrambling Enable
      CS3SE          : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMC_SMC_OCMS_Register use record
      SMSE           at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      CS0SE          at 0 range 8 .. 8;
      CS1SE          at 0 range 9 .. 9;
      CS2SE          at 0 range 10 .. 10;
      CS3SE          at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Write Protection Key
   type SMC_WPMR_WPKEY_Field is
     (
      --  Reset value for the field
      Smc_Wpmr_Wpkey_Field_Reset,
      --  Writing any other value in this field aborts the write operation of
      --  the WPEN bit. Always reads as 0.
      Passwd)
     with Size => 24;
   for SMC_WPMR_WPKEY_Field use
     (Smc_Wpmr_Wpkey_Field_Reset => 0,
      Passwd => 5459267);

   --  SMC Write Protection Mode Register
   type SMC_SMC_WPMR_Register is record
      --  Write Protect Enable
      WPEN         : Boolean := False;
      --  unspecified
      Reserved_1_7 : HAL.UInt7 := 16#0#;
      --  Write Protection Key
      WPKEY        : SMC_WPMR_WPKEY_Field := Smc_Wpmr_Wpkey_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMC_SMC_WPMR_Register use record
      WPEN         at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
      WPKEY        at 0 range 8 .. 31;
   end record;

   subtype SMC_SMC_WPSR_WPVSRC_Field is HAL.UInt16;

   --  SMC Write Protection Status Register
   type SMC_SMC_WPSR_Register is record
      --  Read-only. Write Protection Violation Status
      WPVS           : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. Write Protection Violation Source
      WPVSRC         : SMC_SMC_WPSR_WPVSRC_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMC_SMC_WPSR_Register use record
      WPVS           at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      WPVSRC         at 0 range 8 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype SMC_SMC_VERSION_VERSION_Field is HAL.UInt12;
   subtype SMC_SMC_VERSION_MFN_Field is HAL.UInt3;

   --  SMC Version Register
   type SMC_SMC_VERSION_Register is record
      --  Read-only. Hardware Module Version
      VERSION        : SMC_SMC_VERSION_VERSION_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Metal Fix Number
      MFN            : SMC_SMC_VERSION_MFN_Field;
      --  unspecified
      Reserved_19_31 : HAL.UInt13;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMC_SMC_VERSION_Register use record
      VERSION        at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      MFN            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Static Memory Controller
   type SMC_Peripheral is record
      --  SMC Setup Register (CS_number = 0)
      SMC_SMC_CS_NUMBER : aliased SMC_SMC_CS_NUMBER_Clusters;
      --  SMC Off-Chip Memory Scrambling Register
      SMC_OCMS          : aliased SMC_SMC_OCMS_Register;
      --  SMC Off-Chip Memory Scrambling KEY1 Register
      SMC_KEY1          : aliased HAL.UInt32;
      --  SMC Off-Chip Memory Scrambling KEY2 Register
      SMC_KEY2          : aliased HAL.UInt32;
      --  SMC Write Protection Mode Register
      SMC_WPMR          : aliased SMC_SMC_WPMR_Register;
      --  SMC Write Protection Status Register
      SMC_WPSR          : aliased SMC_SMC_WPSR_Register;
      --  SMC Version Register
      SMC_VERSION       : aliased SMC_SMC_VERSION_Register;
   end record
     with Volatile;

   for SMC_Peripheral use record
      SMC_SMC_CS_NUMBER at 16#0# range 0 .. 511;
      SMC_OCMS          at 16#80# range 0 .. 31;
      SMC_KEY1          at 16#84# range 0 .. 31;
      SMC_KEY2          at 16#88# range 0 .. 31;
      SMC_WPMR          at 16#E4# range 0 .. 31;
      SMC_WPSR          at 16#E8# range 0 .. 31;
      SMC_VERSION       at 16#FC# range 0 .. 31;
   end record;

   --  Static Memory Controller
   SMC_Periph : aliased SMC_Peripheral
     with Import, Address => System'To_Address (16#40080000#);

end SAM_SVD.SMC;
