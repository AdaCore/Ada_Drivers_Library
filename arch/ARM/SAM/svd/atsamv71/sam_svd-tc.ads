--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.TC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ---------------------------------------
   -- TC_TC_CHANNEL cluster's Registers --
   ---------------------------------------

   --  Channel Control Register (channel = 0)
   type TC_TC_CCR_TC_TC_CHANNEL_Register is record
      --  Write-only. Counter Clock Enable Command
      CLKEN         : Boolean := False;
      --  Write-only. Counter Clock Disable Command
      CLKDIS        : Boolean := False;
      --  Write-only. Software Trigger Command
      SWTRG         : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_CCR_TC_TC_CHANNEL_Register use record
      CLKEN         at 0 range 0 .. 0;
      CLKDIS        at 0 range 1 .. 1;
      SWTRG         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Clock Selection
   type TC_CMR_TCCLKS_Field is
     (
      --  Clock selected: internal PCK6 clock signal (from PMC)
      Timer_Clock1,
      --  Clock selected: internal MCK/8 clock signal (from PMC)
      Timer_Clock2,
      --  Clock selected: internal MCK/32 clock signal (from PMC)
      Timer_Clock3,
      --  Clock selected: internal MCK/128 clock signal (from PMC)
      Timer_Clock4,
      --  Clock selected: internal SLCK clock signal (from PMC)
      Timer_Clock5,
      --  Clock selected: XC0
      Xc0,
      --  Clock selected: XC1
      Xc1,
      --  Clock selected: XC2
      Xc2)
     with Size => 3;
   for TC_CMR_TCCLKS_Field use
     (Timer_Clock1 => 0,
      Timer_Clock2 => 1,
      Timer_Clock3 => 2,
      Timer_Clock4 => 3,
      Timer_Clock5 => 4,
      Xc0 => 5,
      Xc1 => 6,
      Xc2 => 7);

   --  Burst Signal Selection
   type TC_CMR_BURST_Field is
     (
      --  The clock is not gated by an external signal.
      None,
      --  XC0 is ANDed with the selected clock.
      Xc0,
      --  XC1 is ANDed with the selected clock.
      Xc1,
      --  XC2 is ANDed with the selected clock.
      Xc2)
     with Size => 2;
   for TC_CMR_BURST_Field use
     (None => 0,
      Xc0 => 1,
      Xc1 => 2,
      Xc2 => 3);

   --  External Trigger Edge Selection
   type TC_CMR_ETRGEDG_Field is
     (
      --  The clock is not gated by an external signal.
      None,
      --  Rising edge
      Rising,
      --  Falling edge
      Falling,
      --  Each edge
      Edge)
     with Size => 2;
   for TC_CMR_ETRGEDG_Field use
     (None => 0,
      Rising => 1,
      Falling => 2,
      Edge => 3);

   --  RA Loading Edge Selection
   type TC_CMR_LDRA_Field is
     (
      --  None
      None,
      --  Rising edge of TIOAx
      Rising,
      --  Falling edge of TIOAx
      Falling,
      --  Each edge of TIOAx
      Edge)
     with Size => 2;
   for TC_CMR_LDRA_Field use
     (None => 0,
      Rising => 1,
      Falling => 2,
      Edge => 3);

   --  RB Loading Edge Selection
   type TC_CMR_LDRB_Field is
     (
      --  None
      None,
      --  Rising edge of TIOAx
      Rising,
      --  Falling edge of TIOAx
      Falling,
      --  Each edge of TIOAx
      Edge)
     with Size => 2;
   for TC_CMR_LDRB_Field use
     (None => 0,
      Rising => 1,
      Falling => 2,
      Edge => 3);

   --  Loading Edge Subsampling Ratio
   type TC_CMR_SBSMPLR_Field is
     (
      --  Load a Capture Register each selected edge
      One,
      --  Load a Capture Register every 2 selected edges
      Half,
      --  Load a Capture Register every 4 selected edges
      Fourth,
      --  Load a Capture Register every 8 selected edges
      Eighth,
      --  Load a Capture Register every 16 selected edges
      Sixteenth)
     with Size => 3;
   for TC_CMR_SBSMPLR_Field use
     (One => 0,
      Half => 1,
      Fourth => 2,
      Eighth => 3,
      Sixteenth => 4);

   --  Channel Mode Register (channel = 0)
   type TC_TC_CMR_TC_TC_CHANNEL_Register is record
      --  Clock Selection
      TCCLKS         : TC_CMR_TCCLKS_Field := SAM_SVD.TC.Timer_Clock1;
      --  Clock Invert
      CLKI           : Boolean := False;
      --  Burst Signal Selection
      BURST          : TC_CMR_BURST_Field := SAM_SVD.TC.None;
      --  Counter Clock Stopped with RB Loading
      LDBSTOP        : Boolean := False;
      --  Counter Clock Disable with RB Loading
      LDBDIS         : Boolean := False;
      --  External Trigger Edge Selection
      ETRGEDG        : TC_CMR_ETRGEDG_Field := SAM_SVD.TC.None;
      --  TIOAx or TIOBx External Trigger Selection
      ABETRG         : Boolean := False;
      --  unspecified
      Reserved_11_13 : HAL.UInt3 := 16#0#;
      --  RC Compare Trigger Enable
      CPCTRG         : Boolean := False;
      --  Waveform Mode
      WAVE           : Boolean := False;
      --  RA Loading Edge Selection
      LDRA           : TC_CMR_LDRA_Field := SAM_SVD.TC.None;
      --  RB Loading Edge Selection
      LDRB           : TC_CMR_LDRB_Field := SAM_SVD.TC.None;
      --  Loading Edge Subsampling Ratio
      SBSMPLR        : TC_CMR_SBSMPLR_Field := SAM_SVD.TC.One;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_CMR_TC_TC_CHANNEL_Register use record
      TCCLKS         at 0 range 0 .. 2;
      CLKI           at 0 range 3 .. 3;
      BURST          at 0 range 4 .. 5;
      LDBSTOP        at 0 range 6 .. 6;
      LDBDIS         at 0 range 7 .. 7;
      ETRGEDG        at 0 range 8 .. 9;
      ABETRG         at 0 range 10 .. 10;
      Reserved_11_13 at 0 range 11 .. 13;
      CPCTRG         at 0 range 14 .. 14;
      WAVE           at 0 range 15 .. 15;
      LDRA           at 0 range 16 .. 17;
      LDRB           at 0 range 18 .. 19;
      SBSMPLR        at 0 range 20 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  Stepper Motor Mode Register (channel = 0)
   type TC_TC_SMMR_TC_TC_CHANNEL_Register is record
      --  Gray Count Enable
      GCEN          : Boolean := False;
      --  Down Count
      DOWN          : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_SMMR_TC_TC_CHANNEL_Register use record
      GCEN          at 0 range 0 .. 0;
      DOWN          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Status Register (channel = 0)
   type TC_TC_SR_TC_TC_CHANNEL_Register is record
      --  Read-only. Counter Overflow Status (cleared on read)
      COVFS          : Boolean;
      --  Read-only. Load Overrun Status (cleared on read)
      LOVRS          : Boolean;
      --  Read-only. RA Compare Status (cleared on read)
      CPAS           : Boolean;
      --  Read-only. RB Compare Status (cleared on read)
      CPBS           : Boolean;
      --  Read-only. RC Compare Status (cleared on read)
      CPCS           : Boolean;
      --  Read-only. RA Loading Status (cleared on read)
      LDRAS          : Boolean;
      --  Read-only. RB Loading Status (cleared on read)
      LDRBS          : Boolean;
      --  Read-only. External Trigger Status (cleared on read)
      ETRGS          : Boolean;
      --  unspecified
      Reserved_8_15  : HAL.UInt8;
      --  Read-only. Clock Enabling Status
      CLKSTA         : Boolean;
      --  Read-only. TIOAx Mirror
      MTIOA          : Boolean;
      --  Read-only. TIOBx Mirror
      MTIOB          : Boolean;
      --  unspecified
      Reserved_19_31 : HAL.UInt13;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_SR_TC_TC_CHANNEL_Register use record
      COVFS          at 0 range 0 .. 0;
      LOVRS          at 0 range 1 .. 1;
      CPAS           at 0 range 2 .. 2;
      CPBS           at 0 range 3 .. 3;
      CPCS           at 0 range 4 .. 4;
      LDRAS          at 0 range 5 .. 5;
      LDRBS          at 0 range 6 .. 6;
      ETRGS          at 0 range 7 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      CLKSTA         at 0 range 16 .. 16;
      MTIOA          at 0 range 17 .. 17;
      MTIOB          at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  Interrupt Enable Register (channel = 0)
   type TC_TC_IER_TC_TC_CHANNEL_Register is record
      --  Write-only. Counter Overflow
      COVFS         : Boolean := False;
      --  Write-only. Load Overrun
      LOVRS         : Boolean := False;
      --  Write-only. RA Compare
      CPAS          : Boolean := False;
      --  Write-only. RB Compare
      CPBS          : Boolean := False;
      --  Write-only. RC Compare
      CPCS          : Boolean := False;
      --  Write-only. RA Loading
      LDRAS         : Boolean := False;
      --  Write-only. RB Loading
      LDRBS         : Boolean := False;
      --  Write-only. External Trigger
      ETRGS         : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_IER_TC_TC_CHANNEL_Register use record
      COVFS         at 0 range 0 .. 0;
      LOVRS         at 0 range 1 .. 1;
      CPAS          at 0 range 2 .. 2;
      CPBS          at 0 range 3 .. 3;
      CPCS          at 0 range 4 .. 4;
      LDRAS         at 0 range 5 .. 5;
      LDRBS         at 0 range 6 .. 6;
      ETRGS         at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Interrupt Disable Register (channel = 0)
   type TC_TC_IDR_TC_TC_CHANNEL_Register is record
      --  Write-only. Counter Overflow
      COVFS         : Boolean := False;
      --  Write-only. Load Overrun
      LOVRS         : Boolean := False;
      --  Write-only. RA Compare
      CPAS          : Boolean := False;
      --  Write-only. RB Compare
      CPBS          : Boolean := False;
      --  Write-only. RC Compare
      CPCS          : Boolean := False;
      --  Write-only. RA Loading
      LDRAS         : Boolean := False;
      --  Write-only. RB Loading
      LDRBS         : Boolean := False;
      --  Write-only. External Trigger
      ETRGS         : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_IDR_TC_TC_CHANNEL_Register use record
      COVFS         at 0 range 0 .. 0;
      LOVRS         at 0 range 1 .. 1;
      CPAS          at 0 range 2 .. 2;
      CPBS          at 0 range 3 .. 3;
      CPCS          at 0 range 4 .. 4;
      LDRAS         at 0 range 5 .. 5;
      LDRBS         at 0 range 6 .. 6;
      ETRGS         at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Interrupt Mask Register (channel = 0)
   type TC_TC_IMR_TC_TC_CHANNEL_Register is record
      --  Read-only. Counter Overflow
      COVFS         : Boolean;
      --  Read-only. Load Overrun
      LOVRS         : Boolean;
      --  Read-only. RA Compare
      CPAS          : Boolean;
      --  Read-only. RB Compare
      CPBS          : Boolean;
      --  Read-only. RC Compare
      CPCS          : Boolean;
      --  Read-only. RA Loading
      LDRAS         : Boolean;
      --  Read-only. RB Loading
      LDRBS         : Boolean;
      --  Read-only. External Trigger
      ETRGS         : Boolean;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_IMR_TC_TC_CHANNEL_Register use record
      COVFS         at 0 range 0 .. 0;
      LOVRS         at 0 range 1 .. 1;
      CPAS          at 0 range 2 .. 2;
      CPBS          at 0 range 3 .. 3;
      CPCS          at 0 range 4 .. 4;
      LDRAS         at 0 range 5 .. 5;
      LDRBS         at 0 range 6 .. 6;
      ETRGS         at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Trigger Source for Input A
   type TC_EMR_TRIGSRCA_Field is
     (
      --  The trigger/capture input A is driven by external pin TIOAx
      External_Tioax,
      --  The trigger/capture input A is driven internally by PWMx
      Pwmx)
     with Size => 2;
   for TC_EMR_TRIGSRCA_Field use
     (External_Tioax => 0,
      Pwmx => 1);

   --  Trigger Source for Input B
   type TC_EMR_TRIGSRCB_Field is
     (
      --  The trigger/capture input B is driven by external pin TIOBx
      External_Tiobx,
      --  For TC0 to TC10: The trigger/capture input B is driven internally by
      --  the comparator output (see Figure 7-16) of the PWMx.For TC11: The
      --  trigger/capture input B is driven internally by the GTSUCOMP signal
      --  of the Ethernet MAC (GMAC).
      Pwmx)
     with Size => 2;
   for TC_EMR_TRIGSRCB_Field use
     (External_Tiobx => 0,
      Pwmx => 1);

   --  Extended Mode Register (channel = 0)
   type TC_TC_EMR_TC_TC_CHANNEL_Register is record
      --  Trigger Source for Input A
      TRIGSRCA      : TC_EMR_TRIGSRCA_Field := SAM_SVD.TC.External_Tioax;
      --  unspecified
      Reserved_2_3  : HAL.UInt2 := 16#0#;
      --  Trigger Source for Input B
      TRIGSRCB      : TC_EMR_TRIGSRCB_Field := SAM_SVD.TC.External_Tiobx;
      --  unspecified
      Reserved_6_7  : HAL.UInt2 := 16#0#;
      --  No Divided Clock
      NODIVCLK      : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_EMR_TC_TC_CHANNEL_Register use record
      TRIGSRCA      at 0 range 0 .. 1;
      Reserved_2_3  at 0 range 2 .. 3;
      TRIGSRCB      at 0 range 4 .. 5;
      Reserved_6_7  at 0 range 6 .. 7;
      NODIVCLK      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   type TC_TC_RESERVED_Region is array (1 .. 3) of HAL.UInt32;

   --  Channel Control Register (channel = 0)
   type TC_TC_CHANNEL_Cluster is record
      --  Channel Control Register (channel = 0)
      TC_CCR  : aliased TC_TC_CCR_TC_TC_CHANNEL_Register;
      --  Channel Mode Register (channel = 0)
      TC_CMR  : aliased TC_TC_CMR_TC_TC_CHANNEL_Register;
      --  Stepper Motor Mode Register (channel = 0)
      TC_SMMR : aliased TC_TC_SMMR_TC_TC_CHANNEL_Register;
      --  Register AB (channel = 0)
      TC_RAB  : aliased HAL.UInt32;
      --  Counter Value (channel = 0)
      TC_CV   : aliased HAL.UInt32;
      --  Register A (channel = 0)
      TC_RA   : aliased HAL.UInt32;
      --  Register B (channel = 0)
      TC_RB   : aliased HAL.UInt32;
      --  Register C (channel = 0)
      TC_RC   : aliased HAL.UInt32;
      --  Status Register (channel = 0)
      TC_SR   : aliased TC_TC_SR_TC_TC_CHANNEL_Register;
      --  Interrupt Enable Register (channel = 0)
      TC_IER  : aliased TC_TC_IER_TC_TC_CHANNEL_Register;
      --  Interrupt Disable Register (channel = 0)
      TC_IDR  : aliased TC_TC_IDR_TC_TC_CHANNEL_Register;
      --  Interrupt Mask Register (channel = 0)
      TC_IMR  : aliased TC_TC_IMR_TC_TC_CHANNEL_Register;
      --  Extended Mode Register (channel = 0)
      TC_EMR  : aliased TC_TC_EMR_TC_TC_CHANNEL_Register;
      --  Reserved Regions
      Reserved_34_3F : aliased TC_TC_RESERVED_Region;
   end record
     with Volatile, Size => 512;

   for TC_TC_CHANNEL_Cluster use record
      TC_CCR  at 16#0# range 0 .. 31;
      TC_CMR  at 16#4# range 0 .. 31;
      TC_SMMR at 16#8# range 0 .. 31;
      TC_RAB  at 16#C# range 0 .. 31;
      TC_CV   at 16#10# range 0 .. 31;
      TC_RA   at 16#14# range 0 .. 31;
      TC_RB   at 16#18# range 0 .. 31;
      TC_RC   at 16#1C# range 0 .. 31;
      TC_SR   at 16#20# range 0 .. 31;
      TC_IER  at 16#24# range 0 .. 31;
      TC_IDR  at 16#28# range 0 .. 31;
      TC_IMR  at 16#2C# range 0 .. 31;
      TC_EMR  at 16#30# range 0 .. 31;
      Reserved_34_3F at 16#34# range 0 .. 95;
   end record;

   --  Channel Control Register (channel = 0)
   type TC_TC_CHANNEL_Clusters is array (0 .. 2) of TC_TC_CHANNEL_Cluster;

   --  Block Control Register
   type TC_TC_BCR_Register is record
      --  Write-only. Synchro Command
      SYNC          : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_BCR_Register use record
      SYNC          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  External Clock Signal 0 Selection
   type TC_BMR_TC0XC0S_Field is
     (
      --  Signal connected to XC0: TCLK0
      Tclk0,
      --  Signal connected to XC0: TIOA1
      Tioa1,
      --  Signal connected to XC0: TIOA2
      Tioa2)
     with Size => 2;
   for TC_BMR_TC0XC0S_Field use
     (Tclk0 => 0,
      Tioa1 => 2,
      Tioa2 => 3);

   --  External Clock Signal 1 Selection
   type TC_BMR_TC1XC1S_Field is
     (
      --  Signal connected to XC1: TCLK1
      Tclk1,
      --  Signal connected to XC1: TIOA0
      Tioa0,
      --  Signal connected to XC1: TIOA2
      Tioa2)
     with Size => 2;
   for TC_BMR_TC1XC1S_Field use
     (Tclk1 => 0,
      Tioa0 => 2,
      Tioa2 => 3);

   --  External Clock Signal 2 Selection
   type TC_BMR_TC2XC2S_Field is
     (
      --  Signal connected to XC2: TCLK2
      Tclk2,
      --  Signal connected to XC2: TIOA0
      Tioa0,
      --  Signal connected to XC2: TIOA1
      Tioa1)
     with Size => 2;
   for TC_BMR_TC2XC2S_Field use
     (Tclk2 => 0,
      Tioa0 => 2,
      Tioa1 => 3);

   subtype TC_TC_BMR_MAXFILT_Field is HAL.UInt6;

   --  Block Mode Register
   type TC_TC_BMR_Register is record
      --  External Clock Signal 0 Selection
      TC0XC0S        : TC_BMR_TC0XC0S_Field := SAM_SVD.TC.Tclk0;
      --  External Clock Signal 1 Selection
      TC1XC1S        : TC_BMR_TC1XC1S_Field := SAM_SVD.TC.Tclk1;
      --  External Clock Signal 2 Selection
      TC2XC2S        : TC_BMR_TC2XC2S_Field := SAM_SVD.TC.Tclk2;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Quadrature Decoder Enabled
      QDEN           : Boolean := False;
      --  Position Enabled
      POSEN          : Boolean := False;
      --  Speed Enabled
      SPEEDEN        : Boolean := False;
      --  Quadrature Decoding Transparent
      QDTRANS        : Boolean := False;
      --  Edge on PHA Count Mode
      EDGPHA         : Boolean := False;
      --  Inverted PHA
      INVA           : Boolean := False;
      --  Inverted PHB
      INVB           : Boolean := False;
      --  Inverted Index
      INVIDX         : Boolean := False;
      --  Swap PHA and PHB
      SWAP           : Boolean := False;
      --  Index Pin is PHB Pin
      IDXPHB         : Boolean := False;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Maximum Filter
      MAXFILT        : TC_TC_BMR_MAXFILT_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_BMR_Register use record
      TC0XC0S        at 0 range 0 .. 1;
      TC1XC1S        at 0 range 2 .. 3;
      TC2XC2S        at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      QDEN           at 0 range 8 .. 8;
      POSEN          at 0 range 9 .. 9;
      SPEEDEN        at 0 range 10 .. 10;
      QDTRANS        at 0 range 11 .. 11;
      EDGPHA         at 0 range 12 .. 12;
      INVA           at 0 range 13 .. 13;
      INVB           at 0 range 14 .. 14;
      INVIDX         at 0 range 15 .. 15;
      SWAP           at 0 range 16 .. 16;
      IDXPHB         at 0 range 17 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      MAXFILT        at 0 range 20 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  QDEC Interrupt Enable Register
   type TC_TC_QIER_Register is record
      --  Write-only. Index
      IDX           : Boolean := False;
      --  Write-only. Direction Change
      DIRCHG        : Boolean := False;
      --  Write-only. Quadrature Error
      QERR          : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_QIER_Register use record
      IDX           at 0 range 0 .. 0;
      DIRCHG        at 0 range 1 .. 1;
      QERR          at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  QDEC Interrupt Disable Register
   type TC_TC_QIDR_Register is record
      --  Write-only. Index
      IDX           : Boolean := False;
      --  Write-only. Direction Change
      DIRCHG        : Boolean := False;
      --  Write-only. Quadrature Error
      QERR          : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_QIDR_Register use record
      IDX           at 0 range 0 .. 0;
      DIRCHG        at 0 range 1 .. 1;
      QERR          at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  QDEC Interrupt Mask Register
   type TC_TC_QIMR_Register is record
      --  Read-only. Index
      IDX           : Boolean;
      --  Read-only. Direction Change
      DIRCHG        : Boolean;
      --  Read-only. Quadrature Error
      QERR          : Boolean;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_QIMR_Register use record
      IDX           at 0 range 0 .. 0;
      DIRCHG        at 0 range 1 .. 1;
      QERR          at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  QDEC Interrupt Status Register
   type TC_TC_QISR_Register is record
      --  Read-only. Index
      IDX           : Boolean;
      --  Read-only. Direction Change
      DIRCHG        : Boolean;
      --  Read-only. Quadrature Error
      QERR          : Boolean;
      --  unspecified
      Reserved_3_7  : HAL.UInt5;
      --  Read-only. Direction
      DIR           : Boolean;
      --  unspecified
      Reserved_9_31 : HAL.UInt23;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_QISR_Register use record
      IDX           at 0 range 0 .. 0;
      DIRCHG        at 0 range 1 .. 1;
      QERR          at 0 range 2 .. 2;
      Reserved_3_7  at 0 range 3 .. 7;
      DIR           at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  TC_TC_FMR_ENCF array
   type TC_TC_FMR_ENCF_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for TC_TC_FMR_ENCF
   type TC_TC_FMR_ENCF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ENCF as a value
            Val : HAL.UInt2;
         when True =>
            --  ENCF as an array
            Arr : TC_TC_FMR_ENCF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for TC_TC_FMR_ENCF_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Fault Mode Register
   type TC_TC_FMR_Register is record
      --  Enable Compare Fault Channel 0
      ENCF          : TC_TC_FMR_ENCF_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_FMR_Register use record
      ENCF          at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Write Protection Key
   type TC_WPMR_WPKEY_Field is
     (
      --  Reset value for the field
      Tc_Wpmr_Wpkey_Field_Reset,
      --  Writing any other value in this field aborts the write operation of
      --  the WPEN bit.Always reads as 0.
      Passwd)
     with Size => 24;
   for TC_WPMR_WPKEY_Field use
     (Tc_Wpmr_Wpkey_Field_Reset => 0,
      Passwd => 5523789);

   --  Write Protection Mode Register
   type TC_TC_WPMR_Register is record
      --  Write Protection Enable
      WPEN         : Boolean := False;
      --  unspecified
      Reserved_1_7 : HAL.UInt7 := 16#0#;
      --  Write Protection Key
      WPKEY        : TC_WPMR_WPKEY_Field := Tc_Wpmr_Wpkey_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_WPMR_Register use record
      WPEN         at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
      WPKEY        at 0 range 8 .. 31;
   end record;

   subtype TC_TC_VER_VERSION_Field is HAL.UInt12;
   subtype TC_TC_VER_MFN_Field is HAL.UInt3;

   --  Version Register
   type TC_TC_VER_Register is record
      --  Read-only. Version of the Hardware Module
      VERSION        : TC_TC_VER_VERSION_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Metal Fix Number
      MFN            : TC_TC_VER_MFN_Field;
      --  unspecified
      Reserved_19_31 : HAL.UInt13;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TC_TC_VER_Register use record
      VERSION        at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      MFN            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   ---------------------------------------
   -- TC_TC_CHANNEL cluster's Registers --
   ---------------------------------------

   ---------------------------------------
   -- TC_TC_CHANNEL cluster's Registers --
   ---------------------------------------

   ---------------------------------------
   -- TC_TC_CHANNEL cluster's Registers --
   ---------------------------------------

   -----------------
   -- Peripherals --
   -----------------

   --  Timer Counter
   type TC_Peripheral is record
      --  Channel Control Register (channel = 0)
      TC_TC_CHANNEL : aliased TC_TC_CHANNEL_Clusters;
      --  Block Control Register
      TC_BCR        : aliased TC_TC_BCR_Register;
      --  Block Mode Register
      TC_BMR        : aliased TC_TC_BMR_Register;
      --  QDEC Interrupt Enable Register
      TC_QIER       : aliased TC_TC_QIER_Register;
      --  QDEC Interrupt Disable Register
      TC_QIDR       : aliased TC_TC_QIDR_Register;
      --  QDEC Interrupt Mask Register
      TC_QIMR       : aliased TC_TC_QIMR_Register;
      --  QDEC Interrupt Status Register
      TC_QISR       : aliased TC_TC_QISR_Register;
      --  Fault Mode Register
      TC_FMR        : aliased TC_TC_FMR_Register;
      --  Write Protection Mode Register
      TC_WPMR       : aliased TC_TC_WPMR_Register;
      --  Version Register
      TC_VER        : aliased TC_TC_VER_Register;
   end record
     with Volatile;

   for TC_Peripheral use record
      TC_TC_CHANNEL at 16#0# range 0 .. 1535;
      TC_BCR        at 16#C0# range 0 .. 31;
      TC_BMR        at 16#C4# range 0 .. 31;
      TC_QIER       at 16#C8# range 0 .. 31;
      TC_QIDR       at 16#CC# range 0 .. 31;
      TC_QIMR       at 16#D0# range 0 .. 31;
      TC_QISR       at 16#D4# range 0 .. 31;
      TC_FMR        at 16#D8# range 0 .. 31;
      TC_WPMR       at 16#E4# range 0 .. 31;
      TC_VER        at 16#FC# range 0 .. 31;
   end record;

   --  Timer Counter
   TC0_Periph : aliased TC_Peripheral
     with Import, Address => System'To_Address (16#4000C000#);

   --  Timer Counter
   TC1_Periph : aliased TC_Peripheral
     with Import, Address => System'To_Address (16#40010000#);

   --  Timer Counter
   TC2_Periph : aliased TC_Peripheral
     with Import, Address => System'To_Address (16#40014000#);

   --  Timer Counter
   TC3_Periph : aliased TC_Peripheral
     with Import, Address => System'To_Address (16#40054000#);

end SAM_SVD.TC;
