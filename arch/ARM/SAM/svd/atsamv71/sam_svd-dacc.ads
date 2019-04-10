--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.DACC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Control Register
   type DACC_DACC_CR_Register is record
      --  Write-only. Software Reset
      SWRST         : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DACC_DACC_CR_Register use record
      SWRST         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Max Speed Mode for Channel 0
   type DACC_MR_MAXS0_Field is
     (
      --  Trigger mode or Free-running mode enabled. (See TRGENx.DACC_TRIGR.)
      Trig_Event,
      --  Max speed mode enabled.
      Maximum)
     with Size => 1;
   for DACC_MR_MAXS0_Field use
     (Trig_Event => 0,
      Maximum => 1);

   --  DACC_DACC_MR_MAXS array
   type DACC_DACC_MR_MAXS_Field_Array is array (0 .. 1)
     of DACC_MR_MAXS0_Field
     with Component_Size => 1, Size => 2;

   --  Type definition for DACC_DACC_MR_MAXS
   type DACC_DACC_MR_MAXS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MAXS as a value
            Val : HAL.UInt2;
         when True =>
            --  MAXS as an array
            Arr : DACC_DACC_MR_MAXS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DACC_DACC_MR_MAXS_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Word Transfer Mode
   type DACC_MR_WORD_Field is
     (
      --  One data to convert is written to the FIFO per access to DACC.
      Disabled,
      --  Two data to convert are written to the FIFO per access to DACC
      --  (reduces the number of requests to DMA and the number of system bus
      --  accesses).
      Enabled)
     with Size => 1;
   for DACC_MR_WORD_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Differential Mode
   type DACC_MR_DIFF_Field is
     (
      --  DAC0 and DAC1 are single-ended outputs.
      Disabled,
      --  DACP and DACN are differential outputs. The differential level is
      --  configured by the channel 0 value.
      Enabled)
     with Size => 1;
   for DACC_MR_DIFF_Field use
     (Disabled => 0,
      Enabled => 1);

   subtype DACC_DACC_MR_PRESCALER_Field is HAL.UInt4;

   --  Mode Register
   type DACC_DACC_MR_Register is record
      --  Max Speed Mode for Channel 0
      MAXS           : DACC_DACC_MR_MAXS_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Word Transfer Mode
      WORD           : DACC_MR_WORD_Field := SAM_SVD.DACC.Disabled;
      --  Must always be written to 0.
      ZERO           : Boolean := False;
      --  unspecified
      Reserved_6_22  : HAL.UInt17 := 16#0#;
      --  Differential Mode
      DIFF           : DACC_MR_DIFF_Field := SAM_SVD.DACC.Disabled;
      --  Peripheral Clock to DAC Clock Ratio
      PRESCALER      : DACC_DACC_MR_PRESCALER_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DACC_DACC_MR_Register use record
      MAXS           at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      WORD           at 0 range 4 .. 4;
      ZERO           at 0 range 5 .. 5;
      Reserved_6_22  at 0 range 6 .. 22;
      DIFF           at 0 range 23 .. 23;
      PRESCALER      at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  Trigger Enable of Channel 0
   type DACC_TRIGR_TRGEN0_Field is
     (
      --  Trigger mode disabled. DACC is in Free-running mode or Max speed
      --  mode.
      Dis,
      --  Trigger mode enabled.
      En)
     with Size => 1;
   for DACC_TRIGR_TRGEN0_Field use
     (Dis => 0,
      En => 1);

   --  DACC_DACC_TRIGR_TRGEN array
   type DACC_DACC_TRIGR_TRGEN_Field_Array is array (0 .. 1)
     of DACC_TRIGR_TRGEN0_Field
     with Component_Size => 1, Size => 2;

   --  Type definition for DACC_DACC_TRIGR_TRGEN
   type DACC_DACC_TRIGR_TRGEN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TRGEN as a value
            Val : HAL.UInt2;
         when True =>
            --  TRGEN as an array
            Arr : DACC_DACC_TRIGR_TRGEN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DACC_DACC_TRIGR_TRGEN_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Trigger Selection of Channel 0
   type DACC_TRIGR_TRGSEL0_Field is
     (
      --  DATRG
      Trgsel0,
      --  TC0 output
      Trgsel1,
      --  TC1 output
      Trgsel2,
      --  TC2 output
      Trgsel3,
      --  PWM0 event 0
      Trgsel4,
      --  PWM0 event 1
      Trgsel5,
      --  PWM1 event 0
      Trgsel6,
      --  PWM1 event 1
      Trgsel7)
     with Size => 3;
   for DACC_TRIGR_TRGSEL0_Field use
     (Trgsel0 => 0,
      Trgsel1 => 1,
      Trgsel2 => 2,
      Trgsel3 => 3,
      Trgsel4 => 4,
      Trgsel5 => 5,
      Trgsel6 => 6,
      Trgsel7 => 7);

   --  Trigger Selection of Channel 1
   type DACC_TRIGR_TRGSEL1_Field is
     (
      --  DATRG
      Trgsel0,
      --  TC0 output
      Trgsel1,
      --  TC1 output
      Trgsel2,
      --  TC2 output
      Trgsel3,
      --  PWM0 event 0
      Trgsel4,
      --  PWM0 event 1
      Trgsel5,
      --  PWM1 event 0
      Trgsel6,
      --  PWM1 event 1
      Trgsel7)
     with Size => 3;
   for DACC_TRIGR_TRGSEL1_Field use
     (Trgsel0 => 0,
      Trgsel1 => 1,
      Trgsel2 => 2,
      Trgsel3 => 3,
      Trgsel4 => 4,
      Trgsel5 => 5,
      Trgsel6 => 6,
      Trgsel7 => 7);

   --  Over Sampling Ratio of Channel 0
   type DACC_TRIGR_OSR0_Field is
     (
      --  OSR = 1
      Osr_1,
      --  OSR = 2
      Osr_2,
      --  OSR = 4
      Osr_4,
      --  OSR = 8
      Osr_8,
      --  OSR = 16
      Osr_16,
      --  OSR = 32
      Osr_32)
     with Size => 3;
   for DACC_TRIGR_OSR0_Field use
     (Osr_1 => 0,
      Osr_2 => 1,
      Osr_4 => 2,
      Osr_8 => 3,
      Osr_16 => 4,
      Osr_32 => 5);

   --  Over Sampling Ratio of Channel 1
   type DACC_TRIGR_OSR1_Field is
     (
      --  OSR = 1
      Osr_1,
      --  OSR = 2
      Osr_2,
      --  OSR = 4
      Osr_4,
      --  OSR = 8
      Osr_8,
      --  OSR = 16
      Osr_16,
      --  OSR = 32
      Osr_32)
     with Size => 3;
   for DACC_TRIGR_OSR1_Field use
     (Osr_1 => 0,
      Osr_2 => 1,
      Osr_4 => 2,
      Osr_8 => 3,
      Osr_16 => 4,
      Osr_32 => 5);

   --  Trigger Register
   type DACC_DACC_TRIGR_Register is record
      --  Trigger Enable of Channel 0
      TRGEN          : DACC_DACC_TRIGR_TRGEN_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Trigger Selection of Channel 0
      TRGSEL0        : DACC_TRIGR_TRGSEL0_Field := SAM_SVD.DACC.Trgsel0;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Trigger Selection of Channel 1
      TRGSEL1        : DACC_TRIGR_TRGSEL1_Field := SAM_SVD.DACC.Trgsel0;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Over Sampling Ratio of Channel 0
      OSR0           : DACC_TRIGR_OSR0_Field := SAM_SVD.DACC.Osr_1;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  Over Sampling Ratio of Channel 1
      OSR1           : DACC_TRIGR_OSR1_Field := SAM_SVD.DACC.Osr_1;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DACC_DACC_TRIGR_Register use record
      TRGEN          at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      TRGSEL0        at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      TRGSEL1        at 0 range 8 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      OSR0           at 0 range 16 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      OSR1           at 0 range 20 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  DACC_DACC_CHER_CH array
   type DACC_DACC_CHER_CH_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for DACC_DACC_CHER_CH
   type DACC_DACC_CHER_CH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CH as a value
            Val : HAL.UInt2;
         when True =>
            --  CH as an array
            Arr : DACC_DACC_CHER_CH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DACC_DACC_CHER_CH_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Channel Enable Register
   type DACC_DACC_CHER_Register is record
      --  Write-only. Channel 0 Enable
      CH            : DACC_DACC_CHER_CH_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DACC_DACC_CHER_Register use record
      CH            at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  DACC_DACC_CHDR_CH array
   type DACC_DACC_CHDR_CH_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for DACC_DACC_CHDR_CH
   type DACC_DACC_CHDR_CH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CH as a value
            Val : HAL.UInt2;
         when True =>
            --  CH as an array
            Arr : DACC_DACC_CHDR_CH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DACC_DACC_CHDR_CH_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Channel Disable Register
   type DACC_DACC_CHDR_Register is record
      --  Write-only. Channel 0 Disable
      CH            : DACC_DACC_CHDR_CH_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DACC_DACC_CHDR_Register use record
      CH            at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  DACC_DACC_CHSR_CH array
   type DACC_DACC_CHSR_CH_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for DACC_DACC_CHSR_CH
   type DACC_DACC_CHSR_CH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CH as a value
            Val : HAL.UInt2;
         when True =>
            --  CH as an array
            Arr : DACC_DACC_CHSR_CH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DACC_DACC_CHSR_CH_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  DACC_DACC_CHSR_DACRDY array
   type DACC_DACC_CHSR_DACRDY_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for DACC_DACC_CHSR_DACRDY
   type DACC_DACC_CHSR_DACRDY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DACRDY as a value
            Val : HAL.UInt2;
         when True =>
            --  DACRDY as an array
            Arr : DACC_DACC_CHSR_DACRDY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DACC_DACC_CHSR_DACRDY_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Channel Status Register
   type DACC_DACC_CHSR_Register is record
      --  Read-only. Channel 0 Status
      CH             : DACC_DACC_CHSR_CH_Field;
      --  unspecified
      Reserved_2_7   : HAL.UInt6;
      --  Read-only. DAC Ready Flag
      DACRDY         : DACC_DACC_CHSR_DACRDY_Field;
      --  unspecified
      Reserved_10_31 : HAL.UInt22;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DACC_DACC_CHSR_Register use record
      CH             at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      DACRDY         at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  DACC_DACC_CDR_DATA array element
   subtype DACC_DACC_CDR_DATA_Element is HAL.UInt16;

   --  DACC_DACC_CDR_DATA array
   type DACC_DACC_CDR_DATA_Field_Array is array (0 .. 1)
     of DACC_DACC_CDR_DATA_Element
     with Component_Size => 16, Size => 32;

   --  Conversion Data Register 0
   type DACC_DACC_CDR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
         when True =>
            --  DATA as an array
            Arr : DACC_DACC_CDR_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for DACC_DACC_CDR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Conversion Data Register 0
   type DACC_DACC_CDR_Registers is array (0 .. 1) of DACC_DACC_CDR_Register
     with Volatile;

   --  DACC_DACC_IER_TXRDY array
   type DACC_DACC_IER_TXRDY_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for DACC_DACC_IER_TXRDY
   type DACC_DACC_IER_TXRDY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TXRDY as a value
            Val : HAL.UInt2;
         when True =>
            --  TXRDY as an array
            Arr : DACC_DACC_IER_TXRDY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DACC_DACC_IER_TXRDY_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  DACC_DACC_IER_EOC array
   type DACC_DACC_IER_EOC_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for DACC_DACC_IER_EOC
   type DACC_DACC_IER_EOC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EOC as a value
            Val : HAL.UInt2;
         when True =>
            --  EOC as an array
            Arr : DACC_DACC_IER_EOC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DACC_DACC_IER_EOC_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Interrupt Enable Register
   type DACC_DACC_IER_Register is record
      --  Write-only. Transmit Ready Interrupt Enable of channel 0
      TXRDY         : DACC_DACC_IER_TXRDY_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_3  : HAL.UInt2 := 16#0#;
      --  Write-only. End of Conversion Interrupt Enable of channel 0
      EOC           : DACC_DACC_IER_EOC_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DACC_DACC_IER_Register use record
      TXRDY         at 0 range 0 .. 1;
      Reserved_2_3  at 0 range 2 .. 3;
      EOC           at 0 range 4 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  DACC_DACC_IDR_TXRDY array
   type DACC_DACC_IDR_TXRDY_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for DACC_DACC_IDR_TXRDY
   type DACC_DACC_IDR_TXRDY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TXRDY as a value
            Val : HAL.UInt2;
         when True =>
            --  TXRDY as an array
            Arr : DACC_DACC_IDR_TXRDY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DACC_DACC_IDR_TXRDY_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  DACC_DACC_IDR_EOC array
   type DACC_DACC_IDR_EOC_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for DACC_DACC_IDR_EOC
   type DACC_DACC_IDR_EOC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EOC as a value
            Val : HAL.UInt2;
         when True =>
            --  EOC as an array
            Arr : DACC_DACC_IDR_EOC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DACC_DACC_IDR_EOC_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Interrupt Disable Register
   type DACC_DACC_IDR_Register is record
      --  Write-only. Transmit Ready Interrupt Disable of channel 0
      TXRDY         : DACC_DACC_IDR_TXRDY_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_3  : HAL.UInt2 := 16#0#;
      --  Write-only. End of Conversion Interrupt Disable of channel 0
      EOC           : DACC_DACC_IDR_EOC_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DACC_DACC_IDR_Register use record
      TXRDY         at 0 range 0 .. 1;
      Reserved_2_3  at 0 range 2 .. 3;
      EOC           at 0 range 4 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  DACC_DACC_IMR_TXRDY array
   type DACC_DACC_IMR_TXRDY_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for DACC_DACC_IMR_TXRDY
   type DACC_DACC_IMR_TXRDY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TXRDY as a value
            Val : HAL.UInt2;
         when True =>
            --  TXRDY as an array
            Arr : DACC_DACC_IMR_TXRDY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DACC_DACC_IMR_TXRDY_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  DACC_DACC_IMR_EOC array
   type DACC_DACC_IMR_EOC_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for DACC_DACC_IMR_EOC
   type DACC_DACC_IMR_EOC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EOC as a value
            Val : HAL.UInt2;
         when True =>
            --  EOC as an array
            Arr : DACC_DACC_IMR_EOC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DACC_DACC_IMR_EOC_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Interrupt Mask Register
   type DACC_DACC_IMR_Register is record
      --  Read-only. Transmit Ready Interrupt Mask of channel 0
      TXRDY         : DACC_DACC_IMR_TXRDY_Field;
      --  unspecified
      Reserved_2_3  : HAL.UInt2;
      --  Read-only. End of Conversion Interrupt Mask of channel 0
      EOC           : DACC_DACC_IMR_EOC_Field;
      --  unspecified
      Reserved_6_31 : HAL.UInt26;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DACC_DACC_IMR_Register use record
      TXRDY         at 0 range 0 .. 1;
      Reserved_2_3  at 0 range 2 .. 3;
      EOC           at 0 range 4 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  DACC_DACC_ISR_TXRDY array
   type DACC_DACC_ISR_TXRDY_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for DACC_DACC_ISR_TXRDY
   type DACC_DACC_ISR_TXRDY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TXRDY as a value
            Val : HAL.UInt2;
         when True =>
            --  TXRDY as an array
            Arr : DACC_DACC_ISR_TXRDY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DACC_DACC_ISR_TXRDY_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  DACC_DACC_ISR_EOC array
   type DACC_DACC_ISR_EOC_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for DACC_DACC_ISR_EOC
   type DACC_DACC_ISR_EOC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EOC as a value
            Val : HAL.UInt2;
         when True =>
            --  EOC as an array
            Arr : DACC_DACC_ISR_EOC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DACC_DACC_ISR_EOC_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Interrupt Status Register
   type DACC_DACC_ISR_Register is record
      --  Read-only. Transmit Ready Interrupt Flag of channel 0
      TXRDY         : DACC_DACC_ISR_TXRDY_Field;
      --  unspecified
      Reserved_2_3  : HAL.UInt2;
      --  Read-only. End of Conversion Interrupt Flag of channel 0
      EOC           : DACC_DACC_ISR_EOC_Field;
      --  unspecified
      Reserved_6_31 : HAL.UInt26;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DACC_DACC_ISR_Register use record
      TXRDY         at 0 range 0 .. 1;
      Reserved_2_3  at 0 range 2 .. 3;
      EOC           at 0 range 4 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  DACC_DACC_ACR_IBCTLCH array element
   subtype DACC_DACC_ACR_IBCTLCH_Element is HAL.UInt2;

   --  DACC_DACC_ACR_IBCTLCH array
   type DACC_DACC_ACR_IBCTLCH_Field_Array is array (0 .. 1)
     of DACC_DACC_ACR_IBCTLCH_Element
     with Component_Size => 2, Size => 4;

   --  Type definition for DACC_DACC_ACR_IBCTLCH
   type DACC_DACC_ACR_IBCTLCH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IBCTLCH as a value
            Val : HAL.UInt4;
         when True =>
            --  IBCTLCH as an array
            Arr : DACC_DACC_ACR_IBCTLCH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for DACC_DACC_ACR_IBCTLCH_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Analog Current Register
   type DACC_DACC_ACR_Register is record
      --  Analog Output Current Control
      IBCTLCH       : DACC_DACC_ACR_IBCTLCH_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DACC_DACC_ACR_Register use record
      IBCTLCH       at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Write Protect Key
   type DACC_WPMR_WPKEY_Field is
     (
      --  Reset value for the field
      Dacc_Wpmr_Wpkey_Field_Reset,
      --  Writing any other value in this field aborts the write operation of
      --  bit WPEN.Always reads as 0.
      Passwd)
     with Size => 24;
   for DACC_WPMR_WPKEY_Field use
     (Dacc_Wpmr_Wpkey_Field_Reset => 0,
      Passwd => 4473155);

   --  Write Protection Mode Register
   type DACC_DACC_WPMR_Register is record
      --  Write Protection Enable
      WPEN         : Boolean := False;
      --  unspecified
      Reserved_1_7 : HAL.UInt7 := 16#0#;
      --  Write Protect Key
      WPKEY        : DACC_WPMR_WPKEY_Field := Dacc_Wpmr_Wpkey_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DACC_DACC_WPMR_Register use record
      WPEN         at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
      WPKEY        at 0 range 8 .. 31;
   end record;

   subtype DACC_DACC_WPSR_WPVSRC_Field is HAL.UInt8;

   --  Write Protection Status Register
   type DACC_DACC_WPSR_Register is record
      --  Read-only. Write Protection Violation Status
      WPVS           : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. Write Protection Violation Source
      WPVSRC         : DACC_DACC_WPSR_WPVSRC_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DACC_DACC_WPSR_Register use record
      WPVS           at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      WPVSRC         at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DACC_DACC_VERSION_VERSION_Field is HAL.UInt12;
   subtype DACC_DACC_VERSION_MFN_Field is HAL.UInt3;

   --  Version Register
   type DACC_DACC_VERSION_Register is record
      --  Read-only. Version
      VERSION        : DACC_DACC_VERSION_VERSION_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Metal Fix Number
      MFN            : DACC_DACC_VERSION_MFN_Field;
      --  unspecified
      Reserved_19_31 : HAL.UInt13;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DACC_DACC_VERSION_Register use record
      VERSION        at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      MFN            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Digital-to-Analog Converter Controller
   type DACC_Peripheral is record
      --  Control Register
      DACC_CR      : aliased DACC_DACC_CR_Register;
      --  Mode Register
      DACC_MR      : aliased DACC_DACC_MR_Register;
      --  Trigger Register
      DACC_TRIGR   : aliased DACC_DACC_TRIGR_Register;
      --  Channel Enable Register
      DACC_CHER    : aliased DACC_DACC_CHER_Register;
      --  Channel Disable Register
      DACC_CHDR    : aliased DACC_DACC_CHDR_Register;
      --  Channel Status Register
      DACC_CHSR    : aliased DACC_DACC_CHSR_Register;
      --  Conversion Data Register 0
      DACC_CDR     : aliased DACC_DACC_CDR_Registers;
      --  Interrupt Enable Register
      DACC_IER     : aliased DACC_DACC_IER_Register;
      --  Interrupt Disable Register
      DACC_IDR     : aliased DACC_DACC_IDR_Register;
      --  Interrupt Mask Register
      DACC_IMR     : aliased DACC_DACC_IMR_Register;
      --  Interrupt Status Register
      DACC_ISR     : aliased DACC_DACC_ISR_Register;
      --  Analog Current Register
      DACC_ACR     : aliased DACC_DACC_ACR_Register;
      --  Write Protection Mode Register
      DACC_WPMR    : aliased DACC_DACC_WPMR_Register;
      --  Write Protection Status Register
      DACC_WPSR    : aliased DACC_DACC_WPSR_Register;
      --  Version Register
      DACC_VERSION : aliased DACC_DACC_VERSION_Register;
   end record
     with Volatile;

   for DACC_Peripheral use record
      DACC_CR      at 16#0# range 0 .. 31;
      DACC_MR      at 16#4# range 0 .. 31;
      DACC_TRIGR   at 16#8# range 0 .. 31;
      DACC_CHER    at 16#10# range 0 .. 31;
      DACC_CHDR    at 16#14# range 0 .. 31;
      DACC_CHSR    at 16#18# range 0 .. 31;
      DACC_CDR     at 16#1C# range 0 .. 63;
      DACC_IER     at 16#24# range 0 .. 31;
      DACC_IDR     at 16#28# range 0 .. 31;
      DACC_IMR     at 16#2C# range 0 .. 31;
      DACC_ISR     at 16#30# range 0 .. 31;
      DACC_ACR     at 16#94# range 0 .. 31;
      DACC_WPMR    at 16#E4# range 0 .. 31;
      DACC_WPSR    at 16#E8# range 0 .. 31;
      DACC_VERSION at 16#FC# range 0 .. 31;
   end record;

   --  Digital-to-Analog Converter Controller
   DACC_Periph : aliased DACC_Peripheral
     with Import, Address => System'To_Address (16#40040000#);

end SAM_SVD.DACC;
