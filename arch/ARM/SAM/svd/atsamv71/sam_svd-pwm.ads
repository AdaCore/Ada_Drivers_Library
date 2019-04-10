--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.PWM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  CLKA Divide Factor
   type PWM_CLK_DIVA_Field is
     (
      --  CLKA clock is turned off
      Clka_Poff,
      --  CLKA clock is clock selected by PREA
      Prea)
     with Size => 8;
   for PWM_CLK_DIVA_Field use
     (Clka_Poff => 0,
      Prea => 1);

   --  CLKA Source Clock Selection
   type PWM_CLK_PREA_Field is
     (
      --  Peripheral clock
      Clk,
      --  Peripheral clock/2
      Clk_Div2,
      --  Peripheral clock/4
      Clk_Div4,
      --  Peripheral clock/8
      Clk_Div8,
      --  Peripheral clock/16
      Clk_Div16,
      --  Peripheral clock/32
      Clk_Div32,
      --  Peripheral clock/64
      Clk_Div64,
      --  Peripheral clock/128
      Clk_Div128,
      --  Peripheral clock/256
      Clk_Div256,
      --  Peripheral clock/512
      Clk_Div512,
      --  Peripheral clock/1024
      Clk_Div1024)
     with Size => 4;
   for PWM_CLK_PREA_Field use
     (Clk => 0,
      Clk_Div2 => 1,
      Clk_Div4 => 2,
      Clk_Div8 => 3,
      Clk_Div16 => 4,
      Clk_Div32 => 5,
      Clk_Div64 => 6,
      Clk_Div128 => 7,
      Clk_Div256 => 8,
      Clk_Div512 => 9,
      Clk_Div1024 => 10);

   --  CLKB Divide Factor
   type PWM_CLK_DIVB_Field is
     (
      --  CLKB clock is turned off
      Clkb_Poff,
      --  CLKB clock is clock selected by PREB
      Preb)
     with Size => 8;
   for PWM_CLK_DIVB_Field use
     (Clkb_Poff => 0,
      Preb => 1);

   --  CLKB Source Clock Selection
   type PWM_CLK_PREB_Field is
     (
      --  Peripheral clock
      Clk,
      --  Peripheral clock/2
      Clk_Div2,
      --  Peripheral clock/4
      Clk_Div4,
      --  Peripheral clock/8
      Clk_Div8,
      --  Peripheral clock/16
      Clk_Div16,
      --  Peripheral clock/32
      Clk_Div32,
      --  Peripheral clock/64
      Clk_Div64,
      --  Peripheral clock/128
      Clk_Div128,
      --  Peripheral clock/256
      Clk_Div256,
      --  Peripheral clock/512
      Clk_Div512,
      --  Peripheral clock/1024
      Clk_Div1024)
     with Size => 4;
   for PWM_CLK_PREB_Field use
     (Clk => 0,
      Clk_Div2 => 1,
      Clk_Div4 => 2,
      Clk_Div8 => 3,
      Clk_Div16 => 4,
      Clk_Div32 => 5,
      Clk_Div64 => 6,
      Clk_Div128 => 7,
      Clk_Div256 => 8,
      Clk_Div512 => 9,
      Clk_Div1024 => 10);

   --  PWM Clock Register
   type PWM_PWM_CLK_Register is record
      --  CLKA Divide Factor
      DIVA           : PWM_CLK_DIVA_Field := SAM_SVD.PWM.Clka_Poff;
      --  CLKA Source Clock Selection
      PREA           : PWM_CLK_PREA_Field := SAM_SVD.PWM.Clk;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  CLKB Divide Factor
      DIVB           : PWM_CLK_DIVB_Field := SAM_SVD.PWM.Clkb_Poff;
      --  CLKB Source Clock Selection
      PREB           : PWM_CLK_PREB_Field := SAM_SVD.PWM.Clk;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_CLK_Register use record
      DIVA           at 0 range 0 .. 7;
      PREA           at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      DIVB           at 0 range 16 .. 23;
      PREB           at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  PWM_PWM_ENA_CHID array
   type PWM_PWM_ENA_CHID_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_ENA_CHID
   type PWM_PWM_ENA_CHID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CHID as a value
            Val : HAL.UInt4;
         when True =>
            --  CHID as an array
            Arr : PWM_PWM_ENA_CHID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_ENA_CHID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM Enable Register
   type PWM_PWM_ENA_Register is record
      --  Write-only. Channel ID
      CHID          : PWM_PWM_ENA_CHID_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_ENA_Register use record
      CHID          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  PWM_PWM_DIS_CHID array
   type PWM_PWM_DIS_CHID_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_DIS_CHID
   type PWM_PWM_DIS_CHID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CHID as a value
            Val : HAL.UInt4;
         when True =>
            --  CHID as an array
            Arr : PWM_PWM_DIS_CHID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_DIS_CHID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM Disable Register
   type PWM_PWM_DIS_Register is record
      --  Write-only. Channel ID
      CHID          : PWM_PWM_DIS_CHID_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_DIS_Register use record
      CHID          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  PWM_PWM_SR_CHID array
   type PWM_PWM_SR_CHID_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_SR_CHID
   type PWM_PWM_SR_CHID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CHID as a value
            Val : HAL.UInt4;
         when True =>
            --  CHID as an array
            Arr : PWM_PWM_SR_CHID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_SR_CHID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM Status Register
   type PWM_PWM_SR_Register is record
      --  Read-only. Channel ID
      CHID          : PWM_PWM_SR_CHID_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_SR_Register use record
      CHID          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  PWM_PWM_IER1_CHID array
   type PWM_PWM_IER1_CHID_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_IER1_CHID
   type PWM_PWM_IER1_CHID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CHID as a value
            Val : HAL.UInt4;
         when True =>
            --  CHID as an array
            Arr : PWM_PWM_IER1_CHID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_IER1_CHID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM_PWM_IER1_FCHID array
   type PWM_PWM_IER1_FCHID_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_IER1_FCHID
   type PWM_PWM_IER1_FCHID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FCHID as a value
            Val : HAL.UInt4;
         when True =>
            --  FCHID as an array
            Arr : PWM_PWM_IER1_FCHID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_IER1_FCHID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM Interrupt Enable Register 1
   type PWM_PWM_IER1_Register is record
      --  Write-only. Counter Event on Channel 0 Interrupt Enable
      CHID           : PWM_PWM_IER1_CHID_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      --  Write-only. Fault Protection Trigger on Channel 0 Interrupt Enable
      FCHID          : PWM_PWM_IER1_FCHID_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_IER1_Register use record
      CHID           at 0 range 0 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      FCHID          at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  PWM_PWM_IDR1_CHID array
   type PWM_PWM_IDR1_CHID_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_IDR1_CHID
   type PWM_PWM_IDR1_CHID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CHID as a value
            Val : HAL.UInt4;
         when True =>
            --  CHID as an array
            Arr : PWM_PWM_IDR1_CHID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_IDR1_CHID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM_PWM_IDR1_FCHID array
   type PWM_PWM_IDR1_FCHID_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_IDR1_FCHID
   type PWM_PWM_IDR1_FCHID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FCHID as a value
            Val : HAL.UInt4;
         when True =>
            --  FCHID as an array
            Arr : PWM_PWM_IDR1_FCHID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_IDR1_FCHID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM Interrupt Disable Register 1
   type PWM_PWM_IDR1_Register is record
      --  Write-only. Counter Event on Channel 0 Interrupt Disable
      CHID           : PWM_PWM_IDR1_CHID_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      --  Write-only. Fault Protection Trigger on Channel 0 Interrupt Disable
      FCHID          : PWM_PWM_IDR1_FCHID_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_IDR1_Register use record
      CHID           at 0 range 0 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      FCHID          at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  PWM_PWM_IMR1_CHID array
   type PWM_PWM_IMR1_CHID_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_IMR1_CHID
   type PWM_PWM_IMR1_CHID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CHID as a value
            Val : HAL.UInt4;
         when True =>
            --  CHID as an array
            Arr : PWM_PWM_IMR1_CHID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_IMR1_CHID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM_PWM_IMR1_FCHID array
   type PWM_PWM_IMR1_FCHID_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_IMR1_FCHID
   type PWM_PWM_IMR1_FCHID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FCHID as a value
            Val : HAL.UInt4;
         when True =>
            --  FCHID as an array
            Arr : PWM_PWM_IMR1_FCHID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_IMR1_FCHID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM Interrupt Mask Register 1
   type PWM_PWM_IMR1_Register is record
      --  Read-only. Counter Event on Channel 0 Interrupt Mask
      CHID           : PWM_PWM_IMR1_CHID_Field;
      --  unspecified
      Reserved_4_15  : HAL.UInt12;
      --  Read-only. Fault Protection Trigger on Channel 0 Interrupt Mask
      FCHID          : PWM_PWM_IMR1_FCHID_Field;
      --  unspecified
      Reserved_20_31 : HAL.UInt12;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_IMR1_Register use record
      CHID           at 0 range 0 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      FCHID          at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  PWM_PWM_ISR1_CHID array
   type PWM_PWM_ISR1_CHID_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_ISR1_CHID
   type PWM_PWM_ISR1_CHID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CHID as a value
            Val : HAL.UInt4;
         when True =>
            --  CHID as an array
            Arr : PWM_PWM_ISR1_CHID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_ISR1_CHID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM_PWM_ISR1_FCHID array
   type PWM_PWM_ISR1_FCHID_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_ISR1_FCHID
   type PWM_PWM_ISR1_FCHID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FCHID as a value
            Val : HAL.UInt4;
         when True =>
            --  FCHID as an array
            Arr : PWM_PWM_ISR1_FCHID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_ISR1_FCHID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM Interrupt Status Register 1
   type PWM_PWM_ISR1_Register is record
      --  Read-only. Counter Event on Channel 0
      CHID           : PWM_PWM_ISR1_CHID_Field;
      --  unspecified
      Reserved_4_15  : HAL.UInt12;
      --  Read-only. Fault Protection Trigger on Channel 0
      FCHID          : PWM_PWM_ISR1_FCHID_Field;
      --  unspecified
      Reserved_20_31 : HAL.UInt12;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_ISR1_Register use record
      CHID           at 0 range 0 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      FCHID          at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  PWM_PWM_SCM_SYNC array
   type PWM_PWM_SCM_SYNC_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_SCM_SYNC
   type PWM_PWM_SCM_SYNC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SYNC as a value
            Val : HAL.UInt4;
         when True =>
            --  SYNC as an array
            Arr : PWM_PWM_SCM_SYNC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_SCM_SYNC_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Synchronous Channels Update Mode
   type PWM_SCM_UPDM_Field is
     (
      --  Manual write of double buffer registers and manual update of
      --  synchronous channels
      Mode0,
      --  Manual write of double buffer registers and automatic update of
      --  synchronous channels
      Mode1,
      --  Automatic write of duty-cycle update registers by the DMA Controller
      --  and automatic update of synchronous channels
      Mode2)
     with Size => 2;
   for PWM_SCM_UPDM_Field use
     (Mode0 => 0,
      Mode1 => 1,
      Mode2 => 2);

   subtype PWM_PWM_SCM_PTRCS_Field is HAL.UInt3;

   --  PWM Sync Channels Mode Register
   type PWM_PWM_SCM_Register is record
      --  Synchronous Channel 0
      SYNC           : PWM_PWM_SCM_SYNC_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      --  Synchronous Channels Update Mode
      UPDM           : PWM_SCM_UPDM_Field := SAM_SVD.PWM.Mode0;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  DMA Controller Transfer Request Mode
      PTRM           : Boolean := False;
      --  DMA Controller Transfer Request Comparison Selection
      PTRCS          : PWM_PWM_SCM_PTRCS_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_SCM_Register use record
      SYNC           at 0 range 0 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      UPDM           at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      PTRM           at 0 range 20 .. 20;
      PTRCS          at 0 range 21 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype PWM_PWM_DMAR_DMADUTY_Field is HAL.UInt24;

   --  PWM DMA Register
   type PWM_PWM_DMAR_Register is record
      --  Write-only. Duty-Cycle Holding Register for DMA Access
      DMADUTY        : PWM_PWM_DMAR_DMADUTY_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_DMAR_Register use record
      DMADUTY        at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  PWM Sync Channels Update Control Register
   type PWM_PWM_SCUC_Register is record
      --  Synchronous Channels Update Unlock
      UPDULOCK      : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_SCUC_Register use record
      UPDULOCK      at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype PWM_PWM_SCUP_UPR_Field is HAL.UInt4;
   subtype PWM_PWM_SCUP_UPRCNT_Field is HAL.UInt4;

   --  PWM Sync Channels Update Period Register
   type PWM_PWM_SCUP_Register is record
      --  Update Period
      UPR           : PWM_PWM_SCUP_UPR_Field := 16#0#;
      --  Update Period Counter
      UPRCNT        : PWM_PWM_SCUP_UPRCNT_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_SCUP_Register use record
      UPR           at 0 range 0 .. 3;
      UPRCNT        at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype PWM_PWM_SCUPUPD_UPRUPD_Field is HAL.UInt4;

   --  PWM Sync Channels Update Period Update Register
   type PWM_PWM_SCUPUPD_Register is record
      --  Write-only. Update Period Update
      UPRUPD        : PWM_PWM_SCUPUPD_UPRUPD_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_SCUPUPD_Register use record
      UPRUPD        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  PWM_PWM_IER2_CMPM array
   type PWM_PWM_IER2_CMPM_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for PWM_PWM_IER2_CMPM
   type PWM_PWM_IER2_CMPM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMPM as a value
            Val : HAL.UInt8;
         when True =>
            --  CMPM as an array
            Arr : PWM_PWM_IER2_CMPM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for PWM_PWM_IER2_CMPM_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  PWM_PWM_IER2_CMPU array
   type PWM_PWM_IER2_CMPU_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for PWM_PWM_IER2_CMPU
   type PWM_PWM_IER2_CMPU_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMPU as a value
            Val : HAL.UInt8;
         when True =>
            --  CMPU as an array
            Arr : PWM_PWM_IER2_CMPU_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for PWM_PWM_IER2_CMPU_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  PWM Interrupt Enable Register 2
   type PWM_PWM_IER2_Register is record
      --  Write-only. Write Ready for Synchronous Channels Update Interrupt
      --  Enable
      WRDY           : Boolean := False;
      --  unspecified
      Reserved_1_2   : HAL.UInt2 := 16#0#;
      --  Write-only. Synchronous Channels Update Underrun Error Interrupt
      --  Enable
      UNRE           : Boolean := False;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Write-only. Comparison 0 Match Interrupt Enable
      CMPM           : PWM_PWM_IER2_CMPM_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Write-only. Comparison 0 Update Interrupt Enable
      CMPU           : PWM_PWM_IER2_CMPU_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_IER2_Register use record
      WRDY           at 0 range 0 .. 0;
      Reserved_1_2   at 0 range 1 .. 2;
      UNRE           at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      CMPM           at 0 range 8 .. 15;
      CMPU           at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  PWM_PWM_IDR2_CMPM array
   type PWM_PWM_IDR2_CMPM_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for PWM_PWM_IDR2_CMPM
   type PWM_PWM_IDR2_CMPM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMPM as a value
            Val : HAL.UInt8;
         when True =>
            --  CMPM as an array
            Arr : PWM_PWM_IDR2_CMPM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for PWM_PWM_IDR2_CMPM_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  PWM_PWM_IDR2_CMPU array
   type PWM_PWM_IDR2_CMPU_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for PWM_PWM_IDR2_CMPU
   type PWM_PWM_IDR2_CMPU_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMPU as a value
            Val : HAL.UInt8;
         when True =>
            --  CMPU as an array
            Arr : PWM_PWM_IDR2_CMPU_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for PWM_PWM_IDR2_CMPU_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  PWM Interrupt Disable Register 2
   type PWM_PWM_IDR2_Register is record
      --  Write-only. Write Ready for Synchronous Channels Update Interrupt
      --  Disable
      WRDY           : Boolean := False;
      --  unspecified
      Reserved_1_2   : HAL.UInt2 := 16#0#;
      --  Write-only. Synchronous Channels Update Underrun Error Interrupt
      --  Disable
      UNRE           : Boolean := False;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Write-only. Comparison 0 Match Interrupt Disable
      CMPM           : PWM_PWM_IDR2_CMPM_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Write-only. Comparison 0 Update Interrupt Disable
      CMPU           : PWM_PWM_IDR2_CMPU_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_IDR2_Register use record
      WRDY           at 0 range 0 .. 0;
      Reserved_1_2   at 0 range 1 .. 2;
      UNRE           at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      CMPM           at 0 range 8 .. 15;
      CMPU           at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  PWM_PWM_IMR2_CMPM array
   type PWM_PWM_IMR2_CMPM_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for PWM_PWM_IMR2_CMPM
   type PWM_PWM_IMR2_CMPM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMPM as a value
            Val : HAL.UInt8;
         when True =>
            --  CMPM as an array
            Arr : PWM_PWM_IMR2_CMPM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for PWM_PWM_IMR2_CMPM_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  PWM_PWM_IMR2_CMPU array
   type PWM_PWM_IMR2_CMPU_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for PWM_PWM_IMR2_CMPU
   type PWM_PWM_IMR2_CMPU_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMPU as a value
            Val : HAL.UInt8;
         when True =>
            --  CMPU as an array
            Arr : PWM_PWM_IMR2_CMPU_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for PWM_PWM_IMR2_CMPU_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  PWM Interrupt Mask Register 2
   type PWM_PWM_IMR2_Register is record
      --  Read-only. Write Ready for Synchronous Channels Update Interrupt Mask
      WRDY           : Boolean;
      --  unspecified
      Reserved_1_2   : HAL.UInt2;
      --  Read-only. Synchronous Channels Update Underrun Error Interrupt Mask
      UNRE           : Boolean;
      --  unspecified
      Reserved_4_7   : HAL.UInt4;
      --  Read-only. Comparison 0 Match Interrupt Mask
      CMPM           : PWM_PWM_IMR2_CMPM_Field;
      --  Read-only. Comparison 0 Update Interrupt Mask
      CMPU           : PWM_PWM_IMR2_CMPU_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_IMR2_Register use record
      WRDY           at 0 range 0 .. 0;
      Reserved_1_2   at 0 range 1 .. 2;
      UNRE           at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      CMPM           at 0 range 8 .. 15;
      CMPU           at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  PWM_PWM_ISR2_CMPM array
   type PWM_PWM_ISR2_CMPM_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for PWM_PWM_ISR2_CMPM
   type PWM_PWM_ISR2_CMPM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMPM as a value
            Val : HAL.UInt8;
         when True =>
            --  CMPM as an array
            Arr : PWM_PWM_ISR2_CMPM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for PWM_PWM_ISR2_CMPM_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  PWM_PWM_ISR2_CMPU array
   type PWM_PWM_ISR2_CMPU_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for PWM_PWM_ISR2_CMPU
   type PWM_PWM_ISR2_CMPU_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CMPU as a value
            Val : HAL.UInt8;
         when True =>
            --  CMPU as an array
            Arr : PWM_PWM_ISR2_CMPU_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for PWM_PWM_ISR2_CMPU_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  PWM Interrupt Status Register 2
   type PWM_PWM_ISR2_Register is record
      --  Read-only. Write Ready for Synchronous Channels Update
      WRDY           : Boolean;
      --  unspecified
      Reserved_1_2   : HAL.UInt2;
      --  Read-only. Synchronous Channels Update Underrun Error
      UNRE           : Boolean;
      --  unspecified
      Reserved_4_7   : HAL.UInt4;
      --  Read-only. Comparison 0 Match
      CMPM           : PWM_PWM_ISR2_CMPM_Field;
      --  Read-only. Comparison 0 Update
      CMPU           : PWM_PWM_ISR2_CMPU_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_ISR2_Register use record
      WRDY           at 0 range 0 .. 0;
      Reserved_1_2   at 0 range 1 .. 2;
      UNRE           at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      CMPM           at 0 range 8 .. 15;
      CMPU           at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  PWM_PWM_OOV_OOVH array
   type PWM_PWM_OOV_OOVH_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_OOV_OOVH
   type PWM_PWM_OOV_OOVH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OOVH as a value
            Val : HAL.UInt4;
         when True =>
            --  OOVH as an array
            Arr : PWM_PWM_OOV_OOVH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_OOV_OOVH_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM_PWM_OOV_OOVL array
   type PWM_PWM_OOV_OOVL_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_OOV_OOVL
   type PWM_PWM_OOV_OOVL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OOVL as a value
            Val : HAL.UInt4;
         when True =>
            --  OOVL as an array
            Arr : PWM_PWM_OOV_OOVL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_OOV_OOVL_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM Output Override Value Register
   type PWM_PWM_OOV_Register is record
      --  Output Override Value for PWMH output of the channel 0
      OOVH           : PWM_PWM_OOV_OOVH_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      --  Output Override Value for PWML output of the channel 0
      OOVL           : PWM_PWM_OOV_OOVL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_OOV_Register use record
      OOVH           at 0 range 0 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      OOVL           at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  PWM_PWM_OS_OSH array
   type PWM_PWM_OS_OSH_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_OS_OSH
   type PWM_PWM_OS_OSH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OSH as a value
            Val : HAL.UInt4;
         when True =>
            --  OSH as an array
            Arr : PWM_PWM_OS_OSH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_OS_OSH_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM_PWM_OS_OSL array
   type PWM_PWM_OS_OSL_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_OS_OSL
   type PWM_PWM_OS_OSL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OSL as a value
            Val : HAL.UInt4;
         when True =>
            --  OSL as an array
            Arr : PWM_PWM_OS_OSL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_OS_OSL_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM Output Selection Register
   type PWM_PWM_OS_Register is record
      --  Output Selection for PWMH output of the channel 0
      OSH            : PWM_PWM_OS_OSH_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      --  Output Selection for PWML output of the channel 0
      OSL            : PWM_PWM_OS_OSL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_OS_Register use record
      OSH            at 0 range 0 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      OSL            at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  PWM_PWM_OSS_OSSH array
   type PWM_PWM_OSS_OSSH_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_OSS_OSSH
   type PWM_PWM_OSS_OSSH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OSSH as a value
            Val : HAL.UInt4;
         when True =>
            --  OSSH as an array
            Arr : PWM_PWM_OSS_OSSH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_OSS_OSSH_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM_PWM_OSS_OSSL array
   type PWM_PWM_OSS_OSSL_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_OSS_OSSL
   type PWM_PWM_OSS_OSSL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OSSL as a value
            Val : HAL.UInt4;
         when True =>
            --  OSSL as an array
            Arr : PWM_PWM_OSS_OSSL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_OSS_OSSL_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM Output Selection Set Register
   type PWM_PWM_OSS_Register is record
      --  Write-only. Output Selection Set for PWMH output of the channel 0
      OSSH           : PWM_PWM_OSS_OSSH_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      --  Write-only. Output Selection Set for PWML output of the channel 0
      OSSL           : PWM_PWM_OSS_OSSL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_OSS_Register use record
      OSSH           at 0 range 0 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      OSSL           at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  PWM_PWM_OSC_OSCH array
   type PWM_PWM_OSC_OSCH_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_OSC_OSCH
   type PWM_PWM_OSC_OSCH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OSCH as a value
            Val : HAL.UInt4;
         when True =>
            --  OSCH as an array
            Arr : PWM_PWM_OSC_OSCH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_OSC_OSCH_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM_PWM_OSC_OSCL array
   type PWM_PWM_OSC_OSCL_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_OSC_OSCL
   type PWM_PWM_OSC_OSCL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OSCL as a value
            Val : HAL.UInt4;
         when True =>
            --  OSCL as an array
            Arr : PWM_PWM_OSC_OSCL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_OSC_OSCL_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM Output Selection Clear Register
   type PWM_PWM_OSC_Register is record
      --  Write-only. Output Selection Clear for PWMH output of the channel 0
      OSCH           : PWM_PWM_OSC_OSCH_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      --  Write-only. Output Selection Clear for PWML output of the channel 0
      OSCL           : PWM_PWM_OSC_OSCL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_OSC_Register use record
      OSCH           at 0 range 0 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      OSCL           at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  PWM_PWM_OSSUPD_OSSUPH array
   type PWM_PWM_OSSUPD_OSSUPH_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_OSSUPD_OSSUPH
   type PWM_PWM_OSSUPD_OSSUPH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OSSUPH as a value
            Val : HAL.UInt4;
         when True =>
            --  OSSUPH as an array
            Arr : PWM_PWM_OSSUPD_OSSUPH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_OSSUPD_OSSUPH_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM_PWM_OSSUPD_OSSUPL array
   type PWM_PWM_OSSUPD_OSSUPL_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_OSSUPD_OSSUPL
   type PWM_PWM_OSSUPD_OSSUPL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OSSUPL as a value
            Val : HAL.UInt4;
         when True =>
            --  OSSUPL as an array
            Arr : PWM_PWM_OSSUPD_OSSUPL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_OSSUPD_OSSUPL_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM Output Selection Set Update Register
   type PWM_PWM_OSSUPD_Register is record
      --  Write-only. Output Selection Set for PWMH output of the channel 0
      OSSUPH         : PWM_PWM_OSSUPD_OSSUPH_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      --  Write-only. Output Selection Set for PWML output of the channel 0
      OSSUPL         : PWM_PWM_OSSUPD_OSSUPL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_OSSUPD_Register use record
      OSSUPH         at 0 range 0 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      OSSUPL         at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  PWM_PWM_OSCUPD_OSCUPH array
   type PWM_PWM_OSCUPD_OSCUPH_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_OSCUPD_OSCUPH
   type PWM_PWM_OSCUPD_OSCUPH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OSCUPH as a value
            Val : HAL.UInt4;
         when True =>
            --  OSCUPH as an array
            Arr : PWM_PWM_OSCUPD_OSCUPH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_OSCUPD_OSCUPH_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM_PWM_OSCUPD_OSCUPL array
   type PWM_PWM_OSCUPD_OSCUPL_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_OSCUPD_OSCUPL
   type PWM_PWM_OSCUPD_OSCUPL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OSCUPL as a value
            Val : HAL.UInt4;
         when True =>
            --  OSCUPL as an array
            Arr : PWM_PWM_OSCUPD_OSCUPL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_OSCUPD_OSCUPL_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM Output Selection Clear Update Register
   type PWM_PWM_OSCUPD_Register is record
      --  Write-only. Output Selection Clear for PWMH output of the channel 0
      OSCUPH         : PWM_PWM_OSCUPD_OSCUPH_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      --  Write-only. Output Selection Clear for PWML output of the channel 0
      OSCUPL         : PWM_PWM_OSCUPD_OSCUPL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_OSCUPD_Register use record
      OSCUPH         at 0 range 0 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      OSCUPL         at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype PWM_PWM_FMR_FPOL_Field is HAL.UInt8;
   subtype PWM_PWM_FMR_FMOD_Field is HAL.UInt8;
   subtype PWM_PWM_FMR_FFIL_Field is HAL.UInt8;

   --  PWM Fault Mode Register
   type PWM_PWM_FMR_Register is record
      --  Fault Polarity
      FPOL           : PWM_PWM_FMR_FPOL_Field := 16#0#;
      --  Fault Activation Mode
      FMOD           : PWM_PWM_FMR_FMOD_Field := 16#0#;
      --  Fault Filtering
      FFIL           : PWM_PWM_FMR_FFIL_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_FMR_Register use record
      FPOL           at 0 range 0 .. 7;
      FMOD           at 0 range 8 .. 15;
      FFIL           at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype PWM_PWM_FSR_FIV_Field is HAL.UInt8;
   subtype PWM_PWM_FSR_FS_Field is HAL.UInt8;

   --  PWM Fault Status Register
   type PWM_PWM_FSR_Register is record
      --  Read-only. Fault Input Value
      FIV            : PWM_PWM_FSR_FIV_Field;
      --  Read-only. Fault Status
      FS             : PWM_PWM_FSR_FS_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_FSR_Register use record
      FIV            at 0 range 0 .. 7;
      FS             at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype PWM_PWM_FCR_FCLR_Field is HAL.UInt8;

   --  PWM Fault Clear Register
   type PWM_PWM_FCR_Register is record
      --  Write-only. Fault Clear
      FCLR          : PWM_PWM_FCR_FCLR_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_FCR_Register use record
      FCLR          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  PWM_PWM_FPV1_FPVH array
   type PWM_PWM_FPV1_FPVH_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_FPV1_FPVH
   type PWM_PWM_FPV1_FPVH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FPVH as a value
            Val : HAL.UInt4;
         when True =>
            --  FPVH as an array
            Arr : PWM_PWM_FPV1_FPVH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_FPV1_FPVH_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM_PWM_FPV1_FPVL array
   type PWM_PWM_FPV1_FPVL_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_FPV1_FPVL
   type PWM_PWM_FPV1_FPVL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FPVL as a value
            Val : HAL.UInt4;
         when True =>
            --  FPVL as an array
            Arr : PWM_PWM_FPV1_FPVL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_FPV1_FPVL_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM Fault Protection Value Register 1
   type PWM_PWM_FPV1_Register is record
      --  Fault Protection Value for PWMH output on channel 0
      FPVH           : PWM_PWM_FPV1_FPVH_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      --  Fault Protection Value for PWML output on channel 0
      FPVL           : PWM_PWM_FPV1_FPVL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_FPV1_Register use record
      FPVH           at 0 range 0 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      FPVL           at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  PWM_PWM_FPE_FPE array element
   subtype PWM_PWM_FPE_FPE_Element is HAL.UInt8;

   --  PWM_PWM_FPE_FPE array
   type PWM_PWM_FPE_FPE_Field_Array is array (0 .. 3)
     of PWM_PWM_FPE_FPE_Element
     with Component_Size => 8, Size => 32;

   --  PWM Fault Protection Enable Register
   type PWM_PWM_FPE_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FPE as a value
            Val : HAL.UInt32;
         when True =>
            --  FPE as an array
            Arr : PWM_PWM_FPE_FPE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_FPE_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  PWM_PWM_ELMR_CSEL array
   type PWM_PWM_ELMR_CSEL_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for PWM_PWM_ELMR_CSEL
   type PWM_PWM_ELMR_CSEL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CSEL as a value
            Val : HAL.UInt8;
         when True =>
            --  CSEL as an array
            Arr : PWM_PWM_ELMR_CSEL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for PWM_PWM_ELMR_CSEL_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  PWM Event Line 0 Mode Register 0
   type PWM_PWM_ELMR_Register is record
      --  Comparison 0 Selection
      CSEL          : PWM_PWM_ELMR_CSEL_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_ELMR_Register use record
      CSEL          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  PWM Event Line 0 Mode Register 0
   type PWM_PWM_ELMR_Registers is array (0 .. 1) of PWM_PWM_ELMR_Register
     with Volatile;

   subtype PWM_PWM_SSPR_SPRD_Field is HAL.UInt24;

   --  PWM Spread Spectrum Register
   type PWM_PWM_SSPR_Register is record
      --  Spread Spectrum Limit Value
      SPRD           : PWM_PWM_SSPR_SPRD_Field := 16#0#;
      --  Spread Spectrum Counter Mode
      SPRDM          : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_SSPR_Register use record
      SPRD           at 0 range 0 .. 23;
      SPRDM          at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype PWM_PWM_SSPUP_SPRDUP_Field is HAL.UInt24;

   --  PWM Spread Spectrum Update Register
   type PWM_PWM_SSPUP_Register is record
      --  Write-only. Spread Spectrum Limit Value Update
      SPRDUP         : PWM_PWM_SSPUP_SPRDUP_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_SSPUP_Register use record
      SPRDUP         at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  PWM_PWM_SMMR_GCEN array
   type PWM_PWM_SMMR_GCEN_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for PWM_PWM_SMMR_GCEN
   type PWM_PWM_SMMR_GCEN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  GCEN as a value
            Val : HAL.UInt2;
         when True =>
            --  GCEN as an array
            Arr : PWM_PWM_SMMR_GCEN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for PWM_PWM_SMMR_GCEN_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  PWM_PWM_SMMR_DOWN array
   type PWM_PWM_SMMR_DOWN_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for PWM_PWM_SMMR_DOWN
   type PWM_PWM_SMMR_DOWN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DOWN as a value
            Val : HAL.UInt2;
         when True =>
            --  DOWN as an array
            Arr : PWM_PWM_SMMR_DOWN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for PWM_PWM_SMMR_DOWN_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  PWM Stepper Motor Mode Register
   type PWM_PWM_SMMR_Register is record
      --  Gray Count ENable
      GCEN           : PWM_PWM_SMMR_GCEN_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  DOWN Count
      DOWN           : PWM_PWM_SMMR_DOWN_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_SMMR_Register use record
      GCEN           at 0 range 0 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      DOWN           at 0 range 16 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  PWM_PWM_FPV2_FPZH array
   type PWM_PWM_FPV2_FPZH_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_FPV2_FPZH
   type PWM_PWM_FPV2_FPZH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FPZH as a value
            Val : HAL.UInt4;
         when True =>
            --  FPZH as an array
            Arr : PWM_PWM_FPV2_FPZH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_FPV2_FPZH_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM_PWM_FPV2_FPZL array
   type PWM_PWM_FPV2_FPZL_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PWM_PWM_FPV2_FPZL
   type PWM_PWM_FPV2_FPZL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FPZL as a value
            Val : HAL.UInt4;
         when True =>
            --  FPZL as an array
            Arr : PWM_PWM_FPV2_FPZL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PWM_PWM_FPV2_FPZL_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PWM Fault Protection Value 2 Register
   type PWM_PWM_FPV2_Register is record
      --  Fault Protection to Hi-Z for PWMH output on channel 0
      FPZH           : PWM_PWM_FPV2_FPZH_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      --  Fault Protection to Hi-Z for PWML output on channel 0
      FPZL           : PWM_PWM_FPV2_FPZL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_FPV2_Register use record
      FPZH           at 0 range 0 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      FPZL           at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Write Protection Command
   type PWM_WPCR_WPCMD_Field is
     (
      --  Disables the software write protection of the register groups of
      --  which the bit WPRGx is at '1'.
      Disable_Sw_Prot,
      --  Enables the software write protection of the register groups of which
      --  the bit WPRGx is at '1'.
      Enable_Sw_Prot,
      --  Enables the hardware write protection of the register groups of which
      --  the bit WPRGx is at '1'. Only a hardware reset of the PWM controller
      --  can disable the hardware write protection. Moreover, to meet security
      --  requirements, the PIO lines associated with the PWM can not be
      --  configured through the PIO interface.
      Enable_Hw_Prot)
     with Size => 2;
   for PWM_WPCR_WPCMD_Field use
     (Disable_Sw_Prot => 0,
      Enable_Sw_Prot => 1,
      Enable_Hw_Prot => 2);

   --  PWM_PWM_WPCR_WPRG array
   type PWM_PWM_WPCR_WPRG_Field_Array is array (0 .. 5) of Boolean
     with Component_Size => 1, Size => 6;

   --  Type definition for PWM_PWM_WPCR_WPRG
   type PWM_PWM_WPCR_WPRG_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WPRG as a value
            Val : HAL.UInt6;
         when True =>
            --  WPRG as an array
            Arr : PWM_PWM_WPCR_WPRG_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for PWM_PWM_WPCR_WPRG_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   --  Write Protection Key
   type PWM_WPCR_WPKEY_Field is
     (
      --  Reset value for the field
      Pwm_Wpcr_Wpkey_Field_Reset,
      --  Writing any other value in this field aborts the write operation of
      --  the WPCMD field.Always reads as 0
      Passwd)
     with Size => 24;
   for PWM_WPCR_WPKEY_Field use
     (Pwm_Wpcr_Wpkey_Field_Reset => 0,
      Passwd => 5265229);

   --  PWM Write Protection Control Register
   type PWM_PWM_WPCR_Register is record
      --  Write-only. Write Protection Command
      WPCMD : PWM_WPCR_WPCMD_Field := SAM_SVD.PWM.Disable_Sw_Prot;
      --  Write-only. Write Protection Register Group 0
      WPRG  : PWM_PWM_WPCR_WPRG_Field := (As_Array => False, Val => 16#0#);
      --  Write-only. Write Protection Key
      WPKEY : PWM_WPCR_WPKEY_Field := Pwm_Wpcr_Wpkey_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_WPCR_Register use record
      WPCMD at 0 range 0 .. 1;
      WPRG  at 0 range 2 .. 7;
      WPKEY at 0 range 8 .. 31;
   end record;

   --  PWM_PWM_WPSR_WPSWS array
   type PWM_PWM_WPSR_WPSWS_Field_Array is array (0 .. 5) of Boolean
     with Component_Size => 1, Size => 6;

   --  Type definition for PWM_PWM_WPSR_WPSWS
   type PWM_PWM_WPSR_WPSWS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WPSWS as a value
            Val : HAL.UInt6;
         when True =>
            --  WPSWS as an array
            Arr : PWM_PWM_WPSR_WPSWS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for PWM_PWM_WPSR_WPSWS_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   --  PWM_PWM_WPSR_WPHWS array
   type PWM_PWM_WPSR_WPHWS_Field_Array is array (0 .. 5) of Boolean
     with Component_Size => 1, Size => 6;

   --  Type definition for PWM_PWM_WPSR_WPHWS
   type PWM_PWM_WPSR_WPHWS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WPHWS as a value
            Val : HAL.UInt6;
         when True =>
            --  WPHWS as an array
            Arr : PWM_PWM_WPSR_WPHWS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for PWM_PWM_WPSR_WPHWS_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   subtype PWM_PWM_WPSR_WPVSRC_Field is HAL.UInt16;

   --  PWM Write Protection Status Register
   type PWM_PWM_WPSR_Register is record
      --  Read-only. Write Protect SW Status
      WPSWS          : PWM_PWM_WPSR_WPSWS_Field;
      --  unspecified
      Reserved_6_6   : HAL.Bit;
      --  Read-only. Write Protect Violation Status
      WPVS           : Boolean;
      --  Read-only. Write Protect HW Status
      WPHWS          : PWM_PWM_WPSR_WPHWS_Field;
      --  unspecified
      Reserved_14_15 : HAL.UInt2;
      --  Read-only. Write Protect Violation Source
      WPVSRC         : PWM_PWM_WPSR_WPVSRC_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_WPSR_Register use record
      WPSWS          at 0 range 0 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      WPVS           at 0 range 7 .. 7;
      WPHWS          at 0 range 8 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      WPVSRC         at 0 range 16 .. 31;
   end record;

   subtype PWM_PWM_VERSION_VERSION_Field is HAL.UInt12;
   subtype PWM_PWM_VERSION_MFN_Field is HAL.UInt3;

   --  Version Register
   type PWM_PWM_VERSION_Register is record
      --  Read-only. Version of the Hardware Module
      VERSION        : PWM_PWM_VERSION_VERSION_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Metal Fix Number
      MFN            : PWM_PWM_VERSION_MFN_Field;
      --  unspecified
      Reserved_19_31 : HAL.UInt13;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_VERSION_Register use record
      VERSION        at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      MFN            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   -------------------------------------
   -- PWM_PWM_CMP cluster's Registers --
   -------------------------------------

   subtype PWM_PWM_CMPV_PWM_PWM_CMP_CV_Field is HAL.UInt24;

   --  PWM Comparison 0 Value Register
   type PWM_PWM_CMPV_PWM_PWM_CMP_Register is record
      --  Comparison x Value
      CV             : PWM_PWM_CMPV_PWM_PWM_CMP_CV_Field := 16#0#;
      --  Comparison x Value Mode
      CVM            : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_CMPV_PWM_PWM_CMP_Register use record
      CV             at 0 range 0 .. 23;
      CVM            at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype PWM_PWM_CMPVUPD_PWM_PWM_CMP_CVUPD_Field is HAL.UInt24;

   --  PWM Comparison 0 Value Update Register
   type PWM_PWM_CMPVUPD_PWM_PWM_CMP_Register is record
      --  Write-only. Comparison x Value Update
      CVUPD          : PWM_PWM_CMPVUPD_PWM_PWM_CMP_CVUPD_Field := 16#0#;
      --  Write-only. Comparison x Value Mode Update
      CVMUPD         : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_CMPVUPD_PWM_PWM_CMP_Register use record
      CVUPD          at 0 range 0 .. 23;
      CVMUPD         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype PWM_PWM_CMPM_PWM_PWM_CMP_CTR_Field is HAL.UInt4;
   subtype PWM_PWM_CMPM_PWM_PWM_CMP_CPR_Field is HAL.UInt4;
   subtype PWM_PWM_CMPM_PWM_PWM_CMP_CPRCNT_Field is HAL.UInt4;
   subtype PWM_PWM_CMPM_PWM_PWM_CMP_CUPR_Field is HAL.UInt4;
   subtype PWM_PWM_CMPM_PWM_PWM_CMP_CUPRCNT_Field is HAL.UInt4;

   --  PWM Comparison 0 Mode Register
   type PWM_PWM_CMPM_PWM_PWM_CMP_Register is record
      --  Comparison x Enable
      CEN            : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Comparison x Trigger
      CTR            : PWM_PWM_CMPM_PWM_PWM_CMP_CTR_Field := 16#0#;
      --  Comparison x Period
      CPR            : PWM_PWM_CMPM_PWM_PWM_CMP_CPR_Field := 16#0#;
      --  Comparison x Period Counter
      CPRCNT         : PWM_PWM_CMPM_PWM_PWM_CMP_CPRCNT_Field := 16#0#;
      --  Comparison x Update Period
      CUPR           : PWM_PWM_CMPM_PWM_PWM_CMP_CUPR_Field := 16#0#;
      --  Comparison x Update Period Counter
      CUPRCNT        : PWM_PWM_CMPM_PWM_PWM_CMP_CUPRCNT_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_CMPM_PWM_PWM_CMP_Register use record
      CEN            at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      CTR            at 0 range 4 .. 7;
      CPR            at 0 range 8 .. 11;
      CPRCNT         at 0 range 12 .. 15;
      CUPR           at 0 range 16 .. 19;
      CUPRCNT        at 0 range 20 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype PWM_PWM_CMPMUPD_PWM_PWM_CMP_CTRUPD_Field is HAL.UInt4;
   subtype PWM_PWM_CMPMUPD_PWM_PWM_CMP_CPRUPD_Field is HAL.UInt4;
   subtype PWM_PWM_CMPMUPD_PWM_PWM_CMP_CUPRUPD_Field is HAL.UInt4;

   --  PWM Comparison 0 Mode Update Register
   type PWM_PWM_CMPMUPD_PWM_PWM_CMP_Register is record
      --  Write-only. Comparison x Enable Update
      CENUPD         : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Write-only. Comparison x Trigger Update
      CTRUPD         : PWM_PWM_CMPMUPD_PWM_PWM_CMP_CTRUPD_Field := 16#0#;
      --  Write-only. Comparison x Period Update
      CPRUPD         : PWM_PWM_CMPMUPD_PWM_PWM_CMP_CPRUPD_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Write-only. Comparison x Update Period Update
      CUPRUPD        : PWM_PWM_CMPMUPD_PWM_PWM_CMP_CUPRUPD_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_CMPMUPD_PWM_PWM_CMP_Register use record
      CENUPD         at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      CTRUPD         at 0 range 4 .. 7;
      CPRUPD         at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      CUPRUPD        at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  PWM Comparison 0 Value Register
   type PWM_PWM_CMP_Cluster is record
      --  PWM Comparison 0 Value Register
      PWM_CMPV    : aliased PWM_PWM_CMPV_PWM_PWM_CMP_Register;
      --  PWM Comparison 0 Value Update Register
      PWM_CMPVUPD : aliased PWM_PWM_CMPVUPD_PWM_PWM_CMP_Register;
      --  PWM Comparison 0 Mode Register
      PWM_CMPM    : aliased PWM_PWM_CMPM_PWM_PWM_CMP_Register;
      --  PWM Comparison 0 Mode Update Register
      PWM_CMPMUPD : aliased PWM_PWM_CMPMUPD_PWM_PWM_CMP_Register;
   end record
     with Volatile, Size => 128;

   for PWM_PWM_CMP_Cluster use record
      PWM_CMPV    at 16#0# range 0 .. 31;
      PWM_CMPVUPD at 16#4# range 0 .. 31;
      PWM_CMPM    at 16#8# range 0 .. 31;
      PWM_CMPMUPD at 16#C# range 0 .. 31;
   end record;

   --  PWM Comparison 0 Value Register
   type PWM_PWM_CMP_Clusters is array (0 .. 7) of PWM_PWM_CMP_Cluster;

   ----------------------------------------
   -- PWM_PWM_CH_NUM cluster's Registers --
   ----------------------------------------

   --  Channel Pre-scaler
   type PWM_CMR_CPRE_Field is
     (
      --  Peripheral clock
      Mck,
      --  Peripheral clock/2
      Mck_Div_2,
      --  Peripheral clock/4
      Mck_Div_4,
      --  Peripheral clock/8
      Mck_Div_8,
      --  Peripheral clock/16
      Mck_Div_16,
      --  Peripheral clock/32
      Mck_Div_32,
      --  Peripheral clock/64
      Mck_Div_64,
      --  Peripheral clock/128
      Mck_Div_128,
      --  Peripheral clock/256
      Mck_Div_256,
      --  Peripheral clock/512
      Mck_Div_512,
      --  Peripheral clock/1024
      Mck_Div_1024,
      --  Clock A
      Clka,
      --  Clock B
      Clkb)
     with Size => 4;
   for PWM_CMR_CPRE_Field use
     (Mck => 0,
      Mck_Div_2 => 1,
      Mck_Div_4 => 2,
      Mck_Div_8 => 3,
      Mck_Div_16 => 4,
      Mck_Div_32 => 5,
      Mck_Div_64 => 6,
      Mck_Div_128 => 7,
      Mck_Div_256 => 8,
      Mck_Div_512 => 9,
      Mck_Div_1024 => 10,
      Clka => 11,
      Clkb => 12);

   --  PWM Channel Mode Register (ch_num = 0)
   type PWM_PWM_CMR_PWM_PWM_CH_NUM_Register is record
      --  Channel Pre-scaler
      CPRE           : PWM_CMR_CPRE_Field := SAM_SVD.PWM.Mck;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Channel Alignment
      CALG           : Boolean := False;
      --  Channel Polarity
      CPOL           : Boolean := False;
      --  Counter Event Selection
      CES            : Boolean := False;
      --  Update Selection
      UPDS           : Boolean := False;
      --  Disabled Polarity Inverted
      DPOLI          : Boolean := False;
      --  Timer Counter Trigger Selection
      TCTS           : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Dead-Time Generator Enable
      DTE            : Boolean := False;
      --  Dead-Time PWMHx Output Inverted
      DTHI           : Boolean := False;
      --  Dead-Time PWMLx Output Inverted
      DTLI           : Boolean := False;
      --  Push-Pull Mode
      PPM            : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_CMR_PWM_PWM_CH_NUM_Register use record
      CPRE           at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      CALG           at 0 range 8 .. 8;
      CPOL           at 0 range 9 .. 9;
      CES            at 0 range 10 .. 10;
      UPDS           at 0 range 11 .. 11;
      DPOLI          at 0 range 12 .. 12;
      TCTS           at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      DTE            at 0 range 16 .. 16;
      DTHI           at 0 range 17 .. 17;
      DTLI           at 0 range 18 .. 18;
      PPM            at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype PWM_PWM_CDTY_PWM_PWM_CH_NUM_CDTY_Field is HAL.UInt24;

   --  PWM Channel Duty Cycle Register (ch_num = 0)
   type PWM_PWM_CDTY_PWM_PWM_CH_NUM_Register is record
      --  Channel Duty-Cycle
      CDTY           : PWM_PWM_CDTY_PWM_PWM_CH_NUM_CDTY_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_CDTY_PWM_PWM_CH_NUM_Register use record
      CDTY           at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype PWM_PWM_CDTYUPD_PWM_PWM_CH_NUM_CDTYUPD_Field is HAL.UInt24;

   --  PWM Channel Duty Cycle Update Register (ch_num = 0)
   type PWM_PWM_CDTYUPD_PWM_PWM_CH_NUM_Register is record
      --  Write-only. Channel Duty-Cycle Update
      CDTYUPD        : PWM_PWM_CDTYUPD_PWM_PWM_CH_NUM_CDTYUPD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_CDTYUPD_PWM_PWM_CH_NUM_Register use record
      CDTYUPD        at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype PWM_PWM_CPRD_PWM_PWM_CH_NUM_CPRD_Field is HAL.UInt24;

   --  PWM Channel Period Register (ch_num = 0)
   type PWM_PWM_CPRD_PWM_PWM_CH_NUM_Register is record
      --  Channel Period
      CPRD           : PWM_PWM_CPRD_PWM_PWM_CH_NUM_CPRD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_CPRD_PWM_PWM_CH_NUM_Register use record
      CPRD           at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype PWM_PWM_CPRDUPD_PWM_PWM_CH_NUM_CPRDUPD_Field is HAL.UInt24;

   --  PWM Channel Period Update Register (ch_num = 0)
   type PWM_PWM_CPRDUPD_PWM_PWM_CH_NUM_Register is record
      --  Write-only. Channel Period Update
      CPRDUPD        : PWM_PWM_CPRDUPD_PWM_PWM_CH_NUM_CPRDUPD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_CPRDUPD_PWM_PWM_CH_NUM_Register use record
      CPRDUPD        at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype PWM_PWM_CCNT_PWM_PWM_CH_NUM_CNT_Field is HAL.UInt24;

   --  PWM Channel Counter Register (ch_num = 0)
   type PWM_PWM_CCNT_PWM_PWM_CH_NUM_Register is record
      --  Read-only. Channel Counter Register
      CNT            : PWM_PWM_CCNT_PWM_PWM_CH_NUM_CNT_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_CCNT_PWM_PWM_CH_NUM_Register use record
      CNT            at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype PWM_PWM_DT_PWM_PWM_CH_NUM_DTH_Field is HAL.UInt16;
   subtype PWM_PWM_DT_PWM_PWM_CH_NUM_DTL_Field is HAL.UInt16;

   --  PWM Channel Dead Time Register (ch_num = 0)
   type PWM_PWM_DT_PWM_PWM_CH_NUM_Register is record
      --  Dead-Time Value for PWMHx Output
      DTH : PWM_PWM_DT_PWM_PWM_CH_NUM_DTH_Field := 16#0#;
      --  Dead-Time Value for PWMLx Output
      DTL : PWM_PWM_DT_PWM_PWM_CH_NUM_DTL_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_DT_PWM_PWM_CH_NUM_Register use record
      DTH at 0 range 0 .. 15;
      DTL at 0 range 16 .. 31;
   end record;

   subtype PWM_PWM_DTUPD_PWM_PWM_CH_NUM_DTHUPD_Field is HAL.UInt16;
   subtype PWM_PWM_DTUPD_PWM_PWM_CH_NUM_DTLUPD_Field is HAL.UInt16;

   --  PWM Channel Dead Time Update Register (ch_num = 0)
   type PWM_PWM_DTUPD_PWM_PWM_CH_NUM_Register is record
      --  Write-only. Dead-Time Value Update for PWMHx Output
      DTHUPD : PWM_PWM_DTUPD_PWM_PWM_CH_NUM_DTHUPD_Field := 16#0#;
      --  Write-only. Dead-Time Value Update for PWMLx Output
      DTLUPD : PWM_PWM_DTUPD_PWM_PWM_CH_NUM_DTLUPD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_PWM_DTUPD_PWM_PWM_CH_NUM_Register use record
      DTHUPD at 0 range 0 .. 15;
      DTLUPD at 0 range 16 .. 31;
   end record;

   --  PWM Channel Mode Register (ch_num = 0)
   type PWM_PWM_CH_NUM_Cluster is record
      --  PWM Channel Mode Register (ch_num = 0)
      PWM_CMR     : aliased PWM_PWM_CMR_PWM_PWM_CH_NUM_Register;
      --  PWM Channel Duty Cycle Register (ch_num = 0)
      PWM_CDTY    : aliased PWM_PWM_CDTY_PWM_PWM_CH_NUM_Register;
      --  PWM Channel Duty Cycle Update Register (ch_num = 0)
      PWM_CDTYUPD : aliased PWM_PWM_CDTYUPD_PWM_PWM_CH_NUM_Register;
      --  PWM Channel Period Register (ch_num = 0)
      PWM_CPRD    : aliased PWM_PWM_CPRD_PWM_PWM_CH_NUM_Register;
      --  PWM Channel Period Update Register (ch_num = 0)
      PWM_CPRDUPD : aliased PWM_PWM_CPRDUPD_PWM_PWM_CH_NUM_Register;
      --  PWM Channel Counter Register (ch_num = 0)
      PWM_CCNT    : aliased PWM_PWM_CCNT_PWM_PWM_CH_NUM_Register;
      --  PWM Channel Dead Time Register (ch_num = 0)
      PWM_DT      : aliased PWM_PWM_DT_PWM_PWM_CH_NUM_Register;
      --  PWM Channel Dead Time Update Register (ch_num = 0)
      PWM_DTUPD   : aliased PWM_PWM_DTUPD_PWM_PWM_CH_NUM_Register;
   end record
     with Volatile, Size => 256;

   for PWM_PWM_CH_NUM_Cluster use record
      PWM_CMR     at 16#0# range 0 .. 31;
      PWM_CDTY    at 16#4# range 0 .. 31;
      PWM_CDTYUPD at 16#8# range 0 .. 31;
      PWM_CPRD    at 16#C# range 0 .. 31;
      PWM_CPRDUPD at 16#10# range 0 .. 31;
      PWM_CCNT    at 16#14# range 0 .. 31;
      PWM_DT      at 16#18# range 0 .. 31;
      PWM_DTUPD   at 16#1C# range 0 .. 31;
   end record;

   --  PWM Channel Mode Register (ch_num = 0)
   type PWM_PWM_CH_NUM_Clusters is array (0 .. 3) of PWM_PWM_CH_NUM_Cluster;

   --  PWM Channel Mode Update Register (ch_num = 0)
   type PWM_CMUPD_Register is record
      --  unspecified
      Reserved_0_8   : HAL.UInt9 := 16#0#;
      --  Write-only. Channel Polarity Update
      CPOLUP         : Boolean := False;
      --  unspecified
      Reserved_10_12 : HAL.UInt3 := 16#0#;
      --  Write-only. Channel Polarity Inversion Update
      CPOLINVUP      : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_CMUPD_Register use record
      Reserved_0_8   at 0 range 0 .. 8;
      CPOLUP         at 0 range 9 .. 9;
      Reserved_10_12 at 0 range 10 .. 12;
      CPOLINVUP      at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype PWM_ETRG_MAXCNT_Field is HAL.UInt24;

   --  External Trigger Mode
   type PWM_ETRG1_TRGMODE_Field is
     (
      --  External trigger is not enabled.
      Off,
      --  External PWM Reset Mode
      Mode1,
      --  External PWM Start Mode
      Mode2,
      --  Cycle-by-cycle Duty Mode
      Mode3)
     with Size => 2;
   for PWM_ETRG1_TRGMODE_Field use
     (Off => 0,
      Mode1 => 1,
      Mode2 => 2,
      Mode3 => 3);

   --  Edge Selection
   type PWM_ETRG1_TRGEDGE_Field is
     (
      --  TRGMODE = 1: TRGINx event detection on falling edge.TRGMODE = 2, 3:
      --  TRGINx active level is 0
      Falling_Zero,
      --  TRGMODE = 1: TRGINx event detection on rising edge.TRGMODE = 2, 3:
      --  TRGINx active level is 1
      Rising_One)
     with Size => 1;
   for PWM_ETRG1_TRGEDGE_Field use
     (Falling_Zero => 0,
      Rising_One => 1);

   --  PWM External Trigger Register (trg_num = 1)
   type PWM_ETRG_Register is record
      --  Maximum Counter value
      MAXCNT         : PWM_ETRG_MAXCNT_Field := 16#0#;
      --  External Trigger Mode
      TRGMODE        : PWM_ETRG1_TRGMODE_Field := SAM_SVD.PWM.Off;
      --  unspecified
      Reserved_26_27 : HAL.UInt2 := 16#0#;
      --  Edge Selection
      TRGEDGE        : PWM_ETRG1_TRGEDGE_Field := SAM_SVD.PWM.Falling_Zero;
      --  Filtered input
      TRGFILT        : Boolean := False;
      --  Trigger Source
      TRGSRC         : Boolean := False;
      --  Recoverable Fault Enable
      RFEN           : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_ETRG_Register use record
      MAXCNT         at 0 range 0 .. 23;
      TRGMODE        at 0 range 24 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      TRGEDGE        at 0 range 28 .. 28;
      TRGFILT        at 0 range 29 .. 29;
      TRGSRC         at 0 range 30 .. 30;
      RFEN           at 0 range 31 .. 31;
   end record;

   subtype PWM_LEBR_LEBDELAY_Field is HAL.UInt7;

   --  PWM Leading-Edge Blanking Register (trg_num = 1)
   type PWM_LEBR_Register is record
      --  Leading-Edge Blanking Delay for TRGINx
      LEBDELAY       : PWM_LEBR_LEBDELAY_Field := 16#0#;
      --  unspecified
      Reserved_7_15  : HAL.UInt9 := 16#0#;
      --  PWML Falling Edge Enable
      PWMLFEN        : Boolean := False;
      --  PWML Rising Edge Enable
      PWMLREN        : Boolean := False;
      --  PWMH Falling Edge Enable
      PWMHFEN        : Boolean := False;
      --  PWMH Rising Edge Enable
      PWMHREN        : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWM_LEBR_Register use record
      LEBDELAY       at 0 range 0 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      PWMLFEN        at 0 range 16 .. 16;
      PWMLREN        at 0 range 17 .. 17;
      PWMHFEN        at 0 range 18 .. 18;
      PWMHREN        at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   -------------------------------------
   -- PWM_PWM_CMP cluster's Registers --
   -------------------------------------

   ----------------------------------------
   -- PWM_PWM_CH_NUM cluster's Registers --
   ----------------------------------------

   -----------------
   -- Peripherals --
   -----------------

   --  Pulse Width Modulation Controller
   type PWM_Peripheral is record
      --  PWM Clock Register
      PWM_CLK        : aliased PWM_PWM_CLK_Register;
      --  PWM Enable Register
      PWM_ENA        : aliased PWM_PWM_ENA_Register;
      --  PWM Disable Register
      PWM_DIS        : aliased PWM_PWM_DIS_Register;
      --  PWM Status Register
      PWM_SR         : aliased PWM_PWM_SR_Register;
      --  PWM Interrupt Enable Register 1
      PWM_IER1       : aliased PWM_PWM_IER1_Register;
      --  PWM Interrupt Disable Register 1
      PWM_IDR1       : aliased PWM_PWM_IDR1_Register;
      --  PWM Interrupt Mask Register 1
      PWM_IMR1       : aliased PWM_PWM_IMR1_Register;
      --  PWM Interrupt Status Register 1
      PWM_ISR1       : aliased PWM_PWM_ISR1_Register;
      --  PWM Sync Channels Mode Register
      PWM_SCM        : aliased PWM_PWM_SCM_Register;
      --  PWM DMA Register
      PWM_DMAR       : aliased PWM_PWM_DMAR_Register;
      --  PWM Sync Channels Update Control Register
      PWM_SCUC       : aliased PWM_PWM_SCUC_Register;
      --  PWM Sync Channels Update Period Register
      PWM_SCUP       : aliased PWM_PWM_SCUP_Register;
      --  PWM Sync Channels Update Period Update Register
      PWM_SCUPUPD    : aliased PWM_PWM_SCUPUPD_Register;
      --  PWM Interrupt Enable Register 2
      PWM_IER2       : aliased PWM_PWM_IER2_Register;
      --  PWM Interrupt Disable Register 2
      PWM_IDR2       : aliased PWM_PWM_IDR2_Register;
      --  PWM Interrupt Mask Register 2
      PWM_IMR2       : aliased PWM_PWM_IMR2_Register;
      --  PWM Interrupt Status Register 2
      PWM_ISR2       : aliased PWM_PWM_ISR2_Register;
      --  PWM Output Override Value Register
      PWM_OOV        : aliased PWM_PWM_OOV_Register;
      --  PWM Output Selection Register
      PWM_OS         : aliased PWM_PWM_OS_Register;
      --  PWM Output Selection Set Register
      PWM_OSS        : aliased PWM_PWM_OSS_Register;
      --  PWM Output Selection Clear Register
      PWM_OSC        : aliased PWM_PWM_OSC_Register;
      --  PWM Output Selection Set Update Register
      PWM_OSSUPD     : aliased PWM_PWM_OSSUPD_Register;
      --  PWM Output Selection Clear Update Register
      PWM_OSCUPD     : aliased PWM_PWM_OSCUPD_Register;
      --  PWM Fault Mode Register
      PWM_FMR        : aliased PWM_PWM_FMR_Register;
      --  PWM Fault Status Register
      PWM_FSR        : aliased PWM_PWM_FSR_Register;
      --  PWM Fault Clear Register
      PWM_FCR        : aliased PWM_PWM_FCR_Register;
      --  PWM Fault Protection Value Register 1
      PWM_FPV1       : aliased PWM_PWM_FPV1_Register;
      --  PWM Fault Protection Enable Register
      PWM_FPE        : aliased PWM_PWM_FPE_Register;
      --  PWM Event Line 0 Mode Register 0
      PWM_ELMR       : aliased PWM_PWM_ELMR_Registers;
      --  PWM Spread Spectrum Register
      PWM_SSPR       : aliased PWM_PWM_SSPR_Register;
      --  PWM Spread Spectrum Update Register
      PWM_SSPUP      : aliased PWM_PWM_SSPUP_Register;
      --  PWM Stepper Motor Mode Register
      PWM_SMMR       : aliased PWM_PWM_SMMR_Register;
      --  PWM Fault Protection Value 2 Register
      PWM_FPV2       : aliased PWM_PWM_FPV2_Register;
      --  PWM Write Protection Control Register
      PWM_WPCR       : aliased PWM_PWM_WPCR_Register;
      --  PWM Write Protection Status Register
      PWM_WPSR       : aliased PWM_PWM_WPSR_Register;
      --  Version Register
      PWM_VERSION    : aliased PWM_PWM_VERSION_Register;
      --  PWM Comparison 0 Value Register
      PWM_PWM_CMP    : aliased PWM_PWM_CMP_Clusters;
      --  PWM Channel Mode Register (ch_num = 0)
      PWM_PWM_CH_NUM : aliased PWM_PWM_CH_NUM_Clusters;
      --  PWM Channel Mode Update Register (ch_num = 0)
      PWM_CMUPD0     : aliased PWM_CMUPD_Register;
      --  PWM Channel Mode Update Register (ch_num = 1)
      PWM_CMUPD1     : aliased PWM_CMUPD_Register;
      --  PWM External Trigger Register (trg_num = 1)
      PWM_ETRG1      : aliased PWM_ETRG_Register;
      --  PWM Leading-Edge Blanking Register (trg_num = 1)
      PWM_LEBR1      : aliased PWM_LEBR_Register;
      --  PWM Channel Mode Update Register (ch_num = 2)
      PWM_CMUPD2     : aliased PWM_CMUPD_Register;
      --  PWM External Trigger Register (trg_num = 2)
      PWM_ETRG2      : aliased PWM_ETRG_Register;
      --  PWM Leading-Edge Blanking Register (trg_num = 2)
      PWM_LEBR2      : aliased PWM_LEBR_Register;
      --  PWM Channel Mode Update Register (ch_num = 3)
      PWM_CMUPD3     : aliased PWM_CMUPD_Register;
   end record
     with Volatile;

   for PWM_Peripheral use record
      PWM_CLK        at 16#0# range 0 .. 31;
      PWM_ENA        at 16#4# range 0 .. 31;
      PWM_DIS        at 16#8# range 0 .. 31;
      PWM_SR         at 16#C# range 0 .. 31;
      PWM_IER1       at 16#10# range 0 .. 31;
      PWM_IDR1       at 16#14# range 0 .. 31;
      PWM_IMR1       at 16#18# range 0 .. 31;
      PWM_ISR1       at 16#1C# range 0 .. 31;
      PWM_SCM        at 16#20# range 0 .. 31;
      PWM_DMAR       at 16#24# range 0 .. 31;
      PWM_SCUC       at 16#28# range 0 .. 31;
      PWM_SCUP       at 16#2C# range 0 .. 31;
      PWM_SCUPUPD    at 16#30# range 0 .. 31;
      PWM_IER2       at 16#34# range 0 .. 31;
      PWM_IDR2       at 16#38# range 0 .. 31;
      PWM_IMR2       at 16#3C# range 0 .. 31;
      PWM_ISR2       at 16#40# range 0 .. 31;
      PWM_OOV        at 16#44# range 0 .. 31;
      PWM_OS         at 16#48# range 0 .. 31;
      PWM_OSS        at 16#4C# range 0 .. 31;
      PWM_OSC        at 16#50# range 0 .. 31;
      PWM_OSSUPD     at 16#54# range 0 .. 31;
      PWM_OSCUPD     at 16#58# range 0 .. 31;
      PWM_FMR        at 16#5C# range 0 .. 31;
      PWM_FSR        at 16#60# range 0 .. 31;
      PWM_FCR        at 16#64# range 0 .. 31;
      PWM_FPV1       at 16#68# range 0 .. 31;
      PWM_FPE        at 16#6C# range 0 .. 31;
      PWM_ELMR       at 16#7C# range 0 .. 63;
      PWM_SSPR       at 16#A0# range 0 .. 31;
      PWM_SSPUP      at 16#A4# range 0 .. 31;
      PWM_SMMR       at 16#B0# range 0 .. 31;
      PWM_FPV2       at 16#C0# range 0 .. 31;
      PWM_WPCR       at 16#E4# range 0 .. 31;
      PWM_WPSR       at 16#E8# range 0 .. 31;
      PWM_VERSION    at 16#FC# range 0 .. 31;
      PWM_PWM_CMP    at 16#130# range 0 .. 1023;
      PWM_PWM_CH_NUM at 16#200# range 0 .. 1023;
      PWM_CMUPD0     at 16#400# range 0 .. 31;
      PWM_CMUPD1     at 16#420# range 0 .. 31;
      PWM_ETRG1      at 16#42C# range 0 .. 31;
      PWM_LEBR1      at 16#430# range 0 .. 31;
      PWM_CMUPD2     at 16#440# range 0 .. 31;
      PWM_ETRG2      at 16#44C# range 0 .. 31;
      PWM_LEBR2      at 16#450# range 0 .. 31;
      PWM_CMUPD3     at 16#460# range 0 .. 31;
   end record;

   --  Pulse Width Modulation Controller
   PWM0_Periph : aliased PWM_Peripheral
     with Import, Address => System'To_Address (16#40020000#);

   --  Pulse Width Modulation Controller
   PWM1_Periph : aliased PWM_Peripheral
     with Import, Address => System'To_Address (16#4005C000#);

end SAM_SVD.PWM;
