--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.PMC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  PMC_PMC_SCER_PCK array
   type PMC_PMC_SCER_PCK_Field_Array is array (0 .. 6) of Boolean
     with Component_Size => 1, Size => 7;

   --  Type definition for PMC_PMC_SCER_PCK
   type PMC_PMC_SCER_PCK_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCK as a value
            Val : HAL.UInt7;
         when True =>
            --  PCK as an array
            Arr : PMC_PMC_SCER_PCK_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 7;

   for PMC_PMC_SCER_PCK_Field use record
      Val at 0 range 0 .. 6;
      Arr at 0 range 0 .. 6;
   end record;

   --  System Clock Enable Register
   type PMC_PMC_SCER_Register is record
      --  unspecified
      Reserved_0_4   : HAL.UInt5 := 16#0#;
      --  Write-only. Enable USB FS Clock
      USBCLK         : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Write-only. Programmable Clock 0 Output Enable
      PCK            : PMC_PMC_SCER_PCK_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_SCER_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      USBCLK         at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      PCK            at 0 range 8 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  PMC_PMC_SCDR_PCK array
   type PMC_PMC_SCDR_PCK_Field_Array is array (0 .. 6) of Boolean
     with Component_Size => 1, Size => 7;

   --  Type definition for PMC_PMC_SCDR_PCK
   type PMC_PMC_SCDR_PCK_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCK as a value
            Val : HAL.UInt7;
         when True =>
            --  PCK as an array
            Arr : PMC_PMC_SCDR_PCK_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 7;

   for PMC_PMC_SCDR_PCK_Field use record
      Val at 0 range 0 .. 6;
      Arr at 0 range 0 .. 6;
   end record;

   --  System Clock Disable Register
   type PMC_PMC_SCDR_Register is record
      --  unspecified
      Reserved_0_4   : HAL.UInt5 := 16#0#;
      --  Write-only. Disable USB FS Clock
      USBCLK         : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Write-only. Programmable Clock 0 Output Disable
      PCK            : PMC_PMC_SCDR_PCK_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_SCDR_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      USBCLK         at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      PCK            at 0 range 8 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  PMC_PMC_SCSR_PCK array
   type PMC_PMC_SCSR_PCK_Field_Array is array (0 .. 6) of Boolean
     with Component_Size => 1, Size => 7;

   --  Type definition for PMC_PMC_SCSR_PCK
   type PMC_PMC_SCSR_PCK_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCK as a value
            Val : HAL.UInt7;
         when True =>
            --  PCK as an array
            Arr : PMC_PMC_SCSR_PCK_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 7;

   for PMC_PMC_SCSR_PCK_Field use record
      Val at 0 range 0 .. 6;
      Arr at 0 range 0 .. 6;
   end record;

   --  System Clock Status Register
   type PMC_PMC_SCSR_Register is record
      --  Read-only. HCLK Status
      HCLKS          : Boolean;
      --  unspecified
      Reserved_1_4   : HAL.UInt4;
      --  Read-only. USB FS Clock Status
      USBCLK         : Boolean;
      --  unspecified
      Reserved_6_7   : HAL.UInt2;
      --  Read-only. Programmable Clock 0 Output Status
      PCK            : PMC_PMC_SCSR_PCK_Field;
      --  unspecified
      Reserved_15_31 : HAL.UInt17;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_SCSR_Register use record
      HCLKS          at 0 range 0 .. 0;
      Reserved_1_4   at 0 range 1 .. 4;
      USBCLK         at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      PCK            at 0 range 8 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  PMC_PMC_PCER0_PID array
   type PMC_PMC_PCER0_PID_Field_Array is array (7 .. 31) of Boolean
     with Component_Size => 1, Size => 25;

   --  Type definition for PMC_PMC_PCER0_PID
   type PMC_PMC_PCER0_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt25;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_PCER0_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 25;

   for PMC_PMC_PCER0_PID_Field use record
      Val at 0 range 0 .. 24;
      Arr at 0 range 0 .. 24;
   end record;

   --  Peripheral Clock Enable Register 0
   type PMC_PMC_PCER0_Register is record
      --  unspecified
      Reserved_0_6 : HAL.UInt7 := 16#0#;
      --  Write-only. Peripheral Clock 7 Enable
      PID          : PMC_PMC_PCER0_PID_Field :=
                      (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_PCER0_Register use record
      Reserved_0_6 at 0 range 0 .. 6;
      PID          at 0 range 7 .. 31;
   end record;

   --  PMC_PMC_PCDR0_PID array
   type PMC_PMC_PCDR0_PID_Field_Array is array (7 .. 31) of Boolean
     with Component_Size => 1, Size => 25;

   --  Type definition for PMC_PMC_PCDR0_PID
   type PMC_PMC_PCDR0_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt25;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_PCDR0_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 25;

   for PMC_PMC_PCDR0_PID_Field use record
      Val at 0 range 0 .. 24;
      Arr at 0 range 0 .. 24;
   end record;

   --  Peripheral Clock Disable Register 0
   type PMC_PMC_PCDR0_Register is record
      --  unspecified
      Reserved_0_6 : HAL.UInt7 := 16#0#;
      --  Write-only. Peripheral Clock 7 Disable
      PID          : PMC_PMC_PCDR0_PID_Field :=
                      (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_PCDR0_Register use record
      Reserved_0_6 at 0 range 0 .. 6;
      PID          at 0 range 7 .. 31;
   end record;

   --  PMC_PMC_PCSR0_PID array
   type PMC_PMC_PCSR0_PID_Field_Array is array (7 .. 31) of Boolean
     with Component_Size => 1, Size => 25;

   --  Type definition for PMC_PMC_PCSR0_PID
   type PMC_PMC_PCSR0_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt25;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_PCSR0_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 25;

   for PMC_PMC_PCSR0_PID_Field use record
      Val at 0 range 0 .. 24;
      Arr at 0 range 0 .. 24;
   end record;

   --  Peripheral Clock Status Register 0
   type PMC_PMC_PCSR0_Register is record
      --  unspecified
      Reserved_0_6 : HAL.UInt7;
      --  Read-only. Peripheral Clock 7 Status
      PID          : PMC_PMC_PCSR0_PID_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_PCSR0_Register use record
      Reserved_0_6 at 0 range 0 .. 6;
      PID          at 0 range 7 .. 31;
   end record;

   subtype PMC_CKGR_UCKR_UPLLCOUNT_Field is HAL.UInt4;

   --  UTMI Clock Register
   type PMC_CKGR_UCKR_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  UTMI PLL Enable
      UPLLEN         : Boolean := False;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  UTMI PLL Startup Time
      UPLLCOUNT      : PMC_CKGR_UCKR_UPLLCOUNT_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_CKGR_UCKR_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      UPLLEN         at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      UPLLCOUNT      at 0 range 20 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Main RC Oscillator Frequency Selection
   type CKGR_MOR_MOSCRCF_Field is
     (
      --  The RC oscillator frequency is at 4 MHz
      Val_4_Mhz,
      --  The RC oscillator frequency is at 8 MHz
      Val_8_Mhz,
      --  The RC oscillator frequency is at 12 MHz
      Val_12_Mhz)
     with Size => 3;
   for CKGR_MOR_MOSCRCF_Field use
     (Val_4_Mhz => 0,
      Val_8_Mhz => 1,
      Val_12_Mhz => 2);

   subtype PMC_CKGR_MOR_MOSCXTST_Field is HAL.UInt8;

   --  Write Access Password
   type CKGR_MOR_KEY_Field is
     (
      --  Reset value for the field
      Ckgr_Mor_Key_Field_Reset,
      --  Writing any other value in this field aborts the write
      --  operation.Always reads as 0.
      Passwd)
     with Size => 8;
   for CKGR_MOR_KEY_Field use
     (Ckgr_Mor_Key_Field_Reset => 0,
      Passwd => 55);

   --  Main Oscillator Register
   type PMC_CKGR_MOR_Register is record
      --  Main Crystal Oscillator Enable
      MOSCXTEN       : Boolean := False;
      --  Main Crystal Oscillator Bypass
      MOSCXTBY       : Boolean := False;
      --  Wait Mode Command (Write-only)
      WAITMODE       : Boolean := False;
      --  Main RC Oscillator Enable
      MOSCRCEN       : Boolean := False;
      --  Main RC Oscillator Frequency Selection
      MOSCRCF        : CKGR_MOR_MOSCRCF_Field := SAM_SVD.PMC.Val_4_Mhz;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Main Crystal Oscillator Startup Time
      MOSCXTST       : PMC_CKGR_MOR_MOSCXTST_Field := 16#0#;
      --  Write Access Password
      KEY            : CKGR_MOR_KEY_Field := Ckgr_Mor_Key_Field_Reset;
      --  Main Clock Oscillator Selection
      MOSCSEL        : Boolean := False;
      --  Clock Failure Detector Enable
      CFDEN          : Boolean := False;
      --  32.768 kHz Crystal Oscillator Frequency Monitoring Enable
      XT32KFME       : Boolean := False;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_CKGR_MOR_Register use record
      MOSCXTEN       at 0 range 0 .. 0;
      MOSCXTBY       at 0 range 1 .. 1;
      WAITMODE       at 0 range 2 .. 2;
      MOSCRCEN       at 0 range 3 .. 3;
      MOSCRCF        at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MOSCXTST       at 0 range 8 .. 15;
      KEY            at 0 range 16 .. 23;
      MOSCSEL        at 0 range 24 .. 24;
      CFDEN          at 0 range 25 .. 25;
      XT32KFME       at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype PMC_CKGR_MCFR_MAINF_Field is HAL.UInt16;

   --  Main Clock Frequency Register
   type PMC_CKGR_MCFR_Register is record
      --  Main Clock Frequency
      MAINF          : PMC_CKGR_MCFR_MAINF_Field := 16#0#;
      --  Main Clock Frequency Measure Ready
      MAINFRDY       : Boolean := False;
      --  unspecified
      Reserved_17_19 : HAL.UInt3 := 16#0#;
      --  RC Oscillator Frequency Measure (write-only)
      RCMEAS         : Boolean := False;
      --  unspecified
      Reserved_21_23 : HAL.UInt3 := 16#0#;
      --  Counter Clock Source Selection
      CCSS           : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_CKGR_MCFR_Register use record
      MAINF          at 0 range 0 .. 15;
      MAINFRDY       at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      RCMEAS         at 0 range 20 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      CCSS           at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype PMC_CKGR_PLLAR_DIVA_Field is HAL.UInt8;
   subtype PMC_CKGR_PLLAR_PLLACOUNT_Field is HAL.UInt6;
   subtype PMC_CKGR_PLLAR_MULA_Field is HAL.UInt11;

   --  PLLA Register
   type PMC_CKGR_PLLAR_Register is record
      --  PLLA Front End Divider
      DIVA           : PMC_CKGR_PLLAR_DIVA_Field := 16#0#;
      --  PLLA Counter
      PLLACOUNT      : PMC_CKGR_PLLAR_PLLACOUNT_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  PLLA Multiplier
      MULA           : PMC_CKGR_PLLAR_MULA_Field := 16#0#;
      --  unspecified
      Reserved_27_28 : HAL.UInt2 := 16#0#;
      --  Must Be Set to 1
      ONE            : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_CKGR_PLLAR_Register use record
      DIVA           at 0 range 0 .. 7;
      PLLACOUNT      at 0 range 8 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      MULA           at 0 range 16 .. 26;
      Reserved_27_28 at 0 range 27 .. 28;
      ONE            at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  Master Clock Source Selection
   type PMC_MCKR_CSS_Field is
     (
      --  SLCK is selected
      Slow_Clk,
      --  MAINCK is selected
      Main_Clk,
      --  PLLACK is selected
      Plla_Clk,
      --  UPPLLCKDIV is selected
      Upll_Clk)
     with Size => 2;
   for PMC_MCKR_CSS_Field use
     (Slow_Clk => 0,
      Main_Clk => 1,
      Plla_Clk => 2,
      Upll_Clk => 3);

   --  Processor Clock Prescaler
   type PMC_MCKR_PRES_Field is
     (
      --  Selected clock
      Clk_1,
      --  Selected clock divided by 2
      Clk_2,
      --  Selected clock divided by 4
      Clk_4,
      --  Selected clock divided by 8
      Clk_8,
      --  Selected clock divided by 16
      Clk_16,
      --  Selected clock divided by 32
      Clk_32,
      --  Selected clock divided by 64
      Clk_64,
      --  Selected clock divided by 3
      Clk_3)
     with Size => 3;
   for PMC_MCKR_PRES_Field use
     (Clk_1 => 0,
      Clk_2 => 1,
      Clk_4 => 2,
      Clk_8 => 3,
      Clk_16 => 4,
      Clk_32 => 5,
      Clk_64 => 6,
      Clk_3 => 7);

   --  Master Clock Division
   type PMC_MCKR_MDIV_Field is
     (
      --  MCK is FCLK divided by 1.
      Eq_Pck,
      --  MCK is FCLK divided by 2.
      Pck_Div2,
      --  MCK is FCLK divided by 4.
      Pck_Div4,
      --  MCK is FCLK divided by 3.
      Pck_Div3)
     with Size => 2;
   for PMC_MCKR_MDIV_Field use
     (Eq_Pck => 0,
      Pck_Div2 => 1,
      Pck_Div4 => 2,
      Pck_Div3 => 3);

   --  Master Clock Register
   type PMC_PMC_MCKR_Register is record
      --  Master Clock Source Selection
      CSS            : PMC_MCKR_CSS_Field := SAM_SVD.PMC.Slow_Clk;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Processor Clock Prescaler
      PRES           : PMC_MCKR_PRES_Field := SAM_SVD.PMC.Clk_1;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Master Clock Division
      MDIV           : PMC_MCKR_MDIV_Field := SAM_SVD.PMC.Eq_Pck;
      --  unspecified
      Reserved_10_12 : HAL.UInt3 := 16#0#;
      --  UPLL Divider by 2
      UPLLDIV2       : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_MCKR_Register use record
      CSS            at 0 range 0 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      PRES           at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MDIV           at 0 range 8 .. 9;
      Reserved_10_12 at 0 range 10 .. 12;
      UPLLDIV2       at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype PMC_PMC_USB_USBDIV_Field is HAL.UInt4;

   --  USB Clock Register
   type PMC_PMC_USB_Register is record
      --  USB Input Clock Selection
      USBS           : Boolean := False;
      --  unspecified
      Reserved_1_7   : HAL.UInt7 := 16#0#;
      --  Divider for USB_48M
      USBDIV         : PMC_PMC_USB_USBDIV_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_USB_Register use record
      USBS           at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      USBDIV         at 0 range 8 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Programmable Clock Source Selection
   type PMC_PCK_CSS_Field is
     (
      --  SLCK is selected
      Slow_Clk,
      --  MAINCK is selected
      Main_Clk,
      --  PLLACK is selected
      Plla_Clk,
      --  UPLLCKDIV is selected
      Upll_Clk,
      --  MCK is selected
      Mck)
     with Size => 3;
   for PMC_PCK_CSS_Field use
     (Slow_Clk => 0,
      Main_Clk => 1,
      Plla_Clk => 2,
      Upll_Clk => 3,
      Mck => 4);

   subtype PMC_PMC_PCK_PRES_Field is HAL.UInt8;

   --  Programmable Clock Register (chid = 0) 0
   type PMC_PMC_PCK_Register is record
      --  Programmable Clock Source Selection
      CSS            : PMC_PCK_CSS_Field := SAM_SVD.PMC.Slow_Clk;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Programmable Clock Prescaler
      PRES           : PMC_PMC_PCK_PRES_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_PCK_Register use record
      CSS            at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      PRES           at 0 range 4 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Programmable Clock Register (chid = 0) 0
   type PMC_PMC_PCK_Registers is array (0 .. 7) of PMC_PMC_PCK_Register
     with Volatile;

   --  PMC_PMC_IER_PCKRDY array
   type PMC_PMC_IER_PCKRDY_Field_Array is array (0 .. 6) of Boolean
     with Component_Size => 1, Size => 7;

   --  Type definition for PMC_PMC_IER_PCKRDY
   type PMC_PMC_IER_PCKRDY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCKRDY as a value
            Val : HAL.UInt7;
         when True =>
            --  PCKRDY as an array
            Arr : PMC_PMC_IER_PCKRDY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 7;

   for PMC_PMC_IER_PCKRDY_Field use record
      Val at 0 range 0 .. 6;
      Arr at 0 range 0 .. 6;
   end record;

   --  Interrupt Enable Register
   type PMC_PMC_IER_Register is record
      --  Write-only. Main Crystal Oscillator Status Interrupt Enable
      MOSCXTS        : Boolean := False;
      --  Write-only. PLLA Lock Interrupt Enable
      LOCKA          : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Write-only. Master Clock Ready Interrupt Enable
      MCKRDY         : Boolean := False;
      --  unspecified
      Reserved_4_5   : HAL.UInt2 := 16#0#;
      --  Write-only. UTMI PLL Lock Interrupt Enable
      LOCKU          : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Write-only. Programmable Clock Ready 0 Interrupt Enable
      PCKRDY         : PMC_PMC_IER_PCKRDY_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Write-only. Main Clock Source Oscillator Selection Status Interrupt
      --  Enable
      MOSCSELS       : Boolean := False;
      --  Write-only. Main RC Oscillator Status Interrupt Enable
      MOSCRCS        : Boolean := False;
      --  Write-only. Clock Failure Detector Event Interrupt Enable
      CFDEV          : Boolean := False;
      --  unspecified
      Reserved_19_20 : HAL.UInt2 := 16#0#;
      --  Write-only. 32.768 kHz Crystal Oscillator Error Interrupt Enable
      XT32KERR       : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_IER_Register use record
      MOSCXTS        at 0 range 0 .. 0;
      LOCKA          at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      MCKRDY         at 0 range 3 .. 3;
      Reserved_4_5   at 0 range 4 .. 5;
      LOCKU          at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      PCKRDY         at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      MOSCSELS       at 0 range 16 .. 16;
      MOSCRCS        at 0 range 17 .. 17;
      CFDEV          at 0 range 18 .. 18;
      Reserved_19_20 at 0 range 19 .. 20;
      XT32KERR       at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  PMC_PMC_IDR_PCKRDY array
   type PMC_PMC_IDR_PCKRDY_Field_Array is array (0 .. 6) of Boolean
     with Component_Size => 1, Size => 7;

   --  Type definition for PMC_PMC_IDR_PCKRDY
   type PMC_PMC_IDR_PCKRDY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCKRDY as a value
            Val : HAL.UInt7;
         when True =>
            --  PCKRDY as an array
            Arr : PMC_PMC_IDR_PCKRDY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 7;

   for PMC_PMC_IDR_PCKRDY_Field use record
      Val at 0 range 0 .. 6;
      Arr at 0 range 0 .. 6;
   end record;

   --  Interrupt Disable Register
   type PMC_PMC_IDR_Register is record
      --  Write-only. Main Crystal Oscillator Status Interrupt Disable
      MOSCXTS        : Boolean := False;
      --  Write-only. PLLA Lock Interrupt Disable
      LOCKA          : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Write-only. Master Clock Ready Interrupt Disable
      MCKRDY         : Boolean := False;
      --  unspecified
      Reserved_4_5   : HAL.UInt2 := 16#0#;
      --  Write-only. UTMI PLL Lock Interrupt Disable
      LOCKU          : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Write-only. Programmable Clock Ready 0 Interrupt Disable
      PCKRDY         : PMC_PMC_IDR_PCKRDY_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Write-only. Main Clock Source Oscillator Selection Status Interrupt
      --  Disable
      MOSCSELS       : Boolean := False;
      --  Write-only. Main RC Status Interrupt Disable
      MOSCRCS        : Boolean := False;
      --  Write-only. Clock Failure Detector Event Interrupt Disable
      CFDEV          : Boolean := False;
      --  unspecified
      Reserved_19_20 : HAL.UInt2 := 16#0#;
      --  Write-only. 32.768 kHz Crystal Oscillator Error Interrupt Disable
      XT32KERR       : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_IDR_Register use record
      MOSCXTS        at 0 range 0 .. 0;
      LOCKA          at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      MCKRDY         at 0 range 3 .. 3;
      Reserved_4_5   at 0 range 4 .. 5;
      LOCKU          at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      PCKRDY         at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      MOSCSELS       at 0 range 16 .. 16;
      MOSCRCS        at 0 range 17 .. 17;
      CFDEV          at 0 range 18 .. 18;
      Reserved_19_20 at 0 range 19 .. 20;
      XT32KERR       at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  PMC_PMC_SR_PCKRDY array
   type PMC_PMC_SR_PCKRDY_Field_Array is array (0 .. 6) of Boolean
     with Component_Size => 1, Size => 7;

   --  Type definition for PMC_PMC_SR_PCKRDY
   type PMC_PMC_SR_PCKRDY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCKRDY as a value
            Val : HAL.UInt7;
         when True =>
            --  PCKRDY as an array
            Arr : PMC_PMC_SR_PCKRDY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 7;

   for PMC_PMC_SR_PCKRDY_Field use record
      Val at 0 range 0 .. 6;
      Arr at 0 range 0 .. 6;
   end record;

   --  Status Register
   type PMC_PMC_SR_Register is record
      --  Read-only. Main Crystal Oscillator Status
      MOSCXTS        : Boolean;
      --  Read-only. PLLA Lock Status
      LOCKA          : Boolean;
      --  unspecified
      Reserved_2_2   : HAL.Bit;
      --  Read-only. Master Clock Status
      MCKRDY         : Boolean;
      --  unspecified
      Reserved_4_5   : HAL.UInt2;
      --  Read-only. UTMI PLL Lock Status
      LOCKU          : Boolean;
      --  Read-only. Slow Clock Source Oscillator Selection
      OSCSELS        : Boolean;
      --  Read-only. Programmable Clock Ready Status
      PCKRDY         : PMC_PMC_SR_PCKRDY_Field;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. Main Clock Source Oscillator Selection Status
      MOSCSELS       : Boolean;
      --  Read-only. Main RC Oscillator Status
      MOSCRCS        : Boolean;
      --  Read-only. Clock Failure Detector Event
      CFDEV          : Boolean;
      --  Read-only. Clock Failure Detector Status
      CFDS           : Boolean;
      --  Read-only. Clock Failure Detector Fault Output Status
      FOS            : Boolean;
      --  Read-only. Slow Crystal Oscillator Error
      XT32KERR       : Boolean;
      --  unspecified
      Reserved_22_31 : HAL.UInt10;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_SR_Register use record
      MOSCXTS        at 0 range 0 .. 0;
      LOCKA          at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      MCKRDY         at 0 range 3 .. 3;
      Reserved_4_5   at 0 range 4 .. 5;
      LOCKU          at 0 range 6 .. 6;
      OSCSELS        at 0 range 7 .. 7;
      PCKRDY         at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      MOSCSELS       at 0 range 16 .. 16;
      MOSCRCS        at 0 range 17 .. 17;
      CFDEV          at 0 range 18 .. 18;
      CFDS           at 0 range 19 .. 19;
      FOS            at 0 range 20 .. 20;
      XT32KERR       at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  PMC_PMC_IMR_PCKRDY array
   type PMC_PMC_IMR_PCKRDY_Field_Array is array (0 .. 6) of Boolean
     with Component_Size => 1, Size => 7;

   --  Type definition for PMC_PMC_IMR_PCKRDY
   type PMC_PMC_IMR_PCKRDY_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCKRDY as a value
            Val : HAL.UInt7;
         when True =>
            --  PCKRDY as an array
            Arr : PMC_PMC_IMR_PCKRDY_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 7;

   for PMC_PMC_IMR_PCKRDY_Field use record
      Val at 0 range 0 .. 6;
      Arr at 0 range 0 .. 6;
   end record;

   --  Interrupt Mask Register
   type PMC_PMC_IMR_Register is record
      --  Read-only. Main Crystal Oscillator Status Interrupt Mask
      MOSCXTS        : Boolean;
      --  Read-only. PLLA Lock Interrupt Mask
      LOCKA          : Boolean;
      --  unspecified
      Reserved_2_2   : HAL.Bit;
      --  Read-only. Master Clock Ready Interrupt Mask
      MCKRDY         : Boolean;
      --  unspecified
      Reserved_4_5   : HAL.UInt2;
      --  Read-only. UTMI PLL Lock Interrupt Mask
      LOCKU          : Boolean;
      --  unspecified
      Reserved_7_7   : HAL.Bit;
      --  Read-only. Programmable Clock Ready 0 Interrupt Mask
      PCKRDY         : PMC_PMC_IMR_PCKRDY_Field;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. Main Clock Source Oscillator Selection Status Interrupt
      --  Mask
      MOSCSELS       : Boolean;
      --  Read-only. Main RC Status Interrupt Mask
      MOSCRCS        : Boolean;
      --  Read-only. Clock Failure Detector Event Interrupt Mask
      CFDEV          : Boolean;
      --  unspecified
      Reserved_19_20 : HAL.UInt2;
      --  Read-only. 32.768 kHz Crystal Oscillator Error Interrupt Mask
      XT32KERR       : Boolean;
      --  unspecified
      Reserved_22_31 : HAL.UInt10;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_IMR_Register use record
      MOSCXTS        at 0 range 0 .. 0;
      LOCKA          at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      MCKRDY         at 0 range 3 .. 3;
      Reserved_4_5   at 0 range 4 .. 5;
      LOCKU          at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      PCKRDY         at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      MOSCSELS       at 0 range 16 .. 16;
      MOSCRCS        at 0 range 17 .. 17;
      CFDEV          at 0 range 18 .. 18;
      Reserved_19_20 at 0 range 19 .. 20;
      XT32KERR       at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  PMC_PMC_FSMR_FSTT array
   type PMC_PMC_FSMR_FSTT_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PMC_PMC_FSMR_FSTT
   type PMC_PMC_FSMR_FSTT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FSTT as a value
            Val : HAL.UInt16;
         when True =>
            --  FSTT as an array
            Arr : PMC_PMC_FSMR_FSTT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PMC_PMC_FSMR_FSTT_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Flash Low-power Mode
   type PMC_FSMR_FLPM_Field is
     (
      --  Flash is in Standby Mode when system enters Wait Mode
      Flash_Standby,
      --  Flash is in Deep-power-down mode when system enters Wait Mode
      Flash_Deep_Powerdown,
      --  Idle mode
      Flash_Idle)
     with Size => 2;
   for PMC_FSMR_FLPM_Field use
     (Flash_Standby => 0,
      Flash_Deep_Powerdown => 1,
      Flash_Idle => 2);

   --  Fast Startup Mode Register
   type PMC_PMC_FSMR_Register is record
      --  Fast Startup Input Enable 0
      FSTT           : PMC_PMC_FSMR_FSTT_Field :=
                        (As_Array => False, Val => 16#0#);
      --  RTT Alarm Enable
      RTTAL          : Boolean := False;
      --  RTC Alarm Enable
      RTCAL          : Boolean := False;
      --  USB Alarm Enable
      USBAL          : Boolean := False;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  Low-power Mode
      LPM            : Boolean := False;
      --  Flash Low-power Mode
      FLPM           : PMC_FSMR_FLPM_Field := SAM_SVD.PMC.Flash_Standby;
      --  Force Flash Low-power Mode
      FFLPM          : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_FSMR_Register use record
      FSTT           at 0 range 0 .. 15;
      RTTAL          at 0 range 16 .. 16;
      RTCAL          at 0 range 17 .. 17;
      USBAL          at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      LPM            at 0 range 20 .. 20;
      FLPM           at 0 range 21 .. 22;
      FFLPM          at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  PMC_PMC_FSPR_FSTP array
   type PMC_PMC_FSPR_FSTP_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for PMC_PMC_FSPR_FSTP
   type PMC_PMC_FSPR_FSTP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FSTP as a value
            Val : HAL.UInt16;
         when True =>
            --  FSTP as an array
            Arr : PMC_PMC_FSPR_FSTP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for PMC_PMC_FSPR_FSTP_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Fast Startup Polarity Register
   type PMC_PMC_FSPR_Register is record
      --  Fast Startup Input Polarity 0
      FSTP           : PMC_PMC_FSPR_FSTP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_FSPR_Register use record
      FSTP           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Fault Output Clear Register
   type PMC_PMC_FOCR_Register is record
      --  Write-only. Fault Output Clear
      FOCLR         : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_FOCR_Register use record
      FOCLR         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Write Protection Key
   type PMC_WPMR_WPKEY_Field is
     (
      --  Reset value for the field
      Pmc_Wpmr_Wpkey_Field_Reset,
      --  Writing any other value in this field aborts the write operation of
      --  the WPEN bit. Always reads as 0.
      Passwd)
     with Size => 24;
   for PMC_WPMR_WPKEY_Field use
     (Pmc_Wpmr_Wpkey_Field_Reset => 0,
      Passwd => 5262659);

   --  Write Protection Mode Register
   type PMC_PMC_WPMR_Register is record
      --  Write Protection Enable
      WPEN         : Boolean := False;
      --  unspecified
      Reserved_1_7 : HAL.UInt7 := 16#0#;
      --  Write Protection Key
      WPKEY        : PMC_WPMR_WPKEY_Field := Pmc_Wpmr_Wpkey_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_WPMR_Register use record
      WPEN         at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
      WPKEY        at 0 range 8 .. 31;
   end record;

   subtype PMC_PMC_WPSR_WPVSRC_Field is HAL.UInt16;

   --  Write Protection Status Register
   type PMC_PMC_WPSR_Register is record
      --  Read-only. Write Protection Violation Status
      WPVS           : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. Write Protection Violation Source
      WPVSRC         : PMC_PMC_WPSR_WPVSRC_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_WPSR_Register use record
      WPVS           at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      WPVSRC         at 0 range 8 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype PMC_PMC_VERSION_VERSION_Field is HAL.UInt12;
   subtype PMC_PMC_VERSION_MFN_Field is HAL.UInt3;

   --  Version Register
   type PMC_PMC_VERSION_Register is record
      --  Read-only. Version of the Hardware Module
      VERSION        : PMC_PMC_VERSION_VERSION_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Metal Fix Number
      MFN            : PMC_PMC_VERSION_MFN_Field;
      --  unspecified
      Reserved_19_31 : HAL.UInt13;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_VERSION_Register use record
      VERSION        at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      MFN            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  PMC_PMC_PCER1_PID array
   type PMC_PMC_PCER1_PID_Field_Array is array (32 .. 35) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PMC_PMC_PCER1_PID
   type PMC_PMC_PCER1_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt4;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_PCER1_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PMC_PMC_PCER1_PID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PMC_PMC_PCER1_PID array
   type PMC_PMC_PCER1_PID_Field_Array_1 is array (39 .. 53) of Boolean
     with Component_Size => 1, Size => 15;

   --  Type definition for PMC_PMC_PCER1_PID
   type PMC_PMC_PCER1_PID_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt15;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_PCER1_PID_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 15;

   for PMC_PMC_PCER1_PID_Field_1 use record
      Val at 0 range 0 .. 14;
      Arr at 0 range 0 .. 14;
   end record;

   --  PMC_PMC_PCER1_PID array
   type PMC_PMC_PCER1_PID_Field_Array_2 is array (56 .. 60) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for PMC_PMC_PCER1_PID
   type PMC_PMC_PCER1_PID_Field_2
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt5;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_PCER1_PID_Field_Array_2;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for PMC_PMC_PCER1_PID_Field_2 use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  Peripheral Clock Enable Register 1
   type PMC_PMC_PCER1_Register is record
      --  Write-only. Peripheral Clock 32 Enable
      PID            : PMC_PMC_PCER1_PID_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_4   : HAL.Bit := 16#0#;
      --  Write-only. Peripheral Clock 37 Enable
      PID37          : Boolean := False;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  Write-only. Peripheral Clock 39 Enable
      PID_1          : PMC_PMC_PCER1_PID_Field_1 :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Write-only. Peripheral Clock 56 Enable
      PID_2          : PMC_PMC_PCER1_PID_Field_2 :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_PCER1_Register use record
      PID            at 0 range 0 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      PID37          at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      PID_1          at 0 range 7 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      PID_2          at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  PMC_PMC_PCDR1_PID array
   type PMC_PMC_PCDR1_PID_Field_Array is array (32 .. 35) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PMC_PMC_PCDR1_PID
   type PMC_PMC_PCDR1_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt4;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_PCDR1_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PMC_PMC_PCDR1_PID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PMC_PMC_PCDR1_PID array
   type PMC_PMC_PCDR1_PID_Field_Array_1 is array (39 .. 53) of Boolean
     with Component_Size => 1, Size => 15;

   --  Type definition for PMC_PMC_PCDR1_PID
   type PMC_PMC_PCDR1_PID_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt15;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_PCDR1_PID_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 15;

   for PMC_PMC_PCDR1_PID_Field_1 use record
      Val at 0 range 0 .. 14;
      Arr at 0 range 0 .. 14;
   end record;

   --  PMC_PMC_PCDR1_PID array
   type PMC_PMC_PCDR1_PID_Field_Array_2 is array (56 .. 60) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for PMC_PMC_PCDR1_PID
   type PMC_PMC_PCDR1_PID_Field_2
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt5;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_PCDR1_PID_Field_Array_2;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for PMC_PMC_PCDR1_PID_Field_2 use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  Peripheral Clock Disable Register 1
   type PMC_PMC_PCDR1_Register is record
      --  Write-only. Peripheral Clock 32 Disable
      PID            : PMC_PMC_PCDR1_PID_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_4   : HAL.Bit := 16#0#;
      --  Write-only. Peripheral Clock 37 Disable
      PID37          : Boolean := False;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  Write-only. Peripheral Clock 39 Disable
      PID_1          : PMC_PMC_PCDR1_PID_Field_1 :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Write-only. Peripheral Clock 56 Disable
      PID_2          : PMC_PMC_PCDR1_PID_Field_2 :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_PCDR1_Register use record
      PID            at 0 range 0 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      PID37          at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      PID_1          at 0 range 7 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      PID_2          at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  PMC_PMC_PCSR1_PID array
   type PMC_PMC_PCSR1_PID_Field_Array is array (32 .. 35) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PMC_PMC_PCSR1_PID
   type PMC_PMC_PCSR1_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt4;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_PCSR1_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PMC_PMC_PCSR1_PID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PMC_PMC_PCSR1_PID array
   type PMC_PMC_PCSR1_PID_Field_Array_1 is array (39 .. 53) of Boolean
     with Component_Size => 1, Size => 15;

   --  Type definition for PMC_PMC_PCSR1_PID
   type PMC_PMC_PCSR1_PID_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt15;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_PCSR1_PID_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 15;

   for PMC_PMC_PCSR1_PID_Field_1 use record
      Val at 0 range 0 .. 14;
      Arr at 0 range 0 .. 14;
   end record;

   --  PMC_PMC_PCSR1_PID array
   type PMC_PMC_PCSR1_PID_Field_Array_2 is array (56 .. 60) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for PMC_PMC_PCSR1_PID
   type PMC_PMC_PCSR1_PID_Field_2
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt5;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_PCSR1_PID_Field_Array_2;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for PMC_PMC_PCSR1_PID_Field_2 use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  Peripheral Clock Status Register 1
   type PMC_PMC_PCSR1_Register is record
      --  Read-only. Peripheral Clock 32 Status
      PID            : PMC_PMC_PCSR1_PID_Field;
      --  unspecified
      Reserved_4_4   : HAL.Bit;
      --  Read-only. Peripheral Clock 37 Status
      PID37          : Boolean;
      --  unspecified
      Reserved_6_6   : HAL.Bit;
      --  Read-only. Peripheral Clock 39 Status
      PID_1          : PMC_PMC_PCSR1_PID_Field_1;
      --  unspecified
      Reserved_22_23 : HAL.UInt2;
      --  Read-only. Peripheral Clock 56 Status
      PID_2          : PMC_PMC_PCSR1_PID_Field_2;
      --  unspecified
      Reserved_29_31 : HAL.UInt3;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_PCSR1_Register use record
      PID            at 0 range 0 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      PID37          at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      PID_1          at 0 range 7 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      PID_2          at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype PMC_PMC_PCR_PID_Field is HAL.UInt7;

   --  Generic Clock Source Selection
   type PMC_PCR_GCLKCSS_Field is
     (
      --  SLCK is selected
      Slow_Clk,
      --  MAINCK is selected
      Main_Clk,
      --  PLLACK is selected
      Plla_Clk,
      --  UPLLCK is selected
      Upll_Clk,
      --  MCK is selected
      Mck_Clk)
     with Size => 3;
   for PMC_PCR_GCLKCSS_Field use
     (Slow_Clk => 0,
      Main_Clk => 1,
      Plla_Clk => 2,
      Upll_Clk => 3,
      Mck_Clk => 4);

   subtype PMC_PMC_PCR_GCLKDIV_Field is HAL.UInt8;

   --  Peripheral Control Register
   type PMC_PMC_PCR_Register is record
      --  Peripheral ID
      PID            : PMC_PMC_PCR_PID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Generic Clock Source Selection
      GCLKCSS        : PMC_PCR_GCLKCSS_Field := SAM_SVD.PMC.Slow_Clk;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  Command
      CMD            : Boolean := False;
      --  unspecified
      Reserved_13_19 : HAL.UInt7 := 16#0#;
      --  Generic Clock Division Ratio
      GCLKDIV        : PMC_PMC_PCR_GCLKDIV_Field := 16#0#;
      --  Enable
      EN             : Boolean := False;
      --  Generic Clock Enable
      GCLKEN         : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_PCR_Register use record
      PID            at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      GCLKCSS        at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      CMD            at 0 range 12 .. 12;
      Reserved_13_19 at 0 range 13 .. 19;
      GCLKDIV        at 0 range 20 .. 27;
      EN             at 0 range 28 .. 28;
      GCLKEN         at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype PMC_PMC_OCR_CAL4_Field is HAL.UInt7;
   subtype PMC_PMC_OCR_CAL8_Field is HAL.UInt7;
   subtype PMC_PMC_OCR_CAL12_Field is HAL.UInt7;

   --  Oscillator Calibration Register
   type PMC_PMC_OCR_Register is record
      --  Main RC Oscillator Calibration Bits for 4 MHz
      CAL4           : PMC_PMC_OCR_CAL4_Field := 16#0#;
      --  Selection of Main RC Oscillator Calibration Bits for 4 MHz
      SEL4           : Boolean := False;
      --  Main RC Oscillator Calibration Bits for 8 MHz
      CAL8           : PMC_PMC_OCR_CAL8_Field := 16#0#;
      --  Selection of Main RC Oscillator Calibration Bits for 8 MHz
      SEL8           : Boolean := False;
      --  Main RC Oscillator Calibration Bits for 12 MHz
      CAL12          : PMC_PMC_OCR_CAL12_Field := 16#0#;
      --  Selection of Main RC Oscillator Calibration Bits for 12 MHz
      SEL12          : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_OCR_Register use record
      CAL4           at 0 range 0 .. 6;
      SEL4           at 0 range 7 .. 7;
      CAL8           at 0 range 8 .. 14;
      SEL8           at 0 range 15 .. 15;
      CAL12          at 0 range 16 .. 22;
      SEL12          at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  PMC_PMC_SLPWK_ER0_PID array
   type PMC_PMC_SLPWK_ER0_PID_Field_Array is array (7 .. 31) of Boolean
     with Component_Size => 1, Size => 25;

   --  Type definition for PMC_PMC_SLPWK_ER0_PID
   type PMC_PMC_SLPWK_ER0_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt25;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_ER0_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 25;

   for PMC_PMC_SLPWK_ER0_PID_Field use record
      Val at 0 range 0 .. 24;
      Arr at 0 range 0 .. 24;
   end record;

   --  SleepWalking Enable Register 0
   type PMC_PMC_SLPWK_ER0_Register is record
      --  unspecified
      Reserved_0_6 : HAL.UInt7 := 16#0#;
      --  Write-only. Peripheral 7 SleepWalking Enable
      PID          : PMC_PMC_SLPWK_ER0_PID_Field :=
                      (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_SLPWK_ER0_Register use record
      Reserved_0_6 at 0 range 0 .. 6;
      PID          at 0 range 7 .. 31;
   end record;

   --  PMC_PMC_SLPWK_DR0_PID array
   type PMC_PMC_SLPWK_DR0_PID_Field_Array is array (7 .. 31) of Boolean
     with Component_Size => 1, Size => 25;

   --  Type definition for PMC_PMC_SLPWK_DR0_PID
   type PMC_PMC_SLPWK_DR0_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt25;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_DR0_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 25;

   for PMC_PMC_SLPWK_DR0_PID_Field use record
      Val at 0 range 0 .. 24;
      Arr at 0 range 0 .. 24;
   end record;

   --  SleepWalking Disable Register 0
   type PMC_PMC_SLPWK_DR0_Register is record
      --  unspecified
      Reserved_0_6 : HAL.UInt7 := 16#0#;
      --  Write-only. Peripheral 7 SleepWalking Disable
      PID          : PMC_PMC_SLPWK_DR0_PID_Field :=
                      (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_SLPWK_DR0_Register use record
      Reserved_0_6 at 0 range 0 .. 6;
      PID          at 0 range 7 .. 31;
   end record;

   --  PMC_PMC_SLPWK_SR0_PID array
   type PMC_PMC_SLPWK_SR0_PID_Field_Array is array (7 .. 31) of Boolean
     with Component_Size => 1, Size => 25;

   --  Type definition for PMC_PMC_SLPWK_SR0_PID
   type PMC_PMC_SLPWK_SR0_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt25;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_SR0_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 25;

   for PMC_PMC_SLPWK_SR0_PID_Field use record
      Val at 0 range 0 .. 24;
      Arr at 0 range 0 .. 24;
   end record;

   --  SleepWalking Status Register 0
   type PMC_PMC_SLPWK_SR0_Register is record
      --  unspecified
      Reserved_0_6 : HAL.UInt7;
      --  Read-only. Peripheral 7 SleepWalking Status
      PID          : PMC_PMC_SLPWK_SR0_PID_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_SLPWK_SR0_Register use record
      Reserved_0_6 at 0 range 0 .. 6;
      PID          at 0 range 7 .. 31;
   end record;

   --  PMC_PMC_SLPWK_ASR0_PID array
   type PMC_PMC_SLPWK_ASR0_PID_Field_Array is array (7 .. 31) of Boolean
     with Component_Size => 1, Size => 25;

   --  Type definition for PMC_PMC_SLPWK_ASR0_PID
   type PMC_PMC_SLPWK_ASR0_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt25;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_ASR0_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 25;

   for PMC_PMC_SLPWK_ASR0_PID_Field use record
      Val at 0 range 0 .. 24;
      Arr at 0 range 0 .. 24;
   end record;

   --  SleepWalking Activity Status Register 0
   type PMC_PMC_SLPWK_ASR0_Register is record
      --  unspecified
      Reserved_0_6 : HAL.UInt7;
      --  Read-only. Peripheral 7 Activity Status
      PID          : PMC_PMC_SLPWK_ASR0_PID_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_SLPWK_ASR0_Register use record
      Reserved_0_6 at 0 range 0 .. 6;
      PID          at 0 range 7 .. 31;
   end record;

   subtype PMC_PMC_PMMR_PLLA_MMAX_Field is HAL.UInt11;

   --  PLL Maximum Multiplier Value Register
   type PMC_PMC_PMMR_Register is record
      --  PLLA Maximum Allowed Multiplier Value
      PLLA_MMAX      : PMC_PMC_PMMR_PLLA_MMAX_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_PMMR_Register use record
      PLLA_MMAX      at 0 range 0 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  PMC_PMC_SLPWK_ER1_PID array
   type PMC_PMC_SLPWK_ER1_PID_Field_Array is array (32 .. 35) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PMC_PMC_SLPWK_ER1_PID
   type PMC_PMC_SLPWK_ER1_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt4;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_ER1_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PMC_PMC_SLPWK_ER1_PID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PMC_PMC_SLPWK_ER1_PID array
   type PMC_PMC_SLPWK_ER1_PID_Field_Array_1 is array (39 .. 53) of Boolean
     with Component_Size => 1, Size => 15;

   --  Type definition for PMC_PMC_SLPWK_ER1_PID
   type PMC_PMC_SLPWK_ER1_PID_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt15;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_ER1_PID_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 15;

   for PMC_PMC_SLPWK_ER1_PID_Field_1 use record
      Val at 0 range 0 .. 14;
      Arr at 0 range 0 .. 14;
   end record;

   --  PMC_PMC_SLPWK_ER1_PID array
   type PMC_PMC_SLPWK_ER1_PID_Field_Array_2 is array (56 .. 60) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for PMC_PMC_SLPWK_ER1_PID
   type PMC_PMC_SLPWK_ER1_PID_Field_2
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt5;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_ER1_PID_Field_Array_2;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for PMC_PMC_SLPWK_ER1_PID_Field_2 use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  SleepWalking Enable Register 1
   type PMC_PMC_SLPWK_ER1_Register is record
      --  Write-only. Peripheral 32 SleepWalking Enable
      PID            : PMC_PMC_SLPWK_ER1_PID_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_4   : HAL.Bit := 16#0#;
      --  Write-only. Peripheral 37 SleepWalking Enable
      PID37          : Boolean := False;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  Write-only. Peripheral 39 SleepWalking Enable
      PID_1          : PMC_PMC_SLPWK_ER1_PID_Field_1 :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Write-only. Peripheral 56 SleepWalking Enable
      PID_2          : PMC_PMC_SLPWK_ER1_PID_Field_2 :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_SLPWK_ER1_Register use record
      PID            at 0 range 0 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      PID37          at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      PID_1          at 0 range 7 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      PID_2          at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  PMC_PMC_SLPWK_DR1_PID array
   type PMC_PMC_SLPWK_DR1_PID_Field_Array is array (32 .. 35) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PMC_PMC_SLPWK_DR1_PID
   type PMC_PMC_SLPWK_DR1_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt4;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_DR1_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PMC_PMC_SLPWK_DR1_PID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PMC_PMC_SLPWK_DR1_PID array
   type PMC_PMC_SLPWK_DR1_PID_Field_Array_1 is array (39 .. 53) of Boolean
     with Component_Size => 1, Size => 15;

   --  Type definition for PMC_PMC_SLPWK_DR1_PID
   type PMC_PMC_SLPWK_DR1_PID_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt15;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_DR1_PID_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 15;

   for PMC_PMC_SLPWK_DR1_PID_Field_1 use record
      Val at 0 range 0 .. 14;
      Arr at 0 range 0 .. 14;
   end record;

   --  PMC_PMC_SLPWK_DR1_PID array
   type PMC_PMC_SLPWK_DR1_PID_Field_Array_2 is array (56 .. 60) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for PMC_PMC_SLPWK_DR1_PID
   type PMC_PMC_SLPWK_DR1_PID_Field_2
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt5;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_DR1_PID_Field_Array_2;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for PMC_PMC_SLPWK_DR1_PID_Field_2 use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  SleepWalking Disable Register 1
   type PMC_PMC_SLPWK_DR1_Register is record
      --  Write-only. Peripheral 32 SleepWalking Disable
      PID            : PMC_PMC_SLPWK_DR1_PID_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_4   : HAL.Bit := 16#0#;
      --  Write-only. Peripheral 37 SleepWalking Disable
      PID37          : Boolean := False;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  Write-only. Peripheral 39 SleepWalking Disable
      PID_1          : PMC_PMC_SLPWK_DR1_PID_Field_1 :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Write-only. Peripheral 56 SleepWalking Disable
      PID_2          : PMC_PMC_SLPWK_DR1_PID_Field_2 :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_SLPWK_DR1_Register use record
      PID            at 0 range 0 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      PID37          at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      PID_1          at 0 range 7 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      PID_2          at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  PMC_PMC_SLPWK_SR1_PID array
   type PMC_PMC_SLPWK_SR1_PID_Field_Array is array (32 .. 35) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PMC_PMC_SLPWK_SR1_PID
   type PMC_PMC_SLPWK_SR1_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt4;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_SR1_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PMC_PMC_SLPWK_SR1_PID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PMC_PMC_SLPWK_SR1_PID array
   type PMC_PMC_SLPWK_SR1_PID_Field_Array_1 is array (39 .. 53) of Boolean
     with Component_Size => 1, Size => 15;

   --  Type definition for PMC_PMC_SLPWK_SR1_PID
   type PMC_PMC_SLPWK_SR1_PID_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt15;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_SR1_PID_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 15;

   for PMC_PMC_SLPWK_SR1_PID_Field_1 use record
      Val at 0 range 0 .. 14;
      Arr at 0 range 0 .. 14;
   end record;

   --  PMC_PMC_SLPWK_SR1_PID array
   type PMC_PMC_SLPWK_SR1_PID_Field_Array_2 is array (56 .. 60) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for PMC_PMC_SLPWK_SR1_PID
   type PMC_PMC_SLPWK_SR1_PID_Field_2
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt5;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_SR1_PID_Field_Array_2;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for PMC_PMC_SLPWK_SR1_PID_Field_2 use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  SleepWalking Status Register 1
   type PMC_PMC_SLPWK_SR1_Register is record
      --  Read-only. Peripheral 32 SleepWalking Status
      PID            : PMC_PMC_SLPWK_SR1_PID_Field;
      --  unspecified
      Reserved_4_4   : HAL.Bit;
      --  Read-only. Peripheral 37 SleepWalking Status
      PID37          : Boolean;
      --  unspecified
      Reserved_6_6   : HAL.Bit;
      --  Read-only. Peripheral 39 SleepWalking Status
      PID_1          : PMC_PMC_SLPWK_SR1_PID_Field_1;
      --  unspecified
      Reserved_22_23 : HAL.UInt2;
      --  Read-only. Peripheral 56 SleepWalking Status
      PID_2          : PMC_PMC_SLPWK_SR1_PID_Field_2;
      --  unspecified
      Reserved_29_31 : HAL.UInt3;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_SLPWK_SR1_Register use record
      PID            at 0 range 0 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      PID37          at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      PID_1          at 0 range 7 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      PID_2          at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  PMC_PMC_SLPWK_ASR1_PID array
   type PMC_PMC_SLPWK_ASR1_PID_Field_Array is array (32 .. 35) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for PMC_PMC_SLPWK_ASR1_PID
   type PMC_PMC_SLPWK_ASR1_PID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt4;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_ASR1_PID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for PMC_PMC_SLPWK_ASR1_PID_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  PMC_PMC_SLPWK_ASR1_PID array
   type PMC_PMC_SLPWK_ASR1_PID_Field_Array_1 is array (39 .. 53) of Boolean
     with Component_Size => 1, Size => 15;

   --  Type definition for PMC_PMC_SLPWK_ASR1_PID
   type PMC_PMC_SLPWK_ASR1_PID_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt15;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_ASR1_PID_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 15;

   for PMC_PMC_SLPWK_ASR1_PID_Field_1 use record
      Val at 0 range 0 .. 14;
      Arr at 0 range 0 .. 14;
   end record;

   --  PMC_PMC_SLPWK_ASR1_PID array
   type PMC_PMC_SLPWK_ASR1_PID_Field_Array_2 is array (56 .. 60) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for PMC_PMC_SLPWK_ASR1_PID
   type PMC_PMC_SLPWK_ASR1_PID_Field_2
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PID as a value
            Val : HAL.UInt5;
         when True =>
            --  PID as an array
            Arr : PMC_PMC_SLPWK_ASR1_PID_Field_Array_2;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for PMC_PMC_SLPWK_ASR1_PID_Field_2 use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  SleepWalking Activity Status Register 1
   type PMC_PMC_SLPWK_ASR1_Register is record
      --  Read-only. Peripheral 32 Activity Status
      PID            : PMC_PMC_SLPWK_ASR1_PID_Field;
      --  unspecified
      Reserved_4_4   : HAL.Bit;
      --  Read-only. Peripheral 37 Activity Status
      PID37          : Boolean;
      --  unspecified
      Reserved_6_6   : HAL.Bit;
      --  Read-only. Peripheral 39 Activity Status
      PID_1          : PMC_PMC_SLPWK_ASR1_PID_Field_1;
      --  unspecified
      Reserved_22_23 : HAL.UInt2;
      --  Read-only. Peripheral 56 Activity Status
      PID_2          : PMC_PMC_SLPWK_ASR1_PID_Field_2;
      --  unspecified
      Reserved_29_31 : HAL.UInt3;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_SLPWK_ASR1_Register use record
      PID            at 0 range 0 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      PID37          at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      PID_1          at 0 range 7 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      PID_2          at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  SleepWalking Activity In Progress Register
   type PMC_PMC_SLPWK_AIPR_Register is record
      --  Read-only. Activity In Progress
      AIP           : Boolean;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_SLPWK_AIPR_Register use record
      AIP           at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype PMC_PMC_APLLACR_DCOFLTSEL_Field is HAL.UInt4;
   subtype PMC_PMC_APLLACR_FLTSEL_Field is HAL.UInt4;
   subtype PMC_PMC_APLLACR_BIAS_Field is HAL.UInt2;

   --  Audio PLL Analog Configuration Register
   type PMC_PMC_APLLACR_Register is record
      --  DCO Filter Selection
      DCOFLTSEL      : PMC_PMC_APLLACR_DCOFLTSEL_Field := 16#0#;
      --  PLL Filter Selection
      FLTSEL         : PMC_PMC_APLLACR_FLTSEL_Field := 16#0#;
      --  Bias Voltage Selection
      BIAS           : PMC_PMC_APLLACR_BIAS_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_APLLACR_Register use record
      DCOFLTSEL      at 0 range 0 .. 3;
      FLTSEL         at 0 range 4 .. 7;
      BIAS           at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype PMC_PMC_WMST_WMST_Field is HAL.UInt8;

   --  Write Access Password
   type PMC_WMST_KEY_Field is
     (
      --  Reset value for the field
      Pmc_Wmst_Key_Field_Reset,
      --  Writing any other value in this field aborts the write
      --  operation.Always reads as 0.
      Passwd)
     with Size => 8;
   for PMC_WMST_KEY_Field use
     (Pmc_Wmst_Key_Field_Reset => 0,
      Passwd => 90);

   --  Wait Mode Startup Time Register
   type PMC_PMC_WMST_Register is record
      --  Wait Mode Startup Time
      WMST          : PMC_PMC_WMST_WMST_Field := 16#0#;
      --  unspecified
      Reserved_8_23 : HAL.UInt16 := 16#0#;
      --  Write Access Password
      KEY           : PMC_WMST_KEY_Field := Pmc_Wmst_Key_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_PMC_WMST_Register use record
      WMST          at 0 range 0 .. 7;
      Reserved_8_23 at 0 range 8 .. 23;
      KEY           at 0 range 24 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Power Management Controller
   type PMC_Peripheral is record
      --  System Clock Enable Register
      PMC_SCER       : aliased PMC_PMC_SCER_Register;
      --  System Clock Disable Register
      PMC_SCDR       : aliased PMC_PMC_SCDR_Register;
      --  System Clock Status Register
      PMC_SCSR       : aliased PMC_PMC_SCSR_Register;
      --  Peripheral Clock Enable Register 0
      PMC_PCER0      : aliased PMC_PMC_PCER0_Register;
      --  Peripheral Clock Disable Register 0
      PMC_PCDR0      : aliased PMC_PMC_PCDR0_Register;
      --  Peripheral Clock Status Register 0
      PMC_PCSR0      : aliased PMC_PMC_PCSR0_Register;
      --  UTMI Clock Register
      CKGR_UCKR      : aliased PMC_CKGR_UCKR_Register;
      --  Main Oscillator Register
      CKGR_MOR       : aliased PMC_CKGR_MOR_Register;
      --  Main Clock Frequency Register
      CKGR_MCFR      : aliased PMC_CKGR_MCFR_Register;
      --  PLLA Register
      CKGR_PLLAR     : aliased PMC_CKGR_PLLAR_Register;
      --  Master Clock Register
      PMC_MCKR       : aliased PMC_PMC_MCKR_Register;
      --  USB Clock Register
      PMC_USB        : aliased PMC_PMC_USB_Register;
      --  Programmable Clock Register (chid = 0) 0
      PMC_PCK        : aliased PMC_PMC_PCK_Registers;
      --  Interrupt Enable Register
      PMC_IER        : aliased PMC_PMC_IER_Register;
      --  Interrupt Disable Register
      PMC_IDR        : aliased PMC_PMC_IDR_Register;
      --  Status Register
      PMC_SR         : aliased PMC_PMC_SR_Register;
      --  Interrupt Mask Register
      PMC_IMR        : aliased PMC_PMC_IMR_Register;
      --  Fast Startup Mode Register
      PMC_FSMR       : aliased PMC_PMC_FSMR_Register;
      --  Fast Startup Polarity Register
      PMC_FSPR       : aliased PMC_PMC_FSPR_Register;
      --  Fault Output Clear Register
      PMC_FOCR       : aliased PMC_PMC_FOCR_Register;
      --  Write Protection Mode Register
      PMC_WPMR       : aliased PMC_PMC_WPMR_Register;
      --  Write Protection Status Register
      PMC_WPSR       : aliased PMC_PMC_WPSR_Register;
      --  Version Register
      PMC_VERSION    : aliased PMC_PMC_VERSION_Register;
      --  Peripheral Clock Enable Register 1
      PMC_PCER1      : aliased PMC_PMC_PCER1_Register;
      --  Peripheral Clock Disable Register 1
      PMC_PCDR1      : aliased PMC_PMC_PCDR1_Register;
      --  Peripheral Clock Status Register 1
      PMC_PCSR1      : aliased PMC_PMC_PCSR1_Register;
      --  Peripheral Control Register
      PMC_PCR        : aliased PMC_PMC_PCR_Register;
      --  Oscillator Calibration Register
      PMC_OCR        : aliased PMC_PMC_OCR_Register;
      --  SleepWalking Enable Register 0
      PMC_SLPWK_ER0  : aliased PMC_PMC_SLPWK_ER0_Register;
      --  SleepWalking Disable Register 0
      PMC_SLPWK_DR0  : aliased PMC_PMC_SLPWK_DR0_Register;
      --  SleepWalking Status Register 0
      PMC_SLPWK_SR0  : aliased PMC_PMC_SLPWK_SR0_Register;
      --  SleepWalking Activity Status Register 0
      PMC_SLPWK_ASR0 : aliased PMC_PMC_SLPWK_ASR0_Register;
      --  PLL Maximum Multiplier Value Register
      PMC_PMMR       : aliased PMC_PMC_PMMR_Register;
      --  SleepWalking Enable Register 1
      PMC_SLPWK_ER1  : aliased PMC_PMC_SLPWK_ER1_Register;
      --  SleepWalking Disable Register 1
      PMC_SLPWK_DR1  : aliased PMC_PMC_SLPWK_DR1_Register;
      --  SleepWalking Status Register 1
      PMC_SLPWK_SR1  : aliased PMC_PMC_SLPWK_SR1_Register;
      --  SleepWalking Activity Status Register 1
      PMC_SLPWK_ASR1 : aliased PMC_PMC_SLPWK_ASR1_Register;
      --  SleepWalking Activity In Progress Register
      PMC_SLPWK_AIPR : aliased PMC_PMC_SLPWK_AIPR_Register;
      --  Audio PLL Analog Configuration Register
      PMC_APLLACR    : aliased PMC_PMC_APLLACR_Register;
      --  Wait Mode Startup Time Register
      PMC_WMST       : aliased PMC_PMC_WMST_Register;
   end record
     with Volatile;

   for PMC_Peripheral use record
      PMC_SCER       at 16#0# range 0 .. 31;
      PMC_SCDR       at 16#4# range 0 .. 31;
      PMC_SCSR       at 16#8# range 0 .. 31;
      PMC_PCER0      at 16#10# range 0 .. 31;
      PMC_PCDR0      at 16#14# range 0 .. 31;
      PMC_PCSR0      at 16#18# range 0 .. 31;
      CKGR_UCKR      at 16#1C# range 0 .. 31;
      CKGR_MOR       at 16#20# range 0 .. 31;
      CKGR_MCFR      at 16#24# range 0 .. 31;
      CKGR_PLLAR     at 16#28# range 0 .. 31;
      PMC_MCKR       at 16#30# range 0 .. 31;
      PMC_USB        at 16#38# range 0 .. 31;
      PMC_PCK        at 16#40# range 0 .. 255;
      PMC_IER        at 16#60# range 0 .. 31;
      PMC_IDR        at 16#64# range 0 .. 31;
      PMC_SR         at 16#68# range 0 .. 31;
      PMC_IMR        at 16#6C# range 0 .. 31;
      PMC_FSMR       at 16#70# range 0 .. 31;
      PMC_FSPR       at 16#74# range 0 .. 31;
      PMC_FOCR       at 16#78# range 0 .. 31;
      PMC_WPMR       at 16#E4# range 0 .. 31;
      PMC_WPSR       at 16#E8# range 0 .. 31;
      PMC_VERSION    at 16#FC# range 0 .. 31;
      PMC_PCER1      at 16#100# range 0 .. 31;
      PMC_PCDR1      at 16#104# range 0 .. 31;
      PMC_PCSR1      at 16#108# range 0 .. 31;
      PMC_PCR        at 16#10C# range 0 .. 31;
      PMC_OCR        at 16#110# range 0 .. 31;
      PMC_SLPWK_ER0  at 16#114# range 0 .. 31;
      PMC_SLPWK_DR0  at 16#118# range 0 .. 31;
      PMC_SLPWK_SR0  at 16#11C# range 0 .. 31;
      PMC_SLPWK_ASR0 at 16#120# range 0 .. 31;
      PMC_PMMR       at 16#130# range 0 .. 31;
      PMC_SLPWK_ER1  at 16#134# range 0 .. 31;
      PMC_SLPWK_DR1  at 16#138# range 0 .. 31;
      PMC_SLPWK_SR1  at 16#13C# range 0 .. 31;
      PMC_SLPWK_ASR1 at 16#140# range 0 .. 31;
      PMC_SLPWK_AIPR at 16#144# range 0 .. 31;
      PMC_APLLACR    at 16#158# range 0 .. 31;
      PMC_WMST       at 16#15C# range 0 .. 31;
   end record;

   --  Power Management Controller
   PMC_Periph : aliased PMC_Peripheral
     with Import, Address => System'To_Address (16#400E0600#);

end SAM_SVD.PMC;
