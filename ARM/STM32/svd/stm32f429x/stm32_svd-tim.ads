--  This spec has been automatically generated from STM32F429x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with HAL;
with System;

package STM32_SVD.TIM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ------------------
   -- CR1_Register --
   ------------------

   subtype CR1_CMS_Field is HAL.UInt2;
   subtype CR1_CKD_Field is HAL.UInt2;

   --  control register 1
   type CR1_Register is record
      --  Counter enable
      CEN            : Boolean := False;
      --  Update disable
      UDIS           : Boolean := False;
      --  Update request source
      URS            : Boolean := False;
      --  One-pulse mode
      OPM            : Boolean := False;
      --  Direction
      DIR            : Boolean := False;
      --  Center-aligned mode selection
      CMS            : CR1_CMS_Field := 16#0#;
      --  Auto-reload preload enable
      ARPE           : Boolean := False;
      --  Clock division
      CKD            : CR1_CKD_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register use record
      CEN            at 0 range 0 .. 0;
      UDIS           at 0 range 1 .. 1;
      URS            at 0 range 2 .. 2;
      OPM            at 0 range 3 .. 3;
      DIR            at 0 range 4 .. 4;
      CMS            at 0 range 5 .. 6;
      ARPE           at 0 range 7 .. 7;
      CKD            at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   ------------------
   -- CR2_Register --
   ------------------

   subtype CR2_MMS_Field is HAL.UInt3;

   --  control register 2
   type CR2_Register is record
      --  Capture/compare preloaded control
      CCPC           : Boolean := False;
      --  unspecified
      Reserved_1_1   : HAL.Bit := 16#0#;
      --  Capture/compare control update selection
      CCUS           : Boolean := False;
      --  Capture/compare DMA selection
      CCDS           : Boolean := False;
      --  Master mode selection
      MMS            : CR2_MMS_Field := 16#0#;
      --  TI1 selection
      TI1S           : Boolean := False;
      --  Output Idle state 1
      OIS1           : Boolean := False;
      --  Output Idle state 1
      OIS1N          : Boolean := False;
      --  Output Idle state 2
      OIS2           : Boolean := False;
      --  Output Idle state 2
      OIS2N          : Boolean := False;
      --  Output Idle state 3
      OIS3           : Boolean := False;
      --  Output Idle state 3
      OIS3N          : Boolean := False;
      --  Output Idle state 4
      OIS4           : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register use record
      CCPC           at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      CCUS           at 0 range 2 .. 2;
      CCDS           at 0 range 3 .. 3;
      MMS            at 0 range 4 .. 6;
      TI1S           at 0 range 7 .. 7;
      OIS1           at 0 range 8 .. 8;
      OIS1N          at 0 range 9 .. 9;
      OIS2           at 0 range 10 .. 10;
      OIS2N          at 0 range 11 .. 11;
      OIS3           at 0 range 12 .. 12;
      OIS3N          at 0 range 13 .. 13;
      OIS4           at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   -------------------
   -- SMCR_Register --
   -------------------

   subtype SMCR_SMS_Field is HAL.UInt3;
   subtype SMCR_TS_Field is HAL.UInt3;
   subtype SMCR_ETF_Field is HAL.UInt4;
   subtype SMCR_ETPS_Field is HAL.UInt2;

   --  slave mode control register
   type SMCR_Register is record
      --  Slave mode selection
      SMS            : SMCR_SMS_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Trigger selection
      TS             : SMCR_TS_Field := 16#0#;
      --  Master/Slave mode
      MSM            : Boolean := False;
      --  External trigger filter
      ETF            : SMCR_ETF_Field := 16#0#;
      --  External trigger prescaler
      ETPS           : SMCR_ETPS_Field := 16#0#;
      --  External clock enable
      ECE            : Boolean := False;
      --  External trigger polarity
      ETP            : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMCR_Register use record
      SMS            at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      TS             at 0 range 4 .. 6;
      MSM            at 0 range 7 .. 7;
      ETF            at 0 range 8 .. 11;
      ETPS           at 0 range 12 .. 13;
      ECE            at 0 range 14 .. 14;
      ETP            at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -------------------
   -- DIER_Register --
   -------------------

   --  DMA/Interrupt enable register
   type DIER_Register is record
      --  Update interrupt enable
      UIE            : Boolean := False;
      --  Capture/Compare 1 interrupt enable
      CC1IE          : Boolean := False;
      --  Capture/Compare 2 interrupt enable
      CC2IE          : Boolean := False;
      --  Capture/Compare 3 interrupt enable
      CC3IE          : Boolean := False;
      --  Capture/Compare 4 interrupt enable
      CC4IE          : Boolean := False;
      --  COM interrupt enable
      COMIE          : Boolean := False;
      --  Trigger interrupt enable
      TIE            : Boolean := False;
      --  Break interrupt enable
      BIE            : Boolean := False;
      --  Update DMA request enable
      UDE            : Boolean := False;
      --  Capture/Compare 1 DMA request enable
      CC1DE          : Boolean := False;
      --  Capture/Compare 2 DMA request enable
      CC2DE          : Boolean := False;
      --  Capture/Compare 3 DMA request enable
      CC3DE          : Boolean := False;
      --  Capture/Compare 4 DMA request enable
      CC4DE          : Boolean := False;
      --  COM DMA request enable
      COMDE          : Boolean := False;
      --  Trigger DMA request enable
      TDE            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIER_Register use record
      UIE            at 0 range 0 .. 0;
      CC1IE          at 0 range 1 .. 1;
      CC2IE          at 0 range 2 .. 2;
      CC3IE          at 0 range 3 .. 3;
      CC4IE          at 0 range 4 .. 4;
      COMIE          at 0 range 5 .. 5;
      TIE            at 0 range 6 .. 6;
      BIE            at 0 range 7 .. 7;
      UDE            at 0 range 8 .. 8;
      CC1DE          at 0 range 9 .. 9;
      CC2DE          at 0 range 10 .. 10;
      CC3DE          at 0 range 11 .. 11;
      CC4DE          at 0 range 12 .. 12;
      COMDE          at 0 range 13 .. 13;
      TDE            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   -----------------
   -- SR_Register --
   -----------------

   --  status register
   type SR_Register is record
      --  Update interrupt flag
      UIF            : Boolean := False;
      --  Capture/compare 1 interrupt flag
      CC1IF          : Boolean := False;
      --  Capture/Compare 2 interrupt flag
      CC2IF          : Boolean := False;
      --  Capture/Compare 3 interrupt flag
      CC3IF          : Boolean := False;
      --  Capture/Compare 4 interrupt flag
      CC4IF          : Boolean := False;
      --  COM interrupt flag
      COMIF          : Boolean := False;
      --  Trigger interrupt flag
      TIF            : Boolean := False;
      --  Break interrupt flag
      BIF            : Boolean := False;
      --  unspecified
      Reserved_8_8   : HAL.Bit := 16#0#;
      --  Capture/Compare 1 overcapture flag
      CC1OF          : Boolean := False;
      --  Capture/compare 2 overcapture flag
      CC2OF          : Boolean := False;
      --  Capture/Compare 3 overcapture flag
      CC3OF          : Boolean := False;
      --  Capture/Compare 4 overcapture flag
      CC4OF          : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      UIF            at 0 range 0 .. 0;
      CC1IF          at 0 range 1 .. 1;
      CC2IF          at 0 range 2 .. 2;
      CC3IF          at 0 range 3 .. 3;
      CC4IF          at 0 range 4 .. 4;
      COMIF          at 0 range 5 .. 5;
      TIF            at 0 range 6 .. 6;
      BIF            at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      CC1OF          at 0 range 9 .. 9;
      CC2OF          at 0 range 10 .. 10;
      CC3OF          at 0 range 11 .. 11;
      CC4OF          at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   ------------------
   -- EGR_Register --
   ------------------

   --  event generation register
   type EGR_Register is record
      --  Write-only. Update generation
      UG            : Boolean := False;
      --  Write-only. Capture/compare 1 generation
      CC1G          : Boolean := False;
      --  Write-only. Capture/compare 2 generation
      CC2G          : Boolean := False;
      --  Write-only. Capture/compare 3 generation
      CC3G          : Boolean := False;
      --  Write-only. Capture/compare 4 generation
      CC4G          : Boolean := False;
      --  Write-only. Capture/Compare control update generation
      COMG          : Boolean := False;
      --  Write-only. Trigger generation
      TG            : Boolean := False;
      --  Write-only. Break generation
      BG            : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EGR_Register use record
      UG            at 0 range 0 .. 0;
      CC1G          at 0 range 1 .. 1;
      CC2G          at 0 range 2 .. 2;
      CC3G          at 0 range 3 .. 3;
      CC4G          at 0 range 4 .. 4;
      COMG          at 0 range 5 .. 5;
      TG            at 0 range 6 .. 6;
      BG            at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   ---------------------------
   -- CCMR1_Output_Register --
   ---------------------------

   subtype CCMR1_Output_CC1S_Field is HAL.UInt2;
   subtype CCMR1_Output_OC1M_Field is HAL.UInt3;
   subtype CCMR1_Output_CC2S_Field is HAL.UInt2;
   subtype CCMR1_Output_OC2M_Field is HAL.UInt3;

   --  capture/compare mode register 1 (output mode)
   type CCMR1_Output_Register is record
      --  Capture/Compare 1 selection
      CC1S           : CCMR1_Output_CC1S_Field := 16#0#;
      --  Output Compare 1 fast enable
      OC1FE          : Boolean := False;
      --  Output Compare 1 preload enable
      OC1PE          : Boolean := False;
      --  Output Compare 1 mode
      OC1M           : CCMR1_Output_OC1M_Field := 16#0#;
      --  Output Compare 1 clear enable
      OC1CE          : Boolean := False;
      --  Capture/Compare 2 selection
      CC2S           : CCMR1_Output_CC2S_Field := 16#0#;
      --  Output Compare 2 fast enable
      OC2FE          : Boolean := False;
      --  Output Compare 2 preload enable
      OC2PE          : Boolean := False;
      --  Output Compare 2 mode
      OC2M           : CCMR1_Output_OC2M_Field := 16#0#;
      --  Output Compare 2 clear enable
      OC2CE          : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCMR1_Output_Register use record
      CC1S           at 0 range 0 .. 1;
      OC1FE          at 0 range 2 .. 2;
      OC1PE          at 0 range 3 .. 3;
      OC1M           at 0 range 4 .. 6;
      OC1CE          at 0 range 7 .. 7;
      CC2S           at 0 range 8 .. 9;
      OC2FE          at 0 range 10 .. 10;
      OC2PE          at 0 range 11 .. 11;
      OC2M           at 0 range 12 .. 14;
      OC2CE          at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --------------------------
   -- CCMR1_Input_Register --
   --------------------------

   subtype CCMR1_Input_CC1S_Field is HAL.UInt2;
   subtype CCMR1_Input_ICPCS_Field is HAL.UInt2;
   subtype CCMR1_Input_IC1F_Field is HAL.UInt4;
   subtype CCMR1_Input_CC2S_Field is HAL.UInt2;
   subtype CCMR1_Input_IC2PCS_Field is HAL.UInt2;
   subtype CCMR1_Input_IC2F_Field is HAL.UInt4;

   --  capture/compare mode register 1 (input mode)
   type CCMR1_Input_Register is record
      --  Capture/Compare 1 selection
      CC1S           : CCMR1_Input_CC1S_Field := 16#0#;
      --  Input capture 1 prescaler
      ICPCS          : CCMR1_Input_ICPCS_Field := 16#0#;
      --  Input capture 1 filter
      IC1F           : CCMR1_Input_IC1F_Field := 16#0#;
      --  Capture/Compare 2 selection
      CC2S           : CCMR1_Input_CC2S_Field := 16#0#;
      --  Input capture 2 prescaler
      IC2PCS         : CCMR1_Input_IC2PCS_Field := 16#0#;
      --  Input capture 2 filter
      IC2F           : CCMR1_Input_IC2F_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCMR1_Input_Register use record
      CC1S           at 0 range 0 .. 1;
      ICPCS          at 0 range 2 .. 3;
      IC1F           at 0 range 4 .. 7;
      CC2S           at 0 range 8 .. 9;
      IC2PCS         at 0 range 10 .. 11;
      IC2F           at 0 range 12 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ---------------------------
   -- CCMR2_Output_Register --
   ---------------------------

   subtype CCMR2_Output_CC3S_Field is HAL.UInt2;
   subtype CCMR2_Output_OC3M_Field is HAL.UInt3;
   subtype CCMR2_Output_CC4S_Field is HAL.UInt2;
   subtype CCMR2_Output_OC4M_Field is HAL.UInt3;

   --  capture/compare mode register 2 (output mode)
   type CCMR2_Output_Register is record
      --  Capture/Compare 3 selection
      CC3S           : CCMR2_Output_CC3S_Field := 16#0#;
      --  Output compare 3 fast enable
      OC3FE          : Boolean := False;
      --  Output compare 3 preload enable
      OC3PE          : Boolean := False;
      --  Output compare 3 mode
      OC3M           : CCMR2_Output_OC3M_Field := 16#0#;
      --  Output compare 3 clear enable
      OC3CE          : Boolean := False;
      --  Capture/Compare 4 selection
      CC4S           : CCMR2_Output_CC4S_Field := 16#0#;
      --  Output compare 4 fast enable
      OC4FE          : Boolean := False;
      --  Output compare 4 preload enable
      OC4PE          : Boolean := False;
      --  Output compare 4 mode
      OC4M           : CCMR2_Output_OC4M_Field := 16#0#;
      --  Output compare 4 clear enable
      OC4CE          : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCMR2_Output_Register use record
      CC3S           at 0 range 0 .. 1;
      OC3FE          at 0 range 2 .. 2;
      OC3PE          at 0 range 3 .. 3;
      OC3M           at 0 range 4 .. 6;
      OC3CE          at 0 range 7 .. 7;
      CC4S           at 0 range 8 .. 9;
      OC4FE          at 0 range 10 .. 10;
      OC4PE          at 0 range 11 .. 11;
      OC4M           at 0 range 12 .. 14;
      OC4CE          at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --------------------------
   -- CCMR2_Input_Register --
   --------------------------

   subtype CCMR2_Input_CC3S_Field is HAL.UInt2;
   subtype CCMR2_Input_IC3PSC_Field is HAL.UInt2;
   subtype CCMR2_Input_IC3F_Field is HAL.UInt4;
   subtype CCMR2_Input_CC4S_Field is HAL.UInt2;
   subtype CCMR2_Input_IC4PSC_Field is HAL.UInt2;
   subtype CCMR2_Input_IC4F_Field is HAL.UInt4;

   --  capture/compare mode register 2 (input mode)
   type CCMR2_Input_Register is record
      --  Capture/compare 3 selection
      CC3S           : CCMR2_Input_CC3S_Field := 16#0#;
      --  Input capture 3 prescaler
      IC3PSC         : CCMR2_Input_IC3PSC_Field := 16#0#;
      --  Input capture 3 filter
      IC3F           : CCMR2_Input_IC3F_Field := 16#0#;
      --  Capture/Compare 4 selection
      CC4S           : CCMR2_Input_CC4S_Field := 16#0#;
      --  Input capture 4 prescaler
      IC4PSC         : CCMR2_Input_IC4PSC_Field := 16#0#;
      --  Input capture 4 filter
      IC4F           : CCMR2_Input_IC4F_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCMR2_Input_Register use record
      CC3S           at 0 range 0 .. 1;
      IC3PSC         at 0 range 2 .. 3;
      IC3F           at 0 range 4 .. 7;
      CC4S           at 0 range 8 .. 9;
      IC4PSC         at 0 range 10 .. 11;
      IC4F           at 0 range 12 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -------------------
   -- CCER_Register --
   -------------------

   --  capture/compare enable register
   type CCER_Register is record
      --  Capture/Compare 1 output enable
      CC1E           : Boolean := False;
      --  Capture/Compare 1 output Polarity
      CC1P           : Boolean := False;
      --  Capture/Compare 1 complementary output enable
      CC1NE          : Boolean := False;
      --  Capture/Compare 1 output Polarity
      CC1NP          : Boolean := False;
      --  Capture/Compare 2 output enable
      CC2E           : Boolean := False;
      --  Capture/Compare 2 output Polarity
      CC2P           : Boolean := False;
      --  Capture/Compare 2 complementary output enable
      CC2NE          : Boolean := False;
      --  Capture/Compare 2 output Polarity
      CC2NP          : Boolean := False;
      --  Capture/Compare 3 output enable
      CC3E           : Boolean := False;
      --  Capture/Compare 3 output Polarity
      CC3P           : Boolean := False;
      --  Capture/Compare 3 complementary output enable
      CC3NE          : Boolean := False;
      --  Capture/Compare 3 output Polarity
      CC3NP          : Boolean := False;
      --  Capture/Compare 4 output enable
      CC4E           : Boolean := False;
      --  Capture/Compare 3 output Polarity
      CC4P           : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCER_Register use record
      CC1E           at 0 range 0 .. 0;
      CC1P           at 0 range 1 .. 1;
      CC1NE          at 0 range 2 .. 2;
      CC1NP          at 0 range 3 .. 3;
      CC2E           at 0 range 4 .. 4;
      CC2P           at 0 range 5 .. 5;
      CC2NE          at 0 range 6 .. 6;
      CC2NP          at 0 range 7 .. 7;
      CC3E           at 0 range 8 .. 8;
      CC3P           at 0 range 9 .. 9;
      CC3NE          at 0 range 10 .. 10;
      CC3NP          at 0 range 11 .. 11;
      CC4E           at 0 range 12 .. 12;
      CC4P           at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   ------------------
   -- CNT_Register --
   ------------------

   subtype CNT_CNT_Field is HAL.Short;

   --  counter
   type CNT_Register is record
      --  counter value
      CNT            : CNT_CNT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNT_Register use record
      CNT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ------------------
   -- PSC_Register --
   ------------------

   subtype PSC_PSC_Field is HAL.Short;

   --  prescaler
   type PSC_Register is record
      --  Prescaler value
      PSC            : PSC_PSC_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PSC_Register use record
      PSC            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ------------------
   -- ARR_Register --
   ------------------

   subtype ARR_ARR_Field is HAL.Short;

   --  auto-reload register
   type ARR_Register is record
      --  Auto-reload value
      ARR            : ARR_ARR_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ARR_Register use record
      ARR            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ------------------
   -- RCR_Register --
   ------------------

   subtype RCR_REP_Field is HAL.Byte;

   --  repetition counter register
   type RCR_Register is record
      --  Repetition counter value
      REP           : RCR_REP_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RCR_Register use record
      REP           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -------------------
   -- CCR1_Register --
   -------------------

   subtype CCR1_CCR1_Field is HAL.Short;

   --  capture/compare register 1
   type CCR1_Register is record
      --  Capture/Compare 1 value
      CCR1           : CCR1_CCR1_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR1_Register use record
      CCR1           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -------------------
   -- CCR2_Register --
   -------------------

   subtype CCR2_CCR2_Field is HAL.Short;

   --  capture/compare register 2
   type CCR2_Register is record
      --  Capture/Compare 2 value
      CCR2           : CCR2_CCR2_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR2_Register use record
      CCR2           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -------------------
   -- CCR3_Register --
   -------------------

   subtype CCR3_CCR3_Field is HAL.Short;

   --  capture/compare register 3
   type CCR3_Register is record
      --  Capture/Compare value
      CCR3           : CCR3_CCR3_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR3_Register use record
      CCR3           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -------------------
   -- CCR4_Register --
   -------------------

   subtype CCR4_CCR4_Field is HAL.Short;

   --  capture/compare register 4
   type CCR4_Register is record
      --  Capture/Compare value
      CCR4           : CCR4_CCR4_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR4_Register use record
      CCR4           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -------------------
   -- BDTR_Register --
   -------------------

   subtype BDTR_DTG_Field is HAL.Byte;
   subtype BDTR_LOCK_Field is HAL.UInt2;

   --  break and dead-time register
   type BDTR_Register is record
      --  Dead-time generator setup
      DTG            : BDTR_DTG_Field := 16#0#;
      --  Lock configuration
      LOCK           : BDTR_LOCK_Field := 16#0#;
      --  Off-state selection for Idle mode
      OSSI           : Boolean := False;
      --  Off-state selection for Run mode
      OSSR           : Boolean := False;
      --  Break enable
      BKE            : Boolean := False;
      --  Break polarity
      BKP            : Boolean := False;
      --  Automatic output enable
      AOE            : Boolean := False;
      --  Main output enable
      MOE            : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BDTR_Register use record
      DTG            at 0 range 0 .. 7;
      LOCK           at 0 range 8 .. 9;
      OSSI           at 0 range 10 .. 10;
      OSSR           at 0 range 11 .. 11;
      BKE            at 0 range 12 .. 12;
      BKP            at 0 range 13 .. 13;
      AOE            at 0 range 14 .. 14;
      MOE            at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ------------------
   -- DCR_Register --
   ------------------

   subtype DCR_DBA_Field is HAL.UInt5;
   subtype DCR_DBL_Field is HAL.UInt5;

   --  DMA control register
   type DCR_Register is record
      --  DMA base address
      DBA            : DCR_DBA_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  DMA burst length
      DBL            : DCR_DBL_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCR_Register use record
      DBA            at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      DBL            at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   -------------------
   -- DMAR_Register --
   -------------------

   subtype DMAR_DMAB_Field is HAL.Short;

   --  DMA address for full transfer
   type DMAR_Register is record
      --  DMA register for burst accesses
      DMAB           : DMAR_DMAB_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAR_Register use record
      DMAB           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ------------------
   -- CR2_Register --
   ------------------

   --  control register 2
   type CR2_Register_1 is record
      --  unspecified
      Reserved_0_2  : HAL.UInt3 := 16#0#;
      --  Capture/compare DMA selection
      CCDS          : Boolean := False;
      --  Master mode selection
      MMS           : CR2_MMS_Field := 16#0#;
      --  TI1 selection
      TI1S          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register_1 use record
      Reserved_0_2  at 0 range 0 .. 2;
      CCDS          at 0 range 3 .. 3;
      MMS           at 0 range 4 .. 6;
      TI1S          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -------------------
   -- DIER_Register --
   -------------------

   --  DMA/Interrupt enable register
   type DIER_Register_1 is record
      --  Update interrupt enable
      UIE            : Boolean := False;
      --  Capture/Compare 1 interrupt enable
      CC1IE          : Boolean := False;
      --  Capture/Compare 2 interrupt enable
      CC2IE          : Boolean := False;
      --  Capture/Compare 3 interrupt enable
      CC3IE          : Boolean := False;
      --  Capture/Compare 4 interrupt enable
      CC4IE          : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Trigger interrupt enable
      TIE            : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Update DMA request enable
      UDE            : Boolean := False;
      --  Capture/Compare 1 DMA request enable
      CC1DE          : Boolean := False;
      --  Capture/Compare 2 DMA request enable
      CC2DE          : Boolean := False;
      --  Capture/Compare 3 DMA request enable
      CC3DE          : Boolean := False;
      --  Capture/Compare 4 DMA request enable
      CC4DE          : Boolean := False;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Trigger DMA request enable
      TDE            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIER_Register_1 use record
      UIE            at 0 range 0 .. 0;
      CC1IE          at 0 range 1 .. 1;
      CC2IE          at 0 range 2 .. 2;
      CC3IE          at 0 range 3 .. 3;
      CC4IE          at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      TIE            at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      UDE            at 0 range 8 .. 8;
      CC1DE          at 0 range 9 .. 9;
      CC2DE          at 0 range 10 .. 10;
      CC3DE          at 0 range 11 .. 11;
      CC4DE          at 0 range 12 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      TDE            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   -----------------
   -- SR_Register --
   -----------------

   --  status register
   type SR_Register_1 is record
      --  Update interrupt flag
      UIF            : Boolean := False;
      --  Capture/compare 1 interrupt flag
      CC1IF          : Boolean := False;
      --  Capture/Compare 2 interrupt flag
      CC2IF          : Boolean := False;
      --  Capture/Compare 3 interrupt flag
      CC3IF          : Boolean := False;
      --  Capture/Compare 4 interrupt flag
      CC4IF          : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Trigger interrupt flag
      TIF            : Boolean := False;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  Capture/Compare 1 overcapture flag
      CC1OF          : Boolean := False;
      --  Capture/compare 2 overcapture flag
      CC2OF          : Boolean := False;
      --  Capture/Compare 3 overcapture flag
      CC3OF          : Boolean := False;
      --  Capture/Compare 4 overcapture flag
      CC4OF          : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register_1 use record
      UIF            at 0 range 0 .. 0;
      CC1IF          at 0 range 1 .. 1;
      CC2IF          at 0 range 2 .. 2;
      CC3IF          at 0 range 3 .. 3;
      CC4IF          at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      TIF            at 0 range 6 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      CC1OF          at 0 range 9 .. 9;
      CC2OF          at 0 range 10 .. 10;
      CC3OF          at 0 range 11 .. 11;
      CC4OF          at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   ------------------
   -- EGR_Register --
   ------------------

   --  event generation register
   type EGR_Register_1 is record
      --  Write-only. Update generation
      UG            : Boolean := False;
      --  Write-only. Capture/compare 1 generation
      CC1G          : Boolean := False;
      --  Write-only. Capture/compare 2 generation
      CC2G          : Boolean := False;
      --  Write-only. Capture/compare 3 generation
      CC3G          : Boolean := False;
      --  Write-only. Capture/compare 4 generation
      CC4G          : Boolean := False;
      --  unspecified
      Reserved_5_5  : HAL.Bit := 16#0#;
      --  Write-only. Trigger generation
      TG            : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EGR_Register_1 use record
      UG            at 0 range 0 .. 0;
      CC1G          at 0 range 1 .. 1;
      CC2G          at 0 range 2 .. 2;
      CC3G          at 0 range 3 .. 3;
      CC4G          at 0 range 4 .. 4;
      Reserved_5_5  at 0 range 5 .. 5;
      TG            at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   ---------------------------
   -- CCMR2_Output_Register --
   ---------------------------

   --  capture/compare mode register 2 (output mode)
   type CCMR2_Output_Register_1 is record
      --  CC3S
      CC3S           : CCMR2_Output_CC3S_Field := 16#0#;
      --  OC3FE
      OC3FE          : Boolean := False;
      --  OC3PE
      OC3PE          : Boolean := False;
      --  OC3M
      OC3M           : CCMR2_Output_OC3M_Field := 16#0#;
      --  OC3CE
      OC3CE          : Boolean := False;
      --  CC4S
      CC4S           : CCMR2_Output_CC4S_Field := 16#0#;
      --  OC4FE
      OC4FE          : Boolean := False;
      --  OC4PE
      OC4PE          : Boolean := False;
      --  OC4M
      OC4M           : CCMR2_Output_OC4M_Field := 16#0#;
      --  O24CE
      O24CE          : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCMR2_Output_Register_1 use record
      CC3S           at 0 range 0 .. 1;
      OC3FE          at 0 range 2 .. 2;
      OC3PE          at 0 range 3 .. 3;
      OC3M           at 0 range 4 .. 6;
      OC3CE          at 0 range 7 .. 7;
      CC4S           at 0 range 8 .. 9;
      OC4FE          at 0 range 10 .. 10;
      OC4PE          at 0 range 11 .. 11;
      OC4M           at 0 range 12 .. 14;
      O24CE          at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -------------------
   -- CCER_Register --
   -------------------

   --  capture/compare enable register
   type CCER_Register_1 is record
      --  Capture/Compare 1 output enable
      CC1E           : Boolean := False;
      --  Capture/Compare 1 output Polarity
      CC1P           : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Capture/Compare 1 output Polarity
      CC1NP          : Boolean := False;
      --  Capture/Compare 2 output enable
      CC2E           : Boolean := False;
      --  Capture/Compare 2 output Polarity
      CC2P           : Boolean := False;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  Capture/Compare 2 output Polarity
      CC2NP          : Boolean := False;
      --  Capture/Compare 3 output enable
      CC3E           : Boolean := False;
      --  Capture/Compare 3 output Polarity
      CC3P           : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  Capture/Compare 3 output Polarity
      CC3NP          : Boolean := False;
      --  Capture/Compare 4 output enable
      CC4E           : Boolean := False;
      --  Capture/Compare 3 output Polarity
      CC4P           : Boolean := False;
      --  unspecified
      Reserved_14_14 : HAL.Bit := 16#0#;
      --  Capture/Compare 4 output Polarity
      CC4NP          : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCER_Register_1 use record
      CC1E           at 0 range 0 .. 0;
      CC1P           at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      CC1NP          at 0 range 3 .. 3;
      CC2E           at 0 range 4 .. 4;
      CC2P           at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      CC2NP          at 0 range 7 .. 7;
      CC3E           at 0 range 8 .. 8;
      CC3P           at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      CC3NP          at 0 range 11 .. 11;
      CC4E           at 0 range 12 .. 12;
      CC4P           at 0 range 13 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      CC4NP          at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ------------------
   -- CNT_Register --
   ------------------

   subtype CNT_CNT_L_Field is HAL.Short;
   subtype CNT_CNT_H_Field is HAL.Short;

   --  counter
   type CNT_Register_1 is record
      --  Low counter value
      CNT_L : CNT_CNT_L_Field := 16#0#;
      --  High counter value
      CNT_H : CNT_CNT_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNT_Register_1 use record
      CNT_L at 0 range 0 .. 15;
      CNT_H at 0 range 16 .. 31;
   end record;

   ------------------
   -- ARR_Register --
   ------------------

   subtype ARR_ARR_L_Field is HAL.Short;
   subtype ARR_ARR_H_Field is HAL.Short;

   --  auto-reload register
   type ARR_Register_1 is record
      --  Low Auto-reload value
      ARR_L : ARR_ARR_L_Field := 16#0#;
      --  High Auto-reload value
      ARR_H : ARR_ARR_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ARR_Register_1 use record
      ARR_L at 0 range 0 .. 15;
      ARR_H at 0 range 16 .. 31;
   end record;

   -------------------
   -- CCR1_Register --
   -------------------

   subtype CCR1_CCR1_L_Field is HAL.Short;
   subtype CCR1_CCR1_H_Field is HAL.Short;

   --  capture/compare register 1
   type CCR1_Register_1 is record
      --  Low Capture/Compare 1 value
      CCR1_L : CCR1_CCR1_L_Field := 16#0#;
      --  High Capture/Compare 1 value
      CCR1_H : CCR1_CCR1_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR1_Register_1 use record
      CCR1_L at 0 range 0 .. 15;
      CCR1_H at 0 range 16 .. 31;
   end record;

   -------------------
   -- CCR2_Register --
   -------------------

   subtype CCR2_CCR2_L_Field is HAL.Short;
   subtype CCR2_CCR2_H_Field is HAL.Short;

   --  capture/compare register 2
   type CCR2_Register_1 is record
      --  Low Capture/Compare 2 value
      CCR2_L : CCR2_CCR2_L_Field := 16#0#;
      --  High Capture/Compare 2 value
      CCR2_H : CCR2_CCR2_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR2_Register_1 use record
      CCR2_L at 0 range 0 .. 15;
      CCR2_H at 0 range 16 .. 31;
   end record;

   -------------------
   -- CCR3_Register --
   -------------------

   subtype CCR3_CCR3_L_Field is HAL.Short;
   subtype CCR3_CCR3_H_Field is HAL.Short;

   --  capture/compare register 3
   type CCR3_Register_1 is record
      --  Low Capture/Compare value
      CCR3_L : CCR3_CCR3_L_Field := 16#0#;
      --  High Capture/Compare value
      CCR3_H : CCR3_CCR3_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR3_Register_1 use record
      CCR3_L at 0 range 0 .. 15;
      CCR3_H at 0 range 16 .. 31;
   end record;

   -------------------
   -- CCR4_Register --
   -------------------

   subtype CCR4_CCR4_L_Field is HAL.Short;
   subtype CCR4_CCR4_H_Field is HAL.Short;

   --  capture/compare register 4
   type CCR4_Register_1 is record
      --  Low Capture/Compare value
      CCR4_L : CCR4_CCR4_L_Field := 16#0#;
      --  High Capture/Compare value
      CCR4_H : CCR4_CCR4_H_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR4_Register_1 use record
      CCR4_L at 0 range 0 .. 15;
      CCR4_H at 0 range 16 .. 31;
   end record;

   -----------------
   -- OR_Register --
   -----------------

   subtype OR_ITR1_RMP_Field is HAL.UInt2;

   --  TIM5 option register
   type OR_Register is record
      --  unspecified
      Reserved_0_9   : HAL.UInt10 := 16#0#;
      --  Timer Input 4 remap
      ITR1_RMP       : OR_ITR1_RMP_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OR_Register use record
      Reserved_0_9   at 0 range 0 .. 9;
      ITR1_RMP       at 0 range 10 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   -----------------
   -- OR_Register --
   -----------------

   subtype OR_IT4_RMP_Field is HAL.UInt2;

   --  TIM5 option register
   type OR_Register_1 is record
      --  unspecified
      Reserved_0_5  : HAL.UInt6 := 16#0#;
      --  Timer Input 4 remap
      IT4_RMP       : OR_IT4_RMP_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OR_Register_1 use record
      Reserved_0_5  at 0 range 0 .. 5;
      IT4_RMP       at 0 range 6 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   ------------------
   -- CR1_Register --
   ------------------

   --  control register 1
   type CR1_Register_1 is record
      --  Counter enable
      CEN            : Boolean := False;
      --  Update disable
      UDIS           : Boolean := False;
      --  Update request source
      URS            : Boolean := False;
      --  One-pulse mode
      OPM            : Boolean := False;
      --  unspecified
      Reserved_4_6   : HAL.UInt3 := 16#0#;
      --  Auto-reload preload enable
      ARPE           : Boolean := False;
      --  Clock division
      CKD            : CR1_CKD_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register_1 use record
      CEN            at 0 range 0 .. 0;
      UDIS           at 0 range 1 .. 1;
      URS            at 0 range 2 .. 2;
      OPM            at 0 range 3 .. 3;
      Reserved_4_6   at 0 range 4 .. 6;
      ARPE           at 0 range 7 .. 7;
      CKD            at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   ------------------
   -- CR2_Register --
   ------------------

   --  control register 2
   type CR2_Register_2 is record
      --  unspecified
      Reserved_0_3  : HAL.UInt4 := 16#0#;
      --  Master mode selection
      MMS           : CR2_MMS_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register_2 use record
      Reserved_0_3  at 0 range 0 .. 3;
      MMS           at 0 range 4 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   -------------------
   -- SMCR_Register --
   -------------------

   --  slave mode control register
   type SMCR_Register_1 is record
      --  Slave mode selection
      SMS           : SMCR_SMS_Field := 16#0#;
      --  unspecified
      Reserved_3_3  : HAL.Bit := 16#0#;
      --  Trigger selection
      TS            : SMCR_TS_Field := 16#0#;
      --  Master/Slave mode
      MSM           : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMCR_Register_1 use record
      SMS           at 0 range 0 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      TS            at 0 range 4 .. 6;
      MSM           at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -------------------
   -- DIER_Register --
   -------------------

   --  DMA/Interrupt enable register
   type DIER_Register_2 is record
      --  Update interrupt enable
      UIE           : Boolean := False;
      --  Capture/Compare 1 interrupt enable
      CC1IE         : Boolean := False;
      --  Capture/Compare 2 interrupt enable
      CC2IE         : Boolean := False;
      --  unspecified
      Reserved_3_5  : HAL.UInt3 := 16#0#;
      --  Trigger interrupt enable
      TIE           : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIER_Register_2 use record
      UIE           at 0 range 0 .. 0;
      CC1IE         at 0 range 1 .. 1;
      CC2IE         at 0 range 2 .. 2;
      Reserved_3_5  at 0 range 3 .. 5;
      TIE           at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   -----------------
   -- SR_Register --
   -----------------

   --  status register
   type SR_Register_2 is record
      --  Update interrupt flag
      UIF            : Boolean := False;
      --  Capture/compare 1 interrupt flag
      CC1IF          : Boolean := False;
      --  Capture/Compare 2 interrupt flag
      CC2IF          : Boolean := False;
      --  unspecified
      Reserved_3_5   : HAL.UInt3 := 16#0#;
      --  Trigger interrupt flag
      TIF            : Boolean := False;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  Capture/Compare 1 overcapture flag
      CC1OF          : Boolean := False;
      --  Capture/compare 2 overcapture flag
      CC2OF          : Boolean := False;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register_2 use record
      UIF            at 0 range 0 .. 0;
      CC1IF          at 0 range 1 .. 1;
      CC2IF          at 0 range 2 .. 2;
      Reserved_3_5   at 0 range 3 .. 5;
      TIF            at 0 range 6 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      CC1OF          at 0 range 9 .. 9;
      CC2OF          at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   ------------------
   -- EGR_Register --
   ------------------

   --  event generation register
   type EGR_Register_2 is record
      --  Write-only. Update generation
      UG            : Boolean := False;
      --  Write-only. Capture/compare 1 generation
      CC1G          : Boolean := False;
      --  Write-only. Capture/compare 2 generation
      CC2G          : Boolean := False;
      --  unspecified
      Reserved_3_5  : HAL.UInt3 := 16#0#;
      --  Write-only. Trigger generation
      TG            : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EGR_Register_2 use record
      UG            at 0 range 0 .. 0;
      CC1G          at 0 range 1 .. 1;
      CC2G          at 0 range 2 .. 2;
      Reserved_3_5  at 0 range 3 .. 5;
      TG            at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   ---------------------------
   -- CCMR1_Output_Register --
   ---------------------------

   --  capture/compare mode register 1 (output mode)
   type CCMR1_Output_Register_1 is record
      --  Capture/Compare 1 selection
      CC1S           : CCMR1_Output_CC1S_Field := 16#0#;
      --  Output Compare 1 fast enable
      OC1FE          : Boolean := False;
      --  Output Compare 1 preload enable
      OC1PE          : Boolean := False;
      --  Output Compare 1 mode
      OC1M           : CCMR1_Output_OC1M_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Capture/Compare 2 selection
      CC2S           : CCMR1_Output_CC2S_Field := 16#0#;
      --  Output Compare 2 fast enable
      OC2FE          : Boolean := False;
      --  Output Compare 2 preload enable
      OC2PE          : Boolean := False;
      --  Output Compare 2 mode
      OC2M           : CCMR1_Output_OC2M_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCMR1_Output_Register_1 use record
      CC1S           at 0 range 0 .. 1;
      OC1FE          at 0 range 2 .. 2;
      OC1PE          at 0 range 3 .. 3;
      OC1M           at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      CC2S           at 0 range 8 .. 9;
      OC2FE          at 0 range 10 .. 10;
      OC2PE          at 0 range 11 .. 11;
      OC2M           at 0 range 12 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --------------------------
   -- CCMR1_Input_Register --
   --------------------------

   subtype CCMR1_Input_IC1F_Field_1 is HAL.UInt3;
   subtype CCMR1_Input_IC2F_Field_1 is HAL.UInt3;

   --  capture/compare mode register 1 (input mode)
   type CCMR1_Input_Register_1 is record
      --  Capture/Compare 1 selection
      CC1S           : CCMR1_Input_CC1S_Field := 16#0#;
      --  Input capture 1 prescaler
      ICPCS          : CCMR1_Input_ICPCS_Field := 16#0#;
      --  Input capture 1 filter
      IC1F           : CCMR1_Input_IC1F_Field_1 := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Capture/Compare 2 selection
      CC2S           : CCMR1_Input_CC2S_Field := 16#0#;
      --  Input capture 2 prescaler
      IC2PCS         : CCMR1_Input_IC2PCS_Field := 16#0#;
      --  Input capture 2 filter
      IC2F           : CCMR1_Input_IC2F_Field_1 := 16#0#;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCMR1_Input_Register_1 use record
      CC1S           at 0 range 0 .. 1;
      ICPCS          at 0 range 2 .. 3;
      IC1F           at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      CC2S           at 0 range 8 .. 9;
      IC2PCS         at 0 range 10 .. 11;
      IC2F           at 0 range 12 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   -------------------
   -- CCER_Register --
   -------------------

   --  capture/compare enable register
   type CCER_Register_2 is record
      --  Capture/Compare 1 output enable
      CC1E          : Boolean := False;
      --  Capture/Compare 1 output Polarity
      CC1P          : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  Capture/Compare 1 output Polarity
      CC1NP         : Boolean := False;
      --  Capture/Compare 2 output enable
      CC2E          : Boolean := False;
      --  Capture/Compare 2 output Polarity
      CC2P          : Boolean := False;
      --  unspecified
      Reserved_6_6  : HAL.Bit := 16#0#;
      --  Capture/Compare 2 output Polarity
      CC2NP         : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCER_Register_2 use record
      CC1E          at 0 range 0 .. 0;
      CC1P          at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      CC1NP         at 0 range 3 .. 3;
      CC2E          at 0 range 4 .. 4;
      CC2P          at 0 range 5 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      CC2NP         at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   ------------------
   -- CR1_Register --
   ------------------

   --  control register 1
   type CR1_Register_2 is record
      --  Counter enable
      CEN            : Boolean := False;
      --  Update disable
      UDIS           : Boolean := False;
      --  Update request source
      URS            : Boolean := False;
      --  unspecified
      Reserved_3_6   : HAL.UInt4 := 16#0#;
      --  Auto-reload preload enable
      ARPE           : Boolean := False;
      --  Clock division
      CKD            : CR1_CKD_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register_2 use record
      CEN            at 0 range 0 .. 0;
      UDIS           at 0 range 1 .. 1;
      URS            at 0 range 2 .. 2;
      Reserved_3_6   at 0 range 3 .. 6;
      ARPE           at 0 range 7 .. 7;
      CKD            at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   -------------------
   -- DIER_Register --
   -------------------

   --  DMA/Interrupt enable register
   type DIER_Register_3 is record
      --  Update interrupt enable
      UIE           : Boolean := False;
      --  Capture/Compare 1 interrupt enable
      CC1IE         : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIER_Register_3 use record
      UIE           at 0 range 0 .. 0;
      CC1IE         at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   -----------------
   -- SR_Register --
   -----------------

   --  status register
   type SR_Register_3 is record
      --  Update interrupt flag
      UIF            : Boolean := False;
      --  Capture/compare 1 interrupt flag
      CC1IF          : Boolean := False;
      --  unspecified
      Reserved_2_8   : HAL.UInt7 := 16#0#;
      --  Capture/Compare 1 overcapture flag
      CC1OF          : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register_3 use record
      UIF            at 0 range 0 .. 0;
      CC1IF          at 0 range 1 .. 1;
      Reserved_2_8   at 0 range 2 .. 8;
      CC1OF          at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   ------------------
   -- EGR_Register --
   ------------------

   --  event generation register
   type EGR_Register_3 is record
      --  Write-only. Update generation
      UG            : Boolean := False;
      --  Write-only. Capture/compare 1 generation
      CC1G          : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EGR_Register_3 use record
      UG            at 0 range 0 .. 0;
      CC1G          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   ---------------------------
   -- CCMR1_Output_Register --
   ---------------------------

   --  capture/compare mode register 1 (output mode)
   type CCMR1_Output_Register_2 is record
      --  Capture/Compare 1 selection
      CC1S          : CCMR1_Output_CC1S_Field := 16#0#;
      --  Output Compare 1 fast enable
      OC1FE         : Boolean := False;
      --  Output Compare 1 preload enable
      OC1PE         : Boolean := False;
      --  Output Compare 1 mode
      OC1M          : CCMR1_Output_OC1M_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCMR1_Output_Register_2 use record
      CC1S          at 0 range 0 .. 1;
      OC1FE         at 0 range 2 .. 2;
      OC1PE         at 0 range 3 .. 3;
      OC1M          at 0 range 4 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --------------------------
   -- CCMR1_Input_Register --
   --------------------------

   --  capture/compare mode register 1 (input mode)
   type CCMR1_Input_Register_2 is record
      --  Capture/Compare 1 selection
      CC1S          : CCMR1_Input_CC1S_Field := 16#0#;
      --  Input capture 1 prescaler
      ICPCS         : CCMR1_Input_ICPCS_Field := 16#0#;
      --  Input capture 1 filter
      IC1F          : CCMR1_Input_IC1F_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCMR1_Input_Register_2 use record
      CC1S          at 0 range 0 .. 1;
      ICPCS         at 0 range 2 .. 3;
      IC1F          at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -------------------
   -- CCER_Register --
   -------------------

   --  capture/compare enable register
   type CCER_Register_3 is record
      --  Capture/Compare 1 output enable
      CC1E          : Boolean := False;
      --  Capture/Compare 1 output Polarity
      CC1P          : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  Capture/Compare 1 output Polarity
      CC1NP         : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCER_Register_3 use record
      CC1E          at 0 range 0 .. 0;
      CC1P          at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      CC1NP         at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- OR_Register --
   -----------------

   subtype OR_RMP_Field is HAL.UInt2;

   --  option register
   type OR_Register_2 is record
      --  Input 1 remapping capability
      RMP           : OR_RMP_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OR_Register_2 use record
      RMP           at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   ------------------
   -- CR1_Register --
   ------------------

   --  control register 1
   type CR1_Register_3 is record
      --  Counter enable
      CEN           : Boolean := False;
      --  Update disable
      UDIS          : Boolean := False;
      --  Update request source
      URS           : Boolean := False;
      --  One-pulse mode
      OPM           : Boolean := False;
      --  unspecified
      Reserved_4_6  : HAL.UInt3 := 16#0#;
      --  Auto-reload preload enable
      ARPE          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register_3 use record
      CEN           at 0 range 0 .. 0;
      UDIS          at 0 range 1 .. 1;
      URS           at 0 range 2 .. 2;
      OPM           at 0 range 3 .. 3;
      Reserved_4_6  at 0 range 4 .. 6;
      ARPE          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -------------------
   -- DIER_Register --
   -------------------

   --  DMA/Interrupt enable register
   type DIER_Register_4 is record
      --  Update interrupt enable
      UIE           : Boolean := False;
      --  unspecified
      Reserved_1_7  : HAL.UInt7 := 16#0#;
      --  Update DMA request enable
      UDE           : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIER_Register_4 use record
      UIE           at 0 range 0 .. 0;
      Reserved_1_7  at 0 range 1 .. 7;
      UDE           at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   -----------------
   -- SR_Register --
   -----------------

   --  status register
   type SR_Register_4 is record
      --  Update interrupt flag
      UIF           : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register_4 use record
      UIF           at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   ------------------
   -- EGR_Register --
   ------------------

   --  event generation register
   type EGR_Register_4 is record
      --  Write-only. Update generation
      UG            : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EGR_Register_4 use record
      UG            at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   type CCMR1_Discriminent is
     (
      Output,
      Input);

   type CCMR1_Aliased_Register
     (Disc : CCMR1_Discriminent := Output)
   is record
      case Disc is
         when Output =>
            Output : CCMR1_Output_Register;
         when Input =>
            Input : CCMR1_Input_Register;
      end case;
   end record
     with Unchecked_Union;

   for CCMR1_Aliased_Register use record
      Output at 0 range 0 .. 31;
      Input  at 0 range 0 .. 31;
   end record;

   type CCMR2_Discriminent is
     (
      Output,
      Input);

   type CCMR2_Aliased_Register
     (Disc : CCMR2_Discriminent := Output)
   is record
      case Disc is
         when Output =>
            Output : CCMR2_Output_Register;
         when Input =>
            Input : CCMR2_Input_Register;
      end case;
   end record
     with Unchecked_Union;

   for CCMR2_Aliased_Register use record
      Output at 0 range 0 .. 31;
      Input  at 0 range 0 .. 31;
   end record;

   type CCMR2_Aliased_Register_1
     (Disc : CCMR2_Discriminent := Output)
   is record
      case Disc is
         when Output =>
            Output : CCMR2_Output_Register_1;
         when Input =>
            Input : CCMR2_Input_Register;
      end case;
   end record
     with Unchecked_Union;

   for CCMR2_Aliased_Register_1 use record
      Output at 0 range 0 .. 31;
      Input  at 0 range 0 .. 31;
   end record;

   type CCMR1_Aliased_Register_1
     (Disc : CCMR1_Discriminent := Output)
   is record
      case Disc is
         when Output =>
            Output : CCMR1_Output_Register_1;
         when Input =>
            Input : CCMR1_Input_Register_1;
      end case;
   end record
     with Unchecked_Union;

   for CCMR1_Aliased_Register_1 use record
      Output at 0 range 0 .. 31;
      Input  at 0 range 0 .. 31;
   end record;

   type CCMR1_Aliased_Register_2
     (Disc : CCMR1_Discriminent := Output)
   is record
      case Disc is
         when Output =>
            Output : CCMR1_Output_Register_2;
         when Input =>
            Input : CCMR1_Input_Register_2;
      end case;
   end record
     with Unchecked_Union;

   for CCMR1_Aliased_Register_2 use record
      Output at 0 range 0 .. 31;
      Input  at 0 range 0 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  General purpose timers
   type TIM2_Peripheral is record
      --  control register 1
      CR1   : CR1_Register;
      --  control register 2
      CR2   : CR2_Register_1;
      --  slave mode control register
      SMCR  : SMCR_Register;
      --  DMA/Interrupt enable register
      DIER  : DIER_Register_1;
      --  status register
      SR    : SR_Register_1;
      --  event generation register
      EGR   : EGR_Register_1;
      --  capture/compare mode register 1 (output mode)
      CCMR1 : CCMR1_Aliased_Register;
      --  capture/compare mode register 2 (output mode)
      CCMR2 : CCMR2_Aliased_Register_1;
      --  capture/compare enable register
      CCER  : CCER_Register_1;
      --  counter
      CNT   : CNT_Register_1;
      --  prescaler
      PSC   : PSC_Register;
      --  auto-reload register
      ARR   : ARR_Register_1;
      --  capture/compare register 1
      CCR1  : CCR1_Register_1;
      --  capture/compare register 2
      CCR2  : CCR2_Register_1;
      --  capture/compare register 3
      CCR3  : CCR3_Register_1;
      --  capture/compare register 4
      CCR4  : CCR4_Register_1;
      --  DMA control register
      DCR   : DCR_Register;
      --  DMA address for full transfer
      DMAR  : DMAR_Register;
      --  TIM5 option register
      OR_k  : OR_Register;
   end record
     with Volatile;

   for TIM2_Peripheral use record
      CR1   at 0 range 0 .. 31;
      CR2   at 4 range 0 .. 31;
      SMCR  at 8 range 0 .. 31;
      DIER  at 12 range 0 .. 31;
      SR    at 16 range 0 .. 31;
      EGR   at 20 range 0 .. 31;
      CCMR1 at 24 range 0 .. 31;
      CCMR2 at 28 range 0 .. 31;
      CCER  at 32 range 0 .. 31;
      CNT   at 36 range 0 .. 31;
      PSC   at 40 range 0 .. 31;
      ARR   at 44 range 0 .. 31;
      CCR1  at 52 range 0 .. 31;
      CCR2  at 56 range 0 .. 31;
      CCR3  at 60 range 0 .. 31;
      CCR4  at 64 range 0 .. 31;
      DCR   at 72 range 0 .. 31;
      DMAR  at 76 range 0 .. 31;
      OR_k  at 80 range 0 .. 31;
   end record;

   --  General purpose timers
   TIM2_Periph : aliased TIM2_Peripheral
     with Import, Address => TIM2_Base;

   --  General purpose timers
   type TIM3_Peripheral is record
      --  control register 1
      CR1   : CR1_Register;
      --  control register 2
      CR2   : CR2_Register_1;
      --  slave mode control register
      SMCR  : SMCR_Register;
      --  DMA/Interrupt enable register
      DIER  : DIER_Register_1;
      --  status register
      SR    : SR_Register_1;
      --  event generation register
      EGR   : EGR_Register_1;
      --  capture/compare mode register 1 (output mode)
      CCMR1 : CCMR1_Aliased_Register;
      --  capture/compare mode register 2 (output mode)
      CCMR2 : CCMR2_Aliased_Register_1;
      --  capture/compare enable register
      CCER  : CCER_Register_1;
      --  counter
      CNT   : CNT_Register_1;
      --  prescaler
      PSC   : PSC_Register;
      --  auto-reload register
      ARR   : ARR_Register_1;
      --  capture/compare register 1
      CCR1  : CCR1_Register_1;
      --  capture/compare register 2
      CCR2  : CCR2_Register_1;
      --  capture/compare register 3
      CCR3  : CCR3_Register_1;
      --  capture/compare register 4
      CCR4  : CCR4_Register_1;
      --  DMA control register
      DCR   : DCR_Register;
      --  DMA address for full transfer
      DMAR  : DMAR_Register;
   end record
     with Volatile;

   for TIM3_Peripheral use record
      CR1   at 0 range 0 .. 31;
      CR2   at 4 range 0 .. 31;
      SMCR  at 8 range 0 .. 31;
      DIER  at 12 range 0 .. 31;
      SR    at 16 range 0 .. 31;
      EGR   at 20 range 0 .. 31;
      CCMR1 at 24 range 0 .. 31;
      CCMR2 at 28 range 0 .. 31;
      CCER  at 32 range 0 .. 31;
      CNT   at 36 range 0 .. 31;
      PSC   at 40 range 0 .. 31;
      ARR   at 44 range 0 .. 31;
      CCR1  at 52 range 0 .. 31;
      CCR2  at 56 range 0 .. 31;
      CCR3  at 60 range 0 .. 31;
      CCR4  at 64 range 0 .. 31;
      DCR   at 72 range 0 .. 31;
      DMAR  at 76 range 0 .. 31;
   end record;

   --  General purpose timers
   TIM3_Periph : aliased TIM3_Peripheral
     with Import, Address => TIM3_Base;

   --  General purpose timers
   TIM4_Periph : aliased TIM3_Peripheral
     with Import, Address => TIM4_Base;

   --  General-purpose-timers
   type TIM5_Peripheral is record
      --  control register 1
      CR1   : CR1_Register;
      --  control register 2
      CR2   : CR2_Register_1;
      --  slave mode control register
      SMCR  : SMCR_Register;
      --  DMA/Interrupt enable register
      DIER  : DIER_Register_1;
      --  status register
      SR    : SR_Register_1;
      --  event generation register
      EGR   : EGR_Register_1;
      --  capture/compare mode register 1 (output mode)
      CCMR1 : CCMR1_Aliased_Register;
      --  capture/compare mode register 2 (output mode)
      CCMR2 : CCMR2_Aliased_Register_1;
      --  capture/compare enable register
      CCER  : CCER_Register_1;
      --  counter
      CNT   : CNT_Register_1;
      --  prescaler
      PSC   : PSC_Register;
      --  auto-reload register
      ARR   : ARR_Register_1;
      --  capture/compare register 1
      CCR1  : CCR1_Register_1;
      --  capture/compare register 2
      CCR2  : CCR2_Register_1;
      --  capture/compare register 3
      CCR3  : CCR3_Register_1;
      --  capture/compare register 4
      CCR4  : CCR4_Register_1;
      --  DMA control register
      DCR   : DCR_Register;
      --  DMA address for full transfer
      DMAR  : DMAR_Register;
      --  TIM5 option register
      OR_k  : OR_Register_1;
   end record
     with Volatile;

   for TIM5_Peripheral use record
      CR1   at 0 range 0 .. 31;
      CR2   at 4 range 0 .. 31;
      SMCR  at 8 range 0 .. 31;
      DIER  at 12 range 0 .. 31;
      SR    at 16 range 0 .. 31;
      EGR   at 20 range 0 .. 31;
      CCMR1 at 24 range 0 .. 31;
      CCMR2 at 28 range 0 .. 31;
      CCER  at 32 range 0 .. 31;
      CNT   at 36 range 0 .. 31;
      PSC   at 40 range 0 .. 31;
      ARR   at 44 range 0 .. 31;
      CCR1  at 52 range 0 .. 31;
      CCR2  at 56 range 0 .. 31;
      CCR3  at 60 range 0 .. 31;
      CCR4  at 64 range 0 .. 31;
      DCR   at 72 range 0 .. 31;
      DMAR  at 76 range 0 .. 31;
      OR_k  at 80 range 0 .. 31;
   end record;

   --  General-purpose-timers
   TIM5_Periph : aliased TIM5_Peripheral
     with Import, Address => TIM5_Base;

   --  Basic timers
   type TIM6_Peripheral is record
      --  control register 1
      CR1  : CR1_Register_3;
      --  control register 2
      CR2  : CR2_Register_2;
      --  DMA/Interrupt enable register
      DIER : DIER_Register_4;
      --  status register
      SR   : SR_Register_4;
      --  event generation register
      EGR  : EGR_Register_4;
      --  counter
      CNT  : CNT_Register;
      --  prescaler
      PSC  : PSC_Register;
      --  auto-reload register
      ARR  : ARR_Register;
   end record
     with Volatile;

   for TIM6_Peripheral use record
      CR1  at 0 range 0 .. 31;
      CR2  at 4 range 0 .. 31;
      DIER at 12 range 0 .. 31;
      SR   at 16 range 0 .. 31;
      EGR  at 20 range 0 .. 31;
      CNT  at 36 range 0 .. 31;
      PSC  at 40 range 0 .. 31;
      ARR  at 44 range 0 .. 31;
   end record;

   --  Basic timers
   TIM6_Periph : aliased TIM6_Peripheral
     with Import, Address => TIM6_Base;

   --  Basic timers
   TIM7_Periph : aliased TIM6_Peripheral
     with Import, Address => TIM7_Base;

   --  General purpose timers
   type TIM12_Peripheral is record
      --  control register 1
      CR1   : CR1_Register_1;
      --  control register 2
      CR2   : CR2_Register_2;
      --  slave mode control register
      SMCR  : SMCR_Register_1;
      --  DMA/Interrupt enable register
      DIER  : DIER_Register_2;
      --  status register
      SR    : SR_Register_2;
      --  event generation register
      EGR   : EGR_Register_2;
      --  capture/compare mode register 1 (output mode)
      CCMR1 : CCMR1_Aliased_Register_1;
      --  capture/compare enable register
      CCER  : CCER_Register_2;
      --  counter
      CNT   : CNT_Register;
      --  prescaler
      PSC   : PSC_Register;
      --  auto-reload register
      ARR   : ARR_Register;
      --  capture/compare register 1
      CCR1  : CCR1_Register;
      --  capture/compare register 2
      CCR2  : CCR2_Register;
   end record
     with Volatile;

   for TIM12_Peripheral use record
      CR1   at 0 range 0 .. 31;
      CR2   at 4 range 0 .. 31;
      SMCR  at 8 range 0 .. 31;
      DIER  at 12 range 0 .. 31;
      SR    at 16 range 0 .. 31;
      EGR   at 20 range 0 .. 31;
      CCMR1 at 24 range 0 .. 31;
      CCER  at 32 range 0 .. 31;
      CNT   at 36 range 0 .. 31;
      PSC   at 40 range 0 .. 31;
      ARR   at 44 range 0 .. 31;
      CCR1  at 52 range 0 .. 31;
      CCR2  at 56 range 0 .. 31;
   end record;

   --  General purpose timers
   TIM12_Periph : aliased TIM12_Peripheral
     with Import, Address => TIM12_Base;

   --  General purpose timers
   TIM9_Periph : aliased TIM12_Peripheral
     with Import, Address => TIM9_Base;

   --  General-purpose-timers
   type TIM13_Peripheral is record
      --  control register 1
      CR1   : CR1_Register_2;
      --  DMA/Interrupt enable register
      DIER  : DIER_Register_3;
      --  status register
      SR    : SR_Register_3;
      --  event generation register
      EGR   : EGR_Register_3;
      --  capture/compare mode register 1 (output mode)
      CCMR1 : CCMR1_Aliased_Register_2;
      --  capture/compare enable register
      CCER  : CCER_Register_3;
      --  counter
      CNT   : CNT_Register;
      --  prescaler
      PSC   : PSC_Register;
      --  auto-reload register
      ARR   : ARR_Register;
      --  capture/compare register 1
      CCR1  : CCR1_Register;
   end record
     with Volatile;

   for TIM13_Peripheral use record
      CR1   at 0 range 0 .. 31;
      DIER  at 12 range 0 .. 31;
      SR    at 16 range 0 .. 31;
      EGR   at 20 range 0 .. 31;
      CCMR1 at 24 range 0 .. 31;
      CCER  at 32 range 0 .. 31;
      CNT   at 36 range 0 .. 31;
      PSC   at 40 range 0 .. 31;
      ARR   at 44 range 0 .. 31;
      CCR1  at 52 range 0 .. 31;
   end record;

   --  General-purpose-timers
   TIM13_Periph : aliased TIM13_Peripheral
     with Import, Address => TIM13_Base;

   --  General-purpose-timers
   TIM14_Periph : aliased TIM13_Peripheral
     with Import, Address => TIM14_Base;

   --  General-purpose-timers
   TIM10_Periph : aliased TIM13_Peripheral
     with Import, Address => TIM10_Base;

   --  Advanced-timers
   type TIM1_Peripheral is record
      --  control register 1
      CR1   : CR1_Register;
      --  control register 2
      CR2   : CR2_Register;
      --  slave mode control register
      SMCR  : SMCR_Register;
      --  DMA/Interrupt enable register
      DIER  : DIER_Register;
      --  status register
      SR    : SR_Register;
      --  event generation register
      EGR   : EGR_Register;
      --  capture/compare mode register 1 (output mode)
      CCMR1 : CCMR1_Aliased_Register;
      --  capture/compare mode register 2 (output mode)
      CCMR2 : CCMR2_Aliased_Register;
      --  capture/compare enable register
      CCER  : CCER_Register;
      --  counter
      CNT   : CNT_Register;
      --  prescaler
      PSC   : PSC_Register;
      --  auto-reload register
      ARR   : ARR_Register;
      --  repetition counter register
      RCR   : RCR_Register;
      --  capture/compare register 1
      CCR1  : CCR1_Register;
      --  capture/compare register 2
      CCR2  : CCR2_Register;
      --  capture/compare register 3
      CCR3  : CCR3_Register;
      --  capture/compare register 4
      CCR4  : CCR4_Register;
      --  break and dead-time register
      BDTR  : BDTR_Register;
      --  DMA control register
      DCR   : DCR_Register;
      --  DMA address for full transfer
      DMAR  : DMAR_Register;
   end record
     with Volatile;

   for TIM1_Peripheral use record
      CR1   at 0 range 0 .. 31;
      CR2   at 4 range 0 .. 31;
      SMCR  at 8 range 0 .. 31;
      DIER  at 12 range 0 .. 31;
      SR    at 16 range 0 .. 31;
      EGR   at 20 range 0 .. 31;
      CCMR1 at 24 range 0 .. 31;
      CCMR2 at 28 range 0 .. 31;
      CCER  at 32 range 0 .. 31;
      CNT   at 36 range 0 .. 31;
      PSC   at 40 range 0 .. 31;
      ARR   at 44 range 0 .. 31;
      RCR   at 48 range 0 .. 31;
      CCR1  at 52 range 0 .. 31;
      CCR2  at 56 range 0 .. 31;
      CCR3  at 60 range 0 .. 31;
      CCR4  at 64 range 0 .. 31;
      BDTR  at 68 range 0 .. 31;
      DCR   at 72 range 0 .. 31;
      DMAR  at 76 range 0 .. 31;
   end record;

   --  Advanced-timers
   TIM1_Periph : aliased TIM1_Peripheral
     with Import, Address => TIM1_Base;

   --  Advanced-timers
   TIM8_Periph : aliased TIM1_Peripheral
     with Import, Address => TIM8_Base;

   --  General-purpose-timers
   type TIM11_Peripheral is record
      --  control register 1
      CR1   : CR1_Register_2;
      --  DMA/Interrupt enable register
      DIER  : DIER_Register_3;
      --  status register
      SR    : SR_Register_3;
      --  event generation register
      EGR   : EGR_Register_3;
      --  capture/compare mode register 1 (output mode)
      CCMR1 : CCMR1_Aliased_Register_2;
      --  capture/compare enable register
      CCER  : CCER_Register_3;
      --  counter
      CNT   : CNT_Register;
      --  prescaler
      PSC   : PSC_Register;
      --  auto-reload register
      ARR   : ARR_Register;
      --  capture/compare register 1
      CCR1  : CCR1_Register;
      --  option register
      OR_k  : OR_Register_2;
   end record
     with Volatile;

   for TIM11_Peripheral use record
      CR1   at 0 range 0 .. 31;
      DIER  at 12 range 0 .. 31;
      SR    at 16 range 0 .. 31;
      EGR   at 20 range 0 .. 31;
      CCMR1 at 24 range 0 .. 31;
      CCER  at 32 range 0 .. 31;
      CNT   at 36 range 0 .. 31;
      PSC   at 40 range 0 .. 31;
      ARR   at 44 range 0 .. 31;
      CCR1  at 52 range 0 .. 31;
      OR_k  at 80 range 0 .. 31;
   end record;

   --  General-purpose-timers
   TIM11_Periph : aliased TIM11_Peripheral
     with Import, Address => TIM11_Base;

end STM32_SVD.TIM;
