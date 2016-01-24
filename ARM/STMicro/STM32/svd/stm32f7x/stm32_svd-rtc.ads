--  Automatically generated from STM32F7x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with STM32_SVD;
with System;

package STM32_SVD.RTC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -----------------
   -- TR_Register --
   -----------------

   subtype TR_SU_Field is STM32_SVD.UInt4;
   subtype TR_ST_Field is STM32_SVD.UInt3;
   subtype TR_MNU_Field is STM32_SVD.UInt4;
   subtype TR_MNT_Field is STM32_SVD.UInt3;
   subtype TR_HU_Field is STM32_SVD.UInt4;
   subtype TR_HT_Field is STM32_SVD.UInt2;
   subtype TR_PM_Field is STM32_SVD.Bit;

   --  time register
   type TR_Register is record
      --  Second units in BCD format
      SU             : TR_SU_Field := 16#0#;
      --  Second tens in BCD format
      ST             : TR_ST_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32_SVD.Bit := 16#0#;
      --  Minute units in BCD format
      MNU            : TR_MNU_Field := 16#0#;
      --  Minute tens in BCD format
      MNT            : TR_MNT_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : STM32_SVD.Bit := 16#0#;
      --  Hour units in BCD format
      HU             : TR_HU_Field := 16#0#;
      --  Hour tens in BCD format
      HT             : TR_HT_Field := 16#0#;
      --  AM/PM notation
      PM             : TR_PM_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : STM32_SVD.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TR_Register use record
      SU             at 0 range 0 .. 3;
      ST             at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MNU            at 0 range 8 .. 11;
      MNT            at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      HU             at 0 range 16 .. 19;
      HT             at 0 range 20 .. 21;
      PM             at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   -----------------
   -- DR_Register --
   -----------------

   subtype DR_DU_Field is STM32_SVD.UInt4;
   subtype DR_DT_Field is STM32_SVD.UInt2;
   subtype DR_MU_Field is STM32_SVD.UInt4;
   subtype DR_MT_Field is STM32_SVD.Bit;
   subtype DR_WDU_Field is STM32_SVD.UInt3;
   subtype DR_YU_Field is STM32_SVD.UInt4;
   subtype DR_YT_Field is STM32_SVD.UInt4;

   --  date register
   type DR_Register is record
      --  Date units in BCD format
      DU             : DR_DU_Field := 16#1#;
      --  Date tens in BCD format
      DT             : DR_DT_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : STM32_SVD.UInt2 := 16#0#;
      --  Month units in BCD format
      MU             : DR_MU_Field := 16#1#;
      --  Month tens in BCD format
      MT             : DR_MT_Field := 16#0#;
      --  Week day units
      WDU            : DR_WDU_Field := 16#1#;
      --  Year units in BCD format
      YU             : DR_YU_Field := 16#0#;
      --  Year tens in BCD format
      YT             : DR_YT_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32_SVD.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR_Register use record
      DU             at 0 range 0 .. 3;
      DT             at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      MU             at 0 range 8 .. 11;
      MT             at 0 range 12 .. 12;
      WDU            at 0 range 13 .. 15;
      YU             at 0 range 16 .. 19;
      YT             at 0 range 20 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   -----------------
   -- CR_Register --
   -----------------

   subtype CR_WCKSEL_Field is STM32_SVD.UInt3;
   subtype CR_TSEDGE_Field is STM32_SVD.Bit;
   subtype CR_REFCKON_Field is STM32_SVD.Bit;
   subtype CR_BYPSHAD_Field is STM32_SVD.Bit;
   subtype CR_FMT_Field is STM32_SVD.Bit;
   subtype CR_ALRAE_Field is STM32_SVD.Bit;
   subtype CR_ALRBE_Field is STM32_SVD.Bit;
   subtype CR_WUTE_Field is STM32_SVD.Bit;
   subtype CR_TSE_Field is STM32_SVD.Bit;
   subtype CR_ALRAIE_Field is STM32_SVD.Bit;
   subtype CR_ALRBIE_Field is STM32_SVD.Bit;
   subtype CR_WUTIE_Field is STM32_SVD.Bit;
   subtype CR_TSIE_Field is STM32_SVD.Bit;
   subtype CR_ADD1H_Field is STM32_SVD.Bit;
   subtype CR_SUB1H_Field is STM32_SVD.Bit;
   subtype CR_BKP_Field is STM32_SVD.Bit;
   subtype CR_COSEL_Field is STM32_SVD.Bit;
   subtype CR_POL_Field is STM32_SVD.Bit;
   subtype CR_OSEL_Field is STM32_SVD.UInt2;
   subtype CR_COE_Field is STM32_SVD.Bit;
   subtype CR_ITSE_Field is STM32_SVD.Bit;

   --  control register
   type CR_Register is record
      --  Wakeup clock selection
      WCKSEL         : CR_WCKSEL_Field := 16#0#;
      --  Time-stamp event active edge
      TSEDGE         : CR_TSEDGE_Field := 16#0#;
      --  Reference clock detection enable (50 or 60 Hz)
      REFCKON        : CR_REFCKON_Field := 16#0#;
      --  Bypass the shadow registers
      BYPSHAD        : CR_BYPSHAD_Field := 16#0#;
      --  Hour format
      FMT            : CR_FMT_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32_SVD.Bit := 16#0#;
      --  Alarm A enable
      ALRAE          : CR_ALRAE_Field := 16#0#;
      --  Alarm B enable
      ALRBE          : CR_ALRBE_Field := 16#0#;
      --  Wakeup timer enable
      WUTE           : CR_WUTE_Field := 16#0#;
      --  Time stamp enable
      TSE            : CR_TSE_Field := 16#0#;
      --  Alarm A interrupt enable
      ALRAIE         : CR_ALRAIE_Field := 16#0#;
      --  Alarm B interrupt enable
      ALRBIE         : CR_ALRBIE_Field := 16#0#;
      --  Wakeup timer interrupt enable
      WUTIE          : CR_WUTIE_Field := 16#0#;
      --  Time-stamp interrupt enable
      TSIE           : CR_TSIE_Field := 16#0#;
      --  Add 1 hour (summer time change)
      ADD1H          : CR_ADD1H_Field := 16#0#;
      --  Subtract 1 hour (winter time change)
      SUB1H          : CR_SUB1H_Field := 16#0#;
      --  Backup
      BKP            : CR_BKP_Field := 16#0#;
      --  Calibration output selection
      COSEL          : CR_COSEL_Field := 16#0#;
      --  Output polarity
      POL            : CR_POL_Field := 16#0#;
      --  Output selection
      OSEL           : CR_OSEL_Field := 16#0#;
      --  Calibration output enable
      COE            : CR_COE_Field := 16#0#;
      --  timestamp on internal event enable
      ITSE           : CR_ITSE_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : STM32_SVD.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      WCKSEL         at 0 range 0 .. 2;
      TSEDGE         at 0 range 3 .. 3;
      REFCKON        at 0 range 4 .. 4;
      BYPSHAD        at 0 range 5 .. 5;
      FMT            at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      ALRAE          at 0 range 8 .. 8;
      ALRBE          at 0 range 9 .. 9;
      WUTE           at 0 range 10 .. 10;
      TSE            at 0 range 11 .. 11;
      ALRAIE         at 0 range 12 .. 12;
      ALRBIE         at 0 range 13 .. 13;
      WUTIE          at 0 range 14 .. 14;
      TSIE           at 0 range 15 .. 15;
      ADD1H          at 0 range 16 .. 16;
      SUB1H          at 0 range 17 .. 17;
      BKP            at 0 range 18 .. 18;
      COSEL          at 0 range 19 .. 19;
      POL            at 0 range 20 .. 20;
      OSEL           at 0 range 21 .. 22;
      COE            at 0 range 23 .. 23;
      ITSE           at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   ------------------
   -- ISR_Register --
   ------------------

   subtype ISR_ALRAWF_Field is STM32_SVD.Bit;
   subtype ISR_ALRBWF_Field is STM32_SVD.Bit;
   subtype ISR_WUTWF_Field is STM32_SVD.Bit;
   subtype ISR_SHPF_Field is STM32_SVD.Bit;
   subtype ISR_INITS_Field is STM32_SVD.Bit;
   subtype ISR_RSF_Field is STM32_SVD.Bit;
   subtype ISR_INITF_Field is STM32_SVD.Bit;
   subtype ISR_INIT_Field is STM32_SVD.Bit;
   subtype ISR_ALRAF_Field is STM32_SVD.Bit;
   subtype ISR_ALRBF_Field is STM32_SVD.Bit;
   subtype ISR_WUTF_Field is STM32_SVD.Bit;
   subtype ISR_TSF_Field is STM32_SVD.Bit;
   subtype ISR_TSOVF_Field is STM32_SVD.Bit;
   subtype ISR_TAMP1F_Field is STM32_SVD.Bit;
   subtype ISR_TAMP2F_Field is STM32_SVD.Bit;
   subtype ISR_TAMP3F_Field is STM32_SVD.Bit;
   subtype ISR_RECALPF_Field is STM32_SVD.Bit;

   --  initialization and status register
   type ISR_Register is record
      --  Alarm A write flag
      ALRAWF         : ISR_ALRAWF_Field := 16#1#;
      --  Alarm B write flag
      ALRBWF         : ISR_ALRBWF_Field := 16#1#;
      --  Wakeup timer write flag
      WUTWF          : ISR_WUTWF_Field := 16#1#;
      --  Shift operation pending
      SHPF           : ISR_SHPF_Field := 16#0#;
      --  Initialization status flag
      INITS          : ISR_INITS_Field := 16#0#;
      --  Registers synchronization flag
      RSF            : ISR_RSF_Field := 16#0#;
      --  Initialization flag
      INITF          : ISR_INITF_Field := 16#0#;
      --  Initialization mode
      INIT           : ISR_INIT_Field := 16#0#;
      --  Alarm A flag
      ALRAF          : ISR_ALRAF_Field := 16#0#;
      --  Alarm B flag
      ALRBF          : ISR_ALRBF_Field := 16#0#;
      --  Wakeup timer flag
      WUTF           : ISR_WUTF_Field := 16#0#;
      --  Time-stamp flag
      TSF            : ISR_TSF_Field := 16#0#;
      --  Time-stamp overflow flag
      TSOVF          : ISR_TSOVF_Field := 16#0#;
      --  Tamper detection flag
      TAMP1F         : ISR_TAMP1F_Field := 16#0#;
      --  RTC_TAMP2 detection flag
      TAMP2F         : ISR_TAMP2F_Field := 16#0#;
      --  RTC_TAMP3 detection flag
      TAMP3F         : ISR_TAMP3F_Field := 16#0#;
      --  Recalibration pending Flag
      RECALPF        : ISR_RECALPF_Field := 16#0#;
      --  unspecified
      Reserved_17_31 : STM32_SVD.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      ALRAWF         at 0 range 0 .. 0;
      ALRBWF         at 0 range 1 .. 1;
      WUTWF          at 0 range 2 .. 2;
      SHPF           at 0 range 3 .. 3;
      INITS          at 0 range 4 .. 4;
      RSF            at 0 range 5 .. 5;
      INITF          at 0 range 6 .. 6;
      INIT           at 0 range 7 .. 7;
      ALRAF          at 0 range 8 .. 8;
      ALRBF          at 0 range 9 .. 9;
      WUTF           at 0 range 10 .. 10;
      TSF            at 0 range 11 .. 11;
      TSOVF          at 0 range 12 .. 12;
      TAMP1F         at 0 range 13 .. 13;
      TAMP2F         at 0 range 14 .. 14;
      TAMP3F         at 0 range 15 .. 15;
      RECALPF        at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   -------------------
   -- PRER_Register --
   -------------------

   subtype PRER_PREDIV_S_Field is STM32_SVD.UInt15;
   subtype PRER_PREDIV_A_Field is STM32_SVD.UInt7;

   --  prescaler register
   type PRER_Register is record
      --  Synchronous prescaler factor
      PREDIV_S       : PRER_PREDIV_S_Field := 16#FF#;
      --  unspecified
      Reserved_15_15 : STM32_SVD.Bit := 16#0#;
      --  Asynchronous prescaler factor
      PREDIV_A       : PRER_PREDIV_A_Field := 16#7F#;
      --  unspecified
      Reserved_23_31 : STM32_SVD.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRER_Register use record
      PREDIV_S       at 0 range 0 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      PREDIV_A       at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   -------------------
   -- WUTR_Register --
   -------------------

   subtype WUTR_WUT_Field is STM32_SVD.Short;

   --  wakeup timer register
   type WUTR_Register is record
      --  Wakeup auto-reload value bits
      WUT            : WUTR_WUT_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WUTR_Register use record
      WUT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ---------------------
   -- ALRMAR_Register --
   ---------------------

   subtype ALRMAR_SU_Field is STM32_SVD.UInt4;
   subtype ALRMAR_ST_Field is STM32_SVD.UInt3;
   subtype ALRMAR_MSK1_Field is STM32_SVD.Bit;
   subtype ALRMAR_MNU_Field is STM32_SVD.UInt4;
   subtype ALRMAR_MNT_Field is STM32_SVD.UInt3;
   subtype ALRMAR_MSK2_Field is STM32_SVD.Bit;
   subtype ALRMAR_HU_Field is STM32_SVD.UInt4;
   subtype ALRMAR_HT_Field is STM32_SVD.UInt2;
   subtype ALRMAR_PM_Field is STM32_SVD.Bit;
   subtype ALRMAR_MSK3_Field is STM32_SVD.Bit;
   subtype ALRMAR_DU_Field is STM32_SVD.UInt4;
   subtype ALRMAR_DT_Field is STM32_SVD.UInt2;
   subtype ALRMAR_WDSEL_Field is STM32_SVD.Bit;
   subtype ALRMAR_MSK4_Field is STM32_SVD.Bit;

   --  alarm A register
   type ALRMAR_Register is record
      --  Second units in BCD format
      SU    : ALRMAR_SU_Field := 16#0#;
      --  Second tens in BCD format
      ST    : ALRMAR_ST_Field := 16#0#;
      --  Alarm A seconds mask
      MSK1  : ALRMAR_MSK1_Field := 16#0#;
      --  Minute units in BCD format
      MNU   : ALRMAR_MNU_Field := 16#0#;
      --  Minute tens in BCD format
      MNT   : ALRMAR_MNT_Field := 16#0#;
      --  Alarm A minutes mask
      MSK2  : ALRMAR_MSK2_Field := 16#0#;
      --  Hour units in BCD format
      HU    : ALRMAR_HU_Field := 16#0#;
      --  Hour tens in BCD format
      HT    : ALRMAR_HT_Field := 16#0#;
      --  AM/PM notation
      PM    : ALRMAR_PM_Field := 16#0#;
      --  Alarm A hours mask
      MSK3  : ALRMAR_MSK3_Field := 16#0#;
      --  Date units or day in BCD format
      DU    : ALRMAR_DU_Field := 16#0#;
      --  Date tens in BCD format
      DT    : ALRMAR_DT_Field := 16#0#;
      --  Week day selection
      WDSEL : ALRMAR_WDSEL_Field := 16#0#;
      --  Alarm A date mask
      MSK4  : ALRMAR_MSK4_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ALRMAR_Register use record
      SU    at 0 range 0 .. 3;
      ST    at 0 range 4 .. 6;
      MSK1  at 0 range 7 .. 7;
      MNU   at 0 range 8 .. 11;
      MNT   at 0 range 12 .. 14;
      MSK2  at 0 range 15 .. 15;
      HU    at 0 range 16 .. 19;
      HT    at 0 range 20 .. 21;
      PM    at 0 range 22 .. 22;
      MSK3  at 0 range 23 .. 23;
      DU    at 0 range 24 .. 27;
      DT    at 0 range 28 .. 29;
      WDSEL at 0 range 30 .. 30;
      MSK4  at 0 range 31 .. 31;
   end record;

   ---------------------
   -- ALRMBR_Register --
   ---------------------

   subtype ALRMBR_SU_Field is STM32_SVD.UInt4;
   subtype ALRMBR_ST_Field is STM32_SVD.UInt3;
   subtype ALRMBR_MSK1_Field is STM32_SVD.Bit;
   subtype ALRMBR_MNU_Field is STM32_SVD.UInt4;
   subtype ALRMBR_MNT_Field is STM32_SVD.UInt3;
   subtype ALRMBR_MSK2_Field is STM32_SVD.Bit;
   subtype ALRMBR_HU_Field is STM32_SVD.UInt4;
   subtype ALRMBR_HT_Field is STM32_SVD.UInt2;
   subtype ALRMBR_PM_Field is STM32_SVD.Bit;
   subtype ALRMBR_MSK3_Field is STM32_SVD.Bit;
   subtype ALRMBR_DU_Field is STM32_SVD.UInt4;
   subtype ALRMBR_DT_Field is STM32_SVD.UInt2;
   subtype ALRMBR_WDSEL_Field is STM32_SVD.Bit;
   subtype ALRMBR_MSK4_Field is STM32_SVD.Bit;

   --  alarm B register
   type ALRMBR_Register is record
      --  Second units in BCD format
      SU    : ALRMBR_SU_Field := 16#0#;
      --  Second tens in BCD format
      ST    : ALRMBR_ST_Field := 16#0#;
      --  Alarm B seconds mask
      MSK1  : ALRMBR_MSK1_Field := 16#0#;
      --  Minute units in BCD format
      MNU   : ALRMBR_MNU_Field := 16#0#;
      --  Minute tens in BCD format
      MNT   : ALRMBR_MNT_Field := 16#0#;
      --  Alarm B minutes mask
      MSK2  : ALRMBR_MSK2_Field := 16#0#;
      --  Hour units in BCD format
      HU    : ALRMBR_HU_Field := 16#0#;
      --  Hour tens in BCD format
      HT    : ALRMBR_HT_Field := 16#0#;
      --  AM/PM notation
      PM    : ALRMBR_PM_Field := 16#0#;
      --  Alarm B hours mask
      MSK3  : ALRMBR_MSK3_Field := 16#0#;
      --  Date units or day in BCD format
      DU    : ALRMBR_DU_Field := 16#0#;
      --  Date tens in BCD format
      DT    : ALRMBR_DT_Field := 16#0#;
      --  Week day selection
      WDSEL : ALRMBR_WDSEL_Field := 16#0#;
      --  Alarm B date mask
      MSK4  : ALRMBR_MSK4_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ALRMBR_Register use record
      SU    at 0 range 0 .. 3;
      ST    at 0 range 4 .. 6;
      MSK1  at 0 range 7 .. 7;
      MNU   at 0 range 8 .. 11;
      MNT   at 0 range 12 .. 14;
      MSK2  at 0 range 15 .. 15;
      HU    at 0 range 16 .. 19;
      HT    at 0 range 20 .. 21;
      PM    at 0 range 22 .. 22;
      MSK3  at 0 range 23 .. 23;
      DU    at 0 range 24 .. 27;
      DT    at 0 range 28 .. 29;
      WDSEL at 0 range 30 .. 30;
      MSK4  at 0 range 31 .. 31;
   end record;

   ------------------
   -- WPR_Register --
   ------------------

   subtype WPR_KEY_Field is STM32_SVD.Byte;

   --  write protection register
   type WPR_Register is record
      --  Write protection key
      KEY           : WPR_KEY_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WPR_Register use record
      KEY           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   ------------------
   -- SSR_Register --
   ------------------

   subtype SSR_SS_Field is STM32_SVD.Short;

   --  sub second register
   type SSR_Register is record
      --  Sub second value
      SS             : SSR_SS_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SSR_Register use record
      SS             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ---------------------
   -- SHIFTR_Register --
   ---------------------

   subtype SHIFTR_SUBFS_Field is STM32_SVD.UInt15;
   subtype SHIFTR_ADD1S_Field is STM32_SVD.Bit;

   --  shift control register
   type SHIFTR_Register is record
      --  Subtract a fraction of a second
      SUBFS          : SHIFTR_SUBFS_Field := 16#0#;
      --  unspecified
      Reserved_15_30 : STM32_SVD.Short := 16#0#;
      --  Add one second
      ADD1S          : SHIFTR_ADD1S_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHIFTR_Register use record
      SUBFS          at 0 range 0 .. 14;
      Reserved_15_30 at 0 range 15 .. 30;
      ADD1S          at 0 range 31 .. 31;
   end record;

   -------------------
   -- TSTR_Register --
   -------------------

   subtype TSTR_SU_Field is STM32_SVD.UInt4;
   subtype TSTR_ST_Field is STM32_SVD.UInt3;
   subtype TSTR_MNU_Field is STM32_SVD.UInt4;
   subtype TSTR_MNT_Field is STM32_SVD.UInt3;
   subtype TSTR_HU_Field is STM32_SVD.UInt4;
   subtype TSTR_HT_Field is STM32_SVD.UInt2;
   subtype TSTR_PM_Field is STM32_SVD.Bit;

   --  time stamp time register
   type TSTR_Register is record
      --  Second units in BCD format
      SU             : TSTR_SU_Field := 16#0#;
      --  Second tens in BCD format
      ST             : TSTR_ST_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32_SVD.Bit := 16#0#;
      --  Minute units in BCD format
      MNU            : TSTR_MNU_Field := 16#0#;
      --  Minute tens in BCD format
      MNT            : TSTR_MNT_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : STM32_SVD.Bit := 16#0#;
      --  Hour units in BCD format
      HU             : TSTR_HU_Field := 16#0#;
      --  Hour tens in BCD format
      HT             : TSTR_HT_Field := 16#0#;
      --  AM/PM notation
      PM             : TSTR_PM_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : STM32_SVD.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSTR_Register use record
      SU             at 0 range 0 .. 3;
      ST             at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MNU            at 0 range 8 .. 11;
      MNT            at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      HU             at 0 range 16 .. 19;
      HT             at 0 range 20 .. 21;
      PM             at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   -------------------
   -- TSDR_Register --
   -------------------

   subtype TSDR_DU_Field is STM32_SVD.UInt4;
   subtype TSDR_DT_Field is STM32_SVD.UInt2;
   subtype TSDR_MU_Field is STM32_SVD.UInt4;
   subtype TSDR_MT_Field is STM32_SVD.Bit;
   subtype TSDR_WDU_Field is STM32_SVD.UInt3;

   --  time stamp date register
   type TSDR_Register is record
      --  Date units in BCD format
      DU             : TSDR_DU_Field := 16#0#;
      --  Date tens in BCD format
      DT             : TSDR_DT_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : STM32_SVD.UInt2 := 16#0#;
      --  Month units in BCD format
      MU             : TSDR_MU_Field := 16#0#;
      --  Month tens in BCD format
      MT             : TSDR_MT_Field := 16#0#;
      --  Week day units
      WDU            : TSDR_WDU_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSDR_Register use record
      DU             at 0 range 0 .. 3;
      DT             at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      MU             at 0 range 8 .. 11;
      MT             at 0 range 12 .. 12;
      WDU            at 0 range 13 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --------------------
   -- TSSSR_Register --
   --------------------

   subtype TSSSR_SS_Field is STM32_SVD.Short;

   --  timestamp sub second register
   type TSSSR_Register is record
      --  Sub second value
      SS             : TSSSR_SS_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSSSR_Register use record
      SS             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -------------------
   -- CALR_Register --
   -------------------

   subtype CALR_CALM_Field is STM32_SVD.UInt9;

   ---------------
   -- CALR.CALW --
   ---------------

   --  CALR_CALW array element
   subtype CALR_CALW_Element is STM32_SVD.Bit;

   --  CALR_CALW array
   type CALR_CALW_Field_Array is array (0 .. 1) of CALR_CALW_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for CALR_CALW
   type CALR_CALW_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CALW as a value
            Val : STM32_SVD.UInt2;
         when True =>
            --  CALW as an array
            Arr : CALR_CALW_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for CALR_CALW_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype CALR_CALP_Field is STM32_SVD.Bit;

   --  calibration register
   type CALR_Register is record
      --  Calibration minus
      CALM           : CALR_CALM_Field := 16#0#;
      --  unspecified
      Reserved_9_12  : STM32_SVD.UInt4 := 16#0#;
      --  Use a 16-second calibration cycle period
      CALW           : CALR_CALW_Field := (As_Array => False, Val => 16#0#);
      --  Increase frequency of RTC by 488.5 ppm
      CALP           : CALR_CALP_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CALR_Register use record
      CALM           at 0 range 0 .. 8;
      Reserved_9_12  at 0 range 9 .. 12;
      CALW           at 0 range 13 .. 14;
      CALP           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ---------------------
   -- TAMPCR_Register --
   ---------------------

   subtype TAMPCR_TAMP1E_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMP1TRG_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMPIE_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMP2E_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMP2TRG_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMP3E_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMP3TRG_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMPTS_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMPFREQ_Field is STM32_SVD.UInt3;
   subtype TAMPCR_TAMPFLT_Field is STM32_SVD.UInt2;
   subtype TAMPCR_TAMPPRCH_Field is STM32_SVD.UInt2;
   subtype TAMPCR_TAMPPUDIS_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMP1IE_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMP1NOERASE_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMP1MF_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMP2IE_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMP2NOERASE_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMP2MF_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMP3IE_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMP3NOERASE_Field is STM32_SVD.Bit;
   subtype TAMPCR_TAMP3MF_Field is STM32_SVD.Bit;

   --  tamper configuration register
   type TAMPCR_Register is record
      --  Tamper 1 detection enable
      TAMP1E         : TAMPCR_TAMP1E_Field := 16#0#;
      --  Active level for tamper 1
      TAMP1TRG       : TAMPCR_TAMP1TRG_Field := 16#0#;
      --  Tamper interrupt enable
      TAMPIE         : TAMPCR_TAMPIE_Field := 16#0#;
      --  Tamper 2 detection enable
      TAMP2E         : TAMPCR_TAMP2E_Field := 16#0#;
      --  Active level for tamper 2
      TAMP2TRG       : TAMPCR_TAMP2TRG_Field := 16#0#;
      --  Tamper 3 detection enable
      TAMP3E         : TAMPCR_TAMP3E_Field := 16#0#;
      --  Active level for tamper 3
      TAMP3TRG       : TAMPCR_TAMP3TRG_Field := 16#0#;
      --  Activate timestamp on tamper detection event
      TAMPTS         : TAMPCR_TAMPTS_Field := 16#0#;
      --  Tamper sampling frequency
      TAMPFREQ       : TAMPCR_TAMPFREQ_Field := 16#0#;
      --  Tamper filter count
      TAMPFLT        : TAMPCR_TAMPFLT_Field := 16#0#;
      --  Tamper precharge duration
      TAMPPRCH       : TAMPCR_TAMPPRCH_Field := 16#0#;
      --  TAMPER pull-up disable
      TAMPPUDIS      : TAMPCR_TAMPPUDIS_Field := 16#0#;
      --  Tamper 1 interrupt enable
      TAMP1IE        : TAMPCR_TAMP1IE_Field := 16#0#;
      --  Tamper 1 no erase
      TAMP1NOERASE   : TAMPCR_TAMP1NOERASE_Field := 16#0#;
      --  Tamper 1 mask flag
      TAMP1MF        : TAMPCR_TAMP1MF_Field := 16#0#;
      --  Tamper 2 interrupt enable
      TAMP2IE        : TAMPCR_TAMP2IE_Field := 16#0#;
      --  Tamper 2 no erase
      TAMP2NOERASE   : TAMPCR_TAMP2NOERASE_Field := 16#0#;
      --  Tamper 2 mask flag
      TAMP2MF        : TAMPCR_TAMP2MF_Field := 16#0#;
      --  Tamper 3 interrupt enable
      TAMP3IE        : TAMPCR_TAMP3IE_Field := 16#0#;
      --  Tamper 3 no erase
      TAMP3NOERASE   : TAMPCR_TAMP3NOERASE_Field := 16#0#;
      --  Tamper 3 mask flag
      TAMP3MF        : TAMPCR_TAMP3MF_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : STM32_SVD.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TAMPCR_Register use record
      TAMP1E         at 0 range 0 .. 0;
      TAMP1TRG       at 0 range 1 .. 1;
      TAMPIE         at 0 range 2 .. 2;
      TAMP2E         at 0 range 3 .. 3;
      TAMP2TRG       at 0 range 4 .. 4;
      TAMP3E         at 0 range 5 .. 5;
      TAMP3TRG       at 0 range 6 .. 6;
      TAMPTS         at 0 range 7 .. 7;
      TAMPFREQ       at 0 range 8 .. 10;
      TAMPFLT        at 0 range 11 .. 12;
      TAMPPRCH       at 0 range 13 .. 14;
      TAMPPUDIS      at 0 range 15 .. 15;
      TAMP1IE        at 0 range 16 .. 16;
      TAMP1NOERASE   at 0 range 17 .. 17;
      TAMP1MF        at 0 range 18 .. 18;
      TAMP2IE        at 0 range 19 .. 19;
      TAMP2NOERASE   at 0 range 20 .. 20;
      TAMP2MF        at 0 range 21 .. 21;
      TAMP3IE        at 0 range 22 .. 22;
      TAMP3NOERASE   at 0 range 23 .. 23;
      TAMP3MF        at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   -----------------------
   -- ALRMASSR_Register --
   -----------------------

   subtype ALRMASSR_SS_Field is STM32_SVD.UInt15;
   subtype ALRMASSR_MASKSS_Field is STM32_SVD.UInt4;

   --  alarm A sub second register
   type ALRMASSR_Register is record
      --  Sub seconds value
      SS             : ALRMASSR_SS_Field := 16#0#;
      --  unspecified
      Reserved_15_23 : STM32_SVD.UInt9 := 16#0#;
      --  Mask the most-significant bits starting at this bit
      MASKSS         : ALRMASSR_MASKSS_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ALRMASSR_Register use record
      SS             at 0 range 0 .. 14;
      Reserved_15_23 at 0 range 15 .. 23;
      MASKSS         at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   -----------------------
   -- ALRMBSSR_Register --
   -----------------------

   subtype ALRMBSSR_SS_Field is STM32_SVD.UInt15;
   subtype ALRMBSSR_MASKSS_Field is STM32_SVD.UInt4;

   --  alarm B sub second register
   type ALRMBSSR_Register is record
      --  Sub seconds value
      SS             : ALRMBSSR_SS_Field := 16#0#;
      --  unspecified
      Reserved_15_23 : STM32_SVD.UInt9 := 16#0#;
      --  Mask the most-significant bits starting at this bit
      MASKSS         : ALRMBSSR_MASKSS_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ALRMBSSR_Register use record
      SS             at 0 range 0 .. 14;
      Reserved_15_23 at 0 range 15 .. 23;
      MASKSS         at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   -----------------
   -- OR_Register --
   -----------------

   subtype OR_RTC_ALARM_TYPE_Field is STM32_SVD.Bit;
   subtype OR_RTC_OUT_RMP_Field is STM32_SVD.Bit;

   --  option register
   type OR_Register is record
      --  RTC_ALARM on PC13 output type
      RTC_ALARM_TYPE : OR_RTC_ALARM_TYPE_Field := 16#0#;
      --  RTC_OUT remap
      RTC_OUT_RMP    : OR_RTC_OUT_RMP_Field := 16#0#;
      --  unspecified
      Reserved_2_31  : STM32_SVD.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OR_Register use record
      RTC_ALARM_TYPE at 0 range 0 .. 0;
      RTC_OUT_RMP    at 0 range 1 .. 1;
      Reserved_2_31  at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Real-time clock
   type RTC_Peripheral is record
      --  time register
      TR       : TR_Register;
      --  date register
      DR       : DR_Register;
      --  control register
      CR       : CR_Register;
      --  initialization and status register
      ISR      : ISR_Register;
      --  prescaler register
      PRER     : PRER_Register;
      --  wakeup timer register
      WUTR     : WUTR_Register;
      --  alarm A register
      ALRMAR   : ALRMAR_Register;
      --  alarm B register
      ALRMBR   : ALRMBR_Register;
      --  write protection register
      WPR      : WPR_Register;
      --  sub second register
      SSR      : SSR_Register;
      --  shift control register
      SHIFTR   : SHIFTR_Register;
      --  time stamp time register
      TSTR     : TSTR_Register;
      --  time stamp date register
      TSDR     : TSDR_Register;
      --  timestamp sub second register
      TSSSR    : TSSSR_Register;
      --  calibration register
      CALR     : CALR_Register;
      --  tamper configuration register
      TAMPCR   : TAMPCR_Register;
      --  alarm A sub second register
      ALRMASSR : ALRMASSR_Register;
      --  alarm B sub second register
      ALRMBSSR : ALRMBSSR_Register;
      --  option register
      OR_k     : OR_Register;
      --  backup register
      BKP0R    : STM32_SVD.Word;
      --  backup register
      BKP1R    : STM32_SVD.Word;
      --  backup register
      BKP2R    : STM32_SVD.Word;
      --  backup register
      BKP3R    : STM32_SVD.Word;
      --  backup register
      BKP4R    : STM32_SVD.Word;
      --  backup register
      BKP5R    : STM32_SVD.Word;
      --  backup register
      BKP6R    : STM32_SVD.Word;
      --  backup register
      BKP7R    : STM32_SVD.Word;
      --  backup register
      BKP8R    : STM32_SVD.Word;
      --  backup register
      BKP9R    : STM32_SVD.Word;
      --  backup register
      BKP10R   : STM32_SVD.Word;
      --  backup register
      BKP11R   : STM32_SVD.Word;
      --  backup register
      BKP12R   : STM32_SVD.Word;
      --  backup register
      BKP13R   : STM32_SVD.Word;
      --  backup register
      BKP14R   : STM32_SVD.Word;
      --  backup register
      BKP15R   : STM32_SVD.Word;
      --  backup register
      BKP16R   : STM32_SVD.Word;
      --  backup register
      BKP17R   : STM32_SVD.Word;
      --  backup register
      BKP18R   : STM32_SVD.Word;
      --  backup register
      BKP19R   : STM32_SVD.Word;
      --  backup register
      BKP20R   : STM32_SVD.Word;
      --  backup register
      BKP21R   : STM32_SVD.Word;
      --  backup register
      BKP22R   : STM32_SVD.Word;
      --  backup register
      BKP23R   : STM32_SVD.Word;
      --  backup register
      BKP24R   : STM32_SVD.Word;
      --  backup register
      BKP25R   : STM32_SVD.Word;
      --  backup register
      BKP26R   : STM32_SVD.Word;
      --  backup register
      BKP27R   : STM32_SVD.Word;
      --  backup register
      BKP28R   : STM32_SVD.Word;
      --  backup register
      BKP29R   : STM32_SVD.Word;
      --  backup register
      BKP30R   : STM32_SVD.Word;
      --  backup register
      BKP31R   : STM32_SVD.Word;
   end record
     with Volatile;

   for RTC_Peripheral use record
      TR       at 0 range 0 .. 31;
      DR       at 4 range 0 .. 31;
      CR       at 8 range 0 .. 31;
      ISR      at 12 range 0 .. 31;
      PRER     at 16 range 0 .. 31;
      WUTR     at 20 range 0 .. 31;
      ALRMAR   at 28 range 0 .. 31;
      ALRMBR   at 32 range 0 .. 31;
      WPR      at 36 range 0 .. 31;
      SSR      at 40 range 0 .. 31;
      SHIFTR   at 44 range 0 .. 31;
      TSTR     at 48 range 0 .. 31;
      TSDR     at 52 range 0 .. 31;
      TSSSR    at 56 range 0 .. 31;
      CALR     at 60 range 0 .. 31;
      TAMPCR   at 64 range 0 .. 31;
      ALRMASSR at 68 range 0 .. 31;
      ALRMBSSR at 72 range 0 .. 31;
      OR_k     at 76 range 0 .. 31;
      BKP0R    at 80 range 0 .. 31;
      BKP1R    at 84 range 0 .. 31;
      BKP2R    at 88 range 0 .. 31;
      BKP3R    at 92 range 0 .. 31;
      BKP4R    at 96 range 0 .. 31;
      BKP5R    at 100 range 0 .. 31;
      BKP6R    at 104 range 0 .. 31;
      BKP7R    at 108 range 0 .. 31;
      BKP8R    at 112 range 0 .. 31;
      BKP9R    at 116 range 0 .. 31;
      BKP10R   at 120 range 0 .. 31;
      BKP11R   at 124 range 0 .. 31;
      BKP12R   at 128 range 0 .. 31;
      BKP13R   at 132 range 0 .. 31;
      BKP14R   at 136 range 0 .. 31;
      BKP15R   at 140 range 0 .. 31;
      BKP16R   at 144 range 0 .. 31;
      BKP17R   at 148 range 0 .. 31;
      BKP18R   at 152 range 0 .. 31;
      BKP19R   at 156 range 0 .. 31;
      BKP20R   at 160 range 0 .. 31;
      BKP21R   at 164 range 0 .. 31;
      BKP22R   at 168 range 0 .. 31;
      BKP23R   at 172 range 0 .. 31;
      BKP24R   at 176 range 0 .. 31;
      BKP25R   at 180 range 0 .. 31;
      BKP26R   at 184 range 0 .. 31;
      BKP27R   at 188 range 0 .. 31;
      BKP28R   at 192 range 0 .. 31;
      BKP29R   at 196 range 0 .. 31;
      BKP30R   at 200 range 0 .. 31;
      BKP31R   at 204 range 0 .. 31;
   end record;

   --  Real-time clock
   RTC_Periph : aliased RTC_Peripheral
     with Import, Address => System'To_Address (16#40002800#);

end STM32_SVD.RTC;
