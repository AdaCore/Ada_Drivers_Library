--  This spec has been automatically generated from STM32F46_79x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.SAI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype GCR_SYNCOUT_Field is HAL.UInt2;

   --  Global configuration register
   type GCR_Register is record
      --  Synchronization outputs
      SYNCOUT       : GCR_SYNCOUT_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for GCR_Register use record
      SYNCOUT       at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype ACR1_MODE_Field is HAL.UInt2;
   subtype ACR1_PRTCFG_Field is HAL.UInt2;
   subtype ACR1_DS_Field is HAL.UInt3;
   subtype ACR1_SYNCEN_Field is HAL.UInt2;
   subtype ACR1_MCJDIV_Field is HAL.UInt4;

   --  AConfiguration register 1
   type ACR1_Register is record
      --  Audio block mode
      MODE           : ACR1_MODE_Field := 16#0#;
      --  Protocol configuration
      PRTCFG         : ACR1_PRTCFG_Field := 16#0#;
      --  unspecified
      Reserved_4_4   : HAL.Bit := 16#0#;
      --  Data size
      DS             : ACR1_DS_Field := 16#2#;
      --  Least significant bit first
      LSBFIRST       : Boolean := False;
      --  Clock strobing edge
      CKSTR          : Boolean := False;
      --  Synchronization enable
      SYNCEN         : ACR1_SYNCEN_Field := 16#0#;
      --  Mono mode
      MONO           : Boolean := False;
      --  Output drive
      OutDri         : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Audio block A enable
      SAIAEN         : Boolean := False;
      --  DMA enable
      DMAEN          : Boolean := False;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  No divider
      NODIV          : Boolean := False;
      --  Master clock divider
      MCJDIV         : ACR1_MCJDIV_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ACR1_Register use record
      MODE           at 0 range 0 .. 1;
      PRTCFG         at 0 range 2 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      DS             at 0 range 5 .. 7;
      LSBFIRST       at 0 range 8 .. 8;
      CKSTR          at 0 range 9 .. 9;
      SYNCEN         at 0 range 10 .. 11;
      MONO           at 0 range 12 .. 12;
      OutDri         at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      SAIAEN         at 0 range 16 .. 16;
      DMAEN          at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      NODIV          at 0 range 19 .. 19;
      MCJDIV         at 0 range 20 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype ACR2_FTH_Field is HAL.UInt3;
   subtype ACR2_MUTECN_Field is HAL.UInt6;
   subtype ACR2_COMP_Field is HAL.UInt2;

   --  AConfiguration register 2
   type ACR2_Register is record
      --  FIFO threshold
      FTH            : ACR2_FTH_Field := 16#0#;
      --  FIFO flush
      FFLUS          : Boolean := False;
      --  Tristate management on data line
      TRIS           : Boolean := False;
      --  Mute
      MUTE           : Boolean := False;
      --  Mute value
      MUTEVAL        : Boolean := False;
      --  Mute counter
      MUTECN         : ACR2_MUTECN_Field := 16#0#;
      --  Complement bit
      CPL            : Boolean := False;
      --  Companding mode
      COMP           : ACR2_COMP_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ACR2_Register use record
      FTH            at 0 range 0 .. 2;
      FFLUS          at 0 range 3 .. 3;
      TRIS           at 0 range 4 .. 4;
      MUTE           at 0 range 5 .. 5;
      MUTEVAL        at 0 range 6 .. 6;
      MUTECN         at 0 range 7 .. 12;
      CPL            at 0 range 13 .. 13;
      COMP           at 0 range 14 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype AFRCR_FRL_Field is HAL.UInt8;
   subtype AFRCR_FSALL_Field is HAL.UInt7;

   --  AFRCR
   type AFRCR_Register is record
      --  Frame length
      FRL            : AFRCR_FRL_Field := 16#7#;
      --  Frame synchronization active level length
      FSALL          : AFRCR_FSALL_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Frame synchronization definition
      FSDEF          : Boolean := False;
      --  Frame synchronization polarity
      FSPOL          : Boolean := False;
      --  Frame synchronization offset
      FSOFF          : Boolean := False;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AFRCR_Register use record
      FRL            at 0 range 0 .. 7;
      FSALL          at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FSDEF          at 0 range 16 .. 16;
      FSPOL          at 0 range 17 .. 17;
      FSOFF          at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype ASLOTR_FBOFF_Field is HAL.UInt5;
   subtype ASLOTR_SLOTSZ_Field is HAL.UInt2;
   subtype ASLOTR_NBSLOT_Field is HAL.UInt4;
   subtype ASLOTR_SLOTEN_Field is HAL.UInt16;

   --  ASlot register
   type ASLOTR_Register is record
      --  First bit offset
      FBOFF          : ASLOTR_FBOFF_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Slot size
      SLOTSZ         : ASLOTR_SLOTSZ_Field := 16#0#;
      --  Number of slots in an audio frame
      NBSLOT         : ASLOTR_NBSLOT_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Slot enable
      SLOTEN         : ASLOTR_SLOTEN_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ASLOTR_Register use record
      FBOFF          at 0 range 0 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      SLOTSZ         at 0 range 6 .. 7;
      NBSLOT         at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SLOTEN         at 0 range 16 .. 31;
   end record;

   --  AInterrupt mask register2
   type AIM_Register is record
      --  Overrun/underrun interrupt enable
      OVRUDRIE      : Boolean := False;
      --  Mute detection interrupt enable
      MUTEDET       : Boolean := False;
      --  Wrong clock configuration interrupt enable
      WCKCFG        : Boolean := False;
      --  FIFO request interrupt enable
      FREQIE        : Boolean := False;
      --  Codec not ready interrupt enable
      CNRDYIE       : Boolean := False;
      --  Anticipated frame synchronization detection interrupt enable
      AFSDETIE      : Boolean := False;
      --  Late frame synchronization detection interrupt enable
      LFSDET        : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AIM_Register use record
      OVRUDRIE      at 0 range 0 .. 0;
      MUTEDET       at 0 range 1 .. 1;
      WCKCFG        at 0 range 2 .. 2;
      FREQIE        at 0 range 3 .. 3;
      CNRDYIE       at 0 range 4 .. 4;
      AFSDETIE      at 0 range 5 .. 5;
      LFSDET        at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   subtype ASR_FLVL_Field is HAL.UInt3;

   --  AStatus register
   type ASR_Register is record
      --  Overrun / underrun
      OVRUDR         : Boolean := False;
      --  Mute detection
      MUTEDET        : Boolean := False;
      --  Wrong clock configuration flag. This bit is read only.
      WCKCFG         : Boolean := False;
      --  FIFO request
      FREQ           : Boolean := False;
      --  Codec not ready
      CNRDY          : Boolean := False;
      --  Anticipated frame synchronization detection
      AFSDET         : Boolean := False;
      --  Late frame synchronization detection
      LFSDET         : Boolean := False;
      --  unspecified
      Reserved_7_15  : HAL.UInt9 := 16#0#;
      --  FIFO level threshold
      FLVL           : ASR_FLVL_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ASR_Register use record
      OVRUDR         at 0 range 0 .. 0;
      MUTEDET        at 0 range 1 .. 1;
      WCKCFG         at 0 range 2 .. 2;
      FREQ           at 0 range 3 .. 3;
      CNRDY          at 0 range 4 .. 4;
      AFSDET         at 0 range 5 .. 5;
      LFSDET         at 0 range 6 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      FLVL           at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  AClear flag register
   type ACLRFR_Register is record
      --  Clear overrun / underrun
      OVRUDR        : Boolean := False;
      --  Mute detection flag
      MUTEDET       : Boolean := False;
      --  Clear wrong clock configuration flag
      WCKCFG        : Boolean := False;
      --  unspecified
      Reserved_3_3  : HAL.Bit := 16#0#;
      --  Clear codec not ready flag
      CNRDY         : Boolean := False;
      --  Clear anticipated frame synchronization detection flag.
      CAFSDET       : Boolean := False;
      --  Clear late frame synchronization detection flag
      LFSDET        : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ACLRFR_Register use record
      OVRUDR        at 0 range 0 .. 0;
      MUTEDET       at 0 range 1 .. 1;
      WCKCFG        at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      CNRDY         at 0 range 4 .. 4;
      CAFSDET       at 0 range 5 .. 5;
      LFSDET        at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   subtype BCR1_MODE_Field is HAL.UInt2;
   subtype BCR1_PRTCFG_Field is HAL.UInt2;
   subtype BCR1_DS_Field is HAL.UInt3;
   subtype BCR1_SYNCEN_Field is HAL.UInt2;
   subtype BCR1_MCJDIV_Field is HAL.UInt4;

   --  BConfiguration register 1
   type BCR1_Register is record
      --  Audio block mode
      MODE           : BCR1_MODE_Field := 16#0#;
      --  Protocol configuration
      PRTCFG         : BCR1_PRTCFG_Field := 16#0#;
      --  unspecified
      Reserved_4_4   : HAL.Bit := 16#0#;
      --  Data size
      DS             : BCR1_DS_Field := 16#2#;
      --  Least significant bit first
      LSBFIRST       : Boolean := False;
      --  Clock strobing edge
      CKSTR          : Boolean := False;
      --  Synchronization enable
      SYNCEN         : BCR1_SYNCEN_Field := 16#0#;
      --  Mono mode
      MONO           : Boolean := False;
      --  Output drive
      OutDri         : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Audio block B enable
      SAIBEN         : Boolean := False;
      --  DMA enable
      DMAEN          : Boolean := False;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  No divider
      NODIV          : Boolean := False;
      --  Master clock divider
      MCJDIV         : BCR1_MCJDIV_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BCR1_Register use record
      MODE           at 0 range 0 .. 1;
      PRTCFG         at 0 range 2 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      DS             at 0 range 5 .. 7;
      LSBFIRST       at 0 range 8 .. 8;
      CKSTR          at 0 range 9 .. 9;
      SYNCEN         at 0 range 10 .. 11;
      MONO           at 0 range 12 .. 12;
      OutDri         at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      SAIBEN         at 0 range 16 .. 16;
      DMAEN          at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      NODIV          at 0 range 19 .. 19;
      MCJDIV         at 0 range 20 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype BCR2_FTH_Field is HAL.UInt3;
   subtype BCR2_MUTECN_Field is HAL.UInt6;
   subtype BCR2_COMP_Field is HAL.UInt2;

   --  BConfiguration register 2
   type BCR2_Register is record
      --  FIFO threshold
      FTH            : BCR2_FTH_Field := 16#0#;
      --  FIFO flush
      FFLUS          : Boolean := False;
      --  Tristate management on data line
      TRIS           : Boolean := False;
      --  Mute
      MUTE           : Boolean := False;
      --  Mute value
      MUTEVAL        : Boolean := False;
      --  Mute counter
      MUTECN         : BCR2_MUTECN_Field := 16#0#;
      --  Complement bit
      CPL            : Boolean := False;
      --  Companding mode
      COMP           : BCR2_COMP_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BCR2_Register use record
      FTH            at 0 range 0 .. 2;
      FFLUS          at 0 range 3 .. 3;
      TRIS           at 0 range 4 .. 4;
      MUTE           at 0 range 5 .. 5;
      MUTEVAL        at 0 range 6 .. 6;
      MUTECN         at 0 range 7 .. 12;
      CPL            at 0 range 13 .. 13;
      COMP           at 0 range 14 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype BFRCR_FRL_Field is HAL.UInt8;
   subtype BFRCR_FSALL_Field is HAL.UInt7;

   --  BFRCR
   type BFRCR_Register is record
      --  Frame length
      FRL            : BFRCR_FRL_Field := 16#7#;
      --  Frame synchronization active level length
      FSALL          : BFRCR_FSALL_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Frame synchronization definition
      FSDEF          : Boolean := False;
      --  Frame synchronization polarity
      FSPOL          : Boolean := False;
      --  Frame synchronization offset
      FSOFF          : Boolean := False;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BFRCR_Register use record
      FRL            at 0 range 0 .. 7;
      FSALL          at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      FSDEF          at 0 range 16 .. 16;
      FSPOL          at 0 range 17 .. 17;
      FSOFF          at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype BSLOTR_FBOFF_Field is HAL.UInt5;
   subtype BSLOTR_SLOTSZ_Field is HAL.UInt2;
   subtype BSLOTR_NBSLOT_Field is HAL.UInt4;
   subtype BSLOTR_SLOTEN_Field is HAL.UInt16;

   --  BSlot register
   type BSLOTR_Register is record
      --  First bit offset
      FBOFF          : BSLOTR_FBOFF_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Slot size
      SLOTSZ         : BSLOTR_SLOTSZ_Field := 16#0#;
      --  Number of slots in an audio frame
      NBSLOT         : BSLOTR_NBSLOT_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Slot enable
      SLOTEN         : BSLOTR_SLOTEN_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BSLOTR_Register use record
      FBOFF          at 0 range 0 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      SLOTSZ         at 0 range 6 .. 7;
      NBSLOT         at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SLOTEN         at 0 range 16 .. 31;
   end record;

   --  BInterrupt mask register2
   type BIM_Register is record
      --  Overrun/underrun interrupt enable
      OVRUDRIE      : Boolean := False;
      --  Mute detection interrupt enable
      MUTEDET       : Boolean := False;
      --  Wrong clock configuration interrupt enable
      WCKCFG        : Boolean := False;
      --  FIFO request interrupt enable
      FREQIE        : Boolean := False;
      --  Codec not ready interrupt enable
      CNRDYIE       : Boolean := False;
      --  Anticipated frame synchronization detection interrupt enable
      AFSDETIE      : Boolean := False;
      --  Late frame synchronization detection interrupt enable
      LFSDETIE      : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BIM_Register use record
      OVRUDRIE      at 0 range 0 .. 0;
      MUTEDET       at 0 range 1 .. 1;
      WCKCFG        at 0 range 2 .. 2;
      FREQIE        at 0 range 3 .. 3;
      CNRDYIE       at 0 range 4 .. 4;
      AFSDETIE      at 0 range 5 .. 5;
      LFSDETIE      at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   subtype BSR_FLVL_Field is HAL.UInt3;

   --  BStatus register
   type BSR_Register is record
      --  Read-only. Overrun / underrun
      OVRUDR         : Boolean;
      --  Read-only. Mute detection
      MUTEDET        : Boolean;
      --  Read-only. Wrong clock configuration flag
      WCKCFG         : Boolean;
      --  Read-only. FIFO request
      FREQ           : Boolean;
      --  Read-only. Codec not ready
      CNRDY          : Boolean;
      --  Read-only. Anticipated frame synchronization detection
      AFSDET         : Boolean;
      --  Read-only. Late frame synchronization detection
      LFSDET         : Boolean;
      --  unspecified
      Reserved_7_15  : HAL.UInt9;
      --  Read-only. FIFO level threshold
      FLVL           : BSR_FLVL_Field;
      --  unspecified
      Reserved_19_31 : HAL.UInt13;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BSR_Register use record
      OVRUDR         at 0 range 0 .. 0;
      MUTEDET        at 0 range 1 .. 1;
      WCKCFG         at 0 range 2 .. 2;
      FREQ           at 0 range 3 .. 3;
      CNRDY          at 0 range 4 .. 4;
      AFSDET         at 0 range 5 .. 5;
      LFSDET         at 0 range 6 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      FLVL           at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  BClear flag register
   type BCLRFR_Register is record
      --  Write-only. Clear overrun / underrun
      OVRUDR        : Boolean := False;
      --  Write-only. Mute detection flag
      MUTEDET       : Boolean := False;
      --  Write-only. Clear wrong clock configuration flag
      WCKCFG        : Boolean := False;
      --  unspecified
      Reserved_3_3  : HAL.Bit := 16#0#;
      --  Write-only. Clear codec not ready flag
      CNRDY         : Boolean := False;
      --  Write-only. Clear anticipated frame synchronization detection flag
      CAFSDET       : Boolean := False;
      --  Write-only. Clear late frame synchronization detection flag
      LFSDET        : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BCLRFR_Register use record
      OVRUDR        at 0 range 0 .. 0;
      MUTEDET       at 0 range 1 .. 1;
      WCKCFG        at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      CNRDY         at 0 range 4 .. 4;
      CAFSDET       at 0 range 5 .. 5;
      LFSDET        at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Serial audio interface
   type SAI_Peripheral is record
      --  Global configuration register
      GCR    : aliased GCR_Register;
      --  AConfiguration register 1
      ACR1   : aliased ACR1_Register;
      --  AConfiguration register 2
      ACR2   : aliased ACR2_Register;
      --  AFRCR
      AFRCR  : aliased AFRCR_Register;
      --  ASlot register
      ASLOTR : aliased ASLOTR_Register;
      --  AInterrupt mask register2
      AIM    : aliased AIM_Register;
      --  AStatus register
      ASR    : aliased ASR_Register;
      --  AClear flag register
      ACLRFR : aliased ACLRFR_Register;
      --  AData register
      ADR    : aliased HAL.UInt32;
      --  BConfiguration register 1
      BCR1   : aliased BCR1_Register;
      --  BConfiguration register 2
      BCR2   : aliased BCR2_Register;
      --  BFRCR
      BFRCR  : aliased BFRCR_Register;
      --  BSlot register
      BSLOTR : aliased BSLOTR_Register;
      --  BInterrupt mask register2
      BIM    : aliased BIM_Register;
      --  BStatus register
      BSR    : aliased BSR_Register;
      --  BClear flag register
      BCLRFR : aliased BCLRFR_Register;
      --  BData register
      BDR    : aliased HAL.UInt32;
   end record
     with Volatile;

   for SAI_Peripheral use record
      GCR    at 16#0# range 0 .. 31;
      ACR1   at 16#4# range 0 .. 31;
      ACR2   at 16#8# range 0 .. 31;
      AFRCR  at 16#C# range 0 .. 31;
      ASLOTR at 16#10# range 0 .. 31;
      AIM    at 16#14# range 0 .. 31;
      ASR    at 16#18# range 0 .. 31;
      ACLRFR at 16#1C# range 0 .. 31;
      ADR    at 16#20# range 0 .. 31;
      BCR1   at 16#24# range 0 .. 31;
      BCR2   at 16#28# range 0 .. 31;
      BFRCR  at 16#2C# range 0 .. 31;
      BSLOTR at 16#30# range 0 .. 31;
      BIM    at 16#34# range 0 .. 31;
      BSR    at 16#38# range 0 .. 31;
      BCLRFR at 16#3C# range 0 .. 31;
      BDR    at 16#40# range 0 .. 31;
   end record;

   --  Serial audio interface
   SAI_Periph : aliased SAI_Peripheral
     with Import, Address => System'To_Address (16#40015800#);

end STM32_SVD.SAI;
