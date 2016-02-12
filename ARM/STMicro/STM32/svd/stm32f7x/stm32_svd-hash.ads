--  Automatically generated from STM32F7x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with STM32_SVD;
with System;

package STM32_SVD.HASH is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -----------------
   -- CR_Register --
   -----------------

   subtype CR_INIT_Field is STM32_SVD.Bit;
   subtype CR_DMAE_Field is STM32_SVD.Bit;
   subtype CR_DATATYPE_Field is STM32_SVD.UInt2;
   subtype CR_MODE_Field is STM32_SVD.Bit;
   subtype CR_ALGO0_Field is STM32_SVD.Bit;
   subtype CR_NBW_Field is STM32_SVD.UInt4;
   subtype CR_DINNE_Field is STM32_SVD.Bit;
   subtype CR_MDMAT_Field is STM32_SVD.Bit;
   subtype CR_LKEY_Field is STM32_SVD.Bit;
   subtype CR_ALGO1_Field is STM32_SVD.Bit;

   --  control register
   type CR_Register is record
      --  unspecified
      Reserved_0_1   : STM32_SVD.UInt2 := 16#0#;
      --  Initialize message digest calculation
      INIT           : CR_INIT_Field := 16#0#;
      --  DMA enable
      DMAE           : CR_DMAE_Field := 16#0#;
      --  Data type selection
      DATATYPE       : CR_DATATYPE_Field := 16#0#;
      --  Mode selection
      MODE           : CR_MODE_Field := 16#0#;
      --  Algorithm selection
      ALGO0          : CR_ALGO0_Field := 16#0#;
      --  Number of words already pushed
      NBW            : CR_NBW_Field := 16#0#;
      --  DIN not empty
      DINNE          : CR_DINNE_Field := 16#0#;
      --  Multiple DMA Transfers
      MDMAT          : CR_MDMAT_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : STM32_SVD.UInt2 := 16#0#;
      --  Long key selection
      LKEY           : CR_LKEY_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32_SVD.Bit := 16#0#;
      --  ALGO
      ALGO1          : CR_ALGO1_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : STM32_SVD.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      INIT           at 0 range 2 .. 2;
      DMAE           at 0 range 3 .. 3;
      DATATYPE       at 0 range 4 .. 5;
      MODE           at 0 range 6 .. 6;
      ALGO0          at 0 range 7 .. 7;
      NBW            at 0 range 8 .. 11;
      DINNE          at 0 range 12 .. 12;
      MDMAT          at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      LKEY           at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      ALGO1          at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   ------------------
   -- STR_Register --
   ------------------

   subtype STR_NBLW_Field is STM32_SVD.UInt5;
   subtype STR_DCAL_Field is STM32_SVD.Bit;

   --  start register
   type STR_Register is record
      --  Number of valid bits in the last word of the message
      NBLW          : STR_NBLW_Field := 16#0#;
      --  unspecified
      Reserved_5_7  : STM32_SVD.UInt3 := 16#0#;
      --  Digest calculation
      DCAL          : STR_DCAL_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : STM32_SVD.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STR_Register use record
      NBLW          at 0 range 0 .. 4;
      Reserved_5_7  at 0 range 5 .. 7;
      DCAL          at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   ------------------
   -- IMR_Register --
   ------------------

   subtype IMR_DINIE_Field is STM32_SVD.Bit;
   subtype IMR_DCIE_Field is STM32_SVD.Bit;

   --  interrupt enable register
   type IMR_Register is record
      --  Data input interrupt enable
      DINIE         : IMR_DINIE_Field := 16#0#;
      --  Digest calculation completion interrupt enable
      DCIE          : IMR_DCIE_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : STM32_SVD.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMR_Register use record
      DINIE         at 0 range 0 .. 0;
      DCIE          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   -----------------
   -- SR_Register --
   -----------------

   subtype SR_DINIS_Field is STM32_SVD.Bit;
   subtype SR_DCIS_Field is STM32_SVD.Bit;
   subtype SR_DMAS_Field is STM32_SVD.Bit;
   subtype SR_BUSY_Field is STM32_SVD.Bit;

   --  status register
   type SR_Register is record
      --  Data input interrupt status
      DINIS         : SR_DINIS_Field := 16#1#;
      --  Digest calculation completion interrupt status
      DCIS          : SR_DCIS_Field := 16#0#;
      --  DMA Status
      DMAS          : SR_DMAS_Field := 16#0#;
      --  Busy bit
      BUSY          : SR_BUSY_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : STM32_SVD.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      DINIS         at 0 range 0 .. 0;
      DCIS          at 0 range 1 .. 1;
      DMAS          at 0 range 2 .. 2;
      BUSY          at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Hash processor
   type HASH_Peripheral is record
      --  control register
      CR       : CR_Register;
      --  data input register
      DIN      : STM32_SVD.Word;
      --  start register
      STR      : STR_Register;
      --  digest registers
      HR0      : STM32_SVD.Word;
      --  digest registers
      HR1      : STM32_SVD.Word;
      --  digest registers
      HR2      : STM32_SVD.Word;
      --  digest registers
      HR3      : STM32_SVD.Word;
      --  digest registers
      HR4      : STM32_SVD.Word;
      --  interrupt enable register
      IMR      : IMR_Register;
      --  status register
      SR       : SR_Register;
      --  context swap registers
      CSR0     : STM32_SVD.Word;
      --  context swap registers
      CSR1     : STM32_SVD.Word;
      --  context swap registers
      CSR2     : STM32_SVD.Word;
      --  context swap registers
      CSR3     : STM32_SVD.Word;
      --  context swap registers
      CSR4     : STM32_SVD.Word;
      --  context swap registers
      CSR5     : STM32_SVD.Word;
      --  context swap registers
      CSR6     : STM32_SVD.Word;
      --  context swap registers
      CSR7     : STM32_SVD.Word;
      --  context swap registers
      CSR8     : STM32_SVD.Word;
      --  context swap registers
      CSR9     : STM32_SVD.Word;
      --  context swap registers
      CSR10    : STM32_SVD.Word;
      --  context swap registers
      CSR11    : STM32_SVD.Word;
      --  context swap registers
      CSR12    : STM32_SVD.Word;
      --  context swap registers
      CSR13    : STM32_SVD.Word;
      --  context swap registers
      CSR14    : STM32_SVD.Word;
      --  context swap registers
      CSR15    : STM32_SVD.Word;
      --  context swap registers
      CSR16    : STM32_SVD.Word;
      --  context swap registers
      CSR17    : STM32_SVD.Word;
      --  context swap registers
      CSR18    : STM32_SVD.Word;
      --  context swap registers
      CSR19    : STM32_SVD.Word;
      --  context swap registers
      CSR20    : STM32_SVD.Word;
      --  context swap registers
      CSR21    : STM32_SVD.Word;
      --  context swap registers
      CSR22    : STM32_SVD.Word;
      --  context swap registers
      CSR23    : STM32_SVD.Word;
      --  context swap registers
      CSR24    : STM32_SVD.Word;
      --  context swap registers
      CSR25    : STM32_SVD.Word;
      --  context swap registers
      CSR26    : STM32_SVD.Word;
      --  context swap registers
      CSR27    : STM32_SVD.Word;
      --  context swap registers
      CSR28    : STM32_SVD.Word;
      --  context swap registers
      CSR29    : STM32_SVD.Word;
      --  context swap registers
      CSR30    : STM32_SVD.Word;
      --  context swap registers
      CSR31    : STM32_SVD.Word;
      --  context swap registers
      CSR32    : STM32_SVD.Word;
      --  context swap registers
      CSR33    : STM32_SVD.Word;
      --  context swap registers
      CSR34    : STM32_SVD.Word;
      --  context swap registers
      CSR35    : STM32_SVD.Word;
      --  context swap registers
      CSR36    : STM32_SVD.Word;
      --  context swap registers
      CSR37    : STM32_SVD.Word;
      --  context swap registers
      CSR38    : STM32_SVD.Word;
      --  context swap registers
      CSR39    : STM32_SVD.Word;
      --  context swap registers
      CSR40    : STM32_SVD.Word;
      --  context swap registers
      CSR41    : STM32_SVD.Word;
      --  context swap registers
      CSR42    : STM32_SVD.Word;
      --  context swap registers
      CSR43    : STM32_SVD.Word;
      --  context swap registers
      CSR44    : STM32_SVD.Word;
      --  context swap registers
      CSR45    : STM32_SVD.Word;
      --  context swap registers
      CSR46    : STM32_SVD.Word;
      --  context swap registers
      CSR47    : STM32_SVD.Word;
      --  context swap registers
      CSR48    : STM32_SVD.Word;
      --  context swap registers
      CSR49    : STM32_SVD.Word;
      --  context swap registers
      CSR50    : STM32_SVD.Word;
      --  context swap registers
      CSR51    : STM32_SVD.Word;
      --  context swap registers
      CSR52    : STM32_SVD.Word;
      --  context swap registers
      CSR53    : STM32_SVD.Word;
      --  HASH digest register
      HASH_HR0 : STM32_SVD.Word;
      --  read-only
      HASH_HR1 : STM32_SVD.Word;
      --  read-only
      HASH_HR2 : STM32_SVD.Word;
      --  read-only
      HASH_HR3 : STM32_SVD.Word;
      --  read-only
      HASH_HR4 : STM32_SVD.Word;
      --  read-only
      HASH_HR5 : STM32_SVD.Word;
      --  read-only
      HASH_HR6 : STM32_SVD.Word;
      --  read-only
      HASH_HR7 : STM32_SVD.Word;
   end record
     with Volatile;

   for HASH_Peripheral use record
      CR       at 0 range 0 .. 31;
      DIN      at 4 range 0 .. 31;
      STR      at 8 range 0 .. 31;
      HR0      at 12 range 0 .. 31;
      HR1      at 16 range 0 .. 31;
      HR2      at 20 range 0 .. 31;
      HR3      at 24 range 0 .. 31;
      HR4      at 28 range 0 .. 31;
      IMR      at 32 range 0 .. 31;
      SR       at 36 range 0 .. 31;
      CSR0     at 248 range 0 .. 31;
      CSR1     at 252 range 0 .. 31;
      CSR2     at 256 range 0 .. 31;
      CSR3     at 260 range 0 .. 31;
      CSR4     at 264 range 0 .. 31;
      CSR5     at 268 range 0 .. 31;
      CSR6     at 272 range 0 .. 31;
      CSR7     at 276 range 0 .. 31;
      CSR8     at 280 range 0 .. 31;
      CSR9     at 284 range 0 .. 31;
      CSR10    at 288 range 0 .. 31;
      CSR11    at 292 range 0 .. 31;
      CSR12    at 296 range 0 .. 31;
      CSR13    at 300 range 0 .. 31;
      CSR14    at 304 range 0 .. 31;
      CSR15    at 308 range 0 .. 31;
      CSR16    at 312 range 0 .. 31;
      CSR17    at 316 range 0 .. 31;
      CSR18    at 320 range 0 .. 31;
      CSR19    at 324 range 0 .. 31;
      CSR20    at 328 range 0 .. 31;
      CSR21    at 332 range 0 .. 31;
      CSR22    at 336 range 0 .. 31;
      CSR23    at 340 range 0 .. 31;
      CSR24    at 344 range 0 .. 31;
      CSR25    at 348 range 0 .. 31;
      CSR26    at 352 range 0 .. 31;
      CSR27    at 356 range 0 .. 31;
      CSR28    at 360 range 0 .. 31;
      CSR29    at 364 range 0 .. 31;
      CSR30    at 368 range 0 .. 31;
      CSR31    at 372 range 0 .. 31;
      CSR32    at 376 range 0 .. 31;
      CSR33    at 380 range 0 .. 31;
      CSR34    at 384 range 0 .. 31;
      CSR35    at 388 range 0 .. 31;
      CSR36    at 392 range 0 .. 31;
      CSR37    at 396 range 0 .. 31;
      CSR38    at 400 range 0 .. 31;
      CSR39    at 404 range 0 .. 31;
      CSR40    at 408 range 0 .. 31;
      CSR41    at 412 range 0 .. 31;
      CSR42    at 416 range 0 .. 31;
      CSR43    at 420 range 0 .. 31;
      CSR44    at 424 range 0 .. 31;
      CSR45    at 428 range 0 .. 31;
      CSR46    at 432 range 0 .. 31;
      CSR47    at 436 range 0 .. 31;
      CSR48    at 440 range 0 .. 31;
      CSR49    at 444 range 0 .. 31;
      CSR50    at 448 range 0 .. 31;
      CSR51    at 452 range 0 .. 31;
      CSR52    at 456 range 0 .. 31;
      CSR53    at 460 range 0 .. 31;
      HASH_HR0 at 784 range 0 .. 31;
      HASH_HR1 at 788 range 0 .. 31;
      HASH_HR2 at 792 range 0 .. 31;
      HASH_HR3 at 796 range 0 .. 31;
      HASH_HR4 at 800 range 0 .. 31;
      HASH_HR5 at 804 range 0 .. 31;
      HASH_HR6 at 808 range 0 .. 31;
      HASH_HR7 at 812 range 0 .. 31;
   end record;

   --  Hash processor
   HASH_Periph : aliased HASH_Peripheral
     with Import, Address => System'To_Address (16#50060400#);

end STM32_SVD.HASH;
