--  This spec has been automatically generated from STM32F7x9.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with HAL;
with System;

package STM32_SVD.HASH is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR_DATATYPE_Field is HAL.UInt2;
   subtype CR_NBW_Field is HAL.UInt4;

   --  control register
   type CR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Write-only. Initialize message digest calculation
      INIT           : Boolean := False;
      --  DMA enable
      DMAE           : Boolean := False;
      --  Data type selection
      DATATYPE       : CR_DATATYPE_Field := 16#0#;
      --  Mode selection
      MODE           : Boolean := False;
      --  Algorithm selection
      ALGO0          : Boolean := False;
      --  Read-only. Number of words already pushed
      NBW            : CR_NBW_Field := 16#0#;
      --  Read-only. DIN not empty
      DINNE          : Boolean := False;
      --  Multiple DMA Transfers
      MDMAT          : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Long key selection
      LKEY           : Boolean := False;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  ALGO
      ALGO1          : Boolean := False;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
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

   subtype STR_NBLW_Field is HAL.UInt5;

   --  start register
   type STR_Register is record
      --  Number of valid bits in the last word of the message
      NBLW          : STR_NBLW_Field := 16#0#;
      --  unspecified
      Reserved_5_7  : HAL.UInt3 := 16#0#;
      --  Write-only. Digest calculation
      DCAL          : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STR_Register use record
      NBLW          at 0 range 0 .. 4;
      Reserved_5_7  at 0 range 5 .. 7;
      DCAL          at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  interrupt enable register
   type IMR_Register is record
      --  Data input interrupt enable
      DINIE         : Boolean := False;
      --  Digest calculation completion interrupt enable
      DCIE          : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMR_Register use record
      DINIE         at 0 range 0 .. 0;
      DCIE          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  status register
   type SR_Register is record
      --  Data input interrupt status
      DINIS         : Boolean := True;
      --  Digest calculation completion interrupt status
      DCIS          : Boolean := False;
      --  Read-only. DMA Status
      DMAS          : Boolean := False;
      --  Read-only. Busy bit
      BUSY          : Boolean := False;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
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
      DIN      : HAL.Word;
      --  start register
      STR      : STR_Register;
      --  digest registers
      HR0      : HAL.Word;
      --  digest registers
      HR1      : HAL.Word;
      --  digest registers
      HR2      : HAL.Word;
      --  digest registers
      HR3      : HAL.Word;
      --  digest registers
      HR4      : HAL.Word;
      --  interrupt enable register
      IMR      : IMR_Register;
      --  status register
      SR       : SR_Register;
      --  context swap registers
      CSR0     : HAL.Word;
      --  context swap registers
      CSR1     : HAL.Word;
      --  context swap registers
      CSR2     : HAL.Word;
      --  context swap registers
      CSR3     : HAL.Word;
      --  context swap registers
      CSR4     : HAL.Word;
      --  context swap registers
      CSR5     : HAL.Word;
      --  context swap registers
      CSR6     : HAL.Word;
      --  context swap registers
      CSR7     : HAL.Word;
      --  context swap registers
      CSR8     : HAL.Word;
      --  context swap registers
      CSR9     : HAL.Word;
      --  context swap registers
      CSR10    : HAL.Word;
      --  context swap registers
      CSR11    : HAL.Word;
      --  context swap registers
      CSR12    : HAL.Word;
      --  context swap registers
      CSR13    : HAL.Word;
      --  context swap registers
      CSR14    : HAL.Word;
      --  context swap registers
      CSR15    : HAL.Word;
      --  context swap registers
      CSR16    : HAL.Word;
      --  context swap registers
      CSR17    : HAL.Word;
      --  context swap registers
      CSR18    : HAL.Word;
      --  context swap registers
      CSR19    : HAL.Word;
      --  context swap registers
      CSR20    : HAL.Word;
      --  context swap registers
      CSR21    : HAL.Word;
      --  context swap registers
      CSR22    : HAL.Word;
      --  context swap registers
      CSR23    : HAL.Word;
      --  context swap registers
      CSR24    : HAL.Word;
      --  context swap registers
      CSR25    : HAL.Word;
      --  context swap registers
      CSR26    : HAL.Word;
      --  context swap registers
      CSR27    : HAL.Word;
      --  context swap registers
      CSR28    : HAL.Word;
      --  context swap registers
      CSR29    : HAL.Word;
      --  context swap registers
      CSR30    : HAL.Word;
      --  context swap registers
      CSR31    : HAL.Word;
      --  context swap registers
      CSR32    : HAL.Word;
      --  context swap registers
      CSR33    : HAL.Word;
      --  context swap registers
      CSR34    : HAL.Word;
      --  context swap registers
      CSR35    : HAL.Word;
      --  context swap registers
      CSR36    : HAL.Word;
      --  context swap registers
      CSR37    : HAL.Word;
      --  context swap registers
      CSR38    : HAL.Word;
      --  context swap registers
      CSR39    : HAL.Word;
      --  context swap registers
      CSR40    : HAL.Word;
      --  context swap registers
      CSR41    : HAL.Word;
      --  context swap registers
      CSR42    : HAL.Word;
      --  context swap registers
      CSR43    : HAL.Word;
      --  context swap registers
      CSR44    : HAL.Word;
      --  context swap registers
      CSR45    : HAL.Word;
      --  context swap registers
      CSR46    : HAL.Word;
      --  context swap registers
      CSR47    : HAL.Word;
      --  context swap registers
      CSR48    : HAL.Word;
      --  context swap registers
      CSR49    : HAL.Word;
      --  context swap registers
      CSR50    : HAL.Word;
      --  context swap registers
      CSR51    : HAL.Word;
      --  context swap registers
      CSR52    : HAL.Word;
      --  context swap registers
      CSR53    : HAL.Word;
      --  HASH digest register
      HASH_HR0 : HAL.Word;
      --  read-only
      HASH_HR1 : HAL.Word;
      --  read-only
      HASH_HR2 : HAL.Word;
      --  read-only
      HASH_HR3 : HAL.Word;
      --  read-only
      HASH_HR4 : HAL.Word;
      --  read-only
      HASH_HR5 : HAL.Word;
      --  read-only
      HASH_HR6 : HAL.Word;
      --  read-only
      HASH_HR7 : HAL.Word;
   end record
     with Volatile;

   for HASH_Peripheral use record
      CR       at 16#0# range 0 .. 31;
      DIN      at 16#4# range 0 .. 31;
      STR      at 16#8# range 0 .. 31;
      HR0      at 16#C# range 0 .. 31;
      HR1      at 16#10# range 0 .. 31;
      HR2      at 16#14# range 0 .. 31;
      HR3      at 16#18# range 0 .. 31;
      HR4      at 16#1C# range 0 .. 31;
      IMR      at 16#20# range 0 .. 31;
      SR       at 16#24# range 0 .. 31;
      CSR0     at 16#F8# range 0 .. 31;
      CSR1     at 16#FC# range 0 .. 31;
      CSR2     at 16#100# range 0 .. 31;
      CSR3     at 16#104# range 0 .. 31;
      CSR4     at 16#108# range 0 .. 31;
      CSR5     at 16#10C# range 0 .. 31;
      CSR6     at 16#110# range 0 .. 31;
      CSR7     at 16#114# range 0 .. 31;
      CSR8     at 16#118# range 0 .. 31;
      CSR9     at 16#11C# range 0 .. 31;
      CSR10    at 16#120# range 0 .. 31;
      CSR11    at 16#124# range 0 .. 31;
      CSR12    at 16#128# range 0 .. 31;
      CSR13    at 16#12C# range 0 .. 31;
      CSR14    at 16#130# range 0 .. 31;
      CSR15    at 16#134# range 0 .. 31;
      CSR16    at 16#138# range 0 .. 31;
      CSR17    at 16#13C# range 0 .. 31;
      CSR18    at 16#140# range 0 .. 31;
      CSR19    at 16#144# range 0 .. 31;
      CSR20    at 16#148# range 0 .. 31;
      CSR21    at 16#14C# range 0 .. 31;
      CSR22    at 16#150# range 0 .. 31;
      CSR23    at 16#154# range 0 .. 31;
      CSR24    at 16#158# range 0 .. 31;
      CSR25    at 16#15C# range 0 .. 31;
      CSR26    at 16#160# range 0 .. 31;
      CSR27    at 16#164# range 0 .. 31;
      CSR28    at 16#168# range 0 .. 31;
      CSR29    at 16#16C# range 0 .. 31;
      CSR30    at 16#170# range 0 .. 31;
      CSR31    at 16#174# range 0 .. 31;
      CSR32    at 16#178# range 0 .. 31;
      CSR33    at 16#17C# range 0 .. 31;
      CSR34    at 16#180# range 0 .. 31;
      CSR35    at 16#184# range 0 .. 31;
      CSR36    at 16#188# range 0 .. 31;
      CSR37    at 16#18C# range 0 .. 31;
      CSR38    at 16#190# range 0 .. 31;
      CSR39    at 16#194# range 0 .. 31;
      CSR40    at 16#198# range 0 .. 31;
      CSR41    at 16#19C# range 0 .. 31;
      CSR42    at 16#1A0# range 0 .. 31;
      CSR43    at 16#1A4# range 0 .. 31;
      CSR44    at 16#1A8# range 0 .. 31;
      CSR45    at 16#1AC# range 0 .. 31;
      CSR46    at 16#1B0# range 0 .. 31;
      CSR47    at 16#1B4# range 0 .. 31;
      CSR48    at 16#1B8# range 0 .. 31;
      CSR49    at 16#1BC# range 0 .. 31;
      CSR50    at 16#1C0# range 0 .. 31;
      CSR51    at 16#1C4# range 0 .. 31;
      CSR52    at 16#1C8# range 0 .. 31;
      CSR53    at 16#1CC# range 0 .. 31;
      HASH_HR0 at 16#310# range 0 .. 31;
      HASH_HR1 at 16#314# range 0 .. 31;
      HASH_HR2 at 16#318# range 0 .. 31;
      HASH_HR3 at 16#31C# range 0 .. 31;
      HASH_HR4 at 16#320# range 0 .. 31;
      HASH_HR5 at 16#324# range 0 .. 31;
      HASH_HR6 at 16#328# range 0 .. 31;
      HASH_HR7 at 16#32C# range 0 .. 31;
   end record;

   --  Hash processor
   HASH_Periph : aliased HASH_Peripheral
     with Import, Address => HASH_Base;

end STM32_SVD.HASH;
