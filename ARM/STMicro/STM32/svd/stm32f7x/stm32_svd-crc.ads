--  Automatically generated from CMSIS-SVD description file
pragma Restrictions (No_Elaboration_Code);

with System;

package STM32_SVD.CRC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype IDR_IDR_Field is STM32_SVD.Byte;

   --  Independent Data register
   type IDR_Register is record
      --  Independent Data register
      IDR           : IDR_IDR_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDR_Register use record
      IDR           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CR_CR_Field is STM32_SVD.Bit;

   --  Control register
   type CR_Register is record
      --  Control regidter
      CR            : CR_CR_Field := 16#0#;
      --  unspecified
      Reserved_1_31 : STM32_SVD.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      CR            at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Cryptographic processor
   type CRC_Peripheral is record
      --  Data register
      DR   : STM32_SVD.Word;
      --  Independent Data register
      IDR  : IDR_Register;
      --  Control register
      CR   : CR_Register;
      --  Initial CRC value
      INIT : STM32_SVD.Word;
      --  CRC polynomial
      POL  : STM32_SVD.Word;
   end record
     with Volatile;

   for CRC_Peripheral use record
      DR   at 0 range 0 .. 31;
      IDR  at 4 range 0 .. 31;
      CR   at 8 range 0 .. 31;
      INIT at 12 range 0 .. 31;
      POL  at 16 range 0 .. 31;
   end record;

   --  Cryptographic processor
   CRC_Periph : aliased CRC_Peripheral
     with Import, Address => System'To_Address(16#40023000#);

end STM32_SVD.CRC;
