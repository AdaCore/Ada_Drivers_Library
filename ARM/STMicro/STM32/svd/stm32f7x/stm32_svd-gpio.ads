--  Automatically generated from CMSIS-SVD description file by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with System;

package STM32_SVD.GPIO is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype MODER_MODER0_Field is STM32_SVD.UInt2;

   type MODER_Field_Array is array (0 .. 15) of MODER_MODER0_Field
     with Component_Size => 2, Size => 32;

   --  Type definition for MODER
   type MODER_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of MODER0
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of MODER0
            Arr : MODER_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for MODER_Union use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  GPIO port mode register
   type MODER_Register is record
      --  Port x configuration bits (y = 0..15)
      MODER : MODER_Union := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MODER_Register use record
      MODER at 0 range 0 .. 31;
   end record;

   subtype OTYPER_OT0_Field is STM32_SVD.Bit;

   type OT_Field_Array is array (0 .. 15) of OTYPER_OT0_Field
     with Component_Size => 1, Size => 16;

   --  Type definition for OT
   type OT_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of OT0
            Val : STM32_SVD.Short;
         when True =>
            --  Array vision of OT0
            Arr : OT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for OT_Union use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  GPIO port output type register
   type OTYPER_Register is record
      --  Port x configuration bits (y = 0..15)
      OT             : OT_Union := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTYPER_Register use record
      OT             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype GPIOB_OSPEEDR_OSPEEDR0_Field is STM32_SVD.UInt2;

   type OSPEEDR_Field_Array is array (0 .. 15) of GPIOB_OSPEEDR_OSPEEDR0_Field
     with Component_Size => 2, Size => 32;

   --  Type definition for OSPEEDR
   type OSPEEDR_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of OSPEEDR0
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of OSPEEDR0
            Arr : OSPEEDR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for OSPEEDR_Union use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  GPIO port output speed register
   type GPIOB_OSPEEDR_Register is record
      --  Port x configuration bits (y = 0..15)
      OSPEEDR : OSPEEDR_Union := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPIOB_OSPEEDR_Register use record
      OSPEEDR at 0 range 0 .. 31;
   end record;

   subtype PUPDR_PUPDR0_Field is STM32_SVD.UInt2;

   type PUPDR_Field_Array is array (0 .. 15) of PUPDR_PUPDR0_Field
     with Component_Size => 2, Size => 32;

   --  Type definition for PUPDR
   type PUPDR_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of PUPDR0
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of PUPDR0
            Arr : PUPDR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for PUPDR_Union use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  GPIO port pull-up/pull-down register
   type PUPDR_Register is record
      --  Port x configuration bits (y = 0..15)
      PUPDR : PUPDR_Union := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PUPDR_Register use record
      PUPDR at 0 range 0 .. 31;
   end record;

   subtype IDR_IDR0_Field is STM32_SVD.Bit;

   type IDR_Field_Array is array (0 .. 15) of IDR_IDR0_Field
     with Component_Size => 1, Size => 16;

   --  Type definition for IDR
   type IDR_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of IDR0
            Val : STM32_SVD.Short;
         when True =>
            --  Array vision of IDR0
            Arr : IDR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for IDR_Union use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  GPIO port input data register
   type IDR_Register is record
      --  Port input data (y = 0..15)
      IDR            : IDR_Union := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDR_Register use record
      IDR            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype ODR_ODR0_Field is STM32_SVD.Bit;

   type ODR_Field_Array is array (0 .. 15) of ODR_ODR0_Field
     with Component_Size => 1, Size => 16;

   --  Type definition for ODR
   type ODR_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of ODR0
            Val : STM32_SVD.Short;
         when True =>
            --  Array vision of ODR0
            Arr : ODR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for ODR_Union use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  GPIO port output data register
   type ODR_Register is record
      --  Port output data (y = 0..15)
      ODR            : ODR_Union := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ODR_Register use record
      ODR            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype BSRR_BS0_Field is STM32_SVD.Bit;

   type BS_Field_Array is array (0 .. 15) of BSRR_BS0_Field
     with Component_Size => 1, Size => 16;

   --  Type definition for BS
   type BS_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of BS0
            Val : STM32_SVD.Short;
         when True =>
            --  Array vision of BS0
            Arr : BS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for BS_Union use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   subtype BSRR_BR0_Field is STM32_SVD.Bit;

   type BR_Field_Array is array (0 .. 15) of BSRR_BR0_Field
     with Component_Size => 1, Size => 16;

   --  Type definition for BR
   type BR_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of BR0
            Val : STM32_SVD.Short;
         when True =>
            --  Array vision of BR0
            Arr : BR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for BR_Union use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  GPIO port bit set/reset register
   type BSRR_Register is record
      --  Port x set bit y (y= 0..15)
      BS : BS_Union := (As_Array => False, Val => 16#0#);
      --  Port x set bit y (y= 0..15)
      BR : BR_Union := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BSRR_Register use record
      BS at 0 range 0 .. 15;
      BR at 0 range 16 .. 31;
   end record;

   subtype LCKR_LCK0_Field is STM32_SVD.Bit;

   type LCK_Field_Array is array (0 .. 15) of LCKR_LCK0_Field
     with Component_Size => 1, Size => 16;

   --  Type definition for LCK
   type LCK_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of LCK0
            Val : STM32_SVD.Short;
         when True =>
            --  Array vision of LCK0
            Arr : LCK_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for LCK_Union use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   subtype LCKR_LCKK_Field is STM32_SVD.Bit;

   --  GPIO port configuration lock register
   type LCKR_Register is record
      --  Port x lock bit y (y= 0..15)
      LCK            : LCK_Union := (As_Array => False, Val => 16#0#);
      --  Port x lock bit y (y= 0..15)
      LCKK           : LCKR_LCKK_Field := 16#0#;
      --  unspecified
      Reserved_17_31 : STM32_SVD.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LCKR_Register use record
      LCK            at 0 range 0 .. 15;
      LCKK           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype AFRL_AFRL0_Field is STM32_SVD.UInt4;

   type AFRL_Field_Array is array (0 .. 7) of AFRL_AFRL0_Field
     with Component_Size => 4, Size => 32;

   --  Type definition for AFRL
   type AFRL_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of AFRL0
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of AFRL0
            Arr : AFRL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for AFRL_Union use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  GPIO alternate function lowregister
   type AFRL_Register is record
      --  Alternate function selection for port x bit y (y = 0..7)
      AFRL : AFRL_Union := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AFRL_Register use record
      AFRL at 0 range 0 .. 31;
   end record;

   subtype AFRH_AFRH8_Field is STM32_SVD.UInt4;

   type AFRH_Field_Array is array (0 .. 7) of AFRH_AFRH8_Field
     with Component_Size => 4, Size => 32;

   --  Type definition for AFRH
   type AFRH_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of AFRH8
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of AFRH8
            Arr : AFRH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for AFRH_Union use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  GPIO alternate function high register
   type AFRH_Register is record
      --  Alternate function selection for port x bit y (y = 8..15)
      AFRH : AFRH_Union := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AFRH_Register use record
      AFRH at 0 range 0 .. 31;
   end record;

   subtype BRR_BR0_Field is STM32_SVD.Bit;

   type BR_Field_Array_1 is array (0 .. 15) of BRR_BR0_Field
     with Component_Size => 1, Size => 16;

   --  Type definition for BR
   type BR_Union_1 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of BR0
            Val : STM32_SVD.Short;
         when True =>
            --  Array vision of BR0
            Arr : BR_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for BR_Union_1 use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  GPIO port bit reset register
   type BRR_Register is record
      --  Port D Reset bit 0
      BR             : BR_Union_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BRR_Register use record
      BR             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  General-purpose I/Os
   type GPIO_Peripheral is record
      --  GPIO port mode register
      MODER         : MODER_Register;
      --  GPIO port output type register
      OTYPER        : OTYPER_Register;
      --  GPIO port output speed register
      GPIOB_OSPEEDR : GPIOB_OSPEEDR_Register;
      --  GPIO port pull-up/pull-down register
      PUPDR         : PUPDR_Register;
      --  GPIO port input data register
      IDR           : IDR_Register;
      --  GPIO port output data register
      ODR           : ODR_Register;
      --  GPIO port bit set/reset register
      BSRR          : BSRR_Register;
      --  GPIO port configuration lock register
      LCKR          : LCKR_Register;
      --  GPIO alternate function low register
      AFRL          : AFRL_Register;
      --  GPIO alternate function high register
      AFRH          : AFRH_Register;
      --  GPIO port bit reset register
      BRR           : BRR_Register;
   end record
     with Volatile;

   for GPIO_Peripheral use record
      MODER         at 0 range 0 .. 31;
      OTYPER        at 4 range 0 .. 31;
      GPIOB_OSPEEDR at 8 range 0 .. 31;
      PUPDR         at 12 range 0 .. 31;
      IDR           at 16 range 0 .. 31;
      ODR           at 20 range 0 .. 31;
      BSRR          at 24 range 0 .. 31;
      LCKR          at 28 range 0 .. 31;
      AFRL          at 32 range 0 .. 31;
      AFRH          at 36 range 0 .. 31;
      BRR           at 40 range 0 .. 31;
   end record;

   --  General-purpose I/Os
   GPIOA_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address(16#40020000#);

   --  General-purpose I/Os
   GPIOB_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address(16#40020400#);

   --  General-purpose I/Os
   GPIOC_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address(16#40020800#);

   --  General-purpose I/Os
   GPIOD_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address(16#40020C00#);

   --  General-purpose I/Os
   GPIOE_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address(16#40021000#);

   --  General-purpose I/Os
   GPIOF_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address(16#40021400#);

   --  General-purpose I/Os
   GPIOG_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address(16#40021800#);

   --  General-purpose I/Os
   GPIOH_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address(16#40021C00#);

   --  General-purpose I/Os
   GPIOI_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address(16#40022000#);

   --  General-purpose I/Os
   GPIOJ_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address(16#40022400#);

   --  General-purpose I/Os
   GPIOK_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address(16#40022800#);

end STM32_SVD.GPIO;
