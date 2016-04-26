--  This spec has been automatically generated from STM32F429x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with HAL;
with System;

package STM32_SVD.SYSCFG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --------------------
   -- MEMRM_Register --
   --------------------

   subtype MEMRM_MEM_MODE_Field is HAL.UInt3;
   subtype MEMRM_SWP_FMC_Field is HAL.UInt2;

   --  memory remap register
   type MEMRM_Register is record
      --  Memory mapping selection
      MEM_MODE       : MEMRM_MEM_MODE_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  Flash bank mode selection
      FB_MODE        : Boolean := False;
      --  unspecified
      Reserved_9_9   : HAL.Bit := 16#0#;
      --  FMC memory mapping swap
      SWP_FMC        : MEMRM_SWP_FMC_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MEMRM_Register use record
      MEM_MODE       at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      FB_MODE        at 0 range 8 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      SWP_FMC        at 0 range 10 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   ------------------
   -- PMC_Register --
   ------------------

   --  peripheral mode configuration register
   type PMC_Register is record
      --  unspecified
      Reserved_0_15  : HAL.Short := 16#0#;
      --  ADC1DC2
      ADC1DC2        : Boolean := False;
      --  ADC2DC2
      ADC2DC2        : Boolean := False;
      --  ADC3DC2
      ADC3DC2        : Boolean := False;
      --  unspecified
      Reserved_19_22 : HAL.UInt4 := 16#0#;
      --  Ethernet PHY interface selection
      MII_RMII_SEL   : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMC_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      ADC1DC2        at 0 range 16 .. 16;
      ADC2DC2        at 0 range 17 .. 17;
      ADC3DC2        at 0 range 18 .. 18;
      Reserved_19_22 at 0 range 19 .. 22;
      MII_RMII_SEL   at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   ---------------------
   -- EXTICR_Register --
   ---------------------

   ------------------
   -- EXTICR1.EXTI --
   ------------------

   --  EXTICR1_EXTI array element
   subtype EXTICR1_EXTI_Element is HAL.UInt4;

   --  EXTICR1_EXTI array
   type EXTICR1_EXTI_Field_Array is array (0 .. 3) of EXTICR1_EXTI_Element
     with Component_Size => 4, Size => 16;

   --  Type definition for EXTICR1_EXTI
   type EXTICR1_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : HAL.Short;
         when True =>
            --  EXTI as an array
            Arr : EXTICR1_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for EXTICR1_EXTI_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  external interrupt configuration register 1
   type EXTICR_Register is record
      --  EXTI x configuration (x = 0 to 3)
      EXTI           : EXTICR1_EXTI_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR_Register use record
      EXTI           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --------------------
   -- CMPCR_Register --
   --------------------

   --  Compensation cell control register
   type CMPCR_Register is record
      --  Read-only. Compensation cell power-down
      CMP_PD        : Boolean := False;
      --  unspecified
      Reserved_1_7  : HAL.UInt7;
      --  Read-only. READY
      READY         : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMPCR_Register use record
      CMP_PD        at 0 range 0 .. 0;
      Reserved_1_7  at 0 range 1 .. 7;
      READY         at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  System configuration controller
   type SYSCFG_Peripheral is record
      --  memory remap register
      MEMRM   : MEMRM_Register;
      --  peripheral mode configuration register
      PMC     : PMC_Register;
      --  external interrupt configuration register 1
      EXTICR1 : EXTICR_Register;
      --  external interrupt configuration register 2
      EXTICR2 : EXTICR_Register;
      --  external interrupt configuration register 3
      EXTICR3 : EXTICR_Register;
      --  external interrupt configuration register 4
      EXTICR4 : EXTICR_Register;
      --  Compensation cell control register
      CMPCR   : CMPCR_Register;
   end record
     with Volatile;

   for SYSCFG_Peripheral use record
      MEMRM   at 0 range 0 .. 31;
      PMC     at 4 range 0 .. 31;
      EXTICR1 at 8 range 0 .. 31;
      EXTICR2 at 12 range 0 .. 31;
      EXTICR3 at 16 range 0 .. 31;
      EXTICR4 at 20 range 0 .. 31;
      CMPCR   at 32 range 0 .. 31;
   end record;

   --  System configuration controller
   SYSCFG_Periph : aliased SYSCFG_Peripheral
     with Import, Address => SYSCFG_Base;

end STM32_SVD.SYSCFG;
