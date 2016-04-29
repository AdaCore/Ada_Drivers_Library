--  This spec has been automatically generated from STM32F7x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with HAL;
with System;

package STM32_SVD.NVIC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -------------------
   -- ICTR_Register --
   -------------------

   subtype ICTR_INTLINESNUM_Field is HAL.UInt4;

   --  Interrupt Controller Type Register
   type ICTR_Register is record
      --  Read-only. Total number of interrupt lines in groups
      INTLINESNUM   : ICTR_INTLINESNUM_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICTR_Register use record
      INTLINESNUM   at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   ------------------
   -- IPR_Register --
   ------------------

   --  IPR0_IPR_N array element
   subtype IPR0_IPR_N_Element is HAL.Byte;

   --  IPR0_IPR_N array
   type IPR0_IPR_N_Field_Array is array (0 .. 3) of IPR0_IPR_N_Element
     with Component_Size => 8, Size => 32;

   --  Interrupt Priority Register
   type IPR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IPR_N as a value
            Val : HAL.Word;
         when True =>
            --  IPR_N as an array
            Arr : IPR0_IPR_N_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for IPR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- STIR_Register --
   -------------------

   subtype STIR_INTID_Field is HAL.UInt9;

   --  Software Triggered Interrupt Register
   type STIR_Register is record
      --  Write-only. interrupt to be triggered
      INTID         : STIR_INTID_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STIR_Register use record
      INTID         at 0 range 0 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Nested Vectored Interrupt Controller
   type NVIC_Peripheral is record
      --  Interrupt Controller Type Register
      ICTR  : ICTR_Register;
      --  Interrupt Set-Enable Register
      ISER0 : HAL.Word;
      --  Interrupt Set-Enable Register
      ISER1 : HAL.Word;
      --  Interrupt Set-Enable Register
      ISER2 : HAL.Word;
      --  Interrupt Clear-Enable Register
      ICER0 : HAL.Word;
      --  Interrupt Clear-Enable Register
      ICER1 : HAL.Word;
      --  Interrupt Clear-Enable Register
      ICER2 : HAL.Word;
      --  Interrupt Set-Pending Register
      ISPR0 : HAL.Word;
      --  Interrupt Set-Pending Register
      ISPR1 : HAL.Word;
      --  Interrupt Set-Pending Register
      ISPR2 : HAL.Word;
      --  Interrupt Clear-Pending Register
      ICPR0 : HAL.Word;
      --  Interrupt Clear-Pending Register
      ICPR1 : HAL.Word;
      --  Interrupt Clear-Pending Register
      ICPR2 : HAL.Word;
      --  Interrupt Active Bit Register
      IABR0 : HAL.Word;
      --  Interrupt Active Bit Register
      IABR1 : HAL.Word;
      --  Interrupt Active Bit Register
      IABR2 : HAL.Word;
      --  Interrupt Priority Register
      IPR0  : IPR_Register;
      --  Interrupt Priority Register
      IPR1  : IPR_Register;
      --  Interrupt Priority Register
      IPR2  : IPR_Register;
      --  Interrupt Priority Register
      IPR3  : IPR_Register;
      --  Interrupt Priority Register
      IPR4  : IPR_Register;
      --  Interrupt Priority Register
      IPR5  : IPR_Register;
      --  Interrupt Priority Register
      IPR6  : IPR_Register;
      --  Interrupt Priority Register
      IPR7  : IPR_Register;
      --  Interrupt Priority Register
      IPR8  : IPR_Register;
      --  Interrupt Priority Register
      IPR9  : IPR_Register;
      --  Interrupt Priority Register
      IPR10 : IPR_Register;
      --  Interrupt Priority Register
      IPR11 : IPR_Register;
      --  Interrupt Priority Register
      IPR12 : IPR_Register;
      --  Interrupt Priority Register
      IPR13 : IPR_Register;
      --  Interrupt Priority Register
      IPR14 : IPR_Register;
      --  Interrupt Priority Register
      IPR15 : IPR_Register;
      --  Interrupt Priority Register
      IPR16 : IPR_Register;
      --  Interrupt Priority Register
      IPR17 : IPR_Register;
      --  Interrupt Priority Register
      IPR18 : IPR_Register;
      --  Interrupt Priority Register
      IPR19 : IPR_Register;
      --  Interrupt Priority Register
      IPR20 : IPR_Register;
      --  Software Triggered Interrupt Register
      STIR  : STIR_Register;
   end record
     with Volatile;

   for NVIC_Peripheral use record
      ICTR  at 4 range 0 .. 31;
      ISER0 at 256 range 0 .. 31;
      ISER1 at 260 range 0 .. 31;
      ISER2 at 264 range 0 .. 31;
      ICER0 at 384 range 0 .. 31;
      ICER1 at 388 range 0 .. 31;
      ICER2 at 392 range 0 .. 31;
      ISPR0 at 512 range 0 .. 31;
      ISPR1 at 516 range 0 .. 31;
      ISPR2 at 520 range 0 .. 31;
      ICPR0 at 640 range 0 .. 31;
      ICPR1 at 644 range 0 .. 31;
      ICPR2 at 648 range 0 .. 31;
      IABR0 at 768 range 0 .. 31;
      IABR1 at 772 range 0 .. 31;
      IABR2 at 776 range 0 .. 31;
      IPR0  at 1024 range 0 .. 31;
      IPR1  at 1028 range 0 .. 31;
      IPR2  at 1032 range 0 .. 31;
      IPR3  at 1036 range 0 .. 31;
      IPR4  at 1040 range 0 .. 31;
      IPR5  at 1044 range 0 .. 31;
      IPR6  at 1048 range 0 .. 31;
      IPR7  at 1052 range 0 .. 31;
      IPR8  at 1056 range 0 .. 31;
      IPR9  at 1060 range 0 .. 31;
      IPR10 at 1064 range 0 .. 31;
      IPR11 at 1068 range 0 .. 31;
      IPR12 at 1072 range 0 .. 31;
      IPR13 at 1076 range 0 .. 31;
      IPR14 at 1080 range 0 .. 31;
      IPR15 at 1084 range 0 .. 31;
      IPR16 at 1088 range 0 .. 31;
      IPR17 at 1092 range 0 .. 31;
      IPR18 at 1096 range 0 .. 31;
      IPR19 at 1100 range 0 .. 31;
      IPR20 at 1104 range 0 .. 31;
      STIR  at 3840 range 0 .. 31;
   end record;

   --  Nested Vectored Interrupt Controller
   NVIC_Periph : aliased NVIC_Peripheral
     with Import, Address => NVIC_Base;

end STM32_SVD.NVIC;
