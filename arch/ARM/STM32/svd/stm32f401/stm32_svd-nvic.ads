--  This spec has been automatically generated from STM32F401.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.NVIC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  IPR_IPR_N array element
   subtype IPR_IPR_N_Element is HAL.UInt8;

   --  IPR_IPR_N array
   type IPR_IPR_N_Field_Array is array (0 .. 3) of IPR_IPR_N_Element
     with Component_Size => 8, Size => 32;

   --  Interrupt Priority Register
   type IPR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IPR_N as a value
            Val : HAL.UInt32;
         when True =>
            --  IPR_N as an array
            Arr : IPR_IPR_N_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IPR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype STIR_INTID_Field is HAL.UInt9;

   --  Software trigger interrupt register
   type STIR_Register is record
      --  Software generated interrupt ID
      INTID         : STIR_INTID_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
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
      --  Interrupt Set-Enable Register
      ISER0 : aliased HAL.UInt32;
      --  Interrupt Set-Enable Register
      ISER1 : aliased HAL.UInt32;
      --  Interrupt Set-Enable Register
      ISER2 : aliased HAL.UInt32;
      --  Interrupt Clear-Enable Register
      ICER0 : aliased HAL.UInt32;
      --  Interrupt Clear-Enable Register
      ICER1 : aliased HAL.UInt32;
      --  Interrupt Clear-Enable Register
      ICER2 : aliased HAL.UInt32;
      --  Interrupt Set-Pending Register
      ISPR0 : aliased HAL.UInt32;
      --  Interrupt Set-Pending Register
      ISPR1 : aliased HAL.UInt32;
      --  Interrupt Set-Pending Register
      ISPR2 : aliased HAL.UInt32;
      --  Interrupt Clear-Pending Register
      ICPR0 : aliased HAL.UInt32;
      --  Interrupt Clear-Pending Register
      ICPR1 : aliased HAL.UInt32;
      --  Interrupt Clear-Pending Register
      ICPR2 : aliased HAL.UInt32;
      --  Interrupt Active Bit Register
      IABR0 : aliased HAL.UInt32;
      --  Interrupt Active Bit Register
      IABR1 : aliased HAL.UInt32;
      --  Interrupt Active Bit Register
      IABR2 : aliased HAL.UInt32;
      --  Interrupt Priority Register
      IPR0  : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR1  : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR2  : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR3  : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR4  : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR5  : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR6  : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR7  : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR8  : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR9  : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR10 : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR11 : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR12 : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR13 : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR14 : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR15 : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR16 : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR17 : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR18 : aliased IPR_Register;
      --  Interrupt Priority Register
      IPR19 : aliased IPR_Register;
   end record
     with Volatile;

   for NVIC_Peripheral use record
      ISER0 at 16#0# range 0 .. 31;
      ISER1 at 16#4# range 0 .. 31;
      ISER2 at 16#8# range 0 .. 31;
      ICER0 at 16#80# range 0 .. 31;
      ICER1 at 16#84# range 0 .. 31;
      ICER2 at 16#88# range 0 .. 31;
      ISPR0 at 16#100# range 0 .. 31;
      ISPR1 at 16#104# range 0 .. 31;
      ISPR2 at 16#108# range 0 .. 31;
      ICPR0 at 16#180# range 0 .. 31;
      ICPR1 at 16#184# range 0 .. 31;
      ICPR2 at 16#188# range 0 .. 31;
      IABR0 at 16#200# range 0 .. 31;
      IABR1 at 16#204# range 0 .. 31;
      IABR2 at 16#208# range 0 .. 31;
      IPR0  at 16#300# range 0 .. 31;
      IPR1  at 16#304# range 0 .. 31;
      IPR2  at 16#308# range 0 .. 31;
      IPR3  at 16#30C# range 0 .. 31;
      IPR4  at 16#310# range 0 .. 31;
      IPR5  at 16#314# range 0 .. 31;
      IPR6  at 16#318# range 0 .. 31;
      IPR7  at 16#31C# range 0 .. 31;
      IPR8  at 16#320# range 0 .. 31;
      IPR9  at 16#324# range 0 .. 31;
      IPR10 at 16#328# range 0 .. 31;
      IPR11 at 16#32C# range 0 .. 31;
      IPR12 at 16#330# range 0 .. 31;
      IPR13 at 16#334# range 0 .. 31;
      IPR14 at 16#338# range 0 .. 31;
      IPR15 at 16#33C# range 0 .. 31;
      IPR16 at 16#340# range 0 .. 31;
      IPR17 at 16#344# range 0 .. 31;
      IPR18 at 16#348# range 0 .. 31;
      IPR19 at 16#34C# range 0 .. 31;
   end record;

   --  Nested Vectored Interrupt Controller
   NVIC_Periph : aliased NVIC_Peripheral
     with Import, Address => NVIC_Base;

   --  Nested vectored interrupt controller
   type NVIC_STIR_Peripheral is record
      --  Software trigger interrupt register
      STIR : aliased STIR_Register;
   end record
     with Volatile;

   for NVIC_STIR_Peripheral use record
      STIR at 0 range 0 .. 31;
   end record;

   --  Nested vectored interrupt controller
   NVIC_STIR_Periph : aliased NVIC_STIR_Peripheral
     with Import, Address => NVIC_STIR_Base;

end STM32_SVD.NVIC;
