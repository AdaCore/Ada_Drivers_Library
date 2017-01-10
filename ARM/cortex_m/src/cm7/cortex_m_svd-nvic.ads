--  This spec has been automatically generated from cm7.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package Cortex_M_SVD.NVIC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Interrupt Set-Enable Registers

   --  Interrupt Set-Enable Registers
   type NVIC_ISER_Registers is array (0 .. 7) of HAL.UInt32
     with Volatile;

   --  Interrupt Clear-Enable Registers

   --  Interrupt Clear-Enable Registers
   type NVIC_ICER_Registers is array (0 .. 7) of HAL.UInt32
     with Volatile;

   --  Interrupt Set-Pending Registers

   --  Interrupt Set-Pending Registers
   type NVIC_ISPR_Registers is array (0 .. 7) of HAL.UInt32
     with Volatile;

   --  Interrupt Clear-Pending Registers

   --  Interrupt Clear-Pending Registers
   type NVIC_ICPR_Registers is array (0 .. 7) of HAL.UInt32
     with Volatile;

   --  Interrupt Active Bit Register

   --  Interrupt Active Bit Register
   type NVIC_IABR_Registers is array (0 .. 7) of HAL.UInt32
     with Volatile;

   --  Interrupt Priority Register

   --  Interrupt Priority Register
   type NVIC_IPR_Registers is array (0 .. 59) of HAL.UInt32
     with Volatile;

   subtype STIR_INTID_Field is HAL.UInt9;

   --  Software Trigger Interrupt Register
   type STIR_Register is record
      --  Write-only. Interrupt ID of the interrupt to trigger, in the range
      --  0-239.
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

   type NVIC_Peripheral is record
      --  Interrupt Set-Enable Registers
      NVIC_ISER : aliased NVIC_ISER_Registers;
      --  Interrupt Clear-Enable Registers
      NVIC_ICER : aliased NVIC_ICER_Registers;
      --  Interrupt Set-Pending Registers
      NVIC_ISPR : aliased NVIC_ISPR_Registers;
      --  Interrupt Clear-Pending Registers
      NVIC_ICPR : aliased NVIC_ICPR_Registers;
      --  Interrupt Active Bit Register
      NVIC_IABR : aliased NVIC_IABR_Registers;
      --  Interrupt Priority Register
      NVIC_IPR  : aliased NVIC_IPR_Registers;
      --  Software Trigger Interrupt Register
      STIR      : aliased STIR_Register;
   end record
     with Volatile;

   for NVIC_Peripheral use record
      NVIC_ISER at 16#0# range 0 .. 255;
      NVIC_ICER at 16#80# range 0 .. 255;
      NVIC_ISPR at 16#100# range 0 .. 255;
      NVIC_ICPR at 16#180# range 0 .. 255;
      NVIC_IABR at 16#200# range 0 .. 255;
      NVIC_IPR  at 16#300# range 0 .. 1919;
      STIR      at 16#E00# range 0 .. 31;
   end record;

   NVIC_Periph : aliased NVIC_Peripheral
     with Import, Address => NVIC_Base;

end Cortex_M_SVD.NVIC;
