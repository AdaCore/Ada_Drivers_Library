--  This spec has been automatically generated from cm0.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with HAL;

package Cortex_M_SVD.NVIC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Interrupt Priority Register

   --  Interrupt Priority Register
   type NVIC_IPR_Registers is array (0 .. 31) of HAL.Byte;

   -----------------
   -- Peripherals --
   -----------------

   type NVIC_Peripheral is record
      --  Interrupt Set-Enable Registers
      NVIC_ISER : HAL.UInt32;
      --  Interrupt Clear-Enable Registers
      NVIC_ICER : HAL.UInt32;
      --  Interrupt Set-Pending Registers
      NVIC_ISPR : HAL.UInt32;
      --  Interrupt Clear-Pending Registers
      NVIC_ICPR : HAL.UInt32;
      --  Interrupt Priority Register
      NVIC_IPR  : NVIC_IPR_Registers;
   end record
     with Volatile;

   for NVIC_Peripheral use record
      NVIC_ISER at 16#0# range 0 .. 31;
      NVIC_ICER at 16#80# range 0 .. 31;
      NVIC_ISPR at 16#100# range 0 .. 31;
      NVIC_ICPR at 16#180# range 0 .. 31;
      NVIC_IPR  at 16#300# range 0 .. 255;
   end record;

   NVIC_Periph : aliased NVIC_Peripheral
     with Import, Address => NVIC_Base;

end Cortex_M_SVD.NVIC;
