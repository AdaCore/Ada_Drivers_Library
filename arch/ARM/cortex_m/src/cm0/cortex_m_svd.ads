--  This spec has been automatically generated from cm0.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package Cortex_M_SVD is
   pragma Preelaborate;

   --------------------
   -- Base addresses --
   --------------------

   NVIC_Base : constant System.Address :=
     System'To_Address (16#E000E100#);
   SCB_Base : constant System.Address :=
     System'To_Address (16#E000ED00#);
   SysTick_Base : constant System.Address :=
     System'To_Address (16#E000E010#);
   Debug_Base : constant System.Address :=
     System'To_Address (16#E000ED00#);
   DWT_Base : constant System.Address :=
     System'To_Address (16#E0001000#);

end Cortex_M_SVD;
