--  This spec has been automatically generated from cortex_m-debug.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with System;

--  ARM 32-bit Cortex-M Microcontroller
package Cortex_M_SVD is
   pragma Preelaborate;

   --------------------
   -- Base addresses --
   --------------------

   Debug_Base : constant System.Address :=
     System'To_Address (16#E000ED00#);

end Cortex_M_SVD;
