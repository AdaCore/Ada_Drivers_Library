--  This spec has been automatically generated from FE310.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  E31 CPU Coreplex, high-performance, 32-bit RV32IMAC core
--    
package FE310_SVD is
   pragma Preelaborate;

   --------------------
   -- Base addresses --
   --------------------

   GPIO0_Base : constant System.Address :=
     System'To_Address (16#10012000#);
   UART0_Base : constant System.Address :=
     System'To_Address (16#10013000#);
   UART1_Base : constant System.Address :=
     System'To_Address (16#10023000#);
   PWM0_Base : constant System.Address :=
     System'To_Address (16#10015000#);
   PWM1_Base : constant System.Address :=
     System'To_Address (16#10025000#);
   PWM2_Base : constant System.Address :=
     System'To_Address (16#10035000#);

end FE310_SVD;
