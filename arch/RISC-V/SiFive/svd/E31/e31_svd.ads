--  This spec has been automatically generated from E31.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  E31 CPU Coreplex, high-performance, 32-bit RV32IMAC core
--    
package E31_SVD is
   pragma Preelaborate;

   --------------------
   -- Base addresses --
   --------------------

   PLIC_Base : constant System.Address := System'To_Address (16#C000000#);
   CLINT_Base : constant System.Address := System'To_Address (16#2000000#);
   GPIO0_Base : constant System.Address := System'To_Address (16#20002000#);
   QSPI0_Base : constant System.Address := System'To_Address (16#20004000#);
   UART0_Base : constant System.Address := System'To_Address (16#20000000#);

end E31_SVD;
