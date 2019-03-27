--  This spec has been automatically generated from SiFive.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  E31 CPU Coreplex, high-performance, 32-bit RV32IMAC core
--    
package SiFive_SVD is
   pragma Preelaborate;

   CLINT_Base : constant System.Address := System'To_Address (16#2000000#);
   PLIC_Base  : constant System.Address := System'To_Address (16#C000000#);
end SiFive_SVD;
