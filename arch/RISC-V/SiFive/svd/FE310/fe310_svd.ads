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

   OTP_Mem_Base : constant System.Address := System'To_Address (16#20000#);
   PRIC_Base : constant System.Address := System'To_Address (16#10008000#);
   PLIC_Base : constant System.Address := System'To_Address (16#C000000#);
   CLINT_Base : constant System.Address := System'To_Address (16#2000000#);
   GPIO0_Base : constant System.Address := System'To_Address (16#10012000#);
   QSPI0_Base : constant System.Address := System'To_Address (16#10014000#);
   QSPI1_Base : constant System.Address := System'To_Address (16#10024000#);
   QSPI2_Base : constant System.Address := System'To_Address (16#10034000#);
   UART0_Base : constant System.Address := System'To_Address (16#10013000#);
   UART1_Base : constant System.Address := System'To_Address (16#10023000#);
   PWM0_Base : constant System.Address := System'To_Address (16#10015000#);
   PWM1_Base : constant System.Address := System'To_Address (16#10025000#);
   PWM2_Base : constant System.Address := System'To_Address (16#10035000#);
   WDT_Base : constant System.Address := System'To_Address (16#10000000#);
   RTC_Base : constant System.Address := System'To_Address (16#10000040#);
   AON_Base : constant System.Address := System'To_Address (16#10000070#);
   BACKUP_Base : constant System.Address := System'To_Address (16#10000080#);
   PMU_Base : constant System.Address := System'To_Address (16#10000100#);

end FE310_SVD;
