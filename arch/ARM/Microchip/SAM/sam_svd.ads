--  ============================================================================
--  Atmel Microcontroller Software Support
--  ============================================================================
--  Copyright (c) 2017 Atmel Corporation,
--  a wholly owned subsidiary of Microchip Technology Inc.
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the Licence at
--
--  http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
--  ============================================================================

--  This spec has been automatically generated from ATSAMD51J19A.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  Atmel ATSAMD51J19A device: Cortex-M4 Microcontroller with 512KB Flash,
--  192KB SRAM, 64-pin package (refer to
--  http://www.atmel.com/devices/SAMD51J19A.aspx for more)
package SAM_SVD is
   pragma Preelaborate;

   --------------------
   -- Base addresses --
   --------------------

   AC_Base : constant System.Address :=
     System'To_Address (16#42002000#);
   ADC0_Base : constant System.Address :=
     System'To_Address (16#43001C00#);
   ADC1_Base : constant System.Address :=
     System'To_Address (16#43002000#);
   AES_Base : constant System.Address :=
     System'To_Address (16#42002400#);
   CCL_Base : constant System.Address :=
     System'To_Address (16#42003800#);
   CMCC_Base : constant System.Address :=
     System'To_Address (16#41006000#);
   DAC_Base : constant System.Address :=
     System'To_Address (16#43002400#);
   DMAC_Base : constant System.Address :=
     System'To_Address (16#4100A000#);
   DSU_Base : constant System.Address :=
     System'To_Address (16#41002000#);
   EIC_Base : constant System.Address :=
     System'To_Address (16#40002800#);
   EVSYS_Base : constant System.Address :=
     System'To_Address (16#4100E000#);
   FREQM_Base : constant System.Address :=
     System'To_Address (16#40002C00#);
   GCLK_Base : constant System.Address :=
     System'To_Address (16#40001C00#);
   HMATRIX_Base : constant System.Address :=
     System'To_Address (16#4100C000#);
   ICM_Base : constant System.Address :=
     System'To_Address (16#42002C00#);
   I2S_Base : constant System.Address :=
     System'To_Address (16#43002800#);
   MCLK_Base : constant System.Address :=
     System'To_Address (16#40000800#);
   NVMCTRL_Base : constant System.Address :=
     System'To_Address (16#41004000#);
   OSCCTRL_Base : constant System.Address :=
     System'To_Address (16#40001000#);
   OSC32KCTRL_Base : constant System.Address :=
     System'To_Address (16#40001400#);
   PAC_Base : constant System.Address :=
     System'To_Address (16#40000000#);
   PCC_Base : constant System.Address :=
     System'To_Address (16#43002C00#);
   PDEC_Base : constant System.Address :=
     System'To_Address (16#42001C00#);
   PM_Base : constant System.Address :=
     System'To_Address (16#40000400#);
   PORT_Base : constant System.Address :=
     System'To_Address (16#41008000#);
   QSPI_Base : constant System.Address :=
     System'To_Address (16#42003400#);
   RAMECC_Base : constant System.Address :=
     System'To_Address (16#41020000#);
   RSTC_Base : constant System.Address :=
     System'To_Address (16#40000C00#);
   RTC_Base : constant System.Address :=
     System'To_Address (16#40002400#);
   SDHC0_Base : constant System.Address :=
     System'To_Address (16#45000000#);
   SERCOM0_Base : constant System.Address :=
     System'To_Address (16#40003000#);
   SERCOM1_Base : constant System.Address :=
     System'To_Address (16#40003400#);
   SERCOM2_Base : constant System.Address :=
     System'To_Address (16#41012000#);
   SERCOM3_Base : constant System.Address :=
     System'To_Address (16#41014000#);
   SERCOM4_Base : constant System.Address :=
     System'To_Address (16#43000000#);
   SERCOM5_Base : constant System.Address :=
     System'To_Address (16#43000400#);
   SUPC_Base : constant System.Address :=
     System'To_Address (16#40001800#);
   TAL_Base : constant System.Address :=
     System'To_Address (16#4101E000#);
   TC0_Base : constant System.Address :=
     System'To_Address (16#40003800#);
   TC1_Base : constant System.Address :=
     System'To_Address (16#40003C00#);
   TC2_Base : constant System.Address :=
     System'To_Address (16#4101A000#);
   TC3_Base : constant System.Address :=
     System'To_Address (16#4101C000#);
   TC4_Base : constant System.Address :=
     System'To_Address (16#42001400#);
   TC5_Base : constant System.Address :=
     System'To_Address (16#42001800#);
   TCC0_Base : constant System.Address :=
     System'To_Address (16#41016000#);
   TCC1_Base : constant System.Address :=
     System'To_Address (16#41018000#);
   TCC2_Base : constant System.Address :=
     System'To_Address (16#42000C00#);
   TCC3_Base : constant System.Address :=
     System'To_Address (16#42001000#);
   TCC4_Base : constant System.Address :=
     System'To_Address (16#43001000#);
   TRNG_Base : constant System.Address :=
     System'To_Address (16#42002800#);
   USB_Base : constant System.Address :=
     System'To_Address (16#41000000#);
   WDT_Base : constant System.Address :=
     System'To_Address (16#40002000#);

end SAM_SVD;
