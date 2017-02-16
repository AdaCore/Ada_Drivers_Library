--  Copyright (c) 2013, Nordic Semiconductor ASA
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  * Redistributions of source code must retain the above copyright notice, this
--  list of conditions and the following disclaimer.
--
--  * Redistributions in binary form must reproduce the above copyright notice,
--  this list of conditions and the following disclaimer in the documentation
--  and/or other materials provided with the distribution.
--
--  * Neither the name of Nordic Semiconductor ASA nor the names of its
--  contributors may be used to endorse or promote products derived from
--  this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
--  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
--  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
--  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
--  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
--  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--  This spec has been automatically generated from nrf51.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with System;

--  nRF51 reference description for radio MCU with ARM 32-bit Cortex-M0
--  Microcontroller at 16MHz CPU clock
package NRF51_SVD is
   pragma Preelaborate;

   --------------------
   -- Base addresses --
   --------------------

   POWER_Base : constant System.Address :=
     System'To_Address (16#40000000#);
   CLOCK_Base : constant System.Address :=
     System'To_Address (16#40000000#);
   MPU_Base : constant System.Address :=
     System'To_Address (16#40000000#);
   AMLI_Base : constant System.Address :=
     System'To_Address (16#40000000#);
   RADIO_Base : constant System.Address :=
     System'To_Address (16#40001000#);
   UART0_Base : constant System.Address :=
     System'To_Address (16#40002000#);
   SPI0_Base : constant System.Address :=
     System'To_Address (16#40003000#);
   TWI0_Base : constant System.Address :=
     System'To_Address (16#40003000#);
   SPI1_Base : constant System.Address :=
     System'To_Address (16#40004000#);
   TWI1_Base : constant System.Address :=
     System'To_Address (16#40004000#);
   SPIS1_Base : constant System.Address :=
     System'To_Address (16#40004000#);
   SPIM1_Base : constant System.Address :=
     System'To_Address (16#40004000#);
   GPIOTE_Base : constant System.Address :=
     System'To_Address (16#40006000#);
   ADC_Base : constant System.Address :=
     System'To_Address (16#40007000#);
   TIMER0_Base : constant System.Address :=
     System'To_Address (16#40008000#);
   TIMER1_Base : constant System.Address :=
     System'To_Address (16#40009000#);
   TIMER2_Base : constant System.Address :=
     System'To_Address (16#4000A000#);
   RTC0_Base : constant System.Address :=
     System'To_Address (16#4000B000#);
   TEMP_Base : constant System.Address :=
     System'To_Address (16#4000C000#);
   RNG_Base : constant System.Address :=
     System'To_Address (16#4000D000#);
   ECB_Base : constant System.Address :=
     System'To_Address (16#4000E000#);
   AAR_Base : constant System.Address :=
     System'To_Address (16#4000F000#);
   CCM_Base : constant System.Address :=
     System'To_Address (16#4000F000#);
   WDT_Base : constant System.Address :=
     System'To_Address (16#40010000#);
   RTC1_Base : constant System.Address :=
     System'To_Address (16#40011000#);
   QDEC_Base : constant System.Address :=
     System'To_Address (16#40012000#);
   LPCOMP_Base : constant System.Address :=
     System'To_Address (16#40013000#);
   SWI_Base : constant System.Address :=
     System'To_Address (16#40014000#);
   NVMC_Base : constant System.Address :=
     System'To_Address (16#4001E000#);
   PPI_Base : constant System.Address :=
     System'To_Address (16#4001F000#);
   FICR_Base : constant System.Address :=
     System'To_Address (16#10000000#);
   UICR_Base : constant System.Address :=
     System'To_Address (16#10001000#);
   GPIO_Base : constant System.Address :=
     System'To_Address (16#50000000#);

end NRF51_SVD;
