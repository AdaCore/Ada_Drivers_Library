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

with HAL;
with System;

package NRF51_SVD.MPU is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  POWER_CLOCK region configuration.
   type PERR0_POWER_CLOCK_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_POWER_CLOCK_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  RADIO region configuration.
   type PERR0_RADIO_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_RADIO_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  UART0 region configuration.
   type PERR0_UART0_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_UART0_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  SPI0 and TWI0 region configuration.
   type PERR0_SPI0_TWI0_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_SPI0_TWI0_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  SPI1 and TWI1 region configuration.
   type PERR0_SPI1_TWI1_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_SPI1_TWI1_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  GPIOTE region configuration.
   type PERR0_GPIOTE_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_GPIOTE_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  ADC region configuration.
   type PERR0_ADC_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_ADC_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  TIMER0 region configuration.
   type PERR0_TIMER0_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_TIMER0_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  PERR0_TIMER array
   type PERR0_TIMER_Field_Array is array (0 .. 2) of PERR0_TIMER0_Field
     with Component_Size => 1, Size => 3;

   --  Type definition for PERR0_TIMER
   type PERR0_TIMER_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TIMER as a value
            Val : HAL.UInt3;
         when True =>
            --  TIMER as an array
            Arr : PERR0_TIMER_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for PERR0_TIMER_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  RTC0 region configuration.
   type PERR0_RTC0_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_RTC0_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  TEMP region configuration.
   type PERR0_TEMP_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_TEMP_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  RNG region configuration.
   type PERR0_RNG_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_RNG_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  ECB region configuration.
   type PERR0_ECB_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_ECB_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  CCM and AAR region configuration.
   type PERR0_CCM_AAR_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_CCM_AAR_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  WDT region configuration.
   type PERR0_WDT_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_WDT_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  RTC1 region configuration.
   type PERR0_RTC1_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_RTC1_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  QDEC region configuration.
   type PERR0_QDEC_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_QDEC_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  LPCOMP region configuration.
   type PERR0_LPCOMP_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_LPCOMP_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  NVMC region configuration.
   type PERR0_NVMC_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_NVMC_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  PPI region configuration.
   type PERR0_PPI_Field is
     (
      --  Peripheral configured in region 1.
      Inregion1,
      --  Peripheral configured in region 0.
      Inregion0)
     with Size => 1;
   for PERR0_PPI_Field use
     (Inregion1 => 0,
      Inregion0 => 1);

   --  Configuration of peripherals in mpu regions.
   type PERR0_Register is record
      --  POWER_CLOCK region configuration.
      POWER_CLOCK    : PERR0_POWER_CLOCK_Field := NRF51_SVD.MPU.Inregion1;
      --  RADIO region configuration.
      RADIO          : PERR0_RADIO_Field := NRF51_SVD.MPU.Inregion1;
      --  UART0 region configuration.
      UART0          : PERR0_UART0_Field := NRF51_SVD.MPU.Inregion1;
      --  SPI0 and TWI0 region configuration.
      SPI0_TWI0      : PERR0_SPI0_TWI0_Field := NRF51_SVD.MPU.Inregion1;
      --  SPI1 and TWI1 region configuration.
      SPI1_TWI1      : PERR0_SPI1_TWI1_Field := NRF51_SVD.MPU.Inregion1;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  GPIOTE region configuration.
      GPIOTE         : PERR0_GPIOTE_Field := NRF51_SVD.MPU.Inregion1;
      --  ADC region configuration.
      ADC            : PERR0_ADC_Field := NRF51_SVD.MPU.Inregion1;
      --  TIMER0 region configuration.
      TIMER          : PERR0_TIMER_Field := (As_Array => False, Val => 16#0#);
      --  RTC0 region configuration.
      RTC0           : PERR0_RTC0_Field := NRF51_SVD.MPU.Inregion1;
      --  TEMP region configuration.
      TEMP           : PERR0_TEMP_Field := NRF51_SVD.MPU.Inregion1;
      --  RNG region configuration.
      RNG            : PERR0_RNG_Field := NRF51_SVD.MPU.Inregion1;
      --  ECB region configuration.
      ECB            : PERR0_ECB_Field := NRF51_SVD.MPU.Inregion1;
      --  CCM and AAR region configuration.
      CCM_AAR        : PERR0_CCM_AAR_Field := NRF51_SVD.MPU.Inregion1;
      --  WDT region configuration.
      WDT            : PERR0_WDT_Field := NRF51_SVD.MPU.Inregion1;
      --  RTC1 region configuration.
      RTC1           : PERR0_RTC1_Field := NRF51_SVD.MPU.Inregion1;
      --  QDEC region configuration.
      QDEC           : PERR0_QDEC_Field := NRF51_SVD.MPU.Inregion1;
      --  LPCOMP region configuration.
      LPCOMP         : PERR0_LPCOMP_Field := NRF51_SVD.MPU.Inregion1;
      --  unspecified
      Reserved_20_29 : HAL.UInt10 := 16#0#;
      --  NVMC region configuration.
      NVMC           : PERR0_NVMC_Field := NRF51_SVD.MPU.Inregion1;
      --  PPI region configuration.
      PPI            : PERR0_PPI_Field := NRF51_SVD.MPU.Inregion1;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PERR0_Register use record
      POWER_CLOCK    at 0 range 0 .. 0;
      RADIO          at 0 range 1 .. 1;
      UART0          at 0 range 2 .. 2;
      SPI0_TWI0      at 0 range 3 .. 3;
      SPI1_TWI1      at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      GPIOTE         at 0 range 6 .. 6;
      ADC            at 0 range 7 .. 7;
      TIMER          at 0 range 8 .. 10;
      RTC0           at 0 range 11 .. 11;
      TEMP           at 0 range 12 .. 12;
      RNG            at 0 range 13 .. 13;
      ECB            at 0 range 14 .. 14;
      CCM_AAR        at 0 range 15 .. 15;
      WDT            at 0 range 16 .. 16;
      RTC1           at 0 range 17 .. 17;
      QDEC           at 0 range 18 .. 18;
      LPCOMP         at 0 range 19 .. 19;
      Reserved_20_29 at 0 range 20 .. 29;
      NVMC           at 0 range 30 .. 30;
      PPI            at 0 range 31 .. 31;
   end record;

   --  Protection enable for region 0.
   type PROTENSET0_PROTREG0_Field is
     (
      --  Protection disabled.
      Disabled,
      --  Protection enabled.
      Enabled)
     with Size => 1;
   for PROTENSET0_PROTREG0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Protection enable for region 0.
   type PROTENSET0_PROTREG0_Field_1 is
     (
      --  Reset value for the field
      Protenset0_Protreg0_Field_Reset,
      --  Enable protection on write.
      Set)
     with Size => 1;
   for PROTENSET0_PROTREG0_Field_1 use
     (Protenset0_Protreg0_Field_Reset => 0,
      Set => 1);

   --  PROTENSET0_PROTREG array
   type PROTENSET0_PROTREG_Field_Array is array (0 .. 31)
     of PROTENSET0_PROTREG0_Field_1
     with Component_Size => 1, Size => 32;

   --  Erase and write protection bit enable set register.
   type PROTENSET0_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PROTREG as a value
            Val : HAL.UInt32;
         when True =>
            --  PROTREG as an array
            Arr : PROTENSET0_PROTREG_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for PROTENSET0_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Protection enable for region 32.
   type PROTENSET1_PROTREG32_Field is
     (
      --  Protection disabled.
      Disabled,
      --  Protection enabled.
      Enabled)
     with Size => 1;
   for PROTENSET1_PROTREG32_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Protection enable for region 32.
   type PROTENSET1_PROTREG32_Field_1 is
     (
      --  Reset value for the field
      Protenset1_Protreg32_Field_Reset,
      --  Enable protection on write.
      Set)
     with Size => 1;
   for PROTENSET1_PROTREG32_Field_1 use
     (Protenset1_Protreg32_Field_Reset => 0,
      Set => 1);

   --  PROTENSET1_PROTREG array
   type PROTENSET1_PROTREG_Field_Array is array (32 .. 63)
     of PROTENSET1_PROTREG32_Field_1
     with Component_Size => 1, Size => 32;

   --  Erase and write protection bit enable set register.
   type PROTENSET1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PROTREG as a value
            Val : HAL.UInt32;
         when True =>
            --  PROTREG as an array
            Arr : PROTENSET1_PROTREG_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for PROTENSET1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Disable protection mechanism in debug mode.
   type DISABLEINDEBUG_DISABLEINDEBUG_Field is
     (
      --  Protection enabled.
      Enabled,
      --  Protection disabled.
      Disabled)
     with Size => 1;
   for DISABLEINDEBUG_DISABLEINDEBUG_Field use
     (Enabled => 0,
      Disabled => 1);

   --  Disable erase and write protection mechanism in debug mode.
   type DISABLEINDEBUG_Register is record
      --  Disable protection mechanism in debug mode.
      DISABLEINDEBUG : DISABLEINDEBUG_DISABLEINDEBUG_Field :=
                        NRF51_SVD.MPU.Disabled;
      --  unspecified
      Reserved_1_31  : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DISABLEINDEBUG_Register use record
      DISABLEINDEBUG at 0 range 0 .. 0;
      Reserved_1_31  at 0 range 1 .. 31;
   end record;

   --  Erase and write protection block size.
   type PROTBLOCKSIZE_PROTBLOCKSIZE_Field is
     (
      --  Erase and write protection block size is 4k.
      PROTBLOCKSIZE_PROTBLOCKSIZE_Field_4K)
     with Size => 2;
   for PROTBLOCKSIZE_PROTBLOCKSIZE_Field use
     (PROTBLOCKSIZE_PROTBLOCKSIZE_Field_4K => 0);

   --  Erase and write protection block size.
   type PROTBLOCKSIZE_Register is record
      --  Erase and write protection block size.
      PROTBLOCKSIZE : PROTBLOCKSIZE_PROTBLOCKSIZE_Field :=
                       NRF51_SVD.MPU.PROTBLOCKSIZE_PROTBLOCKSIZE_Field_4K;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PROTBLOCKSIZE_Register use record
      PROTBLOCKSIZE at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Memory Protection Unit.
   type MPU_Peripheral is record
      --  Configuration of peripherals in mpu regions.
      PERR0          : aliased PERR0_Register;
      --  Length of RAM region 0.
      RLENR0         : aliased HAL.UInt32;
      --  Erase and write protection bit enable set register.
      PROTENSET0     : aliased PROTENSET0_Register;
      --  Erase and write protection bit enable set register.
      PROTENSET1     : aliased PROTENSET1_Register;
      --  Disable erase and write protection mechanism in debug mode.
      DISABLEINDEBUG : aliased DISABLEINDEBUG_Register;
      --  Erase and write protection block size.
      PROTBLOCKSIZE  : aliased PROTBLOCKSIZE_Register;
   end record
     with Volatile;

   for MPU_Peripheral use record
      PERR0          at 16#528# range 0 .. 31;
      RLENR0         at 16#52C# range 0 .. 31;
      PROTENSET0     at 16#600# range 0 .. 31;
      PROTENSET1     at 16#604# range 0 .. 31;
      DISABLEINDEBUG at 16#608# range 0 .. 31;
      PROTBLOCKSIZE  at 16#60C# range 0 .. 31;
   end record;

   --  Memory Protection Unit.
   MPU_Periph : aliased MPU_Peripheral
     with Import, Address => System'To_Address (16#40000000#);

end NRF51_SVD.MPU;
