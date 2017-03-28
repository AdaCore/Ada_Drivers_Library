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

package NRF51_SVD.AMLI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -------------------------------------
   -- AMLI_RAMPRI cluster's Registers --
   -------------------------------------

   --  Configuration field for RAM block 0.
   type CPU0_RAM0_Field is
     (
      --  Priority 0.
      Pri0,
      --  Priority 2.
      Pri2,
      --  Priority 4.
      Pri4,
      --  Priority 6.
      Pri6,
      --  Priority 8.
      Pri8,
      --  Priority 10.
      Pri10,
      --  Priority 12.
      Pri12,
      --  Priority 14.
      Pri14)
     with Size => 4;
   for CPU0_RAM0_Field use
     (Pri0 => 0,
      Pri2 => 2,
      Pri4 => 4,
      Pri6 => 6,
      Pri8 => 8,
      Pri10 => 10,
      Pri12 => 12,
      Pri14 => 14);

   --  CPU0_RAMPRI_RAM array
   type CPU0_RAMPRI_RAM_Field_Array is array (0 .. 7) of CPU0_RAM0_Field
     with Component_Size => 4, Size => 32;

   --  Configurable priority configuration register for CPU0.
   type CPU0_RAMPRI_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RAM as a value
            Val : HAL.UInt32;
         when True =>
            --  RAM as an array
            Arr : CPU0_RAMPRI_RAM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for CPU0_RAMPRI_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Configuration field for RAM block 0.
   type SPIS1_RAM0_Field is
     (
      --  Priority 0.
      Pri0,
      --  Priority 2.
      Pri2,
      --  Priority 4.
      Pri4,
      --  Priority 6.
      Pri6,
      --  Priority 8.
      Pri8,
      --  Priority 10.
      Pri10,
      --  Priority 12.
      Pri12,
      --  Priority 14.
      Pri14)
     with Size => 4;
   for SPIS1_RAM0_Field use
     (Pri0 => 0,
      Pri2 => 2,
      Pri4 => 4,
      Pri6 => 6,
      Pri8 => 8,
      Pri10 => 10,
      Pri12 => 12,
      Pri14 => 14);

   --  SPIS1_RAMPRI_RAM array
   type SPIS1_RAMPRI_RAM_Field_Array is array (0 .. 7) of SPIS1_RAM0_Field
     with Component_Size => 4, Size => 32;

   --  Configurable priority configuration register for SPIS1.
   type SPIS1_RAMPRI_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RAM as a value
            Val : HAL.UInt32;
         when True =>
            --  RAM as an array
            Arr : SPIS1_RAMPRI_RAM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for SPIS1_RAMPRI_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Configuration field for RAM block 0.
   type RADIO_RAM0_Field is
     (
      --  Priority 0.
      Pri0,
      --  Priority 2.
      Pri2,
      --  Priority 4.
      Pri4,
      --  Priority 6.
      Pri6,
      --  Priority 8.
      Pri8,
      --  Priority 10.
      Pri10,
      --  Priority 12.
      Pri12,
      --  Priority 14.
      Pri14)
     with Size => 4;
   for RADIO_RAM0_Field use
     (Pri0 => 0,
      Pri2 => 2,
      Pri4 => 4,
      Pri6 => 6,
      Pri8 => 8,
      Pri10 => 10,
      Pri12 => 12,
      Pri14 => 14);

   --  RADIO_RAMPRI_RAM array
   type RADIO_RAMPRI_RAM_Field_Array is array (0 .. 7) of RADIO_RAM0_Field
     with Component_Size => 4, Size => 32;

   --  Configurable priority configuration register for RADIO.
   type RADIO_RAMPRI_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RAM as a value
            Val : HAL.UInt32;
         when True =>
            --  RAM as an array
            Arr : RADIO_RAMPRI_RAM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for RADIO_RAMPRI_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Configuration field for RAM block 0.
   type ECB_RAM0_Field is
     (
      --  Priority 0.
      Pri0,
      --  Priority 2.
      Pri2,
      --  Priority 4.
      Pri4,
      --  Priority 6.
      Pri6,
      --  Priority 8.
      Pri8,
      --  Priority 10.
      Pri10,
      --  Priority 12.
      Pri12,
      --  Priority 14.
      Pri14)
     with Size => 4;
   for ECB_RAM0_Field use
     (Pri0 => 0,
      Pri2 => 2,
      Pri4 => 4,
      Pri6 => 6,
      Pri8 => 8,
      Pri10 => 10,
      Pri12 => 12,
      Pri14 => 14);

   --  ECB_RAMPRI_RAM array
   type ECB_RAMPRI_RAM_Field_Array is array (0 .. 7) of ECB_RAM0_Field
     with Component_Size => 4, Size => 32;

   --  Configurable priority configuration register for ECB.
   type ECB_RAMPRI_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RAM as a value
            Val : HAL.UInt32;
         when True =>
            --  RAM as an array
            Arr : ECB_RAMPRI_RAM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for ECB_RAMPRI_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Configuration field for RAM block 0.
   type CCM_RAM0_Field is
     (
      --  Priority 0.
      Pri0,
      --  Priority 2.
      Pri2,
      --  Priority 4.
      Pri4,
      --  Priority 6.
      Pri6,
      --  Priority 8.
      Pri8,
      --  Priority 10.
      Pri10,
      --  Priority 12.
      Pri12,
      --  Priority 14.
      Pri14)
     with Size => 4;
   for CCM_RAM0_Field use
     (Pri0 => 0,
      Pri2 => 2,
      Pri4 => 4,
      Pri6 => 6,
      Pri8 => 8,
      Pri10 => 10,
      Pri12 => 12,
      Pri14 => 14);

   --  CCM_RAMPRI_RAM array
   type CCM_RAMPRI_RAM_Field_Array is array (0 .. 7) of CCM_RAM0_Field
     with Component_Size => 4, Size => 32;

   --  Configurable priority configuration register for CCM.
   type CCM_RAMPRI_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RAM as a value
            Val : HAL.UInt32;
         when True =>
            --  RAM as an array
            Arr : CCM_RAMPRI_RAM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for CCM_RAMPRI_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Configuration field for RAM block 0.
   type AAR_RAM0_Field is
     (
      --  Priority 0.
      Pri0,
      --  Priority 2.
      Pri2,
      --  Priority 4.
      Pri4,
      --  Priority 6.
      Pri6,
      --  Priority 8.
      Pri8,
      --  Priority 10.
      Pri10,
      --  Priority 12.
      Pri12,
      --  Priority 14.
      Pri14)
     with Size => 4;
   for AAR_RAM0_Field use
     (Pri0 => 0,
      Pri2 => 2,
      Pri4 => 4,
      Pri6 => 6,
      Pri8 => 8,
      Pri10 => 10,
      Pri12 => 12,
      Pri14 => 14);

   --  AAR_RAMPRI_RAM array
   type AAR_RAMPRI_RAM_Field_Array is array (0 .. 7) of AAR_RAM0_Field
     with Component_Size => 4, Size => 32;

   --  Configurable priority configuration register for AAR.
   type AAR_RAMPRI_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RAM as a value
            Val : HAL.UInt32;
         when True =>
            --  RAM as an array
            Arr : AAR_RAMPRI_RAM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for AAR_RAMPRI_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  RAM configurable priority configuration structure.
   type AMLI_RAMPRI_Cluster is record
      --  Configurable priority configuration register for CPU0.
      CPU0  : aliased CPU0_RAMPRI_Register;
      --  Configurable priority configuration register for SPIS1.
      SPIS1 : aliased SPIS1_RAMPRI_Register;
      --  Configurable priority configuration register for RADIO.
      RADIO : aliased RADIO_RAMPRI_Register;
      --  Configurable priority configuration register for ECB.
      ECB   : aliased ECB_RAMPRI_Register;
      --  Configurable priority configuration register for CCM.
      CCM   : aliased CCM_RAMPRI_Register;
      --  Configurable priority configuration register for AAR.
      AAR   : aliased AAR_RAMPRI_Register;
   end record
     with Volatile, Size => 192;

   for AMLI_RAMPRI_Cluster use record
      CPU0  at 16#0# range 0 .. 31;
      SPIS1 at 16#4# range 0 .. 31;
      RADIO at 16#8# range 0 .. 31;
      ECB   at 16#C# range 0 .. 31;
      CCM   at 16#10# range 0 .. 31;
      AAR   at 16#14# range 0 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  AHB Multi-Layer Interface.
   type AMLI_Peripheral is record
      --  RAM configurable priority configuration structure.
      RAMPRI : aliased AMLI_RAMPRI_Cluster;
   end record
     with Volatile;

   for AMLI_Peripheral use record
      RAMPRI at 16#E00# range 0 .. 191;
   end record;

   --  AHB Multi-Layer Interface.
   AMLI_Periph : aliased AMLI_Peripheral
     with Import, Address => System'To_Address (16#40000000#);

end NRF51_SVD.AMLI;
