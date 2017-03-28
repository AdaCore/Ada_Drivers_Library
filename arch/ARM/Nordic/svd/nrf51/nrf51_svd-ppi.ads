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

package NRF51_SVD.PPI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ---------------------------------------
   -- PPI_TASKS_CHG cluster's Registers --
   ---------------------------------------

   --  Channel group tasks.
   type PPI_TASKS_CHG_Cluster is record
      --  Enable channel group.
      EN  : aliased HAL.UInt32;
      --  Disable channel group.
      DIS : aliased HAL.UInt32;
   end record
     with Volatile, Size => 64;

   for PPI_TASKS_CHG_Cluster use record
      EN  at 16#0# range 0 .. 31;
      DIS at 16#4# range 0 .. 31;
   end record;

   --  Channel group tasks.
   type PPI_TASKS_CHG_Clusters is array (0 .. 3) of PPI_TASKS_CHG_Cluster;

   --  Enable PPI channel 0.
   type CHEN_CH0_Field is
     (
      --  Channel disabled.
      Disabled,
      --  Channel enabled.
      Enabled)
     with Size => 1;
   for CHEN_CH0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  CHEN_CH array
   type CHEN_CH_Field_Array is array (0 .. 2) of CHEN_CH0_Field
     with Component_Size => 1, Size => 3;

   --  Type definition for CHEN_CH
   type CHEN_CH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CH as a value
            Val : HAL.UInt3;
         when True =>
            --  CH as an array
            Arr : CHEN_CH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for CHEN_CH_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  Enable PPI channel 3.
   type CHEN_CH3_Field is
     (
      --  Channel disabled
      Disabled,
      --  Channel enabled
      Enabled)
     with Size => 1;
   for CHEN_CH3_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable PPI channel 4.
   type CHEN_CH4_Field is
     (
      --  Channel disabled.
      Disabled,
      --  Channel enabled.
      Enabled)
     with Size => 1;
   for CHEN_CH4_Field use
     (Disabled => 0,
      Enabled => 1);

   --  CHEN_CH array
   type CHEN_CH_Field_Array_1 is array (4 .. 15) of CHEN_CH4_Field
     with Component_Size => 1, Size => 12;

   --  Type definition for CHEN_CH
   type CHEN_CH_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CH as a value
            Val : HAL.UInt12;
         when True =>
            --  CH as an array
            Arr : CHEN_CH_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 12;

   for CHEN_CH_Field_1 use record
      Val at 0 range 0 .. 11;
      Arr at 0 range 0 .. 11;
   end record;

   --  Enable PPI channel 20.
   type CHEN_CH20_Field is
     (
      --  Channel disabled.
      Disabled,
      --  Channel enabled.
      Enabled)
     with Size => 1;
   for CHEN_CH20_Field use
     (Disabled => 0,
      Enabled => 1);

   --  CHEN_CH array
   type CHEN_CH_Field_Array_2 is array (20 .. 31) of CHEN_CH20_Field
     with Component_Size => 1, Size => 12;

   --  Type definition for CHEN_CH
   type CHEN_CH_Field_2
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CH as a value
            Val : HAL.UInt12;
         when True =>
            --  CH as an array
            Arr : CHEN_CH_Field_Array_2;
      end case;
   end record
     with Unchecked_Union, Size => 12;

   for CHEN_CH_Field_2 use record
      Val at 0 range 0 .. 11;
      Arr at 0 range 0 .. 11;
   end record;

   --  Channel enable.
   type CHEN_Register is record
      --  Enable PPI channel 0.
      CH             : CHEN_CH_Field := (As_Array => False, Val => 16#0#);
      --  Enable PPI channel 3.
      CH3            : CHEN_CH3_Field := NRF51_SVD.PPI.Disabled;
      --  Enable PPI channel 4.
      CH_1           : CHEN_CH_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  Enable PPI channel 20.
      CH_2           : CHEN_CH_Field_2 := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CHEN_Register use record
      CH             at 0 range 0 .. 2;
      CH3            at 0 range 3 .. 3;
      CH_1           at 0 range 4 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      CH_2           at 0 range 20 .. 31;
   end record;

   --  Enable PPI channel 0.
   type CHENSET_CH0_Field is
     (
      --  Channel disabled.
      Disabled,
      --  Channel enabled.
      Enabled)
     with Size => 1;
   for CHENSET_CH0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable PPI channel 0.
   type CHENSET_CH0_Field_1 is
     (
      --  Reset value for the field
      Chenset_Ch0_Field_Reset,
      --  Enable channel on write.
      Set)
     with Size => 1;
   for CHENSET_CH0_Field_1 use
     (Chenset_Ch0_Field_Reset => 0,
      Set => 1);

   --  CHENSET_CH array
   type CHENSET_CH_Field_Array is array (0 .. 15) of CHENSET_CH0_Field_1
     with Component_Size => 1, Size => 16;

   --  Type definition for CHENSET_CH
   type CHENSET_CH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CH as a value
            Val : HAL.UInt16;
         when True =>
            --  CH as an array
            Arr : CHENSET_CH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for CHENSET_CH_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Enable PPI channel 20.
   type CHENSET_CH20_Field is
     (
      --  Channel disabled.
      Disabled,
      --  Channel enabled.
      Enabled)
     with Size => 1;
   for CHENSET_CH20_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable PPI channel 20.
   type CHENSET_CH20_Field_1 is
     (
      --  Reset value for the field
      Chenset_Ch20_Field_Reset,
      --  Enable channel on write.
      Set)
     with Size => 1;
   for CHENSET_CH20_Field_1 use
     (Chenset_Ch20_Field_Reset => 0,
      Set => 1);

   --  CHENSET_CH array
   type CHENSET_CH_Field_Array_1 is array (20 .. 31) of CHENSET_CH20_Field_1
     with Component_Size => 1, Size => 12;

   --  Type definition for CHENSET_CH
   type CHENSET_CH_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CH as a value
            Val : HAL.UInt12;
         when True =>
            --  CH as an array
            Arr : CHENSET_CH_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 12;

   for CHENSET_CH_Field_1 use record
      Val at 0 range 0 .. 11;
      Arr at 0 range 0 .. 11;
   end record;

   --  Channel enable set.
   type CHENSET_Register is record
      --  Enable PPI channel 0.
      CH             : CHENSET_CH_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  Enable PPI channel 20.
      CH_1           : CHENSET_CH_Field_1 :=
                        (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CHENSET_Register use record
      CH             at 0 range 0 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      CH_1           at 0 range 20 .. 31;
   end record;

   --  Disable PPI channel 0.
   type CHENCLR_CH0_Field is
     (
      --  Channel disabled.
      Disabled,
      --  Channel enabled.
      Enabled)
     with Size => 1;
   for CHENCLR_CH0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable PPI channel 0.
   type CHENCLR_CH0_Field_1 is
     (
      --  Reset value for the field
      Chenclr_Ch0_Field_Reset,
      --  Disable channel on write.
      Clear)
     with Size => 1;
   for CHENCLR_CH0_Field_1 use
     (Chenclr_Ch0_Field_Reset => 0,
      Clear => 1);

   --  CHENCLR_CH array
   type CHENCLR_CH_Field_Array is array (0 .. 15) of CHENCLR_CH0_Field_1
     with Component_Size => 1, Size => 16;

   --  Type definition for CHENCLR_CH
   type CHENCLR_CH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CH as a value
            Val : HAL.UInt16;
         when True =>
            --  CH as an array
            Arr : CHENCLR_CH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for CHENCLR_CH_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Disable PPI channel 20.
   type CHENCLR_CH20_Field is
     (
      --  Channel disabled.
      Disabled,
      --  Channel enabled.
      Enabled)
     with Size => 1;
   for CHENCLR_CH20_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable PPI channel 20.
   type CHENCLR_CH20_Field_1 is
     (
      --  Reset value for the field
      Chenclr_Ch20_Field_Reset,
      --  Disable channel on write.
      Clear)
     with Size => 1;
   for CHENCLR_CH20_Field_1 use
     (Chenclr_Ch20_Field_Reset => 0,
      Clear => 1);

   --  CHENCLR_CH array
   type CHENCLR_CH_Field_Array_1 is array (20 .. 31) of CHENCLR_CH20_Field_1
     with Component_Size => 1, Size => 12;

   --  Type definition for CHENCLR_CH
   type CHENCLR_CH_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CH as a value
            Val : HAL.UInt12;
         when True =>
            --  CH as an array
            Arr : CHENCLR_CH_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 12;

   for CHENCLR_CH_Field_1 use record
      Val at 0 range 0 .. 11;
      Arr at 0 range 0 .. 11;
   end record;

   --  Channel enable clear.
   type CHENCLR_Register is record
      --  Disable PPI channel 0.
      CH             : CHENCLR_CH_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  Disable PPI channel 20.
      CH_1           : CHENCLR_CH_Field_1 :=
                        (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CHENCLR_Register use record
      CH             at 0 range 0 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      CH_1           at 0 range 20 .. 31;
   end record;

   --------------------------------
   -- PPI_CH cluster's Registers --
   --------------------------------

   --  PPI Channel.
   type PPI_CH_Cluster is record
      --  Channel event end-point.
      EEP : aliased HAL.UInt32;
      --  Channel task end-point.
      TEP : aliased HAL.UInt32;
   end record
     with Volatile, Size => 64;

   for PPI_CH_Cluster use record
      EEP at 16#0# range 0 .. 31;
      TEP at 16#4# range 0 .. 31;
   end record;

   --  PPI Channel.
   type PPI_CH_Clusters is array (0 .. 15) of PPI_CH_Cluster;

   --  Include CH0 in channel group.
   type CHG_CH0_Field is
     (
      --  Channel excluded.
      Excluded,
      --  Channel included.
      Included)
     with Size => 1;
   for CHG_CH0_Field use
     (Excluded => 0,
      Included => 1);

   --  CHG_CH array
   type CHG_CH_Field_Array is array (0 .. 15) of CHG_CH0_Field
     with Component_Size => 1, Size => 16;

   --  Type definition for CHG_CH
   type CHG_CH_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CH as a value
            Val : HAL.UInt16;
         when True =>
            --  CH as an array
            Arr : CHG_CH_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for CHG_CH_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  Include CH20 in channel group.
   type CHG_CH20_Field is
     (
      --  Channel excluded.
      Excluded,
      --  Channel included.
      Included)
     with Size => 1;
   for CHG_CH20_Field use
     (Excluded => 0,
      Included => 1);

   --  CHG_CH array
   type CHG_CH_Field_Array_1 is array (20 .. 31) of CHG_CH20_Field
     with Component_Size => 1, Size => 12;

   --  Type definition for CHG_CH
   type CHG_CH_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CH as a value
            Val : HAL.UInt12;
         when True =>
            --  CH as an array
            Arr : CHG_CH_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 12;

   for CHG_CH_Field_1 use record
      Val at 0 range 0 .. 11;
      Arr at 0 range 0 .. 11;
   end record;

   --  Channel group configuration.
   type CHG_Register is record
      --  Include CH0 in channel group.
      CH             : CHG_CH_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  Include CH20 in channel group.
      CH_1           : CHG_CH_Field_1 := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CHG_Register use record
      CH             at 0 range 0 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      CH_1           at 0 range 20 .. 31;
   end record;

   --  Channel group configuration.
   type CHG_Registers is array (0 .. 3) of CHG_Register
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   --  PPI controller.
   type PPI_Peripheral is record
      --  Channel group tasks.
      TASKS_CHG : aliased PPI_TASKS_CHG_Clusters;
      --  Channel enable.
      CHEN      : aliased CHEN_Register;
      --  Channel enable set.
      CHENSET   : aliased CHENSET_Register;
      --  Channel enable clear.
      CHENCLR   : aliased CHENCLR_Register;
      --  PPI Channel.
      CH        : aliased PPI_CH_Clusters;
      --  Channel group configuration.
      CHG       : aliased CHG_Registers;
   end record
     with Volatile;

   for PPI_Peripheral use record
      TASKS_CHG at 16#0# range 0 .. 255;
      CHEN      at 16#500# range 0 .. 31;
      CHENSET   at 16#504# range 0 .. 31;
      CHENCLR   at 16#508# range 0 .. 31;
      CH        at 16#510# range 0 .. 1023;
      CHG       at 16#800# range 0 .. 127;
   end record;

   --  PPI controller.
   PPI_Periph : aliased PPI_Peripheral
     with Import, Address => System'To_Address (16#4001F000#);

end NRF51_SVD.PPI;
