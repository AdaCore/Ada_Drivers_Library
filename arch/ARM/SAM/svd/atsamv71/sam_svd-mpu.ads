--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.MPU is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype MPU_MPU_TYPE_DREGION_Field is HAL.UInt8;
   subtype MPU_MPU_TYPE_IREGION_Field is HAL.UInt8;

   --  MPU Type Register
   type MPU_MPU_TYPE_Register is record
      --  Indicates support for unified or separate instruction and date memory
      --  maps.
      SEPARATE_k     : Boolean := False;
      --  unspecified
      Reserved_1_7   : HAL.UInt7 := 16#0#;
      --  Indicates the number of supported MPU instruction regions.
      DREGION        : MPU_MPU_TYPE_DREGION_Field := 16#0#;
      --  Indicates the number of supported MPU data regions.
      IREGION        : MPU_MPU_TYPE_IREGION_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_MPU_TYPE_Register use record
      SEPARATE_k     at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      DREGION        at 0 range 8 .. 15;
      IREGION        at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  MPU Control Register
   type MPU_MPU_CTRL_Register is record
      --  Enables the MPU
      ENABLE        : Boolean := False;
      --  Enables the operation of MPU during hard fault, NMI, and FAULTMASK
      --  handlers.
      HFNMIENA      : Boolean := False;
      --  Enables privileged software access to the default memory map.
      PRIVDEFENA    : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_MPU_CTRL_Register use record
      ENABLE        at 0 range 0 .. 0;
      HFNMIENA      at 0 range 1 .. 1;
      PRIVDEFENA    at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype MPU_MPU_RNR_REGION_Field is HAL.UInt8;

   --  MPU Region Number Register
   type MPU_MPU_RNR_Register is record
      --  Indicates the MPU region referenced by the MPU_RBAR and MPU_RASR
      --  registers.
      REGION        : MPU_MPU_RNR_REGION_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_MPU_RNR_Register use record
      REGION        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype MPU_MPU_RBAR_REGION_Field is HAL.UInt4;
   subtype MPU_MPU_RBAR_ADDR_Field is HAL.UInt27;

   --  MPU Region Base Address Register
   type MPU_MPU_RBAR_Register is record
      --  MPU region field.
      REGION : MPU_MPU_RBAR_REGION_Field := 16#0#;
      --  MPU Region Number valid bit.
      VALID  : Boolean := False;
      --  Region base address field.
      ADDR   : MPU_MPU_RBAR_ADDR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_MPU_RBAR_Register use record
      REGION at 0 range 0 .. 3;
      VALID  at 0 range 4 .. 4;
      ADDR   at 0 range 5 .. 31;
   end record;

   subtype MPU_MPU_RASR_SIZE_Field is HAL.UInt5;
   subtype MPU_MPU_RASR_SRD_Field is HAL.UInt8;
   subtype MPU_MPU_RASR_TEX_Field is HAL.UInt3;
   subtype MPU_MPU_RASR_AP_Field is HAL.UInt3;

   --  MPU Region Attribute and Size Register
   type MPU_MPU_RASR_Register is record
      --  Region enable bit.
      ENABLE         : Boolean := False;
      --  Specifies the size of the MPU protection region.
      SIZE           : MPU_MPU_RASR_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Subregion disable bits.
      SRD            : MPU_MPU_RASR_SRD_Field := 16#0#;
      --  MPU access permission attributes.
      B              : Boolean := False;
      --  MPU access permission attributes.
      C              : Boolean := False;
      --  Shareable bit.
      S              : Boolean := False;
      --  MPU access permission attributes.
      TEX            : MPU_MPU_RASR_TEX_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Access permission field.
      AP             : MPU_MPU_RASR_AP_Field := 16#0#;
      --  unspecified
      Reserved_27_27 : HAL.Bit := 16#0#;
      --  Instruction access disable bit.
      XN             : Boolean := False;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_MPU_RASR_Register use record
      ENABLE         at 0 range 0 .. 0;
      SIZE           at 0 range 1 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      SRD            at 0 range 8 .. 15;
      B              at 0 range 16 .. 16;
      C              at 0 range 17 .. 17;
      S              at 0 range 18 .. 18;
      TEX            at 0 range 19 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      AP             at 0 range 24 .. 26;
      Reserved_27_27 at 0 range 27 .. 27;
      XN             at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Memory Protection Unit Registers
   type MPU_Peripheral is record
      --  MPU Type Register
      MPU_TYPE : aliased MPU_MPU_TYPE_Register;
      --  MPU Control Register
      MPU_CTRL : aliased MPU_MPU_CTRL_Register;
      --  MPU Region Number Register
      MPU_RNR  : aliased MPU_MPU_RNR_Register;
      --  MPU Region Base Address Register
      MPU_RBAR : aliased MPU_MPU_RBAR_Register;
      --  MPU Region Attribute and Size Register
      MPU_RASR : aliased MPU_MPU_RASR_Register;
   end record
     with Volatile;

   for MPU_Peripheral use record
      MPU_TYPE at 16#0# range 0 .. 31;
      MPU_CTRL at 16#4# range 0 .. 31;
      MPU_RNR  at 16#8# range 0 .. 31;
      MPU_RBAR at 16#C# range 0 .. 31;
      MPU_RASR at 16#10# range 0 .. 31;
   end record;

   --  Memory Protection Unit Registers
   MPU_Periph : aliased MPU_Peripheral
     with Import, Address => System'To_Address (16#E000ED90#);

end SAM_SVD.MPU;
