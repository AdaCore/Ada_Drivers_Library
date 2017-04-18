--  This spec has been automatically generated from cm4.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

--  Memory Protection Unit
package Cortex_M_SVD.MPU is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Indicates support for unified or separate instruction and data memory
   --  maps.
   type TYPE_SEPARATE_Field is
     (
      --  Only unified memory maps are supported.
      Unified)
     with Size => 1;
   for TYPE_SEPARATE_Field use
     (Unified => 0);

   subtype MPU_TYPE_DREGION_Field is HAL.UInt8;
   subtype MPU_TYPE_IREGION_Field is HAL.UInt8;

   --  MPU Type Register
   type MPU_TYPE_Register is record
      --  Read-only. Indicates support for unified or separate instruction and
      --  data memory maps.
      SEPARATE_k     : TYPE_SEPARATE_Field;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. Indicates the number of supported MPU data regions
      --  depending on your implementation.
      DREGION        : MPU_TYPE_DREGION_Field;
      --  Read-only. Indicates the number of supported MPU instruction regions.
      --  Always contains 0x0: the MPU memory map is unified and is described
      --  by the DREGION field.
      IREGION        : MPU_TYPE_IREGION_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_TYPE_Register use record
      SEPARATE_k     at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      DREGION        at 0 range 8 .. 15;
      IREGION        at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  MPU Control Register
   type MPU_CTRL_Register is record
      --  Enables the optional MPU.
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

   for MPU_CTRL_Register use record
      ENABLE        at 0 range 0 .. 0;
      HFNMIENA      at 0 range 1 .. 1;
      PRIVDEFENA    at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype MPU_RNR_REGION_Field is HAL.UInt8;

   --  MPU Region Number Register
   type MPU_RNR_Register is record
      --  Indicates the MPU region referenced by the MPU_RBAR and MPU_RASR
      --  registers.
      REGION        : MPU_RNR_REGION_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_RNR_Register use record
      REGION        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype MPU_RBAR_REGION_Field is HAL.UInt4;
   subtype MPU_RBAR_ADDR_Field is HAL.UInt27;

   --  MPU Region Base Address Register
   type MPU_RBAR_Register is record
      --  On Write, see the VALID field. On read, specifies the region number.
      REGION : MPU_RBAR_REGION_Field := 16#0#;
      --  MPU Region number valid bit. Depending on your implementation, this
      --  has the following effect: 0 - either updates the base address for the
      --  region specified by MPU_RNR or ignores the value of the REGION field.
      --  1 - either updates the value of the MPU_RNR to the value of the
      --  REGION field or updates the base address for the region specified in
      --  the REGION field.
      VALID  : Boolean := False;
      --  The ADDR field is bits[31:N] of the MPU_RBAR. The region size, as
      --  specified by the SIZE field in the MPU_RASR, defines the value of N:
      --  N = Log2(Region size in bytes). If the region size is configured to
      --  4GB, in the MPU_RASR, there is no valid ADDR field. In this case, the
      --  region occupies the complete memory map, and the base address is
      --  0x00000000. The base address is aligned to the size of the region.
      --  For example, a 64KB region must be aligned on a multiple of 64KB, for
      --  example, at 0x00010000 or 0x00020000.
      ADDR   : MPU_RBAR_ADDR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_RBAR_Register use record
      REGION at 0 range 0 .. 3;
      VALID  at 0 range 4 .. 4;
      ADDR   at 0 range 5 .. 31;
   end record;

   subtype MPU_RASR_SIZE_Field is HAL.UInt5;

   --  MPU_RASR_SRD array
   type MPU_RASR_SRD_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for MPU_RASR_SRD
   type MPU_RASR_SRD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SRD as a value
            Val : HAL.UInt8;
         when True =>
            --  SRD as an array
            Arr : MPU_RASR_SRD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for MPU_RASR_SRD_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   subtype MPU_RASR_TEX_Field is HAL.UInt3;

   --  Access permission field
   type RASR_AP_Field is
     (
      --  All accesses generate a permission fault.
      No_Access,
      --  Access from privileged software only.
      Privileged_Rw,
      --  Writes by unprivileged software generates a permission fault.
      Unpriviledged_Ro,
      --  Full Access.
      Full_Access,
      --  Reads by privileged software only.
      Privileged_Ro,
      --  Read_Only by privileged or unprivileged software.
      Read_Only,
      --  Read_Only by privileged or unprivileged software.
      Read_Only_1)
     with Size => 3;
   for RASR_AP_Field use
     (No_Access => 0,
      Privileged_Rw => 1,
      Unpriviledged_Ro => 2,
      Full_Access => 3,
      Privileged_Ro => 5,
      Read_Only => 6,
      Read_Only_1 => 7);

   --  Instruction access disable bit
   type RASR_XN_Field is
     (
      --  Instruction fetches enabled.
      I_Enabled,
      --  Instruction fetches disabled.
      I_Disabled)
     with Size => 1;
   for RASR_XN_Field use
     (I_Enabled => 0,
      I_Disabled => 1);

   --  MPU Region Base Attribute and Size Register
   type MPU_RASR_Register is record
      --  Region enable bit.
      ENABLE         : Boolean := False;
      --  Specifies the size of the MPU protection region. Minimum value is 4.
      --  The Region size is defined as (Region size in bytes) = 2^(SIZE+1)
      SIZE           : MPU_RASR_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Subregion disable bits
      SRD            : MPU_RASR_SRD_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Memory access attribute.
      B              : Boolean := False;
      --  Memory access attribute.
      C              : Boolean := False;
      --  Shareable bit. Applies to Normal memory only.
      S              : Boolean := False;
      --  Memory access attribute.
      TEX            : MPU_RASR_TEX_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Access permission field
      AP             : RASR_AP_Field := Cortex_M_SVD.MPU.No_Access;
      --  unspecified
      Reserved_27_27 : HAL.Bit := 16#0#;
      --  Instruction access disable bit
      XN             : RASR_XN_Field := Cortex_M_SVD.MPU.I_Enabled;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MPU_RASR_Register use record
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

   subtype RBAR_A_REGION_Field is HAL.UInt4;
   subtype RBAR_A_ADDR_Field is HAL.UInt27;

   --  Uses (MPU_RNR[7:2]<<2) + 1
   type RBAR_A_Register is record
      --  On Write, see the VALID field. On read, specifies the region number.
      REGION : RBAR_A_REGION_Field := 16#0#;
      --  MPU Region number valid bit. Depending on your implementation, this
      --  has the following effect: 0 - either updates the base address for the
      --  region specified by MPU_RNR or ignores the value of the REGION field.
      --  1 - either updates the value of the MPU_RNR to the value of the
      --  REGION field or updates the base address for the region specified in
      --  the REGION field.
      VALID  : Boolean := False;
      --  The ADDR field is bits[31:N] of the MPU_RBAR. The region size, as
      --  specified by the SIZE field in the MPU_RASR, defines the value of N:
      --  N = Log2(Region size in bytes). If the region size is configured to
      --  4GB, in the MPU_RASR, there is no valid ADDR field. In this case, the
      --  region occupies the complete memory map, and the base address is
      --  0x00000000. The base address is aligned to the size of the region.
      --  For example, a 64KB region must be aligned on a multiple of 64KB, for
      --  example, at 0x00010000 or 0x00020000.
      ADDR   : RBAR_A_ADDR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RBAR_A_Register use record
      REGION at 0 range 0 .. 3;
      VALID  at 0 range 4 .. 4;
      ADDR   at 0 range 5 .. 31;
   end record;

   subtype RASR_A_SIZE_Field is HAL.UInt5;

   --  RASR_A_SRD array
   type RASR_A_SRD_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for RASR_A_SRD
   type RASR_A_SRD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SRD as a value
            Val : HAL.UInt8;
         when True =>
            --  SRD as an array
            Arr : RASR_A_SRD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for RASR_A_SRD_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   subtype RASR_A_TEX_Field is HAL.UInt3;

   --  Access permission field
   type RASR_A1_AP_Field is
     (
      --  All accesses generate a permission fault.
      No_Access,
      --  Access from privileged software only.
      Privileged_Rw,
      --  Writes by unprivileged software generates a permission fault.
      Unpriviledged_Ro,
      --  Full Access.
      Full_Access,
      --  Reads by privileged software only.
      Privileged_Ro,
      --  Read_Only by privileged or unprivileged software.
      Read_Only,
      --  Read_Only by privileged or unprivileged software.
      Read_Only_1)
     with Size => 3;
   for RASR_A1_AP_Field use
     (No_Access => 0,
      Privileged_Rw => 1,
      Unpriviledged_Ro => 2,
      Full_Access => 3,
      Privileged_Ro => 5,
      Read_Only => 6,
      Read_Only_1 => 7);

   --  Instruction access disable bit
   type RASR_A1_XN_Field is
     (
      --  Instruction fetches enabled.
      I_Enabled,
      --  Instruction fetches disabled.
      I_Disabled)
     with Size => 1;
   for RASR_A1_XN_Field use
     (I_Enabled => 0,
      I_Disabled => 1);

   --  Uses (MPU_RNR[7:2]<<2) + 1
   type RASR_A_Register is record
      --  Region enable bit.
      ENABLE         : Boolean := False;
      --  Specifies the size of the MPU protection region. Minimum value is 4.
      --  The Region size is defined as (Region size in bytes) = 2^(SIZE+1)
      SIZE           : RASR_A_SIZE_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Subregion disable bits
      SRD            : RASR_A_SRD_Field := (As_Array => False, Val => 16#0#);
      --  Memory access attribute.
      B              : Boolean := False;
      --  Memory access attribute.
      C              : Boolean := False;
      --  Shareable bit. Applies to Normal memory only.
      S              : Boolean := False;
      --  Memory access attribute.
      TEX            : RASR_A_TEX_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Access permission field
      AP             : RASR_A1_AP_Field := Cortex_M_SVD.MPU.No_Access;
      --  unspecified
      Reserved_27_27 : HAL.Bit := 16#0#;
      --  Instruction access disable bit
      XN             : RASR_A1_XN_Field := Cortex_M_SVD.MPU.I_Enabled;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RASR_A_Register use record
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

   --  Memory Protection Unit
   type MPU_Peripheral is record
      --  MPU Type Register
      TYPE_k  : aliased MPU_TYPE_Register;
      --  MPU Control Register
      CTRL    : aliased MPU_CTRL_Register;
      --  MPU Region Number Register
      RNR     : aliased MPU_RNR_Register;
      --  MPU Region Base Address Register
      RBAR    : aliased MPU_RBAR_Register;
      --  MPU Region Base Attribute and Size Register
      RASR    : aliased MPU_RASR_Register;
      --  Uses (MPU_RNR[7:2]<<2) + 1
      RBAR_A1 : aliased RBAR_A_Register;
      --  Uses (MPU_RNR[7:2]<<2) + 1
      RASR_A1 : aliased RASR_A_Register;
      --  Uses (MPU_RNR[7:2]<<2) + 2
      RBAR_A2 : aliased RBAR_A_Register;
      --  Uses (MPU_RNR[7:2]<<2) + 2
      RASR_A2 : aliased RASR_A_Register;
      --  Uses (MPU_RNR[7:2]<<2) + 3
      RBAR_A3 : aliased RBAR_A_Register;
      --  Uses (MPU_RNR[7:2]<<2) + 3
      RASR_A3 : aliased RASR_A_Register;
   end record
     with Volatile;

   for MPU_Peripheral use record
      TYPE_k  at 16#0# range 0 .. 31;
      CTRL    at 16#4# range 0 .. 31;
      RNR     at 16#8# range 0 .. 31;
      RBAR    at 16#C# range 0 .. 31;
      RASR    at 16#10# range 0 .. 31;
      RBAR_A1 at 16#14# range 0 .. 31;
      RASR_A1 at 16#18# range 0 .. 31;
      RBAR_A2 at 16#1C# range 0 .. 31;
      RASR_A2 at 16#20# range 0 .. 31;
      RBAR_A3 at 16#24# range 0 .. 31;
      RASR_A3 at 16#28# range 0 .. 31;
   end record;

   --  Memory Protection Unit
   MPU_Periph : aliased MPU_Peripheral
     with Import, Address => MPU_Base;

end Cortex_M_SVD.MPU;
