--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.ICM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype ICM_ICM_CFG_BBC_Field is HAL.UInt4;

   --  User SHA Algorithm
   type ICM_CFG_UALGO_Field is
     (
      --  SHA1 algorithm processed
      Sha1,
      --  SHA256 algorithm processed
      Sha256,
      --  SHA224 algorithm processed
      Sha224)
     with Size => 3;
   for ICM_CFG_UALGO_Field use
     (Sha1 => 0,
      Sha256 => 1,
      Sha224 => 4);

   subtype ICM_ICM_CFG_HAPROT_Field is HAL.UInt6;
   subtype ICM_ICM_CFG_DAPROT_Field is HAL.UInt6;

   --  Configuration Register
   type ICM_ICM_CFG_Register is record
      --  Write Back Disable
      WBDIS          : Boolean := False;
      --  End of Monitoring Disable
      EOMDIS         : Boolean := False;
      --  Secondary List Branching Disable
      SLBDIS         : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Bus Burden Control
      BBC            : ICM_ICM_CFG_BBC_Field := 16#0#;
      --  Automatic Switch To Compare Digest
      ASCD           : Boolean := False;
      --  Dual Input Buffer
      DUALBUFF       : Boolean := False;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  User Initial Hash Value
      UIHASH         : Boolean := False;
      --  User SHA Algorithm
      UALGO          : ICM_CFG_UALGO_Field := SAM_SVD.ICM.Sha1;
      --  Region Hash Area Protection
      HAPROT         : ICM_ICM_CFG_HAPROT_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Region Descriptor Area Protection
      DAPROT         : ICM_ICM_CFG_DAPROT_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICM_ICM_CFG_Register use record
      WBDIS          at 0 range 0 .. 0;
      EOMDIS         at 0 range 1 .. 1;
      SLBDIS         at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      BBC            at 0 range 4 .. 7;
      ASCD           at 0 range 8 .. 8;
      DUALBUFF       at 0 range 9 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      UIHASH         at 0 range 12 .. 12;
      UALGO          at 0 range 13 .. 15;
      HAPROT         at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      DAPROT         at 0 range 24 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype ICM_ICM_CTRL_REHASH_Field is HAL.UInt4;
   subtype ICM_ICM_CTRL_RMDIS_Field is HAL.UInt4;
   subtype ICM_ICM_CTRL_RMEN_Field is HAL.UInt4;

   --  Control Register
   type ICM_ICM_CTRL_Register is record
      --  Write-only. ICM Enable
      ENABLE         : Boolean := False;
      --  Write-only. ICM Disable Register
      DISABLE        : Boolean := False;
      --  Write-only. Software Reset
      SWRST          : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Write-only. Recompute Internal Hash
      REHASH         : ICM_ICM_CTRL_REHASH_Field := 16#0#;
      --  Write-only. Region Monitoring Disable
      RMDIS          : ICM_ICM_CTRL_RMDIS_Field := 16#0#;
      --  Write-only. Region Monitoring Enable
      RMEN           : ICM_ICM_CTRL_RMEN_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICM_ICM_CTRL_Register use record
      ENABLE         at 0 range 0 .. 0;
      DISABLE        at 0 range 1 .. 1;
      SWRST          at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      REHASH         at 0 range 4 .. 7;
      RMDIS          at 0 range 8 .. 11;
      RMEN           at 0 range 12 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype ICM_ICM_SR_RAWRMDIS_Field is HAL.UInt4;
   subtype ICM_ICM_SR_RMDIS_Field is HAL.UInt4;

   --  Status Register
   type ICM_ICM_SR_Register is record
      --  Read-only. ICM Controller Enable Register
      ENABLE         : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. Region Monitoring Disabled Raw Status
      RAWRMDIS       : ICM_ICM_SR_RAWRMDIS_Field;
      --  Read-only. Region Monitoring Disabled Status
      RMDIS          : ICM_ICM_SR_RMDIS_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICM_ICM_SR_Register use record
      ENABLE         at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      RAWRMDIS       at 0 range 8 .. 11;
      RMDIS          at 0 range 12 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype ICM_ICM_IER_RHC_Field is HAL.UInt4;
   subtype ICM_ICM_IER_RDM_Field is HAL.UInt4;
   subtype ICM_ICM_IER_RBE_Field is HAL.UInt4;
   subtype ICM_ICM_IER_RWC_Field is HAL.UInt4;
   subtype ICM_ICM_IER_REC_Field is HAL.UInt4;
   subtype ICM_ICM_IER_RSU_Field is HAL.UInt4;

   --  Interrupt Enable Register
   type ICM_ICM_IER_Register is record
      --  Write-only. Region Hash Completed Interrupt Enable
      RHC            : ICM_ICM_IER_RHC_Field := 16#0#;
      --  Write-only. Region Digest Mismatch Interrupt Enable
      RDM            : ICM_ICM_IER_RDM_Field := 16#0#;
      --  Write-only. Region Bus Error Interrupt Enable
      RBE            : ICM_ICM_IER_RBE_Field := 16#0#;
      --  Write-only. Region Wrap Condition detected Interrupt Enable
      RWC            : ICM_ICM_IER_RWC_Field := 16#0#;
      --  Write-only. Region End bit Condition Detected Interrupt Enable
      REC            : ICM_ICM_IER_REC_Field := 16#0#;
      --  Write-only. Region Status Updated Interrupt Disable
      RSU            : ICM_ICM_IER_RSU_Field := 16#0#;
      --  Write-only. Undefined Register Access Detection Interrupt Enable
      URAD           : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICM_ICM_IER_Register use record
      RHC            at 0 range 0 .. 3;
      RDM            at 0 range 4 .. 7;
      RBE            at 0 range 8 .. 11;
      RWC            at 0 range 12 .. 15;
      REC            at 0 range 16 .. 19;
      RSU            at 0 range 20 .. 23;
      URAD           at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype ICM_ICM_IDR_RHC_Field is HAL.UInt4;
   subtype ICM_ICM_IDR_RDM_Field is HAL.UInt4;
   subtype ICM_ICM_IDR_RBE_Field is HAL.UInt4;
   subtype ICM_ICM_IDR_RWC_Field is HAL.UInt4;
   subtype ICM_ICM_IDR_REC_Field is HAL.UInt4;
   subtype ICM_ICM_IDR_RSU_Field is HAL.UInt4;

   --  Interrupt Disable Register
   type ICM_ICM_IDR_Register is record
      --  Write-only. Region Hash Completed Interrupt Disable
      RHC            : ICM_ICM_IDR_RHC_Field := 16#0#;
      --  Write-only. Region Digest Mismatch Interrupt Disable
      RDM            : ICM_ICM_IDR_RDM_Field := 16#0#;
      --  Write-only. Region Bus Error Interrupt Disable
      RBE            : ICM_ICM_IDR_RBE_Field := 16#0#;
      --  Write-only. Region Wrap Condition Detected Interrupt Disable
      RWC            : ICM_ICM_IDR_RWC_Field := 16#0#;
      --  Write-only. Region End bit Condition detected Interrupt Disable
      REC            : ICM_ICM_IDR_REC_Field := 16#0#;
      --  Write-only. Region Status Updated Interrupt Disable
      RSU            : ICM_ICM_IDR_RSU_Field := 16#0#;
      --  Write-only. Undefined Register Access Detection Interrupt Disable
      URAD           : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICM_ICM_IDR_Register use record
      RHC            at 0 range 0 .. 3;
      RDM            at 0 range 4 .. 7;
      RBE            at 0 range 8 .. 11;
      RWC            at 0 range 12 .. 15;
      REC            at 0 range 16 .. 19;
      RSU            at 0 range 20 .. 23;
      URAD           at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype ICM_ICM_IMR_RHC_Field is HAL.UInt4;
   subtype ICM_ICM_IMR_RDM_Field is HAL.UInt4;
   subtype ICM_ICM_IMR_RBE_Field is HAL.UInt4;
   subtype ICM_ICM_IMR_RWC_Field is HAL.UInt4;
   subtype ICM_ICM_IMR_REC_Field is HAL.UInt4;
   subtype ICM_ICM_IMR_RSU_Field is HAL.UInt4;

   --  Interrupt Mask Register
   type ICM_ICM_IMR_Register is record
      --  Read-only. Region Hash Completed Interrupt Mask
      RHC            : ICM_ICM_IMR_RHC_Field;
      --  Read-only. Region Digest Mismatch Interrupt Mask
      RDM            : ICM_ICM_IMR_RDM_Field;
      --  Read-only. Region Bus Error Interrupt Mask
      RBE            : ICM_ICM_IMR_RBE_Field;
      --  Read-only. Region Wrap Condition Detected Interrupt Mask
      RWC            : ICM_ICM_IMR_RWC_Field;
      --  Read-only. Region End bit Condition Detected Interrupt Mask
      REC            : ICM_ICM_IMR_REC_Field;
      --  Read-only. Region Status Updated Interrupt Mask
      RSU            : ICM_ICM_IMR_RSU_Field;
      --  Read-only. Undefined Register Access Detection Interrupt Mask
      URAD           : Boolean;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICM_ICM_IMR_Register use record
      RHC            at 0 range 0 .. 3;
      RDM            at 0 range 4 .. 7;
      RBE            at 0 range 8 .. 11;
      RWC            at 0 range 12 .. 15;
      REC            at 0 range 16 .. 19;
      RSU            at 0 range 20 .. 23;
      URAD           at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype ICM_ICM_ISR_RHC_Field is HAL.UInt4;
   subtype ICM_ICM_ISR_RDM_Field is HAL.UInt4;
   subtype ICM_ICM_ISR_RBE_Field is HAL.UInt4;
   subtype ICM_ICM_ISR_RWC_Field is HAL.UInt4;
   subtype ICM_ICM_ISR_REC_Field is HAL.UInt4;
   subtype ICM_ICM_ISR_RSU_Field is HAL.UInt4;

   --  Interrupt Status Register
   type ICM_ICM_ISR_Register is record
      --  Read-only. Region Hash Completed
      RHC            : ICM_ICM_ISR_RHC_Field;
      --  Read-only. Region Digest Mismatch
      RDM            : ICM_ICM_ISR_RDM_Field;
      --  Read-only. Region Bus Error
      RBE            : ICM_ICM_ISR_RBE_Field;
      --  Read-only. Region Wrap Condition Detected
      RWC            : ICM_ICM_ISR_RWC_Field;
      --  Read-only. Region End bit Condition Detected
      REC            : ICM_ICM_ISR_REC_Field;
      --  Read-only. Region Status Updated Detected
      RSU            : ICM_ICM_ISR_RSU_Field;
      --  Read-only. Undefined Register Access Detection Status
      URAD           : Boolean;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICM_ICM_ISR_Register use record
      RHC            at 0 range 0 .. 3;
      RDM            at 0 range 4 .. 7;
      RBE            at 0 range 8 .. 11;
      RWC            at 0 range 12 .. 15;
      REC            at 0 range 16 .. 19;
      RSU            at 0 range 20 .. 23;
      URAD           at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  Undefined Register Access Trace
   type ICM_UASR_URAT_Field is
     (
      --  Unspecified structure member set to one detected when the descriptor
      --  is loaded.
      Unspec_Struct_Member,
      --  ICM_CFG modified during active monitoring.
      Icm_Cfg_Modified,
      --  ICM_DSCR modified during active monitoring.
      Icm_Dscr_Modified,
      --  ICM_HASH modified during active monitoring
      Icm_Hash_Modified,
      --  Write-only register read access
      Read_Access)
     with Size => 3;
   for ICM_UASR_URAT_Field use
     (Unspec_Struct_Member => 0,
      Icm_Cfg_Modified => 1,
      Icm_Dscr_Modified => 2,
      Icm_Hash_Modified => 3,
      Read_Access => 4);

   --  Undefined Access Status Register
   type ICM_ICM_UASR_Register is record
      --  Read-only. Undefined Register Access Trace
      URAT          : ICM_UASR_URAT_Field;
      --  unspecified
      Reserved_3_31 : HAL.UInt29;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICM_ICM_UASR_Register use record
      URAT          at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype ICM_ICM_DSCR_DASA_Field is HAL.UInt26;

   --  Region Descriptor Area Start Address Register
   type ICM_ICM_DSCR_Register is record
      --  unspecified
      Reserved_0_5 : HAL.UInt6 := 16#0#;
      --  Descriptor Area Start Address
      DASA         : ICM_ICM_DSCR_DASA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICM_ICM_DSCR_Register use record
      Reserved_0_5 at 0 range 0 .. 5;
      DASA         at 0 range 6 .. 31;
   end record;

   subtype ICM_ICM_HASH_HASA_Field is HAL.UInt25;

   --  Region Hash Area Start Address Register
   type ICM_ICM_HASH_Register is record
      --  unspecified
      Reserved_0_6 : HAL.UInt7 := 16#0#;
      --  Hash Area Start Address
      HASA         : ICM_ICM_HASH_HASA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICM_ICM_HASH_Register use record
      Reserved_0_6 at 0 range 0 .. 6;
      HASA         at 0 range 7 .. 31;
   end record;

   --  User Initial Hash Value 0 Register 0

   --  User Initial Hash Value 0 Register 0
   type ICM_ICM_UIHVAL_Registers is array (0 .. 7) of HAL.UInt32
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   --  Integrity Check Monitor
   type ICM_Peripheral is record
      --  Configuration Register
      ICM_CFG    : aliased ICM_ICM_CFG_Register;
      --  Control Register
      ICM_CTRL   : aliased ICM_ICM_CTRL_Register;
      --  Status Register
      ICM_SR     : aliased ICM_ICM_SR_Register;
      --  Interrupt Enable Register
      ICM_IER    : aliased ICM_ICM_IER_Register;
      --  Interrupt Disable Register
      ICM_IDR    : aliased ICM_ICM_IDR_Register;
      --  Interrupt Mask Register
      ICM_IMR    : aliased ICM_ICM_IMR_Register;
      --  Interrupt Status Register
      ICM_ISR    : aliased ICM_ICM_ISR_Register;
      --  Undefined Access Status Register
      ICM_UASR   : aliased ICM_ICM_UASR_Register;
      --  Region Descriptor Area Start Address Register
      ICM_DSCR   : aliased ICM_ICM_DSCR_Register;
      --  Region Hash Area Start Address Register
      ICM_HASH   : aliased ICM_ICM_HASH_Register;
      --  User Initial Hash Value 0 Register 0
      ICM_UIHVAL : aliased ICM_ICM_UIHVAL_Registers;
   end record
     with Volatile;

   for ICM_Peripheral use record
      ICM_CFG    at 16#0# range 0 .. 31;
      ICM_CTRL   at 16#4# range 0 .. 31;
      ICM_SR     at 16#8# range 0 .. 31;
      ICM_IER    at 16#10# range 0 .. 31;
      ICM_IDR    at 16#14# range 0 .. 31;
      ICM_IMR    at 16#18# range 0 .. 31;
      ICM_ISR    at 16#1C# range 0 .. 31;
      ICM_UASR   at 16#20# range 0 .. 31;
      ICM_DSCR   at 16#30# range 0 .. 31;
      ICM_HASH   at 16#34# range 0 .. 31;
      ICM_UIHVAL at 16#38# range 0 .. 255;
   end record;

   --  Integrity Check Monitor
   ICM_Periph : aliased ICM_Peripheral
     with Import, Address => System'To_Address (16#40048000#);

end SAM_SVD.ICM;
