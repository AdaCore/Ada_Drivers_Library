--  This spec has been automatically generated from STM32F7x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with HAL;
with System;

package STM32_SVD.SPDIF_RX is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -----------------
   -- CR_Register --
   -----------------

   subtype CR_SPDIFEN_Field is HAL.UInt2;
   subtype CR_DRFMT_Field is HAL.UInt2;
   subtype CR_NBTR_Field is HAL.UInt2;
   subtype CR_INSEL_Field is HAL.UInt3;

   --  Control register
   type CR_Register is record
      --  Peripheral Block Enable
      SPDIFEN        : CR_SPDIFEN_Field := 16#0#;
      --  Receiver DMA ENable for data flow
      RXDMAEN        : Boolean := False;
      --  STerEO Mode
      RXSTEO         : Boolean := False;
      --  RX Data format
      DRFMT          : CR_DRFMT_Field := 16#0#;
      --  Mask Parity error bit
      PMSK           : Boolean := False;
      --  Mask of Validity bit
      VMSK           : Boolean := False;
      --  Mask of channel status and user bits
      CUMSK          : Boolean := False;
      --  Mask of Preamble Type bits
      PTMSK          : Boolean := False;
      --  Control Buffer DMA ENable for control flow
      CBDMAEN        : Boolean := False;
      --  Channel Selection
      CHSEL          : Boolean := False;
      --  Maximum allowed re-tries during synchronization phase
      NBTR           : CR_NBTR_Field := 16#0#;
      --  Wait For Activity
      WFA            : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  input selection
      INSEL          : CR_INSEL_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      SPDIFEN        at 0 range 0 .. 1;
      RXDMAEN        at 0 range 2 .. 2;
      RXSTEO         at 0 range 3 .. 3;
      DRFMT          at 0 range 4 .. 5;
      PMSK           at 0 range 6 .. 6;
      VMSK           at 0 range 7 .. 7;
      CUMSK          at 0 range 8 .. 8;
      PTMSK          at 0 range 9 .. 9;
      CBDMAEN        at 0 range 10 .. 10;
      CHSEL          at 0 range 11 .. 11;
      NBTR           at 0 range 12 .. 13;
      WFA            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      INSEL          at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   ------------------
   -- IMR_Register --
   ------------------

   --  Interrupt mask register
   type IMR_Register is record
      --  RXNE interrupt enable
      RXNEIE        : Boolean := False;
      --  Control Buffer Ready Interrupt Enable
      CSRNEIE       : Boolean := False;
      --  Parity error interrupt enable
      PERRIE        : Boolean := False;
      --  Overrun error Interrupt Enable
      OVRIE         : Boolean := False;
      --  Synchronization Block Detected Interrupt Enable
      SBLKIE        : Boolean := False;
      --  Synchronization Done
      SYNCDIE       : Boolean := False;
      --  Serial Interface Error Interrupt Enable
      IFEIE         : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMR_Register use record
      RXNEIE        at 0 range 0 .. 0;
      CSRNEIE       at 0 range 1 .. 1;
      PERRIE        at 0 range 2 .. 2;
      OVRIE         at 0 range 3 .. 3;
      SBLKIE        at 0 range 4 .. 4;
      SYNCDIE       at 0 range 5 .. 5;
      IFEIE         at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   -----------------
   -- SR_Register --
   -----------------

   subtype SR_WIDTH5_Field is HAL.UInt15;

   --  Status register
   type SR_Register is record
      --  Read-only. Read data register not empty
      RXNE           : Boolean;
      --  Read-only. Control Buffer register is not empty
      CSRNE          : Boolean;
      --  Read-only. Parity error
      PERR           : Boolean;
      --  Read-only. Overrun error
      OVR            : Boolean;
      --  Read-only. Synchronization Block Detected
      SBD            : Boolean;
      --  Read-only. Synchronization Done
      SYNCD          : Boolean;
      --  Read-only. Framing error
      FERR           : Boolean;
      --  Read-only. Synchronization error
      SERR           : Boolean;
      --  Read-only. Time-out error
      TERR           : Boolean;
      --  unspecified
      Reserved_9_15  : HAL.UInt7;
      --  Read-only. Duration of 5 symbols counted with SPDIF_CLK
      WIDTH5         : SR_WIDTH5_Field;
      --  unspecified
      Reserved_31_31 : HAL.Bit;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      RXNE           at 0 range 0 .. 0;
      CSRNE          at 0 range 1 .. 1;
      PERR           at 0 range 2 .. 2;
      OVR            at 0 range 3 .. 3;
      SBD            at 0 range 4 .. 4;
      SYNCD          at 0 range 5 .. 5;
      FERR           at 0 range 6 .. 6;
      SERR           at 0 range 7 .. 7;
      TERR           at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      WIDTH5         at 0 range 16 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   -------------------
   -- IFCR_Register --
   -------------------

   --  Interrupt Flag Clear register
   type IFCR_Register is record
      --  unspecified
      Reserved_0_1  : HAL.UInt2 := 16#0#;
      --  Write-only. Clears the Parity error flag
      PERRCF        : Boolean := False;
      --  Write-only. Clears the Overrun error flag
      OVRCF         : Boolean := False;
      --  Write-only. Clears the Synchronization Block Detected flag
      SBDCF         : Boolean := False;
      --  Write-only. Clears the Synchronization Done flag
      SYNCDCF       : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IFCR_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      PERRCF        at 0 range 2 .. 2;
      OVRCF         at 0 range 3 .. 3;
      SBDCF         at 0 range 4 .. 4;
      SYNCDCF       at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   -----------------
   -- DR_Register --
   -----------------

   subtype DR_DR_Field is HAL.UInt24;
   subtype DR_PT_Field is HAL.UInt2;

   --  Data input register
   type DR_Register is record
      --  Read-only. Parity Error bit
      DR             : DR_DR_Field;
      --  Read-only. Parity Error bit
      PE             : Boolean;
      --  Read-only. Validity bit
      V              : Boolean;
      --  Read-only. User bit
      U              : Boolean;
      --  Read-only. Channel Status bit
      C              : Boolean;
      --  Read-only. Preamble Type
      PT             : DR_PT_Field;
      --  unspecified
      Reserved_30_31 : HAL.UInt2;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR_Register use record
      DR             at 0 range 0 .. 23;
      PE             at 0 range 24 .. 24;
      V              at 0 range 25 .. 25;
      U              at 0 range 26 .. 26;
      C              at 0 range 27 .. 27;
      PT             at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   ------------------
   -- CSR_Register --
   ------------------

   subtype CSR_USR_Field is HAL.Short;
   subtype CSR_CS_Field is HAL.Byte;

   --  Channel Status register
   type CSR_Register is record
      --  Read-only. User data information
      USR            : CSR_USR_Field;
      --  Read-only. Channel A status information
      CS             : CSR_CS_Field;
      --  Read-only. Start Of Block
      SOB            : Boolean;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR_Register use record
      USR            at 0 range 0 .. 15;
      CS             at 0 range 16 .. 23;
      SOB            at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   ------------------
   -- DIR_Register --
   ------------------

   subtype DIR_THI_Field is HAL.UInt13;
   subtype DIR_TLO_Field is HAL.UInt13;

   --  Debug Information register
   type DIR_Register is record
      --  Read-only. Threshold HIGH
      THI            : DIR_THI_Field;
      --  unspecified
      Reserved_13_15 : HAL.UInt3;
      --  Read-only. Threshold LOW
      TLO            : DIR_TLO_Field;
      --  unspecified
      Reserved_29_31 : HAL.UInt3;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIR_Register use record
      THI            at 0 range 0 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      TLO            at 0 range 16 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Receiver Interface
   type SPDIF_RX_Peripheral is record
      --  Control register
      CR   : CR_Register;
      --  Interrupt mask register
      IMR  : IMR_Register;
      --  Status register
      SR   : SR_Register;
      --  Interrupt Flag Clear register
      IFCR : IFCR_Register;
      --  Data input register
      DR   : DR_Register;
      --  Channel Status register
      CSR  : CSR_Register;
      --  Debug Information register
      DIR  : DIR_Register;
   end record
     with Volatile;

   for SPDIF_RX_Peripheral use record
      CR   at 0 range 0 .. 31;
      IMR  at 4 range 0 .. 31;
      SR   at 8 range 0 .. 31;
      IFCR at 12 range 0 .. 31;
      DR   at 16 range 0 .. 31;
      CSR  at 20 range 0 .. 31;
      DIR  at 24 range 0 .. 31;
   end record;

   --  Receiver Interface
   SPDIF_RX_Periph : aliased SPDIF_RX_Peripheral
     with Import, Address => SPDIF_RX_Base;

end STM32_SVD.SPDIF_RX;
