--  This spec has been automatically generated from cm7.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package Cortex_M_SVD.Debug is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Debug Fault Status Register
   type DFSR_Register is record
      HALTED        : Boolean := False;
      --  BKPT instruction executed or breakpoint match in FPB.
      BKPT          : Boolean := False;
      --  Data Watchpoint and Trace trap. Indicates that the core halted due to
      --  at least one DWT trap event.
      DWTTRAP       : Boolean := False;
      --  Vector catch triggered. Corresponding FSR will contain the primary
      --  cause of the exception.
      VCATCH        : Boolean := False;
      --  An asynchronous exception generated due to the assertion of EDBGRQ.
      EXTERNAL      : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSR_Register use record
      HALTED        at 0 range 0 .. 0;
      BKPT          at 0 range 1 .. 1;
      DWTTRAP       at 0 range 2 .. 2;
      VCATCH        at 0 range 3 .. 3;
      EXTERNAL      at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   -------------------------------
   -- DHCSR cluster's Registers --
   -------------------------------

   type Read_DHCSR_Register is record
      --  Read-only.
      C_DEBUGGEN     : Boolean;
      --  Read-only.
      C_HALT         : Boolean;
      --  Read-only.
      C_STEP         : Boolean;
      --  Read-only.
      C_MASKINTS     : Boolean;
      --  unspecified
      Reserved_4_4   : HAL.Bit;
      --  Read-only.
      C_SNAPSTALL    : Boolean;
      --  unspecified
      Reserved_6_15  : HAL.UInt10;
      --  Read-only.
      S_REGRDY       : Boolean;
      --  Read-only.
      S_HALT         : Boolean;
      --  Read-only.
      S_SLEEP        : Boolean;
      --  Read-only.
      S_LOCKUP       : Boolean;
      --  unspecified
      Reserved_20_23 : HAL.UInt4;
      --  Read-only.
      S_RETIRE_ST    : Boolean;
      --  Read-only.
      S_RESET_ST     : Boolean;
      --  unspecified
      Reserved_26_31 : HAL.UInt6;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for Read_DHCSR_Register use record
      C_DEBUGGEN     at 0 range 0 .. 0;
      C_HALT         at 0 range 1 .. 1;
      C_STEP         at 0 range 2 .. 2;
      C_MASKINTS     at 0 range 3 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      C_SNAPSTALL    at 0 range 5 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      S_REGRDY       at 0 range 16 .. 16;
      S_HALT         at 0 range 17 .. 17;
      S_SLEEP        at 0 range 18 .. 18;
      S_LOCKUP       at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      S_RETIRE_ST    at 0 range 24 .. 24;
      S_RESET_ST     at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype Write_DHCSR_S_RESET_ST_Field is HAL.UInt16;

   type Write_DHCSR_Register is record
      --  Write-only.
      C_DEBUGGEN    : Boolean := False;
      --  Write-only.
      C_HALT        : Boolean := False;
      --  Write-only.
      C_STEP        : Boolean := False;
      --  Write-only.
      C_MASKINTS    : Boolean := False;
      --  unspecified
      Reserved_4_4  : HAL.Bit := 16#0#;
      --  Write-only.
      C_SNAPSTALL   : Boolean := False;
      --  unspecified
      Reserved_6_15 : HAL.UInt10 := 16#0#;
      --  Write-only. Debug Key. The value 0xA05F must be written to enable
      --  write accesses to bits [15:0], otherwise the write access will be
      --  ignored. Read behavior of bits [31:16] is as listed below.
      S_RESET_ST    : Write_DHCSR_S_RESET_ST_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for Write_DHCSR_Register use record
      C_DEBUGGEN    at 0 range 0 .. 0;
      C_HALT        at 0 range 1 .. 1;
      C_STEP        at 0 range 2 .. 2;
      C_MASKINTS    at 0 range 3 .. 3;
      Reserved_4_4  at 0 range 4 .. 4;
      C_SNAPSTALL   at 0 range 5 .. 5;
      Reserved_6_15 at 0 range 6 .. 15;
      S_RESET_ST    at 0 range 16 .. 31;
   end record;

   type DHCSR_Disc is
     (
      Mode_1,
      Mode_2);

   --  Debug Halting Control and Status Register
   type DHCSR_Cluster
     (Discriminent : DHCSR_Disc := Mode_1)
   is record
      case Discriminent is
         when Mode_1 =>
            Read : aliased Read_DHCSR_Register;
         when Mode_2 =>
            Write : aliased Write_DHCSR_Register;
      end case;
   end record
     with Unchecked_Union, Volatile, Size => 32;

   for DHCSR_Cluster use record
      Read  at 0 range 0 .. 31;
      Write at 0 range 0 .. 31;
   end record;

   type DCRSR_HALTED_Field is
     (
      Register_0,
      Register_1,
      Register_2,
      Register_3,
      Register_4,
      Register_5,
      Register_6,
      Register_7,
      Register_8,
      Register_9,
      Register_10,
      Register_11,
      Register_12,
      Current_Sp,
      Link_Rregister,
      Debug_Return_Address,
      XPsr,
      Msp,
      Psp,
      Control_Faultmask_Basepri_Primask)
     with Size => 5;
   for DCRSR_HALTED_Field use
     (Register_0 => 0,
      Register_1 => 1,
      Register_2 => 2,
      Register_3 => 3,
      Register_4 => 4,
      Register_5 => 5,
      Register_6 => 6,
      Register_7 => 7,
      Register_8 => 8,
      Register_9 => 9,
      Register_10 => 10,
      Register_11 => 11,
      Register_12 => 12,
      Current_Sp => 13,
      Link_Rregister => 14,
      Debug_Return_Address => 15,
      XPsr => 16,
      Msp => 17,
      Psp => 18,
      Control_Faultmask_Basepri_Primask => 19);

   type DCRSR_REGWnR_Field is
     (
      Read,
      Write)
     with Size => 1;
   for DCRSR_REGWnR_Field use
     (Read => 0,
      Write => 1);

   --  Debug Core Register Selector Register: The DCRSR write-only register
   --  generates a handshake to the core to transfer the selected register
   --  to/from the DCRDR. The DHCSR S_REGRDY bit is cleared when the DCRSR is
   --  written, and remains clear until the core transaction completes. This
   --  register is only accessible from Debug state.
   type DCRSR_Register is record
      --  Write-only.
      HALTED         : DCRSR_HALTED_Field := Cortex_M_SVD.Debug.Register_0;
      --  unspecified
      Reserved_5_15  : HAL.UInt11 := 16#0#;
      --  Write-only.
      REGWnR         : DCRSR_REGWnR_Field := Cortex_M_SVD.Debug.Read;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCRSR_Register use record
      HALTED         at 0 range 0 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      REGWnR         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  Debug Exception and Monitor Control Register
   type DEMCR_Register is record
      VC_CORERESET   : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      VC_MMERR       : Boolean := False;
      VC_NOCPERR     : Boolean := False;
      VC_CHKERR      : Boolean := False;
      VC_STATERR     : Boolean := False;
      VC_BUSERR      : Boolean := False;
      VC_INTERR      : Boolean := False;
      VC_HARDERR     : Boolean := False;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      MON_EN         : Boolean := False;
      MON_PEND       : Boolean := False;
      MON_STEP       : Boolean := False;
      MON_REQ        : Boolean := False;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      TRCENA         : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEMCR_Register use record
      VC_CORERESET   at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      VC_MMERR       at 0 range 4 .. 4;
      VC_NOCPERR     at 0 range 5 .. 5;
      VC_CHKERR      at 0 range 6 .. 6;
      VC_STATERR     at 0 range 7 .. 7;
      VC_BUSERR      at 0 range 8 .. 8;
      VC_INTERR      at 0 range 9 .. 9;
      VC_HARDERR     at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      MON_EN         at 0 range 16 .. 16;
      MON_PEND       at 0 range 17 .. 17;
      MON_STEP       at 0 range 18 .. 18;
      MON_REQ        at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      TRCENA         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type Debug_Peripheral is record
      --  Debug Fault Status Register
      DFSR  : aliased DFSR_Register;
      --  Debug Halting Control and Status Register
      DHCSR : aliased DHCSR_Cluster;
      --  Debug Core Register Selector Register: The DCRSR write-only register
      --  generates a handshake to the core to transfer the selected register
      --  to/from the DCRDR. The DHCSR S_REGRDY bit is cleared when the DCRSR
      --  is written, and remains clear until the core transaction completes.
      --  This register is only accessible from Debug state.
      DCRSR : aliased DCRSR_Register;
      --  Debug Core Register Data Register
      DCRDR : aliased HAL.UInt32;
      --  Debug Exception and Monitor Control Register
      DEMCR : aliased DEMCR_Register;
   end record
     with Volatile;

   for Debug_Peripheral use record
      DFSR  at 16#30# range 0 .. 31;
      DHCSR at 16#F0# range 0 .. 31;
      DCRSR at 16#F4# range 0 .. 31;
      DCRDR at 16#F8# range 0 .. 31;
      DEMCR at 16#FC# range 0 .. 31;
   end record;

   Debug_Periph : aliased Debug_Peripheral
     with Import, Address => Debug_Base;

end Cortex_M_SVD.Debug;
