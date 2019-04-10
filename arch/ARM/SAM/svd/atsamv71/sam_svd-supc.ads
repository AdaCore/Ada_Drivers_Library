--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.SUPC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Voltage Regulator Off
   type SUPC_CR_VROFF_Field is
     (
      --  No effect.
      No_Effect,
      --  If KEY is correct, VROFF asserts the vddcore_nreset and stops the
      --  voltage regulator.
      Stop_Vreg)
     with Size => 1;
   for SUPC_CR_VROFF_Field use
     (No_Effect => 0,
      Stop_Vreg => 1);

   --  Crystal Oscillator Select
   type SUPC_CR_XTALSEL_Field is
     (
      --  No effect.
      No_Effect,
      --  If KEY is correct, XTALSEL switches the slow clock on the crystal
      --  oscillator output.
      Crystal_Sel)
     with Size => 1;
   for SUPC_CR_XTALSEL_Field use
     (No_Effect => 0,
      Crystal_Sel => 1);

   --  Password
   type SUPC_CR_KEY_Field is
     (
      --  Reset value for the field
      Supc_Cr_Key_Field_Reset,
      --  Writing any other value in this field aborts the write operation.
      Passwd)
     with Size => 8;
   for SUPC_CR_KEY_Field use
     (Supc_Cr_Key_Field_Reset => 0,
      Passwd => 165);

   --  Supply Controller Control Register
   type SUPC_SUPC_CR_Register is record
      --  unspecified
      Reserved_0_1  : HAL.UInt2 := 16#0#;
      --  Write-only. Voltage Regulator Off
      VROFF         : SUPC_CR_VROFF_Field := SAM_SVD.SUPC.No_Effect;
      --  Write-only. Crystal Oscillator Select
      XTALSEL       : SUPC_CR_XTALSEL_Field := SAM_SVD.SUPC.No_Effect;
      --  unspecified
      Reserved_4_23 : HAL.UInt20 := 16#0#;
      --  Write-only. Password
      KEY           : SUPC_CR_KEY_Field := Supc_Cr_Key_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_SUPC_CR_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      VROFF         at 0 range 2 .. 2;
      XTALSEL       at 0 range 3 .. 3;
      Reserved_4_23 at 0 range 4 .. 23;
      KEY           at 0 range 24 .. 31;
   end record;

   subtype SUPC_SUPC_SMMR_SMTH_Field is HAL.UInt4;

   --  Supply Monitor Sampling Period
   type SUPC_SMMR_SMSMPL_Field is
     (
      --  Supply Monitor disabled
      Smd,
      --  Continuous Supply Monitor
      Csm,
      --  Supply Monitor enabled one SLCK period every 32 SLCK periods
      Val_32Slck,
      --  Supply Monitor enabled one SLCK period every 256 SLCK periods
      Val_256Slck,
      --  Supply Monitor enabled one SLCK period every 2,048 SLCK periods
      Val_2048Slck)
     with Size => 3;
   for SUPC_SMMR_SMSMPL_Field use
     (Smd => 0,
      Csm => 1,
      Val_32Slck => 2,
      Val_256Slck => 3,
      Val_2048Slck => 4);

   --  Supply Monitor Reset Enable
   type SUPC_SMMR_SMRSTEN_Field is
     (
      --  The core reset signal vddcore_nreset is not affected when a supply
      --  monitor detection occurs.
      Not_Enable,
      --  The core reset signal, vddcore_nreset is asserted when a supply
      --  monitor detection occurs.
      Enable)
     with Size => 1;
   for SUPC_SMMR_SMRSTEN_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Supply Monitor Interrupt Enable
   type SUPC_SMMR_SMIEN_Field is
     (
      --  The SUPC interrupt signal is not affected when a supply monitor
      --  detection occurs.
      Not_Enable,
      --  The SUPC interrupt signal is asserted when a supply monitor detection
      --  occurs.
      Enable)
     with Size => 1;
   for SUPC_SMMR_SMIEN_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Supply Controller Supply Monitor Mode Register
   type SUPC_SUPC_SMMR_Register is record
      --  Supply Monitor Threshold
      SMTH           : SUPC_SUPC_SMMR_SMTH_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Supply Monitor Sampling Period
      SMSMPL         : SUPC_SMMR_SMSMPL_Field := SAM_SVD.SUPC.Smd;
      --  unspecified
      Reserved_11_11 : HAL.Bit := 16#0#;
      --  Supply Monitor Reset Enable
      SMRSTEN        : SUPC_SMMR_SMRSTEN_Field := SAM_SVD.SUPC.Not_Enable;
      --  Supply Monitor Interrupt Enable
      SMIEN          : SUPC_SMMR_SMIEN_Field := SAM_SVD.SUPC.Not_Enable;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_SUPC_SMMR_Register use record
      SMTH           at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      SMSMPL         at 0 range 8 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      SMRSTEN        at 0 range 12 .. 12;
      SMIEN          at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  Brownout Detector Reset Enable
   type SUPC_MR_BODRSTEN_Field is
     (
      --  The core reset signal vddcore_nreset is not affected when a brownout
      --  detection occurs.
      Not_Enable,
      --  The core reset signal, vddcore_nreset is asserted when a brownout
      --  detection occurs.
      Enable)
     with Size => 1;
   for SUPC_MR_BODRSTEN_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Brownout Detector Disable
   type SUPC_MR_BODDIS_Field is
     (
      --  The core brownout detector is enabled.
      Enable,
      --  The core brownout detector is disabled.
      Disable)
     with Size => 1;
   for SUPC_MR_BODDIS_Field use
     (Enable => 0,
      Disable => 1);

   --  Voltage Regulator Enable
   type SUPC_MR_ONREG_Field is
     (
      --  Internal voltage regulator is not used (external power supply is
      --  used).
      Onreg_Unused,
      --  Internal voltage regulator is used.
      Onreg_Used)
     with Size => 1;
   for SUPC_MR_ONREG_Field use
     (Onreg_Unused => 0,
      Onreg_Used => 1);

   --  Oscillator Bypass
   type SUPC_MR_OSCBYPASS_Field is
     (
      --  No effect. Clock selection depends on the value of XTALSEL (SUPC_CR).
      No_Effect,
      --  The 32 kHz crystal oscillator is bypassed if XTALSEL (SUPC_CR) is
      --  set. OSCBYPASS must be set prior to setting XTALSEL.
      Bypass)
     with Size => 1;
   for SUPC_MR_OSCBYPASS_Field use
     (No_Effect => 0,
      Bypass => 1);

   --  Password Key
   type SUPC_MR_KEY_Field is
     (
      --  Reset value for the field
      Supc_Mr_Key_Field_Reset,
      --  Writing any other value in this field aborts the write operation.
      Passwd)
     with Size => 8;
   for SUPC_MR_KEY_Field use
     (Supc_Mr_Key_Field_Reset => 0,
      Passwd => 165);

   --  Supply Controller Mode Register
   type SUPC_SUPC_MR_Register is record
      --  unspecified
      Reserved_0_11  : HAL.UInt12 := 16#0#;
      --  Brownout Detector Reset Enable
      BODRSTEN       : SUPC_MR_BODRSTEN_Field := SAM_SVD.SUPC.Not_Enable;
      --  Brownout Detector Disable
      BODDIS         : SUPC_MR_BODDIS_Field := SAM_SVD.SUPC.Enable;
      --  Voltage Regulator Enable
      ONREG          : SUPC_MR_ONREG_Field := SAM_SVD.SUPC.Onreg_Unused;
      --  unspecified
      Reserved_15_16 : HAL.UInt2 := 16#0#;
      --  SRAM On In Backup Mode
      BKUPRETON      : Boolean := False;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Oscillator Bypass
      OSCBYPASS      : SUPC_MR_OSCBYPASS_Field := SAM_SVD.SUPC.No_Effect;
      --  unspecified
      Reserved_21_23 : HAL.UInt3 := 16#0#;
      --  Password Key
      KEY            : SUPC_MR_KEY_Field := Supc_Mr_Key_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_SUPC_MR_Register use record
      Reserved_0_11  at 0 range 0 .. 11;
      BODRSTEN       at 0 range 12 .. 12;
      BODDIS         at 0 range 13 .. 13;
      ONREG          at 0 range 14 .. 14;
      Reserved_15_16 at 0 range 15 .. 16;
      BKUPRETON      at 0 range 17 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      OSCBYPASS      at 0 range 20 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      KEY            at 0 range 24 .. 31;
   end record;

   --  Supply Monitor Wakeup Enable
   type SUPC_WUMR_SMEN_Field is
     (
      --  The supply monitor detection has no wakeup effect.
      Not_Enable,
      --  The supply monitor detection forces the wakeup of the core power
      --  supply.
      Enable)
     with Size => 1;
   for SUPC_WUMR_SMEN_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Real-time Timer Wakeup Enable
   type SUPC_WUMR_RTTEN_Field is
     (
      --  The RTT alarm signal has no wakeup effect.
      Not_Enable,
      --  The RTT alarm signal forces the wakeup of the core power supply.
      Enable)
     with Size => 1;
   for SUPC_WUMR_RTTEN_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Real-time Clock Wakeup Enable
   type SUPC_WUMR_RTCEN_Field is
     (
      --  The RTC alarm signal has no wakeup effect.
      Not_Enable,
      --  The RTC alarm signal forces the wakeup of the core power supply.
      Enable)
     with Size => 1;
   for SUPC_WUMR_RTCEN_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Low-power Debouncer Enable WKUP0
   type SUPC_WUMR_LPDBCEN0_Field is
     (
      --  The WKUP0 input pin is not connected to the low-power debouncer.
      Not_Enable,
      --  The WKUP0 input pin is connected to the low-power debouncer and
      --  forces a system wakeup.
      Enable)
     with Size => 1;
   for SUPC_WUMR_LPDBCEN0_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Low-power Debouncer Enable WKUP1
   type SUPC_WUMR_LPDBCEN1_Field is
     (
      --  The WKUP1 input pin is not connected to the low-power debouncer.
      Not_Enable,
      --  The WKUP1 input pin is connected to the low-power debouncer and
      --  forces a system wakeup.
      Enable)
     with Size => 1;
   for SUPC_WUMR_LPDBCEN1_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Low-power Debouncer Clear
   type SUPC_WUMR_LPDBCCLR_Field is
     (
      --  A low-power debounce event does not create an immediate clear on the
      --  first half of GPBR registers.
      Not_Enable,
      --  A low-power debounce event on WKUP0 or WKUP1 generates an immediate
      --  clear on the first half of GPBR registers.
      Enable)
     with Size => 1;
   for SUPC_WUMR_LPDBCCLR_Field use
     (Not_Enable => 0,
      Enable => 1);

   --  Wakeup Inputs Debouncer Period
   type SUPC_WUMR_WKUPDBC_Field is
     (
      --  Immediate, no debouncing, detected active at least on one Slow Clock
      --  edge.
      Immediate,
      --  WKUPx shall be in its active state for at least 3 SLCK periods
      Val_3_Slck,
      --  WKUPx shall be in its active state for at least 32 SLCK periods
      Val_32_Slck,
      --  WKUPx shall be in its active state for at least 512 SLCK periods
      Val_512_Slck,
      --  WKUPx shall be in its active state for at least 4,096 SLCK periods
      Val_4096_Slck,
      --  WKUPx shall be in its active state for at least 32,768 SLCK periods
      Val_32768_Slck)
     with Size => 3;
   for SUPC_WUMR_WKUPDBC_Field use
     (Immediate => 0,
      Val_3_Slck => 1,
      Val_32_Slck => 2,
      Val_512_Slck => 3,
      Val_4096_Slck => 4,
      Val_32768_Slck => 5);

   --  Low-power Debouncer Period
   type SUPC_WUMR_LPDBC_Field is
     (
      --  Disables the low-power debouncers.
      Disable,
      --  WKUP0/1 in active state for at least 2 RTCOUTx clock periods
      Val_2_Rtcout,
      --  WKUP0/1 in active state for at least 3 RTCOUTx clock periods
      Val_3_Rtcout,
      --  WKUP0/1 in active state for at least 4 RTCOUTx clock periods
      Val_4_Rtcout,
      --  WKUP0/1 in active state for at least 5 RTCOUTx clock periods
      Val_5_Rtcout,
      --  WKUP0/1 in active state for at least 6 RTCOUTx clock periods
      Val_6_Rtcout,
      --  WKUP0/1 in active state for at least 7 RTCOUTx clock periods
      Val_7_Rtcout,
      --  WKUP0/1 in active state for at least 8 RTCOUTx clock periods
      Val_8_Rtcout)
     with Size => 3;
   for SUPC_WUMR_LPDBC_Field use
     (Disable => 0,
      Val_2_Rtcout => 1,
      Val_3_Rtcout => 2,
      Val_4_Rtcout => 3,
      Val_5_Rtcout => 4,
      Val_6_Rtcout => 5,
      Val_7_Rtcout => 6,
      Val_8_Rtcout => 7);

   --  Supply Controller Wakeup Mode Register
   type SUPC_SUPC_WUMR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Supply Monitor Wakeup Enable
      SMEN           : SUPC_WUMR_SMEN_Field := SAM_SVD.SUPC.Not_Enable;
      --  Real-time Timer Wakeup Enable
      RTTEN          : SUPC_WUMR_RTTEN_Field := SAM_SVD.SUPC.Not_Enable;
      --  Real-time Clock Wakeup Enable
      RTCEN          : SUPC_WUMR_RTCEN_Field := SAM_SVD.SUPC.Not_Enable;
      --  unspecified
      Reserved_4_4   : HAL.Bit := 16#0#;
      --  Low-power Debouncer Enable WKUP0
      LPDBCEN0       : SUPC_WUMR_LPDBCEN0_Field := SAM_SVD.SUPC.Not_Enable;
      --  Low-power Debouncer Enable WKUP1
      LPDBCEN1       : SUPC_WUMR_LPDBCEN1_Field := SAM_SVD.SUPC.Not_Enable;
      --  Low-power Debouncer Clear
      LPDBCCLR       : SUPC_WUMR_LPDBCCLR_Field := SAM_SVD.SUPC.Not_Enable;
      --  unspecified
      Reserved_8_11  : HAL.UInt4 := 16#0#;
      --  Wakeup Inputs Debouncer Period
      WKUPDBC        : SUPC_WUMR_WKUPDBC_Field := SAM_SVD.SUPC.Immediate;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Low-power Debouncer Period
      LPDBC          : SUPC_WUMR_LPDBC_Field := SAM_SVD.SUPC.Disable;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_SUPC_WUMR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      SMEN           at 0 range 1 .. 1;
      RTTEN          at 0 range 2 .. 2;
      RTCEN          at 0 range 3 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      LPDBCEN0       at 0 range 5 .. 5;
      LPDBCEN1       at 0 range 6 .. 6;
      LPDBCCLR       at 0 range 7 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      WKUPDBC        at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LPDBC          at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  Wakeup Input Enable 0 to 0
   type SUPC_WUIR_WKUPEN0_Field is
     (
      --  The corresponding wakeup input has no wakeup effect.
      Disable,
      --  The corresponding wakeup input is enabled for a wakeup of the core
      --  power supply.
      Enable)
     with Size => 1;
   for SUPC_WUIR_WKUPEN0_Field use
     (Disable => 0,
      Enable => 1);

   --  SUPC_SUPC_WUIR_WKUPEN array
   type SUPC_SUPC_WUIR_WKUPEN_Field_Array is array (0 .. 13)
     of SUPC_WUIR_WKUPEN0_Field
     with Component_Size => 1, Size => 14;

   --  Type definition for SUPC_SUPC_WUIR_WKUPEN
   type SUPC_SUPC_WUIR_WKUPEN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WKUPEN as a value
            Val : HAL.UInt14;
         when True =>
            --  WKUPEN as an array
            Arr : SUPC_SUPC_WUIR_WKUPEN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 14;

   for SUPC_SUPC_WUIR_WKUPEN_Field use record
      Val at 0 range 0 .. 13;
      Arr at 0 range 0 .. 13;
   end record;

   --  Wakeup Input Type 0 to 0
   type SUPC_WUIR_WKUPT0_Field is
     (
      --  A falling edge followed by a low level for a period defined by
      --  WKUPDBC on the corre-sponding wakeup input forces the wakeup of the
      --  core power supply.
      Low,
      --  A rising edge followed by a high level for a period defined by
      --  WKUPDBC on the cor-responding wakeup input forces the wakeup of the
      --  core power supply.
      High)
     with Size => 1;
   for SUPC_WUIR_WKUPT0_Field use
     (Low => 0,
      High => 1);

   --  SUPC_SUPC_WUIR_WKUPT array
   type SUPC_SUPC_WUIR_WKUPT_Field_Array is array (0 .. 13)
     of SUPC_WUIR_WKUPT0_Field
     with Component_Size => 1, Size => 14;

   --  Type definition for SUPC_SUPC_WUIR_WKUPT
   type SUPC_SUPC_WUIR_WKUPT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WKUPT as a value
            Val : HAL.UInt14;
         when True =>
            --  WKUPT as an array
            Arr : SUPC_SUPC_WUIR_WKUPT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 14;

   for SUPC_SUPC_WUIR_WKUPT_Field use record
      Val at 0 range 0 .. 13;
      Arr at 0 range 0 .. 13;
   end record;

   --  Supply Controller Wakeup Inputs Register
   type SUPC_SUPC_WUIR_Register is record
      --  Wakeup Input Enable 0 to 0
      WKUPEN         : SUPC_SUPC_WUIR_WKUPEN_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Wakeup Input Type 0 to 0
      WKUPT          : SUPC_SUPC_WUIR_WKUPT_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_SUPC_WUIR_Register use record
      WKUPEN         at 0 range 0 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      WKUPT          at 0 range 16 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  WKUP Wakeup Status (cleared on read)
   type SUPC_SR_WKUPS_Field is
     (
      --  No wakeup due to the assertion of the WKUP pins has occurred since
      --  the last read of SUPC_SR.
      No,
      --  At least one wakeup due to the assertion of the WKUP pins has
      --  occurred since the last read of SUPC_SR.
      Present)
     with Size => 1;
   for SUPC_SR_WKUPS_Field use
     (No => 0,
      Present => 1);

   --  Supply Monitor Detection Wakeup Status (cleared on read)
   type SUPC_SR_SMWS_Field is
     (
      --  No wakeup due to a supply monitor detection has occurred since the
      --  last read of SUPC_SR.
      No,
      --  At least one wakeup due to a supply monitor detection has occurred
      --  since the last read of SUPC_SR.
      Present)
     with Size => 1;
   for SUPC_SR_SMWS_Field use
     (No => 0,
      Present => 1);

   --  Brownout Detector Reset Status (cleared on read)
   type SUPC_SR_BODRSTS_Field is
     (
      --  No core brownout rising edge event has been detected since the last
      --  read of the SUPC_SR.
      No,
      --  At least one brownout output rising edge event has been detected
      --  since the last read of the SUPC_SR.
      Present)
     with Size => 1;
   for SUPC_SR_BODRSTS_Field use
     (No => 0,
      Present => 1);

   --  Supply Monitor Reset Status (cleared on read)
   type SUPC_SR_SMRSTS_Field is
     (
      --  No supply monitor detection has generated a core reset since the last
      --  read of the SUPC_SR.
      No,
      --  At least one supply monitor detection has generated a core reset
      --  since the last read of the SUPC_SR.
      Present)
     with Size => 1;
   for SUPC_SR_SMRSTS_Field use
     (No => 0,
      Present => 1);

   --  Supply Monitor Status (cleared on read)
   type SUPC_SR_SMS_Field is
     (
      --  No supply monitor detection since the last read of SUPC_SR.
      No,
      --  At least one supply monitor detection since the last read of SUPC_SR.
      Present)
     with Size => 1;
   for SUPC_SR_SMS_Field use
     (No => 0,
      Present => 1);

   --  Supply Monitor Output Status
   type SUPC_SR_SMOS_Field is
     (
      --  The supply monitor detected VDDIO higher than its threshold at its
      --  last measurement.
      High,
      --  The supply monitor detected VDDIO lower than its threshold at its
      --  last measurement.
      Low)
     with Size => 1;
   for SUPC_SR_SMOS_Field use
     (High => 0,
      Low => 1);

   --  32-kHz Oscillator Selection Status
   type SUPC_SR_OSCSEL_Field is
     (
      --  The slow clock, SLCK, is generated by the embedded 32 kHz RC
      --  oscillator.
      Rc,
      --  The slow clock, SLCK, is generated by the 32 kHz crystal oscillator.
      Cryst)
     with Size => 1;
   for SUPC_SR_OSCSEL_Field use
     (Rc => 0,
      Cryst => 1);

   --  Low-power Debouncer Wakeup Status on WKUP0 (cleared on read)
   type SUPC_SR_LPDBCS0_Field is
     (
      --  No wakeup due to the assertion of the WKUP0 pin has occurred since
      --  the last read of SUPC_SR.
      No,
      --  At least one wakeup due to the assertion of the WKUP0 pin has
      --  occurred since the last read of SUPC_SR.
      Present)
     with Size => 1;
   for SUPC_SR_LPDBCS0_Field use
     (No => 0,
      Present => 1);

   --  Low-power Debouncer Wakeup Status on WKUP1 (cleared on read)
   type SUPC_SR_LPDBCS1_Field is
     (
      --  No wakeup due to the assertion of the WKUP1 pin has occurred since
      --  the last read of SUPC_SR.
      No,
      --  At least one wakeup due to the assertion of the WKUP1 pin has
      --  occurred since the last read of SUPC_SR.
      Present)
     with Size => 1;
   for SUPC_SR_LPDBCS1_Field use
     (No => 0,
      Present => 1);

   --  WKUPx Input Status (cleared on read)
   type SUPC_SR_WKUPIS0_Field is
     (
      --  The corresponding wakeup input is disabled, or was inactive at the
      --  time the debouncer triggered a wakeup event.
      Dis,
      --  The corresponding wakeup input was active at the time the debouncer
      --  triggered a wakeup event since the last read of SUPC_SR.
      En)
     with Size => 1;
   for SUPC_SR_WKUPIS0_Field use
     (Dis => 0,
      En => 1);

   --  SUPC_SUPC_SR_WKUPIS array
   type SUPC_SUPC_SR_WKUPIS_Field_Array is array (0 .. 13)
     of SUPC_SR_WKUPIS0_Field
     with Component_Size => 1, Size => 14;

   --  Type definition for SUPC_SUPC_SR_WKUPIS
   type SUPC_SUPC_SR_WKUPIS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WKUPIS as a value
            Val : HAL.UInt14;
         when True =>
            --  WKUPIS as an array
            Arr : SUPC_SUPC_SR_WKUPIS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 14;

   for SUPC_SUPC_SR_WKUPIS_Field use record
      Val at 0 range 0 .. 13;
      Arr at 0 range 0 .. 13;
   end record;

   --  Supply Controller Status Register
   type SUPC_SUPC_SR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit;
      --  Read-only. WKUP Wakeup Status (cleared on read)
      WKUPS          : SUPC_SR_WKUPS_Field;
      --  Read-only. Supply Monitor Detection Wakeup Status (cleared on read)
      SMWS           : SUPC_SR_SMWS_Field;
      --  Read-only. Brownout Detector Reset Status (cleared on read)
      BODRSTS        : SUPC_SR_BODRSTS_Field;
      --  Read-only. Supply Monitor Reset Status (cleared on read)
      SMRSTS         : SUPC_SR_SMRSTS_Field;
      --  Read-only. Supply Monitor Status (cleared on read)
      SMS            : SUPC_SR_SMS_Field;
      --  Read-only. Supply Monitor Output Status
      SMOS           : SUPC_SR_SMOS_Field;
      --  Read-only. 32-kHz Oscillator Selection Status
      OSCSEL         : SUPC_SR_OSCSEL_Field;
      --  unspecified
      Reserved_8_12  : HAL.UInt5;
      --  Read-only. Low-power Debouncer Wakeup Status on WKUP0 (cleared on
      --  read)
      LPDBCS0        : SUPC_SR_LPDBCS0_Field;
      --  Read-only. Low-power Debouncer Wakeup Status on WKUP1 (cleared on
      --  read)
      LPDBCS1        : SUPC_SR_LPDBCS1_Field;
      --  unspecified
      Reserved_15_15 : HAL.Bit;
      --  Read-only. WKUPx Input Status (cleared on read)
      WKUPIS         : SUPC_SUPC_SR_WKUPIS_Field;
      --  unspecified
      Reserved_30_31 : HAL.UInt2;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_SUPC_SR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      WKUPS          at 0 range 1 .. 1;
      SMWS           at 0 range 2 .. 2;
      BODRSTS        at 0 range 3 .. 3;
      SMRSTS         at 0 range 4 .. 4;
      SMS            at 0 range 5 .. 5;
      SMOS           at 0 range 6 .. 6;
      OSCSEL         at 0 range 7 .. 7;
      Reserved_8_12  at 0 range 8 .. 12;
      LPDBCS0        at 0 range 13 .. 13;
      LPDBCS1        at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      WKUPIS         at 0 range 16 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype SUPC_SYSC_VERSION_VERSION_Field is HAL.UInt12;
   subtype SUPC_SYSC_VERSION_MFN_Field is HAL.UInt3;

   --  Version Register
   type SUPC_SYSC_VERSION_Register is record
      --  Read-only. Version of the Hardware Module
      VERSION        : SUPC_SYSC_VERSION_VERSION_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Metal Fix Number
      MFN            : SUPC_SYSC_VERSION_MFN_Field;
      --  unspecified
      Reserved_19_31 : HAL.UInt13;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SUPC_SYSC_VERSION_Register use record
      VERSION        at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      MFN            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Supply Controller
   type SUPC_Peripheral is record
      --  Supply Controller Control Register
      SUPC_CR      : aliased SUPC_SUPC_CR_Register;
      --  Supply Controller Supply Monitor Mode Register
      SUPC_SMMR    : aliased SUPC_SUPC_SMMR_Register;
      --  Supply Controller Mode Register
      SUPC_MR      : aliased SUPC_SUPC_MR_Register;
      --  Supply Controller Wakeup Mode Register
      SUPC_WUMR    : aliased SUPC_SUPC_WUMR_Register;
      --  Supply Controller Wakeup Inputs Register
      SUPC_WUIR    : aliased SUPC_SUPC_WUIR_Register;
      --  Supply Controller Status Register
      SUPC_SR      : aliased SUPC_SUPC_SR_Register;
      --  Version Register
      SYSC_VERSION : aliased SUPC_SYSC_VERSION_Register;
   end record
     with Volatile;

   for SUPC_Peripheral use record
      SUPC_CR      at 16#0# range 0 .. 31;
      SUPC_SMMR    at 16#4# range 0 .. 31;
      SUPC_MR      at 16#8# range 0 .. 31;
      SUPC_WUMR    at 16#C# range 0 .. 31;
      SUPC_WUIR    at 16#10# range 0 .. 31;
      SUPC_SR      at 16#14# range 0 .. 31;
      SYSC_VERSION at 16#FC# range 0 .. 31;
   end record;

   --  Supply Controller
   SUPC_Periph : aliased SUPC_Peripheral
     with Import, Address => System'To_Address (16#400E1810#);

end SAM_SVD.SUPC;
