with System;
with HAL;    use HAL;

with Cortex_M_SVD.Debug; use Cortex_M_SVD.Debug;
with NRF_SVD.CLOCK;      use NRF_SVD.CLOCK;
with NRF_SVD.FICR;       use NRF_SVD.FICR;
with NRF_SVD.POWER;      use NRF_SVD.POWER;
with NRF_SVD.TEMP;       use NRF_SVD.TEMP;

package body nRF.Device is

   Undocumented_Reg_FE0: UInt32
     with Address => System'To_Address (16#F0000FE0#);
   Undocumented_Reg_FE4: UInt32
     with Address => System'To_Address (16#F0000FE4#);
   Undocumented_Reg_FE8: UInt32
     with Address => System'To_Address (16#F0000FE8#);
   FE0_Is_Six: constant Boolean := (Undocumented_Reg_FE0 and 16#FF#) = 6;
   FE4_Is_Zero: constant Boolean := (Undocumented_Reg_FE0 and 16#0F#) = 0;

   function Errata_12 return Boolean is
   begin
      if FE0_Is_Six and FE4_Is_Zero then
         case Undocumented_Reg_FE8 and 16#F0# is
            when 16#30# => return True;
            when 16#40# => return True;
            when 16#50# => return True;
            when others => return False;
         end case;
      end if;
      return False;
   end Errata_12;

   E12_Undocumented_COMP_Reg_540: UInt32
     with Address => System'To_Address (16#40013540#);
   E12_Undocumented_FICR_Reg_324: UInt32
     with Address => System'To_Address (16#10000324#);

   function Errata_16 return Boolean is
   begin
      if FE0_Is_Six and FE4_Is_Zero and Undocumented_Reg_FE8 = 16#30# then
            return True;
      end if;
      return False;
   end Errata_16;

   E16_Undocumented_Reg_074: UInt32
     with Address => System'To_Address (16#4007C074#);

   function Errata_31 return Boolean is
   begin
      return Errata_12;
   end Errata_31;

   E31_Undocumented_CLOCK_Reg_53C: UInt32 with
     Address => System'To_Address (16#4000053C#);
   E31_Undocumented_FICR_Reg_244: UInt32 with
     Address => System'To_Address (16#10000244#);

   function Errata_32 return Boolean is
   begin
      return Errata_16;
   end Errata_32;

   function Errata_36 return Boolean is
   begin
      return Errata_12;
   end Errata_36;

   function Errata_37 return Boolean is
   begin
      return Errata_16;
   end Errata_37;

   E37_Undocumented_Reg_5A0: UInt32 with
     Address => System'To_Address (16#400005A0#);

   function Errata_57 return Boolean is
   begin
      return Errata_16;
   end Errata_57;

   E57_Undocumented_NFCT_Reg_610: UInt32 with
     Address => System'To_Address (16#40005610#);
   E57_Undocumented_NFCT_Reg_614: UInt32 with
     Address => System'To_Address (16#40005614#);
   E57_Undocumented_NFCT_Reg_618: UInt32 with
     Address => System'To_Address (16#40005618#);
   E57_Undocumented_NFCT_Reg_688: UInt32 with
     Address => System'To_Address (16#40005688#);

   function Errata_66 return Boolean is
   begin
      if FE0_Is_Six and FE4_Is_Zero and Undocumented_Reg_FE8 = 16#50# then
         return True;
      end if;
      return False;
   end Errata_66;

   function Errata_108 return Boolean is
   begin
      return Errata_12;
   end Errata_108;

   E108_Undocumented_Reg_EE4: UInt32 with
     Address => System'To_Address (16#40000EE4#);
   E108_Undocumented_FICR_Reg_258: UInt32 with
     Address => System'To_Address (16#10000258#);

   function Errata_136 return Boolean is
   begin
      return Errata_12;
   end Errata_136;

   function Errata_182 return Boolean is
      Undocumented_FICR_Reg_130: UInt32
        with Address => System'To_Address (16#10000130#);
      Undocumented_FICR_Reg_134: UInt32
        with Address => System'To_Address (16#10000130#);
   begin
      return Undocumented_FICR_Reg_130 = 6 and Undocumented_FICR_Reg_134 = 6;
   end Errata_182;

   E182_Undocumented_Reg_73C: UInt32 with
     Address => System'To_Address (16#4000173C#);

begin
   if Errata_12 then
      -- Workaround for Errata 12 "COMP: Reference ladder not correctly
      -- calibrated"
      E12_Undocumented_COMP_Reg_540 :=
        Shift_Right(E12_Undocumented_FICR_Reg_324 and 16#1F00#, 8);
   end if;

   if Errata_16 then
      -- Workaround for Errata 16 "System: RAM may be corrupt on wakeup from CPU
      -- IDLE"
      E16_Undocumented_Reg_074 := 3131961357;
   end if;

   if Errata_31 then
      -- Workaround for Errata 31 "CLOCK: Calibration values are not correctly
      -- loaded from FICR at reset"
      E31_Undocumented_CLOCK_Reg_53C :=
        Shift_Right(E31_Undocumented_FICR_Reg_244 and 16#E000#, 13);
   end if;

   if Errata_32 then
      -- Workaround for Errata 32 "DIF: Debug session automatically enables
      -- TracePort pins"
      Debug_Periph.DEMCR.TRCENA := False;
   end if;

   if Errata_36 then
      -- Workaround for Errata 36 "CLOCK: Some registers are not reset when
      -- expected"
      CLOCK_Periph.EVENTS_DONE.EVENTS_DONE := False;
      CLOCK_Periph.EVENTS_CTTO.EVENTS_CTTO := False;
      CLOCK_Periph.CTIV.CTIV := 0;
   end if;

   if Errata_37 then
      -- Workaround for Errata 37 "RADIO: Encryption engine is slow by default"
      E37_Undocumented_Reg_5A0 := 3;
   end if;

   if Errata_57 then
      -- Workaround for Errata 57 "NFCT: NFC Modulation amplitude"
      E57_Undocumented_NFCT_Reg_610 := 5;
      E57_Undocumented_NFCT_Reg_688 := 1;
      E57_Undocumented_NFCT_Reg_618 := 0;
      E57_Undocumented_NFCT_Reg_614 := 16#3F#;
   end if;

   if Errata_66 then
      -- Workaround for Errata 66 "TEMP: Linearity specification not met with
      -- default settings"
      TEMP_Periph.A0.A0 := FICR_Periph.TEMP.A0.A;
      TEMP_Periph.A1.A1 := FICR_Periph.TEMP.A1.A;
      TEMP_Periph.A2.A2 := FICR_Periph.TEMP.A2.A;
      TEMP_Periph.A3.A3 := FICR_Periph.TEMP.A3.A;
      TEMP_Periph.A4.A4 := FICR_Periph.TEMP.A4.A;
      TEMP_Periph.A5.A5 := FICR_Periph.TEMP.A5.A;

      TEMP_Periph.B0.B0 := FICR_Periph.TEMP.B0.B;
      TEMP_Periph.B1.B1 := FICR_Periph.TEMP.B1.B;
      TEMP_Periph.B2.B2 := FICR_Periph.TEMP.B2.B;
      TEMP_Periph.B3.B3 := FICR_Periph.TEMP.B3.B;
      TEMP_Periph.B4.B4 := FICR_Periph.TEMP.B4.B;
      TEMP_Periph.B5.B5 := FICR_Periph.TEMP.B5.B;

      TEMP_Periph.T0.T0 := FICR_Periph.TEMP.T0.T;
      TEMP_Periph.T1.T1 := FICR_Periph.TEMP.T1.T;
      TEMP_Periph.T2.T2 := FICR_Periph.TEMP.T2.T;
      TEMP_Periph.T3.T3 := FICR_Periph.TEMP.T3.T;
      TEMP_Periph.T4.T4 := FICR_Periph.TEMP.T4.T;
   end if;

   if Errata_108 then
      -- Workaround for Errata 108 "RAM: RAM content cannot be trusted upon
      -- waking up from System ON Idle or System OFF mode"
      E108_Undocumented_Reg_EE4 := E108_Undocumented_FICR_Reg_258 and 16#4F#;
   end if;

   if Errata_136 then
      -- Workaround for Errata 136 "System: Bits in RESETREAS are set when they
      -- should not be"
      if POWER_Periph.RESETREAS.RESETPIN = Detected then
         POWER_Periph.RESETREAS.RESETPIN := Notdetected;
      end if;
   end if;

   if Errata_182 then
      -- Workaround for Errata 182 "RADIO: Fixes for anomalies #102, #106, and
      -- #107 do not take effect"
      E182_Undocumented_Reg_73C :=
        E182_Undocumented_Reg_73C or Shift_Left(1, 10);
   end if;

end nRF.Device;
