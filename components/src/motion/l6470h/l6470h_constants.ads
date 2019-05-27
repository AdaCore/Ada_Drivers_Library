pragma Ada_2012;

with HAL;

package L6470h_Constants is 
   pragma SPARK_Mode (On);

   type Register_Address is (ABS_POS,
                             EL_POS,
                             MARK, 
                             SPEED,
                             ACC,
                             DEC,
                             MAX_SPEED,
                             MIN_SPEED,
                             KVAL_HOLD,
                             KVAL_RUN,
                             KVAL_ACC,
                             KVAL_DEC,
                             INT_SPEED,
                             ST_SLP,
                             FN_SLP_ACC,
                             FN_SLP_DEC,
                             K_THERM,
                             ADC_OUT,
                             OCD_TH,
                             STALL_TH,
                             FS_SPD,
                             STEP_MODE,
                             ALARM_EN,
                             CONFIG,
                             STATUS);
   for Register_Address use
     (ABS_POS    => 16#01#,
      EL_POS     => 16#02#,
      MARK       => 16#03#, 
      SPEED      => 16#04#,
      ACC        => 16#05#,
      DEC        => 16#06#,
      MAX_SPEED  => 16#07#,
      MIN_SPEED  => 16#08#,
      KVAL_HOLD  => 16#09#,
      KVAL_RUN   => 16#0A#,
      KVAL_ACC   => 16#0B#,
      KVAL_DEC   => 16#0C#,
      INT_SPEED  => 16#0D#,
      ST_SLP     => 16#0E#,
      FN_SLP_ACC => 16#0F#,
      FN_SLP_DEC => 16#10#,
      K_THERM    => 16#11#,
      ADC_OUT    => 16#12#,
      OCD_TH     => 16#13#,
      STALL_TH   => 16#14#,
      FS_SPD     => 16#15#,
      STEP_MODE  => 16#16#,
      ALARM_EN   => 16#17#,
      CONFIG     => 16#18#,
      STATUS     => 16#19#);
   for Register_Address'Size use 5;
   
   type Register (Addr : Register_Address) is tagged record
      Address : Register_Address := Addr;
   end record;

   -----------------------------------------------------------------------------
   -- types for ABS_POS register
   type ABS_POS_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value   : Hal.UInt22;
      end record;

   -----------------------------------------------------------------------------
   -- types for EL_POS register
   type EL_POS_Record is 
      record
         Microstep : Hal.UInt7;
         Step      : Hal.UInt2;
      end record
     with Size => 9;
   for EL_POS_Record use record
      Microstep at 0 range 0 .. 6;
      Step      at 0 range 7 .. 8;
   end record;
   type EL_POS_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value   : EL_POS_Record;
      end record;

   -----------------------------------------------------------------------------
   -- types for MARK register
   type MARK_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value   : Hal.UInt22;
      end record;

   -----------------------------------------------------------------------------
   -- types for SPEED register
   type SPEED_Val_Type is delta 0.28 range 0.0 .. 7000.0 with
     Size => 20, Small => 0.28;
   type SPEED_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : SPEED_Val_Type;
      end record;

   -----------------------------------------------------------------------------
   -- types for ACC register
   type ACC_Val_Type is delta 0.40 range 0.0 .. 100.0 with
     Size => 12, Small => 0.40;
   type ACC_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : ACC_Val_Type;
      end record;

   -----------------------------------------------------------------------------
   -- types for DEC register
   type DEC_Val_Type is delta 0.40 range 0.0 .. 100.0 with
     Size => 12, Small => 0.40;
   type DEC_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : DEC_Val_Type;
      end record;

   -----------------------------------------------------------------------------
   -- types for MAX_SPEED register
   type MAX_SPEED_Val_Type is delta 0.18 range 0.0 .. 100.0 with
     Size => 10, Small => 0.18;
   type MAX_SPEED_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : MAX_SPEED_Val_Type;
      end record;

   -----------------------------------------------------------------------------
   -- types for MIN_SPEED register
   type MIN_SPEED_Record is
      record
         Min_Speed : HAL.UInt12;
         Lspd_Opt  : HAL.Bit;
      end record  
     with Size => 13;
   for MIN_SPEED_Record use record
      MIN_SPEED at 0 range 0 .. 11;
      Lspd_Opt  at 0 range 12 .. 12;
   end record;
   type MIN_SPEED_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : MIN_SPEED_Record;
      end record;

   -----------------------------------------------------------------------------
   -- types for FS_SPD register
   type FS_SPD_Val_Type is delta 0.18 range 0.0 .. 100.0 with
     Size => 10, Small => 0.18;
   type FS_SPD_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : FS_SPD_Val_Type;
      end record;

   -----------------------------------------------------------------------------
   -- types for KVAL_HOLD register
   type KVAL_HOLD_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : HAL.UInt8;
      end record;

   -----------------------------------------------------------------------------
   -- types for KVAL_RUN register
   type KVAL_RUN_Val_Type is delta 0.004 range 0.0 .. 0.996 with
     Size => 8;
   type KVAL_RUN_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : KVAL_RUN_Val_Type := KVAL_RUN_Val_Type'Small;
      end record;

   -----------------------------------------------------------------------------
   -- types for KVAL_ACC register
   type KVAL_ACC_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : HAL.UInt8;
      end record;

   -----------------------------------------------------------------------------
   -- types for KVAL_DEC register
   type KVAL_DEC_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : HAL.UInt8;
      end record;

   -----------------------------------------------------------------------------
   -- types for INT_SPEED register
   type INT_SPEED_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : HAL.UInt14;
      end record;

   -----------------------------------------------------------------------------
   -- types for ST_SLP register
   type ST_SLP_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : HAL.UInt8;
      end record;

   -----------------------------------------------------------------------------
   -- types for FN_SLP_ACC register
   type FN_SLP_ACC_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : HAL.UInt8;
      end record;

   -----------------------------------------------------------------------------
   -- types for FN_SLP_DEC register
   type FN_SLP_DEC_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : HAL.UInt8;
      end record;

   -----------------------------------------------------------------------------
   -- types for K_THERM register
   type K_THERM_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : HAL.UInt4;
      end record;

   -----------------------------------------------------------------------------
   -- types for ADC_OUT register
   type ADC_OUT_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : HAL.UInt5;
      end record;

   -----------------------------------------------------------------------------
   -- types for OCD_TH register
   type OCD_TH_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : HAL.UInt4;
      end record;

   -----------------------------------------------------------------------------
   -- types for STALL_TH register
   type STALL_TH_Val_Type is delta 31.25 range 31.25 .. 3968.75 with
     Size => 7, Small => 31.25;
   type STALL_TH_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : STALL_TH_Val_Type;
      end record;

   -----------------------------------------------------------------------------
   -- types for STEP_MODE register
   type STEP_SEL_Type is (Full_Step,
                          Half_Step,
                          Micro_1_4,
                          Micro_1_8,
                          Micro_1_16,
                          Micro_1_32,
                          Micro_1_64,
                          Micro_1_128)
     with Size => 3;
     
   type STEP_MODE_Record is
      record
         STEP_SEL  : STEP_SEL_Type := Full_Step;
         SYNC_SEL  : HAL.UInt3 := 0;
         SYNC_EN   : HAL.Bit := 0;
      end record  
     with Size => 8;
   for STEP_MODE_Record use record
      STEP_SEL at 0 range 0 .. 2;
      SYNC_SEL at 0 range 4 .. 6;
      SYNC_EN  at 0 range 7 .. 7;
   end record;
   type STEP_MODE_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : STEP_MODE_Record;
      end record;

   -----------------------------------------------------------------------------
   -- types for ALARM_EN register
   type ALARM_EN_Record is
      record
         OVERCURRENT        : HAL.Bit;
         THERMAL_SHUTDOWN   : HAL.Bit;
         THERMAL_WARNING    : HAL.Bit;
         UNDERVOLTAGE       : HAL.Bit;
         STALL_DETECTION_A  : HAL.Bit;
         STALL_DETECTION_B  : HAL.Bit;
         SWITCH_EVENT       : HAL.Bit;
         WRONG_CMD          : HAL.Bit;
      end record  
     with Size => 8;
   for ALARM_EN_Record use record
      OVERCURRENT       at 0 range 0 .. 0;
      THERMAL_SHUTDOWN  at 0 range 1 .. 1;
      THERMAL_WARNING   at 0 range 2 .. 2;
      UNDERVOLTAGE      at 0 range 3 .. 3;
      STALL_DETECTION_A at 0 range 4 .. 4;
      STALL_DETECTION_B at 0 range 5 .. 5;
      SWITCH_EVENT      at 0 range 6 .. 6;
      WRONG_CMD         at 0 range 7 .. 7;
   end record;
   type ALARM_EN_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : ALARM_EN_Record;
      end record;

   -----------------------------------------------------------------------------
   -- types for CONFIG register
   type SW_MODE_Type is (HardStop_Int, User_Disposal)
     with Size => 1;
   type OC_SD_Type is (Bridges_Do_Not_Shutdown, Bridges_Shutdown)
     with Size => 1;
   type EN_VSCOMP_Type is (Disable, Enable)
     with Size => 1;
   type POW_SR_Type is (Rate_320, Rate_75, Rate_110, Rate_260)
     with Size => 2;
   type F_PWM_DEC_Type is (Mult_0_625,
                           Mult_0_75,
                           Mult_0_875,
                           Mult_1, 
                           Mult_1_25,
                           Mult_1_5,
                           Mult_1_75, 
                           Mult_2)
     with Size => 3;
   type F_PWM_INT_Type is (Div_1, Div_2, Div_3, Div_4, Div_5, Div_6, Div_7, Div_8)
     with Size => 3;
   for F_PWM_INT_Type use (Div_1 => 2#000#, 
                           Div_2 => 2#001#,
                           Div_3 => 2#010#, 
                           Div_4 => 2#011#, 
                           Div_5 => 2#100#, 
                           Div_6 => 2#101#, 
                           Div_7 => 2#110#, 
                           Div_8 => 2#111#);
   type CONFIG_Record is
      record
         OSC_SEL   : Hal.Uint3;
         EXT_CLK   : HAL.Bit;
         SW_MODE   : SW_MODE_Type;
         EN_VSCOMP : EN_VSCOMP_Type;
         OC_SD     : OC_SD_Type;
         POW_SR    : POW_SR_Type;
         F_PWM_DEC : F_PWM_DEC_Type;
         F_PWM_INT : F_PWM_INT_Type;
 end record  
     with Size => 16;
   for CONFIG_Record use record
      OSC_SEL   at 0 range 0 .. 2;
      EXT_CLK   at 0 range 3 .. 3;
      SW_MODE   at 0 range 4 .. 4;
      EN_VSCOMP at 0 range 5 .. 5;
      OC_SD     at 0 range 7 .. 7;
      POW_SR    at 0 range 8 .. 9;
      F_PWM_DEC at 0 range 10 .. 12;
      F_PWM_INT at 0 range 13 .. 15;
   end record;
   type CONFIG_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : CONFIG_Record;
      end record;

   -----------------------------------------------------------------------------
   -- types for STATUS register
   type DIR_Type is (Backward, Forward);
   type MOT_STATUS_Type is (Stoped, Acceleration, Deceleration, Constant_Speed);
   type STATUS_Record is
      record
         HiZ                : Hal.Bit;
         BUSY               : Hal.Bit;
         SW_F               : Hal.Bit;
         SW_EVN             : Hal.Bit;
         DIR                : DIR_Type;
         MOT_STATUS         : MOT_STATUS_Type;
         NOTPERF_CMD        : Hal.Bit;
         WRONG_CMD          : Hal.Bit;
         UVLO               : Hal.Bit;
         TH_WRN             : Hal.Bit;
         TH_SD              : Hal.Bit;
         OCD                : Hal.Bit;
         STEP_LOSS_A        : Hal.Bit;
         STEP_LOSS_B        : Hal.Bit;
         SCK_MODE           : Hal.Bit;
      end record  
     with Size => 16;
   for STATUS_Record use record
      HiZ            at 0 range 0 .. 0;
      BUSY           at 0 range 1 .. 1;
      SW_F           at 0 range 2 .. 2;
      SW_EVN         at 0 range 3 .. 3;
      DIR            at 0 range 4 .. 4;
      MOT_STATUS     at 0 range 5 .. 6;
      NOTPERF_CMD    at 0 range 7 .. 7;
      WRONG_CMD      at 0 range 8 .. 8;
      UVLO           at 0 range 9 .. 9;
      TH_WRN         at 0 range 10 .. 10;
      TH_SD          at 0 range 11 .. 11;
      OCD            at 0 range 12 .. 12;
      STEP_LOSS_A    at 0 range 13 .. 13;
      STEP_LOSS_B    at 0 range 14 .. 14;
      SCK_MODE       at 0 range 15 .. 15;
   end record;
   type STATUS_Type (Addr : Register_Address) is new Register (Addr => Addr) with
      record
         Value : STATUS_Record := (others => <>);
      end record;

   subtype EL_POS_Register     is ABS_POS_Type (Addr => EL_POS);
   subtype MARK_Register       is MARK_Type (Addr => MARK);
   subtype SPEED_Register      is SPEED_Type (Addr => SPEED);
   subtype ACC_Register        is ACC_Type (Addr => ACC);
   subtype DEC_Register        is DEC_Type (Addr => DEC);
   subtype MAX_SPEED_Register  is MAX_SPEED_Type (Addr => MAX_SPEED);
   subtype MIN_SPEED_Register  is MIN_SPEED_Type (Addr => MIN_SPEED);
   subtype FS_SPD_Register     is FS_SPD_Type (Addr => FS_SPD);
   subtype KVAL_HOLD_Register  is KVAL_HOLD_Type (Addr => KVAL_HOLD);
   subtype KVAL_RUN_Register   is KVAL_RUN_Type (Addr => KVAL_RUN);
   subtype KVAL_ACC_Register   is KVAL_ACC_Type (Addr => KVAL_ACC);
   subtype KVAL_DEC_Register   is KVAL_DEC_Type (Addr => KVAL_DEC);
   subtype INT_SPEED_Register  is INT_SPEED_Type (Addr => INT_SPEED);
   subtype ST_SLP_Register     is ST_SLP_Type (Addr => ST_SLP);
   subtype FN_SLP_ACC_Register is FN_SLP_ACC_Type (Addr => FN_SLP_ACC);
   subtype FN_SLP_DEC_Register is FN_SLP_DEC_Type (Addr => FN_SLP_DEC);
   subtype K_THERM_Register    is K_THERM_Type (Addr => K_THERM);
   subtype ADC_OUT_Register    is ADC_OUT_Type (Addr => ADC_OUT);
   subtype OCD_TH_Register     is OCD_TH_Type (Addr => OCD_TH);
   subtype STALL_TH_Register   is STALL_TH_Type (Addr => STALL_TH);
   subtype STEP_MODE_Register  is STEP_MODE_Type (Addr => STEP_MODE);
   subtype ALARM_EN_Register   is ALARM_EN_Type (Addr => ALARM_EN);
   subtype CONFIG_Register     is CONFIG_Type (Addr => CONFIG);
   subtype STATUS_Register     is STATUS_Type (Addr => STATUS);
   
end l6470h_constants;
