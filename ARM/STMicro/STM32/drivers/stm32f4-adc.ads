package STM32F4.ADC is

   type Analog_To_Digital_Converter is limited private;

   procedure Enable (This : in out Analog_To_Digital_Converter) with
     Post => Enabled (This);

   procedure Disable (This : in out Analog_To_Digital_Converter) with
     Post => not Enabled (This);

   function Enabled (This : Analog_To_Digital_Converter) return Boolean;

   type Conversion_Resolution is
     (ADC_Resolution_12_Bits,  -- 15 ADC Clock cycles
      ADC_Resolution_10_Bits,  -- 13 ADC Clock cycles
      ADC_Resolution_8_Bits,   -- 11 ADC Clock cycles
      ADC_Resolution_6_Bits);  --  9 ADC Clock cycles

   type Analog_Input_Channel is range 0 .. 18;

   --  TODO: define constants named ADC_Temperature_Channel in STM32Fxxx.ads packages,
   --     with renamings in the *_Discovery packages:
   --       for STM32F40xxx, value is Channel 16
   --       for STM32F42xxx, value is Channel 18 (same as VBat)
   --  see RM pg 410, section 13.10, also pg 389

   VBat_Channel : constant Analog_Input_Channel := 18;
   --  see RM pg 410, section 13.10; also pg 389 section 13.3.3

   type Channel_Sampling_Times is
     (Sample_3_Cycles,
      Sample_15_Cycles,
      Sample_28_Cycles,
      Sample_56_Cycles,
      Sample_84_Cycles,
      Sample_112_Cycles,
      Sample_144_Cycles,
      Sample_480_Cycles)
     with Size => 3;

   type Discontinuous_Mode_Channel_Count is range 1 .. 8;
   --  note this uses a biased representation implicitly because the underlying
   --  representational bit values are 0 ... 7

   type External_Trigger_Enablers is
     (Disabled,
      Trigger_Rising_Edge,
      Trigger_Falling_Edge,
      Trigger_Both_Edges);

   type External_Event_Selections_Regular_Group is
     (Timer1_CC1_Event,
      Timer1_CC2_Event,
      Timer1_CC3_Event,
      Timer2_CC2_Event,
      Timer2_CC3_Event,
      Timer2_CC4_Event,
      Timer2_TRGO_Event,
      Timer3_CC1_Event,
      Timer3_TRGO_Event,
      Timer4_CC4_Event,
      Timer5_CC1_Event,
      Timer5_CC2_Event,
      Timer5_CC3_Event,
      Timer8_CC1_Event,
      Timer8_TRGO_Event,
      EXTI_Line11);

   type External_Event_Selections_Injected_Group is
     (Timer1_CC4_Event,
      Timer1_TRGO_Event,
      Timer2_CC1_Event,
      Timer2_TRGO_Event,
      Timer3_CC2_Event,
      Timer3_CC4_Event,
      Timer4_CC1_Event,
      Timer4_CC2_Event,
      Timer4_CC3_Event,
      Timer4_TRGO_Event,
      Timer5_CC4_Event,
      Timer5_TRGO_Event,
      Timer8_CC2_Event,
      Timer8_CC3_Event,
      Timer8_CC4_Event,
      EXTI_Line15);

   type Data_Alignment is (Left, Right);

   type Injected_Data_Offset is new Bits_12;

   type Watchdog_Threshold is new Bits_12;

   --  common  ----------------------------------------------------------------

   type ADC_Prescalars is
     (PCLK2_Div_2,
      PCLK2_Div_4,
      PCLK2_Div_6,
      PCLK2_Div_8);

   type Multi_ADC_DMA_Modes is
     (Disabled,
      DMA_Mode_1,
      DMA_Mode_2,
      DMA_Mode_3);

   type Sampling_Delay_Selections is
     (Sampling_Delay_5_Cycles,
      Sampling_Delay_6_Cycles,
      Sampling_Delay_7_Cycles,
      Sampling_Delay_8_Cycles,
      Sampling_Delay_9_Cycles,
      Sampling_Delay_10_Cycles,
      Sampling_Delay_11_Cycles,
      Sampling_Delay_12_Cycles,
      Sampling_Delay_13_Cycles,
      Sampling_Delay_14_Cycles,
      Sampling_Delay_15_Cycles,
      Sampling_Delay_16_Cycles,
      Sampling_Delay_17_Cycles,
      Sampling_Delay_18_Cycles,
      Sampling_Delay_19_Cycles,
      Sampling_Delay_20_Cycles);

   type Multi_ADC_Mode_Selections is
     (Independent,
      Dual_Combined_Regular_Injected_Simultaneous,
      Dual_Combined_Regular_Simultaneous_Alternate_Trigger,
      Dual_Injected_Simultaneous,
      Dual_Regular_Simultaneous,
      Dual_Interleaved,
      Dual_Alternate_Trigger,
      Triple_Combined_Regular_Injected_Simultaneous,
      Triple_Combined_Regular_Simultaneous_Alternate_Trigger,
      Triple_Injected_Simultaneous,
      Triple_Regular_Simultaneous,
      Triple_Interleaved,
      Triple_Alternate_Trigger);

   for Multi_ADC_Mode_Selections use
     (Independent                                            => 2#00000#,
      Dual_Combined_Regular_Injected_Simultaneous            => 2#00001#,
      Dual_Combined_Regular_Simultaneous_Alternate_Trigger   => 2#00010#,
      Dual_Injected_Simultaneous                             => 2#00101#,
      Dual_Regular_Simultaneous                              => 2#00110#,
      Dual_Interleaved                                       => 2#00111#,
      Dual_Alternate_Trigger                                 => 2#01001#,
      Triple_Combined_Regular_Injected_Simultaneous          => 2#10001#,
      Triple_Combined_Regular_Simultaneous_Alternate_Trigger => 2#10010#,
      Triple_Injected_Simultaneous                           => 2#10101#,
      Triple_Regular_Simultaneous                            => 2#10110#,
      Triple_Interleaved                                     => 2#10111#,
      Triple_Alternate_Trigger                               => 2#11001#);

   procedure Configure
     (Mode           : Multi_ADC_Mode_Selections := Independent;
      Prescalar      : ADC_Prescalars            := PCLK2_Div_2;
      DMA_Mode       : Multi_ADC_DMA_Modes       := Disabled;
      Sampling_Delay : Sampling_Delay_Selections := Sampling_Delay_5_Cycles);

private

   type Status_Register is record
      Reserved_31_6                        : Bits_26;
      Overrun                              : Boolean;
      Regular_Channel_Conversion_Started   : Boolean;
      Injected_Channel_Conversion_Started  : Boolean;
      Injected_Channel_Conversion_Complete : Boolean;
      Regular_Channel_Conversion_Complete  : Boolean;
      Analog_Watchdog_Event_Occurred       : Boolean;
   end record with
     Volatile_Full_Access,
     Size => 32;

   for Status_Register use record
      Reserved_31_6                        at 0 range 6 .. 31;
      Overrun                              at 0 range 5 .. 5;
      Regular_Channel_Conversion_Started   at 0 range 4 .. 4;
      Injected_Channel_Conversion_Started  at 0 range 3 .. 3;
      Injected_Channel_Conversion_Complete at 0 range 2 .. 2;
      Regular_Channel_Conversion_Complete  at 0 range 1 .. 1;
      Analog_Watchdog_Event_Occurred       at 0 range 0 .. 0;
   end record;

   type Control_Register_1 is record
      Reserved_31_27                         : Bits_5;
      Overrun_Interrupt_Enable               : Boolean;  -- OVRIE
      Resolution                             : Conversion_Resolution;  -- RES
      Enable_Regular_Channels_Watchdog       : Boolean;  -- AWDEN
      Enable_Injected_Channels_Watchdog      : Boolean;  -- JAWDEN
      Reserved_21_16                         : Bits_6;
      Discontinuous_Mode_Channel_Count       : Bits_3;   -- DISCNUM  -- values are 1..8 but uses bit values 0 .. 7
      Discontinuous_Mode_Injected_Enabled    : Boolean;  -- JDISCEN
      Discontinuous_Mode_Regular_Enabled     : Boolean;  -- DISCEN
      Auto_Injected_Group_Conversion_Enabled : Boolean;  -- JAUTO
      Analog_Single_Channel_Watchdog_Enabled : Boolean;  -- AWDSGL
      Scan_Mode_Enabled                      : Boolean;  -- SCAN
      Injected_Channels_Interrupt_Enabled    : Boolean;  -- JEOCIE
      Analog_Watchdog_Interrupt_Enabled      : Boolean;  -- AWDIE
      EOC_Interrupt_Enabled                  : Boolean;  -- EOCIE
      Analog_Watchdog_Channel_Selected       : Analog_Input_Channel;  -- AWDCH
   end record with
     Volatile_Full_Access,
     Size => 32;

   for Control_Register_1 use record
      Reserved_31_27                         at 0 range 27 .. 31;
      Overrun_Interrupt_Enable               at 0 range 26 .. 26;
      Resolution                             at 0 range 24 .. 25;
      Enable_Regular_Channels_Watchdog       at 0 range 23 .. 23;
      Enable_Injected_Channels_Watchdog      at 0 range 22 .. 22;
      Reserved_21_16                         at 0 range 16 .. 21;
      Discontinuous_Mode_Channel_Count       at 0 range 13 .. 15;
      Discontinuous_Mode_Injected_Enabled    at 0 range 12 .. 12;
      Discontinuous_Mode_Regular_Enabled     at 0 range 11 .. 11;
      Auto_Injected_Group_Conversion_Enabled at 0 range 10 .. 10;
      Analog_Single_Channel_Watchdog_Enabled at 0 range  9 .. 9;
      Scan_Mode_Enabled                      at 0 range  8 .. 8;
      Injected_Channels_Interrupt_Enabled    at 0 range  7 .. 7;
      Analog_Watchdog_Interrupt_Enabled      at 0 range  6 .. 6;
      EOC_Interrupt_Enabled                  at 0 range  5 .. 5;
      Analog_Watchdog_Channel_Selected       at 0 range  0 .. 4;
   end record;

   type Control_Register_2 is record
      Reserved_31                        : Bits_1;
      Start_Conversion_Regular_Channels  : Boolean;
      External_Trigger_Enable            : External_Trigger_Enablers;
      External_Event_Select_Regular      : External_Event_Selections_Regular_Group;
      Reserved_23                        : Bits_1;
      Start_Conversion_Injected_Channels : Boolean;
      External_Trigger_Injected_Enable   : External_Trigger_Enablers;
      External_Event_Select_Injected     : External_Event_Selections_Injected_Group;
      Reserved_15_12                     : Bits_4;
      Alignment                          : Data_Alignment;
      End_Of_Conversion_Signal_Enabled   : Boolean;
      DMA_Requests_Enabled               : Boolean;
      DMA_Enabled                        : Boolean;
      Reserved_7_2                       : Bits_6;
      Continuous_Conversion_Enabled      : Boolean;
      ADC_Enabled                        : Boolean;
   end record with
     Volatile_Full_Access,
     Size => 32;

   for Control_Register_2 use record
      Reserved_31                        at 0 range 31 .. 31;
      Start_Conversion_Regular_Channels  at 0 range 30 .. 30;
      External_Trigger_Enable            at 0 range 28 .. 29;
      External_Event_Select_Regular      at 0 range 24 .. 27;
      Reserved_23                        at 0 range 23 .. 23;
      Start_Conversion_Injected_Channels at 0 range 22 .. 22;
      External_Trigger_Injected_Enable   at 0 range 20 .. 21;
      External_Event_Select_Injected     at 0 range 16 .. 19;
      Reserved_15_12                     at 0 range 12 .. 15;
      Alignment                          at 0 range 11 .. 11;
      End_Of_Conversion_Signal_Enabled   at 0 range 10 .. 10;
      DMA_Requests_Enabled               at 0 range  9 .. 9;
      DMA_Enabled                        at 0 range  8 .. 8;
      Reserved_7_2                       at 0 range  2 .. 7;
      Continuous_Conversion_Enabled      at 0 range  1 .. 1;
      ADC_Enabled                        at 0 range  0 .. 0;
   end record;

   type Sample_Time_List is
     array (Analog_Input_Channel range <>) of Channel_Sampling_Times
     with Component_Size => 3;

   type Sample_Time_Register_1 is record
      Reserved_31_27 : Bits_5;
      SMP            : Sample_Time_List (10 .. 18);
   end record with
     Volatile_Full_Access,
     Size => 32;

   for Sample_Time_Register_1 use record
      Reserved_31_27 at 0 range 27 .. 31;
      SMP            at 0 range  0 .. 26;
   end record;

   type Sample_Time_Register_2 is record
      Reserved_31_30 : Bits_2;
      SMP            : Sample_Time_List (0 .. 9);
   end record with
     Volatile_Full_Access,
     Size => 32;

   for Sample_Time_Register_2 use record
      Reserved_31_30 at 0 range 30 .. 31;
      SMP            at 0 range  0 .. 29;
   end record;

   type Injected_Channel_Data_Offset_Register is record
      Reserved_31_12 : Bits_20;
      JOffset        : Injected_Data_Offset;
   end record with
     Volatile_Full_Access,
     Size => 32;

   for Injected_Channel_Data_Offset_Register use record
      Reserved_31_12 at 0 range 12 .. 31;
      JOffset        at 0 range  0 .. 11;
   end record;

   type Watchdog_Threshold_Register is record
      Reserved_31_12 : Bits_20;
      Threshold      : Watchdog_Threshold;
   end record with
     Volatile_Full_Access,
     Size => 32;

   for Watchdog_Threshold_Register use record
      Reserved_31_12 at 0 range 12 .. 31;
      Threshold      at 0 range 0 .. 11;
   end record;

   type Channel_Sequence_Count is range 1 .. 16;

   type Channel_Group is
     array (Channel_Sequence_Count range <>) of Analog_Input_Channel
     with Component_Size => 5;

   type Regular_Sequence_Register_1 is record
      Reserved_31_24 : Bits_8;
      Length         : Bits_4;
      -- really the type for Length is Channel_Sequence_Count, but using a
      -- biased representation ("0" => 1 conversion, etc) to hold 16 values
      SQ             : Channel_Group (13 .. 16);
   end record with
     Volatile_Full_Access,
     Size => 32;

   for Regular_Sequence_Register_1 use record
      Reserved_31_24 at 0 range 24 .. 31;
      Length         at 0 range 20 .. 23;
      SQ             at 0 range  0 .. 19;
   end record;

   type Regular_Sequence_Register_2 is record
      Reserved_31_30 : Bits_2;
      SQ             : Channel_Group (7 .. 12);
   end record with
     Volatile_Full_Access,
     Size => 32;

   for Regular_Sequence_Register_2 use record
      Reserved_31_30 at 0 range 30 .. 31;
      SQ             at 0 range  0 .. 29;
   end record;

   type Regular_Sequence_Register_3 is record
      Reserved_31_30 : Bits_2;
      SQ             : Channel_Group (1 .. 6);
   end record with
     Volatile_Full_Access,
     Size => 32;

   for Regular_Sequence_Register_3 use record
      Reserved_31_30 at 0 range 30 .. 31;
      SQ             at 0 range  0 .. 29;
   end record;

   type Injected_Sequence_Register is record
      Reserved_31_22 : Bits_10;
      Length         : Bits_2;
      -- really the type for Length is a subtype of Channel_Sequence_Count with
      -- a range of range 1 .. 4, but using a biased representation ("0" => 1
      -- conversion, etc) to hold 4 values
      SQ             : Channel_Group (1 .. 4);
   end record with
     Volatile_Full_Access,
     Size => 32;

   for Injected_Sequence_Register use record
      Reserved_31_22 at 0 range 22 .. 31;
      Length         at 0 range 20 .. 21;
      SQ             at 0 range  0 .. 19;
   end record;

   type Data_Register is record
      Reserved_31_16 : Bits_16;
      Data           : Half_Word;
   end record with
     Volatile_Full_Access,
     Size => 32;

   for Data_Register use record
      Reserved_31_16 at 0 range 16 .. 31;
      Data           at 0 range 0 .. 15;
   end record;

   type Analog_To_Digital_Converter is limited record
      SR    : Status_Register;
      CR1   : Control_Register_1;
      CR2   : Control_Register_2;
      SMPR1 : Sample_Time_Register_1;
      SMPR2 : Sample_Time_Register_2;
      JOFR1 : Injected_Channel_Data_Offset_Register;
      JOFR2 : Injected_Channel_Data_Offset_Register;
      JOFR3 : Injected_Channel_Data_Offset_Register;
      JOFR4 : Injected_Channel_Data_Offset_Register;
      HTR   : Watchdog_Threshold_Register;
      LTR   : Watchdog_Threshold_Register;
      SQR1  : Regular_Sequence_Register_1;
      SQR2  : Regular_Sequence_Register_2;
      SQR3  : Regular_Sequence_Register_3;
      JSQR  : Injected_Sequence_Register;
      JDR1  : Data_Register;
      JDR2  : Data_Register;
      JDR3  : Data_Register;
      JDR4  : Data_Register;
      DR    : Data_Register;
   end record with
     Size => 20 * 32;

   for Analog_To_Digital_Converter use record
      SR    at 16#00# range 0 .. 31;
      CR1   at 16#04# range 0 .. 31;
      CR2   at 16#08# range 0 .. 31;
      SMPR1 at 16#0C# range 0 .. 31;
      SMPR2 at 16#10# range 0 .. 31;
      JOFR1 at 16#14# range 0 .. 31;
      JOFR2 at 16#18# range 0 .. 31;
      JOFR3 at 16#1C# range 0 .. 31;
      JOFR4 at 16#20# range 0 .. 31;
      HTR   at 16#24# range 0 .. 31;
      LTR   at 16#28# range 0 .. 31;
      SQR1  at 16#2C# range 0 .. 31;
      SQR2  at 16#30# range 0 .. 31;
      SQR3  at 16#34# range 0 .. 31;
      JSQR  at 16#38# range 0 .. 31;
      JDR1  at 16#3C# range 0 .. 31;
      JDR2  at 16#40# range 0 .. 31;
      JDR3  at 16#44# range 0 .. 31;
      JDR4  at 16#48# range 0 .. 31;
      DR    at 16#4C# range 0 .. 31;
   end record;

   ---------------------------- Common Registers ------------------------------

   type Individual_Status_Register is record
      Overrun                  : Boolean;
      Regular_Channel_Started  : Boolean;
      Injected_Channel_Started : Boolean;
      Injected_Channel_EOC     : Boolean;
      Regular_Channel_EOC      : Boolean;
      Analog_Watchdog          : Boolean;
   end record with
     Size => 8;

   for Individual_Status_Register use record
      Overrun                  at 0 range 5 .. 5;
      Regular_Channel_Started  at 0 range 4 .. 4;
      Injected_Channel_Started at 0 range 3 .. 3;
      Injected_Channel_EOC     at 0 range 2 .. 2;
      Regular_Channel_EOC      at 0 range 1 .. 1;
      Analog_Watchdog          at 0 range 0 .. 0;
   end record;

   type Common_Status_Register is
     array (1 .. 3) of Individual_Status_Register with
     Volatile_Components,
     Component_Size => 8;

   type Common_Control_Register is record
      Reserved_31_24        : Bits_8;
      TSVREF_Enabled        : Boolean;  -- TSVREFE
      Vbat_Channel_Enabled  : Boolean;  -- VBATE
      Reserved_21_18        : Bits_4;
      ADC_Prescalar         : ADC_Prescalars; -- ADCPRE
      Multi_ADC_DMA_Mode    : Multi_ADC_DMA_Modes;  -- DMA
      Multi_ADC_DMA_Enabled : Boolean;  -- DDS
      Reserved_12           : Bits_1;
      Sampling_Delay        : Sampling_Delay_Selections;
      Reserved_7_5          : Bits_3;
      Multi_ADC_Mode        : Multi_ADC_Mode_Selections;
   end record with
     Volatile_Full_Access,
     Size => 32;

   for Common_Control_Register use record
      Reserved_31_24        at 0 range 24 .. 31;
      TSVREF_Enabled        at 0 range 23 .. 23;
      Vbat_Channel_Enabled  at 0 range 22 .. 22;
      Reserved_21_18        at 0 range 18 .. 21;
      ADC_Prescalar         at 0 range 16 .. 17;
      Multi_ADC_DMA_Mode    at 0 range 14 .. 15;
      Multi_ADC_DMA_Enabled at 0 range 13 .. 13;
      Reserved_12           at 0 range 12 .. 12;
      Sampling_Delay        at 0 range  8 .. 11;
      Reserved_7_5          at 0 range  5 .. 7;
      Multi_ADC_Mode        at 0 range  0 .. 4;
   end record;

   type Common_Data_Register is record
      Data_2 : Half_Word;
      Data_1 : Half_Word;
   end record with
     Volatile_Full_Access,
     Size => 32;

   for Common_Data_Register use record
      Data_2 at 0 range 16 .. 31;
      Data_1 at 0 range  0 .. 15;
   end record;

   type Common_Registers is record
      SR : Common_Status_Register;
      CR : Common_Control_Register;
      DR : Common_Data_Register;
   end record with
     Size => 3 * 32;

   for Common_Registers use record
      SR at 0 range 0 .. 23;
      CR at 4 range 0 .. 31;
      DR at 8 range 0 .. 31;
   end record;

   Common : Common_Registers with
     Volatile,
     Address => ADC1_Base + 16#300#;

end STM32F4.ADC;
