package body STM32F4.ADC is

   procedure Check (Flag : Boolean;  Already_Set : in out Boolean; Msg : String);
   --  Checks whether Flag has already been set. If so, raises visible
   --  Invalid_Configuration exception with Msg.


   procedure Set_Sequence_Position
     (This    : in out Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel;
      Rank    : Regular_Channel_Sequence_Count);

   procedure Set_Sampling_Time
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Sample_Time : Channel_Sampling_Times);

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out Analog_To_Digital_Converter) is
   begin
      if not This.CR2.ADC_Enabled then
         This.CR2.ADC_Enabled := True;
         delay until Clock + ADC_Stabilization;
      end if;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out Analog_To_Digital_Converter) is
   begin
      This.CR2.ADC_Enabled := False;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : Analog_To_Digital_Converter) return Boolean is
     (This.CR2.ADC_Enabled);

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This       : out Analog_To_Digital_Converter;
      Continuous : Boolean;
      Resolution : Conversion_Resolution;
      Alignment  : Data_Alignment;
      Enable_EOC : Boolean;
      Regular    : Regular_Channel_Conversions  := No_Regular_Conversions;
      Injected   : Injected_Channel_Conversions := No_Injected_Conversions)
   is
      VBat_Already_Set      : Boolean := False;
      VRef_Temp_Already_Set : Boolean := False;
      Total_Conversions     : Bits_4 := 0;
   begin
      This.CR1.Resolution := Resolution;
      This.CR2.End_Of_Conversion_Signal_Enabled := Enable_EOC;
      This.CR2.Alignment := Alignment;
      This.CR2.Continuous_Conversion_Enabled := Continuous;

      This.CR1.Scan_Mode_Enabled := Regular'Length > 0 or Injected'Length > 0;

      for Rank in Regular'Range loop
         declare
            C : ADC_Conversion_Descriptor renames Regular (Rank);
         begin
            Check (C.VBat, VBat_Already_Set, "VBat already set");
            Check (C.VRef_Temp, VRef_Temp_Already_Set, "VRef_Temp already set");
            Configure_Regular_Channel (This, C.Channel, Rank, C.Sample_Time, C.VBat, C.VRef_Temp);
            Total_Conversions := Total_Conversions + 1;
         end;
      end loop;
--        for Rank in Injected'Range loop
--           declare
--              C : Channel_Conversion_Descriptor renames Injected (Rank);
--           begin
--              Check (C.VBat, VBat_Already_Set, "VBat already set");
--              Check (C.VRef_Temp, VRef_Temp_Already_Set, "VRef_Temp already set");
--              Configure_Injected_Channel (This, C.Channel, Rank, C.Sample_Time, C.VBat, C.VRef_Temp);
--              Total_Conversions := Total_Conversions + 1;
--           end;
--        end loop;
      This.SQR1.Length := Total_Conversions - 1;  -- uses biased representation
   end Configure;

   --------------------------
   -- Conversions_Excepted --
   --------------------------

   function Conversions_Expected (This : Analog_To_Digital_Converter) return Natural is
      (Natural (This.SQR1.Length) + 1);

   -----------------------
   -- Scan_Mode_Enabled --
   -----------------------

   function Scan_Mode_Enabled (This : Analog_To_Digital_Converter) return Boolean is
     (This.CR1.Scan_Mode_Enabled);

   -----------------
   -- EOC_Enabled --
   -----------------

   function EOC_Enabled (This : Analog_To_Digital_Converter) return Boolean is
      (This.CR2.End_Of_Conversion_Signal_Enabled);

   -----------------
   -- DMA_Enabled --
   -----------------

   function DMA_Enabled (This : Analog_To_Digital_Converter) return Boolean is
     (This.CR2.DMA_Enabled);

   -------------------------------
   -- Configure_Regular_Channel --
   -------------------------------

   procedure Configure_Regular_Channel
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Rank        : Regular_Channel_Sequence_Count;
      Sample_Time : Channel_Sampling_Times;
      VBat        : Boolean := False;
      VRef_Temp   : Boolean := False)
   is
   begin
      Set_Sampling_Time (This, Channel, Sample_Time);
      Set_Sequence_Position (This, Channel, Rank);
      if VBat then
         Common.CR.VBat_Channel_Enabled := True;
      elsif VRef_Temp then
         Common.CR.TSVREF_Enabled := True;
         delay until Clock + Temperature_Sensor_Stabilization;
      end if;
   end Configure_Regular_Channel;

   ----------------------
   -- Start_Conversion --
   ----------------------

   procedure Start_Conversion (This : in out Analog_To_Digital_Converter) is
   begin
      if Common.CR.Multi_ADC_Mode = Independent then
         if This.CR2.External_Trigger_Enable = Disabled then
            This.CR2.Start_Conversion_Regular_Channels := True;
         end if;
      else
         if This'Address = ADC1_Base and (This.CR2.External_Trigger_Enable = Disabled) then
            This.CR2.Start_Conversion_Regular_Channels := True;
         end if;
      end if;
   end Start_Conversion;

   ----------------------------------
   -- Poll_For_Conversion_Complete --
   ----------------------------------

   procedure Poll_For_Conversion_Complete (This : in out Analog_To_Digital_Converter) is
   begin
      loop
         exit when This.SR.Regular_Channel_Conversion_Complete;
      end loop;
   end Poll_For_Conversion_Complete;

   ------------
   -- Status --
   ------------

   function Status
     (This : Analog_To_Digital_Converter;
      Flag : ADC_Status_Flag)
      return Boolean
   is
   begin
      case Flag is
         when Overrun =>
            return This.SR.Overrun;
         when Regular_Channel_Conversion_Started =>
            return This.SR.Regular_Channel_Conversion_Started;
         when Injected_Channel_Conversion_Started =>
            return This.SR.Injected_Channel_Conversion_Started;
         when Injected_Channel_Conversion_Complete =>
            return This.SR.Injected_Channel_Conversion_Complete;
         when Regular_Channel_Conversion_Complete =>
            return This.SR.Regular_Channel_Conversion_Complete;
         when Analog_Watchdog_Event_Occurred =>
            return This.SR.Analog_Watchdog_Event_Occurred;
      end case;
   end Status;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status
     (This : in out Analog_To_Digital_Converter;
      Flag : ADC_Status_Flag)
   is
   begin
      case Flag is
         when Overrun =>
            This.SR.Overrun := False;
         when Regular_Channel_Conversion_Started =>
            This.SR.Regular_Channel_Conversion_Started := False;
         when Injected_Channel_Conversion_Started =>
            This.SR.Injected_Channel_Conversion_Started := False;
         when Injected_Channel_Conversion_Complete =>
            This.SR.Injected_Channel_Conversion_Complete := False;
         when Regular_Channel_Conversion_Complete =>
            This.SR.Regular_Channel_Conversion_Complete := False;
         when Analog_Watchdog_Event_Occurred =>
            This.SR.Analog_Watchdog_Event_Occurred := False;
      end case;
   end Clear_Status;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupt)
   is
   begin
      case Source is
         when Overrun =>
            This.CR1.Overrun_Interrupt_Enable := True;
         when Injected_Channel_Conversion_Complete =>
            This.CR1.Injected_EOC_Interrupt_Enabled := True;
         when Regular_Channel_Conversion_Complete =>
            This.CR1.EOC_Interrupt_Enabled := True;
         when Analog_Watchdog_Event =>
            This.CR1.Analog_Watchdog_Interrupt_Enabled := True;
      end case;
   end Enable_Interrupts;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : Analog_To_Digital_Converter;
      Source : ADC_Interrupt)
     return Boolean
   is
   begin
      case Source is
         when Overrun =>
            return This.CR1.Overrun_Interrupt_Enable;
         when Injected_Channel_Conversion_Complete =>
            return This.CR1.Injected_EOC_Interrupt_Enabled;
         when Regular_Channel_Conversion_Complete =>
            return This.CR1.EOC_Interrupt_Enabled;
         when Analog_Watchdog_Event =>
            return This.CR1.Analog_Watchdog_Interrupt_Enabled;
      end case;
   end Interrupt_Enabled;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupt)
   is
   begin
      case Source is
         when Overrun =>
            This.CR1.Overrun_Interrupt_Enable := False;
         when Injected_Channel_Conversion_Complete =>
            This.CR1.Injected_EOC_Interrupt_Enabled := False;
         when Regular_Channel_Conversion_Complete =>
            This.CR1.EOC_Interrupt_Enabled := False;
         when Analog_Watchdog_Event =>
            This.CR1.Analog_Watchdog_Interrupt_Enabled := False;
      end case;
   end Disable_Interrupts;

   --------------------------
   -- Configure_ADC_Common --
   --------------------------

   procedure Configure_ADC_Common
     (Mode           : Multi_ADC_Mode_Selections := Independent;
      Prescalar      : ADC_Prescalars            := PCLK2_Div_2;
      DMA_Mode       : Multi_ADC_DMA_Modes       := Disabled;
      Sampling_Delay : Sampling_Delay_Selections := Sampling_Delay_5_Cycles)
   is
   begin
      Common.CR.Multi_ADC_Mode     := Mode;
      Common.CR.Sampling_Delay     := Sampling_Delay;
      Common.CR.Multi_ADC_DMA_Mode := DMA_Mode;
      Common.CR.ADC_Prescalar      := Prescalar;
   end Configure_ADC_Common;

   ---------------------------
   -- Set_Sequence_Position --
   ---------------------------

   procedure Set_Sequence_Position
     (This    : in out Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel;
      Rank    : Regular_Channel_Sequence_Count)
   is
   begin
      case Rank is
         when 1 .. 6 =>
            This.SQR3.SQ (Rank) := Channel;
         when 7 .. 12 =>
            This.SQR2.SQ (Rank) := Channel;
         when 13 .. 16 =>
            This.SQR1.SQ (Rank) := Channel;
      end case;
   end Set_Sequence_Position;

   ----------------------
   -- Conversion_Value --
   ----------------------

   function Conversion_Value
     (This : Analog_To_Digital_Converter)
      return Half_Word
   is
   begin
      return This.DR.Data;
   end Conversion_Value;

   -----------------------
   -- Set_Sampling_Time --
   -----------------------

   procedure Set_Sampling_Time
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Sample_Time : Channel_Sampling_Times)
   is
   begin
      if Channel > 9 then
         This.SMPR1.SMP (Channel) := Sample_Time;
      else
         This.SMPR2.SMP (Channel) := Sample_Time;
      end if;
   end Set_Sampling_Time;

   -----------
   -- Check --
   -----------

   procedure Check (Flag : Boolean;  Already_Set : in out Boolean; Msg : String) is
   begin
      if Flag then
         if Already_Set then
            raise Invalid_Configuration with Msg;
         else
            Already_Set := True;
         end if;
      end if;
   end Check;

end STM32F4.ADC;
