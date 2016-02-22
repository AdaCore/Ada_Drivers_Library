with Ada.Real_Time; use Ada.Real_Time;
with Interfaces;    use Interfaces;

package body WM8994 is

   WM8994_CHIPID_ADDR      : constant := 16#00#;

   Output_Enabled          : Boolean := False;
   Input_Enabled           : Boolean := False;
   pragma Unreferenced (Input_Enabled);

   ----------
   -- Init --
   ----------

   procedure Init
     (Input     : Input_Device;
      Output    : Output_Device;
      Volume    : Byte;
      Frequency : Audio_Frequency)
   is
      Power_Mgnt_Reg_1 : Half_Word := 0;

   begin
      --  WM8994 Errata work-arounds
      IO_Write (16#102#, 16#0003#);
      IO_Write (16#817#, 16#0000#);
      IO_Write (16#102#, 16#0000#);

      --  Enable VMID soft restart, Start-up Bias current enabled
      IO_Write (16#39#, 16#006C#);

      --  Enable BIAS generator, Enable VMID
      IO_Write (16#01#, 16#0003#);

      delay until Clock + Milliseconds (50);

      Output_Enabled := Output /= No_Output;
      Input_Enabled  := Input /= No_Input;

      Set_Output_Mode (Output);

      case Input is
         when No_Input =>
            null;

         when Microphone =>
            --  Enable AIF1ADC2 (Left), Enable AIF1ADC2 (Right)
            --  Enable DMICDAT2 (Left), Enable DMICDAT2 (Right)
            --  Enable Left ADC, Enable Right ADC
            IO_Write (16#04#, 16#0C30#);
            --  Enable AIF1 DRC2 Signal Detect & DRC in AIF1ADC2 Left/Right
            --  Timeslot 1
            IO_Write (16#450#, 16#00DB#);
            --  Disable IN1L, IN1R, IN2L, IN2R, Enable Thermal sensor &
            --  shutdown
            IO_Write (16#02#, 16#6000#);
            --  Enable the DMIC2(Left) to AIF1 Timeslot 1 (Left) mixer path
            IO_Write (16#608#, 16#0002#);
            --  Enable the DMIC2(Right) to AIF1 Timeslot 1 (Right) mixer path
            IO_Write (16#609#, 16#0002#);
            --  GPIO1 pin configuration GP1_DIR = output, GP1_FN = AIF1 DRC2
            --  signal detect
            IO_Write (16#700#, 16#000E#);

         when Input_Line =>
            --  Enable AIF1ADC1 (Left), Enable AIF1ADC1 (Right)
            --  Enable Left ADC, Enable Right ADC
            IO_Write (16#04#, 16#0303#);
            --  Enable AIF1 DRC1 Signal Detect & DRC in AIF1ADC1 Left/Right
            --  Timeslot 0
            IO_Write (16#440#, 16#00DB#);
            --  Enable IN1L and IN1R, Disable IN2L and IN2R, Enable Thermal
            --  sensor & shutdown
            IO_Write (16#02#, 16#6350#);
            --  Enable the ADCL(Left) to AIF1 Timeslot 0 (Left) mixer path
            IO_Write (16#606#, 16#0002#);
            --  Enable the ADCR(Right) to AIF1 Timeslot 0 (Right) mixer path
            IO_Write (16#607#, 16#0002#);
            --  GPIO1 pin configuration GP1_DIR = output, GP1_FN = AIF1 DRC1
            --  signal detect
            IO_Write (16#700#, 16#000D#);
      end case;

      Set_Frequency (Frequency);

      --  AIF1 Word Length = 16-bits, AIF1 Format = I2S (Default Register
      --  Value)
      IO_Write (16#300#, 16#4010#);
      --  slave mode
      IO_Write (16#302#, 16#0000#);

      --  Enable the DSP processing clock for AIF1, Enable the core clock
      IO_Write (16#208#, 16#000A#);

      --  Enable AIF1 Clock, AIF1 Clock Source = MCLK1 pin
      IO_Write (16#200#, 16#0001#);

      if Output /= No_Output then
         --  Analog Output Configuration

         --  Enable SPKRVOL PGA, Enable SPKMIXR, Enable SPKLVOL PGA, Enable
         --  SPKMIXL
         IO_Write (16#03#, 16#0300#);

         --  Left Speaker Mixer Volume = 0dB
         IO_Write (16#22#, 16#0000#);

         --  Speaker output mode = Class D, Right Speaker Mixer Volume = 0dB
         --  ((16#23#, 16#0100#) = class AB)
         IO_Write (16#23#, 16#0000#);

         --  Unmute DAC2 (Left) to Left Speaker Mixer (SPKMIXL) path,
         --  Unmute DAC2 (Right) to Right Speaker Mixer (SPKMIXR) path
         IO_Write (16#36#, 16#0300#);

         --  Enable bias generator, Enable VMID, Enable SPKOUTL, Enable SPKOUTR
         IO_Write (16#01#, 16#3003#);

         --  Headphone/Speaker Enable

         --  Enable Class W, Class W Envelope Tracking = AIF1 Timeslot 0
         IO_Write (16#51#, 16#0001#);

         --  Enable bias generator, Enable VMID, Enable HPOUT1 (Left) and
         --  Enable HPOUT1 (Right) input stages idem for Speaker
         Power_Mgnt_Reg_1 := Power_Mgnt_Reg_1 or 16#0303# or 16#3003#;
         IO_Write (16#01#, Power_Mgnt_Reg_1);

         --  Enable HPOUT1 (Left) and HPOUT1 (Right) intermediate stages
         IO_Write (16#60#, 16#0022#);

         --  Enable Charge Pump
         IO_Write (16#4C#, 16#9F25#);

         --  Add Delay
         delay until Clock + Milliseconds (15);

         --  Select DAC1 (Left) to Left Headphone Output PGA (HPOUT1LVOL) path
         IO_Write (16#2D#, 16#0001#);

         --  Select DAC1 (Right) to Right Headphone Output PGA (HPOUT1RVOL) path
         IO_Write (16#2E#, 16#0001#);

         --  Enable Left Output Mixer (MIXOUTL), Enable Right Output Mixer (MIXOUTR)
         --  idem for SPKOUTL and SPKOUTR
         IO_Write (16#03#, 16#0030# or 16#0300#);

         --  Enable DC Servo and trigger start-up mode on left and right channels
         IO_Write (16#54#, 16#0033#);

         --  Add Delay
         delay until Clock + Milliseconds (250);

         --  Enable HPOUT1 (Left) and HPOUT1 (Right) intermediate and output stages. Remove clamps
         IO_Write (16#60#, 16#00EE#);

         --  Unmutes

         --  Unmute DAC 1 (Left)
         IO_Write (16#610#, 16#00C0#);

         --  Unmute DAC 1 (Right)
         IO_Write (16#611#, 16#00C0#);

         --  Unmute the AIF1 Timeslot 0 DAC path
         IO_Write (16#420#, 16#0000#);

         --  Unmute DAC 2 (Left)
         IO_Write (16#612#, 16#00C0#);

         --  Unmute DAC 2 (Right)
         IO_Write (16#613#, 16#00C0#);

         --  Unmute the AIF1 Timeslot 1 DAC2 path
         IO_Write (16#422#, 16#0000#);

         --  Volume Control
         Set_Volume (Volume);
      end if;

      if Input /= No_Input then
         if Input = Microphone then
            --  Enable Microphone bias 1 generator, Enable VMID
            Power_Mgnt_Reg_1 := Power_Mgnt_Reg_1 or 16#0013#;
            IO_Write (16#01#, Power_Mgnt_Reg_1);

            --  ADC oversample enable
            IO_Write (16#620#, 16#0002#);

            --  AIF ADC2 HPF enable, HPF cut = voice mode 1 fc=127Hz at fs=8kHz
            IO_Write (16#411#, 16#3800#);

         elsif Input = Input_Line then
            --  Enable normal bias generator, Enable VMID
            Power_Mgnt_Reg_1 := Power_Mgnt_Reg_1 or 16#0003#;
            IO_Write (16#01#, Power_Mgnt_Reg_1);

            --  Disable mute on IN1L, IN1L Volume = +0dB
            IO_Write (16#18#, 16#000B#);

            --  Disable mute on IN1R, IN1R Volume = +0dB
            IO_Write (16#1A#, 16#000B#);

            --  Disable mute on IN1L_TO_MIXINL, Gain = +0dB
            IO_Write (16#29#, 16#0025#);

            --  Disable mute on IN1R_TO_MIXINL, Gain = +0dB
            IO_Write (16#2A#, 16#0025#);

            --  IN1LN_TO_IN1L, IN1LP_TO_VMID, IN1RN_TO_IN1R, IN1RP_TO_VMID
            IO_Write (16#28#, 16#0011#);

            --  AIF ADC1 HPF enable, HPF cut = hifi mode fc=4Hz at fs=48kHz
            IO_Write (16#410#, 16#1800#);
         end if;

         --  Volume Control
         Set_Volume (Volume);
      end if;
    end Init;

   -------------
   -- Read_ID --
   -------------

   function Read_ID return Half_Word is
   begin
      return IO_Read (WM8994_CHIPID_ADDR);
   end Read_ID;

   ----------
   -- Play --
   ----------

   procedure Play is
   begin
      Set_Mute (Mute_Off);
   end Play;

   -----------
   -- Pause --
   -----------

   procedure Pause is
   begin
      --  Pause the audio playing
      Set_Mute (Mute_On);

      --  CODEC in powersave mode
      IO_Write (16#02#, 16#01#);
   end Pause;

   ------------
   -- Resume --
   ------------

   procedure Resume is
   begin
      Set_Mute (Mute_Off);
   end Resume;

   ----------
   -- Stop --
   ----------

   procedure Stop (Cmd : Stop_Mode) is
   begin
      if Output_Enabled then
         --  Mute the output first
         Set_Mute (Mute_On);

         if Cmd = Stop_Power_Down_Sw then
            return;
         end if;

         Output_Enabled := False;

         --  Mute the AIF1 Timeslot 0 DAC1 path
         IO_Write (16#420#, 16#0200#);

         --  Mute the AIF1 Timeslot 1 DAC2 path
         IO_Write (16#422#, 16#0200#);

         --  Disable DAC1L_TO_HPOUT1L
         IO_Write (16#2D#, 16#0000#);

         --  Disable DAC1R_TO_HPOUT1R
         IO_Write (16#2E#, 16#0000#);

         --  Disable DAC1 and DAC2
         IO_Write (16#05#, 16#0000#);

         --  Reset Codec by writing in 0x0000 address register
         IO_Write (16#0000#, 16#0000#);
      end if;
   end Stop;

   ----------------
   -- Set_Volume --
   ----------------

   procedure Set_Volume (Volume : Byte)
   is
      --  Actual Volume in range 0 .. 16#3F#
      Converted_Volume : constant Half_Word := 100;
--                             Half_Word (Shift_Right (Volume, 2));

   begin
      if Volume = 0 then
         --  Mute the codec
         Set_Mute (Mute_On);
      else
         Set_Mute (Mute_Off);

         --  Left Headphone Volume
         IO_Write (16#1C#, Converted_Volume or 16#140#);

         --  Right Headphone volume
         IO_Write (16#1D#, Converted_Volume or 16#140#);

         --  Left Speaker volume
         IO_Write (16#26#, Converted_Volume or 16#140#);

         --  Right Speaker volume
         IO_Write (16#27#, Converted_Volume or 16#140#);
      end if;
   end Set_Volume;

   --------------
   -- Set_Mute --
   --------------

   procedure Set_Mute (Cmd : Mute) is
   begin
      if Output_Enabled then
         case Cmd is
            when Mute_On =>
               --  Soft Mute the AIF1 Timeslot 0 DAC1 path L&R
               IO_Write (16#420#, 16#0200#);
               --  Soft Mute the AIF1 Timeslot 1 DAC2 path L&R
               IO_Write (16#422#, 16#0200#);
            when Mute_Off =>
               --  Unmute the AIF1 Timeslot 0 DAC1 path L&R
               IO_Write (16#420#, 16#0000#);
               --  Unmute the AIF1 Timeslot 1 DAC2 path L&R
               IO_Write (16#422#, 16#0000#);
         end case;
      end if;
   end Set_Mute;

   ---------------------
   -- Set_Output_Mode --
   ---------------------

   procedure Set_Output_Mode (Device : Output_Device) is
   begin
      case Device is
         when No_Output =>
            --  Disable DAC1 (left), DAC1 (Right)
            IO_Write (16#05#, 16#0000#);
            --  Mute the AIF1 Timeslot 0 DAC1 path
            IO_Write (16#420#, 16#0200#);
            --  Mute the AIF1 Timeslot 1 DAC2 path
            IO_Write (16#422#, 16#0200#);

         when Speaker =>
            --  Enable DAC1 (left), DAC1 (Right)
            IO_Write (16#05#, 16#0C0C#);
            --  Enable the AIF1 Timeslot 0 (Left) to DAC1 (left) mixer path
            IO_Write (16#601#, 16#0000#);
            --  Enable the AIF1 Timeslot 0 (Right) to DAC 1 (Right) mixer path
            IO_Write (16#602#, 16#0000#);
            --  Disable the AIF1 Timeslot 1 (Left) to DAC 2 (Left) mixer path
            IO_Write (16#604#, 16#0002#);
            --  Disable the AIF1 Timeslot 1 (Right) to DAC 2 (Right) mixer path
            IO_Write (16#605#, 16#0002#);

         when Headphone | Auto =>
            --  Disable DAC1 (left), DAC1 (Right)
            --  Enable DAC2 (left), DAC2 (Right)
            IO_Write (16#05#, 16#0303#);
            --  Enable the AIF1 Timeslot 0 (Left) to DAC1 (left) mixer path
            IO_Write (16#601#, 16#0001#);
            --  Enable the AIF1 Timeslot 0 (Right) to DAC 1 (Right) mixer path
            IO_Write (16#602#, 16#0001#);
            --  Disable the AIF1 Timeslot 1 (Left) to DAC 2 (Left) mixer path
            IO_Write (16#604#, 16#0000#);
            --  Disable the AIF1 Timeslot 1 (Right) to DAC 2 (Right) mixer path
            IO_Write (16#605#, 16#0000#);

         when Both =>
            --  Enable DAC1 (left), DAC1 (Right)
            --  Enable DAC2 (left), DAC2 (Right)
            IO_Write (16#05#, 16#0303# or 16#0C0C#);
            --  Enable the AIF1 Timeslot 0 (Left) to DAC1 (left) mixer path
            IO_Write (16#601#, 16#0001#);
            --  Enable the AIF1 Timeslot 0 (Right) to DAC 1 (Right) mixer path
            IO_Write (16#602#, 16#0001#);
            --  Enable the AIF1 Timeslot 1 (Left) to DAC 2 (Left) mixer path
            IO_Write (16#604#, 16#0002#);
            --  Enable the AIF1 Timeslot 1 (Right) to DAC 2 (Right) mixer path
            IO_Write (16#605#, 16#0002#);
      end case;
   end Set_Output_Mode;

   -------------------
   -- Set_Frequency --
   -------------------

   procedure Set_Frequency (Freq : Audio_Frequency) is
   begin
      case Freq is
         when Audio_Freq_8kHz =>
            --  AIF1 Sample Rate = 8 (kHz), ratio=256
            IO_Write (16#210#, 16#0003#);
         when Audio_Freq_16kHz =>
            --  AIF1 Sample Rate = 16 (kHz), ratio=256
            IO_Write (16#210#, 16#0033#);
         when Audio_Freq_48kHz =>
            --  AIF1 Sample Rate = 48 (kHz), ratio=256
            IO_Write (16#210#, 16#0083#);
         when Audio_Freq_96kHz =>
            --  AIF1 Sample Rate = 96 (kHz), ratio=256
            IO_Write (16#210#, 16#00A3#);
         when Audio_Freq_11kHz =>
            --  AIF1 Sample Rate = 11.025 (kHz), ratio=256
            IO_Write (16#210#, 16#0013#);
         when Audio_Freq_22kHz =>
            --  AIF1 Sample Rate = 22.050 (kHz), ratio=256
            IO_Write (16#210#, 16#0043#);
         when Audio_Freq_44kHz =>
            --  AIF1 Sample Rate = 44.1 (kHz), ratio=256
            IO_Write (16#210#, 16#0073#);
      end case;
   end Set_Frequency;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      IO_Write (16#0000#, 16#0000#);
      Output_Enabled := False;
      Input_Enabled  := False;
   end Reset;

end WM8994;
