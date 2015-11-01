------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of STMicroelectronics nor the names of its       --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32f4xx_hal_adc.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.3.1                                                        --
--   @date    25-March-2015                                                 --
--   @brief   Header file of ADC HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

package body STM32F4.ADC is

   procedure Set_Sequence_Position
     (This    : in out Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel;
      Rank    : Regular_Channel_Rank)
     with Inline;

   procedure Set_Sampling_Time
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Sample_Time : Channel_Sampling_Times)
     with Inline;

   procedure Set_Injected_Channel_Sequence_Position
     (This    : in out Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel;
      Rank    : Injected_Channel_Rank)
     with Inline;

   procedure Set_Injected_Channel_Offset
     (This   : in out Analog_To_Digital_Converter;
      Rank   : Injected_Channel_Rank;
      Offset : Injected_Data_Offset)
     with Inline;

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

   ---------------------------
   -- Data_Register_Address --
   ---------------------------

   function Data_Register_Address
     (This : Analog_To_Digital_Converter)
      return System.Address
   is
      (This.DR'Address);

   -------------------------------
   -- Injected_Conversion_Value --
   -------------------------------

   function Injected_Conversion_Value
     (This : Analog_To_Digital_Converter;
      Rank : Injected_Channel_Rank)
      return Half_Word
   is
   begin
      case Rank is
         when 1 =>
            return This.JDR1.Data;
         when 2 =>
            return This.JDR2.Data;
         when 3 =>
            return This.JDR3.Data;
         when 4 =>
            return This.JDR4.Data;
      end case;
   end Injected_Conversion_Value;

   --------------------------------
   -- Multimode_Conversion_Value --
   --------------------------------

   function Multimode_Conversion_Value return Word is
     (Common.DR);

   --------------------
   -- Configure_Unit --
   --------------------

   procedure Configure_Unit
     (This       : in out Analog_To_Digital_Converter;
      Resolution : Conversion_Resolution;
      Alignment  : Data_Alignment)
   is
   begin
      This.CR1.Resolution := Resolution;
      This.CR2.Alignment := Alignment;
   end Configure_Unit;

   ------------------------
   -- Current_Resolution --
   ------------------------

   function Current_Resolution
     (This : Analog_To_Digital_Converter)
      return Conversion_Resolution
   is (This.CR1.Resolution);

   -----------------------
   -- Current_Alignment --
   -----------------------

   function Current_Alignment
     (This : Analog_To_Digital_Converter)
      return Data_Alignment
   is (This.CR2.Alignment);

   ---------------------------------
   -- Configure_Common_Properties --
   ---------------------------------

   procedure Configure_Common_Properties
     (Mode           : Multi_ADC_Mode_Selections;
      Prescalar      : ADC_Prescalars;
      DMA_Mode       : Multi_ADC_DMA_Modes;
      Sampling_Delay : Sampling_Delay_Selections)
   is
   begin
      Common.CR.Multi_ADC_Mode     := Mode;
      Common.CR.Sampling_Delay     := Sampling_Delay;
      Common.CR.Multi_ADC_DMA_Mode := DMA_Mode;
      Common.CR.ADC_Prescalar      := Prescalar;
   end Configure_Common_Properties;

   -----------------------------------
   -- Configure_Regular_Conversions --
   -----------------------------------

   procedure Configure_Regular_Conversions
     (This        : in out Analog_To_Digital_Converter;
      Continuous  : Boolean;
      Trigger     : Regular_Channel_Conversion_Trigger;
      Enable_EOC  : Boolean;
      Conversions : Regular_Channel_Conversions)
   is
      Total_Regular_Conversions : Bits_4;
   begin
      This.CR2.End_Of_Conversion_Enabled := Enable_EOC;

      This.CR2.Continuous_Conversion_Enabled := Continuous;

      This.CR1.Scan_Mode_Enabled := Conversions'Length > 1;

      This.CR2.External_Trigger_Enable := Trigger.Enabler;
      if Trigger.Enabler /= Trigger_Disabled then
         This.CR2.External_Event_Select_Regular := Trigger.Event;
      end if;

      Total_Regular_Conversions := 0;
      for Rank in Conversions'Range loop
         declare
            Conversion : Regular_Channel_Conversion renames Conversions (Rank);
         begin
            Configure_Regular_Channel
              (This, Conversion.Channel, Rank, Conversion.Sample_Time);

            --  We check the VBat first because that channel is also used for
            --  the temperature sensor channel on some MCUs, in which case the
            --  VBat conversion is the only one done. This order reflects that
            --  hardware behavior.
            if VBat_Conversion (This, Conversion.Channel) then
               Enable_VBat_Connection;
            elsif VRef_TemperatureSensor_Conversion (This, Conversion.Channel) then
               Enable_VRef_TemperatureSensor_Connection;
            end if;

            Total_Regular_Conversions := Total_Regular_Conversions + 1;
         end;
      end loop;
      This.SQR1.Length := Total_Regular_Conversions - 1;  -- biased
   end Configure_Regular_Conversions;

   ------------------------------------
   -- Configure_Injected_Conversions --
   ------------------------------------

   procedure Configure_Injected_Conversions
     (This          : in out Analog_To_Digital_Converter;
      AutoInjection : Boolean;
      Trigger       : Injected_Channel_Conversion_Trigger;
      Enable_EOC    : Boolean;
      Conversions   : Injected_Channel_Conversions)
   is
      Total_Injected_Conversions : Bits_2; -- at most four channels possible
   begin
      This.CR2.End_Of_Conversion_Enabled := Enable_EOC;

      --  Injected channels cannot be converted continuously. The only
      --  exception is when an injected channel is configured to be converted
      --  automatically after regular channels in continuous mode. See note in
      --  RM 13.3.5, pg 390, and "Auto-injection" section on pg 392.
      This.CR1.Auto_Injected_Group_Conversion_Enabled := AutoInjection;

      This.CR2.External_Trigger_Injected_Enable := Trigger.Enabler;
      if Trigger.Enabler /= Trigger_Disabled then
         This.CR2.External_Event_Select_Injected := Trigger.Event;
      end if;

      Total_Injected_Conversions := 0;
      for Rank in Conversions'Range loop
         declare
            Conversion : Injected_Channel_Conversion renames Conversions (Rank);
         begin
            Configure_Injected_Channel
              (This, Conversion.Channel, Rank, Conversion.Sample_Time, Conversion.Offset);

            --  We check the VBat first because that channel is also used for
            --  the temperature sensor channel on some MCUs, in which case the
            --  VBat conversion is the only one done. This order reflects that
            --  hardware behavior.
            if VBat_Conversion (This, Conversion.Channel) then
               Enable_VBat_Connection;
            elsif VRef_TemperatureSensor_Conversion (This, Conversion.Channel) then
               Enable_VRef_TemperatureSensor_Connection;
            end if;

            Total_Injected_Conversions := Total_Injected_Conversions + 1;
         end;
      end loop;
      This.JSQR.Length := Total_Injected_Conversions - 1;  -- biased
   end Configure_Injected_Conversions;

   ----------------------------
   -- Enable_VBat_Connection --
   ----------------------------

   procedure Enable_VBat_Connection is
   begin
      Common.CR.VBat_Channel_Enabled := True;
   end Enable_VBat_Connection;

   ------------------
   -- VBat_Enabled --
   ------------------

   function VBat_Enabled return Boolean is
      (Common.CR.VBat_Channel_Enabled);

   ----------------------------------------------
   -- Enable_VRef_TemperatureSensor_Connection --
   ----------------------------------------------

   procedure Enable_VRef_TemperatureSensor_Connection is
   begin
      Common.CR.TSVREF_Enabled := True;
      delay until Clock + Temperature_Sensor_Stabilization;
   end Enable_VRef_TemperatureSensor_Connection;

   --------------------------------------
   -- VRef_TemperatureSensor_Connected --
   --------------------------------------

   function VRef_TemperatureSensor_Enabled return Boolean is
      (Common.CR.TSVREF_Enabled);

   ----------------------------------
   -- Regular_Conversions_Expected --
   ----------------------------------

   function Regular_Conversions_Expected (This : Analog_To_Digital_Converter)
     return Natural is
     (Natural (This.SQR1.Length) + 1);

   -----------------------------------
   -- Injected_Conversions_Expected --
   -----------------------------------

   function Injected_Conversions_Expected (This : Analog_To_Digital_Converter)
     return Natural is
     (Natural (This.JSQR.Length) + 1);

   -----------------------
   -- Scan_Mode_Enabled --
   -----------------------

   function Scan_Mode_Enabled (This : Analog_To_Digital_Converter) return Boolean is
     (This.CR1.Scan_Mode_Enabled);

   ---------------------------
   -- EOC_Selection_Enabled --
   ---------------------------

   function EOC_Selection_Enabled (This : Analog_To_Digital_Converter) return Boolean is
      (This.CR2.End_Of_Conversion_Enabled);

   -------------------------------
   -- Configure_Regular_Channel --
   -------------------------------

   procedure Configure_Regular_Channel
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Rank        : Regular_Channel_Rank;
      Sample_Time : Channel_Sampling_Times)
   is
   begin
      Set_Sampling_Time (This, Channel, Sample_Time);
      Set_Sequence_Position (This, Channel, Rank);
   end Configure_Regular_Channel;

   --------------------------------
   -- Configure_Injected_Channel --
   --------------------------------

   procedure Configure_Injected_Channel
     (This        : in out Analog_To_Digital_Converter;
      Channel     : Analog_Input_Channel;
      Rank        : Injected_Channel_Rank;
      Sample_Time : Channel_Sampling_Times;
      Offset      : Injected_Data_Offset)
   is
   begin
      Set_Sampling_Time (This, Channel, Sample_Time);
      Set_Injected_Channel_Sequence_Position (This, Channel, Rank);
      Set_Injected_Channel_Offset (This, Rank, Offset);
   end Configure_Injected_Channel;

   ----------------------
   -- Start_Conversion --
   ----------------------

   procedure Start_Conversion (This : in out Analog_To_Digital_Converter) is
   begin
      if Common.CR.Multi_ADC_Mode = Independent then
         if This.CR2.External_Trigger_Enable = Trigger_Disabled then
            This.CR2.Start_Conversion_Regular_Channels := True;
         end if;
      else
         if (This'Address = ADC1_Base) and
            (This.CR2.External_Trigger_Enable = Trigger_Disabled)
         then
            This.CR2.Start_Conversion_Regular_Channels := True;
         end if;
      end if;
   end Start_Conversion;

   ------------------------
   -- Conversion_Started --
   ------------------------

   function Conversion_Started (This : Analog_To_Digital_Converter)
      return Boolean
   is
      (This.CR2.Start_Conversion_Regular_Channels);

   -------------------------------
   -- Start_Injected_Conversion --
   -------------------------------

   procedure Start_Injected_Conversion
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      This.CR2.Start_Conversion_Injected_Channels := True;
   end Start_Injected_Conversion;

   ---------------------------------
   -- Injected_Conversion_Started --
   ---------------------------------

   function Injected_Conversion_Started (This : Analog_To_Digital_Converter)
     return Boolean
   is
     (This.CR2.Start_Conversion_Injected_Channels);

   ------------------------------
   -- Watchdog_Enable_Channels --
   ------------------------------

   procedure Watchdog_Enable_Channels
     (This : in out Analog_To_Digital_Converter;
      Mode : Multiple_Channels_Watchdog;
      Low  : Watchdog_Threshold;
      High : Watchdog_Threshold)
   is
   begin
      This.HTR.Threshold := High;
      This.LTR.Threshold := Low;
      --  see RM 13.3.7, pg 391, table 66
      case Mode is
         when Watchdog_All_Regular_Channels =>
            This.CR1.Watchdog_Regular_Channels_Enabled := True;
         when Watchdog_All_Injected_Channels =>
            This.CR1.Watchdog_Injected_Channels_Enabled := True;
         when Watchdog_All_Both_Kinds =>
            This.CR1.Watchdog_Regular_Channels_Enabled := True;
            This.CR1.Watchdog_Injected_Channels_Enabled := True;
      end case;
   end Watchdog_Enable_Channels;

   -----------------------------
   -- Watchdog_Enable_Channel --
   -----------------------------

   procedure Watchdog_Enable_Channel
     (This    : in out Analog_To_Digital_Converter;
      Mode    : Single_Channel_Watchdog;
      Channel : Analog_Input_Channel;
      Low     : Watchdog_Threshold;
      High    : Watchdog_Threshold)
   is
   begin
      This.HTR.Threshold := High;
      This.LTR.Threshold := Low;

      This.CR1.Watchdog_Selected_Channel := Channel;

      This.CR1.Watchdog_Single_Channel_Enabled := True;
      case Mode is
         when Watchdog_Single_Regular_Channel =>
            This.CR1.Watchdog_Regular_Channels_Enabled := True;
         when Watchdog_Single_Injected_Channel =>
            This.CR1.Watchdog_Injected_Channels_Enabled := True;
         when Watchdog_Single_Both_Kinds =>
            This.CR1.Watchdog_Regular_Channels_Enabled := True;
            This.CR1.Watchdog_Injected_Channels_Enabled := True;
      end case;
   end Watchdog_Enable_Channel;

   ----------------------
   -- Watchdog_Disable --
   ----------------------

   procedure Watchdog_Disable (This : in out Analog_To_Digital_Converter) is
   begin
      This.CR1.Watchdog_Regular_Channels_Enabled := False;
      This.CR1.Watchdog_Injected_Channels_Enabled := False;
      This.CR1.Watchdog_Single_Channel_Enabled := False;
      --  clearing the single-channel bit (AWGSDL) is not required to disable,
      --  per the RM table 66, section 13.3.7, pg 391, but seems cleanest
   end Watchdog_Disable;

   ----------------------
   -- Watchdog_Enabled --
   ----------------------

   function Watchdog_Enabled (This : Analog_To_Digital_Converter)
     return Boolean
   is
      (This.CR1.Watchdog_Regular_Channels_Enabled or
       This.CR1.Watchdog_Injected_Channels_Enabled);
   --  per the RM table 66, section 13.3.7, pg 391

   -------------------------------
   -- Enable_Discontinuous_Mode --
   -------------------------------

   procedure Enable_Discontinuous_Mode
     (This    : in out Analog_To_Digital_Converter;
      Regular : Boolean;  -- if False, enabling for Injected channels
      Count   : Discontinuous_Mode_Channel_Count)
   is
   begin
      if Regular then
         This.CR1.Discontinuous_Mode_Regular_Enabled := True;
         This.CR1.Discontinuous_Mode_Injected_Enabled := False;
      else -- Injected
         This.CR1.Discontinuous_Mode_Regular_Enabled := False;
         This.CR1.Discontinuous_Mode_Injected_Enabled := True;
      end if;
      This.CR1.Discontinuous_Mode_Channel_Count := Bits_3 (Count - 1);  -- biased
   end Enable_Discontinuous_Mode;

   ----------------------------------------
   -- Disable_Discontinuous_Mode_Regular --
   ---------------------------------------

   procedure Disable_Discontinuous_Mode_Regular
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      This.CR1.Discontinuous_Mode_Regular_Enabled := False;
   end Disable_Discontinuous_Mode_Regular;

   -----------------------------------------
   -- Disable_Discontinuous_Mode_Injected --
   -----------------------------------------

   procedure Disable_Discontinuous_Mode_Injected
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      This.CR1.Discontinuous_Mode_Injected_Enabled := False;
   end Disable_Discontinuous_Mode_Injected;

   ----------------------------------------
   -- Discontinuous_Mode_Regular_Enabled --
   ----------------------------------------

   function Discontinuous_Mode_Regular_Enabled
     (This : Analog_To_Digital_Converter)
      return Boolean
   is (This.CR1.Discontinuous_Mode_Regular_Enabled);

   -----------------------------------------
   -- Discontinuous_Mode_Injected_Enabled --
   -----------------------------------------

   function Discontinuous_Mode_Injected_Enabled
     (This : Analog_To_Digital_Converter)
      return Boolean
   is (This.CR1.Discontinuous_Mode_Injected_Enabled);

   ---------------------------
   -- AutoInjection_Enabled --
   ---------------------------

   function AutoInjection_Enabled
     (This : Analog_To_Digital_Converter)
      return Boolean
   is (This.CR1.Auto_Injected_Group_Conversion_Enabled);

   ----------------
   -- Enable_DMA --
   ----------------

   procedure Enable_DMA (This : in out Analog_To_Digital_Converter) is
   begin
      This.CR2.DMA_Enabled := True;
   end Enable_DMA;

   -----------------
   -- Disable_DMA --
   -----------------

   procedure Disable_DMA (This : in out Analog_To_Digital_Converter) is
   begin
      This.CR2.DMA_Enabled := False;
   end Disable_DMA;

   -----------------
   -- DMA_Enabled --
   -----------------

   function DMA_Enabled (This : Analog_To_Digital_Converter) return Boolean is
     (This.CR2.DMA_Enabled);

   ------------------------------------
   -- Enable_DMA_After_Last_Transfer --
   ------------------------------------

   procedure Enable_DMA_After_Last_Transfer
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      This.CR2.DMA_Requests_After_Last_Xfer := True;
   end Enable_DMA_After_Last_Transfer;

   -------------------------------------
   -- Disable_DMA_After_Last_Transfer --
   -------------------------------------

   procedure Disable_DMA_After_Last_Transfer
     (This : in out Analog_To_Digital_Converter)
   is
   begin
      This.CR2.DMA_Requests_After_Last_Xfer := False;
   end Disable_DMA_After_Last_Transfer;

   -------------------------------------
   -- DMA_Enabled_After_Last_Transfer --
   -------------------------------------

   function DMA_Enabled_After_Last_Transfer
     (This : Analog_To_Digital_Converter)
      return Boolean
   is (This.CR2.DMA_Requests_After_Last_Xfer);

   ------------------------------------------
   -- Multi_Enable_DMA_After_Last_Transfer --
   ------------------------------------------

   procedure Multi_Enable_DMA_After_Last_Transfer is
   begin
      Common.CR.Multi_ADC_DMA_Enabled := True;
   end Multi_Enable_DMA_After_Last_Transfer;

   -------------------------------------------
   -- Multi_Disable_DMA_After_Last_Transfer --
   -------------------------------------------

   procedure Multi_Disable_DMA_After_Last_Transfer is
   begin
      Common.CR.Multi_ADC_DMA_Enabled := False;
   end Multi_Disable_DMA_After_Last_Transfer;

   -------------------------------------------
   -- Multi_DMA_Enabled_After_Last_Transfer --
   -------------------------------------------

   function Multi_DMA_Enabled_After_Last_Transfer return Boolean is
     (Common.CR.Multi_ADC_DMA_Enabled);

   ---------------------
   -- Poll_For_Status --
   ---------------------

   procedure Poll_For_Status
     (This    : in out Analog_To_Digital_Converter;
      Flag    : ADC_Status_Flag;
      Success : out Boolean;
      Timeout : Time_Span := Time_Span_Last)
   is
      Deadline : constant Time := Clock + Timeout;
   begin
      Success := False;
      while Clock < Deadline loop
         if Status (This, Flag) then
            Success := True;
            exit;
         end if;
      end loop;
   end Poll_For_Status;

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
      Source : ADC_Interrupts)
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
            This.CR1.Watchdog_Interrupt_Enabled := True;
      end case;
   end Enable_Interrupts;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
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
            return This.CR1.Watchdog_Interrupt_Enabled;
      end case;
   end Interrupt_Enabled;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
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
            This.CR1.Watchdog_Interrupt_Enabled := False;
      end case;
   end Disable_Interrupts;

   -----------------------------
   -- Clear_Interrupt_Pending --
   -----------------------------

   procedure Clear_Interrupt_Pending
     (This   : in out Analog_To_Digital_Converter;
      Source : ADC_Interrupts)
   is
   begin
      case Source is
         when Overrun =>
            This.SR.Overrun := False;
         when Injected_Channel_Conversion_Complete =>
            This.SR.Injected_Channel_Conversion_Complete := False;
         when Regular_Channel_Conversion_Complete =>
            This.SR.Regular_Channel_Conversion_Complete := False;
         when Analog_Watchdog_Event =>
            This.SR.Analog_Watchdog_Event_Occurred := False;
      end case;
   end Clear_Interrupt_Pending;

   ---------------------------
   -- Set_Sequence_Position --
   ---------------------------

   procedure Set_Sequence_Position
     (This    : in out Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel;
      Rank    : Regular_Channel_Rank)
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

   --------------------------------------------
   -- Set_Injected_Channel_Sequence_Position --
   --------------------------------------------

   procedure Set_Injected_Channel_Sequence_Position
     (This    : in out Analog_To_Digital_Converter;
      Channel : Analog_Input_Channel;
      Rank    : Injected_Channel_Rank)
   is
   begin
      This.JSQR.SQ (Rank) := Channel;
   end Set_Injected_Channel_Sequence_Position;

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

   ---------------------------------
   -- Set_Injected_Channel_Offset --
   ---------------------------------

   procedure Set_Injected_Channel_Offset
     (This   : in out Analog_To_Digital_Converter;
      Rank   : Injected_Channel_Rank;
      Offset : Injected_Data_Offset)
   is
   begin
      case Rank is
         when 1 => This.JOFR1.JOffset := Offset;
         when 2 => This.JOFR2.JOffset := Offset;
         when 3 => This.JOFR3.JOffset := Offset;
         when 4 => This.JOFR4.JOffset := Offset;
      end case;
   end Set_Injected_Channel_Offset;

end STM32F4.ADC;
