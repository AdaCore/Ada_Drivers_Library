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
--   @file    stm32f4xx_hal_dac.h and stm32f4xx_hal_dac_ex.h                --
--   @author  MCD Application Team                                          --
--   @version V1.3.1                                                        --
--   @date    25-March-2015                                                 --
--   @brief   Header file of DAC HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides interfaces for the digital-to-analog converters on the
--  STM32F4 (ARM Cortex M4F) microcontrollers from ST Microelectronics.

package STM32.DAC is

   type Digital_To_Analog_Converter is limited private;

   type DAC_Channel is (Channel_1, Channel_2);

   -- Note that Channel 1 is tied to GPIO pin PA4, and Channel 2 to PA5

   procedure Enable
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with
       Inline,
       Post => Enabled (This, Channel);
   --  Powers up the channel. The channel is then enabled after a startup
   --  time "Twakeup" specified in the datasheet.
   --
   --  NB: When no hardware trigger has been selected, the value in the
   --  DAC_DHRx register is transfered automatically to the DOR register.
   --  Therefore, in that case enabling the channel starts the output
   --  conversion on that channel. See the RM, section 14.3.4 "DAC
   --  conversion" second and third paragraphs.

   procedure Disable
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with
       Inline,
       Post => not Enabled (This, Channel);
   --  When the software trigger has been selected, disabling the channel stops
   --  the output conversion on that channel.

   function Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean;

   type DAC_Resolution is (DAC_Resolution_12_Bits, DAC_Resolution_8_Bits);

   Max_12bit_Resolution : constant := 16#0FFF#;
   Max_8bit_Resolution  : constant := 16#00FF#;

   type Data_Alignment is (Left_Aligned, Right_Aligned);

   procedure Set_Output
     (This       : in out Digital_To_Analog_Converter;
      Channel    : DAC_Channel;
      Value      : Word;
      Resolution : DAC_Resolution;
      Alignment  : Data_Alignment);
   --  For the specified channel, writes the output Value to the data holding
   --  register within This corresponding to the Resolution and Alignment.
   --
   --  The output voltage = ((Value / Max_nbit_Counts) * VRef+), where VRef+ is
   --  the reference input voltage and the 'n' of Max_nbit_Counts is either 12
   --  or 8.

   procedure Trigger_Conversion_By_Software
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with
       Pre => Trigger_Selection (This, Channel) = Software_Trigger and
              Trigger_Enabled (This, Channel);
   --  Cause the conversion to occur and the output to appear, per 14.3.6 "DAC
   --  trigger selection" in the RM. This routine is needed when the Software
   --  Trigger has been selected and the trigger has been enabled, otherwise no
   --  conversion occurs. If you don't enable the trigger any prior selection
   --  has no effect, but note that when no *hardware* trigger is selected the
   --  output happens automatically when the channel is enabled. See the RM,
   --  section 14.3.4 "DAC conversion" second and third paragraphs.

   function Converted_Output_Value
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Word;
   --  Returns the latest output value for the specified channel.

   procedure Set_Dual_Output_Voltages
     (This            : in out Digital_To_Analog_Converter;
      Channel_1_Value : Word;
      Channel_2_Value : Word;
      Resolution      : DAC_Resolution;
      Alignment       : Data_Alignment);

   type Dual_Channel_Output is record
      Channel_1_Data : Half_Word;
      Channel_2_Data : Half_Word;
   end record;

   function Converted_Dual_Output_Value (This : Digital_To_Analog_Converter)
     return Dual_Channel_Output;
   --  Returns the combination of the latest output values for both channels.

   procedure Enable_Output_Buffer
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with Post => Output_Buffer_Enabled (This, Channel);

   procedure Disable_Output_Buffer
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with Post => not Output_Buffer_Enabled (This, Channel);

   function Output_Buffer_Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     return Boolean;

   type External_Event_Trigger_Selection is
     (Timer_6_Output_Trigger,
      Timer_8_Output_Trigger,
      Timer_7_Output_Trigger,
      Timer_5_Output_Trigger,
      Timer_2_Output_Trigger,
      Timer_4_Output_Trigger,
      EXTI_Line_9_Trigger,  -- any GPIO_x Pin_9
      Software_Trigger);

   procedure Select_Trigger
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Trigger : External_Event_Trigger_Selection)
     with
       Pre  => not Trigger_Enabled (This, Channel),  -- per note in RM, pg 435
       Post => Trigger_Selection (This, Channel) = Trigger and
               not Trigger_Enabled (This, Channel);
   --  If the software trigger is selected, output conversion starts once the
   --  channel is enabled.

   function Trigger_Selection
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return External_Event_Trigger_Selection;

   procedure Enable_Trigger
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with Post => Trigger_Enabled (This, Channel);

   procedure Disable_Trigger
     (This : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with Post => not Trigger_Enabled (This, Channel);

   function Trigger_Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean;

   procedure Enable_DMA
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with Post => DMA_Enabled (This, Channel);

   procedure Disable_DMA
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with Post => not DMA_Enabled (This, Channel);

   function DMA_Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean;

   type DAC_Status_Flag is
     (DMA_Underrun_Channel_1,
      DMA_Underrun_Channel_2);
   --  For the indicated channel, the currently selected trigger is driving the
   --  channel conversion at a frequency higher than the DMA service capability
   --  rate

   function Status
     (This : Digital_To_Analog_Converter;
      Flag : DAC_Status_Flag)
      return Boolean;

   procedure Clear_Status
     (This : in out Digital_To_Analog_Converter;
      Flag : DAC_Status_Flag)
     with
       Inline,
       Post => not Status (This, Flag);

   type DAC_Interrupts is
     (DMA_Underrun_Channel_1,
      DMA_Underrun_Channel_2);

   procedure Enable_Interrupts
     (This : in out Digital_To_Analog_Converter;
      Source : DAC_Interrupts)
     with
       Inline,
       Post => Interrupt_Enabled (This, Source);

   procedure Disable_Interrupts
     (This   : in out Digital_To_Analog_Converter;
      Source : DAC_Interrupts)
     with
       Inline,
       Post => not Interrupt_Enabled (This, Source);

   function Interrupt_Enabled
     (This   : Digital_To_Analog_Converter;
      Source : DAC_Interrupts)
      return Boolean
     with Inline;

   function Interrupt_Source
     (This : Digital_To_Analog_Converter)
      return DAC_Interrupts
     with Inline;

   procedure Clear_Interrupt_Pending
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
     with Inline;

   type Wave_Generation_Selection is
     (No_Wave_Generation,
      Noise_Wave,
      Triangle_Wave);

   type Noise_Wave_Mask_Selection is
     (LFSR_Unmask_Bit0,      --  Unmask LFSR bit0 for noise wave generation
      LFSR_Unmask_Bits1_0,   --  Unmask LFSR bit[1:0] for noise wave generation
      LFSR_Unmask_Bits2_0,   --  Unmask LFSR bit[2:0] for noise wave generation
      LFSR_Unmask_Bits3_0,   --  Unmask LFSR bit[3:0] for noise wave generation
      LFSR_Unmask_Bits4_0,   --  Unmask LFSR bit[4:0] for noise wave generation
      LFSR_Unmask_Bits5_0,   --  Unmask LFSR bit[5:0] for noise wave generation
      LFSR_Unmask_Bits6_0,   --  Unmask LFSR bit[6:0] for noise wave generation
      LFSR_Unmask_Bits7_0,   --  Unmask LFSR bit[7:0] for noise wave generation
      LFSR_Unmask_Bits8_0,   --  Unmask LFSR bit[8:0] for noise wave generation
      LFSR_Unmask_Bits9_0,   --  Unmask LFSR bit[9:0] for noise wave generation
      LFSR_Unmask_Bits10_0,  --  Unmask LFSR bit[10:0] for noise wave generation
      LFSR_Unmask_Bits11_0); --  Unmask LFSR bit[11:0] for noise wave generation

   type Triangle_Wave_Amplitude_Selection is
     (Triangle_Amplitude_1,     --  Select max triangle amplitude of 1
      Triangle_Amplitude_3,     --  Select max triangle amplitude of 3
      Triangle_Amplitude_7,     --  Select max triangle amplitude of 7
      Triangle_Amplitude_15,    --  Select max triangle amplitude of 15
      Triangle_Amplitude_31,    --  Select max triangle amplitude of 31
      Triangle_Amplitude_63,    --  Select max triangle amplitude of 63
      Triangle_Amplitude_127,   --  Select max triangle amplitude of 127
      Triangle_Amplitude_255,   --  Select max triangle amplitude of 255
      Triangle_Amplitude_511,   --  Select max triangle amplitude of 511
      Triangle_Amplitude_1023,  --  Select max triangle amplitude of 1023
      Triangle_Amplitude_2047,  --  Select max triangle amplitude of 2047
      Triangle_Amplitude_4095); --  Select max triangle amplitude of 4095

   type Wave_Generation (Kind : Wave_Generation_Selection) is record
      case Kind is
         when No_Wave_Generation =>
            null;
         when Noise_Wave =>
            Mask : Noise_Wave_Mask_Selection;
         when Triangle_Wave =>
            Amplitude : Triangle_Wave_Amplitude_Selection;
      end case;
   end record;

   Wave_Generation_Disabled : constant Wave_Generation := (Kind => No_Wave_Generation);

   procedure Select_Wave_Generation
     (This      : in out Digital_To_Analog_Converter;
      Channel   : DAC_Channel;
      Selection : Wave_Generation)
     with Post => Selected_Wave_Generation (This, Channel) = Selection;

   function Selected_Wave_Generation
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Wave_Generation;

   function Data_Address
     (This       : Digital_To_Analog_Converter;
      Channel    : DAC_Channel;
      Resolution : DAC_Resolution;
      Alignment  : Data_Alignment)
     return Address;
   --  Returns the address of the Data Holding register within This, for the
   --  specified Channel, at the specified Resolution and Alignment.
   --
   --  This function is stricly for use with DMA, all others use the API above.

private

   type Control_Register is record
      Reserved_31_30                           : Bits_2;
      Channel_2_DMA_Underrun_Interrupt_Enabled : Boolean;                          -- DMAUDRIE2
      Channel_2_DMA_Enabled                    : Boolean;                          -- DMAEN2
      Channel_2_Mask_Amplitude_Selector        : Bits_4;                           -- MAMP2
      Channel_2_Wave_Generation_Enabled        : Wave_Generation_Selection;        -- WAVE2
      Channel_2_Trigger_Selection              : External_Event_Trigger_Selection; -- TSEL2
      Channel_2_Trigger_Enabled                : Boolean;                          -- TEN2
      Channel_2_Output_Buffer_Enabled          : Boolean;                          -- BOFF2
      Channel_2_Enabled                        : Boolean;                          -- EN2
      Reserved_15_14                           : Bits_2;
      Channel_1_DMA_Underrun_Interrupt_Enabled : Boolean;                          -- DMAUDRIE1
      Channel_1_DMA_Enabled                    : Boolean;                          -- DMAEN1
      Channel_1_Mask_Amplitude_Selector        : Bits_4;                           -- MAMP1
      Channel_1_Wave_Generation_Enabled        : Wave_Generation_Selection;        -- WAVE1
      Channel_1_Trigger_Selection              : External_Event_Trigger_Selection; -- TSEL1
      Channel_1_Trigger_Enabled                : Boolean;                          -- TEN1
      Channel_1_Output_Buffer_Enabled          : Boolean;                          -- BOFF1
      Channel_1_Enabled                        : Boolean;                          -- EN1
   end record with Volatile_Full_Access, Size => 32;

   for Control_Register use record
      Reserved_31_30                           at 0 range 30 .. 31;
      Channel_2_DMA_Underrun_Interrupt_Enabled at 0 range 29 .. 29;
      Channel_2_DMA_Enabled                    at 0 range 28 .. 28;
      Channel_2_Mask_Amplitude_Selector        at 0 range 24 .. 27;
      Channel_2_Wave_Generation_Enabled        at 0 range 22 .. 23;
      Channel_2_Trigger_Selection              at 0 range 19 .. 21;
      Channel_2_Trigger_Enabled                at 0 range 18 .. 18;
      Channel_2_Output_Buffer_Enabled          at 0 range 17 .. 17;
      Channel_2_Enabled                        at 0 range 16 .. 16;
      Reserved_15_14                           at 0 range 14 .. 15;
      Channel_1_DMA_Underrun_Interrupt_Enabled at 0 range 13 .. 13;
      Channel_1_DMA_Enabled                    at 0 range 12 .. 12;
      Channel_1_Mask_Amplitude_Selector        at 0 range  8 .. 11;
      Channel_1_Wave_Generation_Enabled        at 0 range  6 .. 7;
      Channel_1_Trigger_Selection              at 0 range  3 .. 5;
      Channel_1_Trigger_Enabled                at 0 range  2 .. 2;
      Channel_1_Output_Buffer_Enabled          at 0 range  1 .. 1;
      Channel_1_Enabled                        at 0 range  0 .. 0;
   end record;

   type Software_Trigger_Register is record
      Reserved_31_2                      : Bits_30;
      Channel_2_Software_Trigger_Enabled : Boolean;
      Channel_1_Software_Trigger_Enabled : Boolean;
   end record with Volatile_Full_Access, Size => 32;

   for Software_Trigger_Register use record
      Reserved_31_2                      at 0 range 2 .. 31;
      Channel_2_Software_Trigger_Enabled at 0 range 1 .. 1;
      Channel_1_Software_Trigger_Enabled at 0 range 0 .. 0;
   end record;

   type Right_Aligned_12bit_Data_Holding_Register is record
      Reserved_31_12 : Bits_20;
      Data           : Bits_12;
   end record with Volatile_Full_Access, Size => 32;

   for Right_Aligned_12bit_Data_Holding_Register use record
      Reserved_31_12 at 0 range 12 .. 31;
      Data           at 0 range 0 .. 11;
   end record;

   type Left_Aligned_12bit_Data_Holding_Register is record
      Reserved_31_16 : Half_Word;
      Data           : Bits_12;
      Reserved_3_0   : Bits_4;
   end record with Volatile_Full_Access, Size => 32;

   for Left_Aligned_12bit_Data_Holding_Register use record
      Reserved_31_16 at 0 range 16 .. 31;
      Data           at 0 range  4 .. 15;
      Reserved_3_0   at 0 range  0 .. 3;
   end record;

   type Right_Aligned_8bit_Data_Holding_Register is record
      Reserved_31_8 : Bits_24;
      Data          : Byte;
   end record with Volatile_Full_Access, Size => 32;

   for Right_Aligned_8bit_Data_Holding_Register use record
      Reserved_31_8 at 0 range 8 .. 31;
      Data          at 0 range 0 .. 7;
   end record;

   type Dual_Right_Aligned_12bit_Data_Holding_Register is record
      Reserved_31_28 : Bits_4;
      Channel_2_Data : Bits_12;
      Reserved_15_12 : Bits_4;
      Channel_1_Data : Bits_12;
   end record with Volatile_Full_Access, Size => 32;

   for Dual_Right_Aligned_12bit_Data_Holding_Register use record
      Reserved_31_28 at 0 range 28 .. 31;
      Channel_2_Data at 0 range 16 .. 27;
      Reserved_15_12 at 0 range 12 .. 15;
      Channel_1_Data at 0 range  0 .. 11;
   end record;

   type Dual_Left_Aligned_12bit_Data_Holding_Register is record
      Channel_2_Data : Bits_12;
      Reserved_19_16 : Bits_4;
      Channel_1_Data : Bits_12;
      Reserved_3_0   : Bits_4;
   end record with Volatile_Full_Access, Size => 32;

   for Dual_Left_Aligned_12bit_Data_Holding_Register use record
      Channel_2_Data at 0 range 20 .. 31;
      Reserved_19_16 at 0 range 16 .. 19;
      Channel_1_Data at 0 range  4 .. 15;
      Reserved_3_0   at 0 range  0 .. 3;
   end record;

   type Dual_Right_Aligned_8bit_Data_Holding_Register is record
      Reserved_31_16 : Half_Word;
      Channel_2_Data : Byte;
      Channel_1_Data : Byte;
   end record with Volatile_Full_Access, Size => 32;

   for Dual_Right_Aligned_8bit_Data_Holding_Register use record
      Reserved_31_16 at 0 range 16 .. 31;
      Channel_2_Data at 0 range  8 .. 15;
      Channel_1_Data at 0 range  0 .. 7;
   end record;

   type Data_Output_Register is record
      Reserved_31_12 : Bits_20;
      Data           : Bits_12;
   end record with Volatile_Full_Access, Size => 32;

   for Data_Output_Register use record
      Reserved_31_12 at 0 range 12 .. 31;
      Data           at 0 range  0 .. 11;
   end record;

   type Status_Register is record
      Reserved_31_30         : Bits_2;
      Channel_2_DMA_Underrun : Boolean; -- cleared by writing a one to it
      Reserved_15_14         : Bits_2;
      Channel_1_DMA_Underrun : Boolean; -- cleared by writing a one to it
      Reserved_12_0          : Bits_13;
   end record with Volatile_Full_Access, Size => 32;

   for Status_Register use record
      Reserved_31_30         at 0 range 30 .. 31;
      Channel_2_DMA_Underrun at 0 range 29 .. 29;
      Reserved_15_14         at 0 range 14 .. 15;
      Channel_1_DMA_Underrun at 0 range 13 .. 13;
      Reserved_12_0          at 0 range  0 .. 12;
   end record;

   type Digital_To_Analog_Converter is record
      CR                : Control_Register;
      SWTRIGR           : Software_Trigger_Register;
      DHR_12_Right_1    : Right_Aligned_12bit_Data_Holding_Register; -- channel 1
      DHR_12_Left_1     : Left_Aligned_12bit_Data_Holding_Register;  -- channel 1
      DHR_8_Right_1     : Right_Aligned_8bit_Data_Holding_Register;  -- channel 1
      DHR_12_Right_2    : Right_Aligned_12bit_Data_Holding_Register; -- channel 2
      DHR_12_Left_2     : Left_Aligned_12bit_Data_Holding_Register;  -- channel 2
      DHR_8_Right_2     : Right_Aligned_8bit_Data_Holding_Register;  -- channel 2
      DHR_12_Right_Dual : Dual_Right_Aligned_12bit_Data_Holding_Register;
      DHR_12_Left_Dual  : Dual_Left_Aligned_12bit_Data_Holding_Register;
      DHR_8_Right_Dual  : Dual_Right_Aligned_8bit_Data_Holding_Register;
      DOR1              : Data_Output_Register;                      -- channel 1
      DOR2              : Data_Output_Register;                      -- channel 2
      SR                : Status_Register;
   end record with Volatile, Size => 14 * 32;

   for Digital_To_Analog_Converter use record
      CR                at 16#0#  range 0 .. 31;
      SWTRIGR           at 16#4#  range 0 .. 31;
      DHR_12_Right_1    at 16#8#  range 0 .. 31;
      DHR_12_Left_1     at 16#C#  range 0 .. 31;
      DHR_8_Right_1     at 16#10# range 0 .. 31;
      DHR_12_Right_2    at 16#14# range 0 .. 31;
      DHR_12_Left_2     at 16#18# range 0 .. 31;
      DHR_8_Right_2     at 16#1C# range 0 .. 31;
      DHR_12_Right_Dual at 16#20# range 0 .. 31;
      DHR_12_Left_Dual  at 16#24# range 0 .. 31;
      DHR_8_Right_Dual  at 16#28# range 0 .. 31;
      DOR1              at 16#2C# range 0 .. 31;
      DOR2              at 16#30# range 0 .. 31;
      SR                at 16#34# range 0 .. 31;
   end record;

end STM32.DAC;
