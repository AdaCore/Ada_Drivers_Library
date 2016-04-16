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

with System;                use System;
private with STM32_SVD.DAC;

package STM32.DAC is

   type Digital_To_Analog_Converter is limited private;

   type DAC_Channel is (Channel_1, Channel_2);

   --  Note that Channel 1 is tied to GPIO pin PA4, and Channel 2 to PA5

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
      Channel_1_Data : Short;
      Channel_2_Data : Short;
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
     (LFSR_Unmask_Bit0,
      LFSR_Unmask_Bits1_0,
      LFSR_Unmask_Bits2_0,
      LFSR_Unmask_Bits3_0,
      LFSR_Unmask_Bits4_0,
      LFSR_Unmask_Bits5_0,
      LFSR_Unmask_Bits6_0,
      LFSR_Unmask_Bits7_0,
      LFSR_Unmask_Bits8_0,
      LFSR_Unmask_Bits9_0,
      LFSR_Unmask_Bits10_0,
      LFSR_Unmask_Bits11_0);
   --  Unmask LFSR bits for noise wave generation

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

   Wave_Generation_Disabled : constant Wave_Generation :=
     (Kind => No_Wave_Generation);

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

   type Digital_To_Analog_Converter is new STM32_SVD.DAC.DAC_Peripheral;

end STM32.DAC;
