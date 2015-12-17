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
--   @file    stm32f4xx_hal_dac.c and stm32f4xx_hal_dac_ex.c                --
--   @author  MCD Application Team                                          --
--   @version V1.3.1                                                        --
--   @date    25-March-2015                                                 --
--   @brief   Header file of DAC HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

package body STM32.DAC is

   ------------
   -- Enable --
   ------------

   procedure Enable
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.Channel_1_Enabled := True;
         when Channel_2 =>
            This.CR.Channel_2_Enabled := True;
      end case;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.Channel_1_Enabled := False;
         when Channel_2 =>
            This.CR.Channel_2_Enabled := False;
      end case;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean is
   begin
      case Channel is
         when Channel_1 =>
            return This.CR.Channel_1_Enabled;
         when Channel_2 =>
            return This.CR.Channel_2_Enabled;
      end case;
   end Enabled;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output
     (This       : in out Digital_To_Analog_Converter;
      Channel    : DAC_Channel;
      Value      : Word;
      Resolution : DAC_Resolution;
      Alignment  : Data_Alignment)
   is
   begin
      case Channel is

         when Channel_1 =>
            case Resolution is
               when DAC_Resolution_12_Bits =>
                  case Alignment is
                     when Left_Aligned =>
                        This.DHR_12_Left_1.Data := Bits_12 (Value and Max_12bit_Resolution);
                     when Right_Aligned =>
                        This.DHR_12_Right_1.Data := Bits_12 (Value and Max_12bit_Resolution);
                  end case;
               when DAC_Resolution_8_Bits =>
                  This.DHR_8_Right_1.Data := Bits_8 (Value and Max_8bit_Resolution);
            end case;

         when Channel_2 =>
            case Resolution is
               when DAC_Resolution_12_Bits =>
                  case Alignment is
                     when Left_Aligned =>
                        This.DHR_12_Left_2.Data := Bits_12 (Value and Max_12bit_Resolution);
                     when Right_Aligned =>
                        This.DHR_12_Right_2.Data := Bits_12 (Value and Max_12bit_Resolution);
                  end case;
               when DAC_Resolution_8_Bits =>
                  This.DHR_8_Right_2.Data := Bits_8 (Value and Max_8bit_Resolution);
            end case;

      end case;
   end Set_Output;

   ------------------------------------
   -- Trigger_Conversion_By_Software --
   ------------------------------------

   procedure Trigger_Conversion_By_Software
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.SWTRIGR.Channel_1_Software_Trigger_Enabled := True; -- cleared by hardware
         when Channel_2 =>
            This.SWTRIGR.Channel_2_Software_Trigger_Enabled := True; -- cleared by hardware
      end case;
   end Trigger_Conversion_By_Software;

   ----------------------------
   -- Converted_Output_Value --
   ----------------------------

   function Converted_Output_Value
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Word
   is
   begin
      case Channel is
         when Channel_1 =>
            return Word (This.DOR1.Data);
         when Channel_2 =>
            return Word (This.DOR2.Data);
      end case;
   end Converted_Output_Value;

   ------------------------------
   -- Set_Dual_Output_Voltages --
   ------------------------------

   procedure Set_Dual_Output_Voltages
     (This            : in out Digital_To_Analog_Converter;
      Channel_1_Value : Word;
      Channel_2_Value : Word;
      Resolution      : DAC_Resolution;
      Alignment       : Data_Alignment)
   is
   begin
      case Resolution is
         when DAC_Resolution_12_Bits =>
            case Alignment is
               when Left_Aligned =>
                  This.DHR_12_Left_Dual.Channel_1_Data := Bits_12 (Channel_1_Value and Max_12bit_Resolution);
                  This.DHR_12_Left_Dual.Channel_2_Data := Bits_12 (Channel_2_Value and Max_12bit_Resolution);
               when Right_Aligned =>
                  This.DHR_12_Right_Dual.Channel_1_Data := Bits_12 (Channel_1_Value and Max_12bit_Resolution);
                  This.DHR_12_Right_Dual.Channel_2_Data := Bits_12 (Channel_2_Value and Max_12bit_Resolution);
            end case;
         when DAC_Resolution_8_Bits =>
            This.DHR_8_Right_Dual.Channel_1_Data := Bits_8 (Channel_1_Value and Max_8bit_Resolution);
            This.DHR_8_Right_Dual.Channel_2_Data := Bits_8 (Channel_2_Value and Max_8bit_Resolution);
      end case;
   end Set_Dual_Output_Voltages;

   ---------------------------------
   -- Converted_Dual_Output_Value --
   ---------------------------------

   function Converted_Dual_Output_Value (This : Digital_To_Analog_Converter)
      return Dual_Channel_Output
   is
      Result : Dual_Channel_Output;
   begin
      Result.Channel_1_Data := Half_Word (This.DOR1.Data);
      Result.Channel_2_Data := Half_Word (This.DOR2.Data);
      return Result;
   end Converted_Dual_Output_Value;

   --------------------------
   -- Enable_Output_Buffer --
   --------------------------

   procedure Enable_Output_Buffer
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.Channel_1_Output_Buffer_Enabled := True;
         when Channel_2 =>
            This.CR.Channel_2_Output_Buffer_Enabled := True;
      end case;
   end Enable_Output_Buffer;

   ---------------------------
   -- Disable_Output_Buffer --
   ---------------------------

   procedure Disable_Output_Buffer
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.Channel_1_Output_Buffer_Enabled := False;
         when Channel_2 =>
            This.CR.Channel_2_Output_Buffer_Enabled := False;
      end case;
   end Disable_Output_Buffer;

   ---------------------------
   -- Output_Buffer_Enabled --
   ---------------------------

   function Output_Buffer_Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean
   is
   begin
      case Channel is
         when Channel_1 =>
            return This.CR.Channel_1_Output_Buffer_Enabled;
         when Channel_2 =>
            return This.CR.Channel_2_Output_Buffer_Enabled;
      end case;
   end Output_Buffer_Enabled;

   --------------------
   -- Select_Trigger --
   --------------------

   procedure Select_Trigger
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel;
      Trigger : External_Event_Trigger_Selection)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.Channel_1_Trigger_Selection := Trigger;
         when Channel_2 =>
            This.CR.Channel_2_Trigger_Selection := Trigger;
      end case;
   end Select_Trigger;

   -----------------------
   -- Trigger_Selection --
   -----------------------

   function Trigger_Selection
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return External_Event_Trigger_Selection
   is
   begin
      case Channel is
         when Channel_1 =>
            return This.CR.Channel_1_Trigger_Selection;
         when Channel_2 =>
            return This.CR.Channel_2_Trigger_Selection;
      end case;
   end Trigger_Selection;

   --------------------
   -- Enable_Trigger --
   --------------------

   procedure Enable_Trigger
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.Channel_1_Trigger_Enabled := True;
         when Channel_2 =>
            This.CR.Channel_2_Trigger_Enabled := True;
      end case;
   end Enable_Trigger;

   ---------------------
   -- Disable_Trigger --
   ---------------------

   procedure Disable_Trigger
     (This : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.Channel_1_Trigger_Enabled := False;
         when Channel_2 =>
            This.CR.Channel_2_Trigger_Enabled := False;
      end case;
   end Disable_Trigger;

   ---------------------
   -- Trigger_Enabled --
   ---------------------

   function Trigger_Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean
   is
   begin
      case Channel is
         when Channel_1 =>
            return This.CR.Channel_1_Trigger_Enabled;
         when Channel_2 =>
            return This.CR.Channel_2_Trigger_Enabled;
      end case;
   end Trigger_Enabled;

   ----------------
   -- Enable_DMA --
   ----------------

   procedure Enable_DMA
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.Channel_1_DMA_Enabled := True;
         when Channel_2 =>
            This.CR.Channel_2_DMA_Enabled := True;
      end case;
   end Enable_DMA;

   -----------------
   -- Disable_DMA --
   -----------------

   procedure Disable_DMA
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.CR.Channel_1_DMA_Enabled := False;
         when Channel_2 =>
            This.CR.Channel_2_DMA_Enabled := False;
      end case;
   end Disable_DMA;

   -----------------
   -- DMA_Enabled --
   -----------------

   function DMA_Enabled
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Boolean
   is
   begin
      case Channel is
         when Channel_1 =>
            return This.CR.Channel_1_DMA_Enabled;
         when Channel_2 =>
            return This.CR.Channel_2_DMA_Enabled;
      end case;
   end DMA_Enabled;

   ------------
   -- Status --
   ------------

   function Status
     (This : Digital_To_Analog_Converter;
      Flag : DAC_Status_Flag)
      return Boolean
   is
   begin
      case Flag is
         when DMA_Underrun_Channel_1 =>
            return This.SR.Channel_1_DMA_Underrun;
         when DMA_Underrun_Channel_2 =>
            return This.SR.Channel_2_DMA_Underrun;
      end case;
   end Status;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status
     (This : in out Digital_To_Analog_Converter;
      Flag : DAC_Status_Flag)
   is
   begin
      case Flag is
         when DMA_Underrun_Channel_1 =>
            This.SR.Channel_1_DMA_Underrun := True;  -- set to 1 to clear
         when DMA_Underrun_Channel_2 =>
            This.SR.Channel_2_DMA_Underrun := True;  -- set to 1 to clear
      end case;
   end Clear_Status;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts
     (This : in out Digital_To_Analog_Converter;
      Source : DAC_Interrupts)
   is
   begin
      case Source is
         when DMA_Underrun_Channel_1 =>
            This.CR.Channel_1_DMA_Underrun_Interrupt_Enabled := True;
         when DMA_Underrun_Channel_2 =>
            This.CR.Channel_2_DMA_Underrun_Interrupt_Enabled := True;
      end case;
   end Enable_Interrupts;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts
     (This   : in out Digital_To_Analog_Converter;
      Source : DAC_Interrupts)
   is
   begin
      case Source is
         when DMA_Underrun_Channel_1 =>
            This.CR.Channel_1_DMA_Underrun_Interrupt_Enabled := False;
         when DMA_Underrun_Channel_2 =>
            This.CR.Channel_2_DMA_Underrun_Interrupt_Enabled := False;
      end case;
   end Disable_Interrupts;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : Digital_To_Analog_Converter;
      Source : DAC_Interrupts)
      return Boolean
   is
   begin
      case Source is
         when DMA_Underrun_Channel_1 =>
            return This.CR.Channel_1_DMA_Underrun_Interrupt_Enabled;
         when DMA_Underrun_Channel_2 =>
            return This.CR.Channel_2_DMA_Underrun_Interrupt_Enabled;
      end case;
   end Interrupt_Enabled;

   ----------------------
   -- Interrupt_Source --
   ----------------------

   function Interrupt_Source
     (This : Digital_To_Analog_Converter)
      return DAC_Interrupts
   is
   begin
      if This.CR.Channel_1_DMA_Underrun_Interrupt_Enabled then
         return DMA_Underrun_Channel_1;
      else
         return DMA_Underrun_Channel_2;
      end if;
   end Interrupt_Source;

   -----------------------------
   -- Clear_Interrupt_Pending --
   -----------------------------

   procedure Clear_Interrupt_Pending
     (This    : in out Digital_To_Analog_Converter;
      Channel : DAC_Channel)
   is
   begin
      case Channel is
         when Channel_1 =>
            This.SR.Channel_1_DMA_Underrun := False;
         when Channel_2 =>
            This.SR.Channel_2_DMA_Underrun := False;
      end case;
   end Clear_Interrupt_Pending;

   ----------------------------
   -- Select_Wave_Generation --
   ----------------------------

   procedure Select_Wave_Generation
     (This      : in out Digital_To_Analog_Converter;
      Channel   : DAC_Channel;
      Selection : Wave_Generation)
   is

      function As_Bits_4 is new Ada.Unchecked_Conversion
        (Source => Noise_Wave_Mask_Selection, Target => Bits_4);

      function As_Bits_4 is new Ada.Unchecked_Conversion
        (Source => Triangle_Wave_Amplitude_Selection, Target => Bits_4);

   begin
      case Channel is
         when Channel_1 =>
            This.CR.Channel_1_Wave_Generation_Enabled := Selection.Kind;
         when Channel_2 =>
            This.CR.Channel_2_Wave_Generation_Enabled := Selection.Kind;
      end case;

      case Selection.Kind is

         when No_Wave_Generation =>
            null;

         when Noise_Wave =>
            case Channel is
               when Channel_1 =>
                  This.CR.Channel_1_Mask_Amplitude_Selector := As_Bits_4 (Selection.Mask);
               when Channel_2 =>
                  This.CR.Channel_2_Mask_Amplitude_Selector := As_Bits_4 (Selection.Mask);
            end case;

         when Triangle_Wave =>
            case Channel is
               when Channel_1 =>
                  This.CR.Channel_1_Mask_Amplitude_Selector := As_Bits_4 (Selection.Amplitude);
               when Channel_2 =>
                  This.CR.Channel_2_Mask_Amplitude_Selector := As_Bits_4 (Selection.Amplitude);
            end case;

      end case;
   end Select_Wave_Generation;

   ------------------------------
   -- Selected_Wave_Generation --
   ------------------------------

   function Selected_Wave_Generation
     (This    : Digital_To_Analog_Converter;
      Channel : DAC_Channel)
      return Wave_Generation
   is
      Kind : Wave_Generation_Selection;

      function As_Mask is new Ada.Unchecked_Conversion
        (Target => Noise_Wave_Mask_Selection, Source => Bits_4);

      function As_Amplitude is new Ada.Unchecked_Conversion
        (Target => Triangle_Wave_Amplitude_Selection, Source => Bits_4);

   begin
      case Channel is
         when Channel_1 =>
            Kind := This.CR.Channel_1_Wave_Generation_Enabled;
         when Channel_2 =>
            Kind := This.CR.Channel_2_Wave_Generation_Enabled;
      end case;
      declare
         Result : Wave_Generation (Kind);
      begin
         case Kind is
            when No_Wave_Generation =>
               null;

            when Noise_Wave =>
               case Channel is
                  when Channel_1 =>
                     Result.Mask := As_Mask (This.CR.Channel_1_Mask_Amplitude_Selector);
                  when Channel_2 =>
                     Result.Mask := As_Mask (This.CR.Channel_2_Mask_Amplitude_Selector);
               end case;

            when Triangle_Wave =>
               case Channel is
                  when Channel_1 =>
                     Result.Amplitude := As_Amplitude (This.CR.Channel_1_Mask_Amplitude_Selector);
                  when Channel_2 =>
                     Result.Amplitude := As_Amplitude (This.CR.Channel_2_Mask_Amplitude_Selector);
               end case;
         end case;

         return Result;
      end;
   end Selected_Wave_Generation;

   ------------------
   -- Data_Address --
   ------------------

   function Data_Address
     (This       : Digital_To_Analog_Converter;
      Channel    : DAC_Channel;
      Resolution : DAC_Resolution;
      Alignment  : Data_Alignment)
      return Address
   is
      Result : Address;
   begin
      case Channel is

         when Channel_1 =>
            case Resolution is
               when DAC_Resolution_12_Bits =>
                  case Alignment is
                     when Left_Aligned =>
                        Result := This.DHR_12_Left_1'Address;
                     when Right_Aligned =>
                        Result := This.DHR_12_Right_1'Address;
                  end case;
               when DAC_Resolution_8_Bits =>
                  Result := This.DHR_8_Right_1'Address;
            end case;

         when Channel_2 =>
            case Resolution is
               when DAC_Resolution_12_Bits =>
                  case Alignment is
                     when Left_Aligned =>
                        Result := This.DHR_12_Left_2'Address;
                     when Right_Aligned =>
                        Result := This.DHR_12_Right_2'Address;
                  end case;
               when DAC_Resolution_8_Bits =>
                  Result := This.DHR_8_Right_2'Address;
            end case;

      end case;

      return Result;
   end Data_Address;

end STM32.DAC;
