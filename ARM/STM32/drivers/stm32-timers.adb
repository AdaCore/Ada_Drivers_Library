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
--   @file    stm32f4xx_hal_tim.c                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   timers HAL module driver.                                     --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

package body STM32.Timers is

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This      : in out Timer;
      Prescaler : Short;
      Period    : Word)
   is
   begin
      This.ARR := Period;
      This.Prescaler := Prescaler;
   end Configure;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This          : in out Timer;
      Prescaler     : Short;
      Period        : Word;
      Clock_Divisor : Timer_Clock_Divisor;
      Counter_Mode  : Timer_Counter_Alignment_Mode)
   is
   begin
      This.ARR := Period;
      This.Prescaler := Prescaler;
      This.CR1.Clock_Division := Clock_Divisor;
      This.CR1.Mode_And_Dir := Counter_Mode;
   end Configure;

   ----------------------
   -- Set_Counter_Mode --
   ----------------------

   procedure Set_Counter_Mode
     (This  : in out Timer;
      Value : Timer_Counter_Alignment_Mode)
   is
   begin
      This.CR1.Mode_And_Dir := Value;
   end Set_Counter_Mode;

   ------------------------
   -- Set_Clock_Division --
   ------------------------

   procedure Set_Clock_Division
     (This  : in out Timer;
      Value : Timer_Clock_Divisor)
   is
   begin
      This.CR1.Clock_Division := Value;
   end Set_Clock_Division;

   ----------------------------
   -- Current_Clock_Division --
   ----------------------------

   function Current_Clock_Division (This : Timer) return Timer_Clock_Divisor is
   begin
      return This.CR1.Clock_Division;
   end Current_Clock_Division;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This          : in out Timer;
      Prescaler     : Short;
      Period        : Word;
      Clock_Divisor : Timer_Clock_Divisor;
      Counter_Mode  : Timer_Counter_Alignment_Mode;
      Repetitions   : Byte)
   is
   begin
      This.ARR := Period;
      This.Prescaler := Prescaler;
      This.CR1.Clock_Division := Clock_Divisor;
      This.CR1.Mode_And_Dir := Counter_Mode;
      This.RCR := Word (Repetitions);
      This.EGR := Immediate'Enum_Rep;
   end Configure;

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out Timer) is
   begin
      This.CR1.Timer_Enabled := True;
   end Enable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : Timer) return Boolean is
   begin
      return This.CR1.Timer_Enabled;
   end Enabled;

   ------------------------
   -- No_Outputs_Enabled --
   ------------------------

   function No_Outputs_Enabled (This : Timer) return Boolean is
   begin
      for C in Channel_1 .. Channel_3 loop
         if This.CCER (C).CCxE = Enable or This.CCER (C).CCxNE = Enable then
            return False;
         end if;
      end loop;
      --  Channel_4 doesn't have the complementary enabler and polarity bits.
      --  If it did they would be in the reserved area, which is zero, so we
      --  could be tricky and pretend that they exist for this function but
      --  doing that would be unnecessarily subtle.  The money is on clarity.
      if This.CCER (Channel_4).CCxE = Enable then
         return False;
      end if;
      return True;
   end No_Outputs_Enabled;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out Timer) is
   begin
      if No_Outputs_Enabled (This) then
         This.CR1.Timer_Enabled := False;
      end if;
   end Disable;

   ------------------------
   -- Enable_Main_Output --
   ------------------------

   procedure Enable_Main_Output (This : in out Timer) is
   begin
      This.BDTR.Main_Output_Enabled := True;
   end Enable_Main_Output;

   -------------------------
   -- Disable_Main_Output --
   -------------------------

   procedure Disable_Main_Output (This : in out Timer) is
   begin
      if No_Outputs_Enabled (This) then
         This.BDTR.Main_Output_Enabled := False;
      end if;
   end Disable_Main_Output;

   -------------------------
   -- Main_Output_Enabled --
   -------------------------

   function Main_Output_Enabled (This : Timer) return Boolean is
   begin
      return This.BDTR.Main_Output_Enabled;
   end Main_Output_Enabled;

   -----------------
   -- Set_Counter --
   -----------------

   procedure Set_Counter (This : in out Timer;  Value : Short) is
   begin
      This.Counter := Word (Value);
   end Set_Counter;

   -----------------
   -- Set_Counter --
   -----------------

   procedure Set_Counter (This : in out Timer;  Value : Word) is
   begin
      This.Counter := Value;
   end Set_Counter;

   ---------------------
   -- Current_Counter --
   ---------------------

   function Current_Counter (This : Timer) return Word is
   begin
      return This.Counter;
   end Current_Counter;

   --------------------
   -- Set_Autoreload --
   --------------------

   procedure Set_Autoreload (This : in out Timer;  Value : Word) is
   begin
      This.ARR := Value;
   end Set_Autoreload;

   ------------------------
   -- Current_Autoreload --
   ------------------------

   function Current_Autoreload (This : Timer) return Word is
   begin
      return This.ARR;
   end Current_Autoreload;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (This   : in out Timer;
      Source : Timer_Interrupt)
   is
   begin
      This.DIER := This.DIER or Source'Enum_Rep;
   end Enable_Interrupt;

   ----------------------
   -- Enable_Interrupt --
   ----------------------

   procedure Enable_Interrupt
     (This    : in out Timer;
      Sources : Timer_Interrupt_List)
   is
   begin
      for Source of Sources loop
         This.DIER := This.DIER or Source'Enum_Rep;
      end loop;
   end Enable_Interrupt;

   -----------------------
   -- Disable_Interrupt --
   -----------------------

   procedure Disable_Interrupt
     (This   : in out Timer;
      Source : Timer_Interrupt)
   is
   begin
      This.DIER := This.DIER and not Source'Enum_Rep;
   end Disable_Interrupt;

   -----------------------------
   -- Clear_Pending_Interrupt --
   -----------------------------

   procedure Clear_Pending_Interrupt
     (This   : in out Timer;
      Source : Timer_Interrupt)
   is
   begin
      This.SR := not Source'Enum_Rep;
      --  We do not, and must not, use the read-modify-write pattern because
      --  it leaves a window of vulnerability open to changes to the state
      --  after the read but before the write. The hardware for this register
      --  is designed so that writing other bits will not change them. This is
      --  indicated by the "rc_w0" notation in the status register definition.
      --  See the RM, page 57 for that notation explanation.
   end Clear_Pending_Interrupt;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : Timer;
      Source : Timer_Interrupt)
      return Boolean
   is
   begin
      return (This.DIER and Source'Enum_Rep) = Source'Enum_Rep;
   end Interrupt_Enabled;

   ------------
   -- Status --
   ------------

   function Status (This : Timer;  Flag : Timer_Status_Flag) return Boolean is
   begin
      return (This.SR and Flag'Enum_Rep) = Flag'Enum_Rep;
   end Status;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status (This : in out Timer;  Flag : Timer_Status_Flag) is
   begin
      This.SR := not Flag'Enum_Rep;
      --  We do not, and must not, use the read-modify-write pattern because
      --  it leaves a window of vulnerability open to changes to the state
      --  after the read but before the write. The hardware for this register
      --  is designed so that writing other bits will not change them. This is
      --  indicated by the "rc_w0" notation in the status register definition.
      --  See the RM, page 57 for that notation explanation.
   end Clear_Status;

   -----------------------
   -- Enable_DMA_Source --
   -----------------------

   procedure Enable_DMA_Source
     (This   : in out Timer;
      Source : Timer_DMA_Source)
   is
   begin
      This.DIER := This.DIER or Source'Enum_Rep;
   end Enable_DMA_Source;

   ------------------------
   -- Disable_DMA_Source --
   ------------------------

   procedure Disable_DMA_Source
     (This   : in out Timer;
      Source : Timer_DMA_Source)
   is
   begin
      This.DIER := This.DIER and not Source'Enum_Rep;
   end Disable_DMA_Source;

   ------------------------
   -- DMA_Source_Enabled --
   ------------------------

   function DMA_Source_Enabled
     (This   : Timer;
      Source : Timer_DMA_Source)
      return Boolean
   is
   begin
      return (This.DIER and Source'Enum_Rep) = Source'Enum_Rep;
   end DMA_Source_Enabled;

   -------------------------
   -- Configure_Prescaler --
   -------------------------

   procedure Configure_Prescaler
     (This        : in out Timer;
      Prescaler   : Short;
      Reload_Mode : Timer_Prescaler_Reload_Mode)
   is
   begin
      This.Prescaler := Prescaler;
      This.EGR := Reload_Mode'Enum_Rep;
   end Configure_Prescaler;

   -------------------
   -- Configure_DMA --
   -------------------

   procedure Configure_DMA
     (This         : in out Timer;
      Base_Address : Timer_DMA_Base_Address;
      Burst_Length : Timer_DMA_Burst_Length)
   is
   begin
      This.DCR.Base_Address := Base_Address;
      This.DCR.Burst_Length := Burst_Length;
   end Configure_DMA;

   --------------------------------
   -- Enable_Capture_Compare_DMA --
   --------------------------------

   procedure Enable_Capture_Compare_DMA
     (This : in out Timer)
     --  TODO: note that the CCDS field description in the RM, page 550, seems
     --  to indicate other than simply enabled/disabled
   is
   begin
      This.CR2.Capture_Compare_DMA_Selection := True;
   end Enable_Capture_Compare_DMA;

   ---------------------------------
   -- Disable_Capture_Compare_DMA --
   ---------------------------------

   procedure Disable_Capture_Compare_DMA
     (This : in out Timer)
     --  TODO: note that the CCDS field description in the RM, page 550, seems
     --  to indicate other than simply enabled/disabled
   is
   begin
      This.CR2.Capture_Compare_DMA_Selection := False;
   end Disable_Capture_Compare_DMA;

   -----------------------
   -- Current_Prescaler --
   -----------------------

   function Current_Prescaler (This : Timer) return Short is
   begin
      return This.Prescaler;
   end Current_Prescaler;

   -----------------------
   -- Set_UpdateDisable --
   -----------------------

   procedure Set_UpdateDisable
     (This : in out Timer;
      To   : Boolean)
   is
   begin
      This.CR1.Update_Disable := To;
   end Set_UpdateDisable;

   -----------------------
   -- Set_UpdateRequest --
   -----------------------

   procedure Set_UpdateRequest
     (This   : in out Timer;
      Source : Timer_Update_Source)
   is
   begin
      This.CR1.Update_Request_Source := Source /= Global;
   end Set_UpdateRequest;

   ---------------------------
   -- Select_One_Pulse_Mode --
   ---------------------------

   procedure Select_One_Pulse_Mode
     (This : in out Timer;
      Mode : Timer_One_Pulse_Mode)
   is
   begin
      This.CR1.One_Pulse_Mode := Mode;
   end Select_One_Pulse_Mode;

   ----------------------------
   -- Set_Autoreload_Preload --
   ----------------------------

   procedure Set_Autoreload_Preload
     (This : in out Timer;
      To   : Boolean)
   is
   begin
      This.CR1.ARPE := To;
   end Set_Autoreload_Preload;

   -----------------------
   -- Counter_Direction --
   -----------------------

   function Current_Counter_Mode
     (This : Timer)
      return Timer_Counter_Alignment_Mode
   is
   begin
      if Basic_Timer (This) then
         return Up;
      else
         return This.CR1.Mode_And_Dir;
      end if;
   end Current_Counter_Mode;

   --------------------
   -- Generate_Event --
   --------------------

   procedure Generate_Event
     (This   : in out Timer;
      Source : Timer_Event_Source)
   is
      Temp_EGR : Word := This.EGR;
   begin
      Temp_EGR := Temp_EGR or Source'Enum_Rep;
      This.EGR  := Temp_EGR;
   end Generate_Event;

   ---------------------------
   -- Select_Output_Trigger --
   ---------------------------

   procedure Select_Output_Trigger
     (This   : in out Timer;
      Source : Timer_Trigger_Output_Source)
   is
   begin
      This.CR2.Master_Mode_Selection := Source;
   end Select_Output_Trigger;

   -----------------------
   -- Select_Slave_Mode --
   -----------------------

   procedure Select_Slave_Mode
     (This : in out Timer;
      Mode : Timer_Slave_Mode)
   is
   begin
      This.SMCR.Slave_Mode_Selection := Mode;
   end Select_Slave_Mode;

   ------------------------------
   -- Enable_Master_Slave_Mode --
   ------------------------------

   procedure Enable_Master_Slave_Mode (This : in out Timer) is
   begin
      This.SMCR.Master_Slave_Mode := True;
   end Enable_Master_Slave_Mode;

   -------------------------------
   -- Disable_Master_Slave_Mode --
   -------------------------------

   procedure Disable_Master_Slave_Mode (This : in out Timer) is
   begin
      This.SMCR.Master_Slave_Mode := False;
   end Disable_Master_Slave_Mode;

   --------------------------------
   -- Configure_External_Trigger --
   --------------------------------

   procedure Configure_External_Trigger
     (This      : in out Timer;
      Polarity  : Timer_External_Trigger_Polarity;
      Prescaler : Timer_External_Trigger_Prescaler;
      Filter    : Timer_External_Trigger_Filter)
   is
   begin
      This.SMCR.External_Trigger_Polarity := Polarity;
      This.SMCR.External_Trigger_Prescaler := Prescaler;
      This.SMCR.External_Trigger_Filter := Filter;
   end Configure_External_Trigger;

   ---------------------------------
   -- Configure_As_External_Clock --
   ---------------------------------

   procedure Configure_As_External_Clock
     (This   : in out Timer;
      Source : Timer_Internal_Trigger_Source)
   is
   begin
      Select_Input_Trigger (This, Source);
      Select_Slave_Mode (This, External_1);
   end Configure_As_External_Clock;

   ---------------------------------
   -- Configure_As_External_Clock --
   ---------------------------------

   procedure Configure_As_External_Clock
     (This     : in out Timer;
      Source   : Timer_External_Clock_Source;
      Polarity : Timer_Input_Capture_Polarity;
      Filter   : Timer_Input_Capture_Filter)
   is
   begin
      if Source = Filtered_Timer_Input_2 then
         Configure_Channel_Input
           (This,
            Channel_2,
            Polarity,
            Direct_TI,
            Div1,       --  default prescalar zero value
            Filter);
      else
         Configure_Channel_Input
           (This,
            Channel_1,
            Polarity,
            Direct_TI,
            Div1,       --  default prescalar zero value
            Filter);
      end if;
      Select_Input_Trigger (This, Source);
      Select_Slave_Mode (This, External_1);
   end Configure_As_External_Clock;

   ------------------------------------
   -- Configure_External_Clock_Mode1 --
   ------------------------------------

   procedure Configure_External_Clock_Mode1
     (This      : in out Timer;
      Polarity  : Timer_External_Trigger_Polarity;
      Prescaler : Timer_External_Trigger_Prescaler;
      Filter    : Timer_External_Trigger_Filter)
   is
   begin
      Configure_External_Trigger (This, Polarity, Prescaler, Filter);
      Select_Slave_Mode (This, External_1);
      Select_Input_Trigger (This, External_Trigger_Input);
   end Configure_External_Clock_Mode1;

   ------------------------------------
   -- Configure_External_Clock_Mode2 --
   ------------------------------------

   procedure Configure_External_Clock_Mode2
     (This      : in out Timer;
      Polarity  : Timer_External_Trigger_Polarity;
      Prescaler : Timer_External_Trigger_Prescaler;
      Filter    : Timer_External_Trigger_Filter)
   is
   begin
      Configure_External_Trigger (This, Polarity, Prescaler, Filter);
      This.SMCR.External_Clock_Enable := True;
   end Configure_External_Clock_Mode2;

   --------------------------
   -- Select_Input_Trigger --
   --------------------------

   procedure Select_Input_Trigger
     (This   : in out Timer;
      Source : Timer_Trigger_Input_Source)
   is
   begin
      This.SMCR.Trigger_Selection := Source;
   end Select_Input_Trigger;

   ------------------------------
   -- Configure_Channel_Output --
   ------------------------------

   procedure Configure_Channel_Output
     (This     : in out Timer;
      Channel  : Timer_Channel;
      Mode     : Timer_Output_Compare_And_PWM_Mode;
      State    : Timer_Capture_Compare_State;
      Pulse    : Word;
      Polarity : Timer_Output_Compare_Polarity)
   is
   begin
      --  first disable the channel
      This.CCER (Channel).CCxE := Disable;

      Set_Output_Compare_Mode (This, Channel, Mode);

      This.CCER (Channel).CCxE := State;
      This.CCER (Channel).CCxP := Polarity'Enum_Rep;

      This.CCR1_4 (Channel) := Pulse;
      --  Only timers 2 and 5 have 32-bit CCR registers. The others must
      --  maintain the upper half at zero. We use a precondition to ensure
      --  values greater than a half-word are only specified for the proper
      --  timers.
   end Configure_Channel_Output;

   ------------------------------
   -- Configure_Channel_Output --
   ------------------------------

   procedure Configure_Channel_Output
     (This                     : in out Timer;
      Channel                  : Timer_Channel;
      Mode                     : Timer_Output_Compare_And_PWM_Mode;
      State                    : Timer_Capture_Compare_State;
      Pulse                    : Word;
      Polarity                 : Timer_Output_Compare_Polarity;
      Idle_State               : Timer_Capture_Compare_State;
      Complementary_Polarity   : Timer_Output_Compare_Polarity;
      Complementary_Idle_State : Timer_Capture_Compare_State)
   is
   begin
      --  first disable the channel
      This.CCER (Channel).CCxE := Disable;

      Set_Output_Compare_Mode (This, Channel, Mode);

      This.CCER (Channel).CCxE := State;
      This.CCER (Channel).CCxNP := Complementary_Polarity'Enum_Rep;
      This.CCER (Channel).CCxP := Polarity'Enum_Rep;

      case Channel is
         when Channel_1 =>
            This.CR2.Channel_1_Output_Idle_State := Idle_State;
            This.CR2.Channel_1_Complementary_Output_Idle_State :=
              Complementary_Idle_State;
         when Channel_2 =>
            This.CR2.Channel_2_Output_Idle_State := Idle_State;
            This.CR2.Channel_2_Complementary_Output_Idle_State :=
              Complementary_Idle_State;
         when Channel_3 =>
            This.CR2.Channel_3_Output_Idle_State := Idle_State;
            This.CR2.Channel_3_Complementary_Output_Idle_State :=
              Complementary_Idle_State;
         when Channel_4 =>
            This.CR2.Channel_4_Output_Idle_State := Idle_State;
      end case;

      This.CCR1_4 (Channel) := Pulse;
      --  Only timers 2 and 5 have 32-bit CCR registers. The others must
      --  maintain the upper half at zero. We use a precondition to ensure
      --  values greater than a half-word are only specified for the proper
      --  timers.
   end Configure_Channel_Output;

   -----------------------
   -- Set_Single_Output --
   -----------------------

   procedure Set_Single_Output
     (This             : in out Timer;
      Channel          : Timer_Channel;
      Mode             : Timer_Output_Compare_And_PWM_Mode;
      OC_Clear_Enabled : Boolean;
      Preload_Enabled  : Boolean;
      Fast_Enabled     : Boolean)
   is
      CCMR_Index       : CCMRx_Index;
      Descriptor_Index : Lower_Half_Index;
      Temp             : TIMx_CCMRx;
      Description      : Channel_Output_Descriptor;
   begin
      case Channel is
         when Channel_1 =>
            CCMR_Index := 1;
            Descriptor_Index := 1;
         when Channel_2 =>
            CCMR_Index := 1;
            Descriptor_Index := 2;
         when Channel_3 =>
            CCMR_Index := 2;
            Descriptor_Index := 1;
         when Channel_4 =>
            CCMR_Index := 2;
            Descriptor_Index := 2;
      end case;
      Temp := This.CCMR1_2 (CCMR_Index);  -- effectively get CCMR1 or CCMR2

      Description := (OCxMode  => Mode,
                      OCxFast_Enable => Fast_Enabled,
                      OCxPreload_Enable => Preload_Enabled,
                      OCxClear_Enable => OC_Clear_Enabled);

      Temp.Descriptors (Descriptor_Index) := (Output, Description);

      This.CCMR1_2 (CCMR_Index) := Temp;
   end Set_Single_Output;

   -----------------------------
   -- Set_Output_Compare_Mode --
   -----------------------------

   procedure Set_Output_Compare_Mode
     (This    : in out Timer;
      Channel : Timer_Channel;
      Mode    : Timer_Output_Compare_And_PWM_Mode)
   is
      CCMR_Index       : CCMRx_Index;
      Descriptor_Index : Lower_Half_Index;
      Temp             : TIMx_CCMRx;
   begin
      case Channel is
         when Channel_1 =>
            CCMR_Index := 1;
            Descriptor_Index := 1;
         when Channel_2 =>
            CCMR_Index := 1;
            Descriptor_Index := 2;
         when Channel_3 =>
            CCMR_Index := 2;
            Descriptor_Index := 1;
         when Channel_4 =>
            CCMR_Index := 2;
            Descriptor_Index := 2;
      end case;
      Temp := This.CCMR1_2 (CCMR_Index);  -- effectively get CCMR1 or CCMR2

      if Temp.Descriptors (Descriptor_Index).CCxSelection /= Output then
         raise Timer_Channel_Access_Error;
      end if;

      Temp.Descriptors (Descriptor_Index).Compare.OCxMode := Mode;
      This.CCMR1_2 (CCMR_Index) := Temp;
   end Set_Output_Compare_Mode;

   ----------------------------------
   -- Current_Capture_Compare_Mode --
   ----------------------------------

   function Current_Capture_Compare_Mode
     (This    : Timer;
      Channel : Timer_Channel)
      return Timer_Capture_Compare_Modes
   is
      CCMR_Index       : CCMRx_Index;
      Descriptor_Index : Lower_Half_Index;
      Temp             : TIMx_CCMRx;
   begin
      case Channel is
         when Channel_1 =>
            CCMR_Index := 1;
            Descriptor_Index := 1;
         when Channel_2 =>
            CCMR_Index := 1;
            Descriptor_Index := 2;
         when Channel_3 =>
            CCMR_Index := 2;
            Descriptor_Index := 1;
         when Channel_4 =>
            CCMR_Index := 2;
            Descriptor_Index := 2;
      end case;
      Temp := This.CCMR1_2 (CCMR_Index);  -- effectively get CCMR1 or CCMR2

      return Temp.Descriptors (Descriptor_Index).CCxSelection;
   end Current_Capture_Compare_Mode;

   ------------------------------
   -- Set_Output_Forced_Action --
   ------------------------------

   procedure Set_Output_Forced_Action
     (This    : in out Timer;
      Channel : Timer_Channel;
      Active  : Boolean)
   is
      CCMR_Index       : CCMRx_Index;
      Descriptor_Index : Lower_Half_Index;
      Temp             : TIMx_CCMRx;
   begin
      case Channel is
         when Channel_1 =>
            CCMR_Index := 1;
            Descriptor_Index := 1;
         when Channel_2 =>
            CCMR_Index := 1;
            Descriptor_Index := 2;
         when Channel_3 =>
            CCMR_Index := 2;
            Descriptor_Index := 1;
         when Channel_4 =>
            CCMR_Index := 2;
            Descriptor_Index := 2;
      end case;
      Temp := This.CCMR1_2 (CCMR_Index);  -- effectively get CCMR1 or CCMR2

      if Temp.Descriptors (Descriptor_Index).CCxSelection /= Output then
         raise Timer_Channel_Access_Error;
      end if;

      if Active then
         Temp.Descriptors (Descriptor_Index).Compare.OCxMode := Force_Active;
      else
         Temp.Descriptors (Descriptor_Index).Compare.OCxMode := Force_Inactive;
      end if;

      This.CCMR1_2 (CCMR_Index) := Temp;
   end Set_Output_Forced_Action;

   -------------------------------
   -- Set_Output_Preload_Enable --
   -------------------------------

   procedure Set_Output_Preload_Enable
     (This    : in out Timer;
      Channel : Timer_Channel;
      Enabled : Boolean)
   is
      CCMR_Index       : CCMRx_Index;
      Descriptor_Index : Lower_Half_Index;
      Temp             : TIMx_CCMRx;
   begin
      case Channel is
         when Channel_1 =>
            CCMR_Index := 1;
            Descriptor_Index := 1;
         when Channel_2 =>
            CCMR_Index := 1;
            Descriptor_Index := 2;
         when Channel_3 =>
            CCMR_Index := 2;
            Descriptor_Index := 1;
         when Channel_4 =>
            CCMR_Index := 2;
            Descriptor_Index := 2;
      end case;
      Temp := This.CCMR1_2 (CCMR_Index);  -- effectively get CCMR1 or CCMR2

      Temp.Descriptors (Descriptor_Index).Compare.OCxPreload_Enable := Enabled;
      This.CCMR1_2 (CCMR_Index) := Temp;
   end Set_Output_Preload_Enable;

   ----------------------------
   -- Set_Output_Fast_Enable --
   ----------------------------

   procedure Set_Output_Fast_Enable
     (This    : in out Timer;
      Channel : Timer_Channel;
      Enabled : Boolean)
   is
      CCMR_Index       : CCMRx_Index;
      Descriptor_Index : Lower_Half_Index;
      Temp             : TIMx_CCMRx;
   begin
      case Channel is
         when Channel_1 =>
            CCMR_Index := 1;
            Descriptor_Index := 1;
         when Channel_2 =>
            CCMR_Index := 1;
            Descriptor_Index := 2;
         when Channel_3 =>
            CCMR_Index := 2;
            Descriptor_Index := 1;
         when Channel_4 =>
            CCMR_Index := 2;
            Descriptor_Index := 2;
      end case;
      Temp := This.CCMR1_2 (CCMR_Index);  -- effectively get CCMR1 or CCMR2

      Temp.Descriptors (Descriptor_Index).Compare.OCxFast_Enable := Enabled;
      This.CCMR1_2 (CCMR_Index) := Temp;
   end Set_Output_Fast_Enable;

   -----------------------
   -- Set_Clear_Control --
   -----------------------

   procedure Set_Clear_Control
     (This    : in out Timer;
      Channel : Timer_Channel;
      Enabled : Boolean)
   is
      CCMR_Index       : CCMRx_Index;
      Descriptor_Index : Lower_Half_Index;
      Temp             : TIMx_CCMRx;
   begin
      case Channel is
         when Channel_1 =>
            CCMR_Index := 1;
            Descriptor_Index := 1;
         when Channel_2 =>
            CCMR_Index := 1;
            Descriptor_Index := 2;
         when Channel_3 =>
            CCMR_Index := 2;
            Descriptor_Index := 1;
         when Channel_4 =>
            CCMR_Index := 2;
            Descriptor_Index := 2;
      end case;
      Temp := This.CCMR1_2 (CCMR_Index);  -- effectively get CCMR1 or CCMR2

      Temp.Descriptors (Descriptor_Index).Compare.OCxClear_Enable := Enabled;
      This.CCMR1_2 (CCMR_Index) := Temp;
   end Set_Clear_Control;

   --------------------
   -- Enable_Channel --
   --------------------

   procedure Enable_Channel
     (This    : in out Timer;
      Channel : Timer_Channel)
   is
      Temp_EGR  : Word := This.EGR;
   begin
      This.CCER (Channel).CCxE := Enable;

      --  Trigger an event to initialize preload register
      Temp_EGR := Temp_EGR or (2 ** (Timer_Channel'Pos (Channel) + 1));

      This.EGR  := Temp_EGR;
   end Enable_Channel;

   -------------------------
   -- Set_Output_Polarity --
   -------------------------

   procedure Set_Output_Polarity
     (This     : in out Timer;
      Channel  : Timer_Channel;
      Polarity : Timer_Output_Compare_Polarity)
   is
   begin
      This.CCER (Channel).CCxP := Polarity'Enum_Rep;
   end Set_Output_Polarity;

   ---------------------------------------
   -- Set_Output_Complementary_Polarity --
   ---------------------------------------

   procedure Set_Output_Complementary_Polarity
     (This     : in out Timer;
      Channel  : Timer_Channel;
      Polarity : Timer_Output_Compare_Polarity)
   is
   begin
      This.CCER (Channel).CCxNP := Polarity'Enum_Rep;
   end Set_Output_Complementary_Polarity;

   ---------------------
   -- Disable_Channel --
   ---------------------

   procedure Disable_Channel
     (This    : in out Timer;
      Channel : Timer_Channel)
   is
   begin
      This.CCER (Channel).CCxE := Disable;
   end Disable_Channel;

   ---------------------
   -- Channel_Enabled --
   ---------------------

   function Channel_Enabled
     (This    : Timer;
      Channel : Timer_Channel)
      return Boolean
   is
   begin
      return This.CCER (Channel).CCxE = Enable;
   end Channel_Enabled;

   ----------------------------------
   -- Enable_Complementary_Channel --
   ----------------------------------

   procedure Enable_Complementary_Channel
     (This    : in out Timer;
      Channel : Timer_Channel)
   is
   begin
      This.CCER (Channel).CCxNE := Enable;
   end Enable_Complementary_Channel;

   -----------------------------------
   -- Disable_Complementary_Channel --
   -----------------------------------

   procedure Disable_Complementary_Channel
     (This    : in out Timer;
      Channel : Timer_Channel)
   is
   begin
      This.CCER (Channel).CCxNE := Disable;
   end Disable_Complementary_Channel;

   -----------------------------------
   -- Complementary_Channel_Enabled --
   -----------------------------------

   function Complementary_Channel_Enabled
     (This : Timer;  Channel : Timer_Channel)
      return Boolean
   is
   begin
      return This.CCER (Channel).CCxNE = Enable;
   end Complementary_Channel_Enabled;

   -----------------------
   -- Set_Compare_Value --
   -----------------------

   procedure Set_Compare_Value
     (This       : in out Timer;
      Channel    : Timer_Channel;
      Word_Value : Word)
   is
   begin
      This.CCR1_4 (Channel) := Word_Value;
      --  Timers 2 and 5 really do have 32-bit capture/compare registers so we
      --  don't need to require half-words as inputs.
   end Set_Compare_Value;

   -----------------------
   -- Set_Compare_Value --
   -----------------------

   procedure Set_Compare_Value
     (This    : in out Timer;
      Channel : Timer_Channel;
      Value   : Short)
   is
   begin
      This.CCR1_4 (Channel) := Word (Value);
      --  These capture/compare registers are really only 15-bits wide, except
      --  for those of timers 2 and 5. For the sake of simplicity we represent
      --  all of them with full words, but only write word values when
      --  appropriate. The caller has to treat them as half-word values, since
      --  that's the type for the formal parameter, therefore our casting up to
      --  a word value will retain the reserved upper half-word value of zero.
   end Set_Compare_Value;

   ---------------------------
   -- Current_Capture_Value --
   ---------------------------

   function Current_Capture_Value
     (This    : Timer;
      Channel : Timer_Channel)
      return Word
   is
   begin
      return This.CCR1_4 (Channel);
   end Current_Capture_Value;

   ---------------------------
   -- Current_Capture_Value --
   ---------------------------

   function Current_Capture_Value
     (This    : Timer;
      Channel : Timer_Channel)
      return Short
   is
   begin
      return Short (This.CCR1_4 (Channel));
   end Current_Capture_Value;

   -------------------------------------
   -- Write_Channel_Input_Description --
   -------------------------------------

   procedure Write_Channel_Input_Description
     (This        : in out Timer;
      Channel     : Timer_Channel;
      Kind        : Timer_Input_Capture_Selection;
      Description : Channel_Input_Descriptor)
   is
      CCMR_Index       : CCMRx_Index;
      Descriptor_Index : Lower_Half_Index;
      Temp             : TIMx_CCMRx;
      New_Value        : IO_Descriptor;
   begin
      case Kind is
         when Direct_TI =>
            New_Value := (CCxSelection => Direct_TI, Capture => Description);
         when Indirect_TI =>
            New_Value := (CCxSelection => Indirect_TI, Capture => Description);
         when TRC =>
            New_Value := (CCxSelection => TRC, Capture => Description);
      end case;

      case Channel is
         when Channel_1 =>
            CCMR_Index := 1;
            Descriptor_Index := 1;
         when Channel_2 =>
            CCMR_Index := 1;
            Descriptor_Index := 2;
         when Channel_3 =>
            CCMR_Index := 2;
            Descriptor_Index := 1;
         when Channel_4 =>
            CCMR_Index := 2;
            Descriptor_Index := 2;
      end case;

      Temp := This.CCMR1_2 (CCMR_Index);  -- effectively get CCMR1 or CCMR2
      Temp.Descriptors (Descriptor_Index) := New_Value;
      This.CCMR1_2 (CCMR_Index) := Temp;
   end Write_Channel_Input_Description;

   -------------------------
   -- Set_Input_Prescaler --
   -------------------------

   procedure Set_Input_Prescaler
     (This    : in out Timer;
      Channel : Timer_Channel;
      Value   : Timer_Input_Capture_Prescaler)
   is
      CCMR_Index       : CCMRx_Index;
      Descriptor_Index : Lower_Half_Index;
      Temp             : TIMx_CCMRx;
   begin
      case Channel is
         when Channel_1 =>
            CCMR_Index := 1;
            Descriptor_Index := 1;
         when Channel_2 =>
            CCMR_Index := 1;
            Descriptor_Index := 2;
         when Channel_3 =>
            CCMR_Index := 2;
            Descriptor_Index := 1;
         when Channel_4 =>
            CCMR_Index := 2;
            Descriptor_Index := 2;
      end case;
      Temp := This.CCMR1_2 (CCMR_Index);  -- effectively get CCMR1 or CCMR2

      Temp.Descriptors (Descriptor_Index).Capture.ICxPrescaler := Value;
      This.CCMR1_2 (CCMR_Index) := Temp;
   end Set_Input_Prescaler;

   -----------------------------
   -- Current_Input_Prescaler --
   -----------------------------

   function Current_Input_Prescaler
     (This    : Timer;
      Channel : Timer_Channel)
      return Timer_Input_Capture_Prescaler
   is
      CCMR_Index       : CCMRx_Index;
      Descriptor_Index : Lower_Half_Index;
      Temp             : TIMx_CCMRx;
   begin
      case Channel is
         when Channel_1 =>
            CCMR_Index := 1;
            Descriptor_Index := 1;
         when Channel_2 =>
            CCMR_Index := 1;
            Descriptor_Index := 2;
         when Channel_3 =>
            CCMR_Index := 2;
            Descriptor_Index := 1;
         when Channel_4 =>
            CCMR_Index := 2;
            Descriptor_Index := 2;
      end case;
      Temp := This.CCMR1_2 (CCMR_Index);  -- effectively get CCMR1 or CCMR2

      return Temp.Descriptors (Descriptor_Index).Capture.ICxPrescaler;
   end Current_Input_Prescaler;

   -----------------------------
   -- Configure_Channel_Input --
   -----------------------------

   procedure Configure_Channel_Input
     (This      : in out Timer;
      Channel   : Timer_Channel;
      Polarity  : Timer_Input_Capture_Polarity;
      Selection : Timer_Input_Capture_Selection;
      Prescaler : Timer_Input_Capture_Prescaler;
      Filter    : Timer_Input_Capture_Filter)
   is
      Input     : Channel_Input_Descriptor;
   begin
      --  first disable the channel
      This.CCER (Channel).CCxE := Disable;

      Input := (ICxFilter => Filter, ICxPrescaler => Prescaler);
      Write_Channel_Input_Description
        (This          => This,
         Channel     => Channel,
         Kind        => Selection,
         Description => Input);

      case Polarity is
         when Rising =>
            This.CCER (Channel).CCxNP := 0;
            This.CCER (Channel).CCxP := 0;
         when Falling =>
            This.CCER (Channel).CCxNP := 0;
            This.CCER (Channel).CCxP := 1;
         when Both_Edges =>
            This.CCER (Channel).CCxNP := 1;
            This.CCER (Channel).CCxP := 1;
      end case;

      This.CCER (Channel).CCxE := Enable;
   end Configure_Channel_Input;

   ---------------------------------
   -- Configure_Channel_Input_PWM --
   ---------------------------------

   procedure Configure_Channel_Input_PWM
     (This      : in out Timer;
      Channel   : Timer_Channel;
      Selection : Timer_Input_Capture_Selection;
      Polarity  : Timer_Input_Capture_Polarity;
      Prescaler : Timer_Input_Capture_Prescaler;
      Filter    : Timer_Input_Capture_Filter)
   is
      Opposite_Polarity  : Timer_Input_Capture_Polarity;
      Opposite_Selection : Timer_Input_Capture_Selection;
   begin
      Disable_Channel (This, Channel);

      if Polarity = Rising then
         Opposite_Polarity := Falling;
      else
         Opposite_Polarity := Rising;
      end if;

      if Selection = Indirect_TI then
         Opposite_Selection := Direct_TI;
      else
         Opposite_Selection := Indirect_TI;
      end if;

      if Channel = Channel_1 then
         Configure_Channel_Input
           (This, Channel_1, Polarity, Selection, Prescaler, Filter);

         Configure_Channel_Input (This,
                                  Channel_2,
                                  Opposite_Polarity,
                                  Opposite_Selection,
                                  Prescaler,
                                  Filter);
      else
         Configure_Channel_Input
           (This, Channel_2, Polarity, Selection, Prescaler, Filter);

         Configure_Channel_Input (This,
                                  Channel_1,
                                  Opposite_Polarity,
                                  Opposite_Selection,
                                  Prescaler,
                                  Filter);
      end if;

      Enable_Channel (This, Channel);
   end Configure_Channel_Input_PWM;

   -------------------------------
   -- Enable_CC_Preload_Control --
   -------------------------------

   procedure Enable_CC_Preload_Control (This : in out Timer) is
   begin
      This.CR2.Capture_Compare_Preloaded_Control := True;
   end Enable_CC_Preload_Control;

   --------------------------------
   -- Disable_CC_Preload_Control --
   --------------------------------

   procedure Disable_CC_Preload_Control (This : in out Timer) is
   begin
      This.CR2.Capture_Compare_Preloaded_Control := False;
   end Disable_CC_Preload_Control;

   ------------------------
   -- Select_Commutation --
   ------------------------

   procedure Select_Commutation (This : in out Timer) is
   begin
      This.CR2.Capture_Compare_Control_Update_Selection := True;
   end Select_Commutation;

   --------------------------
   -- Deselect_Commutation --
   --------------------------

   procedure Deselect_Commutation (This : in out Timer) is
   begin
      This.CR2.Capture_Compare_Control_Update_Selection := False;
   end Deselect_Commutation;

   --------------------
   -- Configure_BDTR --
   --------------------

   procedure Configure_BDTR
     (This                          : in out Timer;
      Automatic_Output_Enabled      : Boolean;
      Break_Polarity                : Timer_Break_Polarity;
      Break_Enabled                 : Boolean;
      Off_State_Selection_Run_Mode  : Bit;
      Off_State_Selection_Idle_Mode : Bit;
      Lock_Configuration            : Timer_Lock_Level;
      Deadtime_Generator            : Byte)
   is
   begin
      This.BDTR.Automatic_Output_Enabled      := Automatic_Output_Enabled;
      This.BDTR.Break_Polarity                := Break_Polarity;
      This.BDTR.Break_Enable                  := Break_Enabled;
      This.BDTR.Off_State_Selection_Run_Mode  := Off_State_Selection_Run_Mode;
      This.BDTR.Off_State_Selection_Idle_Mode := Off_State_Selection_Idle_Mode;
      This.BDTR.Lock                          := Lock_Configuration;
      This.BDTR.Deadtime_Generator            := Deadtime_Generator;
   end Configure_BDTR;

   ---------------------------------
   -- Configure_Timer_2_Remapping --
   ---------------------------------

   procedure Configure_Timer_2_Remapping
     (This   : in out Timer;
      Option : Timer_2_Remapping_Options)
   is
   begin
      This.Options.ITR1_RMP := Option;
   end Configure_Timer_2_Remapping;

   ---------------------------------
   -- Configure_Timer_5_Remapping --
   ---------------------------------

   procedure Configure_Timer_5_Remapping
     (This   : in out Timer;
      Option : Timer_5_Remapping_Options)
   is
   begin
      This.Options.TI4_RMP := Option;
   end Configure_Timer_5_Remapping;

   ----------------------------------
   -- Configure_Timer_11_Remapping --
   ----------------------------------

   procedure Configure_Timer_11_Remapping
     (This   : in out Timer;
      Option : Timer_11_Remapping_Options)
   is
   begin
      This.Options.TI1_RMP := Option;
   end Configure_Timer_11_Remapping;

   ---------------------------------
   -- Configure_Encoder_Interface --
   ---------------------------------

   procedure Configure_Encoder_Interface
     (This         : in out Timer;
      Mode         : Timer_Encoder_Mode;
      IC1_Polarity : Timer_Input_Capture_Polarity;
      IC2_Polarity : Timer_Input_Capture_Polarity)
   is
   begin
      This.SMCR.Slave_Mode_Selection := Mode;

      Write_Channel_Input_Description
        (This,
         Channel     => Channel_1,
         Kind        => Direct_TI,
         Description => Channel_Input_Descriptor'(ICxFilter => 0,
                                                  ICxPrescaler => Div1));

      Write_Channel_Input_Description
        (This,
         Channel     => Channel_2,
         Kind        => Direct_TI,
         Description => Channel_Input_Descriptor'(ICxFilter => 0,
                                                  ICxPrescaler => Div1));

      case IC1_Polarity is
         when Rising =>
            This.CCER (Channel_1).CCxNP := 0;
            This.CCER (Channel_1).CCxP := 0;
         when Falling =>
            This.CCER (Channel_1).CCxNP := 0;
            This.CCER (Channel_1).CCxP := 1;
         when Both_Edges =>
            This.CCER (Channel_1).CCxNP := 1;
            This.CCER (Channel_1).CCxP := 1;
      end case;

      case IC2_Polarity is
         when Rising =>
            This.CCER (Channel_2).CCxNP := 0;
            This.CCER (Channel_2).CCxP := 0;
         when Falling =>
            This.CCER (Channel_2).CCxNP := 0;
            This.CCER (Channel_2).CCxP := 1;
         when Both_Edges =>
            This.CCER (Channel_2).CCxNP := 1;
            This.CCER (Channel_2).CCxP := 1;
      end case;
   end Configure_Encoder_Interface;

   ------------------------
   -- Enable_Hall_Sensor --
   ------------------------

   procedure Enable_Hall_Sensor
     (This : in out Timer)
   is
   begin
      This.CR2.TI1_Selection := True;
   end Enable_Hall_Sensor;

   -------------------------
   -- Disable_Hall_Sensor --
   -------------------------

   procedure Disable_Hall_Sensor
     (This : in out Timer)
   is
   begin
      This.CR2.TI1_Selection := False;
   end Disable_Hall_Sensor;

end STM32.Timers;
