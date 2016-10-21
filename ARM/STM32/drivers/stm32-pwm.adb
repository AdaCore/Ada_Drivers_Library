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
------------------------------------------------------------------------------

with System;        use System;
with STM32_SVD;     use STM32_SVD;

with STM32.Device;  use STM32.Device;

package body STM32.PWM is

   subtype Hertz is UInt32;

   procedure Compute_Prescalar_and_Period
     (This                : access Timer;
      Requested_Frequency : Hertz;
      Prescalar           : out UInt32;
      Period              : out UInt32)
     with Pre => Requested_Frequency > 0;
   --  Computes the minimum prescaler and thus the maximum resolution for the
   --  given timer, based on the system clocks and the requested frequency

   function Has_APB2_Frequency  (This : Timer) return Boolean;
   --  timers 1, 8, 9, 10, 11

   function Has_APB1_Frequency (This : Timer) return Boolean;
   --  timers 3, 4, 6, 7, 12, 13, 14

   procedure Configure_PWM_GPIO
     (Output : GPIO_Point;
      PWM_AF : GPIO_Alternate_Function);

   --------------------
   -- Set_Duty_Cycle --
   --------------------

   procedure Set_Duty_Cycle
     (This  : in out PWM_Modulator;
      Value : Percentage)
   is
      Pulse : UInt16;
   begin
      This.Timer.Outputs (This.Channel).Duty_Cycle := Value;

      if Value = 0 then
         Set_Compare_Value (This.Timer.Output_Timer.all,
                            This.Channel,
                            UInt16'(0));
      else
         Pulse :=
           UInt16 ((This.Timer.Timer_Period + 1) * UInt32 (Value) / 100) - 1;
         --  for a Value of 0, the computation of Pulse wraps around to
         --  65535, so we only compute it when not zero
         Set_Compare_Value (This.Timer.Output_Timer.all,
                            This.Channel,
                            Pulse);
      end if;
   end Set_Duty_Cycle;

   -------------------
   -- Set_Duty_Time --
   -------------------

   procedure Set_Duty_Time
     (This  : in out PWM_Modulator;
      Value : Microseconds)
   is
      Pulse         : UInt16;
      Period        : constant UInt32 := This.Timer.Timer_Period + 1;
      uS_Per_Period : constant UInt32 := 1_000_000 / This.Timer.Frequency;
   begin
      if Value > uS_Per_Period then
         raise Invalid_Request with "duty time too high";
      end if;

      Pulse := UInt16 ((Period * Value) / uS_Per_Period) - 1;
      Set_Compare_Value (This.Timer.Output_Timer.all, This.Channel, Pulse);
   end Set_Duty_Time;

   ------------------------
   -- Current_Duty_Cycle --
   ------------------------

   function Current_Duty_Cycle
     (This : PWM_Modulator) return Percentage
   is
   begin
      return This.Timer.Outputs (This.Channel).Duty_Cycle;
   end Current_Duty_Cycle;

   --------------------------
   -- Initialise_PWM_Timer --
   --------------------------

   procedure Initialise_PWM_Timer
     (This                : in out PWM_Timer;
      Requested_Frequency : Float)
   is
      Prescalar : UInt32;
   begin
      Power_Up (This.Output_Timer.all);

      Compute_Prescalar_and_Period
        (This.Output_Timer,
         Requested_Frequency => UInt32 (Requested_Frequency),
         Prescalar           => Prescalar,
         Period              => This.Timer_Period);

      This.Timer_Period := This.Timer_Period - 1;
      This.Frequency    := UInt32 (Requested_Frequency);

      Configure
        (This.Output_Timer.all,
         Prescaler     => UInt16 (Prescalar),
         Period        => This.Timer_Period,
         Clock_Divisor => Div1,
         Counter_Mode  => Up);

      Set_Autoreload_Preload (This.Output_Timer.all, True);

      if Advanced_Timer (This.Output_Timer.all) then
         Enable_Main_Output (This.Output_Timer.all);
      end if;

      Enable (This.Output_Timer.all);

      for Channel in Timer_Channel loop
         This.Outputs (Channel).Attached := False;
      end loop;
   end Initialise_PWM_Timer;

   ------------------------
   -- Attach_PWM_Channel --
   ------------------------

   procedure Attach_PWM_Channel
     (This      : access PWM_Timer;
      Modulator : in out PWM_Modulator;
      Channel   : Timer_Channel;
      Point     : GPIO_Point;
      PWM_AF    : GPIO_Alternate_Function)
   is
   begin
      This.Outputs (Channel).Attached := True;
      Modulator.Timer   := This;
      Modulator.Channel := Channel;

      Power_Up (Point);

      Configure_PWM_GPIO (Point, PWM_AF);

      Configure_Channel_Output
        (This.Output_Timer.all,
         Channel  => Channel,
         Mode     => PWM1,
         State    => Disable,
         Pulse    => 0,
         Polarity => High);

      Set_Compare_Value (This.Output_Timer.all, Channel, UInt16 (0));

      Disable_Channel (This.Output_Timer.all, Channel);
   end Attach_PWM_Channel;

   ----------------
   -- Enable_PWM --
   ----------------

   procedure Enable_PWM
     (This    : in out PWM_Modulator)
   is
   begin
      Enable_Channel (This.Timer.Output_Timer.all, This.Channel);
   end Enable_PWM;

   -------------
   -- Enabled --
   -------------

   function Enabled
     (This : PWM_Modulator)
      return Boolean
   is
   begin
      return Channel_Enabled (This.Timer.Output_Timer.all, This.Channel);
   end Enabled;

   -------------------------
   -- Disable_PWM_Channel --
   -------------------------

   procedure Disable_PWM
     (This : in out PWM_Modulator)
   is
   begin
      Disable_Channel (This.Timer.Output_Timer.all, This.Channel);
   end Disable_PWM;

   --------------
   -- Attached --
   --------------

   function Attached
     (This : PWM_Modulator)
      return Boolean
   is
     (This.Timer /= null and then This.Timer.Outputs (This.Channel).Attached);

   ------------------------
   -- Configure_PWM_GPIO --
   ------------------------

   procedure Configure_PWM_GPIO
     (Output : GPIO_Point;
      PWM_AF : GPIO_Alternate_Function)
   is
      Configuration : GPIO_Port_Configuration;
   begin
      Configuration.Mode        := Mode_AF;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_100MHz;
      Configuration.Resistors   := Floating;

      Output.Configure_IO (Configuration);

      Output.Configure_Alternate_Function (PWM_AF);

      Output.Lock;
   end Configure_PWM_GPIO;

   ----------------------------------
   -- Compute_Prescalar_and_Period --
   ----------------------------------

   procedure Compute_Prescalar_and_Period
     (This                : access Timer;
      Requested_Frequency : Hertz;
      Prescalar           : out UInt32;
      Period              : out UInt32)
   is
      Max_Prescalar      : constant := 16#FFFF#;
      Max_Period         : UInt32;
      Hardware_Frequency : UInt32;
      Clocks             : constant RCC_System_Clocks :=
                             System_Clock_Frequencies;
      CK_CNT             : UInt32;

   begin
      if Has_APB1_Frequency (This.all) then
         Hardware_Frequency := Clocks.TIMCLK1;
      elsif Has_APB2_Frequency (This.all) then
         Hardware_Frequency := Clocks.TIMCLK2;
      else
         raise Unknown_Timer;
      end if;

      if Has_32bit_Counter (This.all) then
         Max_Period := 16#FFFF_FFFF#;
      else
         Max_Period := 16#FFFF#;
      end if;

      if Requested_Frequency > Hardware_Frequency then
         raise Invalid_Request with "Freq too high";
      end if;

      Prescalar := 0;
      loop
         --  Compute the Counter's clock
         CK_CNT := Hardware_Frequency / (Prescalar + 1);
         --  Determine the CK_CNT periods to achieve the requested frequency
         Period := CK_CNT / Requested_Frequency;

         exit when not
           ((Period > Max_Period) and
            (Prescalar <= Max_Prescalar));

         Prescalar := Prescalar + 1;
      end loop;

      if Prescalar > Max_Prescalar then
         raise Invalid_Request with "Freq too low";
      end if;
   end Compute_Prescalar_and_Period;

   ------------------------
   -- Has_APB2_Frequency --
   ------------------------

   function Has_APB2_Frequency (This : Timer) return Boolean is
     (This'Address = STM32_SVD.TIM1_Base or
      This'Address = STM32_SVD.TIM8_Base or
      This'Address = STM32_SVD.TIM9_Base or
      This'Address = STM32_SVD.TIM10_Base or
      This'Address = STM32_SVD.TIM11_Base);

   ------------------------
   -- Has_APB1_Frequency --
   ------------------------

   function Has_APB1_Frequency (This : Timer) return Boolean is
     (This'Address = STM32_SVD.TIM2_Base or
      This'Address = STM32_SVD.TIM3_Base or
      This'Address = STM32_SVD.TIM4_Base or
      This'Address = STM32_SVD.TIM5_Base or
      This'Address = STM32_SVD.TIM6_Base or
      This'Address = STM32_SVD.TIM7_Base or
      This'Address = STM32_SVD.TIM12_Base or
      This'Address = STM32_SVD.TIM13_Base or
      This'Address = STM32_SVD.TIM14_Base);

end STM32.PWM;
