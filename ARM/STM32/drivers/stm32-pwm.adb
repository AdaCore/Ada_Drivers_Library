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

   subtype Hertz is Word;

   procedure Compute_Prescalar_and_Period
     (This                : access Timer;
      Requested_Frequency : Hertz;
      Prescalar           : out Word;
      Period              : out Word)
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
     (This    : in out PWM_Modulator;
      Channel : Timer_Channel;
      Value   : Percentage)
   is
      Pulse : Short;
   begin
      This.Outputs (Channel).Duty_Cycle := Value;
      if Value = 0 then
         Set_Compare_Value (This.Output_Timer.all, Channel, Short'(0));
      else
         Pulse := Short ((This.Timer_Period + 1) * Word (Value) / 100) - 1;
         --  for a Value of 0, the computation of Pulse wraps around to
         --  65535, so we only compute it when not zero
         Set_Compare_Value (This.Output_Timer.all, Channel, Pulse);
      end if;
   end Set_Duty_Cycle;

   -------------------
   -- Set_Duty_Time --
   -------------------

   procedure Set_Duty_Time
     (This    : in out PWM_Modulator;
      Channel : Timer_Channel;
      Value   : Microseconds)
   is
      Pulse         : Short;
      Period        : constant Word := This.Timer_Period + 1;
      uS_Per_Period : constant Word := 1_000_000 / This.Frequency;
   begin
      if Value > uS_Per_Period then
         raise Invalid_Request with "duty time too high";
      end if;
      Pulse := Short ((Period * Value) / uS_Per_Period) - 1;
      Set_Compare_Value (This.Output_Timer.all, Channel, Pulse);
   end Set_Duty_Time;

   ------------------------
   -- Current_Duty_Cycle --
   ------------------------

   function Current_Duty_Cycle
     (This    : PWM_Modulator;
      Channel : Timer_Channel)
      return Percentage
   is
   begin
      return This.Outputs (Channel).Duty_Cycle;
   end Current_Duty_Cycle;

   ------------------------------
   -- Initialise_PWM_Modulator --
   ------------------------------

   procedure Initialise_PWM_Modulator
     (This                   : in out PWM_Modulator;
      Requested_Frequency    : Float;
      PWM_Timer              : not null access Timer;
      PWM_AF                 : GPIO_Alternate_Function)
   is
      Prescalar : Word;
   begin
      This.Output_Timer := PWM_Timer;
      This.AF := PWM_AF;

      Enable_Clock (PWM_Timer.all);

      Compute_Prescalar_and_Period
        (PWM_Timer,
         Requested_Frequency => Word (Requested_Frequency),
         Prescalar           => Prescalar,
         Period              => This.Timer_Period);

      This.Timer_Period := This.Timer_Period - 1;
      This.Frequency := Word (Requested_Frequency);

      Configure
        (PWM_Timer.all,
         Prescaler     => Short (Prescalar),
         Period        => This.Timer_Period,
         Clock_Divisor => Div1,
         Counter_Mode  => Up);

      Set_Autoreload_Preload (PWM_Timer.all, True);

      if Advanced_Timer (This.Output_Timer.all) then
         Enable_Main_Output (This.Output_Timer.all);
      end if;

      Enable (PWM_Timer.all);

      for Channel in Timer_Channel loop
         This.Outputs (Channel).Attached := False;
      end loop;
   end Initialise_PWM_Modulator;

   ------------------------
   -- Attach_PWM_Channel --
   ------------------------

   procedure Attach_PWM_Channel
     (This    : in out PWM_Modulator;
      Channel : Timer_Channel;
      Point   : GPIO_Point)
   is
   begin
      This.Outputs (Channel).Attached := True;

      Enable_Clock (Point);

      Configure_PWM_GPIO (Point, This.AF);

      Configure_Channel_Output
        (This.Output_Timer.all,
         Channel  => Channel,
         Mode     => PWM1,
         State    => Disable,
         Pulse    => 0,
         Polarity => High);

      Set_Compare_Value (This.Output_Timer.all, Channel, Short (0));

   end Attach_PWM_Channel;

   procedure Enable_PWM_Channel
     (This    : in out PWM_Modulator;
      Channel : Timer_Channel)
   is
   begin
      Enable_Channel (This.Output_Timer.all, Channel);
   end Enable_PWM_Channel;

   procedure Disable_PWM_Channel
     (This    : in out PWM_Modulator;
      Channel : Timer_Channel)
   is
   begin
      Disable_Channel (This.Output_Timer.all, Channel);
   end Disable_PWM_Channel;


   --------------
   -- Attached --
   --------------

   function Attached (This : PWM_Modulator;
                      Channel : Timer_Channel)
                      return Boolean
   is (This.Outputs (Channel).Attached);

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
      Prescalar           : out Word;
      Period              : out Word)
   is
      Max_Prescalar      : constant := 16#FFFF#;
      Max_Period         : Word;
      Hardware_Frequency : Word;
      Clocks             : constant RCC_System_Clocks :=
        System_Clock_Frequencies;
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
         Period := Hardware_Frequency / (Prescalar + 1);
         Period := Period / Requested_Frequency;

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
     (This'Address = STM32_SVD.TIM3_Base or
      This'Address = STM32_SVD.TIM4_Base or
      This'Address = STM32_SVD.TIM6_Base or
      This'Address = STM32_SVD.TIM7_Base or
      This'Address = STM32_SVD.TIM12_Base or
      This'Address = STM32_SVD.TIM13_Base or
      This'Address = STM32_SVD.TIM14_Base);

end STM32.PWM;
