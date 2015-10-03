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

--  This program demonstrates use of the LIS3DSH accelerometer and a timer to
--  drive the brightness of the LEDs.

--  Note that this demonstration program is specific to the STM32F4 Discovery
--  boards because it references the specific accelerometer used on (later
--  versions of) those boards and because it references the four user LEDs
--  on those boards. (The LIS3DSH accelerometer is used on board versions
--  designated by the number MB997C printed on the top of the board.)
--
--  The idea is that the person holding the board will "pitch" it up and down
--  and "roll" it left and right around the Z axis running through the center
--  of the chip. As the board is moved, the brightness of the four LEDs
--  surrounding the accelerometer will vary with the accelerations experienced
--  by the board. In particular, as the angles increase the LEDs corresponding
--  to those sides of the board will become brighter. The LEDs will thus become
--  brightest as the board is held with any one side down, pointing toward the
--  ground.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with Ada.Real_Time;     use Ada.Real_Time;

with STM32F4_Discovery; use STM32F4_Discovery;
with STM32F4.LIS3DSH;   use STM32F4.LIS3DSH;  -- on the F4 Disco board

with STM32F4.GPIO;      use STM32F4.GPIO;
with STM32F4.Timers;    use STM32F4.Timers;
with STM32F4.RCC;       use STM32F4.RCC;

use STM32F4;

procedure Demo_LIS3DSH is

   Next_Release : Time := Clock;
   Period       : constant Time_Span := Milliseconds (100);  -- arbitrary

   LED_Timer : Timer renames Timer_4;
   --  we use Timer_4 and GPIO_D because the channels of Timer_4 can drive the
   --  LEDs connected via the alternate function mode with GPIO_D

   Timer_Period : constant := 1000;

   procedure Configure_LEDs;
   --  Initializes the four user LEDs on the F4 Discovery board and connects
   --  them to Timer_4.

   procedure Configure_Timer;
   --  Initializes Timer_4 and the four channels for PWM.

   function Pulse (Acceleration : Axis_Acceleration) return Half_Word;
   --  Computes the pulse for the PWM. The approach is to first compute the
   --  percentage of the given acceleration relative to a maximum acceleration
   --  of 1 G. That percentage is then applied to the timer period, giving the
   --  pulse width.

   procedure Drive_LEDs;
   --  Sets the pulse width for the two axes read from the accelerometer so
   --  that the brightness varies with the angle of the board.

   --------------------
   -- Configure_LEDs --
   --------------------

   procedure Configure_LEDs is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (GPIO_D);

      Configuration.Mode        := Mode_AF;  -- essential
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_50MHz;
      Configuration.Resistors   := Floating;

      Configure_IO (Port   => GPIO_D,
                    Pins   => All_LEDs,
                    Config => Configuration);

      Configure_Alternate_Function (GPIO_D, All_LEDs, AF => GPIO_AF_TIM4);
   end Configure_LEDs;

   ---------------------
   -- Configure_Timer --
   ---------------------

   procedure Configure_Timer is
   begin
      Enable_Clock (Timer_4);

      Reset (Timer_4);

      Configure
        (Timer_4,
         Prescaler     => 1,
         Period        => Timer_Period,
         Clock_Divisor => Div1,
         Counter_Mode  => Up);

      for Next_Channel in Timer_Channel loop
         Configure_Channel_Output
           (Timer_4,
            Channel  => Next_Channel,
            Mode     => PWM1,
            State    => Enable,
            Pulse    => 0,
            Polarity => High);
      end loop;

      Set_Autoreload_Preload (Timer_4, True);
   end Configure_Timer;

   -----------
   -- Pulse --
   -----------

   function Pulse (Acceleration : Axis_Acceleration) return Half_Word is
      Percentage      : Float;
      Result          : Word;
      Bracketed_Value : Axis_Acceleration;
      Duty_Period     : constant Word := Current_Autoreload (Timer_4);
      Max_1g          : constant Axis_Acceleration := 1000;
      --  The approximate reading from the accelerometer for 1g, in
      --  milligravities, used because this demo is for a person holding the
      --  board and rotating it, so at most approximately 1g will be seen on
      --  any axis.
      --
      --  We bracket the value to the range -Max_1g .. Max_1g in order
      --  to filter out any movement beyond that of simply "pitching" and
      --  "rolling" around the Z axis running through the center of the chip.
      --  A person could move the board beyond the parameters intended for this
      --  demo simply by jerking the board laterally, for example.
   begin
      if Acceleration > 0 then
         Bracketed_Value := Axis_Acceleration'Min (Acceleration, Max_1g);
      else
         Bracketed_Value := Axis_Acceleration'Max (Acceleration, -Max_1g);
      end if;
      Percentage := (Float (abs (Bracketed_Value)) / Float (Max_1g)) * 100.0;
      Result := (Word (Percentage) * (Duty_Period + 1)) / 100;
      return Half_Word (Result);
   end Pulse;

   ----------------
   -- Drive_LEDs --
   ----------------

   procedure Drive_LEDs is
      Axes           : Axes_Accelerations;
      High_Threshold : constant Axis_Acceleration := 30;  -- arbitrary
      Low_Threshold  : constant Axis_Acceleration := -30;  -- arbitrary
      Off            : constant Half_Word := 0;
   begin
      Get_Accelerations (Accelerometer, Axes);

      if Axes.X < Low_Threshold then
         Set_Compare_Value (LED_Timer, Channel_1, Pulse (Axes.X));
      else
         Set_Compare_Value (LED_Timer, Channel_1, Off);
      end if;

      if Axes.X > High_Threshold then
         Set_Compare_Value (LED_Timer, Channel_3, Pulse (Axes.X));
      else
         Set_Compare_Value (LED_Timer, Channel_3, Off);
      end if;

      if Axes.Y > High_Threshold then
         Set_Compare_Value (LED_Timer, Channel_2, Pulse (Axes.Y));
      else
         Set_Compare_Value (LED_Timer, Channel_2, Off);
      end if;

      if Axes.Y < Low_Threshold then
         Set_Compare_Value (LED_Timer, Channel_4, Pulse (Axes.Y));
      else
         Set_Compare_Value (LED_Timer, Channel_4, Off);
      end if;
   end Drive_LEDs;

begin
   Configure_Accelerometer
     (Accelerometer,
      Output_DataRate => Data_Rate_100Hz,
      Axes_Enable     => XYZ_Enabled,
      SPI_Wire        => Serial_Interface_4Wire,
      Self_Test       => Self_Test_Normal,
      Full_Scale      => Fullscale_2g,
      Filter_BW       => Filter_800Hz);

   if Device_Id (Accelerometer) /= I_Am_LIS3DSH then
      raise Program_Error with "invalid accelerometer";
   end if;

   Configure_Timer;

   Configure_LEDs;

   for Next_Channel in Timer_Channel loop
      Enable_Channel (Timer_4, Next_Channel);
   end loop;

   Enable (Timer_4);

   loop
      Drive_LEDs;

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
 end Demo_LIS3DSH;
