------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2016, AdaCore                         --
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

with Interfaces;        use Interfaces;
with Ada.Real_Time;     use Ada.Real_Time;

with STM32.Board;       use STM32.Board;
with LIS3DSH;           use LIS3DSH;  -- on the F4 Disco board

with STM32.GPIO;        use STM32.GPIO;
with STM32.Timers;      use STM32.Timers;
with STM32.PWM;         use STM32.PWM;

use STM32;

with Demo_PWM_Settings; use Demo_PWM_Settings;

procedure Demo_LIS3DSH is

   Next_Release : Time := Clock;
   Period       : constant Time_Span := Milliseconds (100);  -- arbitrary

   function Brightness (Acceleration : Axis_Acceleration) return Percentage;
   --  Computes the output for the PWM. The approach is to compute the
   --  percentage of the given acceleration relative to a maximum acceleration
   --  of 1 G.

   procedure Drive_LEDs;
   --  Sets the pulse width for the two axes read from the accelerometer so
   --  that the brightness varies with the angle of the board.

   ----------------
   -- Brightness --
   ----------------

   function Brightness (Acceleration : Axis_Acceleration) return Percentage is
      Result          : Percentage;
      Bracketed_Value : Axis_Acceleration;
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
      Result := Percentage
        ((Float (abs (Bracketed_Value)) / Float (Max_1g)) * 100.0);
      return Result;
   end Brightness;

   ----------------
   -- Drive_LEDs --
   ----------------

   procedure Drive_LEDs is
      Axes           : Axes_Accelerations;
      High_Threshold : constant Axis_Acceleration := 30;  -- arbitrary
      Low_Threshold  : constant Axis_Acceleration := -30;  -- arbitrary
      Off            : constant Percentage := 0;
   begin
      Accelerometer.Get_Accelerations (Axes);

      if Axes.X < Low_Threshold then
         Set_Duty_Cycle (PWM_Output, Channel_1, Brightness (Axes.X));
      else
         Set_Duty_Cycle (PWM_Output, Channel_1, Off);
      end if;

      if Axes.X > High_Threshold then
         Set_Duty_Cycle (PWM_Output, Channel_3, Brightness (Axes.X));
      else
         Set_Duty_Cycle (PWM_Output, Channel_3, Off);
      end if;

      if Axes.Y > High_Threshold then
         Set_Duty_Cycle (PWM_Output, Channel_2, Brightness (Axes.Y));
      else
         Set_Duty_Cycle (PWM_Output, Channel_2, Off);
      end if;

      if Axes.Y < Low_Threshold then
         Set_Duty_Cycle (PWM_Output, Channel_4, Brightness (Axes.Y));
      else
         Set_Duty_Cycle (PWM_Output, Channel_4, Off);
      end if;
   end Drive_LEDs;

begin
   Initialize_Accelerometer;

   Accelerometer.Configure
     (Output_DataRate => Data_Rate_100Hz,
      Axes_Enable     => XYZ_Enabled,
      SPI_Wire        => Serial_Interface_4Wire,
      Self_Test       => Self_Test_Normal,
      Full_Scale      => Fullscale_2g,
      Filter_BW       => Filter_800Hz);

   if Accelerometer.Device_Id /= I_Am_LIS3DSH then
      raise Program_Error with "invalid accelerometer";
   end if;

   Initialise_PWM_Modulator
     (PWM_Output,
      Requested_Frequency => PWM_Frequency,
      PWM_Timer           => PWM_Output_Timer'Access,
      PWM_AF              => PWM_Output_AF);

   Attach_PWM_Channel
     (PWM_Output,
      Channel => Channel_1,
      Point   => Channel_1_Point);

   Attach_PWM_Channel
     (PWM_Output,
      Channel => Channel_2,
      Point   => Channel_2_Point);

   Attach_PWM_Channel
     (PWM_Output,
      Channel => Channel_3,
      Point   => Channel_3_Point);

   Attach_PWM_Channel
     (PWM_Output,
      Channel => Channel_4,
      Point   => Channel_4_Point);

   for C in Timer_Channel loop
      Enable_PWM_Channel (PWM_Output, C);
   end loop;

   loop
      Drive_LEDs;

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Demo_LIS3DSH;
