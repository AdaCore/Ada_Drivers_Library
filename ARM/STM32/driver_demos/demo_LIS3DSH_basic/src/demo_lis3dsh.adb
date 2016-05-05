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

--  This program demonstrates basic use of the LIS3DSH accelerometer chip.
--  (The LIS3DSH is used on later versions of the STM32F4 Discovery boards,
--  designated by the number MB997C printed on the top of the board.)
--
--  The LEDs surrounding the accelerometer will flash on and off as the board
--  is moved, reflecting the directions of the accelerations measured by the
--  device.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with Interfaces;    use Interfaces;
with Ada.Real_Time; use Ada.Real_Time;
with STM32.Board;   use STM32.Board;
with LIS3DSH;       use LIS3DSH;

use STM32;

procedure Demo_LIS3DSH is

   Axes : Axes_Accelerations;

   Threshold : constant := 100;  -- arbitrary

   procedure Panic;

   procedure Panic is
   begin
      loop
         All_LEDs_On;
         delay until Clock + Milliseconds (250);
         All_LEDs_Off;
         delay until Clock + Milliseconds (250);
      end loop;
   end Panic;

begin
   Initialize_LEDs;
   All_LEDs_Off;

   --  short delay on power up
   delay until Clock + Milliseconds (10);

   Initialize_Accelerometer;

   Accelerometer.Configure
     (Output_DataRate => Data_Rate_100Hz,
      Axes_Enable     => XYZ_Enabled,
      SPI_Wire        => Serial_Interface_4Wire,
      Self_Test       => Self_Test_Normal,
      Full_Scale      => Fullscale_2g,
      Filter_BW       => Filter_800Hz);

   if Accelerometer.Device_Id /= I_Am_LIS3DSH then
      Panic;
   end if;

   loop
      Accelerometer.Get_Accelerations (Axes);

      if Axes.X > Threshold then
         Turn_On (Red);
      else
         Turn_Off (Red);
      end if;

      if Axes.X < -Threshold then
         Turn_On (Green);
      else
         Turn_Off (Green);
      end if;

      if Axes.Y > Threshold then
         Turn_On (Orange);
      else
         Turn_Off (Orange);
      end if;

      if Axes.Y < -Threshold then
         Turn_On (Blue);
      else
         Turn_Off (Blue);
      end if;
   end loop;
end Demo_LIS3DSH;
