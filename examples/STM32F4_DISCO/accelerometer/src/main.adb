------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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
--     3. Neither the name of the copyright holder nor the names of its     --
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

--  This is a demo of the features available on the STM32F4-DISCOVERY board.
--
--  Tilt the board and the LED closer to the ground will light up.

with Ada.Real_Time;      use Ada.Real_Time;
with HAL;                use HAL;
with STM32.Board;        use STM32.Board;
with LIS3DSH;            use LIS3DSH;

procedure Main is

   Values : LIS3DSH.Axes_Accelerations;

   Threshold_High : constant LIS3DSH.Axis_Acceleration :=  200;
   Threshold_Low  : constant LIS3DSH.Axis_Acceleration := -200;

   procedure My_Delay (Milli : Natural);

   procedure My_Delay (Milli : Natural) is
   begin
      delay until Clock + Milliseconds (Milli);
   end My_Delay;

begin
   Initialize_LEDs;

   Initialize_Accelerometer;

   Accelerometer.Configure
     (Output_DataRate => Data_Rate_100Hz,
      Axes_Enable     => XYZ_Enabled,
      SPI_Wire        => Serial_Interface_4Wire,
      Self_Test       => Self_Test_Normal,
      Full_Scale      => Fullscale_2g,
      Filter_BW       => Filter_800Hz);

   if Accelerometer.Device_Id /= I_Am_LIS3DSH then
      All_LEDs_On;
      My_Delay (100);
      All_LEDs_Off;
      My_Delay (100);
   else
      loop
         Accelerometer.Get_Accelerations (Values);
         if abs Values.X > abs Values.Y then
            if Values.X > Threshold_High then
               STM32.Board.Red_LED.Set;
            elsif Values.X < Threshold_Low then
               STM32.Board.Green_LED.Set;
            end if;
            My_Delay (10);
         else
            if Values.Y > Threshold_High then
               STM32.Board.Orange_LED.Set;
            elsif Values.Y < Threshold_Low then
               STM32.Board.Blue_LED.Set;
            end if;
            My_Delay (10);
         end if;
         All_LEDs_Off;
      end loop;
   end if;
end Main;
