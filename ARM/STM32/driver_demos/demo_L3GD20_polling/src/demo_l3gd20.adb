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

--  This program demonstrates the on-board gyro provided by the L3GD20 chip
--  on the STM32F429 Discovery boards. The pitch, roll, and yaw values are
--  continuously displayed on the LCD, as are the adjusted raw values. Move
--  the board to see them change. The values will be positive or negative,
--  depending on the direction of movement. Note that the values are not
--  constant, even when the board is not moving, due to noise.

--  Polling is used to determine when gyro data are available.

--  NB: You may need to reset the board after downloading.

with Last_Chance_Handler;      pragma Unreferenced (Last_Chance_Handler);

with STM32.Device; use STM32.Device;
with STM32.Board;  use STM32.Board;
with LCD_Std_Out;  use LCD_Std_Out;

with L3GD20;       use L3GD20;
with Output_Utils; use Output_Utils;

procedure Demo_L3GD20 is

   Axes   : L3GD20.Angle_Rates;
   Stable : L3GD20.Angle_Rates;  -- the values when the board is motionless

   Sensitivity : Float;

   Scaled_X  : Float;
   Scaled_Y  : Float;
   Scaled_Z  : Float;

   procedure Get_Gyro_Offsets
     (Offsets      : out Angle_Rates;
      Sample_Count : in Long_Integer);
   --  computes the averages for the gyro values returned when the board is
   --  motionless

   procedure Configure_Gyro;
   --  configures the on-board gyro chip

   Timeout : exception;
   --  raised by Await_Data_Ready when data is not ready within a reasonable
   --  time

   procedure Await_Data_Ready (This : in out Three_Axis_Gyroscope);
   --  Polls the gyro data status, returning when data for all three axes are
   --  available. Raises Timeout when a "reasonable" number of attempts have
   --  been made.

   --------------------
   -- Configure_Gyro --
   --------------------

   procedure Configure_Gyro is
   begin
      --  Init the on-board gyro SPI and GPIO. This is board-specific, not
      --  every board has a gyro. The F429 Discovery does, for example, but
      --  the F4 Discovery does not.
      STM32.Board.Initialize_Gyro_IO;

      Gyro.Reset;

      Gyro.Configure
        (Power_Mode       => L3GD20_Mode_Active,
         Output_Data_Rate => L3GD20_Output_Data_Rate_95Hz,
         Axes_Enable      => L3GD20_Axes_Enable,
         Bandwidth        => L3GD20_Bandwidth_1,
         BlockData_Update => L3GD20_BlockDataUpdate_Continous,
         Endianness       => L3GD20_Little_Endian,
         Full_Scale       => L3GD20_Fullscale_250);

      Enable_Low_Pass_Filter (Gyro);
   end Configure_Gyro;

   ----------------------
   -- Get_Gyro_Offsets --
   ----------------------

   procedure Get_Gyro_Offsets
     (Offsets      : out Angle_Rates;
      Sample_Count : in Long_Integer)
   is
      Sample  : Angle_Rates;
      Total_X : Long_Integer := 0;
      Total_Y : Long_Integer := 0;
      Total_Z : Long_Integer := 0;
   begin
      for K in 1 .. Sample_Count loop
         Gyro.Get_Raw_Angle_Rates (Sample);
         Total_X := Total_X + Long_Integer (Sample.X);
         Total_Y := Total_Y + Long_Integer (Sample.Y);
         Total_Z := Total_Z + Long_Integer (Sample.Z);
      end loop;
      Offsets.X := Angle_Rate (Total_X / Sample_Count);
      Offsets.Y := Angle_Rate (Total_Y / Sample_Count);
      Offsets.Z := Angle_Rate (Total_Z / Sample_Count);
   end Get_Gyro_Offsets;

   ----------------------
   -- Await_Data_Ready --
   ----------------------

   procedure Await_Data_Ready (This : in out Three_Axis_Gyroscope) is
      Max_Status_Attempts : constant := 10_000;
      --  This timeout value is arbitrary but must be sufficient for the
      --  slower gyro data rate options and higher clock rates.  It need not be
      --  as small as possible, the point is not to hang forever.
   begin
      for K in 1 .. Max_Status_Attempts loop
         exit when Data_Status (This).ZYX_Available;
         if K = Max_Status_Attempts then
            raise Timeout with "no angle rate data";
         end if;
      end loop;
   end Await_Data_Ready;

begin
   LCD_Std_Out.Set_Font (Output_Utils.Selected_Font);

   Configure_Gyro;

   Sensitivity := Gyro.Full_Scale_Sensitivity;

   Print (0, 0, "Calibrating");

   Get_Gyro_Offsets (Stable, Sample_Count => 100);  -- arbitrary count

   Print_Static_Content (Stable);

   loop
      Await_Data_Ready (Gyro);

      Gyro.Get_Raw_Angle_Rates (Axes);

      --  print the raw values
      Print (Col_Raw, Line1_Raw, Axes.X'Img & "   ");
      Print (Col_Raw, Line2_Raw, Axes.Y'Img & "   ");
      Print (Col_Raw, Line3_Raw, Axes.Z'Img & "   ");

      --  remove the computed stable offsets from the raw values
      Axes.X := Axes.X - Stable.X;
      Axes.Y := Axes.Y - Stable.Y;
      Axes.Z := Axes.Z - Stable.Z;

      --  print the values after the stable offset is removed
      Print (Col_Adjusted, Line1_Adjusted, Axes.X'Img & "   ");
      Print (Col_Adjusted, Line2_Adjusted, Axes.Y'Img & "   ");
      Print (Col_Adjusted, Line3_Adjusted, Axes.Z'Img & "   ");

      --  scale the adjusted values
      Scaled_X := Float (Axes.X) * Sensitivity;
      Scaled_Y := Float (Axes.Y) * Sensitivity;
      Scaled_Z := Float (Axes.Z) * Sensitivity;

      --  print the final scaled values
      Print (Final_Column, Line1_Final, Scaled_X'Img & "  ");
      Print (Final_Column, Line2_Final, Scaled_Y'Img & "  ");
      Print (Final_Column, Line3_Final, Scaled_Z'Img & "  ");
   end loop;
end Demo_L3GD20;
