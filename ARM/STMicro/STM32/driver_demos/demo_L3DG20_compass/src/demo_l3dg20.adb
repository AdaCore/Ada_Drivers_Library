------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
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

--  This program demonstrates the on-board gyro provided by the L3DG20 chip on
--  the STM32F429 Discovery boards. Data availability is determined by polling
--  a status flag.
--
--  The heading, along the Z axis, is continuously displayed. Values are
--  between 0 and 359 degrees. Zero degrees, i.e., "north", is at the
--  end of the board where the power cable is attached.

--  NB: You may need to reset or cycle power to the board after downloading!

--  The gyro is not physically centered on the major or minor axis of the
--  Discovery board, so when testing this program you must rotate the board
--  about the center of the gyro, not the center of the board.

--  The gyro will inevitably exhibit drift (because the F429 Discovery board
--  does not have an on-board accelerometer to provide corrective input).

with Last_Chance_Handler;      pragma Unreferenced (Last_Chance_Handler);

with Ada.Real_Time;  use Ada.Real_Time;

with STM32F429_Discovery;  use STM32F429_Discovery;

with STM32F4.L3DG20; use STM32F4.L3DG20;
with STM32F4.GPIO;   use STM32F4.GPIO;
with STM32F4;        use STM32F4;
with STM32F4.RCC;    use STM32F4.RCC;
with STM32F4.SYSCFG; use STM32F4.SYSCFG;
with STM32F4.EXTI;   use STM32F4.EXTI;

with Output_Utils;   use Output_Utils;

procedure Demo_L3DG20 is

   Raw    : L3DG20.Angle_Rates;
   Stable : L3DG20.Angle_Rates;  -- the values when the board is motionless

   Sensitivity       : Float;
   Sensitivity_Tweak : constant Float := 1.55;
   -- The value varies by actual device, so experiment. The sensitivities
   -- specified by the datasheet are statistical, not guaranteed.

   Threshold : constant := 10;  -- tuning
   --  New readings that do not vary by more than the threshold are ignored.
   --  The value is based on the full_scale factor. See "DVoff" in the
   --  datasheet, table 4, pg 9. If this value is too small, slow rotation
   --  rates are lost.

   Interval : constant := 100;
   --  Used as a single definition to then define the Sample_Period* values
   --  used for the heading calculation and scheduling, below. Value is in
   --  milliseconds. Must reflect reality, and the LCD is quite slow. We are
   --  displaying the heading in each iteration, otherwise the period could
   --  be a 10th of this.

   Sample_Period_Time : constant Float := Float (Interval) / 1000.0;
   --  used for calculating angular displacement

   Sample_Period : constant Time_Span := Milliseconds (Interval);
   --  used for the delay statement

   Next_Release: Time;

   New_Z  : Float;

   Angle   : Float := 0.0;  -- angular displacement for the Z axis
   Heading : Integer;

   procedure Get_Gyro_Offsets
     (Offsets      : out Angle_Rates;
      Sample_Count : in Long_Integer);
   --  computes the averages for the gyro values returned when the board is
   --  motionless

   procedure Configure_Gyro;
   --  configures the on-board gyro

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
      Reset (Gyro);

      Configure
        (Gyro,
         Power_Mode       => L3GD20_Mode_Active,
         Output_Data_Rate => L3GD20_Output_Data_Rate_95Hz,
         Axes_Enable      => L3GD20_Axes_Enable,
         Bandwidth        => L3GD20_Bandwidth_1,
         BlockData_Update => L3GD20_BlockDataUpdate_Continous,
         Endianness       => L3GD20_Little_Endian,
         Full_Scale       => L3GD20_Fullscale_250);
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
         Await_Data_Ready (Gyro);
         Get_Raw_Angle_Rates (Gyro, Sample);
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
   Initialize_Display;
   Initialize_Gyro_Hardware;
   Configure_Gyro;

   Sensitivity := Sensitivity_Tweak * Full_Scale_Sensitivity (Gyro);

   Print ((0, 0), "Calibrating");

   Get_Gyro_Offsets (Stable, Sample_Count => 150);  -- arbitrary count

   Stable.Z := -Stable.Z;
   --  Clockwise rotation results in negative values, so we swap the sign

   Print_Static_Content (Stable, Sensitivity);

   Next_Release := Clock;
   loop
      Await_Data_Ready (Gyro);

      Get_Raw_Angle_Rates (Gyro, Raw);

      Raw.Z := -Raw.Z;
      --  Clockwise rotation results in negative values, so we swap the sign

      if abs (Raw.Z - Stable.Z) > Threshold then
         Raw.Z := Raw.Z - Stable.Z;
         New_Z := Float (Raw.Z) * Sensitivity;
         Angle := Angle + (New_Z * Sample_Period_Time);
      end if;

      Heading := Integer (Angle) mod 360;

      Print ((Heading_Column, Heading_Line), Heading'Img & "  ");

      Next_Release := Next_Release + Sample_Period;
      delay until Next_Release;
   end loop;
end Demo_L3DG20;
