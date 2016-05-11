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

--  This program demonstrates the on-board gyro provided by the L3GD20 chip
--  on the STM32F429 Discovery boards. The pitch, roll, and yaw values are
--  continuously displayed on the LCD, as are the adjusted raw values. Move
--  the board to see them change. The values will be positive or negative,
--  depending on the direction of movement. Note that the values are not
--  constant, even when the board is not moving, due to noise.

--  NB: You may need to reset the board after downloading!

--  This program demonstrates use of the gyro's on-board FIFO, driven by
--  an interrupt signalling that the FIFO is partially full. In particular,
--  the gyro generates an interrupt when the FIFO has at least the number of
--  entries specified by a "watermark" configured by the main program. The
--  FIFO contains at least that many sets of raw values for the three X, Y, and
--  Z axes. We read all those actually in the FIFO, even if greater than the
--  number specified by the watermark. We then average them into one set of
--  three axis readings, and use that for further processing. In particular,
--  on each iteration we subtract the stable bias offset from each axis value
--  and scale by the selected sensitivity. The adjusted and scaled values are
--  displayed on each iteration. The stable bias offsets are also displayed,
--  initially (not iteratively since they are not recomputed).

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);

with Gyro_Interrupts;
with Output_Utils;   use Output_Utils;

with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

with STM32.Device; use STM32.Device;
with STM32.Board;  use STM32.Board;

with STM32.GPIO; use STM32.GPIO;
with STM32.EXTI; use STM32.EXTI;

with LCD_Std_Out;

with L3GD20;  use L3GD20;

procedure Demo_L3GD20 is

   Axes   : L3GD20.Angle_Rates;
   Stable : L3GD20.Angle_Rates;  -- the values when the board is motionless

   Sensitivity : Float;

   Scaled_X  : Float;
   Scaled_Y  : Float;
   Scaled_Z  : Float;

   FIFO_Watermark   : constant L3GD20.FIFO_Level := 15;
   --  arbitrary, 1/2 of the FIFO content

   procedure Get_Gyro_Offsets
     (Offsets      : out Angle_Rates;
      Sample_Count : in Long_Integer);
   --  Computes the averages for the gyro values returned when the board is
   --  motionless
   --  Note this uses polling so the FIFO is not yet involved

   procedure Configure_Gyro;
   --  Configures the on-board gyro chip

   procedure Await_Averaged_Angle_Rates (Rates : out Angle_Rates);

   function Averaged_Rates (Buffer : Angle_Rates_FIFO_Buffer) return Angle_Rates;

   procedure Configure_Gyro_Interrupt;
   --  Configures the gyro's FIFO interrupt (interrupt #2) on the
   --  required port/pin for the F429 Discovery board. Enables the interrupt.
   --  See the F429 Disco User Manual, Table 6, pg 19, for the port/pin.

   procedure Reset_and_Start_Collecting with Inline;
   --  go into Bypass_Mode to reset the hardware, then go into FIFO mode to
   --  begin collecting data into the FIFO buffer

   --------------------------------
   -- Await_Averaged_Angle_Rates --
   --------------------------------

   procedure Await_Averaged_Angle_Rates (Rates : out Angle_Rates) is
      FIFO_Buffer  : Angle_Rates_FIFO_Buffer (FIFO_Level);  -- entire FIFO
      Actual_Depth : FIFO_Level;
   begin
      Suspend_Until_True (Gyro_Interrupts.Event);
      Actual_Depth := Gyro.Current_FIFO_Depth;

      if Actual_Depth < 1 then
         return;
      end if;

      Gyro.Get_Raw_Angle_Rates_FIFO (FIFO_Buffer (1 .. Actual_Depth));
      Rates := Averaged_Rates (FIFO_Buffer (1 .. Actual_Depth));
   end Await_Averaged_Angle_Rates;

   --------------------
   -- Averaged_Rates --
   --------------------

   function Averaged_Rates (Buffer : Angle_Rates_FIFO_Buffer) return Angle_Rates is
      Result  : Angle_Rates;
      Total_X : Long_Integer := 0;
      Total_Y : Long_Integer := 0;
      Total_Z : Long_Integer := 0;
   begin
      for Sample of Buffer loop
         Total_X := Total_X + Long_Integer (Sample.X);
         Total_Y := Total_Y + Long_Integer (Sample.Y);
         Total_Z := Total_Z + Long_Integer (Sample.Z);
      end loop;
      Result.X := Angle_Rate (Total_X / Buffer'Length);
      Result.Y := Angle_Rate (Total_Y / Buffer'Length);
      Result.Z := Angle_Rate (Total_Z / Buffer'Length);
      return Result;
   end Averaged_Rates;

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

      Gyro.Enable_Low_Pass_Filter;
   end Configure_Gyro;

   ------------------------------
   -- Configure_Gyro_Interrupt --
   ------------------------------

   procedure Configure_Gyro_Interrupt is
      Config : GPIO_Port_Configuration;
      --  This is the required port/pin configuration on STM32F429 Disco
      --  boards for interrupt 2 on the L3GD20 gyro. See the F429 Disco
      --  User Manual, Table 6, pg 19.
   begin
      Enable_Clock (MEMS_INT2);
      Config.Mode := Mode_In;
      Config.Resistors := Floating;
      Config.Speed := Speed_50MHz;
      Configure_IO (MEMS_INT2, Config);

      Configure_Trigger (MEMS_INT2, Interrupt_Rising_Edge);
   end Configure_Gyro_Interrupt;

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
         loop
            exit when Gyro.Data_Status.ZYX_Available;
         end loop;
         Gyro.Get_Raw_Angle_Rates (Sample);

         Total_X := Total_X + Long_Integer (Sample.X);
         Total_Y := Total_Y + Long_Integer (Sample.Y);
         Total_Z := Total_Z + Long_Integer (Sample.Z);
      end loop;
      Offsets.X := Angle_Rate (Total_X / Sample_Count);
      Offsets.Y := Angle_Rate (Total_Y / Sample_Count);
      Offsets.Z := Angle_Rate (Total_Z / Sample_Count);
   end Get_Gyro_Offsets;

   --------------------------------
   -- Reset_and_Start_Collecting --
   --------------------------------

   procedure Reset_and_Start_Collecting is
   begin
      --  Going to Bypass_Mode resets the FIFO hardware and so is essential
      Gyro.Set_FIFO_Mode (L3GD20_Bypass_Mode);

      --  Going into FIFO_Mode begins the collection into the hardware FIFO
      Gyro.Set_FIFO_Mode (L3GD20_FIFO_Mode);
   end Reset_and_Start_Collecting;

begin
   LCD_Std_Out.Set_Font (Output_Utils.Selected_Font);

   Configure_Gyro;

   Configure_Gyro_Interrupt;

   Enable_FIFO_Watermark_Interrupt (Gyro);  --  L3DG30 gyro interrupt 2

   Gyro.Set_FIFO_Watermark (FIFO_Watermark);

   Sensitivity := Gyro.Full_Scale_Sensitivity;

   Print (0, 0, "Calibrating");

   Get_Gyro_Offsets (Stable, Sample_Count => 100);  -- arbitrary count
   --  Note this uses polling so the FIFO is not yet involved

   Print_Static_Content (Stable);

   Gyro.Enable_FIFO;

   Gyro.Set_FIFO_Mode (L3GD20_FIFO_Mode);
   --  The device starts in Bypass_Mode, in which no data are collected into
   --  the gyro's hardware FIFO, so going into FIFO_Mode begins the collection.

   loop
      Await_Averaged_Angle_Rates (Axes);
      Reset_and_Start_Collecting;
      --  the FIFO is filling while we display the data

      --  TODO: use the Reference mode to do this automatically!
      --  See section 4.3.2 of the 4505 App Note, pg 18
      Axes.X := Axes.X - Stable.X;
      Axes.Y := Axes.Y - Stable.Y;
      Axes.Z := Axes.Z - Stable.Z;

      --  print the values after the stable offset is removed
      Print (Col_Adjusted, Line1_Adjusted, Axes.X'Img & "   ");
      Print (Col_Adjusted, Line2_Adjusted, Axes.Y'Img & "   ");
      Print (Col_Adjusted, Line3_Adjusted, Axes.Z'Img & "   ");

      Scaled_X := Float (Axes.X) * Sensitivity;
      Scaled_Y := Float (Axes.Y) * Sensitivity;
      Scaled_Z := Float (Axes.Z) * Sensitivity;

      --  print the final values
      Print (Final_Column, Line1_Final, Scaled_X'Img & "  ");
      Print (Final_Column, Line2_Final, Scaled_Y'Img & "  ");
      Print (Final_Column, Line3_Final, Scaled_Z'Img & "  ");
   end loop;
end Demo_L3GD20;
