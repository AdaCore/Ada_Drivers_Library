------------------------------------------------------------------------------
--                                                                          --
--                   Copyright (C) 2015-2016, AdaCore                       --
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

--  This program demonstrates the on-board gyro provided by the L3DG20 chip
--  on the STM32F429 Discovery boards. The pitch, roll, and yaw values are
--  continuously displayed on the LCD, as are the adjusted raw values. Move
--  the board to see them change. The values will be positive or negative,
--  depending on the direction of movement. Note that the values are not
--  constant, even when the board is not moving, due to noise.

--  NB: You may need to reset the board after downloading.

with Last_Chance_Handler;      pragma Unreferenced (Last_Chance_Handler);

with STM32F429_Discovery;  use STM32F429_Discovery;

with STM32F4.L3DG20;  use STM32F4.L3DG20;

with Bitmapped_Drawing;
with BMP_Fonts;     use BMP_Fonts;
with STM32F4.ILI9341;
with STM32F4.GPIO;  use STM32F4.GPIO;
with STM32F4;       use STM32F4;
with STM32F4.RCC;   use STM32F4.RCC;

use STM32F4;

procedure Demo_L3DG20 is

   Axes   : L3DG20.Angle_Rates;
   Stable : L3DG20.Angle_Rates;  -- the values when the board is motionless

   Sensitivity : Float;

   Scaled_X  : Float;
   Scaled_Y  : Float;
   Scaled_Z  : Float;

   --  these constants are used for displaying values on the LCD

   Selected_Font : constant BMP_Font := Font12x12;
   Line_Height   : constant Positive := Char_Height (Selected_Font) + 4;

   --  the locations on the screen for the stable offsets
   Line1_Stable : constant Natural := 0;
   Line2_Stable : constant Natural := Line1_Stable + Line_Height;
   Line3_Stable : constant Natural := Line2_Stable + Line_Height;

   --  the locations on the screen for the raw values
   Line1_Raw : constant Natural := 55; -- leaves room for printing stable values
   Line2_Raw : constant Natural := Line1_Raw + Line_Height;
   Line3_Raw : constant Natural := Line2_Raw + Line_Height;

   --  the column number for displaying raw values dynamically, based on
   --  the length of the longest static label
   Col_Raw : constant Natural := String'("Raw X:")'Length * Char_Width (Selected_Font);

   --  the locations on the screen for values after the offset is removed
   Line1_Adjusted : constant Natural := 110; -- leaves room for printing stable values
   Line2_Adjusted : constant Natural := Line1_Adjusted + Line_Height;
   Line3_Adjusted : constant Natural := Line2_Adjusted + Line_Height;

   --  the column number for displaying adjusted values dynamically, based on
   --  the length of the longest static label
   Col_Adjusted : constant Natural := String'("Adjusted X:")'Length * Char_Width (Selected_Font);

   --  the locations on the screen for the final scaled values
   Line1_Final : constant Natural := 165; -- leaves room for printing adjusted values
   Line2_Final : constant Natural := Line1_Final + Line_Height;
   Line3_Final : constant Natural := Line2_Final + Line_Height;

   --  the column number for displaying the final values dynamically, based on
   --  the length of the longest static label
   Final_Column : constant Natural := String'("X:")'Length * Char_Width (Selected_Font);

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
      -- For the page numbers shown below, the required values are specified in
      -- the STM32F429 Discovery kit User Manual (UM1670) on those pages.
      Initialize_Gyro_Hardware
        (Gyro,
         L3GD20_SPI                  => SPI_5'Access,
         SPI_GPIO                    => GPIO_F'Access,  -- required, pg 23
         SPI_GPIO_AF                 => GPIO_AF_SPI5,
         SCK_Pin                     => Pin_7,          -- required, pg 23
         MISO_Pin                    => Pin_8,          -- required, pg 23
         MOSI_Pin                    => Pin_9,          -- required, pg 23
         CS_GPIO                     => GPIO_C'Access,  -- required, pg 21
         CS_Pin                      => Pin_1,          -- required, pg 21
         Enable_SPI_Clock            => RCC.SPI5_Clock_Enable'Access,
         Enable_SPI_GPIO_Clock       => RCC.GPIOF_Clock_Enable'Access,
         Enable_Chip_Select_Clock    => RCC.GPIOC_Clock_Enable'Access);

      Configure
        (Gyro,
         Power_Mode       => L3GD20_Mode_Active,
         Output_Data_Rate => L3GD20_Output_Data_Rate_95Hz,
         Axes_Enable      => L3GD20_Axes_Enable,
         Bandwidth        => L3GD20_Bandwidth_1,
         BlockData_Update => L3GD20_BlockDataUpdate_Continous,
         Endianness       => L3GD20_Little_Endian,
         Full_Scale       => L3GD20_Fullscale_2000);

      Configure_High_Pass_Filter
        (Gyro,
         Mode_Selection   => L3GD20_HPM_Normal_Mode_Reset,
         Cutoff_Frequency => L3GD20_HPFCF_0);

      Enable_High_Pass_Filter (Gyro);

      --  We cannot check it before configuring the device above.
      if L3DG20.Device_Id (Gyro) /= L3DG20.I_Am_L3GD20 then
         raise Program_Error with "No L3DG20 found";
      end if;
   end Configure_Gyro;

   ---------
   -- LCD --
   ---------

   package LCD renames STM32F4.ILI9341;

   -----------------
   -- LCD_Drawing --
   -----------------

   package LCD_Drawing is new Bitmapped_Drawing
     (Color     => LCD.Colors,
      Set_Pixel => LCD.Set_Pixel);

   -----------
   -- Print --
   -----------

   procedure Print (Location : LCD_Drawing.Display_Point;  Msg : String) is
      --  a convenience routine for writing to the LCD
   begin
      LCD_Drawing.Draw_String
        (Location,
         Msg,
         Selected_Font,
         Foreground => LCD.White,  -- arbitrary
         Background => LCD.Black); -- arbitrary
   end Print;

   --------------------------
   -- Print_Static_Content --
   --------------------------

   procedure Print_Static_Content is
   begin
      --  print the constant offsets computed when the device is motionless
      Print ((0, Line1_Stable), "Stable X:" & Stable.X'Img);
      Print ((0, Line2_Stable), "Stable Y:" & Stable.Y'Img);
      Print ((0, Line3_Stable), "Stable Z:" & Stable.Z'Img);

      --  print the static labels for the values before the offset is removed
      Print ((0, Line1_Raw), "Raw X:");
      Print ((0, Line2_Raw), "Raw Y:");
      Print ((0, Line3_Raw), "Raw Z:");

      --  print the static labels for the values after the offset is removed
      Print ((0, Line1_Adjusted), "Adjusted X:");
      Print ((0, Line2_Adjusted), "Adjusted Y:");
      Print ((0, Line3_Adjusted), "Adjusted Z:");

      --  print the static labels for the final values
      Print ((0, Line1_Final), "X:");
      Print ((0, Line2_Final), "Y:");
      Print ((0, Line3_Final), "Z:");
   end Print_Static_Content;

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
   LCD.Initialize
     (Chip_Select             => (GPIO_C'Access, Pin_2),
      Enable_CS_GPIO_Clock    => GPIOC_Clock_Enable'Access,
      WRX                     => (GPIO_D'Access, Pin_13),
      Enable_WRX_GPIO_Clock   => GPIOD_Clock_Enable'Access,
      Reset                   => (GPIO_D'Access, Pin_12),
      Enable_Reset_GPIO_Clock => GPIOD_Clock_Enable'Access,
      SPI_Chip                => SPI_5'Access,
      Enable_SPI_Clock        => SPI5_Clock_Enable'Access,
      SPI_GPIO                => GPIO_F'Access,
      Enable_SPI_GPIO_Clock   => GPIOF_Clock_Enable'Access,
      SPI_AF                  => GPIO_AF_SPI5,
      SCK_Pin                 => Pin_7,
      MISO_Pin                => Pin_8,
      MOSI_Pin                => Pin_9);

   LCD.Set_Orientation (To => LCD.Portrait_2);

   LCD.Fill (LCD.Black);

   Configure_Gyro;

   Sensitivity := Full_Scale_Sensitivity (Gyro);

   Print ((0,0), "Calibrating");

   Get_Gyro_Offsets (Stable, Sample_Count => 100);  -- arbitrary count

   Print_Static_Content;

   loop
      Await_Data_Ready (Gyro);

      Get_Raw_Angle_Rates (Gyro, Axes);

      --  print the raw values
      Print ((Col_Raw, Line1_Raw), Axes.X'Img & "   ");
      Print ((Col_Raw, Line2_Raw), Axes.Y'Img & "   ");
      Print ((Col_Raw, Line3_Raw), Axes.Z'Img & "   ");

      --  remove the computed stable offsets from the raw values
      Axes.X := Axes.X - Stable.X;
      Axes.Y := Axes.Y - Stable.Y;
      Axes.Z := Axes.Z - Stable.Z;

      --  print the values after the stable offset is removed
      Print ((Col_Adjusted, Line1_Adjusted), Axes.X'Img & "   ");
      Print ((Col_Adjusted, Line2_Adjusted), Axes.Y'Img & "   ");
      Print ((Col_Adjusted, Line3_Adjusted), Axes.Z'Img & "   ");

      --  scale the adjusted values
      Scaled_X := Float (Axes.X) * Sensitivity;
      Scaled_Y := Float (Axes.Y) * Sensitivity;
      Scaled_Z := Float (Axes.Z) * Sensitivity;

      --  print the final values
      Print ((Final_Column, Line1_Final), Scaled_X'Img & "  ");
      Print ((Final_Column, Line2_Final), Scaled_Y'Img & "  ");
      Print ((Final_Column, Line3_Final), Scaled_Z'Img & "  ");
   end loop;
end Demo_L3DG20;
