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

--  This program demonstrates the on-board gyro provided by the L3DG20 chip
--  on the STM32F429 Discovery boards. The pitch, roll, and yaw values are
--  continuously displayed on the LCD. Move the board to see them change.
--  The values will be positive or negative, depending on the direction of
--  movement. Note that the values are not constant, even when the board is
--  not moving, due to noise.

with Last_Chance_Handler;      pragma Unreferenced (Last_Chance_Handler);
with Interfaces;   use Interfaces;

with STM32.Device; use STM32.Device;
with STM32_Board;  use STM32_Board;

with STM32.L3DG20; use STM32.L3DG20;

with Bitmapped_Drawing;
with BMP_Fonts;
with STM32.ILI9341;
with STM32;        use STM32;
with STM32.GPIO;   use STM32.GPIO;
with STM32.RCC;    use STM32.RCC;

procedure Demo_L3DG20 is

   Axes   : L3DG20.Angle_Rates;
   Stable : L3DG20.Angle_Rates;

   Sensitivity : Float;

   Selected_Font : constant BMP_Fonts.BMP_Font := BMP_Fonts.Font12x12;
   Line_Height   : constant Positive := BMP_Fonts.Char_Height (Selected_Font) + 4;

   --------------------
   -- Configure_Gyro --
   --------------------

   procedure Configure_Gyro is
   begin
      Initialize_Gyro_Hardware
        (Gyro,
         L3GD20_SPI                  => SPI_5'Access,
         SPI_GPIO                    => GPIO_F'Access,  -- required, see F429 Disco UG (UM1670), pg 23
         SPI_GPIO_AF                 => GPIO_AF_SPI5,
         SCK_Pin                     => Pin_7,          -- required, see F429 Disco UG (UM1670), pg 23
         MISO_Pin                    => Pin_8,          -- required, see F429 Disco UG (UM1670), pg 23
         MOSI_Pin                    => Pin_9,          -- required, see F429 Disco UG (UM1670), pg 23
         CS_GPIO                     => GPIO_C'Access,  -- required, see F429 Disco UG (UM1670), pg 21
         CS_Pin                      => Pin_1,          -- required, see F429 Disco UG (UM1670), pg 21
         Int_GPIO                    => GPIO_A'Access,
         Enable_SPI_Clock            => RCC.SPI5_Clock_Enable'Access,
         Enable_SPI_GPIO_Clock       => RCC.GPIOF_Clock_Enable'Access,
         Enable_Chip_Select_Clock    => RCC.GPIOC_Clock_Enable'Access,
         Enable_GPIO_Interrupt_Clock => RCC.GPIOA_Clock_Enable'Access);

      Configure
        (Gyro,
         Power_Mode       => L3GD20_Mode_Active,  -- ie enabled
         Output_DataRate  => L3GD20_Output_DataRate_4,
         Axes_Enable      => L3GD20_Axes_Enable,
         Band_Width       => L3GD20_Bandwidth_4,
         BlockData_Update => L3GD20_BlockDataUpdate_Continous,
         Endianness       => L3GD20_BLE_LSB,
         Full_Scale       => L3GD20_Fullscale_2000);

      Configure_Filter
        (Gyro,
         Mode_Selection   => L3GD20_HPM_Normal_Mode_Res,
         CutOff_Frequency => L3GD20_HPFCF_0);

      Enable_Filter (Gyro);

      --  We cannot check it before configuring the device above.
      if L3DG20.Device_Id (Gyro) /= L3DG20.I_Am_L3GD20 then
         raise Program_Error with "No L3DG20 found";
      end if;
   end Configure_Gyro;

   ---------
   -- LCD --
   ---------

   package LCD renames STM32.ILI9341; use LCD;

   -----------------
   -- LCD_Drawing --
   -----------------

   package LCD_Drawing is new Bitmapped_Drawing
     (Color     => LCD.Colors,
      Set_Pixel => LCD.Set_Pixel);

   use LCD_Drawing;

   -----------
   -- Print --
   -----------

   procedure Print (Location : Display_Point; Msg : String) is
   begin
      Draw_String (Location,
                   Msg,
                   Selected_Font,
                   Foreground => White,
                   Background => Black);
   end Print;

   -------------
   -- Display --
   -------------

   procedure Display
     (Rates      : Angle_Rates;
      First_Line : Natural;
      X_Prefix   : String;
      Y_Prefix   : String;
      Z_Prefix   : String)
   is
      Line1 : constant Natural := First_Line;
      Line2 : constant Natural := First_Line + Line_Height;
      Line3 : constant Natural := First_Line + 2 * Line_Height;
   begin
      Print ((0, Line1), X_Prefix & Rates.X'Img & "  ");
      Print ((0, Line2), Y_Prefix & Rates.Y'Img & "  ");
      Print ((0, Line3), Z_Prefix & Rates.Z'Img & "  ");
   end Display;

   ----------------------
   -- Get_Gyro_Offsets --
   ----------------------

   procedure Get_Gyro_Offsets (Offsets : out Angle_Rates) is
      Sample       : Angle_Rates;
      Sample_Count : constant := 200; -- arbitrary
   begin
      Offsets := (others => 0);
      for K in 1 .. Sample_Count loop
         Get_Raw_Angle_Rates (Gyro, Sample);
         Offsets.X := Offsets.X + Sample.X;
         Offsets.Y := Offsets.Y + Sample.Y;
         Offsets.Z := Offsets.Z + Sample.Z;
      end loop;
      Offsets.X := Offsets.X / Sample_Count;
      Offsets.Y := Offsets.Y / Sample_Count;
      Offsets.Z := Offsets.Z / Sample_Count;
   end Get_Gyro_Offsets;

begin
   LCD.Initialize
     (Chip_Select             => PC2,
      Enable_CS_GPIO_Clock    => GPIOC_Clock_Enable'Access,
      WRX                     => PD13,
      Enable_WRX_GPIO_Clock   => GPIOD_Clock_Enable'Access,
      Reset                   => PD12,
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

   LCD.Fill (Black);

   Configure_Gyro;

   Sensitivity := Selected_Sensitivity (Gyro);

   Get_Gyro_Offsets (Stable);

   Display (Stable,
            First_Line => 0,
            X_Prefix   => "Stable X:",
            Y_Prefix   => "Stable Y:",
            Z_Prefix   => "Stable Z:");

   loop
      Get_Raw_Angle_Rates (Gyro, Axes);

      Axes.X := Axes.X - Stable.X;
      Axes.Y := Axes.Y - Stable.Y;
      Axes.Z := Axes.Z - Stable.Z;

      Display (Axes,
               First_Line => 55,
               X_Prefix   => "Adjusted X:",
               Y_Prefix   => "Adjusted Y:",
               Z_Prefix   => "Adjusted Z:");

      Axes.X := Angle_Rate (Float (Axes.X) * Sensitivity);
      Axes.Y := Angle_Rate (Float (Axes.Y) * Sensitivity);
      Axes.Z := Angle_Rate (Float (Axes.Z) * Sensitivity);

      Display (Axes,
               First_Line => 110,
               X_Prefix   => "Pitch:",
               Y_Prefix   => "Roll: ",
               Z_Prefix   => "Yaw:  ");
   end loop;
end Demo_L3DG20;
