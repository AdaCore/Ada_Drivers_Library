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

with STM32F429_Discovery;  use STM32F429_Discovery;

with STM32F4.L3DG20;  use STM32F4.L3DG20;

with Bitmapped_Drawing;
with BMP_Fonts;
with STM32F4.ILI9341;
with STM32F4.GPIO;  use STM32F4.GPIO;
with STM32F4;       use STM32F4;
with STM32F4.RCC;   use STM32F4.RCC;

use STM32F4;

procedure Demo_L3DG20 is

   Axes : L3DG20.Raw_Angle_Rates;

   --------------------
   -- Configure_Gyro --
   --------------------

   procedure Configure_Gyro is
   begin
      Initialize_Gyro_Hardware
        (Gyro,
         L3GD20_SPI                  => SPI_5'Access,
         SPI_GPIO                    => GPIO_F'Access,
         SPI_GPIO_AF                 => GPIO_AF_SPI5,
         SCK_Pin                     => Pin_7,
         MISO_Pin                    => Pin_8,
         MOSI_Pin                    => Pin_9,
         CS_GPIO                     => GPIO_C'Access,
         CS_Pin                      => Pin_1,
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

   package LCD renames STM32F4.ILI9341; use LCD;

   -----------------
   -- LCD_Drawing --
   -----------------

   package LCD_Drawing is new Bitmapped_Drawing
     (Color     => LCD.Colors,
      Set_Pixel => LCD.Set_Pixel);

   use LCD_Drawing;

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

   Configure_Gyro;

   loop
      Get_Raw_Angle_Rates (Gyro, Axes);

      Draw_String ((0, 50), "Pitch:" & Axes.X'Img & "  ",
                   Font       => BMP_Fonts.Font16x24,
                   Foreground => LCD.White,
                   Background => LCD.Black);

      Draw_String ((0, 80), "Roll: " & Axes.Y'Img & "  ",
                   Font       => BMP_Fonts.Font16x24,
                   Foreground => LCD.White,
                   Background => LCD.Black);

      Draw_String ((0, 110), "Yaw:  " & Axes.Z'Img & "  ",
                   Font       => BMP_Fonts.Font16x24,
                   Foreground => LCD.White,
                   Background => LCD.Black);
   end loop;
end Demo_L3DG20;
