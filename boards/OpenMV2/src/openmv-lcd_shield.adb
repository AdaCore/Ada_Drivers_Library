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

with STM32.GPIO;
with STM32.Device;
with OpenMV;
with ST7735R;                 use ST7735R;
with ST7735R.RAM_Framebuffer; use ST7735R.RAM_Framebuffer;
with Ravenscar_Time;

package body OpenMV.LCD_Shield is

   LCD_RST : STM32.GPIO.GPIO_Point renames Shield_PWM1;
   LCD_RS  : STM32.GPIO.GPIO_Point renames Shield_PWM2;
   LCD_CS  : STM32.GPIO.GPIO_Point renames Shield_SEL;
   All_Points  : constant STM32.GPIO.GPIO_Points := (LCD_RS, LCD_CS, LCD_RST);

   LCD_Driver : ST7735R_RAM_Framebuffer_Screen (Shield_SPI'Access,
                                                LCD_CS'Access,
                                                LCD_RS'Access,
                                                LCD_RST'Access,
                                                Ravenscar_Time.Delays);
   Is_Initialized : Boolean := False;

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean is (Is_Initialized);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      GPIO_Conf : STM32.GPIO.GPIO_Port_Configuration;
   begin

      --  Initalize shield SPI port
      Initialize_Shield_SPI;

      STM32.Device.Enable_Clock (All_Points);

      GPIO_Conf.Mode        := STM32.GPIO.Mode_Out;
      GPIO_Conf.Output_Type := STM32.GPIO.Push_Pull;
      GPIO_Conf.Speed       := STM32.GPIO.Speed_100MHz;
      GPIO_Conf.Resistors   := STM32.GPIO.Floating;

      STM32.GPIO.Configure_IO (All_Points, GPIO_Conf);

      Initialize (LCD_Driver);

      Set_Memory_Data_Access
        (LCD                 => LCD_Driver,
         Color_Order         => RGB_Order,
         Vertical            => Vertical_Refresh_Top_Bottom,
         Horizontal          => Horizontal_Refresh_Left_Right,
         Row_Addr_Order      => Row_Address_Bottom_Top,
         Column_Addr_Order   => Column_Address_Right_Left,
         Row_Column_Exchange => False);

      Set_Pixel_Format (LCD_Driver, Pixel_16bits);

      Set_Frame_Rate_Normal (LCD_Driver,
                             RTN         => 16#01#,
                             Front_Porch => 16#2C#,
                             Back_Porch  => 16#2D#);
      Set_Frame_Rate_Idle (LCD_Driver,
                           RTN         => 16#01#,
                           Front_Porch => 16#2C#,
                           Back_Porch  => 16#2D#);
      Set_Frame_Rate_Partial_Full (LCD_Driver,
                                   RTN_Part         => 16#01#,
                                   Front_Porch_Part => 16#2C#,
                                   Back_Porch_Part  => 16#2D#,
                                   RTN_Full         => 16#01#,
                                   Front_Porch_Full => 16#2C#,
                                   Back_Porch_Full  => 16#2D#);
      Set_Inversion_Control (LCD_Driver,
                             Normal       => Line_Inversion,
                             Idle         => Line_Inversion,
                             Full_Partial => Line_Inversion);
      Set_Power_Control_1 (LCD_Driver,
                           AVDD => 2#101#,    --  5
                           VRHP => 2#0_0010#, --  4.6
                           VRHN => 2#0_0010#, --  -4.6
                           MODE => 2#10#);    --  AUTO

      Set_Power_Control_2 (LCD_Driver,
                           VGH25 => 2#11#,  --  2.4
                           VGSEL => 2#01#,  --  3*AVDD
                           VGHBT => 2#01#); --  -10

      Set_Power_Control_3 (LCD_Driver, 16#0A#, 16#00#);
      Set_Power_Control_4 (LCD_Driver, 16#8A#, 16#2A#);
      Set_Power_Control_5 (LCD_Driver, 16#8A#, 16#EE#);
      Set_Vcom (LCD_Driver, 16#E#);

      Set_Address (LCD_Driver,
                   X_Start => 0,
                   X_End   => 127,
                   Y_Start => 0,
                   Y_End   => 159);

      Turn_On (LCD_Driver);

      LCD_Driver.Initialize_Layer (Layer  => 1,
                                   Mode   => HAL.Bitmap.RGB_565,
                                   X      => 0,
                                   Y      => 0,
                                   Width  => Image_Width,
                                   Height => Image_Height);
      Is_Initialized := True;
   end Initialize;

   ------------
   -- Bitmap --
   ------------

   function Bitmap return not null HAL.Bitmap.Any_Bitmap_Buffer is
   begin
      return LCD_Driver.Hidden_Buffer (1);
   end Bitmap;

   -----------------------
   -- Rotate_Screen_180 --
   -----------------------

   procedure Rotate_Screen_180 is
   begin
      Set_Memory_Data_Access
        (LCD                 => LCD_Driver,
         Color_Order         => RGB_Order,
         Vertical            => Vertical_Refresh_Top_Bottom,
         Horizontal          => Horizontal_Refresh_Left_Right,
         Row_Addr_Order      => Row_Address_Top_Bottom,
         Column_Addr_Order   => Column_Address_Left_Right,
         Row_Column_Exchange => False);
   end Rotate_Screen_180;

   ---------------------
   -- Rotate_Screen_0 --
   ---------------------

   procedure Rotate_Screen_0 is
   begin
      Set_Memory_Data_Access
        (LCD                 => LCD_Driver,
         Color_Order         => RGB_Order,
         Vertical            => Vertical_Refresh_Top_Bottom,
         Horizontal          => Horizontal_Refresh_Left_Right,
         Row_Addr_Order      => Row_Address_Bottom_Top,
         Column_Addr_Order   => Column_Address_Right_Left,
         Row_Column_Exchange => False);
   end Rotate_Screen_0;

   -------------
   -- Display --
   -------------

   procedure Display is
   begin
      LCD_Driver.Update_Layer (1);
   end Display;

end OpenMV.LCD_Shield;
