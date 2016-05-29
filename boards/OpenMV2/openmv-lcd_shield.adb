with STM32.GPIO;
with STM32.Device;
with OpenMV;
with ST7735R; use ST7735R;

package body OpenMV.LCD_Shield is

   LCD_RST : STM32.GPIO.GPIO_Point renames Shield_PWM1;
   LCD_RS  : STM32.GPIO.GPIO_Point renames Shield_PWM2;
   LCD_CS  : STM32.GPIO.GPIO_Point renames Shield_SEL;
   All_Points  : constant STM32.GPIO.GPIO_Points := (LCD_RS, LCD_CS, LCD_RST);

   LCD_Driver : ST7735R.ST7735R_Device (Shield_SPI'Access,
                                        LCD_CS'Access,
                                        LCD_RS'Access,
                                        LCD_RST'Access);
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

   ----------------
   -- Get_Bitmap --
   ----------------

   function Get_Bitmap return HAL.Bitmap.Bitmap_Buffer'Class is
   begin
      return LCD_Driver.Get_Hidden_Buffer (1);
   end Get_Bitmap;

   -------------
   -- Display --
   -------------

   procedure Display is
   begin
      LCD_Driver.Update_Layer (1);
   end Display;

end OpenMV.LCD_Shield;
