with HAL;      use HAL;
with HAL.SPI;  use HAL.SPI;
with HAL.GPIO; use HAL.GPIO;

package ST7735R is

   type ST7735R_Device
     (Port : not null SPI_Port_Ref;
      CS   : not null GPIO_Point_Ref;
      RS   : not null GPIO_Point_Ref;
      RST  : not null GPIO_Point_Ref)
   is tagged limited private;

   procedure Initialize (LCD : in out ST7735R_Device);

   function Initialized (LCD : ST7735R_Device) return Boolean;

   procedure Turn_On (LCD : ST7735R_Device);
   procedure Turn_Off (LCD : ST7735R_Device);
   procedure Display_Inversion_On (LCD : ST7735R_Device);
   procedure Display_Inversion_Off (LCD : ST7735R_Device);
   procedure Gamma_Set (LCD : ST7735R_Device; Gamma_Curve : UInt4);

   type Pixel_Format is (Pixel_12bits, Pixel_16bits, Pixel_18bits);

   procedure Set_Pixel_Format (LCD : ST7735R_Device; Pix_Fmt : Pixel_Format);

   type RGB_BGR_Order is (RGB_Order, BGR_Order)
     with Size => 1;
   type Vertical_Refresh_Order is (Vertical_Refresh_Top_Bottom,
                                   Vertical_Refresh_Botton_Top)
     with Size => 1;
   type Horizontal_Refresh_Order is (Horizontal_Refresh_Left_Right,
                                     Horizontal_Refresh_Right_Left)
     with Size => 1;
   type Row_Address_Order is (Row_Address_Top_Bottom,
                              Row_Address_Bottom_Top)
     with Size => 1;
   type Column_Address_Order is (Column_Address_Left_Right,
                                 Column_Address_Right_Left)
     with Size => 1;

   procedure Set_Memory_Data_Access
     (LCD                 : ST7735R_Device;
      Color_Order         : RGB_BGR_Order;
      Vertical            : Vertical_Refresh_Order;
      Horizontal          : Horizontal_Refresh_Order;
      Row_Addr_Order      : Row_Address_Order;
      Column_Addr_Order   : Column_Address_Order;
      Row_Column_Exchange : Boolean);

   procedure Set_Frame_Rate_Normal
     (LCD         : ST7735R_Device;
      RTN         : UInt4;
      Front_Porch : UInt6;
      Back_Porch  : UInt6);

   procedure Set_Frame_Rate_Idle
     (LCD         : ST7735R_Device;
      RTN         : UInt4;
      Front_Porch : UInt6;
      Back_Porch  : UInt6);

   procedure Set_Frame_Rate_Partial_Full
     (LCD              : ST7735R_Device;
      RTN_Part         : UInt4;
      Front_Porch_Part : UInt6;
      Back_Porch_Part  : UInt6;
      RTN_Full         : UInt4;
      Front_Porch_Full : UInt6;
      Back_Porch_Full  : UInt6);

   type Inversion_Control is (Dot_Inversion, Line_Inversion);

   procedure Set_Inversion_Control
     (LCD : ST7735R_Device;
      Normal, Idle, Full_Partial : Inversion_Control);

   procedure Set_Power_Control_1
     (LCD  : ST7735R_Device;
      AVDD : UInt3;
      VRHP : UInt5;
      VRHN : UInt5;
      MODE : UInt2);

   procedure Set_Power_Control_2
     (LCD   : ST7735R_Device;
      VGH25 : UInt2;
      VGSEL : UInt2;
      VGHBT : UInt2);

   procedure Set_Power_Control_3
     (LCD : ST7735R_Device;
      P1, P2 : Byte);

   procedure Set_Power_Control_4
     (LCD : ST7735R_Device;
      P1, P2 : Byte);

   procedure Set_Power_Control_5
     (LCD : ST7735R_Device;
      P1, P2 : Byte);

   procedure Set_Vcom (LCD : ST7735R_Device; VCOMS : UInt6);

   procedure Set_Column_Address (LCD : ST7735R_Device; X_Start, X_End : Short);
   procedure Set_Row_Address (LCD : ST7735R_Device; Y_Start, Y_End : Short);
   procedure Set_Address (LCD : ST7735R_Device;
                          X_Start, X_End, Y_Start, Y_End : Short);

   procedure Set_Pixel (LCD   : ST7735R_Device;
                        X, Y  : Short;
                        Color : Short);

   procedure Write_Raw_Pixels (LCD  : ST7735R_Device;
                               Data : HAL.Byte_Array);
   procedure Write_Raw_Pixels (LCD  : ST7735R_Device;
                               Data : HAL.Short_Array);

private

   for RGB_BGR_Order use
     (RGB_Order => 0,
      BGR_Order => 1);

   for Vertical_Refresh_Order use
     (Vertical_Refresh_Top_Bottom => 0,
      Vertical_Refresh_Botton_Top => 1);

   for Horizontal_Refresh_Order use
     (Horizontal_Refresh_Left_Right => 0,
      Horizontal_Refresh_Right_Left => 1);

   for Row_Address_Order use
     (Row_Address_Top_Bottom => 0,
      Row_Address_Bottom_Top => 1);

   for Column_Address_Order use
     (Column_Address_Left_Right => 0,
      Column_Address_Right_Left => 1);

   type ST7735R_Device
     (Port : not null SPI_Port_Ref;
      CS   : not null GPIO_Point_Ref;
      RS   : not null GPIO_Point_Ref;
      RST  : not null GPIO_Point_Ref)
   is tagged limited record
      Initialized : Boolean := True;
   end record;

end ST7735R;
