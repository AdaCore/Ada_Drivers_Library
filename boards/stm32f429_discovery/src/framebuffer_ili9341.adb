with Ada.Unchecked_Conversion;

with STM32.Board;          use STM32.Board;
with STM32.Device;         use STM32.Device;
with STM32.GPIO;           use STM32.GPIO;
with STM32.SPI;            use STM32.SPI;

with HAL.SPI;

package body Framebuffer_ILI9341 is

   LCD_SPI    : SPI_Port renames SPI_5;
   LCD_WIDTH  : constant := 240;
   LCD_HEIGHT : constant := 320;

   procedure LCD_SPI_Init;
   procedure LCD_Pins_Init;

   ------------------
   -- LCD_SPI_Init --
   ------------------

   procedure LCD_SPI_Init
   is
      Conf     : GPIO_Port_Configuration;
      SPI_Conf : SPI_Configuration;
      SPI_Pins : constant GPIO_Points :=
                   (SPI5_SCK, SPI5_MOSI, SPI5_MISO);

   begin
      Enable_Clock (SPI_Pins);
      Enable_Clock (LCD_SPI);

      Conf.Speed       := Speed_100MHz;
      Conf.Mode        := Mode_AF;
      Conf.Output_Type := Push_Pull;
      Conf.Resistors   := Floating;

      Configure_Alternate_Function (SPI_Pins, GPIO_AF_SPI5);
      Configure_IO (SPI_Pins, Conf);

      Reset (LCD_SPI);

      if not Enabled (LCD_SPI) then
         SPI_Conf :=
           (Direction           => D2Lines_FullDuplex,
            Mode                => Master,
            Data_Size           => HAL.SPI.Data_Size_8b,
            Clock_Polarity      => Low,
            Clock_Phase         => P1Edge,
            Slave_Management    => Software_Managed,
            Baud_Rate_Prescaler => BRP_32,
            First_Bit           => MSB,
            CRC_Poly            => 7);
         Configure (LCD_SPI, SPI_Conf);
         STM32.SPI.Enable (LCD_SPI);
      end if;
   end LCD_SPI_Init;

   -------------------
   -- LCD_Pins_Init --
   -------------------

   procedure LCD_Pins_Init is
   begin
      Enable_Clock (GPIO_Points'(LCD_RESET, LCD_CSX, LCD_WRX_DCX));
      Enable_Clock (LCD_PINS);

      Configure_IO
        (Points => (LCD_RESET, LCD_CSX, LCD_WRX_DCX),
         Config => (Speed       => Speed_50MHz,
                    Mode        => Mode_Out,
                    Output_Type => Push_Pull,
                    Resistors   => Floating));

      Configure_Alternate_Function (LCD_PINS, GPIO_AF_LTDC);
      Configure_Alternate_Function (LCD_RGB_AF9, GPIO_AF_LTDC_2);
      Configure_IO
        (Points => LCD_PINS,
         Config => (Speed       => Speed_50MHz,
                    Mode        => Mode_AF,
                    Output_Type => Push_Pull,
                    Resistors   => Floating));

      --  Set LCD_CSX: Chip Unselect
      Set (LCD_CSX);
   end LCD_Pins_Init;


   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Display     : in out Frame_Buffer;
      Orientation : HAL.Framebuffer.Display_Orientation := Default;
      Mode        : HAL.Framebuffer.Wait_Mode := Interrupt)
   is
   begin
      LCD_Pins_Init;
      LCD_SPI_Init;
      Display.Device.Initialize (ILI9341.RGB_Mode);
      Display.Initialize
        (Width         => LCD_WIDTH,
         Height        => LCD_HEIGHT,
         H_Sync        => 10,
         H_Back_Porch  => 20,
         H_Front_Porch => 10,
         V_Sync        => 2,
         V_Back_Porch  => 2,
         V_Front_Porch => 4,
         PLLSAI_N      => 192,
         PLLSAI_R      => 4,
         DivR          => 8,
         Orientation   => Orientation,
         Mode          => Mode);
   end Initialize;

end Framebuffer_ILI9341;
