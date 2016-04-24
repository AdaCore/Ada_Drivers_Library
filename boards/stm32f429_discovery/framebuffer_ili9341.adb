with Ada.Unchecked_Conversion;

with STM32.Board;          use STM32.Board;
with STM32.Device;         use STM32.Device;
with STM32.DMA2D.Interrupt;
with STM32.DMA2D.Polling;
with STM32.GPIO;           use STM32.GPIO;
with STM32.SDRAM;          use STM32.SDRAM;
with STM32.SPI;            use STM32.SPI;

with HAL.SPI;

package body Framebuffer_ILI9341 is

   LCD_SPI    : SPI_Port renames SPI_5;
   LCD_WIDTH  : constant := 240;
   LCD_HEIGHT : constant := 320;

   procedure LCD_SPI_Init;
   procedure LCD_Pins_Init;
   procedure Internal_Update_Layer
     (Display : in out Frame_Buffer;
      Layer   : Positive);

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
      Enable_Clock (GPIO_Points'(LCD_CSX, LCD_WRX_DCX));
      Enable_Clock (LCD_PINS);

      Configure_IO
        (Points => (LCD_CSX, LCD_WRX_DCX),
         Config => (Speed       => Speed_50MHz,
                    Mode        => Mode_Out,
                    Output_Type => Push_Pull,
                    Resistors   => Floating));
      Lock (Points => (LCD_CSX, LCD_WRX_DCX));

      Configure_Alternate_Function (LCD_PINS, GPIO_AF_LTDC);
      Configure_Alternate_Function (LCD_RGB_AF9, GPIO_AF_LTDC_2);
      Configure_IO
        (Points => LCD_PINS,
         Config => (Speed       => Speed_50MHz,
                    Mode        => Mode_AF,
                    Output_Type => Push_Pull,
                    Resistors   => Floating));
      Lock (LCD_PINS);

      --  Set LCD_CSX: Chip Unselect
      Set (LCD_CSX);
   end LCD_Pins_Init;

   --------------------
   -- Get_Max_Layers --
   --------------------

   overriding function Get_Max_Layers
     (Display : Frame_Buffer) return Positive
   is
      pragma Unreferenced (Display);
   begin
      return 2;
   end Get_Max_Layers;

   ------------------
   -- Is_Supported --
   ------------------

   overriding function Is_Supported
     (Display : Frame_Buffer;
      Mode    : HAL.Framebuffer.FB_Color_Mode) return Boolean
   is
      pragma Unreferenced (Display, Mode);
   begin
      --  The LTDC supports all HAL color modes
      return True;
   end Is_Supported;

   ---------------
   -- Get_Width --
   ---------------

   overriding function Get_Width
     (Display : Frame_Buffer) return Positive
   is
   begin
      if not Display.Swapped then
         return LCD_WIDTH;
      else
         return LCD_HEIGHT;
      end if;
   end Get_Width;

   ----------------
   -- Get_Height --
   ----------------

   overriding function Get_Height
     (Display : Frame_Buffer) return Positive
   is
   begin
      if not Display.Swapped then
         return LCD_HEIGHT;
      else
         return LCD_WIDTH;
      end if;
   end Get_Height;

   ----------------
   -- Is_Swapped --
   ----------------

   overriding function Is_Swapped
     (Display : Frame_Buffer) return Boolean
   is
   begin
      return Display.Swapped;
   end Is_Swapped;

   --------------------
   -- Set_Background --
   --------------------

   overriding procedure Set_Background
     (Display : Frame_Buffer; R, G, B : Byte)
   is
      pragma Unreferenced (Display);
   begin
      STM32.LTDC.Set_Background (R, G, B);
   end Set_Background;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Display     : in out Frame_Buffer;
      Orientation : HAL.Framebuffer.Display_Orientation := Default;
      Mode        : HAL.Framebuffer.Wait_Mode := Interrupt)
   is
   begin
      LCD_Pins_Init;
      LCD_SPI_Init;
      Display.Display.Initialize (ILI9341.RGB_Mode);
      STM32.LTDC.Initialize
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
         DivR          => 8);
      STM32.SDRAM.Initialize;

      Display.Swapped := Orientation = Landscape;

      case Mode is
         when Polling =>
            STM32.DMA2D.Polling.Initialize;
         when Interrupt =>
            STM32.DMA2D.Interrupt.Initialize;
      end case;
   end Initialize;

   -----------------
   -- Initialized --
   -----------------

   overriding function Initialized
     (Display : Frame_Buffer) return Boolean
   is
      pragma Unreferenced (Display);
   begin
      return STM32.LTDC.Initialized;
   end Initialized;

   ----------------------
   -- Initialize_Layer --
   ----------------------

   overriding procedure Initialize_Layer
     (Display : in out Frame_Buffer;
      Layer   : Positive;
      Mode    : HAL.Framebuffer.FB_Color_Mode;
      X       : Natural := 0;
      Y       : Natural := 0;
      Width   : Positive := Positive'Last;
      Height  : Positive := Positive'Last)
   is
      function To_LTDC_Mode is new Ada.Unchecked_Conversion
        (HAL.Framebuffer.FB_Color_Mode, STM32.LTDC.Pixel_Format);
      LCD_Layer : constant STM32.LTDC.LCD_Layer :=
                    (if Layer = 1
                     then STM32.LTDC.Layer1
                     else STM32.LTDC.Layer2);
      W         : Natural := Width;
      H         : Natural := Height;
      X0        : Natural := X;
      Y0        : Natural := Y;

   begin
      if Display.Swapped then
         X0 := Y;
         W  := Height;
         if Width = Positive'Last then
            Y0 := 0;
            H  := LCD_HEIGHT;
         else
            Y0 := LCD_HEIGHT - X - Width;
            H  := Width;
         end if;
      end if;

      if X0 >= LCD_WIDTH then
         raise Constraint_Error with "Layer X position outside of screen";
      elsif Y0 >= LCD_HEIGHT then
         raise Constraint_Error with "Layer Y position outside of screen";
      end if;

      if W = Positive'Last or else X0 + W > LCD_WIDTH then
         W := LCD_WIDTH - X0;
      end if;

      if H = Positive'Last or else Y0 + H > LCD_HEIGHT then
         H := LCD_HEIGHT - Y0;
      end if;

      if not Display.Swapped then
         for Buf in 1 .. 2 loop
            Display.Buffers (LCD_Layer, Buf) :=
              (Addr       =>
                 Reserve (Word (HAL.Bitmap.Bits_Per_Pixel (Mode) * W * H / 8)),
               Width      => W,
               Height     => H,
               Color_Mode => Mode,
               Swapped    => False);
            Display.Buffers (LCD_Layer, Buf).Fill (0);
         end loop;
      else
         for Buf in 1 .. 2 loop
            Display.Buffers (LCD_Layer, Buf) :=
              (Addr       =>
                 Reserve (Word (HAL.Bitmap.Bits_Per_Pixel (Mode) * W * H / 8)),
               Width      => H,
               Height     => W,
               Color_Mode => Mode,
               Swapped    => True);
            Display.Buffers (LCD_Layer, Buf).Fill (0);
         end loop;
      end if;

      Display.Current (LCD_Layer) := 1;

      STM32.LTDC.Layer_Init
        (Layer          => LCD_Layer,
         Config         => To_LTDC_Mode (Mode),
         Buffer         => Display.Buffers (LCD_Layer, 1).Addr,
         X              => X0,
         Y              => Y0,
         W              => W,
         H              => H,
         Constant_Alpha => 255,
         BF             => STM32.LTDC.BF_Pixel_Alpha_X_Constant_Alpha);

   end Initialize_Layer;

   -----------------
   -- Initialized --
   -----------------

   overriding function Initialized
     (Display : Frame_Buffer;
      Layer   : Positive) return Boolean
   is
      LCD_Layer  : constant STM32.LTDC.LCD_Layer :=
                     (if Layer = 1
                      then STM32.LTDC.Layer1
                      else STM32.LTDC.Layer2);
      use type STM32.DMA2D_Bitmap.DMA2D_Bitmap_Buffer;
   begin
      return
        Display.Buffers (LCD_Layer, 1) /= STM32.DMA2D_Bitmap.Null_Buffer;
   end Initialized;

   ---------------------------
   -- Internal_Update_Layer --
   ---------------------------

   procedure Internal_Update_Layer
     (Display : in out Frame_Buffer;
      Layer   : Positive)
   is
      LCD_Layer  : constant STM32.LTDC.LCD_Layer :=
                     (if Layer = 1
                      then STM32.LTDC.Layer1
                      else STM32.LTDC.Layer2);
   begin
      case Display.Current (LCD_Layer) is
         when 0 =>
            null;
         when 1 =>
            Display.Buffers (LCD_Layer, 2).Wait_Transfer;
            STM32.LTDC.Set_Frame_Buffer
              (Layer => LCD_Layer,
               Addr  => Display.Buffers (LCD_Layer, 2).Addr);
            Display.Current (LCD_Layer) := 2;
         when 2 =>
            Display.Buffers (LCD_Layer, 1).Wait_Transfer;
            STM32.LTDC.Set_Frame_Buffer
              (Layer => LCD_Layer,
               Addr  => Display.Buffers (LCD_Layer, 1).Addr);
            Display.Current (LCD_Layer) := 1;
      end case;
   end Internal_Update_Layer;

   ------------------
   -- Update_Layer --
   ------------------

   overriding procedure Update_Layer
     (Display   : in out Frame_Buffer;
      Layer     : Positive;
      Copy_Back : Boolean := False)
   is
      Visible, Hidden : STM32.DMA2D_Bitmap.DMA2D_Bitmap_Buffer;
      LCD_Layer       : constant STM32.LTDC.LCD_Layer :=
                          (if Layer = 1
                           then STM32.LTDC.Layer1
                           else STM32.LTDC.Layer2);
   begin
      Internal_Update_Layer (Display, Layer);
      STM32.LTDC.Reload_Config (Immediate => False);

      if Copy_Back then
         if Display.Current (LCD_Layer) = 1 then
            Visible := Display.Buffers (LCD_Layer, 1);
            Hidden  := Display.Buffers (LCD_Layer, 2);
         else
            Visible := Display.Buffers (LCD_Layer, 2);
            Hidden  := Display.Buffers (LCD_Layer, 1);
         end if;

         STM32.DMA2D_Bitmap.Copy_Rect
           (Visible, 0, 0, Hidden, 0, 0, Visible.Width, Visible.Height);
      end if;
   end Update_Layer;

   -------------------
   -- Update_Layers --
   -------------------

   overriding procedure Update_Layers
     (Display : in out Frame_Buffer)
   is
   begin
      for J in 1 .. 2 loop
         if Display.Initialized (J) then
            Internal_Update_Layer (Display, J);
         end if;
      end loop;

      STM32.LTDC.Reload_Config (Immediate => False);
   end Update_Layers;

   --------------------
   -- Get_Color_Mode --
   --------------------

   overriding function Get_Color_Mode
     (Display : Frame_Buffer;
      Layer   : Positive) return HAL.Framebuffer.FB_Color_Mode
   is
      LCD_Layer  : constant STM32.LTDC.LCD_Layer :=
                     (if Layer = 1
                      then STM32.LTDC.Layer1
                      else STM32.LTDC.Layer2);
   begin
      return Display.Buffers (LCD_Layer, 1).Color_Mode;
   end Get_Color_Mode;

   -----------------------
   -- Get_Hidden_Buffer --
   -----------------------

   overriding function Get_Hidden_Buffer
     (Display : Frame_Buffer;
      Layer   : Positive) return HAL.Bitmap.Bitmap_Buffer'Class
   is
      LCD_Layer  : constant STM32.LTDC.LCD_Layer :=
                     (if Layer = 1
                      then STM32.LTDC.Layer1
                      else STM32.LTDC.Layer2);
   begin
      case Display.Current (LCD_Layer) is
         when 0 | 2 =>
            return Display.Buffers (LCD_Layer, 1);
         when 1 =>
            return Display.Buffers (LCD_Layer, 2);
      end case;
   end Get_Hidden_Buffer;

   --------------------
   -- Get_Pixel_Size --
   --------------------

   overriding function Get_Pixel_Size
     (Display : Frame_Buffer;
      Layer   : Positive) return Positive
   is
      LCD_Layer  : constant STM32.LTDC.LCD_Layer :=
                     (if Layer = 1
                      then STM32.LTDC.Layer1
                      else STM32.LTDC.Layer2);
   begin
      return
        HAL.Bitmap.Bits_Per_Pixel
          (Display.Buffers (LCD_Layer, 1).Color_Mode) / 8;
   end Get_Pixel_Size;

end Framebuffer_ILI9341;
