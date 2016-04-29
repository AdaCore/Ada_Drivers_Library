with Ada.Unchecked_Conversion;

with STM32.Device;          use STM32.Device;
with STM32.DMA2D.Interrupt;
with STM32.DMA2D.Polling;
with STM32.SDRAM;           use STM32.SDRAM;

package body Framebuffer_LTDC is

   procedure Internal_Update_Layer
     (Display : in out Frame_Buffer;
      Layer   : Positive);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Display       : in out Frame_Buffer;
      Width         : Positive;
      Height        : Positive;
      H_Sync        : Natural;
      H_Back_Porch  : Natural;
      H_Front_Porch : Natural;
      V_Sync        : Natural;
      V_Back_Porch  : Natural;
      V_Front_Porch : Natural;
      PLLSAI_N      : UInt9;
      PLLSAI_R      : UInt3;
      DivR          : Natural;
      Orientation   : HAL.Framebuffer.Display_Orientation := Default;
      Mode          : HAL.Framebuffer.Wait_Mode := Interrupt)
   is
   begin
      Display.Width := Width;
      Display.Height := Height;

      if (Width > Height and then Orientation = Portrait)
        or else (Height > Width and then Orientation = Landscape)
      then
         Display.Swapped := True;
      else
         Display.Swapped := False;
      end if;

      STM32.LTDC.Initialize
        (Width         => Width,
         Height        => Height,
         H_Sync        => H_Sync,
         H_Back_Porch  => H_Back_Porch,
         H_Front_Porch => H_Front_Porch,
         V_Sync        => V_Sync,
         V_Back_Porch  => V_Back_Porch,
         V_Front_Porch => V_Front_Porch,
         PLLSAI_N      => PLLSAI_N,
         PLLSAI_R      => PLLSAI_R,
         DivR          => DivR);
      STM32.SDRAM.Initialize;

      case Mode is
         when Polling =>
            STM32.DMA2D.Polling.Initialize;
         when Interrupt =>
            STM32.DMA2D.Interrupt.Initialize;
      end case;
   end Initialize;

   ---------------------
   -- Set_Orientation --
   ---------------------

   overriding procedure Set_Orientation
     (Display     : in out Frame_Buffer;
      Orientation : HAL.Framebuffer.Display_Orientation)
   is
      Old : constant Boolean := Display.Swapped;
      Tmp : Natural;
      use STM32.DMA2D_Bitmap;
   begin
      if (Display.Width > Display.Height and then Orientation = Portrait)
        or else
          (Display.Height > Display.Width and then Orientation = Landscape)
      then
         Display.Swapped := True;
      else
         Display.Swapped := False;
      end if;

      if Old = Display.Swapped then
         return;
      end if;

      for Layer in STM32.LTDC.LCD_Layer loop
         for Buf in 1 .. 2 loop
            if Display.Buffers (Layer, Buf) /= Null_Buffer then
               Display.Buffers (Layer, Buf).Swapped := Display.Swapped;
               Tmp := Display.Buffers (Layer, Buf).Width;
               Display.Buffers (Layer, Buf).Width :=
                 Display.Buffers (Layer, Buf).Height;
               Display.Buffers (Layer, Buf).Height := Tmp;
               Display.Buffers (Layer, Buf).Fill (0);
            end if;
         end loop;
      end loop;
   end Set_Orientation;

   --------------
   -- Set_Mode --
   --------------

   overriding procedure Set_Mode
     (Display : in out Frame_Buffer;
      Mode    : HAL.Framebuffer.Wait_Mode)
   is
      pragma Unreferenced (Display);
   begin
      case Mode is
         when Polling =>
            STM32.DMA2D.Polling.Initialize;
         when Interrupt =>
            STM32.DMA2D.Interrupt.Initialize;
      end case;
   end Set_Mode;

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

   --------------------
   -- Get_Max_Layers --
   --------------------

   overriding function Get_Max_Layers
     (Display : Frame_Buffer)
      return Positive
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
     (Display : Frame_Buffer)
      return Positive
   is
   begin
      if not Display.Swapped then
         return Display.Width;
      else
         return Display.Height;
      end if;
   end Get_Width;

   ----------------
   -- Get_Height --
   ----------------

   overriding function Get_Height
     (Display : Frame_Buffer)
      return Positive
   is
   begin
      if not Display.Swapped then
         return Display.Height;
      else
         return Display.Width;
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
         if Height = Positive'Last then
            W := Display.Width;
         else
            W := Height;
         end if;

         if Width = Positive'Last then
            H  := Display.Height;
         else
            H  := Width;
         end if;

         X0 := Y;
         Y0 := Display.Height - X - H;
      end if;

      if X0 >= Display.Width then
         raise Constraint_Error with "Layer X position outside of screen";
      elsif Y0 >= Display.Height then
         raise Constraint_Error with "Layer Y position outside of screen";
      end if;

      if W = Positive'Last or else X0 + W > Display.Width then
         W := Display.Width - X0;
      end if;

      if H = Positive'Last or else Y0 + H > Display.Height then
         H := Display.Height - Y0;
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
         STM32.DMA2D.DMA2D_Wait_Transfer;
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

   -------------------
   -- Update_Layers --
   -------------------

   procedure Update_Layers
     (Display     : in out Frame_Buffer;
      Copy_Layer1 : Boolean;
      Copy_Layer2 : Boolean)
   is
      use type STM32.LTDC.LCD_Layer;
      Visible, Hidden : STM32.DMA2D_Bitmap.DMA2D_Bitmap_Buffer;
   begin
      for J in 1 .. 2 loop
         if Display.Initialized (J) then
            Internal_Update_Layer (Display, J);
         end if;
      end loop;

      STM32.LTDC.Reload_Config (Immediate => False);

      for LCD_Layer in STM32.LTDC.LCD_Layer'Range loop
         if (LCD_Layer = STM32.LTDC.Layer1 and then Copy_Layer1)
           or else (LCD_Layer = STM32.LTDC.Layer2 and then Copy_Layer2)
         then
            if Display.Current (LCD_Layer) = 1 then
               Visible := Display.Buffers (LCD_Layer, 1);
               Hidden  := Display.Buffers (LCD_Layer, 2);
            else
               Visible := Display.Buffers (LCD_Layer, 2);
               Hidden  := Display.Buffers (LCD_Layer, 1);
            end if;

            STM32.DMA2D_Bitmap.Copy_Rect
              (Visible, 0, 0, Hidden, 0, 0, Visible.Width, Visible.Height);
            STM32.DMA2D.DMA2D_Wait_Transfer;
         end if;
      end loop;
   end Update_Layers;

   --------------------
   -- Get_Color_Mode --
   --------------------

   overriding function Get_Color_Mode
     (Display : Frame_Buffer;
      Layer   : Positive)
      return HAL.Framebuffer.FB_Color_Mode
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
      Layer   : Positive)
      return HAL.Bitmap.Bitmap_Buffer'Class
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

end Framebuffer_LTDC;
