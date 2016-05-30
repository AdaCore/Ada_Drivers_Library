with Ada.Real_Time;        use Ada.Real_Time;
with Ada.Unchecked_Conversion;
with Ada.Interrupts.Names;

with STM32.Device;         use STM32.Device;
with STM32.DMA2D.Interrupt;
with STM32.DMA2D.Polling;
with STM32.GPIO;           use STM32.GPIO;
with STM32.DSI;            use STM32.DSI;
with STM32.SDRAM;          use STM32.SDRAM;

with STM32_SVD.DSIHOST;    use STM32_SVD.DSIHOST;
with STM32_SVD.RCC;        use STM32_SVD.RCC;
with STM32_SVD.LTDC;       use STM32_SVD.LTDC;

package body Framebuffer_OTM8009A is

   LCD_XRES    : GPIO_Point renames PH7;

   PLLSAIN     : constant := 417;
   PLLSAIR     : constant := 5;
   PLLSAI_DIVR : constant := 2;

   protected Sync is
      entry Wait;
      procedure Do_Refresh;

   private
      procedure End_Of_Refresh_Callback;
      procedure Tearing_Effect_Callback;
      procedure Interrupt
        with Attach_Handler => Ada.Interrupts.Names.DSI_Interrupt,
             Unreferenced;

      Refreshed   : Boolean := True;
   end Sync;

   procedure LCD_Reset;
   procedure Initialize_Device (Display : in out Frame_Buffer);

   ----------
   -- Sync --
   ----------

   protected body Sync is
      ----------
      -- Wait --
      ----------

      entry Wait when Refreshed is
      begin
         null;
      end Wait;

      ----------------
      -- Do_Refresh --
      ----------------

      procedure Do_Refresh is
      begin
         Refreshed := False;
         --  Enable the end of refresh interrupt
         DSIHOST.DSI_Refresh;
      end Do_Refresh;

      -----------------------------
      -- End_Of_Refresh_Callback --
      -----------------------------

      procedure End_Of_Refresh_Callback
      is
      begin
         Refreshed := True;
      end End_Of_Refresh_Callback;

      -----------------------------
      -- Tearing_Effect_Callback --
      -----------------------------

      procedure Tearing_Effect_Callback
      is
      begin
         null;
      end Tearing_Effect_Callback;

      ---------------
      -- Interrupt --
      ---------------

      procedure Interrupt
      is
      begin
         if DSIHOST_Periph.DSI_WISR.ERIF then
            DSIHOST_Periph.DSI_WIFCR.CERIF := True;
            End_Of_Refresh_Callback;
         end if;

         if DSIHOST_Periph.DSI_WISR.TEIF then
            DSIHOST_Periph.DSI_WIFCR.CTEIF := True;
            Tearing_Effect_Callback;
         end if;
      end Interrupt;

   end Sync;

   ---------------
   -- LCD_Reset --
   ---------------

   procedure LCD_Reset is
   begin
      Enable_Clock (LCD_XRES);
      Configure_IO (LCD_XRES,
                    (Mode        => Mode_Out,
                     Output_Type => Open_Drain,
                     Speed       => Speed_50MHz,
                     Resistors   => Floating));

      --  Activate XRES active low
      Clear (LCD_XRES);

      delay until Clock + Microseconds (20);

      Set (LCD_XRES);

      delay until Clock + Microseconds (10);
   end LCD_Reset;

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
         return LCD_Natural_Width;
      else
         return LCD_Natural_Height;
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
         return LCD_Natural_Height;
      else
         return LCD_Natural_Width;
      end if;
   end Get_Height;

   ----------------
   -- Is_Swapped --
   ----------------

   overriding function Is_Swapped
     (Display : Frame_Buffer) return Boolean
   is
      pragma Unreferenced (Display);
   begin
      --  The display supports natively the swap of the X/Y coordinates.
      --  So to the outside world (e.g. bitmap operations), the buffer always
      --  has to be treated as 'native orientation'
      return False;
   end Is_Swapped;

   --------------------
   -- Set_Background --
   --------------------

   overriding procedure Set_Background
     (Display : Frame_Buffer; R, G, B : Byte)
   is
      pragma Unreferenced (Display);
   begin
      DSIHOST.DSI_Wrapper_Disable;
      STM32.LTDC.Set_Background (R, G, B);
      DSIHOST.DSI_Wrapper_Enable;
   end Set_Background;

   -----------------------
   -- Initialize_Device --
   -----------------------

   procedure Initialize_Device (Display : in out Frame_Buffer)
   is
   begin

      DSIHOST.DSI_Deinit;

      --  HSE input: 25MHz / IN_Div * N_Div => 1000 MHz = VCO
      --  VCO / ODF => 500 MHz
      DSIHOST.DSI_Initialize
        (Auto_Clock_Lane_Control  => True,
         TX_Escape_Clock_Division => 4, -- 62500 / 4 = 15625 kHz < 20kHz (max)
         Number_Of_Lanes          => Two_Data_Lanes,
         PLL_N_Div                => 125,
         PLL_IN_Div               => PLL_IN_DIV2,
         PLL_OUT_Div              => PLL_OUT_DIV1);

      DSIHOST.DSI_Setup_Adapted_Command_Mode
        (Virtual_Channel         => LCD_Channel,
         Color_Coding            => STM32.DSI.RGB888,
         Command_Size            => Short (Display.Get_Width),
         Tearing_Effect_Source   => STM32.DSI.TE_DSI_Link,
         Tearing_Effect_Polarity => STM32.DSI.Rising_Edge,
         HSync_Polarity          => STM32.DSI.Active_High,
         VSync_Polarity          => STM32.DSI.Active_High,
         DataEn_Polarity         => STM32.DSI.Active_High,
         VSync_Edge              => STM32.DSI.Falling_Edge,
         Automatic_Refresh       => False,
         TE_Acknowledge_Request  => True);

      DSIHOST.DSI_Setup_Command
        (LP_Gen_Short_Write_No_P  => True,
         LP_Gen_Short_Write_One_P => True,
         LP_Gen_Short_Write_Two_P => True,
         LP_Gen_Short_Read_No_P   => True,
         LP_Gen_Short_Read_One_P  => True,
         LP_Gen_Short_Read_Two_P  => True,
         LP_Gen_Long_Write        => True,
         LP_DCS_Short_Write_No_P  => True,
         LP_DCS_Short_Write_One_P => True,
         LP_DCS_Short_Read_No_P   => True,
         LP_DCS_Long_Write        => True,
         LP_Max_Read_Packet       => True,
         Acknowledge_Request      => False);

      STM32.LTDC.Initialize
        (Width         => Display.Get_Width,
         Height        => Display.Get_Height,
         H_Sync        => 2,
         H_Back_Porch  => 1,
         H_Front_Porch => 1,
         V_Sync        => 2,
         V_Back_Porch  => 1,
         V_Front_Porch => 1,
         PLLSAI_N      => PLLSAIN,
         PLLSAI_R      => PLLSAIR,
         DivR          => PLLSAI_DIVR);

      --  Enable the DSI Host and Wrapper
      DSIHOST.DSI_Start;

      --  LCD panel init
      Display.Device.Initialize
        (OTM8009A.RGB888,
         (if Display.Swapped then OTM8009A.Portrait else OTM8009A.Landscape));

      DSIHOST.DSI_Setup_Command
        (LP_Gen_Short_Write_No_P  => False,
         LP_Gen_Short_Write_One_P => False,
         LP_Gen_Short_Write_Two_P => False,
         LP_Gen_Short_Read_No_P   => False,
         LP_Gen_Short_Read_One_P  => False,
         LP_Gen_Short_Read_Two_P  => False,
         LP_Gen_Long_Write        => False,
         LP_DCS_Short_Write_No_P  => False,
         LP_DCS_Short_Write_One_P => False,
         LP_DCS_Short_Read_No_P   => False,
         LP_DCS_Long_Write        => False,
         LP_Max_Read_Packet       => False,
         Acknowledge_Request      => False);

      DSIHOST.DSI_Setup_Flow_Control (Flow_Control_BTA);

      DSIHOST.DSI_Refresh;
   end Initialize_Device;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Display     : in out Frame_Buffer;
      Orientation : HAL.Framebuffer.Display_Orientation := Default;
      Mode        : HAL.Framebuffer.Wait_Mode := Interrupt)
   is
   begin
      LCD_Reset;

      --  Init clocks on DSI, LTDC and DMA2D
      RCC_Periph.APB2ENR.LTDCEN := True;
      RCC_Periph.APB2RSTR.LTDCRST := True;
      RCC_Periph.APB2RSTR.LTDCRST := False;

      RCC_Periph.AHB1ENR.DMA2DEN := True;
      RCC_Periph.AHB1RSTR.DMA2DRST := True;
      RCC_Periph.AHB1RSTR.DMA2DRST := False;

      RCC_Periph.APB2ENR.DSIEN := True;
      RCC_Periph.APB2RSTR.DSIRST := True;
      RCC_Periph.APB2RSTR.DSIRST := False;

      --  Make sure the SDRAM is enabled
      STM32.SDRAM.Initialize;

      if Orientation = Portrait then
         Display.Swapped := True;
      end if;

      Display.Initialize_Device;

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

   ---------------------
   -- Set_Orientation --
   ---------------------

   overriding procedure Set_Orientation
     (Display     : in out Frame_Buffer;
      Orientation : HAL.Framebuffer.Display_Orientation)
   is
      Old : constant Boolean := Display.Swapped;
   begin
      Display.Swapped := Orientation = Portrait;

      if Old = Display.Swapped then
         return;
      end if;

      Initialize_Device (Display);
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

   begin
      if X >= Display.Get_Width then
         raise Constraint_Error with "Layer X position outside of screen";
      elsif Y >= Display.Get_Height then
         raise Constraint_Error with "Layer Y position outside of screen";
      end if;

      if W = Positive'Last or else X + W > Display.Get_Width then
         W := Display.Get_Width - X;
      end if;

      if H = Positive'Last or else Y + H > Display.Get_Height then
         H := Display.Get_Height - Y;
      end if;

      Display.Buffers (LCD_Layer) :=
        (Addr       =>
           Reserve (Word (HAL.Bitmap.Bits_Per_Pixel (Mode) * W * H / 8)),
         Width      => W,
         Height     => H,
         Color_Mode => Mode,
         Swapped    => False);
      Display.Buffers (LCD_Layer).Fill (0);

      DSIHOST.DSI_Wrapper_Disable;

      STM32.LTDC.Layer_Init
        (Layer          => LCD_Layer,
         Config         => To_LTDC_Mode (Mode),
         Buffer         => Display.Buffers (LCD_Layer).Addr,
         X              => X,
         Y              => Y,
         W              => W,
         H              => H,
         Constant_Alpha => 255,
         BF             => STM32.LTDC.BF_Pixel_Alpha_X_Constant_Alpha);
      STM32.LTDC.Reload_Config (True);

      DSIHOST.DSI_Wrapper_Enable;

      Display.Update_Layers;
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
        Display.Buffers (LCD_Layer) /= STM32.DMA2D_Bitmap.Null_Buffer;
   end Initialized;

   ------------------
   -- Update_Layer --
   ------------------

   overriding procedure Update_Layer
     (Display   : in out Frame_Buffer;
      Layer     : Positive;
      Copy_Back : Boolean := False)
   is
      pragma Unreferenced (Layer, Copy_Back);
   begin
      Display.Update_Layers;
   end Update_Layer;

   -------------------
   -- Update_Layers --
   -------------------

   overriding procedure Update_Layers
     (Display : in out Frame_Buffer)
   is
      pragma Unreferenced (Display);
   begin
      STM32.DMA2D.DMA2D_Wait_Transfer;
      Sync.Do_Refresh;
      Sync.Wait;
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
      return Display.Buffers (LCD_Layer).Color_Mode;
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
      return Display.Buffers (LCD_Layer);
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
          (Display.Buffers (LCD_Layer).Color_Mode) / 8;
   end Get_Pixel_Size;

end Framebuffer_OTM8009A;
