------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
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

with Ada.Real_Time;        use Ada.Real_Time;
with Ada.Unchecked_Conversion;
with Ada.Interrupts.Names;

with Cortex_M.Cache;

with STM32.Device;         use STM32.Device;
with STM32.DMA2D.Interrupt;
with STM32.DMA2D.Polling;
with STM32.GPIO;           use STM32.GPIO;
with STM32.DSI;            use STM32.DSI;
with STM32.SDRAM;          use STM32.SDRAM;

with STM32_SVD.DSI;        use STM32_SVD.DSI;
with STM32_SVD.RCC;        use STM32_SVD.RCC;
with STM32_SVD.LTDC;       use STM32_SVD.LTDC;

package body Framebuffer_DSI is

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

   procedure LCD_Reset (Reset : in out GPIO_Point);
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
         if DSI_Periph.DSI_WISR.ERIF then
            DSI_Periph.DSI_WIFCR.CERIF := True;
            End_Of_Refresh_Callback;
         end if;

         if DSI_Periph.DSI_WISR.TEIF then
            DSI_Periph.DSI_WIFCR.CTEIF := True;
            Tearing_Effect_Callback;
         end if;
      end Interrupt;

   end Sync;

   ---------------
   -- LCD_Reset --
   ---------------

   procedure LCD_Reset (Reset : in out GPIO_Point) is
   begin
      Enable_Clock (Reset);
      Configure_IO (Reset,
                    (Mode        => Mode_Out,
                     Output_Type => Push_Pull,
                     Speed       => Speed_50MHz,
                     Resistors   => Pull_Up));

      --  Activate XRES active low
      Clear (Reset);

      delay until Clock + Milliseconds (20);

      Set (Reset);

      delay until Clock + Milliseconds (10);
   end LCD_Reset;

   ----------------
   -- Max_Layers --
   ----------------

   overriding function Max_Layers
     (Display : Frame_Buffer) return Positive
   is
      pragma Unreferenced (Display);
   begin
      return 2;
   end Max_Layers;

   ---------------
   -- Supported --
   ---------------

   overriding function Supported
     (Display : Frame_Buffer;
      Mode    : HAL.Framebuffer.FB_Color_Mode) return Boolean
   is
      pragma Unreferenced (Display, Mode);
   begin
      --  The LTDC supports all HAL color modes
      return True;
   end Supported;

   -----------
   -- Width --
   -----------

   overriding function Width
     (Display : Frame_Buffer) return Positive
   is
   begin
      if not Display.Swapped then
         return LCD_Natural_Width;
      else
         return LCD_Natural_Height;
      end if;
   end Width;

   ------------
   -- Height --
   ------------

   overriding function Height
     (Display : Frame_Buffer) return Positive
   is
   begin
      if not Display.Swapped then
         return LCD_Natural_Height;
      else
         return LCD_Natural_Width;
      end if;
   end Height;

   -------------
   -- Swapped --
   -------------

   overriding function Swapped
     (Display : Frame_Buffer) return Boolean
   is
      pragma Unreferenced (Display);
   begin
      --  The display supports natively the swap of the X/Y coordinates.
      --  So to the outside world (e.g. bitmap operations), the buffer always
      --  has to be treated as 'native orientation'
      return False;
   end Swapped;

   --------------------
   -- Set_Background --
   --------------------

   overriding procedure Set_Background
     (Display : Frame_Buffer; R, G, B : UInt8)
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
         PLL_N_Div                => Display.PLL_N_Div,
         PLL_IN_Div               => Display.PLL_IN_Div,
         PLL_OUT_Div              => Display.PLL_OUT_Div);

      DSIHOST.DSI_Setup_Adapted_Command_Mode
        (Virtual_Channel         => LCD_Channel,
         Color_Coding            => STM32.DSI.RGB888,
         Command_Size            => UInt16 (Display.Width),
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
        (Width         => Display.Width,
         Height        => Display.Height,
         H_Sync        => 2,
         H_Back_Porch  => 1,
         H_Front_Porch => 1,
         V_Sync        => 2,
         V_Back_Porch  => 1,
         V_Front_Porch => 1,
         PLLSAI_N      => Display.PLLSAIN,
         PLLSAI_R      => Display.PLLSAIR,
         DivR          => Display.PLLSAI_DIVR);

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
      Mode        : HAL.Framebuffer.Wait_Mode := Interrupt;
      Reset_Point : in out GPIO_Point;
      PLLSAI_N    : UInt9;
      PLLSAI_R    : UInt3;
      DivR        : Natural;
      PLL_N_Div   : DSI_PLLN_Div;
      PLL_IN_Div  : DSI_PLL_IDF;
      PLL_OUT_Div : DSI_PLL_ODF)
   is
   begin
      LCD_Reset (Reset_Point);

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

      Display.PLLSAIN     := PLLSAI_N;
      Display.PLLSAIR     := PLLSAI_R;
      Display.PLLSAI_DIVR := DivR;
      Display.PLL_N_Div   := PLL_N_Div;
      Display.PLL_IN_Div  := PLL_IN_Div;
      Display.PLL_OUT_Div := PLL_OUT_Div;

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
      LCD_Layer : constant STM32.LTDC.LCD_Layer :=
                    (if Layer = 1
                     then STM32.LTDC.Layer1
                     else STM32.LTDC.Layer2);
      W         : Natural := Width;
      H         : Natural := Height;

   begin
      if X >= Display.Width then
         raise Constraint_Error with "Layer X position outside of screen";
      elsif Y >= Display.Height then
         raise Constraint_Error with "Layer Y position outside of screen";
      end if;

      if W = Positive'Last or else X + W > Display.Width then
         W := Display.Width - X;
      end if;

      if H = Positive'Last or else Y + H > Display.Height then
         H := Display.Height - Y;
      end if;

      Display.Buffers (LCD_Layer) :=
        (Addr              =>
           Reserve (UInt32 (HAL.Bitmap.Bits_Per_Pixel (Mode) * W * H / 8)),
         Actual_Width      => W,
         Actual_Height     => H,
         Actual_Color_Mode => Mode,
         Currently_Swapped => False,
         Native_Source     => 0);
      Display.Buffers (LCD_Layer).Fill;

      DSIHOST.DSI_Wrapper_Disable;

      STM32.LTDC.Layer_Init
        (Layer          => LCD_Layer,
         Config         => STM32.LTDC.To_LTDC_Mode (Mode),
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
      use STM32.DMA2D_Bitmap;
   begin
      STM32.DMA2D.DMA2D_Wait_Transfer;

      if Display.Buffers (STM32.LTDC.Layer1) /= Null_Buffer then
         Cortex_M.Cache.Clean_DCache
           (Start => Display.Buffers (STM32.LTDC.Layer1).Addr,
            Len   => Display.Buffers (STM32.LTDC.Layer1).Buffer_Size);
      end if;

      if Display.Buffers (STM32.LTDC.Layer2) /= Null_Buffer then
         Cortex_M.Cache.Clean_DCache
           (Start => Display.Buffers (STM32.LTDC.Layer2).Addr,
            Len   => Display.Buffers (STM32.LTDC.Layer2).Buffer_Size);
      end if;

      Sync.Do_Refresh;
      Sync.Wait;
   end Update_Layers;

   ----------------
   -- Color_Mode --
   ----------------

   overriding function Color_Mode
     (Display : Frame_Buffer;
      Layer   : Positive) return HAL.Framebuffer.FB_Color_Mode
   is
      LCD_Layer  : constant STM32.LTDC.LCD_Layer :=
                     (if Layer = 1
                      then STM32.LTDC.Layer1
                      else STM32.LTDC.Layer2);
   begin
      return Display.Buffers (LCD_Layer).Color_Mode;
   end Color_Mode;

   -------------------
   -- Hidden_Buffer --
   -------------------

   overriding function Hidden_Buffer
     (Display : in out Frame_Buffer;
      Layer   : Positive) return not null HAL.Bitmap.Any_Bitmap_Buffer
   is
      LCD_Layer  : constant STM32.LTDC.LCD_Layer :=
                     (if Layer = 1
                      then STM32.LTDC.Layer1
                      else STM32.LTDC.Layer2);
   begin
      return Display.Buffers (LCD_Layer)'Unchecked_Access;
   end Hidden_Buffer;

   ----------------
   -- Pixel_Size --
   ----------------

   overriding function Pixel_Size
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
   end Pixel_Size;

end Framebuffer_DSI;
