with HAL;             use HAL;
with HAL.Framebuffer; use HAL.Framebuffer;
with HAL.Bitmap;

private with STM32.DMA2D_Bitmap;
private with STM32.LTDC;

package Framebuffer_LTDC is

   type Frame_Buffer is abstract limited
     new HAL.Framebuffer.Frame_Buffer_Display with private;

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
      Mode          : HAL.Framebuffer.Wait_Mode := Interrupt);

   overriding procedure Set_Orientation
     (Display     : in out Frame_Buffer;
      Orientation : HAL.Framebuffer.Display_Orientation);

   overriding procedure Set_Mode
     (Display : in out Frame_Buffer;
      Mode    : HAL.Framebuffer.Wait_Mode);

   overriding function Initialized
     (Display : Frame_Buffer) return Boolean;

   overriding function Get_Max_Layers
     (Display : Frame_Buffer) return Positive;

   overriding function Is_Supported
     (Display : Frame_Buffer;
      Mode    : HAL.Framebuffer.FB_Color_Mode) return Boolean;

   overriding function Get_Width
     (Display : Frame_Buffer) return Positive
     with Pre => Display.Initialized;

   overriding function Get_Height
     (Display : Frame_Buffer) return Positive
     with Pre => Display.Initialized;

   overriding function Is_Swapped
     (Display : Frame_Buffer) return Boolean;

   overriding procedure Set_Background
     (Display : Frame_Buffer; R, G, B : Byte)
     with Pre => Display.Initialized;

   overriding procedure Initialize_Layer
     (Display : in out Frame_Buffer;
      Layer   : Positive;
      Mode    : HAL.Framebuffer.FB_Color_Mode;
      X       : Natural := 0;
      Y       : Natural := 0;
      Width   : Positive := Positive'Last;
      Height  : Positive := Positive'Last)
     with Pre =>
       Initialized (Display) and then
       Is_Supported (Display, Mode) and then
       not Initialized (Display, Layer) and then
       Layer <= Get_Max_Layers (Display);
   --  All layers are double buffered, so an explicit call to Update_Layer
   --  needs to be performed to actually display the current buffer attached
   --  to the layer.
   --  Alloc is called to create the actual buffer.

   overriding function Initialized
     (Display : Frame_Buffer;
      Layer   : Positive) return Boolean
     with Pre =>
       Initialized (Display) and then
       Layer <= Get_Max_Layers (Display);

   overriding procedure Update_Layer
     (Display   : in out Frame_Buffer;
      Layer     : Positive;
      Copy_Back : Boolean := False)
     with Pre => Initialized (Display, Layer);
   --  Updates the layer so that the hidden buffer is displayed.
   --  If Copy_Back is set, then the newly displayed buffer will be copied back
   --  the the hidden buffer

   overriding procedure Update_Layers
     (Display : in out Frame_Buffer)
     with Pre => Initialized (Display);
   --  Updates all initialized layers at once with their respective hidden
   --  buffer

   procedure Update_Layers
     (Display     : in out Frame_Buffer;
      Copy_Layer1 : Boolean;
      Copy_Layer2 : Boolean)
     with Pre => Initialized (Display);
   --  Updates all initialized layers at once with their respective hidden
   --  buffer

   overriding function Get_Color_Mode
     (Display : Frame_Buffer;
      Layer   : Positive) return HAL.Framebuffer.FB_Color_Mode
     with Pre => Initialized (Display, Layer);

   overriding function Get_Hidden_Buffer
     (Display : Frame_Buffer;
      Layer   : Positive) return HAL.Bitmap.Bitmap_Buffer'Class
     with Pre => Initialized (Display, Layer);
   --  Retrieves the current hidden buffer for the layer.

   overriding function Get_Pixel_Size
     (Display : Frame_Buffer;
      Layer   : Positive) return Positive
     with Pre => Initialized (Display, Layer);

private

   type FB_Array is array (STM32.LTDC.LCD_Layer, 1 .. 2) of
     STM32.DMA2D_Bitmap.DMA2D_Bitmap_Buffer;
   type Buffer_Idx is range 0 .. 2;
   type FB_Current is array (STM32.LTDC.LCD_Layer) of Buffer_Idx;

   type Frame_Buffer is abstract limited
   new HAL.Framebuffer.Frame_Buffer_Display with record
      Swapped : Boolean := False;
      Width   : Natural;
      Height  : Natural;
      Buffers : FB_Array :=
                  (others => (others => STM32.DMA2D_Bitmap.Null_Buffer));
      Current : FB_Current := (others => 0);
   end record;

end Framebuffer_LTDC;
