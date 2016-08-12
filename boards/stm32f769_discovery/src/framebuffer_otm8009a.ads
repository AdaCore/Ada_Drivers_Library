with HAL;             use HAL;
with HAL.Framebuffer; use HAL.Framebuffer;
with HAL.Bitmap;

private with STM32.Device;
private with STM32.DMA2D_Bitmap;
private with STM32.LTDC;
private with OTM8009A;
private with HAL.DSI;

package Framebuffer_OTM8009A is

   LCD_Natural_Width  : constant := 800;
   LCD_Natural_Height : constant := 480;

   type Frame_Buffer is limited
     new HAL.Framebuffer.Frame_Buffer_Display with private;

   overriding function Get_Max_Layers
     (Display : Frame_Buffer) return Positive;

   overriding function Is_Supported
     (Display : Frame_Buffer;
      Mode    : HAL.Framebuffer.FB_Color_Mode) return Boolean;

   procedure Initialize
     (Display     : in out Frame_Buffer;
      Orientation : HAL.Framebuffer.Display_Orientation := Default;
      Mode        : HAL.Framebuffer.Wait_Mode := Interrupt);

   overriding function Initialized
     (Display : Frame_Buffer) return Boolean;

   overriding procedure Set_Orientation
     (Display     : in out Frame_Buffer;
      Orientation : HAL.Framebuffer.Display_Orientation);

   overriding procedure Set_Mode
     (Display : in out Frame_Buffer;
      Mode    : HAL.Framebuffer.Wait_Mode);

   overriding function Get_Width
     (Display : Frame_Buffer) return Positive;

   overriding function Get_Height
     (Display : Frame_Buffer) return Positive;

   overriding function Is_Swapped
     (Display : Frame_Buffer) return Boolean;

   overriding procedure Set_Background
     (Display : Frame_Buffer; R, G, B : Byte);

   overriding procedure Initialize_Layer
     (Display : in out Frame_Buffer;
      Layer   : Positive;
      Mode    : HAL.Framebuffer.FB_Color_Mode;
      X       : Natural := 0;
      Y       : Natural := 0;
      Width   : Positive := Positive'Last;
      Height  : Positive := Positive'Last);
   --  All layers are double buffered, so an explicit call to Update_Layer
   --  needs to be performed to actually display the current buffer attached
   --  to the layer.
   --  Alloc is called to create the actual buffer.

   overriding function Initialized
     (Display : Frame_Buffer;
      Layer   : Positive) return Boolean;

   overriding procedure Update_Layer
     (Display   : in out Frame_Buffer;
      Layer     : Positive;
      Copy_Back : Boolean := False);
   --  Updates the layer so that the hidden buffer is displayed.
   --  If Copy_Back is set, then the newly displayed buffer will be copied back
   --  the the hidden buffer

   overriding procedure Update_Layers
     (Display : in out Frame_Buffer);
   --  Updates all initialized layers at once with their respective hidden
   --  buffer

   overriding function Get_Color_Mode
     (Display : Frame_Buffer;
      Layer   : Positive) return HAL.Framebuffer.FB_Color_Mode;

   overriding function Get_Hidden_Buffer
     (Display : Frame_Buffer;
      Layer   : Positive) return HAL.Bitmap.Bitmap_Buffer'Class;
   --  Retrieves the current hidden buffer for the layer.

   overriding function Get_Pixel_Size
     (Display : Frame_Buffer;
      Layer   : Positive) return Positive;

private

   type FB_Array is array (STM32.LTDC.LCD_Layer) of
     STM32.DMA2D_Bitmap.DMA2D_Bitmap_Buffer;

   LCD_Channel : constant HAL.DSI.DSI_Virtual_Channel_ID := 0;
   --  Only one display on this board, constant to 0

   type Frame_Buffer is limited new HAL.Framebuffer.Frame_Buffer_Display with
      record
         Device  : OTM8009A.OTM8009A_Device
                    (DSI_Host   => STM32.Device.DSIHOST'Access,
                     Channel_Id => LCD_Channel);
         Swapped : Boolean;
         Buffers : FB_Array := (others => STM32.DMA2D_Bitmap.Null_Buffer);
      end record;
   type Frame_Buffer_Ref is access all Frame_Buffer;

end Framebuffer_OTM8009A;
