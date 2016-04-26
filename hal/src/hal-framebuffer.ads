with HAL.Bitmap;

package HAL.Framebuffer is

   subtype FB_Color_Mode is HAL.Bitmap.Bitmap_Color_Mode range
     HAL.Bitmap.ARGB_8888 .. HAL.Bitmap.AL_88;

   type Display_Orientation is
     (Default, Landscape, Portrait);

   type Wait_Mode is (Polling, Interrupt);

   type Frame_Buffer_Display is limited interface;
   type Frame_Buffer_Display_Access is access all Frame_Buffer_Display'Class;

   function Get_Max_Layers
     (Display : Frame_Buffer_Display) return Positive
      is abstract;

   function Is_Supported
     (Display : Frame_Buffer_Display;
      Mode    : FB_Color_Mode) return Boolean
      is abstract;

   procedure Initialize
     (Display     : in out Frame_Buffer_Display;
      Orientation : Display_Orientation := Default;
      Mode        : Wait_Mode := Interrupt) is abstract;

   procedure Set_Orientation
     (Display     : in out Frame_Buffer_Display;
      Orientation : Display_Orientation) is abstract;

   procedure Set_Mode
     (Display : in out Frame_Buffer_Display;
      Mode    : Wait_Mode) is abstract;

   function Initialized
     (Display : Frame_Buffer_Display) return Boolean is abstract;

   function Get_Width
     (Display : Frame_Buffer_Display) return Positive is abstract;

   function Get_Height
     (Display : Frame_Buffer_Display) return Positive is abstract;

   function Is_Swapped
     (Display : Frame_Buffer_Display) return Boolean is abstract;
   --  Whether X/Y coordinates are considered Swapped by the drawing primitives
   --  This simulates Landscape/Portrait orientation on displays not supporting
   --  hardware orientation change

   procedure Set_Background
     (Display : Frame_Buffer_Display; R, G, B : Byte) is abstract;

   procedure Initialize_Layer
     (Display : in out Frame_Buffer_Display;
      Layer   : Positive;
      Mode    : FB_Color_Mode;
      X       : Natural := 0;
      Y       : Natural := 0;
      Width   : Positive := Positive'Last;
      Height  : Positive := Positive'Last) is abstract;
   --  All layers are double buffered, so an explicit call to Update_Layer
   --  needs to be performed to actually display the current buffer attached
   --  to the layer.
   --  Alloc is called to create the actual buffer.

   function Initialized
     (Display : Frame_Buffer_Display;
      Layer   : Positive) return Boolean is abstract;

   procedure Update_Layer
     (Display   : in out Frame_Buffer_Display;
      Layer     : Positive;
      Copy_Back : Boolean := False) is abstract;
   --  Updates the layer so that the hidden buffer is displayed.

   procedure Update_Layers
     (Display : in out Frame_Buffer_Display) is abstract;
   --  Updates all initialized layers at once with their respective hidden
   --  buffer

   function Get_Color_Mode
     (Display : Frame_Buffer_Display;
      Layer   : Positive) return FB_Color_Mode is abstract;
   --  Retrieves the current color mode for the layer.

   function Get_Hidden_Buffer
     (Display : Frame_Buffer_Display;
      Layer   : Positive) return HAL.Bitmap.Bitmap_Buffer'Class is abstract;
   --  Retrieves the current hidden buffer for the layer.

   function Get_Pixel_Size
     (Display : Frame_Buffer_Display;
      Layer   : Positive) return Positive is abstract;
   --  Retrieves the current hidden buffer for the layer.

end HAL.Framebuffer;
