with System;

package HAL.Bitmap is

   type Orientation_Mode is
     (Default,
      Portrait,
      Landscape);

   subtype Actual_Orientation is Orientation_Mode range Portrait .. Landscape;

   type Bitmap_Color_Mode is
     (ARGB_8888,
      RGB_888,
      RGB_565,
      ARGB_1555,
      ARGB_4444,
      L_8,
      AL_44,
      AL_88,
      L_4,
      A_8,
      A_4) with Size => 4;

   function Bits_Per_Pixel (Mode : Bitmap_Color_Mode) return Positive is
     (case Mode is
         when ARGB_8888 => 32,
         when RGB_888 => 24,
         when RGB_565 | ARGB_1555 | ARGB_4444 | AL_88 => 16,
         when L_8 | AL_44 | A_8 => 8,
         when L_4 | A_4 => 4);

   type Bitmap_Buffer is tagged record
      Addr       : System.Address;

      Width      : Natural;
      Height     : Natural;
      --  Width and Height of the buffer. Note that it's the user-visible width
      --  (see below for the meaning of the Swapped value).

      Color_Mode : Bitmap_Color_Mode;
      --  The buffer color mode. Note that not all color modes are supported by
      --  the hardware acceleration (if any), so you need to check your actual
      --  hardware to optimize buffer transfers.

      Swapped    : Boolean := False;
      --  If Swap is set, then operations on this buffer will consider:
      --  Width0 = Height
      --  Height0 = Width
      --  Y0 = Buffer.Width - X - 1
      --  X0 = Y
      --
      --  As an example, the Bitmap buffer that corresponds to a 240x320
      --  swapped display (to display images in landscape mode) with have
      --  the following values:
      --  Width => 320
      --  Height => 240
      --  Swapped => True
      --  So Put_Pixel (Buffer, 30, 10, Color) will place the pixel at
      --  Y0 = 320 - 30 - 1 = 289
      --  X0 = 10
   end record;

   type Bitmap_Color is record
      Alpha : Byte;
      Red   : Byte;
      Green : Byte;
      Blue  : Byte;
   end record with Size => 32;

   for Bitmap_Color use record
      Blue  at 0 range 0 .. 7;
      Green at 1 range 0 .. 7;
      Red   at 2 range 0 .. 7;
      Alpha at 3 range 0 .. 7;
   end record;

   Black       : constant Bitmap_Color := (255, 0, 0, 0);
   Blue        : constant Bitmap_Color := (255, 0, 0, 255);
   Light_Blue  : constant Bitmap_Color := (255, 173, 216, 230);
   Brown       : constant Bitmap_Color := (255, 165, 42, 42);
   Cyan        : constant Bitmap_Color := (255, 0, 255, 255);
   Gray        : constant Bitmap_Color := (255, 190, 190, 190);
   Light_Gray  : constant Bitmap_Color := (255, 211, 211, 211);
   Green       : constant Bitmap_Color := (255, 0, 255, 0);
   Light_Green : constant Bitmap_Color := (255, 144, 238, 144);
   Magenta     : constant Bitmap_Color := (255, 255, 0, 255);
   Red         : constant Bitmap_Color := (255, 255, 0, 0);
   Orange      : constant Bitmap_Color := (255, 255, 69, 0);
   Violet      : constant Bitmap_Color := (255, 238, 130, 238);
   Yellow      : constant Bitmap_Color := (255, 255, 255, 0);
   White       : constant Bitmap_Color := (255, 255, 255, 255);
   Transparent : constant Bitmap_Color := (0, 0, 0, 0);

   procedure Set_Pixel
     (Buffer  : Bitmap_Buffer;
      X       : Natural;
      Y       : Natural;
      Value   : Bitmap_Color);

   procedure Set_Pixel
     (Buffer  : Bitmap_Buffer;
      X       : Natural;
      Y       : Natural;
      Value   : Word);

   procedure Set_Pixel_Blend
     (Buffer : Bitmap_Buffer;
      X      : Natural;
      Y      : Natural;
      Value  : Bitmap_Color);

   function Get_Pixel
     (Buffer : Bitmap_Buffer;
      X      : Natural;
      Y      : Natural)
      return Bitmap_Color;

   function Get_Pixel
     (Buffer : Bitmap_Buffer;
      X      : Natural;
      Y      : Natural)
      return Word;

   procedure Fill
     (Buffer : Bitmap_Buffer;
      Color  : Bitmap_Color);
   --  Fill the specified buffer with 'Color'

   procedure Fill
     (Buffer : Bitmap_Buffer;
      Color  : Word);
   --  Same as above, using the destination buffer native color representation

   procedure Fill_Rect
     (Buffer : Bitmap_Buffer;
      Color  : Bitmap_Color;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer);
   --  Fill the specified area of the buffer with 'Color'

   procedure Fill_Rect
     (Buffer : Bitmap_Buffer;
      Color  : Word;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer);
   --  Same as above, using the destination buffer native color representation

   procedure Copy_Rect
     (Src_Buffer : Bitmap_Buffer'Class;
      X_Src      : Natural;
      Y_Src      : Natural;
      Dst_Buffer : Bitmap_Buffer;
      X_Dst      : Natural;
      Y_Dst      : Natural;
      Bg_Buffer  : Bitmap_Buffer'Class;
      X_Bg       : Natural;
      Y_Bg       : Natural;
      Width      : Natural;
      Height     : Natural);

   procedure Copy_Rect
     (Src_Buffer : Bitmap_Buffer;
      X_Src      : Natural;
      Y_Src      : Natural;
      Dst_Buffer : Bitmap_Buffer'Class;
      X_Dst      : Natural;
      Y_Dst      : Natural;
      Width      : Natural;
      Height     : Natural);

   procedure Copy_Rect_Blend
     (Src_Buffer : Bitmap_Buffer;
      X_Src      : Natural;
      Y_Src      : Natural;
      Dst_Buffer : Bitmap_Buffer'Class;
      X_Dst      : Natural;
      Y_Dst      : Natural;
      Width      : Natural;
      Height     : Natural);

   procedure Draw_Vertical_Line
     (Buffer : Bitmap_Buffer;
      Color  : Word;
      X      : Integer;
      Y      : Integer;
      Height : Integer);

   procedure Draw_Vertical_Line
     (Buffer : Bitmap_Buffer;
      Color  : Bitmap_Color;
      X      : Integer;
      Y      : Integer;
      Height : Integer);

   procedure Draw_Horizontal_Line
     (Buffer : Bitmap_Buffer;
      Color  : Word;
      X      : Integer;
      Y      : Integer;
      Width  : Integer);

   procedure Draw_Horizontal_Line
     (Buffer : Bitmap_Buffer;
      Color  : Bitmap_Color;
      X      : Integer;
      Y      : Integer;
      Width  : Integer);

   procedure Draw_Rect
     (Buffer : Bitmap_Buffer;
      Color  : Bitmap_Color;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer);
   --  Draws a rectangle

   function Buffer_Size (Buffer : Bitmap_Buffer) return Natural;

   function Bitmap_Color_To_Word
     (Mode : Bitmap_Color_Mode; Col : Bitmap_Color)
     return Word;
   --  Translates the DMA2D Color into native buffer color

   function Word_To_Bitmap_Color
     (Mode : Bitmap_Color_Mode; Col : Word)
     return Bitmap_Color;
   --  Translates the native buffer color into DMA2D Color

   procedure Wait_Transfer (Buffer : Bitmap_Buffer);
   --  Makes sure the DMA2D transfers are done

end HAL.Bitmap;
