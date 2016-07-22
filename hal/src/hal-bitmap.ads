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
     (Src_Buffer  : Bitmap_Buffer'Class;
      X_Src       : Natural;
      Y_Src       : Natural;
      Dst_Buffer  : Bitmap_Buffer;
      X_Dst       : Natural;
      Y_Dst       : Natural;
      Bg_Buffer   : Bitmap_Buffer'Class;
      X_Bg        : Natural;
      Y_Bg        : Natural;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean);

   procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      X_Src       : Natural;
      Y_Src       : Natural;
      Dst_Buffer  : Bitmap_Buffer;
      X_Dst       : Natural;
      Y_Dst       : Natural;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean);

   procedure Copy_Rect_Blend
     (Src_Buffer  : Bitmap_Buffer;
      X_Src       : Natural;
      Y_Src       : Natural;
      Dst_Buffer  : Bitmap_Buffer'Class;
      X_Dst       : Natural;
      Y_Dst       : Natural;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean);

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

   Transparent         : constant Bitmap_Color := (000, 000, 000, 000);
   Dark_Red            : constant Bitmap_Color := (255, 139, 000, 000);
   Brown               : constant Bitmap_Color := (255, 165, 042, 042);
   Firebrick           : constant Bitmap_Color := (255, 178, 034, 034);
   Crimson             : constant Bitmap_Color := (255, 220, 020, 060);
   Red                 : constant Bitmap_Color := (255, 255, 000, 000);
   Tomato              : constant Bitmap_Color := (255, 255, 099, 071);
   Coral               : constant Bitmap_Color := (255, 255, 127, 080);
   Indian_Red          : constant Bitmap_Color := (255, 205, 092, 092);
   Light_Coral         : constant Bitmap_Color := (255, 240, 128, 128);
   Dark_Salmon         : constant Bitmap_Color := (255, 233, 150, 122);
   Salmon              : constant Bitmap_Color := (255, 250, 128, 114);
   Light_Salmon        : constant Bitmap_Color := (255, 255, 160, 122);
   Dark_Orange         : constant Bitmap_Color := (255, 255, 140, 000);
   Orange              : constant Bitmap_Color := (255, 255, 165, 000);
   Light_Orange        : constant Bitmap_Color := (255, 255, 069, 000);
   Gold                : constant Bitmap_Color := (255, 255, 215, 000);
   Dark_Golden_Rod     : constant Bitmap_Color := (255, 184, 134, 011);
   Golden_Rod          : constant Bitmap_Color := (255, 218, 165, 032);
   Pale_Golden_Rod     : constant Bitmap_Color := (255, 238, 232, 170);
   Dark_Khaki          : constant Bitmap_Color := (255, 189, 183, 107);
   Khaki               : constant Bitmap_Color := (255, 240, 230, 140);
   Olive               : constant Bitmap_Color := (255, 128, 128, 000);
   Yellow              : constant Bitmap_Color := (255, 255, 255, 000);
   Yellow_Green        : constant Bitmap_Color := (255, 154, 205, 050);
   Dark_Olive_Green    : constant Bitmap_Color := (255, 085, 107, 047);
   Olive_Drab          : constant Bitmap_Color := (255, 107, 142, 035);
   Lawn_Green          : constant Bitmap_Color := (255, 124, 252, 000);
   Chart_Reuse         : constant Bitmap_Color := (255, 127, 255, 000);
   Green_Yellow        : constant Bitmap_Color := (255, 173, 255, 047);
   Dark_Green          : constant Bitmap_Color := (255, 000, 100, 000);
   Green               : constant Bitmap_Color := (255, 000, 255, 000);
   Maroon              : constant Bitmap_Color := (255, 128, 000, 000);
   Forest_Green        : constant Bitmap_Color := (255, 034, 139, 034);
   Lime                : constant Bitmap_Color := (255, 000, 255, 000);
   Lime_Green          : constant Bitmap_Color := (255, 050, 205, 050);
   Light_Green         : constant Bitmap_Color := (255, 144, 238, 144);
   Pale_Green          : constant Bitmap_Color := (255, 152, 251, 152);
   Dark_Sea_Green      : constant Bitmap_Color := (255, 143, 188, 143);
   Medium_Spring_Green : constant Bitmap_Color := (255, 000, 250, 154);
   Spring_Green        : constant Bitmap_Color := (255, 000, 255, 127);
   Sea_Green           : constant Bitmap_Color := (255, 046, 139, 087);
   Medium_Aqua_Marine  : constant Bitmap_Color := (255, 102, 205, 170);
   Medium_Sea_Green    : constant Bitmap_Color := (255, 060, 179, 113);
   Light_Sea_Green     : constant Bitmap_Color := (255, 032, 178, 170);
   Dark_Slate_Gray     : constant Bitmap_Color := (255, 047, 079, 079);
   Teal                : constant Bitmap_Color := (255, 000, 128, 128);
   Dark_Cyan           : constant Bitmap_Color := (255, 000, 139, 139);
   Aqua                : constant Bitmap_Color := (255, 000, 255, 255);
   Cyan                : constant Bitmap_Color := (255, 000, 255, 255);
   Light_Cyan          : constant Bitmap_Color := (255, 224, 255, 255);
   Dark_Turquoise      : constant Bitmap_Color := (255, 000, 206, 209);
   Turquoise           : constant Bitmap_Color := (255, 064, 224, 208);
   Medium_Turquoise    : constant Bitmap_Color := (255, 072, 209, 204);
   Pale_Turquoise      : constant Bitmap_Color := (255, 175, 238, 238);
   Aqua_Marine         : constant Bitmap_Color := (255, 127, 255, 212);
   Powder_Blue         : constant Bitmap_Color := (255, 176, 224, 230);
   Cadet_Blue          : constant Bitmap_Color := (255, 095, 158, 160);
   Steel_Blue          : constant Bitmap_Color := (255, 070, 130, 180);
   Corn_Flower_Blue    : constant Bitmap_Color := (255, 100, 149, 237);
   Deep_Sky_Blue       : constant Bitmap_Color := (255, 000, 191, 255);
   Dodger_Blue         : constant Bitmap_Color := (255, 030, 144, 255);
   Light_Blue          : constant Bitmap_Color := (255, 173, 216, 230);
   Sky_Blue            : constant Bitmap_Color := (255, 135, 206, 235);
   Light_Sky_Blue      : constant Bitmap_Color := (255, 135, 206, 250);
   Midnight_Blue       : constant Bitmap_Color := (255, 025, 025, 112);
   Navy                : constant Bitmap_Color := (255, 000, 000, 128);
   Dark_Blue           : constant Bitmap_Color := (255, 000, 000, 139);
   Medium_Blue         : constant Bitmap_Color := (255, 000, 000, 205);
   Blue                : constant Bitmap_Color := (255, 000, 000, 255);
   Royal_Blue          : constant Bitmap_Color := (255, 065, 105, 225);
   Blue_Violet         : constant Bitmap_Color := (255, 138, 043, 226);
   Indigo              : constant Bitmap_Color := (255, 075, 000, 130);
   Dark_Slate_Blue     : constant Bitmap_Color := (255, 072, 061, 139);
   Slate_Blue          : constant Bitmap_Color := (255, 106, 090, 205);
   Medium_Slate_Blue   : constant Bitmap_Color := (255, 123, 104, 238);
   Medium_Purple       : constant Bitmap_Color := (255, 147, 112, 219);
   Dark_Magenta        : constant Bitmap_Color := (255, 139, 000, 139);
   Dark_Violet         : constant Bitmap_Color := (255, 148, 000, 211);
   Dark_Orchid         : constant Bitmap_Color := (255, 153, 050, 204);
   Medium_Orchid       : constant Bitmap_Color := (255, 186, 085, 211);
   Purple              : constant Bitmap_Color := (255, 128, 000, 128);
   Thistle             : constant Bitmap_Color := (255, 216, 191, 216);
   Plum                : constant Bitmap_Color := (255, 221, 160, 221);
   Violet              : constant Bitmap_Color := (255, 238, 130, 238);
   Magenta             : constant Bitmap_Color := (255, 255, 000, 255);
   Orchid              : constant Bitmap_Color := (255, 218, 112, 214);
   Medium_Violet_Red   : constant Bitmap_Color := (255, 199, 021, 133);
   Pale_Violet_Red     : constant Bitmap_Color := (255, 219, 112, 147);
   Deep_Pink           : constant Bitmap_Color := (255, 255, 020, 147);
   Hot_Pink            : constant Bitmap_Color := (255, 255, 105, 180);
   Light_Pink          : constant Bitmap_Color := (255, 255, 182, 193);
   Pink                : constant Bitmap_Color := (255, 255, 192, 203);
   Antique_White       : constant Bitmap_Color := (255, 250, 235, 215);
   Beige               : constant Bitmap_Color := (255, 245, 245, 220);
   Bisque              : constant Bitmap_Color := (255, 255, 228, 196);
   Blanched_Almond     : constant Bitmap_Color := (255, 255, 235, 205);
   Wheat               : constant Bitmap_Color := (255, 245, 222, 179);
   Corn_Silk           : constant Bitmap_Color := (255, 255, 248, 220);
   Lemon_Chiffon       : constant Bitmap_Color := (255, 255, 250, 205);
   Light_Yellow        : constant Bitmap_Color := (255, 255, 255, 224);
   Saddle_Brown        : constant Bitmap_Color := (255, 139, 069, 019);
   Sienna              : constant Bitmap_Color := (255, 160, 082, 045);
   Chocolate           : constant Bitmap_Color := (255, 210, 105, 030);
   Peru                : constant Bitmap_Color := (255, 205, 133, 063);
   Sandy_Brown         : constant Bitmap_Color := (255, 244, 164, 096);
   Burly_Wood          : constant Bitmap_Color := (255, 222, 184, 135);
   Tan                 : constant Bitmap_Color := (255, 210, 180, 140);
   Rosy_Brown          : constant Bitmap_Color := (255, 188, 143, 143);
   Moccasin            : constant Bitmap_Color := (255, 255, 228, 181);
   Navajo_White        : constant Bitmap_Color := (255, 255, 222, 173);
   Peach_Puff          : constant Bitmap_Color := (255, 255, 218, 185);
   Misty_Rose          : constant Bitmap_Color := (255, 255, 228, 225);
   Lavender_Blush      : constant Bitmap_Color := (255, 255, 240, 245);
   Linen               : constant Bitmap_Color := (255, 250, 240, 230);
   Old_Lace            : constant Bitmap_Color := (255, 253, 245, 230);
   Papaya_Whip         : constant Bitmap_Color := (255, 255, 239, 213);
   Sea_Shell           : constant Bitmap_Color := (255, 255, 245, 238);
   Mint_Cream          : constant Bitmap_Color := (255, 245, 255, 250);
   Slate_Gray          : constant Bitmap_Color := (255, 112, 128, 144);
   Light_Slate_Gray    : constant Bitmap_Color := (255, 119, 136, 153);
   Light_Steel_Blue    : constant Bitmap_Color := (255, 176, 196, 222);
   Lavender            : constant Bitmap_Color := (255, 230, 230, 250);
   Floral_White        : constant Bitmap_Color := (255, 255, 250, 240);
   Alice_Blue          : constant Bitmap_Color := (255, 240, 248, 255);
   Ghost_White         : constant Bitmap_Color := (255, 248, 248, 255);
   Honeydew            : constant Bitmap_Color := (255, 240, 255, 240);
   Ivory               : constant Bitmap_Color := (255, 255, 255, 240);
   Azure               : constant Bitmap_Color := (255, 240, 255, 255);
   Snow                : constant Bitmap_Color := (255, 255, 250, 250);
   Black               : constant Bitmap_Color := (255, 000, 000, 000);
   Dim_Grey            : constant Bitmap_Color := (255, 105, 105, 105);
   Grey                : constant Bitmap_Color := (255, 128, 128, 128);
   Gray                : constant Bitmap_Color := (255, 190, 190, 190);
   Dark_Grey           : constant Bitmap_Color := (255, 169, 169, 169);
   Silver              : constant Bitmap_Color := (255, 192, 192, 192);
   Light_Grey          : constant Bitmap_Color := (255, 211, 211, 211);
   Gainsboro           : constant Bitmap_Color := (255, 220, 220, 220);
   White_Smoke         : constant Bitmap_Color := (255, 245, 245, 245);
   White               : constant Bitmap_Color := (255, 255, 255, 255);

end HAL.Bitmap;
