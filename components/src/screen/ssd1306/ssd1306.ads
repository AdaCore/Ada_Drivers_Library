------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

--  This a driver for the SSD1306 monochrome OLED screen drivers.

with HAL;                  use HAL;
with HAL.I2C;              use HAL.I2C;
with HAL.GPIO;             use HAL.GPIO;
with HAL.Framebuffer;      use HAL.Framebuffer;
with HAL.Bitmap;           use HAL.Bitmap;
with HAL.Time;
with Memory_Mapped_Bitmap; use Memory_Mapped_Bitmap;

package SSD1306 is

   --  If your screen is of standard resolution (96x16, 128x32, 128x64), please
   --  use the definition in the package SSD1306.Standard_Resolutions.
   type SSD1306_Screen
     (Buffer_Size_In_Byte : Positive;
      --  Number of byte in the bitmap buffer: ((Width * Height) / 8)

      Width               : Positive;
      --  Width in pixel

      Height              : Positive;
      --  Height in pixel

      Port                : not null Any_I2C_Port;
      --  I2C communication port. Depending on the bus configuration, SSD1306
      --  drivers can work up to 1MHz.

      RST                 : not null Any_GPIO_Point;
      --  Screen reset line

      Time                : not null HAL.Time.Any_Delays)
   is limited new HAL.Framebuffer.Frame_Buffer_Display with private;

   type Any_SSD1306_Screen is access all SSD1306_Screen'Class;

   procedure Initialize (This         : in out SSD1306_Screen;
                         External_VCC : Boolean);

   overriding
   function Initialized (This : SSD1306_Screen) return Boolean;

   procedure Turn_On (This : SSD1306_Screen);
   procedure Turn_Off (This : SSD1306_Screen);
   procedure Display_Inversion_On (This : SSD1306_Screen);
   procedure Display_Inversion_Off (This : SSD1306_Screen);

   procedure Write_Raw_Pixels (This : SSD1306_Screen;
                               Data : HAL.UInt8_Array);

   overriding
   function Max_Layers
     (This : SSD1306_Screen) return Positive;

   overriding
   function Supported
     (This : SSD1306_Screen;
      Mode : FB_Color_Mode) return Boolean;

   overriding
   procedure Set_Orientation
     (This        : in out SSD1306_Screen;
      Orientation : Display_Orientation);

   overriding
   procedure Set_Mode
     (This : in out SSD1306_Screen;
      Mode : Wait_Mode);

   overriding
   function Width
     (This : SSD1306_Screen) return Positive;

   overriding
   function Height
     (This : SSD1306_Screen) return Positive;

   overriding
   function Swapped
     (This : SSD1306_Screen) return Boolean;
   --  Whether X/Y coordinates are considered Swapped by the drawing primitives
   --  This simulates Landscape/Portrait orientation on displays not supporting
   --  hardware orientation change

   overriding
   procedure Set_Background
     (This : SSD1306_Screen; R, G, B : UInt8);

   overriding
   procedure Initialize_Layer
     (This : in out SSD1306_Screen;
      Layer   : Positive;
      Mode    : FB_Color_Mode;
      X       : Natural := 0;
      Y       : Natural := 0;
      Width   : Positive := Positive'Last;
      Height  : Positive := Positive'Last);
   --  All layers are double buffered, so an explicit call to Update_Layer
   --  needs to be performed to actually display the current buffer attached
   --  to the layer.
   --  Alloc is called to create the actual buffer.

   overriding
   function Initialized
     (This  : SSD1306_Screen;
      Layer : Positive) return Boolean;

   overriding
   procedure Update_Layer
     (This      : in out SSD1306_Screen;
      Layer     : Positive;
      Copy_Back : Boolean := False);
   --  Updates the layer so that the hidden buffer is displayed.

   overriding
   procedure Update_Layers
     (This : in out SSD1306_Screen);
   --  Updates all initialized layers at once with their respective hidden
   --  buffer

   overriding
   function Color_Mode
     (This  : SSD1306_Screen;
      Layer : Positive) return FB_Color_Mode;
   --  Retrieves the current color mode for the layer.

   overriding
   function Hidden_Buffer
     (This  : in out SSD1306_Screen;
      Layer : Positive) return not null HAL.Bitmap.Any_Bitmap_Buffer;
   --  Retrieves the current hidden buffer for the layer.

   overriding
   function Pixel_Size
     (This  : SSD1306_Screen;
      Layer : Positive) return Positive;
   --  Retrieves the current hidden buffer for the layer.
private

   --  SSD1306 pixel are stored in a different order than the memory mapped
   --  impelementation. We define our own bitmap implementation to handle
   --  Set_Pixel () and Pixel () correctly.
   type SSD1306_Bitmap_Buffer (Buffer_Size_In_Byte : Positive) is
     new Memory_Mapped_Bitmap_Buffer with record
      Data : UInt8_Array (1 .. Buffer_Size_In_Byte);
     end record;

   overriding
   procedure Set_Pixel
     (Buffer  : in out SSD1306_Bitmap_Buffer;
      Pt      : Point);

   overriding
   procedure Set_Pixel
     (Buffer  : in out SSD1306_Bitmap_Buffer;
      Pt      : Point;
      Color   : Bitmap_Color);

   overriding
   procedure Set_Pixel
     (Buffer  : in out SSD1306_Bitmap_Buffer;
      Pt      : Point;
      Raw     : UInt32);

   overriding
   function Pixel
     (Buffer : SSD1306_Bitmap_Buffer;
      Pt     : Point)
      return Bitmap_Color;

   overriding
   function Pixel
     (Buffer : SSD1306_Bitmap_Buffer;
      Pt     : Point)
      return UInt32;

   overriding
   procedure Fill
     (Buffer : in out SSD1306_Bitmap_Buffer);

   SSD1306_I2C_Addr : constant := 16#78#;

   type Bit_Array is array (Natural range <>) of Bit with Pack;

   type SSD1306_Screen
     (Buffer_Size_In_Byte : Positive;
      Width               : Positive;
      Height              : Positive;
      Port                : not null Any_I2C_Port;
      RST                 : not null Any_GPIO_Point;
      Time                : not null HAL.Time.Any_Delays)
   is limited new HAL.Framebuffer.Frame_Buffer_Display with
      record
         Memory_Layer       : aliased SSD1306_Bitmap_Buffer (Buffer_Size_In_Byte);
         Layer_Initialized  : Boolean := False;
         Device_Initialized : Boolean := False;
      end record;

   --------------
   -- Commands --
   --------------

   DEACTIVATE_SCROLL     : constant := 16#2E#;
   SET_CONTRAST          : constant := 16#81#;
   DISPLAY_ALL_ON_RESUME : constant := 16#A4#;
   DISPLAY_ALL_ON        : constant := 16#A5#;
   NORMAL_DISPLAY        : constant := 16#A6#;
   INVERT_DISPLAY        : constant := 16#A7#;
   DISPLAY_OFF           : constant := 16#AE#;
   DISPLAY_ON            : constant := 16#AF#;
   SET_DISPLAY_OFFSET    : constant := 16#D3#;
   SET_COMPINS           : constant := 16#DA#;
   SET_VCOM_DETECT       : constant := 16#DB#;
   SET_DISPLAY_CLOCK_DIV : constant := 16#D5#;
   SET_PRECHARGE         : constant := 16#D9#;
   SET_MULTIPLEX         : constant := 16#A8#;
   SET_LOW_COLUMN        : constant := 16#00#;
   SET_HIGH_COLUMN       : constant := 16#10#;
   SET_START_LINE        : constant := 16#40#;
   MEMORY_MODE           : constant := 16#20#;
   COLUMN_ADDR           : constant := 16#21#;
   PAGE_ADDR             : constant := 16#22#;
   COM_SCAN_INC          : constant := 16#C0#;
   COM_SCAN_DEC          : constant := 16#C8#;
   SEGREMAP              : constant := 16#A0#;
   CHARGE_PUMP           : constant := 16#8D#;
end SSD1306;
