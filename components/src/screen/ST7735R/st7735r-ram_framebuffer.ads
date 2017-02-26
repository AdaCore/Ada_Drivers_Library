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

--  This a buffered driver for the ST7735R LCD. Pixels are stored in RAM
--  which means more memory consumption but also fast operations.
--
--  Please use ST7735R for a low memory foot print implementation.

with Memory_Mapped_Bitmap; use Memory_Mapped_Bitmap;

package ST7735R.RAM_Framebuffer is

   subtype Parent is ST7735R_Screen;

   type ST7735R_RAM_Framebuffer_Screen
     (Port : not null Any_SPI_Port;
      CS   : not null Any_GPIO_Point;
      RS   : not null Any_GPIO_Point;
      RST  : not null Any_GPIO_Point;
      Time : not null HAL.Time.Any_Delays)
   is limited new Parent with private;

   overriding
   procedure Initialize_Layer
     (Display : in out ST7735R_RAM_Framebuffer_Screen;
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
     (Display : ST7735R_RAM_Framebuffer_Screen;
      Layer   : Positive) return Boolean;

   overriding
   procedure Update_Layer
     (Display   : in out ST7735R_RAM_Framebuffer_Screen;
      Layer     : Positive;
      Copy_Back : Boolean := False);
   --  Updates the layer so that the hidden buffer is displayed.

   overriding
   procedure Update_Layers
     (Display : in out ST7735R_RAM_Framebuffer_Screen);
   --  Updates all initialized layers at once with their respective hidden
   --  buffer

   overriding
   function Hidden_Buffer
     (Display : in out ST7735R_RAM_Framebuffer_Screen;
      Layer   : Positive) return not null HAL.Bitmap.Any_Bitmap_Buffer;
   --  Retrieves the current hidden buffer for the layer.

private

   subtype Pixel_Data is UInt16_Array (0 .. (Screen_Width * Screen_Height) - 1);

   type ST7735R_RAM_Framebuffer_Screen
     (Port : not null Any_SPI_Port;
      CS   : not null Any_GPIO_Point;
      RS   : not null Any_GPIO_Point;
      RST  : not null Any_GPIO_Point;
      Time : not null HAL.Time.Any_Delays)
   is limited new Parent (Port, CS, RS, RST, Time) with record
      Memory_Layer : aliased Memory_Mapped_Bitmap_Buffer;
      Layer_Data : Pixel_Data;
      Layer_Initialized : Boolean := False;
   end record;

end ST7735R.RAM_Framebuffer;
