------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

   overriding function Max_Layers
     (Display : Frame_Buffer) return Positive;

   overriding function Supported
     (Display : Frame_Buffer;
      Mode    : HAL.Framebuffer.FB_Color_Mode) return Boolean;

   overriding function Width
     (Display : Frame_Buffer) return Positive
     with Pre => Display.Initialized;

   overriding function Height
     (Display : Frame_Buffer) return Positive
     with Pre => Display.Initialized;

   overriding function Swapped
     (Display : Frame_Buffer) return Boolean;

   overriding procedure Set_Background
     (Display : Frame_Buffer; R, G, B : UInt8)
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
       Supported (Display, Mode) and then
       not Initialized (Display, Layer) and then
       Layer <= Max_Layers (Display);
   --  All layers are double buffered, so an explicit call to Update_Layer
   --  needs to be performed to actually display the current buffer attached
   --  to the layer.
   --  Alloc is called to create the actual buffer.

   overriding function Initialized
     (Display : Frame_Buffer;
      Layer   : Positive) return Boolean
     with Pre =>
       Initialized (Display) and then
       Layer <= Max_Layers (Display);

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

   overriding function Color_Mode
     (Display : Frame_Buffer;
      Layer   : Positive) return HAL.Framebuffer.FB_Color_Mode
     with Pre => Initialized (Display, Layer);

   overriding function Hidden_Buffer
     (Display : in out Frame_Buffer;
      Layer   : Positive) return not null HAL.Bitmap.Any_Bitmap_Buffer
     with Pre => Initialized (Display, Layer);
   --  Retrieves the current hidden buffer for the layer.

   overriding function Pixel_Size
     (Display : Frame_Buffer;
      Layer   : Positive) return Positive
     with Pre => Initialized (Display, Layer);

private

   type FB_Array is array (STM32.LTDC.LCD_Layer, 1 .. 2) of
     aliased STM32.DMA2D_Bitmap.DMA2D_Bitmap_Buffer;
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
