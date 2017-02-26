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

with HAL.Bitmap;

package HAL.Framebuffer is

   subtype FB_Color_Mode is HAL.Bitmap.Bitmap_Color_Mode range
     HAL.Bitmap.ARGB_8888 .. HAL.Bitmap.M_1;

   type Display_Orientation is
     (Default, Landscape, Portrait);

   type Wait_Mode is (Polling, Interrupt);

   type Frame_Buffer_Display is limited interface;

   type Any_Frame_Buffer_Display is access all Frame_Buffer_Display'Class;

   function Max_Layers
     (This : Frame_Buffer_Display) return Positive
      is abstract;

   function Supported
     (This : Frame_Buffer_Display;
      Mode : FB_Color_Mode) return Boolean
      is abstract;

   procedure Set_Orientation
     (This        : in out Frame_Buffer_Display;
      Orientation : Display_Orientation) is abstract;

   procedure Set_Mode
     (This    : in out Frame_Buffer_Display;
      Mode    : Wait_Mode) is abstract;

   function Initialized
     (This : Frame_Buffer_Display) return Boolean is abstract;

   function Width
     (This : Frame_Buffer_Display) return Positive is abstract;

   function Height
     (This : Frame_Buffer_Display) return Positive is abstract;

   function Swapped
     (This : Frame_Buffer_Display) return Boolean is abstract;
   --  Whether X/Y coordinates are considered Swapped by the drawing primitives
   --  This simulates Landscape/Portrait orientation on displays not supporting
   --  hardware orientation change.

   procedure Set_Background
     (This : Frame_Buffer_Display; R, G, B : UInt8) is abstract;

   procedure Initialize_Layer
     (This   : in out Frame_Buffer_Display;
      Layer  : Positive;
      Mode   : FB_Color_Mode;
      X      : Natural := 0;
      Y      : Natural := 0;
      Width  : Positive := Positive'Last;
      Height : Positive := Positive'Last) is abstract;
   --  All layers are double buffered, so an explicit call to Update_Layer
   --  needs to be performed to actually display the current buffer attached
   --  to the layer.
   --  Alloc is called to create the actual buffer.

   function Initialized
     (This  : Frame_Buffer_Display;
      Layer : Positive) return Boolean is abstract;

   procedure Update_Layer
     (This      : in out Frame_Buffer_Display;
      Layer     : Positive;
      Copy_Back : Boolean := False) is abstract;
   --  Updates the layer so that the hidden buffer is displayed.

   procedure Update_Layers
     (This : in out Frame_Buffer_Display) is abstract;
   --  Updates all initialized layers at once with their respective hidden
   --  buffer.

   function Color_Mode
     (This  : Frame_Buffer_Display;
      Layer : Positive) return FB_Color_Mode is abstract;
   --  Retrieves the current color mode for the layer.

   function Hidden_Buffer
     (This  : in out Frame_Buffer_Display;
      Layer : Positive)
      return not null HAL.Bitmap.Any_Bitmap_Buffer is abstract;
   --  Retrieves the current hidden buffer for the layer.

   function Pixel_Size
     (Display : Frame_Buffer_Display;
      Layer   : Positive) return Positive is abstract;
   --  Retrieves the current hidden buffer for the layer.

end HAL.Framebuffer;
