------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with HAL.Bitmap; use HAL.Bitmap;

with RPi.Bitmap;

package RPi.Framebuffer is

   type Framebuffer_Display is limited private;

   type Color_Depth_Type is range 1 .. 4;
   --  Number of bytes per pixel
   --  1: indexed colors
   --  2: RGB565 mode
   --  3: RGB888 mode
   --  4: ARGB8888 mode

   type Alpha_Mode is
     (Alpha_Channel_Enabled,
      Alpha_Channel_Reversed,
      Alpha_Channel_Ignored);

   procedure Initialize
     (Display     : in out Framebuffer_Display;
      Width       : Natural;
      Height      : Natural;
      Color_Depth : Color_Depth_Type);

   function Hidden_Framebuffer
     (Display : in out Framebuffer_Display)
      return RPi.Bitmap.RPi_Bitmap_Buffer;

   procedure Blank (Display : in out Framebuffer_Display;
                    State   : Boolean);

   procedure Set_Alpha_Mode (Display : in out Framebuffer_Display;
                             Mode    : Alpha_Mode);

   procedure Flip (Display : in out Framebuffer_Display);

   function Color_Mode
     (Display : Framebuffer_Display) return Bitmap_Color_Mode;

private

   type Buffer_Index is range 1 .. 2;

   type Buffer_Array is
     array (Buffer_Index) of aliased RPi.Bitmap.RPi_Bitmap_Buffer;

   type Framebuffer_Display is record
      FB            : Buffer_Array;
      Width         : Natural;
      Height        : Natural;
      Depth         : Color_Depth_Type;
      Active_Buffer : Buffer_Index := 1;
   end record;

   function Color_Mode
     (Display : Framebuffer_Display) return Bitmap_Color_Mode
   is (Display.FB (1).Color_Mode);

end RPi.Framebuffer;
