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

with HAL;             use HAL;
with HAL.Framebuffer; use HAL.Framebuffer;
with HAL.Bitmap;
with STM32.DSI;
with STM32.GPIO;
with Ravenscar_Time;

private with STM32.Device;
private with STM32.DMA2D_Bitmap;
private with STM32.LTDC;
private with OTM8009A;
private with HAL.DSI;

package Framebuffer_DSI is

   LCD_Natural_Width  : constant := 800;
   LCD_Natural_Height : constant := 480;

   type Frame_Buffer is limited
     new HAL.Framebuffer.Frame_Buffer_Display with private;

   overriding function Max_Layers
     (Display : Frame_Buffer) return Positive;

   overriding function Supported
     (Display : Frame_Buffer;
      Mode    : HAL.Framebuffer.FB_Color_Mode) return Boolean;

   procedure Initialize
     (Display     : in out Frame_Buffer;
      Orientation : HAL.Framebuffer.Display_Orientation := Default;
      Mode        : HAL.Framebuffer.Wait_Mode := Interrupt;
      Reset_Point : in out STM32.GPIO.GPIO_Point;
      PLLSAI_N    : UInt9;
      PLLSAI_R    : UInt3;
      DivR        : Natural;
      PLL_N_Div   : STM32.DSI.DSI_PLLN_Div;
      PLL_IN_Div  : STM32.DSI.DSI_PLL_IDF;
      PLL_OUT_Div : STM32.DSI.DSI_PLL_ODF);

   overriding function Initialized
     (Display : Frame_Buffer) return Boolean;

   overriding procedure Set_Orientation
     (Display     : in out Frame_Buffer;
      Orientation : HAL.Framebuffer.Display_Orientation);

   overriding procedure Set_Mode
     (Display : in out Frame_Buffer;
      Mode    : HAL.Framebuffer.Wait_Mode);

   overriding function Width
     (Display : Frame_Buffer) return Positive;

   overriding function Height
     (Display : Frame_Buffer) return Positive;

   overriding function Swapped
     (Display : Frame_Buffer) return Boolean;

   overriding procedure Set_Background
     (Display : Frame_Buffer; R, G, B : UInt8);

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

   overriding function Color_Mode
     (Display : Frame_Buffer;
      Layer   : Positive) return HAL.Framebuffer.FB_Color_Mode;

   overriding function Hidden_Buffer
     (Display : in out Frame_Buffer;
      Layer   : Positive) return not null HAL.Bitmap.Any_Bitmap_Buffer;
   --  Retrieves the current hidden buffer for the layer.

   overriding function Pixel_Size
     (Display : Frame_Buffer;
      Layer   : Positive) return Positive;

private

   type FB_Array is array (STM32.LTDC.LCD_Layer) of
     aliased STM32.DMA2D_Bitmap.DMA2D_Bitmap_Buffer;

   LCD_Channel : constant HAL.DSI.DSI_Virtual_Channel_ID := 0;
   --  Only one display on this board, constant to 0


   type Frame_Buffer is limited new HAL.Framebuffer.Frame_Buffer_Display with
      record
         Device      : OTM8009A.OTM8009A_Device
                        (DSI_Host   => STM32.Device.DSIHOST'Access,
                         Channel_Id => LCD_Channel,
                         Time       => Ravenscar_Time.Delays);
         Swapped     : Boolean;
         Buffers     : FB_Array := (others => STM32.DMA2D_Bitmap.Null_Buffer);

         --  Saved values for the clocks, in case we need them for changing
         --  the orientation of the display
         PLLSAIN     : UInt9;
         PLLSAIR     : UInt3;
         PLLSAI_DIVR : Natural;
         PLL_N_Div   : STM32.DSI.DSI_PLLN_Div;
         PLL_IN_Div  : STM32.DSI.DSI_PLL_IDF;
         PLL_OUT_Div : STM32.DSI.DSI_PLL_ODF;
      end record;
   type Frame_Buffer_Ref is access all Frame_Buffer;

end Framebuffer_DSI;
