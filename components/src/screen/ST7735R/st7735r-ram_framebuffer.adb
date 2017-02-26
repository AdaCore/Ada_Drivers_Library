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

package body ST7735R.RAM_Framebuffer is

   ----------------------
   -- Initialize_Layer --
   ----------------------

   overriding
   procedure Initialize_Layer
     (Display : in out ST7735R_RAM_Framebuffer_Screen;
      Layer   : Positive;
      Mode    : FB_Color_Mode;
      X       : Natural := 0;
      Y       : Natural := 0;
      Width   : Positive := Positive'Last;
      Height  : Positive := Positive'Last)
   is
      pragma Unreferenced (X, Y, Width, Height);
   begin
      if Layer /= 1 or else Mode /= RGB_565 then
         raise Program_Error;
      end if;

      Display.Memory_Layer.Actual_Width := Screen_Width;
      Display.Memory_Layer.Actual_Height := Screen_Height;
      Display.Memory_Layer.Addr := Display.Layer_Data'Address;
      Display.Memory_Layer.Actual_Color_Mode := Mode;
      Display.Layer_Initialized := True;
   end Initialize_Layer;

   -----------------
   -- Initialized --
   -----------------

   overriding
   function Initialized
     (Display : ST7735R_RAM_Framebuffer_Screen;
      Layer   : Positive) return Boolean
   is
   begin
      return Layer = 1 and then Display.Layer_Initialized;
   end Initialized;

   ------------------
   -- Update_Layer --
   ------------------

   overriding
   procedure Update_Layer
     (Display   : in out ST7735R_RAM_Framebuffer_Screen;
      Layer     : Positive;
      Copy_Back : Boolean := False)
   is
      pragma Unreferenced (Copy_Back);
   begin
      if Layer /= 1 then
         raise Program_Error;
      end if;
      Set_Address (Display,
                   X_Start => 0,
                   X_End   => UInt16 (Display.Layer.Width - 1),
                   Y_Start => 0,
                   Y_End   => UInt16 (Display.Layer.Height - 1));
      Display.Write_Raw_Pixels (Display.Layer_Data);
   end Update_Layer;


   -------------------
   -- Update_Layers --
   -------------------

   overriding
   procedure Update_Layers
     (Display : in out ST7735R_RAM_Framebuffer_Screen)
   is
   begin
      Display.Update_Layer (1);
   end  Update_Layers;

   -----------------------
   -- Get_Hidden_Buffer --
   -----------------------

   overriding
   function Hidden_Buffer
     (Display : in out ST7735R_RAM_Framebuffer_Screen;
      Layer   : Positive) return not null HAL.Bitmap.Any_Bitmap_Buffer
   is
   begin
      if Layer /= 1 then
         raise Program_Error;
      end if;
      return Display.Memory_Layer'Unchecked_Access;
   end Hidden_Buffer;

end ST7735R.RAM_Framebuffer;
