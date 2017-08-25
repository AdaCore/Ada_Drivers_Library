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

package body SSD1306 is

   procedure Write_Command (This : SSD1306_Screen;
                            Cmd  : UInt8);

   procedure Write_Data (This : SSD1306_Screen;
                         Data : UInt8_Array);

   -------------------
   -- Write_Command --
   -------------------

   procedure Write_Command (This : SSD1306_Screen;
                            Cmd  : UInt8)
   is
      Status : I2C_Status;
   begin
      This.Port.Master_Transmit (Addr    => SSD1306_I2C_Addr,
                                 Data    => (1 => 0, 2 => (Cmd)),
                                 Status  => Status);
      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;
   end Write_Command;

   ----------------
   -- Write_Data --
   ----------------

   procedure Write_Data (This : SSD1306_Screen;
                         Data : UInt8_Array)
   is
      Status : I2C_Status;
   begin
      This.Port.Master_Transmit (Addr    => SSD1306_I2C_Addr,
                                 Data    => Data,
                                 Status  => Status);
      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;
   end Write_Data;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This         : in out SSD1306_Screen;
                         External_VCC : Boolean)
   is
   begin

      if This.Width * This.Height /= (This.Buffer_Size_In_Byte * 8) then
         raise Program_Error with "Invalid screen parameters";
      end if;

      This.RST.Clear;
      This.Time.Delay_Milliseconds (100);
      This.RST.Set;
      This.Time.Delay_Milliseconds (100);

      Write_Command (This, DISPLAY_OFF);

      Write_Command (This, SET_DISPLAY_CLOCK_DIV);
      Write_Command (This, 16#80#);

      Write_Command (This, SET_MULTIPLEX);
      Write_Command (This, UInt8 (This.Height - 1));

      Write_Command (This, SET_DISPLAY_OFFSET);
      Write_Command (This, 16#00#);

      Write_Command (This, SET_START_LINE or 0);

      Write_Command (This, CHARGE_PUMP);
      Write_Command (This, (if External_VCC then 16#10# else 16#14#));

      Write_Command (This, MEMORY_MODE);
      Write_Command (This, 16#00#);

      Write_Command (This, SEGREMAP or 1);

      Write_Command (This, COM_SCAN_DEC);

      Write_Command (This, SET_COMPINS);
      if This.Height > 32 then
         Write_Command (This, 16#12#);
      else
         Write_Command (This, 16#02#);
      end if;

      Write_Command (This, SET_CONTRAST);
      Write_Command (This, 16#AF#);

      Write_Command (This, SET_PRECHARGE);
      Write_Command (This, (if External_VCC then 16#22# else 16#F1#));

      Write_Command (This, SET_VCOM_DETECT);
      Write_Command (This, 16#40#);

      Write_Command (This, DISPLAY_ALL_ON_RESUME);
      Write_Command (This, NORMAL_DISPLAY);
      Write_Command (This, DEACTIVATE_SCROLL);

      This.Device_Initialized := True;
   end Initialize;

   -----------------
   -- Initialized --
   -----------------

   overriding
   function Initialized (This : SSD1306_Screen) return Boolean is
     (This.Device_Initialized);

   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (This : SSD1306_Screen) is
   begin
      Write_Command (This, DISPLAY_ON);
   end Turn_On;

   --------------
   -- Turn_Off --
   --------------

   procedure Turn_Off (This : SSD1306_Screen) is
   begin
      Write_Command (This, DISPLAY_OFF);
   end Turn_Off;


   --------------------------
   -- Display_Inversion_On --
   --------------------------

   procedure Display_Inversion_On (This : SSD1306_Screen) is
   begin
      Write_Command (This, INVERT_DISPLAY);
   end Display_Inversion_On;


   ---------------------------
   -- Display_Inversion_Off --
   ---------------------------

   procedure Display_Inversion_Off (This : SSD1306_Screen) is
   begin
      Write_Command (This, NORMAL_DISPLAY);
   end Display_Inversion_Off;

   ----------------------
   -- Write_Raw_Pixels --
   ----------------------

   procedure Write_Raw_Pixels (This  : SSD1306_Screen;
                               Data : HAL.UInt8_Array)
   is
   begin
      Write_Command (This, COLUMN_ADDR);
      Write_Command (This, 0);                   --  from
      Write_Command (This, UInt8 (This.Width - 1));  --  to

      Write_Command (This, PAGE_ADDR);
      Write_Command (This, 0);                           --  from
      Write_Command (This, UInt8 (This.Height / 8) - 1); --  to

      Write_Data (This, (1 => 16#40#) & Data);
   end Write_Raw_Pixels;

   --------------------
   -- Get_Max_Layers --
   --------------------

   overriding
   function Max_Layers
     (This : SSD1306_Screen) return Positive is (1);

   ------------------
   -- Is_Supported --
   ------------------

   overriding
   function Supported
     (This : SSD1306_Screen;
      Mode : FB_Color_Mode) return Boolean is
     (Mode = HAL.Bitmap.RGB_565);

   ---------------------
   -- Set_Orientation --
   ---------------------

   overriding
   procedure Set_Orientation
     (This        : in out SSD1306_Screen;
      Orientation : Display_Orientation)
   is
   begin
      null;
   end Set_Orientation;

   --------------
   -- Set_Mode --
   --------------

   overriding
   procedure Set_Mode
     (This : in out SSD1306_Screen;
      Mode : Wait_Mode)
   is
   begin
      null;
   end Set_Mode;

   ---------------
   -- Get_Width --
   ---------------

   overriding
   function Width
     (This : SSD1306_Screen) return Positive is (This.Width);

   ----------------
   -- Get_Height --
   ----------------

   overriding
   function Height
     (This : SSD1306_Screen) return Positive is (This.Height);

   ----------------
   -- Is_Swapped --
   ----------------

   overriding
   function Swapped
     (This : SSD1306_Screen) return Boolean is (False);

   --------------------
   -- Set_Background --
   --------------------

   overriding
   procedure Set_Background
     (This : SSD1306_Screen; R, G, B : UInt8)
   is
   begin
      --  Does it make sense when there's no alpha channel...
      raise Program_Error;
   end Set_Background;

   ----------------------
   -- Initialize_Layer --
   ----------------------

   overriding
   procedure Initialize_Layer
     (This   : in out SSD1306_Screen;
      Layer  : Positive;
      Mode   : FB_Color_Mode;
      X      : Natural := 0;
      Y      : Natural := 0;
      Width  : Positive := Positive'Last;
      Height : Positive := Positive'Last)
   is
      pragma Unreferenced (X, Y, Width, Height);
   begin
      if Layer /= 1 or else Mode /= M_1 then
         raise Program_Error;
      end if;
      This.Memory_Layer.Actual_Width  := This.Width;
      This.Memory_Layer.Actual_Height := This.Height;
      This.Memory_Layer.Addr := This.Memory_Layer.Data'Address;
      This.Memory_Layer.Actual_Color_Mode := Mode;
      This.Layer_Initialized := True;
   end Initialize_Layer;

   -----------------
   -- Initialized --
   -----------------

   overriding
   function Initialized
     (This  : SSD1306_Screen;
      Layer : Positive) return Boolean
   is
   begin
      return Layer = 1 and then This.Layer_Initialized;
   end Initialized;

   ------------------
   -- Update_Layer --
   ------------------

   overriding
   procedure Update_Layer
     (This      : in out SSD1306_Screen;
      Layer     : Positive;
      Copy_Back : Boolean := False)
   is
      pragma Unreferenced (Copy_Back);
   begin
      if Layer /= 1 then
         raise Program_Error;
      end if;

      This.Write_Raw_Pixels (This.Memory_Layer.Data);
   end Update_Layer;

   -------------------
   -- Update_Layers --
   -------------------

   overriding
   procedure Update_Layers
     (This : in out SSD1306_Screen)
   is
   begin
      This.Update_Layer (1);
   end  Update_Layers;

   --------------------
   -- Get_Color_Mode --
   --------------------

   overriding
   function Color_Mode
     (This  : SSD1306_Screen;
      Layer : Positive) return FB_Color_Mode
   is
      pragma Unreferenced (This);
   begin
      if Layer /= 1 then
         raise Program_Error;
      end if;
      return M_1;
   end Color_Mode;

   -----------------------
   -- Get_Hidden_Buffer --
   -----------------------

   overriding
   function Hidden_Buffer
     (This  : in out SSD1306_Screen;
      Layer : Positive) return not null HAL.Bitmap.Any_Bitmap_Buffer
   is
   begin
      if Layer /= 1 then
         raise Program_Error;
      end if;
      return This.Memory_Layer'Unchecked_Access;
   end Hidden_Buffer;

   --------------------
   -- Get_Pixel_Size --
   --------------------

   overriding
   function Pixel_Size
     (This  : SSD1306_Screen;
      Layer : Positive) return Positive is (1);

   ---------------
   -- Set_Pixel --
   ---------------

   overriding
   procedure Set_Pixel
     (Buffer  : in out SSD1306_Bitmap_Buffer;
      Pt      : Point)
   is
      X     : constant Natural := Buffer.Width - 1 - Pt.X;
      Y     : constant Natural := Buffer.Height - 1 - Pt.Y;
      Index : constant Natural := X + (Y / 8) * Buffer.Actual_Width;
      Byte  : UInt8 renames Buffer.Data (Buffer.Data'First + Index);
   begin

      if Buffer.Native_Source = 0 then
         Byte := Byte and not (Shift_Left (1, Y mod 8));
      else
         Byte := Byte or Shift_Left (1, Y mod 8);
      end if;
   end Set_Pixel;

   ---------------
   -- Set_Pixel --
   ---------------

   overriding
   procedure Set_Pixel
     (Buffer  : in out SSD1306_Bitmap_Buffer;
      Pt      : Point;
      Color   : Bitmap_Color)
   is
   begin
      Buffer.Set_Pixel (Pt, (if Color = Black then 0 else 1));
   end Set_Pixel;

   ---------------
   -- Set_Pixel --
   ---------------

   overriding
   procedure Set_Pixel
     (Buffer  : in out SSD1306_Bitmap_Buffer;
      Pt      : Point;
      Raw     : UInt32)
   is
   begin
      Buffer.Native_Source := Raw;
      Buffer.Set_Pixel (Pt);
   end Set_Pixel;

   -----------
   -- Pixel --
   -----------

   overriding
   function Pixel
     (Buffer : SSD1306_Bitmap_Buffer;
      Pt     : Point)
      return Bitmap_Color
   is
   begin
      return (if Buffer.Pixel (Pt) = 0 then Black else White);
   end Pixel;

   -----------
   -- Pixel --
   -----------

   overriding
   function Pixel
     (Buffer : SSD1306_Bitmap_Buffer;
      Pt     : Point)
      return UInt32
   is
      X     : constant Natural := Buffer.Width - 1 - Pt.X;
      Y     : constant Natural := Buffer.Height - 1 - Pt.Y;
      Index : constant Natural :=  X + (Y / 8) * Buffer.Actual_Width;
      Byte  : UInt8 renames Buffer.Data (Buffer.Data'First + Index);
   begin

      if (Byte and (Shift_Left (1, Y mod 8))) /= 0 then
         return 1;
      else
         return 0;
      end if;
   end Pixel;

   ----------
   -- Fill --
   ----------

   overriding
   procedure Fill
     (Buffer : in out SSD1306_Bitmap_Buffer)
   is
      Val : constant UInt8 := (if Buffer.Native_Source /= 0 then 16#FF# else 0);
   begin
      Buffer.Data := (others => Val);
   end Fill;

end SSD1306;
