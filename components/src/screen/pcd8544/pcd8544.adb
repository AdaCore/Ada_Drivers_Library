------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2020, AdaCore                           --
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
--     3. Neither the name of AdaCore nor the names of its                  --
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

package body PCD8544 is

   -------------------
   --  Chip_Select  --
   -------------------

   procedure Chip_Select (This : PCD8544_Device; Enabled : in Boolean)
   is
   begin
      if This.CS /= null then
         if Enabled then
            This.CS.Clear;
         else
            This.CS.Set;
         end if;
      end if;
   end Chip_Select;

   -------------
   --  Reset  --
   -------------

   procedure Reset (This : in out PCD8544_Device) is
   begin
      if This.RST /= null then
         This.RST.Clear;
         This.RST.Set;
         --  The datasheet specifies 100ns minimum for reset but this was
         --  unreliable in testing, so we use a longer delay.
         This.Time.Delay_Microseconds (100);
      end if;
   end Reset;

   ----------------
   --  Transmit  --
   ----------------

   procedure Transmit (This : PCD8544_Device; Data : in UInt8) is
      Status : SPI_Status;
   begin
      This.Chip_Select (True);
      This.DC.Clear;
      This.Port.Transmit (SPI_Data_8b'(1 => Data), Status);
      This.Chip_Select (False);
      if Status /= Ok then
         raise SPI_Error with "PCD8544 SPI command transmit failed: " & Status'Image;
      end if;
   end Transmit;

   ----------------
   --  Transmit  --
   ----------------

   procedure Transmit (This : PCD8544_Device; Data : in UInt8_Array) is
      Status : SPI_Status;
   begin
      This.Chip_Select (True);
      This.DC.Set;
      This.Port.Transmit (SPI_Data_8b (Data), Status);
      This.Chip_Select (False);
      if Status /= Ok then
         raise SPI_Error with "PCD8544 SPI data transmit failed: " & Status'Image;
      end if;
   end Transmit;

   ---------------------
   --  Extended_Mode  --
   ---------------------

   procedure Extended_Mode (This : in out PCD8544_Device) is
   begin
      if This.FR.Extended_Mode /= True then
         This.FR.Extended_Mode := True;
         This.Transmit (PCD8544_CMD_FUNCTION or Convert (This.FR));
      end if;
   end Extended_Mode;

   ------------------
   --  Basic_Mode  --
   ------------------

   procedure Basic_Mode (This : in out PCD8544_Device) is
   begin
      if This.FR.Extended_Mode /= False then
         This.FR.Extended_Mode := False;
         This.Transmit (PCD8544_CMD_FUNCTION or Convert (This.FR));
      end if;
   end Basic_Mode;

   --------------------
   --  Set_Contrast  --
   --------------------

   procedure Set_Contrast
     (This     : in out PCD8544_Device;
      Contrast : in PCD8544_Contrast) is
   begin
      This.Extended_Mode;
      This.Transmit (PCD8544_CMD_SET_VOP or Contrast);
   end Set_Contrast;

   ----------------
   --  Set_Bias  --
   ----------------

   procedure Set_Bias
     (This : in out PCD8544_Device;
      Bias : in PCD8544_Bias) is
   begin
      This.Extended_Mode;
      This.Transmit (PCD8544_CMD_SET_BIAS or Bias);
   end Set_Bias;

   -----------------------
   --  Set_Temperature  --
   -----------------------

   procedure Set_Temperature
     (This : in out PCD8544_Device;
      TC   : in PCD8544_Temperature_Coefficient) is
   begin
      This.Extended_Mode;
      This.Transmit (PCD8544_CMD_SET_TC or TC);
   end Set_Temperature;

   ------------------------
   --  Set_Display_Mode  --
   ------------------------

   procedure Set_Display_Mode
     (This   : in out PCD8544_Device;
      Enable : in Boolean;
      Invert : in Boolean) is
   begin
      This.Basic_Mode;
      This.DR.Enable := Enable;
      This.DR.Invert := Invert;
      This.Transmit (PCD8544_CMD_DISPLAY or Convert (This.DR));
   end Set_Display_Mode;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize (This : in out PCD8544_Device) is
      Default_FR : PCD8544_Function_Register;
      Default_DR : PCD8544_Display_Register;
   begin
      This.DC.Clear;
      This.Reset;

      This.FR := Default_FR;
      This.DR := Default_DR;

      --  Power on must be separate from other commands.
      This.FR.Power_Down := False;
      This.Transmit (PCD8544_CMD_FUNCTION or Convert (This.FR));

      This.Set_Contrast (PCD8544_Default_Contrast);
      This.Set_Bias (PCD8544_Default_Bias);
      This.Set_Temperature (PCD8544_Default_Temperature_Coefficient);

      This.FR.Extended_Mode := False;
      This.FR.Address_Mode  := Vertical;
      This.Transmit (PCD8544_CMD_FUNCTION or Convert (This.FR));

      This.Set_Display_Mode
         (Enable => True,
          Invert => True);

      This.Device_Initialized := True;
   end Initialize;

   ------------------------
   --  Write_Raw_Pixels  --
   ------------------------

   procedure Write_Raw_Pixels
     (This : in out PCD8544_Device;
      Data : UInt8_Array)
   is
   begin
      This.Chip_Select (True);
      This.Basic_Mode;
      This.Transmit (PCD8544_CMD_SET_X or 0);
      This.Transmit (PCD8544_CMD_SET_Y or 0);
      This.Transmit (Data);
      This.Chip_Select (False);
   end Write_Raw_Pixels;

   -------------------
   --  Initialized  --
   -------------------

   overriding function Initialized (This : PCD8544_Device) return Boolean is
     (This.Device_Initialized);

   ------------------
   --  Max_Layers  --
   ------------------

   overriding function Max_Layers (This : PCD8544_Device) return Positive is
     (1);

   -----------------
   --  Supported  --
   -----------------

   overriding function Supported
     (This : PCD8544_Device; Mode : FB_Color_Mode) return Boolean is
     (Mode = HAL.Bitmap.M_1);

   -----------------------
   --  Set_Orientation  --
   -----------------------

   overriding procedure Set_Orientation
     (This : in out PCD8544_Device; Orientation : Display_Orientation)
   is
   begin
      null;
   end Set_Orientation;

   ----------------
   --  Set_Mode  --
   ----------------

   overriding procedure Set_Mode
     (This : in out PCD8544_Device; Mode : Wait_Mode) is null;

   -------------
   --  Width  --
   -------------

   overriding function Width (This : PCD8544_Device) return Positive is
     (Device_Width);

   --------------
   --  Height  --
   --------------

   overriding function Height (This : PCD8544_Device) return Positive is
     (Device_Height);

   ---------------
   --  Swapped  --
   ---------------

   overriding function Swapped (This : PCD8544_Device) return Boolean is
     (False);

   ----------------------
   --  Set_Background  --
   ----------------------

   overriding procedure Set_Background (This : PCD8544_Device; R, G, B : UInt8)
   is
   begin
      raise Program_Error;
   end Set_Background;

   ------------------------
   --  Initialize_Layer  --
   ------------------------

   overriding procedure Initialize_Layer
     (This   : in out PCD8544_Device;
      Layer  : Positive;
      Mode   : FB_Color_Mode;
      X, Y   : Natural := 0;
      Width  : Positive := Positive'Last;
      Height : Positive := Positive'Last)
   is
      pragma Unreferenced (X, Y, Width, Height);
   begin
      if Layer /= 1 or else Mode /= M_1 then
         raise Program_Error;
      end if;
      This.Memory_Layer.Actual_Width      := This.Width;
      This.Memory_Layer.Actual_Height     := This.Height;
      This.Memory_Layer.Addr              := This.Memory_Layer.Data'Address;
      This.Memory_Layer.Actual_Color_Mode := Mode;

      for I in This.Memory_Layer.Data'Range loop
         This.Memory_Layer.Data (I) := 0;
      end loop;
      This.Layer_Initialized := True;
   end Initialize_Layer;

   -------------------
   --  Initialized  --
   -------------------

   overriding function Initialized
     (This  : PCD8544_Device;
      Layer : Positive) return Boolean
   is
   begin
      return Layer = 1 and then This.Layer_Initialized;
   end Initialized;

   --------------------
   --  Update_Layer  --
   --------------------

   overriding procedure Update_Layer
     (This      : in out PCD8544_Device;
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

   ------------------
   --  Color_Mode  --
   ------------------

   overriding function Color_Mode
     (This : PCD8544_Device; Layer : Positive) return FB_Color_Mode
   is
      pragma Unreferenced (This);
   begin
      if Layer /= 1 then
         raise Program_Error;
      end if;
      return M_1;
   end Color_Mode;

   ---------------------
   --  Hidden_Buffer  --
   ---------------------

   overriding function Hidden_Buffer
     (This : in out PCD8544_Device; Layer : Positive)
      return not null HAL.Bitmap.Any_Bitmap_Buffer
   is
   begin
      if Layer /= 1 then
         raise Program_Error;
      end if;
      return This.Memory_Layer'Unchecked_Access;
   end Hidden_Buffer;

   ---------------------
   --  Update_Layers  --
   ---------------------

   overriding procedure Update_Layers (This : in out PCD8544_Device) is
   begin
      This.Update_Layer (1);
   end Update_Layers;

   ------------------
   --  Pixel_Size  --
   ------------------

   overriding function Pixel_Size
     (Display : PCD8544_Device; Layer : Positive) return Positive is
     (1);

end PCD8544;
