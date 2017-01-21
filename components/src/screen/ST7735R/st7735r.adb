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

with Ada.Unchecked_Conversion;
with System;
with Interfaces; use Interfaces;

package body ST7735R is

   ---------------------------
   --  Register definitions --
   ---------------------------

   type MADCTL is record
      Reserved1, Reserved2 : Boolean;
      MH  : Horizontal_Refresh_Order;
      RGB : RGB_BGR_Order;
      ML  : Vertical_Refresh_Order;
      MV  : Boolean;
      MX  : Column_Address_Order;
      MY  : Row_Address_Order;
   end record with Size => 8, Bit_Order => System.Low_Order_First;

   for MADCTL use record
      Reserved1 at 0 range 0 .. 0;
      Reserved2 at 0 range 1 .. 1;
      MH        at 0 range 2 .. 2;
      RGB       at 0 range 3 .. 3;
      ML        at 0 range 4 .. 4;
      MV        at 0 range 5 .. 5;
      MX        at 0 range 6 .. 6;
      MY        at 0 range 7 .. 7;
   end record;

   function To_Byte is new Ada.Unchecked_Conversion (MADCTL, Byte);

   procedure Write_Command (LCD : ST7735R_Device;
                            Cmd : Byte);
   procedure Write_Command (LCD  : ST7735R_Device;
                            Cmd  : Byte;
                            Data : HAL.Byte_Array);

   procedure Write_Data (LCD : ST7735R_Device;
                         Data : Byte);
   pragma Unreferenced (Write_Data);
   procedure Write_Data (LCD  : ST7735R_Device;
                         Data : HAL.Byte_Array);
   procedure Write_Data (LCD  : ST7735R_Device;
                         Data : HAL.UInt16_Array);

   procedure Set_Command_Mode (LCD : ST7735R_Device);
   procedure Set_Data_Mode (LCD : ST7735R_Device);
   procedure Start_Transaction (LCD : ST7735R_Device);
   procedure End_Transaction (LCD : ST7735R_Device);

   ----------------------
   -- Set_Command_Mode --
   ----------------------

   procedure Set_Command_Mode (LCD : ST7735R_Device) is
   begin
      LCD.RS.Clear;
   end Set_Command_Mode;

   -------------------
   -- Set_Data_Mode --
   -------------------

   procedure Set_Data_Mode (LCD : ST7735R_Device) is
   begin
      LCD.RS.Set;
   end Set_Data_Mode;

   -----------------------
   -- Start_Transaction --
   -----------------------

   procedure Start_Transaction (LCD : ST7735R_Device) is
   begin
      LCD.CS.Clear;
   end Start_Transaction;

   ---------------------
   -- End_Transaction --
   ---------------------

   procedure End_Transaction (LCD : ST7735R_Device) is
   begin
      LCD.CS.Set;
   end End_Transaction;

   -------------------
   -- Write_Command --
   -------------------

   procedure Write_Command (LCD : ST7735R_Device;
                            Cmd : Byte)
   is
      Status : SPI_Status;
   begin
      Start_Transaction (LCD);
      Set_Command_Mode (LCD);
      LCD.Port.Transmit (SPI_Data_8b'(1 => Cmd),
                         Status);
      End_Transaction (LCD);
      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;
   end Write_Command;

   -------------------
   -- Write_Command --
   -------------------

   procedure Write_Command (LCD  : ST7735R_Device;
                            Cmd  : Byte;
                            Data : HAL.Byte_Array)
   is
   begin
      Write_Command (LCD, Cmd);
      Write_Data (LCD, Data);
   end Write_Command;

   ----------------
   -- Write_Data --
   ----------------

   procedure Write_Data (LCD : ST7735R_Device;
                         Data : Byte)
   is
      Status : SPI_Status;
   begin
      Start_Transaction (LCD);
      Set_Data_Mode (LCD);
      LCD.Port.Transmit (SPI_Data_8b'(1 => Data),
                         Status);
      End_Transaction (LCD);
      if Status /= Ok then
         --  No error handling...
         raise Program_Error;
      end if;
   end Write_Data;

   ----------------
   -- Write_Data --
   ----------------

   procedure Write_Data (LCD  : ST7735R_Device;
                         Data : HAL.Byte_Array)
   is
      Status : SPI_Status;
   begin
      Start_Transaction (LCD);
      Set_Data_Mode (LCD);
      for Elt of Data loop
         LCD.Port.Transmit (SPI_Data_8b'(1 => Elt),
                            Status);
         if Status /= Ok then
            --  No error handling...
            raise Program_Error;
         end if;
      end loop;
      End_Transaction (LCD);
   end Write_Data;

   ----------------
   -- Write_Data --
   ----------------

   procedure Write_Data (LCD  : ST7735R_Device;
                         Data : HAL.UInt16_Array)
   is
      B1, B2 : Byte;
      Status : SPI_Status;
   begin
      Start_Transaction (LCD);
      Set_Data_Mode (LCD);
      for Elt of Data loop
         B1 := Byte (Interfaces.Shift_Right (Elt, 8));
         B2 := Byte (Elt and 16#FF#);
         LCD.Port.Transmit (SPI_Data_8b'(B1, B2),
                            Status);
         if Status /= Ok then
            --  No error handling...
            raise Program_Error;
         end if;
      end loop;
      End_Transaction (LCD);
   end Write_Data;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (LCD : in out ST7735R_Device) is
   begin
      LCD.Initialized := True;

      LCD.RST.Clear;
      LCD.Time.Delay_Milliseconds (100);
      LCD.RST.Set;
      LCD.Time.Delay_Milliseconds (100);

      --  Sleep Exit
      Write_Command (LCD, 16#11#);

      LCD.Time.Delay_Milliseconds (100);
   end Initialize;

   -----------------
   -- Initialized --
   -----------------

   overriding
   function Initialized (LCD : ST7735R_Device) return Boolean is
     (LCD.Initialized);

   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (LCD : ST7735R_Device) is
   begin
      Write_Command (LCD, 16#29#);
   end Turn_On;

   --------------
   -- Turn_Off --
   --------------

   procedure Turn_Off (LCD : ST7735R_Device) is
   begin
      Write_Command (LCD, 16#28#);
   end Turn_Off;


   --------------------------
   -- Display_Inversion_On --
   --------------------------

   procedure Display_Inversion_On (LCD : ST7735R_Device) is
   begin
      Write_Command (LCD, 16#21#);
   end Display_Inversion_On;


   ---------------------------
   -- Display_Inversion_Off --
   ---------------------------

   procedure Display_Inversion_Off (LCD : ST7735R_Device) is
   begin
      Write_Command (LCD, 16#20#);
   end Display_Inversion_Off;

   ---------------
   -- Gamma_Set --
   ---------------

   procedure Gamma_Set (LCD : ST7735R_Device; Gamma_Curve : UInt4) is
   begin
      Write_Command (LCD, 16#26#, (0 => Byte (Gamma_Curve)));
   end Gamma_Set;

   ----------------------
   -- Set_Pixel_Format --
   ----------------------

   procedure Set_Pixel_Format (LCD : ST7735R_Device; Pix_Fmt : Pixel_Format) is
      Value : constant Byte := (case Pix_Fmt is
                                   when Pixel_12bits => 2#011#,
                                   when Pixel_16bits => 2#101#,
                                   when Pixel_18bits => 2#110#);
   begin
      Write_Command (LCD, 16#3A#, (0 => Value));
   end Set_Pixel_Format;

   ----------------------------
   -- Set_Memory_Data_Access --
   ----------------------------

   procedure Set_Memory_Data_Access
     (LCD                 : ST7735R_Device;
      Color_Order         : RGB_BGR_Order;
      Vertical            : Vertical_Refresh_Order;
      Horizontal          : Horizontal_Refresh_Order;
      Row_Addr_Order      : Row_Address_Order;
      Column_Addr_Order   : Column_Address_Order;
      Row_Column_Exchange : Boolean)
   is

      Value : MADCTL;
   begin
      Value.MY := Row_Addr_Order;
      Value.MX := Column_Addr_Order;
      Value.MV := Row_Column_Exchange;
      Value.ML := Vertical;
      Value.RGB := Color_Order;
      Value.MH := Horizontal;
      Write_Command (LCD, 16#36#, (0 => To_Byte (Value)));
   end Set_Memory_Data_Access;

   ---------------------------
   -- Set_Frame_Rate_Normal --
   ---------------------------

   procedure Set_Frame_Rate_Normal
     (LCD         : ST7735R_Device;
      RTN         : UInt4;
      Front_Porch : UInt6;
      Back_Porch  : UInt6)
   is
   begin
      Write_Command (LCD, 16#B1#,
                     (Byte (RTN), Byte (Front_Porch), Byte (Back_Porch)));
   end Set_Frame_Rate_Normal;

   -------------------------
   -- Set_Frame_Rate_Idle --
   -------------------------

   procedure Set_Frame_Rate_Idle
     (LCD         : ST7735R_Device;
      RTN         : UInt4;
      Front_Porch : UInt6;
      Back_Porch  : UInt6)
   is
   begin
      Write_Command (LCD, 16#B2#,
                     (Byte (RTN), Byte (Front_Porch), Byte (Back_Porch)));
   end Set_Frame_Rate_Idle;

   ---------------------------------
   -- Set_Frame_Rate_Partial_Full --
   ---------------------------------

   procedure Set_Frame_Rate_Partial_Full
     (LCD              : ST7735R_Device;
      RTN_Part         : UInt4;
      Front_Porch_Part : UInt6;
      Back_Porch_Part  : UInt6;
      RTN_Full         : UInt4;
      Front_Porch_Full : UInt6;
      Back_Porch_Full  : UInt6)
   is
   begin
      Write_Command (LCD, 16#B3#,
                     (Byte (RTN_Part),
                      Byte (Front_Porch_Part),
                      Byte (Back_Porch_Part),
                      Byte (RTN_Full),
                      Byte (Front_Porch_Full),
                      Byte (Back_Porch_Full)));
   end Set_Frame_Rate_Partial_Full;

   ---------------------------
   -- Set_Inversion_Control --
   ---------------------------

   procedure Set_Inversion_Control
     (LCD : ST7735R_Device;
      Normal, Idle, Full_Partial : Inversion_Control)
   is
      Value : Byte := 0;
   begin
      if Normal = Line_Inversion then
         Value := Value or 2#100#;
      end if;
      if Idle = Line_Inversion then
         Value := Value or 2#010#;
      end if;
      if Full_Partial = Line_Inversion then
         Value := Value or 2#001#;
      end if;
      Write_Command (LCD, 16#B4#, (0 => Value));
   end Set_Inversion_Control;

   -------------------------
   -- Set_Power_Control_1 --
   -------------------------

   procedure Set_Power_Control_1
     (LCD  : ST7735R_Device;
      AVDD : UInt3;
      VRHP : UInt5;
      VRHN : UInt5;
      MODE : UInt2)
   is
      P1, P2, P3 : Byte;
   begin
      P1 := Interfaces.Shift_Left (Byte (AVDD), 5) or Byte (VRHP);
      P2 := Byte (VRHN);
      P3 := Interfaces.Shift_Left (Byte (MODE), 6) or 2#00_0100#;
      Write_Command (LCD, 16#C0#, (P1, P2, P3));
   end Set_Power_Control_1;

   -------------------------
   -- Set_Power_Control_2 --
   -------------------------

   procedure Set_Power_Control_2
     (LCD   : ST7735R_Device;
      VGH25 : UInt2;
      VGSEL : UInt2;
      VGHBT : UInt2)
   is
      P1 : Byte;
   begin
      P1 := Interfaces.Shift_Left (Byte (VGH25), 6) or
        Interfaces.Shift_Left (Byte (VGSEL), 2) or
        Byte (VGHBT);
      Write_Command (LCD, 16#C1#, (0 => P1));
   end Set_Power_Control_2;

   -------------------------
   -- Set_Power_Control_3 --
   -------------------------

   procedure Set_Power_Control_3
     (LCD : ST7735R_Device;
      P1, P2 : Byte)
   is
   begin
      Write_Command (LCD, 16#C2#, (P1, P2));
   end Set_Power_Control_3;

   -------------------------
   -- Set_Power_Control_4 --
   -------------------------

   procedure Set_Power_Control_4
     (LCD : ST7735R_Device;
      P1, P2 : Byte)
   is
   begin
      Write_Command (LCD, 16#C3#, (P1, P2));
   end Set_Power_Control_4;

   -------------------------
   -- Set_Power_Control_5 --
   -------------------------

   procedure Set_Power_Control_5
     (LCD : ST7735R_Device;
      P1, P2 : Byte)
   is
   begin
      Write_Command (LCD, 16#C4#, (P1, P2));
   end Set_Power_Control_5;

   --------------
   -- Set_Vcom --
   --------------

   procedure Set_Vcom (LCD : ST7735R_Device; VCOMS : UInt6) is
   begin
      Write_Command (LCD, 16#C5#, (0 => Byte (VCOMS)));
   end Set_Vcom;

   ------------------------
   -- Set_Column_Address --
   ------------------------

   procedure Set_Column_Address (LCD : ST7735R_Device; X_Start, X_End : UInt16)
   is
      P1, P2, P3, P4 : Byte;
   begin
      P1 := Byte (Shift_Right (X_Start and 16#FF#, 8));
      P2 := Byte (X_Start and 16#FF#);
      P3 := Byte (Shift_Right (X_End and 16#FF#, 8));
      P4 := Byte (X_End and 16#FF#);
      Write_Command (LCD, 16#2A#, (P1, P2, P3, P4));
   end Set_Column_Address;

   ---------------------
   -- Set_Row_Address --
   ---------------------

   procedure Set_Row_Address (LCD : ST7735R_Device; Y_Start, Y_End : UInt16)
   is
      P1, P2, P3, P4 : Byte;
   begin
      P1 := Byte (Shift_Right (Y_Start and 16#FF#, 8));
      P2 := Byte (Y_Start and 16#FF#);
      P3 := Byte (Shift_Right (Y_End and 16#FF#, 8));
      P4 := Byte (Y_End and 16#FF#);
      Write_Command (LCD, 16#2B#, (P1, P2, P3, P4));
   end Set_Row_Address;

   -----------------
   -- Set_Address --
   -----------------

   procedure Set_Address (LCD : ST7735R_Device;
                          X_Start, X_End, Y_Start, Y_End : UInt16)
   is
   begin
      Set_Column_Address (LCD, X_Start, X_End);
      Set_Row_Address (LCD, Y_Start, Y_End);
   end Set_Address;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (LCD   : ST7735R_Device;
                        X, Y  : UInt16;
                        Color : UInt16)
   is
      Data : constant HAL.UInt16_Array (1 .. 1) := (1 => Color);
   begin
      Set_Address (LCD, X, X + 1, Y, Y + 1);
      Write_Raw_Pixels (LCD, Data);
   end Set_Pixel;

   ----------------------
   -- Write_Raw_Pixels --
   ----------------------

   procedure Write_Raw_Pixels (LCD  : ST7735R_Device;
                               Data : HAL.Byte_Array)
   is
   begin
      Write_Command (LCD, 16#2C#);
      Write_Data (LCD, Data);
   end Write_Raw_Pixels;

   ----------------------
   -- Write_Raw_Pixels --
   ----------------------

   procedure Write_Raw_Pixels (LCD  : ST7735R_Device;
                               Data : HAL.UInt16_Array)
   is
   begin
      Write_Command (LCD, 16#2C#);
      Write_Data (LCD, Data);
   end Write_Raw_Pixels;

   --------------------
   -- Get_Max_Layers --
   --------------------

   overriding
   function Max_Layers
     (Display : ST7735R_Device) return Positive is (1);

   ------------------
   -- Is_Supported --
   ------------------

   overriding
   function Supported
     (Display : ST7735R_Device;
      Mode    : FB_Color_Mode) return Boolean is
     (Mode = HAL.Bitmap.RGB_565);

   ---------------------
   -- Set_Orientation --
   ---------------------

   overriding
   procedure Set_Orientation
     (Display     : in out ST7735R_Device;
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
     (Display : in out ST7735R_Device;
      Mode    : Wait_Mode)
   is
   begin
      null;
   end Set_Mode;

   ---------------
   -- Get_Width --
   ---------------

   overriding
   function Width
     (Display : ST7735R_Device) return Positive is (Screen_Width);

   ----------------
   -- Get_Height --
   ----------------

   overriding
   function Height
     (Display : ST7735R_Device) return Positive is (Screen_Height);

   ----------------
   -- Is_Swapped --
   ----------------

   overriding
   function Swapped
     (Display : ST7735R_Device) return Boolean is (False);

   --------------------
   -- Set_Background --
   --------------------

   overriding
   procedure Set_Background
     (Display : ST7735R_Device; R, G, B : Byte)
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
     (Display : in out ST7735R_Device;
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

      Display.Layer.Actual_Width := Screen_Width;
      Display.Layer.Actual_Height := Screen_Height;
      Display.Layer.Addr := Display.Layer_Data'Address;
      Display.Layer.Actual_Color_Mode := Mode;
      Display.Layer_Initialized := True;
   end Initialize_Layer;

   -----------------
   -- Initialized --
   -----------------

   overriding
   function Initialized
     (Display : ST7735R_Device;
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
     (Display   : in out ST7735R_Device;
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
                   X_End   => Unsigned_16 (Display.Layer.Width - 1),
                   Y_Start => 0,
                   Y_End   => Unsigned_16 (Display.Layer.Height - 1));
      Display.Write_Raw_Pixels (Display.Layer_Data);
   end Update_Layer;


   -------------------
   -- Update_Layers --
   -------------------

   overriding
   procedure Update_Layers
     (Display : in out ST7735R_Device)
   is
   begin
      Display.Update_Layer (1);
   end  Update_Layers;

   --------------------
   -- Get_Color_Mode --
   --------------------

   overriding
   function Color_Mode
     (Display : ST7735R_Device;
      Layer   : Positive) return FB_Color_Mode
   is
   begin
      if Layer /= 1 then
         raise Program_Error;
      end if;
      return Display.Layer.Color_Mode;
   end Color_Mode;

   -----------------------
   -- Get_Hidden_Buffer --
   -----------------------

   overriding
   function Hidden_Buffer
     (Display : in out ST7735R_Device;
      Layer   : Positive) return not null HAL.Bitmap.Any_Bitmap_Buffer
   is
   begin
      if Layer /= 1 then
         raise Program_Error;
      end if;
      return Display.Layer'Unchecked_Access;
   end Hidden_Buffer;

   --------------------
   -- Get_Pixel_Size --
   --------------------

   overriding
   function Pixel_Size
     (Display : ST7735R_Device;
      Layer   : Positive) return Positive is (16);

end ST7735R;
