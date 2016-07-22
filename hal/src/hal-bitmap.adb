with System.Storage_Elements; use System.Storage_Elements;
with Interfaces;              use Interfaces;

package body HAL.Bitmap is

   procedure Handle_Swap
     (Buffer : Bitmap_Buffer'Class;
      X      : in out Natural;
      Y      : in out Natural);

   -----------------
   -- Handle_Swap --
   -----------------

   procedure Handle_Swap
     (Buffer : Bitmap_Buffer'Class;
      X      : in out Natural;
      Y      : in out Natural)
   is
      Tmp : Natural;
   begin
      if not Buffer.Swapped then
         return;
      end if;

      Tmp := X;
      X := Y;
      Y := Buffer.Width - Tmp - 1;
   end Handle_Swap;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel
     (Buffer  : Bitmap_Buffer;
      X       : Natural;
      Y       : Natural;
      Value   : Bitmap_Color)
   is
      Col : constant Word :=
              Bitmap_Color_To_Word (Buffer.Color_Mode, Value);
   begin
      Set_Pixel (Bitmap_Buffer'Class (Buffer), X, Y, Col);
   end Set_Pixel;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel
     (Buffer : Bitmap_Buffer;
      X      : Natural;
      Y      : Natural;
      Value  : Word)
   is
      X0 : Natural := X;
      Y0 : Natural := Y;
      Offset : Natural;

   begin
      if X >= Buffer.Width
        or else Y >= Buffer.Height
      then
         return;
      end if;

      if Buffer.Swapped then
         Handle_Swap (Buffer, X0, Y0);
         Offset := X0 + Y0 * Buffer.Height;

      else
         Offset := X + Y * Buffer.Width;
      end if;

      case Buffer.Color_Mode is
         when ARGB_8888 =>
            declare
               Pixel : aliased Word
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 4);
            begin
               Pixel := Value;
            end;

         when RGB_888 =>
            declare
               Pixel_B : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3);
               Pixel_G : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3 + 1);
               Pixel_R : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3 + 2);
               R       : constant Byte :=
                           Byte (Shift_Right (Value and 16#FF0000#, 16));
               G       : constant Byte :=
                           Byte (Shift_Right (Value and 16#FF00#, 8));
               B       : constant Byte := Byte (Value and 16#FF#);
            begin
               Pixel_B := B;
               Pixel_G := G;
               Pixel_R := R;
            end;

         when ARGB_1555 | ARGB_4444 | RGB_565 | AL_88 =>
            declare
               Pixel : aliased Short
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 2);
            begin
               Pixel := Short (Value and 16#FF_FF#);
            end;

         when L_8 | AL_44 | A_8 =>
            declare
               Pixel : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset);
            begin
               Pixel := Byte (Value and 16#FF#);
            end;

         when L_4 | A_4 =>
            declare
               Pixel : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset / 2);
            begin
               if Offset mod 2 = 0 then
                  Pixel :=
                    (Pixel and 16#0F#) or
                    Shift_Left (Byte (Value and 16#0F#), 4);
               else
                  Pixel := (Pixel and 16#F0#) or Byte (Value and 16#0F#);
               end if;
            end;

      end case;
   end Set_Pixel;

   ---------------------
   -- Set_Pixel_Blend --
   ---------------------

   procedure Set_Pixel_Blend
     (Buffer : Bitmap_Buffer;
      X      : Natural;
      Y      : Natural;
      Value  : Bitmap_Color)
   is
      Col : Bitmap_Color;
      FgA, FgR, FgG, FgB : Float;
      BgA, BgR, BgG, BgB : Float;
      RA, RR, RG, RB     : Float;

   begin
      if Value.Alpha = 255 then
         Set_Pixel (Bitmap_Buffer'Class (Buffer), X, Y, Value);
      else
         Col := Get_Pixel (Bitmap_Buffer'Class (Buffer), X, Y);
         BgA := Float (Col.Alpha) / 255.0;
         BgR := Float (Col.Red) / 255.0;
         BgG := Float (Col.Green) / 255.0;
         BgB := Float (Col.Blue) / 255.0;

         FgA := Float (Value.Alpha) / 255.0;
         FgR := Float (Value.Red) / 255.0;
         FgG := Float (Value.Green) / 255.0;
         FgB := Float (Value.Blue) / 255.0;

         RA := 1.0 - (1.0 - FgA) * (1.0 - FgB);
         RR := FgR * FgA / RA + BgR * BgA * (1.0 - FgA) / RA;
         RG := FgG * FgA / RA + BgG * BgA * (1.0 - FgA) / RA;
         RB := FgB * FgA / RA + BgB * BgA * (1.0 - FgA) / RA;

         Col := (Alpha => Byte (RA * 255.0),
                 Red   => Byte (RR * 255.0),
                 Green => Byte (RG * 255.0),
                 Blue  => Byte (RB * 255.0));
         Set_Pixel (Bitmap_Buffer'Class (Buffer), X, Y, Col);
      end if;
   end Set_Pixel_Blend;

   ---------------
   -- Get_Pixel --
   ---------------

   function Get_Pixel
     (Buffer : Bitmap_Buffer;
      X      : Natural;
      Y      : Natural)
      return Bitmap_Color
   is
      Native_Color : Word;
   begin
      Native_Color := Get_Pixel
        (Bitmap_Buffer'Class (Buffer),
         X, Y);

      return Word_To_Bitmap_Color (Buffer.Color_Mode, Native_Color);
   end Get_Pixel;

   ---------------
   -- Get_Pixel --
   ---------------

   function Get_Pixel
     (Buffer : Bitmap_Buffer;
      X      : Natural;
      Y      : Natural)
      return Word
   is
      X0 : Natural := X;
      Y0 : Natural := Y;
      Offset : Natural;

   begin
      if Buffer.Swapped then
         Handle_Swap (Buffer, X0, Y0);
         Offset := X0 + Y0 * Buffer.Height;

      else
         Offset := X + Y * Buffer.Width;
      end if;

      case Buffer.Color_Mode is
         when ARGB_8888 =>
            declare
               Pixel : aliased Word
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 4);
            begin
               return Pixel;
            end;

         when RGB_888 =>
            declare
               Pixel_B : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3);
               Pixel_G : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3 + 1);
               Pixel_R : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3 + 2);
            begin
               return Shift_Left (Word (Pixel_R), 16)
                 or Shift_Left (Word (Pixel_G), 8) or Word (Pixel_B);
            end;

         when ARGB_1555 | ARGB_4444 | RGB_565 | AL_88 =>
            declare
               Pixel : aliased Short
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 2);
            begin
               return Word (Pixel);
            end;

         when L_8 | AL_44 | A_8 =>
            declare
               Pixel : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset);
            begin
               return Word (Pixel);
            end;

         when L_4 | A_4 =>
            declare
               Pixel : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset / 2);
            begin
               if Offset mod 2 = 0 then
                  return Word (Shift_Right (Pixel and 16#F0#, 4));
               else
                  return Word (Pixel and 16#0F#);
               end if;
            end;
      end case;
   end Get_Pixel;

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Buffer : Bitmap_Buffer;
      Color  : Bitmap_Color)
   is
      Col : constant Word := Bitmap_Color_To_Word (Buffer.Color_Mode, Color);
   begin
      Fill (Bitmap_Buffer'Class (Buffer), Col);
   end Fill;

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Buffer : Bitmap_Buffer;
      Color  : Word)
   is
   begin
      for Y in 0 .. Buffer.Height - 1 loop
         for X in 0 .. Buffer.Width - 1 loop
            Set_Pixel (Buffer, X, Y, Color);
         end loop;
      end loop;
   end Fill;

   ---------------
   -- Fill_Rect --
   ---------------

   procedure Fill_Rect
     (Buffer : Bitmap_Buffer;
      Color  : Bitmap_Color;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer)
   is
   begin
      Fill_Rect
        (Bitmap_Buffer'Class (Buffer),
         Bitmap_Color_To_Word (Buffer.Color_Mode, Color),
         X, Y, Width, Height);
   end Fill_Rect;

   ---------------
   -- Fill_Rect --
   ---------------

   procedure Fill_Rect
     (Buffer : Bitmap_Buffer;
      Color  : Word;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer)
   is
   begin
      for Y0 in Y .. Y + Height - 1 loop
         for X0 in X .. X + Width - 1 loop
            Set_Pixel (Buffer, X0, Y0, Color);
         end loop;
      end loop;
   end Fill_Rect;

   ---------------
   -- Copy_Rect --
   ---------------

   procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      X_Src       : Natural;
      Y_Src       : Natural;
      Dst_Buffer  : Bitmap_Buffer;
      X_Dst       : Natural;
      Y_Dst       : Natural;
      Bg_Buffer   : Bitmap_Buffer'Class;
      X_Bg        : Natural;
      Y_Bg        : Natural;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
   begin
      raise Constraint_Error with "Not implemented yet.";
   end Copy_Rect;

   ---------------
   -- Copy_Rect --
   ---------------

   procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      X_Src       : Natural;
      Y_Src       : Natural;
      Dst_Buffer  : Bitmap_Buffer;
      X_Dst       : Natural;
      Y_Dst       : Natural;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
      Null_Buffer : Bitmap_Buffer'Class := Src_Buffer;
   begin
      Null_Buffer.Addr := System.Null_Address;
      Null_Buffer.Width := 0;
      Null_Buffer.Height := 0;
      Null_Buffer.Swapped := False;

      Copy_Rect
        (Src_Buffer  => Src_Buffer,
         X_Src       => X_Src,
         Y_Src       => Y_Src,
         Dst_Buffer  => Bitmap_Buffer'Class (Dst_Buffer),
         X_Dst       => X_Dst,
         Y_Dst       => Y_Dst,
         Bg_Buffer   => Null_Buffer,
         X_Bg        => 0,
         Y_Bg        => 0,
         Width       => Width,
         Height      => Height,
         Synchronous => Synchronous);
   end Copy_Rect;

   ---------------------
   -- Copy_Rect_Blend --
   ---------------------

   procedure Copy_Rect_Blend
     (Src_Buffer  : Bitmap_Buffer;
      X_Src       : Natural;
      Y_Src       : Natural;
      Dst_Buffer  : Bitmap_Buffer'Class;
      X_Dst       : Natural;
      Y_Dst       : Natural;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
   begin
      Copy_Rect
        (Src_Buffer  => Bitmap_Buffer'Class (Src_Buffer),
         X_Src       => X_Src,
         Y_Src       => Y_Src,
         Dst_Buffer  => Dst_Buffer,
         X_Dst       => X_Dst,
         Y_Dst       => Y_Dst,
         Bg_Buffer   => Dst_Buffer,
         X_Bg        => X_Dst,
         Y_Bg        => Y_Dst,
         Width       => Width,
         Height      => Height,
         Synchronous => Synchronous);
   end Copy_Rect_Blend;

   ------------------------
   -- Draw_Vertical_Line --
   ------------------------

   procedure Draw_Vertical_Line
     (Buffer : Bitmap_Buffer;
      Color  : Word;
      X      : Integer;
      Y      : Integer;
      Height : Integer)
   is
   begin
      Fill_Rect (Bitmap_Buffer'Class (Buffer), Color, X, Y, 1, Height);
   end Draw_Vertical_Line;

   ------------------------
   -- Draw_Vertical_Line --
   ------------------------

   procedure Draw_Vertical_Line
     (Buffer : Bitmap_Buffer;
      Color  : Bitmap_Color;
      X      : Integer;
      Y      : Integer;
      Height : Integer)
   is
   begin
      Fill_Rect (Bitmap_Buffer'Class (Buffer), Color, X, Y, 1, Height);
   end Draw_Vertical_Line;

   --------------------------
   -- Draw_Horizontal_Line --
   --------------------------

   procedure Draw_Horizontal_Line
     (Buffer : Bitmap_Buffer;
      Color  : Word;
      X      : Integer;
      Y      : Integer;
      Width  : Integer)
   is
   begin
      Fill_Rect (Bitmap_Buffer'Class (Buffer), Color, X, Y, Width, 1);
   end Draw_Horizontal_Line;

   --------------------------
   -- Draw_Horizontal_Line --
   --------------------------

   procedure Draw_Horizontal_Line
     (Buffer : Bitmap_Buffer;
      Color  : Bitmap_Color;
      X      : Integer;
      Y      : Integer;
      Width  : Integer)
   is
   begin
      Fill_Rect (Bitmap_Buffer'Class (Buffer), Color, X, Y, Width, 1);
   end Draw_Horizontal_Line;

   ---------------
   -- Draw_Rect --
   ---------------

   procedure Draw_Rect
     (Buffer : Bitmap_Buffer;
      Color  : Bitmap_Color;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer)
   is
      Buf2 : Bitmap_Buffer'Class := Buffer;
      X0   : Integer := X;
      Y0   : Integer := Y;
      W    : Integer := Width;
      H    : Integer := Height;

   begin
      if Buffer.Swapped then
         W := Height;
         H := Width;
         X0 := Y;
         Y0 := Buffer.Width - X - Width;
         Buf2.Width   := Buffer.Height;
         Buf2.Height  := Buffer.Width;
         Buf2.Swapped := False;
      end if;

      Draw_Horizontal_Line (Buf2, Color, X0, Y0, W);
      Draw_Horizontal_Line (Buf2, Color, X0, Y0 + H - 1, W);
      Draw_Vertical_Line (Buf2, Color, X0, Y0, H);
      Draw_Vertical_Line (Buf2, Color, X0 + W - 1, Y0, H);
   end Draw_Rect;

   -----------------
   -- Buffer_Size --
   -----------------

   function Buffer_Size (Buffer : Bitmap_Buffer) return Natural
   is
   begin
      return Bits_Per_Pixel (Buffer.Color_Mode) *
        Buffer.Width * Buffer.Height / 8;
   end Buffer_Size;

   --------------------------
   -- Bitmap_Color_To_Word --
   --------------------------

   function Bitmap_Color_To_Word
     (Mode : Bitmap_Color_Mode; Col : Bitmap_Color)
      return Word
   is
      Ret : Word := 0;

      procedure Add_Byte
        (Value : Byte; Pos : Natural; Size : Positive) with Inline;

      function Luminance return Byte;

      --------------
      -- Add_Byte --
      --------------

      procedure Add_Byte
        (Value : Byte; Pos : Natural; Size : Positive)
      is
         Val : constant Word :=
                 Shift_Left
                   (Word
                      (Shift_Right (Value,
                                    abs (Integer (Size) - 8))),
                    Pos);
      begin
         Ret := Ret or Val;
      end Add_Byte;

      ---------------
      -- Luminance --
      ---------------

      function Luminance return Byte
      is
      begin
         return Byte
           (Shift_Right
              (Word (Col.Red) * 3 + Word (Col.Blue) + Word (Col.Green) * 4,
               3));
      end Luminance;

   begin
      case Mode is
         when ARGB_8888 =>
            Add_Byte (Col.Alpha, 24, 8);
            Add_Byte (Col.Red,   16, 8);
            Add_Byte (Col.Green,  8, 8);
            Add_Byte (Col.Blue,   0, 8);

         when RGB_888 =>
            Add_Byte (Col.Red,   16, 8);
            Add_Byte (Col.Green,  8, 8);
            Add_Byte (Col.Blue,   0, 8);

         when RGB_565 =>
            Add_Byte (Col.Red,   11, 5);
            Add_Byte (Col.Green,  5, 6);
            Add_Byte (Col.Blue,   0, 5);

         when ARGB_1555 =>
            Add_Byte (Col.Alpha, 15, 1);
            Add_Byte (Col.Red,   10, 5);
            Add_Byte (Col.Green,  5, 5);
            Add_Byte (Col.Blue,   0, 5);

         when ARGB_4444 =>
            Add_Byte (Col.Alpha, 12, 4);
            Add_Byte (Col.Red,    8, 4);
            Add_Byte (Col.Green,  4, 4);
            Add_Byte (Col.Blue,   0, 4);

         when L_8 =>
            Add_Byte (Luminance, 0, 8);

         when AL_44 =>
            Add_Byte (Col.Alpha, 4, 4);
            Add_Byte (Luminance, 0, 4);

         when AL_88 =>
            Add_Byte (Col.Alpha, 8, 8);
            Add_Byte (Luminance, 0, 8);

         when L_4 =>
            Add_Byte (Luminance, 0, 4);

         when A_8 =>
            Add_Byte (Col.Alpha, 0, 8);

         when A_4 =>
            Add_Byte (Col.Alpha, 0, 4);
      end case;

      return Ret;
   end Bitmap_Color_To_Word;

   --------------------------
   -- Word_To_Bitmap_Color --
   --------------------------

   function Word_To_Bitmap_Color
     (Mode : Bitmap_Color_Mode; Col : Word)
      return Bitmap_Color
   is

      function Get_Byte
        (Pos : Natural; Size : Positive) return Byte with Inline;

      --------------
      -- Get_Byte --
      --------------

      function Get_Byte
        (Pos : Natural; Size : Positive) return Byte
      is
         Ret : Byte;
         Mask : constant Word := Shift_Left (2 ** Size - 1, Pos);
      begin
         Ret := Byte (Shift_Right (Col and Mask, Pos));

         if Size = 8 then
            return Ret;
         elsif Size = 1 then
            return (if Ret > 0 then 255 else 0);
         elsif Size >= 4 then
            --  return [7..3] => Ret[4 .. 0], [2 .. 0] => Ret[4 .. 2]
            return Shift_Left (Ret, 8 - Size) or
              Shift_Right (Ret, 2 * Size - 8);
         else
            raise Constraint_Error with "Unsupported color component size";
         end if;
      end Get_Byte;

      A, R, G, B : Byte;
   begin
      case Mode is
         when ARGB_8888 =>
            A := Get_Byte (24, 8);
            R := Get_Byte (16, 8);
            G := Get_Byte (8, 8);
            B := Get_Byte (0, 8);

         when RGB_888 =>
            A := 255;
            R := Get_Byte (16, 8);
            G := Get_Byte (8, 8);
            B := Get_Byte (0, 8);

         when RGB_565 =>
            A := 255;
            R := Get_Byte (11, 5);
            G := Get_Byte (5, 6);
            B := Get_Byte (0, 5);

         when ARGB_1555 =>
            A := Get_Byte (15, 1);
            R := Get_Byte (10, 5);
            G := Get_Byte (5, 5);
            B := Get_Byte (0, 5);

         when ARGB_4444 =>
            A := Get_Byte (12, 4);
            R := Get_Byte (8, 4);
            G := Get_Byte (4, 4);
            B := Get_Byte (0, 4);

         when L_8 =>
            A := 255;
            R := Get_Byte (0, 8);
            G := R;
            B := R;

         when AL_44 =>
            A := Get_Byte (4, 4);
            R := Get_Byte (0, 4);
            G := R;
            B := R;

         when AL_88 =>
            A := Get_Byte (8, 8);
            R := Get_Byte (0, 8);
            G := R;
            B := R;

         when L_4 =>
            A := 255;
            R := Get_Byte (0, 4);
            G := R;
            B := R;

         when A_8 =>
            A := Get_Byte (0, 8);
            R := 255;
            G := 255;
            B := 255;

         when A_4 =>
            A := Get_Byte (0, 4);
            R := 255;
            G := 255;
            B := 255;
      end case;

      return (Alpha => A,
              Red   => R,
              Green => G,
              Blue  => B);
   end Word_To_Bitmap_Color;

   -------------------
   -- Wait_Transfer --
   -------------------

   procedure Wait_Transfer (Buffer : Bitmap_Buffer)
   is null;

end HAL.Bitmap;
