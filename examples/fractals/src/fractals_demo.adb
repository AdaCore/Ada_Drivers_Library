with STM32.Board;  use STM32.Board;
with STM32.Button; use STM32.Button;
with Cortex_M.FPU; use Cortex_M.FPU;

with HAL.Framebuffer;
with HAL.Bitmap;
with HAL.Touch_Panel;

with Bitmapped_Drawing;
with BMP_Fonts;

with Fractals; use Fractals;

procedure Fractals_Demo
is
   Max_Depth : constant Positive := 2048;
   Colors    : array (0 .. Max_Depth) of HAL.Bitmap.Bitmap_Color;

   Current_Screen : Screen;

   type Zoom_Box_Record is record
      X0 : Integer := -1;
      Y0 : Integer := -1;
      X1 : Integer := -1;
      Y1 : Integer := -1;
   end record;

   type Fractal_Array is array (Positive range <>) of Fractal_Ref;
   The_Fractals : constant Fractal_Array :=
                    (1 => Mandelbrot'Access,
                     2 => Julia'Access);
   Current      : Positive := The_Fractals'First;

   procedure Initialize_Color_Map;
   procedure Init_Screen (F : Fractal_Ref);
   function To_Coord (X, Y : Natural) return Coordinate;
   function Zoom_Area (Zoom_Box : Zoom_Box_Record) return Zoom_Box_Record;
   procedure Zoom (Zoom_Box : Zoom_Box_Record);
   procedure Draw_Zoom (Zoom_Box : Zoom_Box_Record);

   --------------------------
   -- Initialize_Color_Map --
   --------------------------

   procedure Initialize_Color_Map
   is
      use HAL;
      use type HAL.Byte;

      function Fill
        (Max   : HAL.Byte;
         Ratio : Float) return HAL.Byte;

      function Fill
        (Max   : HAL.Byte;
         Ratio : Float) return HAL.Byte
      is
      begin
         return Byte (Sqrt (Ratio) * Float (Max));
      end Fill;
   begin
      Colors := (others => (255, 0, 0, 0));

      for J in 0 .. 63 loop
         Colors (J).Red := Fill (255, Float (J) / 64.0);
      end loop;

      for J in 64 .. 2047 loop
         Colors (J).Red := 255;
         Colors (J).Green := Fill (255, Float (J - 64) / (2048.0 - 64.0));
      end loop;

      Colors (Max_Depth) := HAL.Bitmap.Black;
   end Initialize_Color_Map;

   -----------------
   -- Init_Screen --
   -----------------

   procedure Init_Screen (F : Fractal_Ref)
   is
      Res   : constant Screen := F.Default_Screen;
      LCD_W : constant Base_Float := Base_Float (Display.Get_Width);
      LCD_H : constant Base_Float := Base_Float (Display.Get_Height);
   begin
      Current_Screen := F.Default_Screen;

      --  Make sure this respects the LCD aspect ratio
      if LCD_W / LCD_H > Res.Width / Res.Height then
         Current_Screen.Width := Res.Height * LCD_W / LCD_H;
         Current_Screen.Height := Res.Height;
      else
         Current_Screen.Height := Res.Width * LCD_H / LCD_W;
         Current_Screen.Width := Res.Width;
      end if;

      Current_Screen.X0 := Res.X0 - (Current_Screen.Width - Res.Width) / 2.0;
      Current_Screen.Y0 := Res.Y0 - (Current_Screen.Height - Res.Height) / 2.0;
   end Init_Screen;

   --------------
   -- To_Coord --
   --------------

   function To_Coord (X, Y : Natural) return Coordinate
   is
   begin
      return
        (X => Current_Screen.X0 +
           Current_Screen.Width / Base_Float (Display.Get_Width) * Base_Float (X),
         Y => Current_Screen.Y0 +
           Current_Screen.Height / Base_Float (Display.Get_Height) * Base_Float (Y));
   end To_Coord;

   ---------------
   -- Zoom_Area --
   ---------------

   function Zoom_Area (Zoom_Box : Zoom_Box_Record) return Zoom_Box_Record
   is
      X0 : constant Natural := Natural'Min (Zoom_Box.X0, Zoom_Box.X1);
      Y0 : constant Natural := Natural'Min (Zoom_Box.Y0, Zoom_Box.Y1);
      W0 : constant Integer := abs (Zoom_Box.X0 - Zoom_Box.X1) + 1;
      H0 : constant Integer := abs (Zoom_Box.Y0 - Zoom_Box.Y1) + 1;
      X  : Integer;
      Y  : Integer;
      W  : Integer;
      H  : Integer;
   begin
      if W0 * Display.Get_Height > H0 * Display.Get_Width then
         --  H not big enough to keep aspect ratio
         H := (W0 * Display.Get_Height) / Display.Get_Width;
         W := W0;
      else
         W := (H0 * Display.Get_Width) / Display.Get_Height;
         H := H0;
      end if;

      X := X0 - ((W - W0) / 2);
      Y := Y0 - ((H - H0) / 2);

      return (X, Y, X + W - 1, Y + H - 1);
   end Zoom_Area;

   ----------
   -- Zoom --
   ----------

   procedure Zoom (Zoom_Box : Zoom_Box_Record)
   is
      Box : constant Zoom_Box_Record := Zoom_Area (Zoom_Box);
      W   : constant Integer := Box.X1 - Box.X0 + 1;
      H   : constant Integer := Box.Y1 - Box.Y0 + 1;
   begin
      Current_Screen.X0     := Current_Screen.X0 +
        Current_Screen.Width / Base_Float (Display.Get_Width) * Base_Float (Box.X0);
      Current_Screen.Y0     := Current_Screen.Y0 +
        Current_Screen.Height / Base_Float (Display.Get_Height) * Base_Float (Box.Y0);
      Current_Screen.Width  :=
        Current_Screen.Width / Base_Float (Display.Get_Width) * Base_Float (W);
      Current_Screen.Height :=
        Current_Screen.Height / Base_Float (Display.Get_Height) * Base_Float (H);
   end Zoom;

   ---------------
   -- Draw_Zoom --
   ---------------

   procedure Draw_Zoom (Zoom_Box : Zoom_Box_Record)
   is
      Buff : constant HAL.Bitmap.Bitmap_Buffer'Class :=
               Display.Get_Hidden_Buffer (2);
      Box  : Zoom_Box_Record := Zoom_Area (Zoom_Box);
      W    : Integer := Box.X1 - Box.X0 + 1;
      H    : Integer := Box.Y1 - Box.Y0 + 1;
   begin
      Buff.Fill (0);
      --  Draw the 'zoomed screen': the part that will be actually drawn after
      --  aspect ration correction

      --  Due to aspect ration constraints, X/YStart may be negative
      if Box.X0 < 0 then
         W := W + Box.X0;
         Box.X0 := 0;
      end if;
      if Box.Y0 < 0 then
         H := H + Box.Y0;
         Box.Y0 := 0;
      end if;
      if Box.X0 + W > Display.Get_Width then
         W := Display.Get_Width - Box.X0;
      end if;
      if Box.Y0 + H >= Display.Get_Height then
         H := Display.Get_Height - Box.Y0;
      end if;

      Buff.Fill_Rect (Color  => (Alpha => 73, others => 255),
                      X      => Box.X0,
                      Y      => Box.Y0,
                      Width  => W,
                      Height => H);
      --  Now draw the square drawn by the user
      Buff.Draw_Rect (Color  => HAL.Bitmap.White,
                      X      => Integer'Min (Zoom_Box.X0, Zoom_Box.X1),
                      Y      => Integer'Min (Zoom_Box.Y0, Zoom_Box.Y1),
                      Width  => abs (Zoom_Box.X0 - Zoom_Box.X1) + 1,
                      Height => abs (Zoom_Box.Y0 - Zoom_Box.Y1) + 1);
      Display.Update_Layer (2);
   end Draw_Zoom;

   Zoom_Box   : Zoom_Box_Record;
   First_Pass : Boolean;
   Do_Paint   : Boolean;

begin
   Display.Initialize (HAL.Framebuffer.Landscape, HAL.Framebuffer.Polling);
   Display.Initialize_Layer (1, HAL.Bitmap.RGB_888);
   Display.Initialize_Layer (2, HAL.Bitmap.ARGB_4444);
   Touch_Panel.Initialize (HAL.Framebuffer.Landscape);
   STM32.Button.Initialize;

   Initialize_Color_Map;

   Main_Loop :
   loop
      Init_Screen (The_Fractals (Current));
      Same_Fractal_Loop :
      loop
         Display.Get_Hidden_Buffer (1).Fill (0);
         Display.Update_Layer (1, True);
         Display.Get_Hidden_Buffer (2).Fill (0);
         Display.Get_Hidden_Buffer (2).Wait_Transfer;
         Bitmapped_Drawing.Draw_String
           (Display.Get_Hidden_Buffer (2),
            Start      => (X => Display.Get_Width / 2 - 112,
                           Y => Display.Get_Height / 2 - 12),
            Msg        => "Calculating...",
            Font       => BMP_Fonts.Font16x24,
            Foreground => (128, 255, 255, 255),
            Background => HAL.Bitmap.Transparent);
         Display.Update_Layer (2, True);

         First_Pass := True;
         for J in reverse 0 .. 3 loop
            declare
               Size : constant Natural := 2 ** J;
            begin
               for Y in 0 .. Display.Get_Height / Size - 1 loop
                  declare
                     use HAL;
                     use type HAL.UInt32;
                     Buff : constant HAL.Bitmap.Bitmap_Buffer'Class :=
                              Display.Get_Hidden_Buffer (1);
                     Col  : UInt32;
                  begin
                     for X in 0 .. Display.Get_Width / Size - 1 loop
                        if First_Pass then
                           Do_Paint := True;
                        else
                           Do_Paint := False;

                           if X mod 2 = 0 and then Y mod 2 = 0 then
                              --  Already calculated in the previous pass
                              Do_Paint := False;

                           elsif X = 0
                             or else Y = 0
                             or else (X + 1) * Size >= Display.Get_Width
                             or else (Y + 1) * Size >= Display.Get_Height
                           then
                              --  Always draw borders
                              Do_Paint := True;

                           else
                              Col := Buff.Get_Pixel (X * Size, Y * Size);
                              Do_Paint := False;

                              if Buff.Get_Pixel ((X - 1) * Size,
                                                 (Y - 1) * Size) /= Col
                                or else Buff.Get_Pixel ((X + 1) * Size,
                                                        (Y + 1) * Size) /= Col
                                or else Buff.Get_Pixel ((X + 1) * Size,
                                                        (Y - 1) * Size) /= Col
                                or else Buff.Get_Pixel ((X - 1) * Size,
                                                        (Y + 1) * Size) /= Col
                                or else Buff.Get_Pixel (X * Size,
                                                        (Y - 1) * Size) /= Col
                                or else Buff.Get_Pixel ((X - 1) * Size,
                                                        Y * Size) /= Col
                                or else Buff.Get_Pixel (X * Size,
                                                        (Y + 1) * Size) /= Col
                                or else Buff.Get_Pixel ((X + 1) * Size,
                                                        Y * Size) /= Col
                              then
                                 Do_Paint := True;
                              end if;
                           end if;
                        end if;

                        if Do_Paint then
                           declare
                              Iter : constant Natural :=
                                       The_Fractals (Current).Compute
                                         (To_Coord (X * Size, Y * Size),
                                          Max_Depth);
                           begin
                              if Size > 1 then
                                 Buff.Fill_Rect (Colors (Iter),
                                                 X * Size,
                                                 Y * Size,
                                                 Size,
                                                 Size);
                              else
                                 Buff.Set_Pixel (X, Y, Colors (Iter));
                              end if;
                           end;
                        end if;
                     end loop;
                  end;

                  Display.Update_Layer (1, True);

                  if STM32.Button.Has_Been_Pressed then
                     exit Same_Fractal_Loop;
                  end if;
               end loop;

               First_Pass := False;
            end;
         end loop;

         Display.Get_Hidden_Buffer (2).Fill (0);
         Display.Update_Layer (2);

         Zoom_Loop :
         loop
            declare
               use type HAL.Touch_Panel.TP_Touch_State;
               State : constant HAL.Touch_Panel.TP_Touch_State :=
                         Touch_Panel.Get_Touch_Point (1);
            begin
               if State = HAL.Touch_Panel.Null_Touch_State then
                  if Zoom_Box.X1 /= -1 then
                     Zoom (Zoom_Box);
                     Zoom_Box.X0 := -1;
                     Zoom_Box.X1 := -1;
                     exit Zoom_Loop;
                  end if;

               elsif Zoom_Box.X0 = -1 then
                  Zoom_Box.X0 := State.X;
                  Zoom_Box.Y0 := State.Y;
                  Zoom_Box.X1   := -1;

               elsif abs (State.X - Zoom_Box.X0) > 10
                 and then abs (State.Y - Zoom_Box.Y0) > 10
               then
                  Zoom_Box.X1 := State.X;
                  Zoom_Box.Y1 := State.Y;
                  Draw_Zoom (Zoom_Box);
               else
                  Zoom_Box.X1 := -1;
               end if;
            end;

            if STM32.Button.Has_Been_Pressed then
               exit Same_Fractal_Loop;
            end if;
         end loop Zoom_Loop;
      end loop Same_Fractal_Loop;

      --  Move on to the next fractal
      Current := Current + 1;

      if Current > The_Fractals'Last then
         Current := The_Fractals'First;
      end if;
   end loop Main_Loop;

end Fractals_Demo;
