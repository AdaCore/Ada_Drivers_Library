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

with Game;

with STM32.Board;          use STM32.Board;

package body Status is

   G_Pct       : Natural := 200;
   Null_Rect   : constant Rect :=
                   (Position => (0, 0), Width => 0, Height => 0);
   G_Area      : Rect := Null_Rect;
   Prog_Area   : Rect := Null_Rect;
   Score_Area  : Rect := Null_Rect;
   High_Area   : Rect := Null_Rect;
   Btn_Area    : Rect := Null_Rect;
   Margin      : Natural := 5;

   FG          : constant Bitmap_Color := (255, others => 80);
   Box_BG      : constant Bitmap_Color :=
                   (Alpha => 255,
                    Red   => 187,
                    Green => 173,
                    Blue  => 160);
   Box_FG      : constant Bitmap_Color := (others => 255);
   Static_FG   : constant Bitmap_Color :=
                   (Alpha => 255,
                    Red   => 230,
                    Green => 213,
                    Blue  => 197);

   G_High_Score : Integer := -1;

   procedure Draw_Button
     (Buffer  : Bitmap_Buffer'Class;
      Area    : Rect;
      Label   : String;
      State   : Boolean;
      Rounded : Boolean);

   ---------------
   -- Init_Area --
   ---------------

   procedure Init_Area (Buffer : HAL.Bitmap.Bitmap_Buffer'Class)
   is
      Small_Progress_Bar : Boolean := False;

   begin
      if G_Area /= Null_Rect then
         return;
      end if;

      G_Area := Game.Get_Status_Area;

      if G_Area.Height > G_Area.Width then
         if G_Area.Height > 400 then
            --  STM32F469
            Margin := 9;
            Prog_Area := (Position => (Margin + 2,
                                       Margin + 2),
                          Width    => G_Area.Width - 2 * Margin - 4,
                          Height   => 8);
            Score_Area :=
              (Position => (Margin,
                            Prog_Area.Position.Y + Prog_Area.Height + Margin),
               Width    => G_Area.Width - 2 * Margin,
               Height   => 100);
            High_Area :=
              (Position => (Margin,
                            Score_Area.Position.Y +
                              Score_Area.Height + Margin),
               Width    => G_Area.Width - 2 * Margin,
               Height   => 100);
            Btn_Area :=
              (Position => (Margin,
                            High_Area.Position.Y + High_Area.Height +
                              (G_Area.Height - High_Area.Position.Y -
                                 High_Area.Height - 20) / 2),
               Width    => G_Area.Width - 2 * Margin,
               Height   => 60);
         else
            --  STM32F7
            Prog_Area := (Position => (Margin + 2,
                                       Margin + 2),
                          Width    => G_Area.Width - 2 * Margin - 4,
                          Height   => 4);
            Score_Area :=
              (Position => (Margin,
                            Prog_Area.Position.Y + Prog_Area.Height + Margin),
               Width    => G_Area.Width - 2 * Margin,
               Height   => 54);
            High_Area :=
              (Position => (Margin,
                            Score_Area.Position.Y + Score_Area.Height +
                              Margin),
               Width    => G_Area.Width - 2 * Margin,
               Height   => 54);
            Btn_Area :=
              (Position => (Margin,
                            High_Area.Position.Y + High_Area.Height +
                              (G_Area.Height - High_Area.Position.Y -
                                 High_Area.Height - 40) / 2),
               Width    => G_Area.Width - 2 * Margin,
               Height   => 36);
         end if;
      else
         --  STM32F429
         Margin := 2;
         Prog_Area := (Position => (Margin, 1),
                       Width    => G_Area.Width - 2 * Margin,
                       Height   => 4);
         Small_Progress_Bar := True;
         Score_Area :=
           (Position => (Margin,
                         Prog_Area.Position.Y + Prog_Area.Height + 2),
            Width    => G_Area.Width / 2 - 2 * Margin,
            Height   => 40);
         High_Area :=
           (Position => (Score_Area.Position.X + Score_Area.Width + Margin,
                         Score_Area.Position.Y),
            Width    => G_Area.Width / 2 - Margin,
            Height   => 40);
         Btn_Area :=
           (Position => (Margin + 50,
                         Score_Area.Position.Y + Score_Area.Height + Margin),
            Width    => G_Area.Width - 2 * Margin - 100,
            Height   => 29);
      end if;

      --  Setup the progress bar border: This is drawn in the background layer
      --  so that we don't have to redraw it
      if not Small_Progress_Bar then
         Buffer.Fill_Rect
           (Color  => FG,
            X      => G_Area.Position.X + Prog_Area.Position.X - 2,
            Y      => G_Area.Position.Y + Prog_Area.Position.Y - 2,
            Width  => Prog_Area.Width + 4,
            Height => Prog_Area.Height + 4);
         Buffer.Fill_Rect
           (Color  => Transparent,
            X      => G_Area.Position.X + Prog_Area.Position.X - 1,
            Y      => G_Area.Position.Y + Prog_Area.Position.Y - 1,
            Width  => Prog_Area.Width + 2,
            Height => Prog_Area.Height + 2);
      end if;

      --  Setup the Score area
      Fill_Rounded_Rectangle
        (Buffer,
         Hue    => Box_BG,
         X      => G_Area.Position.X + Score_Area.Position.X,
         Y      => G_Area.Position.Y + Score_Area.Position.Y,
         Width  => Score_Area.Width,
         Height => Score_Area.Height,
         Radius => Margin);
      Draw_String
        (Buffer,
         Area       => (G_Area.Position + Score_Area.Position,
                        Score_Area.Width,
                        Score_Area.Height / 3),
         Msg        => "Score",
         Font       => Game.Times,
         Bold       => False,
         Outline    => False,
         Foreground => Static_FG,
         Fast       => False);

      --  Setup the High Score area
      Fill_Rounded_Rectangle
        (Buffer,
         Hue    => Box_BG,
         X      => G_Area.Position.X + High_Area.Position.X,
         Y      => G_Area.Position.Y + High_Area.Position.Y,
         Width  => High_Area.Width,
         Height => High_Area.Height,
         Radius => Margin);
      Draw_String
        (Buffer,
         Area       => (G_Area.Position + High_Area.Position,
                        High_Area.Width,
                        High_Area.Height / 3),
         Msg        => "High Score",
         Font       => Game.Times,
         Bold       => False,
         Outline    => False,
         Foreground => Static_FG,
         Fast       => False);
   end Init_Area;

   -----------------
   -- Has_Buttons --
   -----------------

   function Has_Buttons return Boolean is
   begin
      return Btn_Area /= Null_Rect;
   end Has_Buttons;

   ---------------------------
   -- Get_Autoplay_Btn_Area --
   ---------------------------

   function Get_Autoplay_Btn_Area return Rect
   is
   begin
      return (Position => Btn_Area.Position + G_Area.Position,
              Width    => Btn_Area.Width,
              Height   => Btn_Area.Height);
   end Get_Autoplay_Btn_Area;

   -----------------
   -- Draw_Button --
   -----------------

   procedure Draw_Button
     (Buffer  : Bitmap_Buffer'Class;
      Area    : Rect;
      Label   : String;
      State   : Boolean;
      Rounded : Boolean)
   is
      FG     : Bitmap_Color;
      BG     : Bitmap_Color;
      Border : Bitmap_Color;
      Top    : Bitmap_Color;
      Bottom : Bitmap_Color;
      Shadow : Natural;

   begin
      if State then
         FG     := (255, 100, 100, 100);
         BG     := (255, 160, 160, 160);
         Top    := (255, 120, 120, 120);
         Bottom := (255, 130, 130, 130);
         Border := (255, 110, 110, 110);
      else
         FG     := (255, 160, 160, 160);
         BG     := (255, 220, 220, 220);
         Top    := (255, 250, 250, 250);
         Bottom := (255, 200, 200, 200);
         Border := (255, 150, 150, 150);
      end if;

      if Area.Height > 50 then
         Shadow := 3;
      else
         Shadow := 2;
      end if;

      if Rounded then
         Fill_Rounded_Rectangle
           (Buffer,
            Hue    => Top,
            X      => Area.Position.X,
            Y      => Area.Position.Y,
            Width  => Area.Width,
            Height => Area.Height - Shadow,
            Radius => Margin);
         Fill_Rounded_Rectangle
           (Buffer,
            Hue    => Bottom,
            X      => Area.Position.X,
            Y      => Area.Position.Y + Shadow,
            Width  => Area.Width,
            Height => Area.Height - Shadow,
            Radius => Margin);
         Fill_Rounded_Rectangle
           (Buffer,
            Hue    => BG,
            X      => Area.Position.X,
            Y      => Area.Position.Y + Shadow,
            Width  => Area.Width,
            Height => Area.Height - 2 * Shadow,
            Radius => Margin);
         Draw_Rounded_Rectangle
           (Buffer,
            Hue       => Border,
            Area      => Area,
            Radius    => Margin,
            Thickness => 1);
      else
         Buffer.Fill_Rect
           (Color  => Top,
            X      => Area.Position.X,
            Y      => Area.Position.Y,
            Width  => Area.Width,
            Height => Shadow);
         Buffer.Fill_Rect
           (Color  => Bottom,
            X      => Area.Position.X,
            Y      => Area.Position.Y + Area.Height - Shadow - 1,
            Width  => Area.Width,
            Height => Shadow);
         Buffer.Fill_Rect
           (Color  => BG,
            X      => Area.Position.X,
            Y      => Area.Position.Y + Shadow,
            Width  => Area.Width,
            Height => Area.Height - 2 * Shadow);
         Buffer.Draw_Rect
           (Color  => Border,
            X      => Area.Position.X,
            Y      => Area.Position.Y,
            Width  => Area.Width,
            Height => Area.Height);
      end if;

      Draw_String
        (Buffer,
         Area       => ((Area.Position.X + Margin,
                         Area.Position.Y + Area.Height / 6),
                        Area.Width - 2 * Margin,
                        Area.Height * 3 / 4),
         Msg        => Label,
         Font       => Game.Times,
         Bold       => False,
         Outline    => False,
         Foreground => FG,
         Fast       => False);
   end Draw_Button;

   ------------------
   -- Set_Autoplay --
   ------------------

   procedure Set_Autoplay
     (State : Boolean)
   is
   begin
      if Btn_Area = Null_Rect then
         return;
      end if;

      Draw_Button
        (Display.Get_Hidden_Buffer (2), Btn_Area, "Auto Play", State, True);
   end Set_Autoplay;

   --------------
   -- Progress --
   --------------

   procedure Progress (Pct : Float)
   is
      N_Pct  : constant Natural := Natural (Pct * 10.0) * 10;
      Buffer : constant Bitmap_Buffer'Class :=
                 Display.Get_Hidden_Buffer (2);
      Max_W  : Natural;
      W      : Natural;

   begin
      if N_Pct = G_Pct then
         return;
      end if;

      Max_W := Prog_Area.Width;
      W     := Natural (Float (Max_W) * Pct);

      if W > 0 then
         Buffer.Fill_Rect
           (Color  => FG,
            X      => Prog_Area.Position.X,
            Y      => Prog_Area.Position.Y,
            Width  => W,
            Height => Prog_Area.Height);
      end if;

      G_Pct := N_Pct;

      Display.Update_Layer (2);
   end Progress;

   --------------------
   -- Clear_Progress --
   --------------------

   procedure Clear_Progress
   is
      Buffer : constant Bitmap_Buffer'Class :=
                 Display.Get_Hidden_Buffer (2);
   begin
      if G_Pct = 0 then
         return;
      end if;

      G_Pct := 0;
      Buffer.Fill_Rect
        (Color  => Transparent,
         X      => Prog_Area.Position.X,
         Y      => Prog_Area.Position.Y,
         Width  => Prog_Area.Width,
         Height => Prog_Area.Height);
      Display.Update_Layer (2, True);
   end Clear_Progress;

   ---------------
   -- Set_Score --
   ---------------

   procedure Set_Score (Score : Natural)
   is
      Img         : constant String := Score'Img;
      Area, AreaH : Rect;
      Buf1        : constant Bitmap_Buffer'Class :=
                      Display.Get_Hidden_Buffer (2);

   begin
      Area.Position := (Score_Area.Position.X + 10,
                        Score_Area.Position.Y + Score_Area.Height / 3 + 2);
      Area.Width    := Score_Area.Width - 20;
      Area.Height   := Score_Area.Height * 2 / 3 - 2;

      Buf1.Fill_Rect
        (Color  => Transparent,
         X      => Area.Position.X,
         Y      => Area.Position.Y,
         Width  => Area.Width,
         Height => Area.Height);
      Draw_String
        (Buf1,
         Area       => Area,
         Msg        => Img (Img'First + 1 .. Img'Last),
         Font       => Game.Times,
         Bold       => True,
         Outline    => False,
         Foreground => Box_FG,
         Fast       => True);

      if Score >= G_High_Score then
         G_High_Score := Score;
         AreaH.Position := (High_Area.Position.X + 10,
                            High_Area.Position.Y + High_Area.Height / 3 + 2);
         Copy_Rect
           (Buf1,
            Area.Position.X,
            Area.Position.Y,
            Buf1,
            AreaH.Position.X,
            AreaH.Position.Y,
            Area.Width,
            Area.Height,
            False);
      end if;
   end Set_Score;

end Status;
