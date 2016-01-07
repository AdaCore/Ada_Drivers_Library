with System.Storage_Elements; use System.Storage_Elements;
--  with STM32.RCC;               use STM32.RCC;

with STM32.LCD;               use STM32.LCD; --  for pixel format handling

with STM32_SVD.DMA2D;         use STM32_SVD.DMA2D;
with STM32_SVD.RCC;           use STM32_SVD.RCC;

package body STM32.DMA2D is

   function To_Word is new Ada.Unchecked_Conversion (System.Address, Word);

   function To_OCOLR
     (Col : DMA2D_Color; CM : DMA2D_Color_Mode)
      return Word;

   DMA2D_Wait_Transfer : DMA2D_Sync_Procedure := null;
   DMA2D_Init_Transfer : DMA2D_Sync_Procedure := null;

   --------------
   -- To_OCOLR --
   --------------

   function To_OCOLR
     (Col : DMA2D_Color; CM : DMA2D_Color_Mode)
      return Word
   is
      function To_Word is new Ada.Unchecked_Conversion
        (DMA2D_Color, Word);
      R : Word;
      G : Word;
      B : Word;
      A : Word;
   begin
      case CM is
         when Pixel_Fmt_ARGB8888 | Pixel_Fmt_RGB888 =>
            return To_Word (Col);
         when Pixel_Fmt_RGB565   =>
            R := Shift_Left (Word (Shift_Right (Col.Red, 3)), 11);
            G := Shift_Left (Word (Shift_Right (Col.Green, 2)), 5);
            B := Word (Shift_Right (Col.Blue, 3));
            return R or G or B;
         when Pixel_Fmt_ARGB1555 =>
            A := Shift_Left (Word (Shift_Right (Col.Alpha, 7)), 15);
            R := Shift_Left (Word (Shift_Right (Col.Red, 3)), 10);
            G := Shift_Left (Word (Shift_Right (Col.Green, 3)), 5);
            B := Word (Shift_Right (Col.Blue, 3));
            return A or R or G or B;
         when Pixel_Fmt_ARGB4444 =>
            A := Shift_Left (Word (Shift_Right (Col.Alpha, 4)), 12);
            R := Shift_Left (Word (Shift_Right (Col.Red, 4)), 8);
            G := Shift_Left (Word (Shift_Right (Col.Green, 4)), 4);
            B := Word (Shift_Right (Col.Blue, 4));
            return A or R or G or B;
      end case;
   end To_OCOLR;

   -----------------------
   -- DMA2D_Initialized --
   -----------------------

   function DMA2D_Initialized return Boolean is
   begin
      return DMA2D_Init_Transfer /= null;
   end DMA2D_Initialized;

   ------------------
   -- DMA2D_DeInit --
   ------------------

   procedure DMA2D_DeInit is
   begin
      RCC_Periph.AHB1ENR.DMA2DEN := 1;
      RCC_Periph.AHB1ENR.DMA2DEN := 0;
      DMA2D_Init_Transfer := null;
      DMA2D_Wait_Transfer := null;
   end DMA2D_DeInit;

   ----------------
   -- DMA2D_Init --
   ----------------

   procedure DMA2D_Init
     (Init : DMA2D_Sync_Procedure;
      Wait : DMA2D_Sync_Procedure)
   is
   begin
      if DMA2D_Initialized then
         return;
      end if;
      DMA2D_Init_Transfer := Init;
      DMA2D_Wait_Transfer := Wait;

      RCC_Periph.AHB1ENR.DMA2DEN := 1;
      RCC_Periph.AHB1RSTR.DMA2RST := 1;
      RCC_Periph.AHB1RSTR.DMA2RST := 0;
   end DMA2D_Init;

   ----------------
   -- DMA2D_Fill --
   ----------------

   procedure DMA2D_Fill
     (Buffer      : DMA2D_Buffer;
      Color       : DMA2D_Color;
      Synchronous : Boolean := False)
   is
   begin
      DMA2D_Fill (Buffer, To_OCOLR (Color, Buffer.Color_Mode), Synchronous);
   end DMA2D_Fill;

   ----------------
   -- DMA2D_Fill --
   ----------------

   procedure DMA2D_Fill
     (Buffer      : DMA2D_Buffer;
      Color       : Word;
      Synchronous : Boolean := False)
   is
      function Conv is new Ada.Unchecked_Conversion (Word, OCOLR_Register);
   begin
      DMA2D_Wait_Transfer.all;

      DMA2D_Periph.CR.MODE   := DMA2D_MODE'Enum_Rep (R2M);
      DMA2D_Periph.OPFCCR.CM := As_UInt3 (Buffer.Color_Mode);
      DMA2D_Periph.OCOLR     := Conv (Color);
      DMA2D_Periph.OMAR      := To_Word (Buffer.Addr);
      DMA2D_Periph.OOR       := (LO => 0, others => <>);
      DMA2D_Periph.NLR       := (NL     => Short (Buffer.Height),
                                 PL     => UInt14 (Buffer.Width),
                                 others => <>);

      DMA2D_Init_Transfer.all;
      if Synchronous then
         DMA2D_Wait_Transfer.all;
      end if;
   end DMA2D_Fill;

   ---------------------
   -- DMA2D_Fill_Rect --
   ---------------------

   procedure DMA2D_Fill_Rect
     (Buffer      : DMA2D_Buffer;
      Color       : DMA2D_Color;
      X           : Integer;
      Y           : Integer;
      Width       : Integer;
      Height      : Integer;
      Synchronous : Boolean := False)
   is
   begin
      DMA2D_Fill_Rect
        (Buffer,
         To_OCOLR (Color, Buffer.Color_Mode),
         X, Y, Width, Height, Synchronous);
   end DMA2D_Fill_Rect;

   ---------------------
   -- DMA2D_Fill_Rect --
   ---------------------

   procedure DMA2D_Fill_Rect
     (Buffer      : DMA2D_Buffer;
      Color       : Word;
      X           : Integer;
      Y           : Integer;
      Width       : Integer;
      Height      : Integer;
      Synchronous : Boolean := False)
   is
      function Conv is new Ada.Unchecked_Conversion (Word, OCOLR_Register);
   begin
      if Buffer.Swap_X_Y then
         declare
            Buf2 : DMA2D_Buffer := Buffer;
         begin
            Buf2.Swap_X_Y := False;
            Buf2.Width := Buffer.Height;
            Buf2.Height := Buffer.Width;
            DMA2D_Fill_Rect (Buf2, Color,
                             Y,
                             Buffer.Width - X - Width,
                             Height,
                             WIdth,
                             Synchronous);
         end;
      else
         declare
            Offset : constant Word := Word (Y * Buffer.Width + X);
            Off    : constant Word :=
                       To_Word (Buffer.Addr) +
                       (Offset * Word (STM32.LCD.Bytes_Per_Pixel
                        (Buffer.Color_Mode)));

         begin
            DMA2D_Wait_Transfer.all;

            DMA2D_Periph.CR.MODE := DMA2D_MODE'Enum_Rep (R2M);
            DMA2D_Periph.OPFCCR :=
              (CM     => DMA2D_Color_Mode'Enum_Rep (Buffer.Color_Mode),
               others => <>);
            DMA2D_Periph.OCOLR  := Conv (Color);
            DMA2D_Periph.OMAR   := Off;
            DMA2D_Periph.OOR.LO := UInt14 (Buffer.Width -  Width);
            DMA2D_Periph.NLR :=
              (NL => Short (Height), PL => UInt14 (Width), others => <>);

            DMA2D_Init_Transfer.all;
            if Synchronous then
               DMA2D_Wait_Transfer.all;
            end if;
         end;
      end if;
   end DMA2D_Fill_Rect;

   ---------------------
   -- DMA2D_Draw_Rect --
   ---------------------

   procedure DMA2D_Draw_Rect
     (Buffer    : DMA2D_Buffer;
      Color     : DMA2D_Color;
      X         : Integer;
      Y         : Integer;
      Width     : Integer;
      Height    : Integer)
   is
   begin
      DMA2D_Draw_Horizontal_Line (Buffer, Color, X, Y, Width);
      DMA2D_Draw_Horizontal_Line (Buffer, Color, X, Y + Height - 1, Width);
      DMA2D_Draw_Vertical_Line (Buffer, Color, X, Y, Height);
      DMA2D_Draw_Vertical_Line (Buffer, Color, X + Width - 1, Y, Height);
   end DMA2D_Draw_Rect;

   ---------------------
   -- DMA2D_Copy_Rect --
   ---------------------

   procedure DMA2D_Copy_Rect
     (Src_Buffer : DMA2D_Buffer;
      X_Src      : Natural;
      Y_Src      : Natural;
      Dst_Buffer : DMA2D_Buffer;
      X_Dst      : Natural;
      Y_Dst      : Natural;
      Bg_Buffer  : DMA2D_Buffer;
      X_Bg       : Natural;
      Y_Bg       : Natural;
      Width      : Natural;
      Height     : Natural;
      Synchronous  : Boolean := False)
   is
      Src_Idx : constant Natural := Y_Src * Src_Buffer.Width + X_Src;
      Src_BPP : constant Natural := Bytes_Per_Pixel (Src_Buffer.Color_Mode);
      Src_Off : constant System.Storage_Elements.Storage_Offset :=
                  System.Storage_Elements.Storage_Offset (Src_Idx * Src_BPP);
      Dst_Idx : constant Natural := Y_Dst * Dst_Buffer.Width + X_Dst;
      Dst_BPP : constant Natural := Bytes_Per_Pixel (Dst_Buffer.Color_Mode);
      Dst_Off : constant System.Storage_Elements.Storage_Offset :=
                  System.Storage_Elements.Storage_Offset (Dst_Idx * Dst_BPP);
      Bg_Idx  : constant Natural := Y_Bg * Bg_Buffer.Width + X_Bg;
      Bg_BPP  : constant Natural := Bytes_Per_Pixel (Bg_Buffer.Color_Mode);
      Bg_Off  : constant System.Storage_Elements.Storage_Offset :=
                  System.Storage_Elements.Storage_Offset (Bg_Idx * Bg_BPP);
      Use_PFC : Boolean;

   begin
      DMA2D_Wait_Transfer.all;

      if Bg_Buffer /= Null_Buffer then
         --  PFC and blending
         DMA2D_Periph.CR.MODE := DMA2D_MODE'Enum_Rep (M2M_BLEND);
         Use_PFC := True;

      elsif Src_Buffer.Color_Mode = Dst_Buffer.Color_Mode then
         --  Direct memory transfer
         DMA2D_Periph.CR.MODE := DMA2D_MODE'Enum_Rep (M2M);
         Use_PFC := False;

      else
         --  Requires color conversion
         DMA2D_Periph.CR.Mode := DMA2D_Mode'Enum_Rep (M2M_PFC);
         Use_PFC := True;
      end if;

      --  SOURCE CONFIGURATION
      DMA2D_Periph.FGPFCCR.CM    :=
        DMA2D_Color_Mode'Enum_Rep (Src_Buffer.Color_Mode);
      DMA2D_Periph.FGMAR         := To_Word (Src_Buffer.Addr + Src_Off);
      DMA2D_Periph.FGPFCCR.AM    := DMA2D_AM'Enum_Rep (NO_MODIF);
      DMA2D_Periph.FGPFCCR.ALPHA := 255;
      DMA2D_Periph.FGPFCCR.CS    := 0;
      DMA2D_Periph.FGPFCCR.START :=
        DMA2D_START'Enum_Rep ((if Use_PFC then Start else Not_Started));
      DMA2D_Periph.FGOR          :=
        (LO => UInt14 (Src_Buffer.Width - Width), others => <>);
      DMA2D_Periph.FGPFCCR.CCM := 0; -- ARGB8888 CLUT color mode

      if Bg_Buffer /= Null_Buffer then
         DMA2D_Periph.BGPFCCR.CM    :=
           DMA2D_COlor_mode'Enum_Rep (Bg_Buffer.Color_Mode);
         DMA2D_Periph.BGMAR         := To_Word (Bg_Buffer.Addr + Bg_Off);
         DMA2D_Periph.BGPFCCR.CS    := 0;
         DMA2D_Periph.BGPFCCR.START := DMA2D_START'Enum_Rep (Start);
         DMA2D_Periph.BGOR          :=
           (LO => UInt14 (Bg_Buffer.Width - Width), others => <>);
         DMA2D_Periph.BGPFCCR.CCM   := 0; -- ARGB888
      end if;

      --  DST CONFIGURATION
      DMA2D_Periph.OPFCCR.CM  :=
        DMA2D_Color_Mode'Enum_Rep (Dst_Buffer.Color_Mode);
      DMA2D_Periph.OMAR       := To_Word (Dst_Buffer.Addr + Dst_Off);
      DMA2D_Periph.OOR        := (LO     => UInt14 (Dst_Buffer.Width - Width),
                                  others => <>);

      DMA2D_Periph.NLR := (NL     => Short (Height),
                           PL     => UInt14 (Width),
                           others => <>);

      DMA2D_Init_Transfer.all;
      if Synchronous then
         DMA2D_Wait_Transfer.all;
      end if;
   end DMA2D_Copy_Rect;

   ------------------------------
   -- DMA2D_Draw_Vertical_Line --
   ------------------------------

   procedure DMA2D_Draw_Vertical_Line
     (Buffer    : DMA2D_Buffer;
      Color     : DMA2D_Color;
      X         : Integer;
      Y         : Integer;
      Height    : Integer;
      Synchronous : Boolean := False)
   is
      NY, NH : Integer;
   begin
      if Y >= Buffer.Height
        or else X not in 0 .. Buffer.Width - 1
      then
         return;
      end if;

      if Y < 0 then
         NY := 0;
         NH := Height + Y;
      else
         NY := Y;
         NH := Height;
      end if;

      NH := Integer'Min (NH, Buffer.Height - NY - 1);

      DMA2D_Fill_Rect (Buffer, Color, X, NY, 1, NH, Synchronous);
   end DMA2D_Draw_Vertical_Line;

   --------------------------------
   -- DMA2D_Draw_Horizontal_Line --
   --------------------------------

   procedure DMA2D_Draw_Horizontal_Line
     (Buffer    : DMA2D_Buffer;
      Color     : DMA2D_Color;
      X         : Integer;
      Y         : Integer;
      Width     : Integer;
      Synchronous : Boolean := False)
   is
      NX, NW : Integer;
   begin
      if X >= Buffer.Width
        or else Y not in 0 .. Buffer.Height - 1
      then
         return;
      end if;

      if X < 0 then
         NX := 0;
         NW := Width + X;
      else
         NX := X;
         NW := Width;
      end if;

      NW := Integer'Min (NW, Buffer.Width - NX - 1);

      DMA2D_Fill_Rect (Buffer, Color, NX, Y, NW, 1, Synchronous);
   end DMA2D_Draw_Horizontal_Line;

--     -----------------------
--     -- DMA2D_Draw_Circle --
--     -----------------------
--
--     procedure DMA2D_Fill_Circle
--       (Buffer    : DMA2D_Buffer;
--        Color     : DMA2D_Color;
--        X0        : Integer;
--        Y0        : Integer;
--        R         : Integer)
--     is
--        F     : Integer := 1 - R;
--        ddF_X : Integer := 1;
--        ddF_Y : Integer := -(2 * R);
--        X     : Integer := 0;
--        Y     : Integer := R;
--     begin
--        DMA2D_Draw_Vertical_Line (Buffer, Color, X0, Y0 - R, 2 * R);
--        DMA2D_Draw_Horizontal_Line (Buffer, Color, X0 - R, Y0, 2 * R);
--
--        while X < Y loop
--           if F >= 0 then
--              Y := Y - 1;
--              ddF_Y := ddF_Y + 2;
--              F := F + ddF_Y;
--           end if;
--           X := X + 1;
--           ddF_X := ddF_X + 2;
--           F := F + ddF_X;
--
--           DMA2D_Draw_Horizontal_Line (Buffer, Color, X0 - X, Y0 + Y, 2 * X);
--           DMA2D_Draw_Horizontal_Line (Buffer, Color, X0 - X, Y0 - Y, 2 * X);
--           DMA2D_Draw_Horizontal_Line (Buffer, Color, X0 - Y, Y0 + X, 2 * Y);
--           DMA2D_Draw_Horizontal_Line (Buffer, Color, X0 - Y, Y0 - X, 2 * Y);
--        end loop;
--     end DMA2D_Fill_Circle;

   ---------------------
   -- DMA2D_Set_Pixel --
   ---------------------

   procedure DMA2D_Set_Pixel
     (Buffer      : DMA2D_Buffer;
      X, Y        : Integer;
      Color       : DMA2D_Color;
      Synchronous : Boolean := False)
   is
   begin
      DMA2D_Set_Pixel
        (Buffer, X, Y, To_OCOLR (Color, Buffer.Color_Mode), Synchronous);
   end DMA2D_Set_Pixel;

   ---------------------
   -- DMA2D_Set_Pixel --
   ---------------------

   procedure DMA2D_Set_Pixel
     (Buffer      : DMA2D_Buffer;
      X, Y        : Integer;
      Color       : Word;
      Synchronous : Boolean := False)
   is
      function Conv is new Ada.Unchecked_Conversion (Word, OCOLR_Register);
      X0   : constant Integer := (if Buffer.Swap_X_Y then Y else X);
      Y0   : constant Integer := (if Buffer.Swap_X_Y then Buffer.Width - X else Y);
      W    : constant Natural :=
               (if Buffer.Swap_X_Y then Buffer.Height else Buffer.Width);
      Idx  : constant Integer := (Y0 * W) + X0;
      BPP  : constant Natural := Bytes_Per_Pixel (Buffer.Color_Mode);
      Off  : constant System.Storage_Elements.Storage_Offset :=
               System.Storage_Elements.Storage_Offset (Idx * BPP);
      Dead : Boolean := False with Unreferenced;
   begin
      if X < 0 or else Y < 0
        or else X >= Buffer.Width or else Y >= Buffer.Height
      then
         return;
      end if;

      DMA2D_Wait_Transfer.all;

      DMA2D_Periph.CR.MODE   := DMA2D_MODE'Enum_Rep (R2M);
      DMA2D_Periph.OPFCCR.CM := As_UInt3 (Buffer.Color_Mode);
      DMA2D_Periph.OCOLR     := Conv (Color);
      DMA2D_Periph.OMAR      := To_Word (Buffer.Addr + Off);
      DMA2D_Periph.OOR       := (LO => 1, others => <>);
      DMA2D_Periph.NLR       := (NL => 1, PL => 1, others => <>);

      DMA2D_Init_Transfer.all;
      if Synchronous then
         DMA2D_Wait_Transfer.all;
      end if;
   end DMA2D_Set_Pixel;

   ---------------------------
   -- DMA2D_Set_Pixel_Blend --
   ---------------------------

   procedure DMA2D_Set_Pixel_Blend
     (Buffer      : DMA2D_Buffer;
      X, Y        : Integer;
      Color       : DMA2D_Color;
      Synchronous : Boolean := False)
   is
      X0   : constant Natural := (if Buffer.Swap_X_Y then Y else X);
      Y0   : constant Natural := (if Buffer.Swap_X_Y then Buffer.Width - X else Y);
      W    : constant Natural :=
               (if Buffer.Swap_X_Y then Buffer.Height else Buffer.Width);
      Idx  : constant Natural := (Y0 * W) + X0;
      BPP  : constant Natural := Bytes_Per_Pixel (Buffer.Color_Mode);
      Off  : constant System.Storage_Elements.Storage_Offset :=
               System.Storage_Elements.Storage_Offset (Idx * BPP);
      Dead : Boolean := False with Unreferenced;
   begin
      if X < 0 or else Y < 0
        or else X >= Buffer.Width or else Y >= Buffer.Height
      then
         return;
      end if;

      DMA2D_Wait_Transfer.all;

      --  PFC and blending
      DMA2D_Periph.CR.MODE := DMA2D_MODE'Enum_Rep (M2M_BLEND);

      --  SOURCE CONFIGURATION
      DMA2D_Periph.FGPFCCR.CM    := Pixel_Fmt_ARGB8888'Enum_Rep;
      DMA2D_Periph.FGMAR         := To_Word (Color'Address);
      DMA2D_Periph.FGPFCCR.AM    := DMA2D_AM'Enum_Rep (NO_MODIF);
      DMA2D_Periph.FGPFCCR.ALPHA := 255;
      DMA2D_Periph.FGPFCCR.CS    := 0;
      DMA2D_Periph.FGPFCCR.START := DMA2D_START'Enum_Rep (Not_Started);
      DMA2D_Periph.FGOR          := (LO => 0, others => <>);
      DMA2D_Periph.FGPFCCR.CCM   := Pixel_Fmt_ARGB8888'Enum_Rep;

      --  Setup the Background buffer to the destination buffer
      DMA2D_Periph.BGPFCCR.CM    :=
        DMA2D_COlor_mode'Enum_Rep (Buffer.Color_Mode);
      DMA2D_Periph.BGMAR         := To_Word (Buffer.Addr + Off);
      DMA2D_Periph.BGPFCCR.CS    := 0;
      DMA2D_Periph.BGPFCCR.START := DMA2D_START'Enum_Rep (Not_Started);
      DMA2D_Periph.BGOR          := (LO     => UInt14 (Buffer.Width - 1),
                                     others => <>);
      DMA2D_Periph.BGPFCCR.CCM   := 0; -- ARGB888

      --  DST CONFIGURATION
      DMA2D_Periph.OPFCCR.CM  :=
        DMA2D_Color_Mode'Enum_Rep (Buffer.Color_Mode);
      DMA2D_Periph.OMAR       := To_Word (Buffer.Addr + Off);
      DMA2D_Periph.OOR        := (LO     => UInt14 (Buffer.Width - 1),
                                  others => <>);

      DMA2D_Periph.NLR := (NL     => 1,
                           PL     => 1,
                           others => <>);

      DMA2D_Init_Transfer.all;
      if Synchronous then
         DMA2D_Wait_Transfer.all;
      end if;
   end DMA2D_Set_Pixel_Blend;

end STM32.DMA2D;
