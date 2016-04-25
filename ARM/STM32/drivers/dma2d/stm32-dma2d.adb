------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2016, AdaCore                         --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

--  with STM32.RCC;               use STM32.RCC;

with STM32_SVD.DMA2D;         use STM32_SVD.DMA2D;
with STM32_SVD.RCC;           use STM32_SVD.RCC;

package body STM32.DMA2D is

   function To_Word is new Ada.Unchecked_Conversion (System.Address, Word);

   function Offset (Buffer : DMA2D_Buffer;
                    X, Y   : Integer) return Word with Inline_Always;

   DMA2D_Wait_Transfer_Int : DMA2D_Sync_Procedure := null;
   DMA2D_Init_Transfer_Int : DMA2D_Sync_Procedure := null;

   ------------------
   -- DMA2D_DeInit --
   ------------------

   procedure DMA2D_DeInit is
   begin
      RCC_Periph.AHB1ENR.DMA2DEN := False;
      DMA2D_Init_Transfer_Int := null;
      DMA2D_Wait_Transfer_Int := null;
   end DMA2D_DeInit;

   ----------------
   -- DMA2D_Init --
   ----------------

   procedure DMA2D_Init
     (Init : DMA2D_Sync_Procedure;
      Wait : DMA2D_Sync_Procedure)
   is
   begin
      if DMA2D_Init_Transfer_Int = Init then
         return;
      end if;

      DMA2D_DeInit;

      DMA2D_Init_Transfer_Int := Init;
      DMA2D_Wait_Transfer_Int := Wait;

      RCC_Periph.AHB1ENR.DMA2DEN := True;
      RCC_Periph.AHB1RSTR.DMA2DRST := True;
      RCC_Periph.AHB1RSTR.DMA2DRST := False;
   end DMA2D_Init;

   ------------
   -- Offset --
   ------------

   function Offset (Buffer : DMA2D_Buffer;
                    X, Y   : Integer) return Word
   is
      Off : constant Word := Word (X + Buffer.Width * Y);
   begin
      case Buffer.Color_Mode is
         when ARGB8888 =>
            return 4 * Off;
         when RGB888 =>
            return 3 * Off;
         when ARGB1555 | ARGB4444 | RGB565 | AL88 =>
            return 2 * Off;
         when L8 | AL44 | A8 =>
            return Off;
         when L4 | A4 =>
            return Off / 2;
      end case;
   end Offset;

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
      DMA2D_Wait_Transfer_Int.all;

      DMA2D_Periph.CR.MODE   := DMA2D_MODE'Enum_Rep (R2M);
      DMA2D_Periph.OPFCCR.CM := As_UInt3 (Buffer.Color_Mode);
      DMA2D_Periph.OCOLR     := Conv (Color);
      DMA2D_Periph.OMAR      := To_Word (Buffer.Addr);
      DMA2D_Periph.OOR       := (LO => 0, others => <>);
      DMA2D_Periph.NLR       := (NL     => Short (Buffer.Height),
                                 PL     => UInt14 (Buffer.Width),
                                 others => <>);

      DMA2D_Init_Transfer_Int.all;
      if Synchronous then
         DMA2D_Wait_Transfer_Int.all;
      end if;
   end DMA2D_Fill;

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
      Off    : constant Word := Offset (Buffer, X, Y);

   begin
      DMA2D_Wait_Transfer_Int.all;

      DMA2D_Periph.CR.MODE := DMA2D_MODE'Enum_Rep (R2M);
      DMA2D_Periph.OPFCCR :=
        (CM     => DMA2D_Color_Mode'Enum_Rep (Buffer.Color_Mode),
         others => <>);
      DMA2D_Periph.OCOLR  := Conv (Color);
      DMA2D_Periph.OMAR   := To_Word (Buffer.Addr) + Off;
      DMA2D_Periph.OOR.LO := UInt14 (Buffer.Width -  Width);
      DMA2D_Periph.NLR :=
        (NL => Short (Height), PL => UInt14 (Width), others => <>);

      DMA2D_Init_Transfer_Int.all;
      if Synchronous then
         DMA2D_Wait_Transfer_Int.all;
      end if;
   end DMA2D_Fill_Rect;

   ---------------------
   -- DMA2D_Draw_Rect --
   ---------------------

   procedure DMA2D_Draw_Rect
     (Buffer    : DMA2D_Buffer;
      Color     : Word;
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
      Src_Off : constant Word := Offset (Src_Buffer, X_Src, Y_Src);
      Dst_Off : constant Word := Offset (Dst_Buffer, X_Dst, Y_Dst);

   begin
      DMA2D_Wait_Transfer_Int.all;

      if Bg_Buffer /= Null_Buffer then
         --  PFC and blending
         DMA2D_Periph.CR.MODE := DMA2D_MODE'Enum_Rep (M2M_BLEND);

      elsif Src_Buffer.Color_Mode = Dst_Buffer.Color_Mode then
         --  Direct memory transfer
         DMA2D_Periph.CR.MODE := DMA2D_MODE'Enum_Rep (M2M);

      else
         --  Requires color conversion
         --  ??? TODO
         DMA2D_Periph.CR.MODE := DMA2D_MODE'Enum_Rep (M2M_PFC);
      end if;

      --  SOURCE CONFIGURATION
      DMA2D_Periph.FGPFCCR :=
        (CM    => DMA2D_Color_Mode'Enum_Rep (Src_Buffer.Color_Mode),
         AM    => DMA2D_AM'Enum_Rep (NO_MODIF),
         ALPHA => 255,
         others => <>);
      DMA2D_Periph.FGOR    := (LO     => UInt14 (Src_Buffer.Width - Width),
                               others => <>);
      DMA2D_Periph.FGMAR   := To_Word (Src_Buffer.Addr) + Src_Off;

      if Bg_Buffer /= Null_Buffer then
         declare
            Bg_Off  : constant Word := Offset (Bg_Buffer, X_Bg, Y_Bg);
         begin
            DMA2D_Periph.BGPFCCR.CM    :=
              DMA2D_Color_Mode'Enum_Rep (Bg_Buffer.Color_Mode);
            DMA2D_Periph.BGMAR         := To_Word (Bg_Buffer.Addr) + Bg_Off;
            DMA2D_Periph.BGPFCCR.CS    := 0;
            DMA2D_Periph.BGPFCCR.START := False;
            DMA2D_Periph.BGOR          :=
              (LO => UInt14 (Bg_Buffer.Width - Width), others => <>);
            DMA2D_Periph.BGPFCCR.CCM   := False; -- Disable CLUT color mode
         end;
      end if;

      --  DST CONFIGURATION
      DMA2D_Periph.OPFCCR.CM :=
        DMA2D_Color_Mode'Enum_Rep (Dst_Buffer.Color_Mode);
      DMA2D_Periph.OMAR      := To_Word (Dst_Buffer.Addr) + Dst_Off;
      DMA2D_Periph.OOR       := (LO     => UInt14 (Dst_Buffer.Width - Width),
                                  others => <>);

      DMA2D_Periph.NLR       := (NL     => Short (Height),
                                 PL     => UInt14 (Width),
                              others => <>);

      DMA2D_Init_Transfer_Int.all;
      if Synchronous then
         DMA2D_Wait_Transfer_Int.all;
      end if;
   end DMA2D_Copy_Rect;

   ------------------------------
   -- DMA2D_Draw_Vertical_Line --
   ------------------------------

   procedure DMA2D_Draw_Vertical_Line
     (Buffer    : DMA2D_Buffer;
      Color     : Word;
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
      Color     : Word;
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
      Off  : constant Word := Offset (Buffer, X, Y);
      Dead : Boolean := False with Unreferenced;
   begin
      if X < 0 or else Y < 0
        or else X >= Buffer.Width or else Y >= Buffer.Height
      then
         return;
      end if;

      DMA2D_Wait_Transfer_Int.all;

      DMA2D_Periph.CR.MODE   := DMA2D_MODE'Enum_Rep (R2M);
      DMA2D_Periph.OPFCCR.CM := As_UInt3 (Buffer.Color_Mode);
      DMA2D_Periph.OCOLR     := Conv (Color);
      DMA2D_Periph.OMAR      := To_Word (Buffer.Addr) + Off;
      DMA2D_Periph.OOR       := (LO => 1, others => <>);
      DMA2D_Periph.NLR       := (NL => 1, PL => 1, others => <>);

      DMA2D_Init_Transfer_Int.all;
      if Synchronous then
         DMA2D_Wait_Transfer_Int.all;
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
      Off  : constant Word := Offset (Buffer, X, Y);
      Dead : Boolean := False with Unreferenced;
   begin
      if X < 0 or else Y < 0
        or else X >= Buffer.Width or else Y >= Buffer.Height
      then
         return;
      end if;

      DMA2D_Wait_Transfer_Int.all;

      --  PFC and blending
      DMA2D_Periph.CR.MODE := DMA2D_MODE'Enum_Rep (M2M_BLEND);

      --  SOURCE CONFIGURATION
      DMA2D_Periph.FGPFCCR.CM    := ARGB8888'Enum_Rep;
      DMA2D_Periph.FGMAR         := To_Word (Color'Address);
      DMA2D_Periph.FGPFCCR.AM    := DMA2D_AM'Enum_Rep (NO_MODIF);
      DMA2D_Periph.FGPFCCR.ALPHA := 255;
      DMA2D_Periph.FGPFCCR.CS    := 0;
      DMA2D_Periph.FGPFCCR.START := False;
      DMA2D_Periph.FGOR          := (LO => 0, others => <>);
      DMA2D_Periph.FGPFCCR.CCM   := False; --  Disable CLUT color mode

      --  Setup the Background buffer to the destination buffer
      DMA2D_Periph.BGPFCCR.CM    :=
        DMA2D_Color_Mode'Enum_Rep (Buffer.Color_Mode);
      DMA2D_Periph.BGMAR         := To_Word (Buffer.Addr) + Off;
      DMA2D_Periph.BGPFCCR.CS    := 0;
      DMA2D_Periph.BGPFCCR.START := False;
      DMA2D_Periph.BGOR          := (LO     => UInt14 (Buffer.Width - 1),
                                     others => <>);
      DMA2D_Periph.BGPFCCR.CCM   := False; --  Disable CLUT color mode

      --  DST CONFIGURATION
      DMA2D_Periph.OPFCCR.CM  :=
        DMA2D_Color_Mode'Enum_Rep (Buffer.Color_Mode);
      DMA2D_Periph.OMAR       := To_Word (Buffer.Addr) + Off;
      DMA2D_Periph.OOR        := (LO     => UInt14 (Buffer.Width - 1),
                                  others => <>);

      DMA2D_Periph.NLR := (NL     => 1,
                           PL     => 1,
                           others => <>);

      DMA2D_Init_Transfer_Int.all;
      if Synchronous then
         DMA2D_Wait_Transfer_Int.all;
      end if;
   end DMA2D_Set_Pixel_Blend;

   -------------------------
   -- DMA2D_Wait_Transfer --
   -------------------------

   procedure DMA2D_Wait_Transfer is
   begin
      DMA2D_Wait_Transfer_Int.all;
   end DMA2D_Wait_Transfer;

end STM32.DMA2D;
