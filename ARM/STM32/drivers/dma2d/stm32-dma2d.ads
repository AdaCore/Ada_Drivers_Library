------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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

with System;
with Ada.Unchecked_Conversion;

package STM32.DMA2D is

   type DMA2D_Color is record
      Alpha : Byte;
      Red   : Byte;
      Green : Byte;
      Blue  : Byte;
   end record;

   for DMA2D_Color use record
      Blue  at 0 range 0 .. 7;
      Green at 1 range 0 .. 7;
      Red   at 2 range 0 .. 7;
      Alpha at 3 range 0 .. 7;
   end record;

   Black       : constant DMA2D_Color := (255, 0, 0, 0);
   White       : constant DMA2D_Color := (255, 255, 255, 255);
   Transparent : constant DMA2D_Color := (others => 0);

   --  This bit is set and cleared by software. It cannot be modified
   --  while a transfer is ongoing.
   type DMA2D_MODE is
     (
      --  Memory-to-memory (FG fetch only)
      M2M,

      --  Memory-to-memory with PFC (FG fetch only with FG PFC active)
      M2M_PFC,

      --  Memory-to-memory with blending (FG and BG fetch with PFC and
      --  blending)
      M2M_BLEND,

      --  Register-to-memory (no FG nor BG, only output stage active)
      R2M
     );
   for DMA2D_MODE'Size use 2;

   --  Configuration Error Interrupt Enable
   type DMA2D_FLAG is
     (Disable,
      Enable);

   --  Abort
   --  This bit can be used to abort the current transfer. This bit is
   --  set by software and is automatically reset by hardware when the
   --  START bit is reset.
   type DMA2D_ABORT is
     (
      --  0: No transfer abort requested
      Not_Requested,

      --  1: Transfer abort requested
      Requested);

   --  Suspend
   --  This bit can be used to suspend the current transfer. This bit
   --  is set and reset by software. It is automatically reset by
   --  hardware when the START bit is reset.
   type DMA2D_SUSPEND is
     (
      --  Transfer not suspended
      Not_Suspended,

      --  Transfer suspended
      Supended);

   --  Start
   --  This bit can be used to launch the DMA2D according to the
   --  parameters loaded in the  various configuration registers. This
   --  bit is automatically reset by the following events:
   --  - At the end of the transfer
   --  - When the data transfer is aborted by the user application by
   --    setting the ABORT bit in DMA2D_CR
   --  - When a data transfer error occurs
   --  - When the data transfer has not started due to a configuration
   --    error or another transfer operation already ongoing (automatic
   --    CLUT loading)
   type DMA2D_START is
     (Not_Started,
      Start);

   --  These bits defines the color format
   type DMA2D_Color_Mode is
     (ARGB8888,
      RGB888,
      RGB565,
      ARGB1555,
      ARGB4444,
      L8,
      AL44,
      AL88,
      L4,
      A8,
      A4) with Size => 4;

   subtype DMA2D_Dst_Color_Mode is DMA2D_Color_Mode range ARGB8888 .. ARGB4444;

   subtype Foreground_Color_Mode is DMA2D_Color_Mode;
   subtype Background_Color_Mode is DMA2D_Color_Mode;
   subtype Output_Color_Mode is DMA2D_Color_Mode range ARGB8888 .. ARGB4444;

--     function Bytes_Per_Pixel (CM : DMA2D_Color_Mode) return Positive
--     is (case CM is
--            when ARGB8888 => 4,
--            when RGB888 => 3,
--            when others => 2);

   --  Alpha mode
   --  00: No modification of the foreground image alpha channel value
   --  01: Replace original foreground image alpha channel value by ALPHA[7:0]
   --  10: Replace original foreground image alpha channel value by ALPHA[7:0]
   --      multiplied with original alpha channel value
   --  other configurations are meaningless
   type DMA2D_AM is
     (NO_MODIF,
      REPLACE,
      MULTIPLY);
   for DMA2D_AM'Size use 2;

   procedure DMA2D_DeInit;

   type DMA2D_Sync_Procedure is access procedure;

   procedure DMA2D_Init
     (Init : DMA2D_Sync_Procedure;
      Wait : DMA2D_Sync_Procedure);

   type DMA2D_Buffer is record
      Addr       : System.Address;
      Width      : Natural;
      Height     : Natural;
      Color_Mode : DMA2D_Color_Mode;
   end record;

   Null_Buffer : constant DMA2D_Buffer :=
                   (Addr       => System'To_Address (0),
                    Width      => 0,
                    Height     => 0,
                    Color_Mode => DMA2D_Color_Mode'First);

   procedure DMA2D_Fill
     (Buffer      : DMA2D_Buffer;
      Color       : Word;
      Synchronous : Boolean := False)
     with Pre => Buffer.Color_Mode in Output_Color_Mode;
   --  Same as above, using the destination buffer native color representation

   procedure DMA2D_Set_Pixel
     (Buffer      : DMA2D_Buffer;
      X, Y        : Integer;
      Color       : Word;
      Synchronous : Boolean := False)
     with Pre => Buffer.Color_Mode in Output_Color_Mode;
   --  Same as above, using the destination buffer native color representation

   procedure DMA2D_Set_Pixel_Blend
     (Buffer      : DMA2D_Buffer;
      X, Y        : Integer;
      Color       : DMA2D_Color;
      Synchronous : Boolean := False)
     with Pre => Buffer.Color_Mode in Output_Color_Mode;

   procedure DMA2D_Fill_Rect
     (Buffer      : DMA2D_Buffer;
      Color       : Word;
      X           : Integer;
      Y           : Integer;
      Width       : Integer;
      Height      : Integer;
      Synchronous : Boolean := False)
     with Pre => Buffer.Color_Mode in Output_Color_Mode;
   --  Same as above, using the destination buffer native color representation

   procedure DMA2D_Draw_Rect
     (Buffer    : DMA2D_Buffer;
      Color     : Word;
      X         : Integer;
      Y         : Integer;
      Width     : Integer;
      Height    : Integer)
     with Pre => Buffer.Color_Mode in Output_Color_Mode;
   --  Fill the specified area of the buffer with 'Color'

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
     with Pre => Dst_Buffer.Color_Mode in Output_Color_Mode;
   --  Copy a rectangular area from Src to Dst
   --  If Blend is set, then the rectangle will be merged with the destination
   --  area, taking into account the opacity of the source. Else, it is simply
   --  copied.
   --
   --  if Bg_Buffer is not Null_Buffer, then the copy will be performed in
   --  blend mode: Bg_Buffer and Src_Buffer are combined first and then copied
   --  to Dst_Buffer.

   procedure DMA2D_Draw_Vertical_Line
     (Buffer    : DMA2D_Buffer;
      Color     : Word;
      X         : Integer;
      Y         : Integer;
      Height    : Integer;
      Synchronous : Boolean := False)
     with Pre => Buffer.Color_Mode in Output_Color_Mode;
   --  Draws a vertical line

   procedure DMA2D_Draw_Horizontal_Line
     (Buffer    : DMA2D_Buffer;
      Color     : Word;
      X         : Integer;
      Y         : Integer;
      Width     : Integer;
      Synchronous : Boolean := False)
     with Pre => Buffer.Color_Mode in Output_Color_Mode;
   --  Draws a vertical line

   procedure DMA2D_Wait_Transfer;
   --  Makes sure the DMA2D transfers are done

private

   function As_UInt3 is new Ada.Unchecked_Conversion
     (DMA2D_Dst_Color_Mode, UInt3);
   function As_UInt4 is new Ada.Unchecked_Conversion
     (DMA2D_Color_Mode, UInt4);


end STM32.DMA2D;
