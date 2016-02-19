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

with Interfaces; use Interfaces;

with STM32.Touch_Panel;

with STM32;                 use STM32;
with STM32.DMA2D.Polling;   use STM32.DMA2D;
with STM32.LCD;             use STM32.LCD;
with Double_Buffer;         use Double_Buffer;

package body Screen_Interface is

   Initialized : Boolean := False;

   Current_Buffer : DMA2D_Buffer;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if not Initialized then
         Initialized := True;

         STM32.LCD.Initialize;
         STM32.LCD.Set_Orientation (Portrait);
         STM32.DMA2D.Polling.Initialize;
         STM32.Touch_Panel.Initialize;
         Double_Buffer.Initialize
           (Layer_Background => Layer_Single_Buffer,
            Layer_Foreground => Layer_Inactive);

         --  At init the draw layer is the one displayed, this will keep
         --  compatibility with projects not doing double buffering.
         Current_Buffer := Double_Buffer.Get_Visible_Buffer (Layer1);
         Fill_Screen (Black);
      end if;
   end Initialize;

   ------------------
   -- Swap_Buffers --
   ------------------

   procedure Swap_Buffers is
   begin
      Double_Buffer.Swap_Buffers (VSync => True);
      Current_Buffer := Double_Buffer.Get_Hidden_Buffer;
   end Swap_Buffers;

   -------------------------
   -- Current_Touch_State --
   -------------------------

   function Current_Touch_State return Touch_State is
      TS    : Touch_State;
      ST_TS : constant STM32.Touch_Panel.TP_State :=
                STM32.Touch_Panel.Get_State;
   begin
      TS.Touch_Detected := ST_TS'Length > 0;

      if TS.Touch_Detected then
         TS.X := ST_TS (1).X;
         TS.Y := ST_TS (1).Y;
      else
         TS.X := 0;
         TS.Y := 0;
      end if;

      return TS;
   end Current_Touch_State;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (P : Point; Col : Color) is
   begin
      STM32.DMA2D.DMA2D_Set_Pixel (Current_Buffer, P.X, P.Y, Word (Col));
   end Set_Pixel;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (X, Y : Natural; Col : Color) is
   begin
      STM32.DMA2D.DMA2D_Set_Pixel (Current_Buffer, X, Y, Word (Col));
   end Set_Pixel;

   -----------------
   -- Fill_Screen --
   -----------------

   procedure Fill_Screen (Col : Color) is
   begin
      --  ??? Use DMA2D to fill the buffer
      DMA2D_Fill
        (Buffer => Current_Buffer,
         Color  => Word (Col));
   end Fill_Screen;

   --------------
   -- As_Color --
   --------------

   function As_Color (R, G, B : RGB_Value) return Color is
      RF : constant Float := (Float (R) / 255.0) * 31.0;
      GF : constant Float := (Float (G) / 255.0) * 31.0;
      BF : constant Float := (Float (B) / 255.0) * 31.0;
   begin
      return 16#8000# or
        (Color (RF) * (2**10)) or (Color (GF) * (2**5)) or Color (BF);
   end As_Color;

end Screen_Interface;
