------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with STM32.GPIO;   use STM32.GPIO;
with STM32.SPI;    use STM32.SPI;
with STM32.Device; use STM32.Device;
with STM32_H405;   use STM32_H405;
with HAL.Bitmap;   use HAL.Bitmap;
with HAL.SPI;      use HAL.SPI;
with PCD8544;      use PCD8544;
with Ravenscar_Time;

procedure LCD_Test is
   procedure Configure_GPIO;
   procedure Configure_SPI;

   LCD_SPI : STM32.SPI.SPI_Port renames SPI_2;
   LCD_CLK : GPIO_Point renames EXT2_16;
   LCD_DIN : GPIO_Point renames EXT2_19;
   LCD_RST : GPIO_Point renames EXT2_9;
   LCD_CS  : GPIO_Point renames EXT2_17;
   LCD_DC  : GPIO_Point renames EXT2_2;

   procedure Configure_GPIO is
   begin
      Enable_Clock (LCD_DIN & LCD_CLK & LCD_RST & LCD_DC & LCD_CS);
      Configure_IO (LCD_RST & LCD_DC & LCD_CS,
         (Resistors   => Pull_Up,
          Mode        => Mode_Out,
          Output_Type => Push_Pull,
          Speed       => Speed_25MHz));
      Configure_IO (LCD_DIN & LCD_CLK,
         (Resistors      => Pull_Up,
          Mode           => Mode_AF,
          AF_Output_Type => Push_Pull,
          AF_Speed       => Speed_25MHz,
          AF             => GPIO_AF_SPI2_5));
   end Configure_GPIO;

   procedure Configure_SPI is
   begin
      Enable_Clock (LCD_SPI);
      Configure (LCD_SPI,
         (Direction           => D2Lines_FullDuplex,
          Mode                => Master,
          Data_Size           => Data_Size_8b,
          Clock_Polarity      => High,
          Clock_Phase         => P2Edge,
          Slave_Management    => Software_Managed,
          Baud_Rate_Prescaler => BRP_8,
          First_Bit           => MSB,
          CRC_Poly            => 0));
      Enable (LCD_SPI);
   end Configure_SPI;

   Display : PCD8544_Device
      (Port => LCD_SPI'Access,
       RST  => LCD_RST'Access,
       CS   => LCD_CS'Access,
       DC   => LCD_DC'Access,
       Time => Ravenscar_Time.Delays);

   Bitmap : Any_Bitmap_Buffer;

   Cursor : Rect :=
      (Position => (0, 0),
       Width    => 8,
       Height   => 8);
begin
   Configure_GPIO;
   Configure_SPI;
   Display.Initialize;
   Display.Initialize_Layer
      (Layer  => 1,
       Mode   => M_1,
       X      => 0,
       Y      => 0,
       Width  => Display.Width,
       Height => Display.Height);

   Bitmap := Display.Hidden_Buffer (1);

   loop
      for X in 0 .. ((Display.Width / Cursor.Width) - 1) loop
         for Y in 0 .. ((Display.Height / Cursor.Height) - 1) loop
            Bitmap.Set_Source (White);
            Bitmap.Fill;

            Cursor.Position := (X * Cursor.Width, Y * Cursor.Height);
            Bitmap.Set_Source (Black);
            Bitmap.Fill_Rect (Cursor);
            Display.Update_Layers;
            delay 0.250;
         end loop;
      end loop;
   end loop;
end LCD_Test;
