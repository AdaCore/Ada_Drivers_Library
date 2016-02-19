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

--  This package provides a set of convenience routines for putting characters
--  and strings out to the LCD. It is essentially a wrapper around the ILI9341
--  component driver and an instance of the Bitmapped_Drawing generic package.
--  It adds output routines and convenience controls over the colors and
--  screen orientation. The package is higher-level than an instance of
--  Bitmapped_Drawing, in that a logical line/column is maintained, and as
--  a result text will wrap around as necessary.
--
--  It does not offer the facilities of the LTDC but as a result has fewer
--  requirements for GPIO and SPI resources.
--
--  Note that an instance of Bitmapped_Drawing is an alternative when
--  wrap-around semantics are not required. However, in that case you will also
--  be required to initialize the ILI9341, something that this package does
--  automatically.

with STM32.GPIO; use STM32.GPIO;
with STM32.SPI;  use STM32.SPI;

with STM32.Device;       use STM32.Device;
with STM32.LCD;

with BMP_Fonts;          use BMP_Fonts;
with Bitmapped_Drawing;

package LCD_Std_Out is

   Black       : Bitmapped_Drawing.Graphics_Color renames Bitmapped_Drawing.Black;
   Blue        : Bitmapped_Drawing.Graphics_Color renames Bitmapped_Drawing.Blue;
   Light_Blue  : Bitmapped_Drawing.Graphics_Color renames Bitmapped_Drawing.Light_Blue;
   Green       : Bitmapped_Drawing.Graphics_Color renames Bitmapped_Drawing.Green;
   Cyan        : Bitmapped_Drawing.Graphics_Color renames Bitmapped_Drawing.Cyan;
   Gray        : Bitmapped_Drawing.Graphics_Color renames Bitmapped_Drawing.Gray;
   Magenta     : Bitmapped_Drawing.Graphics_Color renames Bitmapped_Drawing.Magenta;
   Light_Green : Bitmapped_Drawing.Graphics_Color renames Bitmapped_Drawing.Light_Green;
   Brown       : Bitmapped_Drawing.Graphics_Color renames Bitmapped_Drawing.Brown;
   Red         : Bitmapped_Drawing.Graphics_Color renames Bitmapped_Drawing.Red;
   Orange      : Bitmapped_Drawing.Graphics_Color renames Bitmapped_Drawing.Orange;
   Yellow      : Bitmapped_Drawing.Graphics_Color renames Bitmapped_Drawing.Yellow;
   White       : Bitmapped_Drawing.Graphics_Color renames Bitmapped_Drawing.White;

   Default_Text_Color       : constant Bitmapped_Drawing.Graphics_Color := White;
   Default_Background_Color : constant Bitmapped_Drawing.Graphics_Color := Black;
   Default_Font             : constant BMP_Font := Font16x24;
--     Default_Orientation      : constant LCD.Orientations := LCD.Portrait_2;

   --  Changes to these current values will appear on subsequent calls to the
   --  output routines.
   Current_Text_Color       : Bitmapped_Drawing.Graphics_Color :=
                                Default_Text_Color;
   Current_Background_Color : Bitmapped_Drawing.Graphics_Color :=
                                Default_Background_Color;

   procedure Set_Font (To : in BMP_Font);
   --  Changes the current font setting so that subsequent output is in the
   --  specified font.

   procedure Set_Orientation (To : in STM32.LCD.Orientation_Mode);
   --  Configures the screen orientation and fills the screen with the current
   --  background color. All previously displayed content is lost.

   procedure Clear_Screen;

   ----------------------------------------------------------------------------

   --  These routines maintain a logical line and column, such that text will
   --  wrap around to the next "line" when necessary, as determined by the
   --  current orientation of the screen.

   procedure Put_Line (Msg : String);
   --  Note: wraps around to the next line if necessary.
   --  Always calls procedure New_Line automatically after printing the string.

   procedure Put (Msg : String);
   --  Note: wraps around to the next line if necessary.

   procedure Put (Msg : Character);

   procedure New_Line;
   --  A subsequent call to Put or Put_Line will start printing characters at
   --  the beginning of the next line, wrapping around to the top of the LCD
   --  screen if necessary.

   ----------------------------------------------------------------------------

   --  These routines are provided for convenience, as an alternative to
   --  using both this package and an instance of Bitmnapped_Drawing directly,
   --  when wanting both the wrap-around semantics and direct X/Y coordinate
   --  control. You can combine calls to these routines with the ones above but
   --  these do not update the logical line/column state, so more likely you
   --  will use one set or the other. If you only need X/Y coordinate control,
   --  consider directly using an instance of Bitmapped_Drawing.

   procedure Put (X, Y : Natural; Msg : Character);
   --  Prints the character at the specified location. Has no other effect
   --  whatsoever, especially none on the state of the current logical line
   --  or logical column.

   procedure Put (X, Y : Natural; Msg : String);
   --  Prints the string, starting at the specified location. Has no other
   --  effect whatsoever, especially none on the state of the current logical
   --  line or logical column. Does not wrap around.

private

   --  The STM32F4.ILI9341 is automatically initialized with the following:

   Chip_Select : GPIO_Point renames PC2;
   WRX         : GPIO_Point renames PD13;
   Reset       : GPIO_Point renames PD11;
   SPI_Chip    : access SPI_Port := SPI_5'Access;
   SPI_GPIO    : access GPIO_Port := GPIO_F'Access;

   SPI_AF      : constant GPIO_Alternate_Function := GPIO_AF_SPI5;
   SCK_Pin     : constant GPIO_Pin := Pin_7;
   MISO_Pin    : constant GPIO_Pin := Pin_8;
   MOSI_Pin    : constant GPIO_Pin := Pin_9;

end LCD_Std_Out;
