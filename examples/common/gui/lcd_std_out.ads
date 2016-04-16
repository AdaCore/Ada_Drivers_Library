------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a set of convenience routines for putting characters
--  and strings out to the LCD.

with STM32.GPIO; use STM32.GPIO;
with STM32.SPI;  use STM32.SPI;

with STM32.Device;       use STM32.Device;
with STM32.LCD;

with BMP_Fonts;          use BMP_Fonts;
with Bitmapped_Drawing;

package LCD_Std_Out is

   Black       : Bitmapped_Drawing.Graphics_Color renames
     Bitmapped_Drawing.Black;
   Blue        : Bitmapped_Drawing.Graphics_Color renames
     Bitmapped_Drawing.Blue;
   Light_Blue  : Bitmapped_Drawing.Graphics_Color renames
     Bitmapped_Drawing.Light_Blue;
   Green       : Bitmapped_Drawing.Graphics_Color renames
     Bitmapped_Drawing.Green;
   Cyan        : Bitmapped_Drawing.Graphics_Color renames
     Bitmapped_Drawing.Cyan;
   Gray        : Bitmapped_Drawing.Graphics_Color renames
     Bitmapped_Drawing.Gray;
   Magenta     : Bitmapped_Drawing.Graphics_Color renames
     Bitmapped_Drawing.Magenta;
   Light_Green : Bitmapped_Drawing.Graphics_Color renames
     Bitmapped_Drawing.Light_Green;
   Brown       : Bitmapped_Drawing.Graphics_Color renames
     Bitmapped_Drawing.Brown;
   Red         : Bitmapped_Drawing.Graphics_Color renames
     Bitmapped_Drawing.Red;
   Orange      : Bitmapped_Drawing.Graphics_Color renames
     Bitmapped_Drawing.Orange;
   Yellow      : Bitmapped_Drawing.Graphics_Color renames
     Bitmapped_Drawing.Yellow;
   White       : Bitmapped_Drawing.Graphics_Color renames
     Bitmapped_Drawing.White;

   Default_Text_Color       : constant Bitmapped_Drawing.Graphics_Color :=
     White;
   Default_Background_Color : constant Bitmapped_Drawing.Graphics_Color :=
     Black;
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
   SPI_Chip    : SPI_Port renames SPI_5;

   SPI_AF      : constant GPIO_Alternate_Function := GPIO_AF_SPI5;
   SPI_SCK     : GPIO_Point renames PF7;
   SPI_MISO    : GPIO_Point renames PF8;
   SPI_MOSI    : GPIO_Point renames PF9;

end LCD_Std_Out;
