------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2016, AdaCore                        --
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

with STM32F4.ILI9341;
with BMP_Fonts;  use BMP_Fonts;
with Bitmapped_Drawing;

generic
   with package Drawing is new Bitmapped_Drawing
     (Color => STM32F4.ILI9341.Colors, Set_Pixel => <>);
package LCD_Std_Out is

   procedure Initialize;

   package LCD renames STM32F4.ILI9341;

   Black       : LCD.Colors renames LCD.Black;
   Blue        : LCD.Colors renames LCD.Blue;
   Light_Blue  : LCD.Colors renames LCD.Light_Blue;
   Green       : LCD.Colors renames LCD.Green;
   Cyan        : LCD.Colors renames LCD.Cyan;
   Gray        : LCD.Colors renames LCD.Gray;
   Magenta     : LCD.Colors renames LCD.Magenta;
   Light_Green : LCD.Colors renames LCD.Light_Green;
   Brown       : LCD.Colors renames LCD.Brown;
   Red         : LCD.Colors renames LCD.Red;
   Orange      : LCD.Colors renames LCD.Orange;
   Yellow      : LCD.Colors renames LCD.Yellow;
   White       : LCD.Colors renames LCD.White;

   Default_Text_Color       : constant LCD.Colors := White;
   Default_Background_Color : constant LCD.Colors := Black;
   Default_Font             : constant BMP_Font := Font16x24;
   Default_Orientation      : constant LCD.Orientations := LCD.Portrait_2;

   --  Changes to these current values will appear on subsequent calls to the
   --  output routines.
   Current_Text_Color       : LCD.Colors := Default_Text_Color;
   Current_Background_Color : LCD.Colors := Default_Background_Color;

   procedure Set_Font (To : in BMP_Font);
   --  Changes the current font setting so that subsequent output is in the
   --  specified font.

   procedure Set_Orientation (To : in LCD.Orientations);
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

end LCD_Std_Out;
