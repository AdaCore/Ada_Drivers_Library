------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2022, AdaCore                      --
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

--  driver for text based LCDs connected via I2C port expander PCF8574

with HAL.Time;
with PCF8574;

package LCD_HD44780.PCF8574 is

   --  Describe the internal wiring between the PCF8574 output pins
   --  and the HD44780 input pins.  The PCF8574 pins are the bit numbers.

   type Bit_Number is range 0 .. 7;

   type Bit_Mapping is array (HD44780_4bit_Pins) of Bit_Number;

   Standard_Mapping : constant Bit_Mapping :=
     (Enable    => 2,
      ReadWrite => 1,
      RegSel    => 0,
      Backlight => 3,
      D0        => 4,
      D1        => 5,
      D2        => 6,
      D3        => 7);

   type LCD_PCF8574 (Display_Width  : Char_Position;
                     Display_Height : Line_Position;
                     Time           : not null HAL.Time.Any_Delays)
     is limited new LCD_Module with private;

   function Create (Display_Width  : Char_Position;
                    Display_Height : Line_Position;
                    Time           : not null HAL.Time.Any_Delays;
                    Expander       : Standard.PCF8574.Any_PCF8574_Module;
                    Mapping        : Bit_Mapping := Standard_Mapping) return LCD_PCF8574;

   overriding
   procedure Set_Backlight (This  : in out LCD_PCF8574;
                            Is_On : Boolean := True);

private

   type LCD_PCF8574 (Display_Width  : Char_Position;
                     Display_Height : Line_Position;
                     Time           : not null HAL.Time.Any_Delays)
     is limited new LCD_Module (Display_Width, Display_Height, Time) with
   record
      I2C_Driver : Standard.PCF8574.Any_PCF8574_Module;
      Pins       : Bit_Mapping;
   end record;

   overriding
   procedure Toggle_Enable (This : LCD_PCF8574);

   overriding
   procedure Output (This    : LCD_PCF8574;
                     Cmd     : UInt8;
                     Is_Data : Boolean := False);

   overriding
   procedure Init_4bit_Mode (This : LCD_PCF8574);

end LCD_HD44780.PCF8574;
