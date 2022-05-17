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

--  driver for text based LCDs in the typical sizes of 8x1, 16x2 or 20x4

with HAL; use HAL;
with HAL.Time;

package LCD_HD44780 is

   subtype Char_Position is UInt8 range 1 .. 40;
   subtype Line_Position is UInt8 range 1 .. 4;

   --  typical display formats are:
   --     8x1          16x1   20x1
   --     8x2   12x2   16x2   20x2   24x2   40x2
   --                  16x4   20x4          40x4

   type LCD_Module (Display_Width  : Char_Position;
                    Display_Height : Line_Position;
                    Time           : not null HAL.Time.Any_Delays)
   is abstract tagged limited private;

   type Any_LCD_Module is access all LCD_Module'Class;

   procedure Initialize (This : in out LCD_Module);

   procedure Put (This : in out LCD_Module; C : Character) with Inline;
   procedure Put (This : in out LCD_Module; Text : String);
   --  output at the current cursor location

   procedure Put (This : in out LCD_Module;
                  X    : Char_Position;
                  Y    : Line_Position;
                  Text : String);
   --  output at the specified cursor location

   procedure Clear_Screen (This : in out LCD_Module);
   --  clear display and move cursor to home position

   procedure Home (This : in out LCD_Module);
   --  move cursor to home position

   procedure Goto_XY (This : in out LCD_Module; X : Char_Position; Y : Line_Position);
   --  move cursor into line Y and before character position X.  Lines
   --  are numbered 1 to 2 (or 1 to 4 on big displays).  The left most
   --  character position is Y = 1.  The right most position is
   --  defined by Display_Width;

   procedure Set_Backlight (This : in out LCD_Module;
                            Is_On : Boolean := True) is abstract;

   type HD44780_Pins is (Enable, ReadWrite, RegSel, Backlight, D0, D1, D2, D3, D4, D5, D6, D7);
   subtype HD44780_4bit_Pins is HD44780_Pins range Enable .. D3;

   --
   --  Custom Characters
   --
   type Custom_Character_Definition is array (1 .. 8) of UInt5;
   subtype Custom_Character_Index is Integer range 0 .. 7;
   procedure Create_Custom_Character (This       : in out LCD_Module;
                                      Position   :        Custom_Character_Index;
                                      Definition :        Custom_Character_Definition);
   function Custom_Char (From_Index : Custom_Character_Index) return Character;


   type Command_Type is new UInt8;
   procedure Command (This : in out LCD_Module; Cmd : Command_Type)
   with Inline;
   --  send the command code Cmd to the display

   package Commands is
      Clear                   : constant Command_Type := 16#01#;
      Home                    : constant Command_Type := 16#02#;

      --  interface data width and number of lines
      Mode_4bit_1line         : constant Command_Type := 16#20#;
      Mode_4bit_2line         : constant Command_Type := 16#28#;
      Mode_8bit_1line         : constant Command_Type := 16#30#;
      Mode_8bit_2line         : constant Command_Type := 16#38#;

      --  display on/off, cursor on/off, blinking char at cursor position
      Display_Off             : constant Command_Type := 16#08#;
      Display_On              : constant Command_Type := 16#0C#;
      Display_On_Blink        : constant Command_Type := 16#0D#;
      Display_On_Cursor       : constant Command_Type := 16#0E#;
      Display_On_Cursor_Blink : constant Command_Type := 16#0F#;

      --  entry mode
      Entry_Inc               : constant Command_Type := 16#06#;
      Entry_Dec               : constant Command_Type := 16#04#;
      Entry_Shift_Inc         : constant Command_Type := 16#07#;
      Entry_Shift_Dec         : constant Command_Type := 16#05#;

      --  cursor/shift display
      Move_Cursor_Left        : constant Command_Type := 16#10#;
      Move_Cursor_Right       : constant Command_Type := 16#14#;
      Move_Display_Left       : constant Command_Type := 16#18#;
      Move_Display_Right      : constant Command_Type := 16#1C#;
   end Commands;

private

   type LCD_Module (Display_Width  : Char_Position;
                    Display_Height : Line_Position;
                    Time           : not null HAL.Time.Any_Delays)
     is abstract tagged limited null record;

   procedure Toggle_Enable (This : LCD_Module) is null;

   procedure Output (This    : LCD_Module;
                     Cmd     : UInt8;
                     Is_Data : Boolean := False) is null;

   procedure Init_4bit_Mode (This : LCD_Module) is null;

end LCD_HD44780;
