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

--
--  From the Hitachi HD44780U data sheet:
--
--  The HD44780U has two 8-bit registers, an instruction register (IR)
--  and a data register (DR).
--
--  The IR stores instruction codes, such as display clear and cursor
--  shift, and address information for display data RAM (DDRAM) and
--  character generator RAM (CGRAM).  The IR can only be written from
--  the MPU.
--
--  The DR temporarily stores data to be written into DDRAM or CGRAM
--  and temporarily stores data to be read from DDRAM or CGRAM.  Data
--  written into the DR from the MPU is automatically written into
--  DDRAM or CGRAM by an internal operation.  The DR is also used for
--  data storage when reading data from DDRAM or CGRAM.  When address
--  information is written into the IR, data is read and then stored
--  into the DR from DDRAM or CGRAM by an internal operation.  Data
--  transfer between the MPU is then completed when the MPU reads the
--  DR.  After the read, data in DDRAM or CGRAM at the next address is
--  sent to the DR for the next read from the MPU.  By the register
--  selector (RS) signal, these two registers can be selected



package body LCD_HD44780 is

   --  initialize display
   procedure Initialize (This : not null Any_LCD_Module) is
   begin
      This.Init_4bit_Mode;

      --  now we can use the standard Command routine for set up
      Command (This, Commands.Display_On); --  implies blink off and cursor off
      Clear_Screen (This);
      Command (This, Commands.Entry_Inc);
   end Initialize;

   --  output at the current cursor location
   procedure Put (This : not null Any_LCD_Module; C : Character) is
   begin
      This.Output (Character'Pos (C), Is_Data => True);
   end Put;

   --  output at the current cursor location
   procedure Put (This : not null Any_LCD_Module; Text : String) is
   begin
      for C of Text loop
         Put (This, C);
      end loop;
   end Put;

   --  output at the specified cursor location
   procedure Put (This : not null Any_LCD_Module;
                  X    : Char_Position;
                  Y    : Line_Position;
                  Text : String)
   is
   begin
      Goto_XY (This, X, Y);
      Put (This, Text);
   end Put;

   --  output the command code Cmd to the display
   procedure Command (This : not null Any_LCD_Module; Cmd : Command_Type) is
   begin
      This.Output (UInt8 (Cmd), Is_Data => False);
   end Command;

   --  clear display and move cursor to home position
   procedure Clear_Screen (This : not null Any_LCD_Module) is
   begin
      Command (This, Commands.Clear);
      This.Time.Delay_Microseconds (1_500);
   end Clear_Screen;

   --  move cursor to home position
   procedure Home (This : not null Any_LCD_Module) is
   begin
      Command (This, 16#02#);
   end Home;

   --  move cursor into line Y and before character position X.  Lines
   --  are numbered 1 to 2 (or 1 to 4 on big displays).  The left most
   --  character position is Y = 1.  The right most position is
   --  defined by Lcd.Display.Width;
   procedure Goto_XY (This : not null Any_LCD_Module;
                      X    : Char_Position;
                      Y    : Line_Position)
   is
   begin
      if X > This.Display_Width then return; end if;
      if Y > This.Display_Height then return; end if;
      case Y is
      when 1 => Command (This, 16#80# + Command_Type (X) - 1);
      when 2 => Command (This, 16#C0# + Command_Type (X) - 1);
      when 3 => Command (This, 16#80# + Command_Type (X + This.Display_Width) - 1);
      when 4 => Command (This, 16#C0# + Command_Type (X + This.Display_Width) - 1);
      --  when others => null;
      end case;
   end Goto_XY;

end LCD_HD44780;
