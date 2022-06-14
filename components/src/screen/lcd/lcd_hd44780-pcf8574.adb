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

pragma Restrictions (No_Allocators);
pragma Restrictions (No_Implicit_Heap_Allocations);

package body LCD_HD44780.PCF8574 is

   type Shadow_Bits is array (Bit_Number) of Boolean
   with Pack, Size => 8;

   I2C_Shadow : Shadow_Bits;

   procedure Write_Shadow (This : LCD_PCF8574)
   with Inline;

   ------------
   -- Create --
   ------------

   function Create (Display_Width  : Char_Position;
                    Display_Height : Line_Position;
                    Time           : not null HAL.Time.Any_Delays;
                    Expander       : Standard.PCF8574.Any_PCF8574_Module;
                    Mapping        : Bit_Mapping := Standard_Mapping) return LCD_PCF8574
   is
   begin
      I2C_Shadow := (others => False);
      return L : LCD_PCF8574 (Display_Width, Display_Height, Time) do
         L.I2C_Driver := Expander;
         L.Pins       := Mapping;
      end return;
   end Create;

   ------------------
   -- Write_Shadow --
   ------------------

   procedure Write_Shadow (This : LCD_PCF8574)
   is
      Shadow_As_Byte : UInt8
        with Address => I2C_Shadow'Address;
   begin
      This.I2C_Driver.Set (Shadow_As_Byte);
   end Write_Shadow;

   -------------------
   -- Set_Backlight --
   -------------------

   overriding
   procedure Set_Backlight (This  : in out LCD_PCF8574;
                            Is_On : Boolean := True)
   is
      Bl : Boolean renames I2C_Shadow (This.Pins (Backlight));
   begin
      Bl := Is_On;
      This.Write_Shadow;
   end Set_Backlight;

   -------------------
   -- Toggle_Enable --
   -------------------

   overriding
   procedure Toggle_Enable (This : LCD_PCF8574)
   is
      Bit : constant Bit_Number := This.Pins (Enable);
      En : Boolean renames I2C_Shadow (Bit);
   begin
      En := True;
      This.Write_Shadow;
      En := False;
      This.Write_Shadow;
      This.Time.Delay_Microseconds (280);
   end Toggle_Enable;

   ------------
   -- Output --
   ------------

   overriding
   procedure Output (This    : LCD_PCF8574;
                     Cmd     : UInt8;
                     Is_Data : Boolean := False)
   is
      RW_Bit : constant Bit_Number := This.Pins (ReadWrite);
      RS_Bit : constant Bit_Number := This.Pins (RegSel);
      B0_Bit : constant Bit_Number := This.Pins (D0);
      B1_Bit : constant Bit_Number := This.Pins (D1);
      B2_Bit : constant Bit_Number := This.Pins (D2);
      B3_Bit : constant Bit_Number := This.Pins (D3);
      RW : Boolean renames I2C_Shadow (RW_Bit);
      RS : Boolean renames I2C_Shadow (RS_Bit);
      P0 : Boolean renames I2C_Shadow (B0_Bit);
      P1 : Boolean renames I2C_Shadow (B1_Bit);
      P2 : Boolean renames I2C_Shadow (B2_Bit);
      P3 : Boolean renames I2C_Shadow (B3_Bit);
   begin
      --  control pins
      RW := False;
      RS := Is_Data;
      --  write data
      --  high nibble first
      P0 := (Cmd and 16#10#) /= 0;
      P1 := (Cmd and 16#20#) /= 0;
      P2 := (Cmd and 16#40#) /= 0;
      P3 := (Cmd and 16#80#) /= 0;

      This.Toggle_Enable;

      P0 := (Cmd and 16#01#) /= 0;
      P1 := (Cmd and 16#02#) /= 0;
      P2 := (Cmd and 16#04#) /= 0;
      P3 := (Cmd and 16#08#) /= 0;

      This.Toggle_Enable;

      This.Time.Delay_Microseconds (50);
   end Output;

   --------------------
   -- Init_4bit_Mode --
   --------------------

   overriding
   procedure Init_4bit_Mode (This : LCD_PCF8574)
   is
      RW_Bit : constant Bit_Number := This.Pins (ReadWrite);
      RS_Bit : constant Bit_Number := This.Pins (RegSel);
      En_Bit : constant Bit_Number := This.Pins (Enable);
      B0_Bit : constant Bit_Number := This.Pins (D0);
      B1_Bit : constant Bit_Number := This.Pins (D1);
      B2_Bit : constant Bit_Number := This.Pins (D2);
      B3_Bit : constant Bit_Number := This.Pins (D3);
      RW : Boolean renames I2C_Shadow (RW_Bit);
      RS : Boolean renames I2C_Shadow (RS_Bit);
      En : Boolean renames I2C_Shadow (En_Bit);
      P0 : Boolean renames I2C_Shadow (B0_Bit);
      P1 : Boolean renames I2C_Shadow (B1_Bit);
      P2 : Boolean renames I2C_Shadow (B2_Bit);
      P3 : Boolean renames I2C_Shadow (B3_Bit);
   begin
      I2C_Shadow := (others => False);

      --  all control lines low
      RS := False;
      En := False;
      RW := False;
      --  write 1 into pins 0 and 1
      P0 := True;
      P1 := True;
      P2 := False;
      P3 := False;
      This.Write_Shadow;

      This.Toggle_Enable;
      This.Time.Delay_Milliseconds (5);

      --  send last command again (is still in register, just toggle E)
      This.Toggle_Enable;
      This.Time.Delay_Milliseconds (5);

      --  send last command a third time
      This.Toggle_Enable;

      This.Time.Delay_Microseconds (150);

      --  set 4 bit mode, clear data bit 0
      P0 := False;

      This.Toggle_Enable;

   end Init_4bit_Mode;

end LCD_HD44780.PCF8574;
