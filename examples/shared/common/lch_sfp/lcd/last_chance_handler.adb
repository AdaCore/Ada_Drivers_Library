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

--  This version of the LCH writes information about the unhandled exception
--  to the LCD. Note that it uses the package LCD_Std_Out, and that package
--  body's elaboration assignes GPIO ports and pins, as well as a SPI port,
--  to initialize the ILI9341 component.

--  Note this version is for use with the ravenscar-sfp runtime, in which full
--  exception semantics are not available.

with STM32.Board; use STM32.Board;

with LCD_Std_Out;
with BMP_Fonts;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Unchecked_Conversion;

package body Last_Chance_Handler is

   package LCD_Text renames LCD_Std_Out;
   --  we use the LCD_Std_Out generic, rather than directly using the Drawing
   --  package, because we want the text to wrap around the screen if necessary

   procedure Put (Ptr : System.Address);

   ---------
   -- Put --
   ---------

   procedure Put (Ptr : System.Address) is

      type C_String_Ptr is access String (1 .. Positive'Last) with
        Storage_Size => 0, Size => Standard'Address_Size;

      function As_C_String_Ptr is new Ada.Unchecked_Conversion
        (System.Address, C_String_Ptr);

      Msg_Str : constant C_String_Ptr := As_C_String_Ptr (Ptr);

   begin
      for J in Msg_Str'Range loop
         exit when Msg_Str (J) = Character'Val (0);
         LCD_Text.Put (Msg_Str (J));
      end loop;
   end Put;

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
   begin
      Initialize_LEDs;  -- in case no other use in the application
      All_LEDs_Off;

      LCD_Text.Set_Font (To => BMP_Fonts.Font12x12);

      LCD_Text.Clear_Screen;

      if Line /= 0 then
         LCD_Text.Put ("Predefined exception at ");
         Put (Msg);
         LCD_Text.Put (" line");
         LCD_Text.Put (Line'Img);
      else
         LCD_Text.Put ("User-defined exception, message: ");
         Put (Msg);
      end if;
      LCD_Text.New_Line;

      loop
         LCH_LED.Toggle;
         delay until Clock + Milliseconds (500);
      end loop;
   end Last_Chance_Handler;

end Last_Chance_Handler;
