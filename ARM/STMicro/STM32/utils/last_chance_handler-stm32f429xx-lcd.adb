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

--  This version of the LCH writes the exception message and the traceback to
--  the LCD. It uses the package LCD_Std_Out, and that package body elaboration
--  assignes GPIO ports and pins, as well as a SPI port, to initialize the
--  ILI9341 component.

with STM32F429_Discovery;     use STM32F429_Discovery;
with LCD_Std_Out;
with Bitmapped_Drawing;
with BMP_Fonts;
with STM32F4.ILI9341;

with Ada.Real_Time;           use Ada.Real_Time;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Unchecked_Conversion;

pragma Warnings (Off);
with System.Traceback;
with System.Traceback_Entries;
pragma Warnings (On);
with GNAT.Debug_Utilities;

--  From the printed non-symbolic traceback you can get the symbolic traceback
--  using arm-eabi-addr2line on the command line. Specifically, given the
--  addresses displayed on the LCD when an exception is raised, first re-build
--  the application with the "-E" binder switch (and the "-g" builder switch).
--  For example:
--
--  gprbuild -P <project-file> -bargs -E
--
--  Then pass that newly-built executable and those addresses to an invocation
--  of arm-eabi-addr2line. For example:
--
--  arm-eabi-addr2line -e <executable> --functions --demangle <address> ...
--
--  See the GNAT User Guide, section 8.1.14. Stack Traceback for details.

package body Last_Chance_Handler is

   type Integer_Pointer is access all Integer with Storage_Size => 0;

   function As_Integer_Pointer is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Integer_Pointer);

   Traceback       : System.Traceback_Entries.Tracebacks_Array (1 .. 64);
   Traceback_Count : Natural;

   package LCD_Drawing is new Bitmapped_Drawing
     (Color     => STM32F4.ILI9341.Colors,
      Set_Pixel => STM32F4.ILI9341.Set_Pixel);

   package LCD_Text is new LCD_Std_Out (LCD_Drawing);
   --  we use the LCD_Std_Out generic, rather than directly using the Drawing
   --  package, because we want the text to wrap around the screen if necessary

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      pragma Unreferenced (Line);

      Length : constant Integer := As_Integer_Pointer (Msg + 8).all;

      subtype Message is String (1 .. Length);
      --  the string at Msg.all is not null-terminated, apparently, so we
      --  need to avoid the bounds representation issue.

      type Message_Pointer is access all Message with
        Size => Standard'Address_Size,
        Storage_Size => 0;

      function As_Message_Pointer is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Message_Pointer);

      Message_Content : Message renames As_Message_Pointer (Msg + 12).all;

   begin
      Initialize_LEDs;  -- in case no other use in the application
      All_LEDs_Off;

      LCD_Text.Set_Font (To => BMP_Fonts.Font12x12);

      LCD_Text.Put_Line (Message_Content);
      LCD_Text.New_Line;
      LCD_Text.Put_Line ("Traceback:");

      System.Traceback.Call_Chain (Traceback, Traceback'Length, Traceback_Count);
      for J in 1 .. Traceback_Count loop
         LCD_Text.Put_Line (GNAT.Debug_Utilities.Image_C (Traceback (J)));
      end loop;

      loop
         Toggle (Red);
         delay until Clock + Milliseconds (500);
      end loop;
   end Last_Chance_Handler;

end Last_Chance_Handler;
