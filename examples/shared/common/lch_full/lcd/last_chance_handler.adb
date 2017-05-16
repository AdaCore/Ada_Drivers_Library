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

--  This version of the LCH writes the exception name and message (if any),
--  followed by the traceback, if any, to the LCD. It uses the package
--  LCD_Std_Out, and that package body elaboration assignes GPIO ports
--  and pins, as well as a SPI port, to initialize the ILI9341 component.

--  Note this version requires building with the ravenscar-full runtime.

--  The non-symbolic traceback addresses are written to the LCD. From these
--  addresses you can get the symbolic traceback using arm-eabi-addr2line on
--  the command line. For example:
--
--  arm-eabi-addr2line -e <executable> --functions --demangle <address> ...
--
--  Note that you must build with the "-E" binder switch for the traceback
--  addresses to be stored with exception occurrences.
--
--  See the GNAT User Guide, section 8.1.14. Stack Traceback for details.

with Ada.Real_Time;             use Ada.Real_Time;
with STM32.Board;               use STM32.Board;
with LCD_Std_Out;
with BMP_Fonts;

with Ada.Exceptions.Traceback;  use Ada.Exceptions.Traceback;
with GNAT.Debug_Utilities;      use GNAT.Debug_Utilities;

package body Last_Chance_Handler is

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Error : Exception_Occurrence) is
   begin
      Initialize_LEDs;  -- in case no other use already within the application
      All_LEDs_Off;

      LCD_Std_Out.Set_Font (To => BMP_Fonts.Font12x12);

      LCD_Std_Out.Clear_Screen;

      No_Exceptions_Propagated : begin
         LCD_Std_Out.Put_Line (Exception_Name (Error));
         LCD_Std_Out.Put_Line (Exception_Message (Error));
         LCD_Std_Out.New_Line;
         LCD_Std_Out.Put_Line ("Traceback:");

         for Call_Stack_Address of Tracebacks (Error) loop
            LCD_Std_Out.Put_Line (Image_C (Call_Stack_Address));
         end loop;
      exception
         when others => null;
      end No_Exceptions_Propagated;

      loop
         Toggle (LCH_LED);
         delay until Clock + Milliseconds (500);
      end loop;
   end Last_Chance_Handler;

end Last_Chance_Handler;
