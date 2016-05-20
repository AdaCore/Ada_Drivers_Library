------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2016, AdaCore                         --
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

with STM32.Board;   use STM32.Board;
with Button;        use Button;
with Ada.Real_Time; use Ada.Real_Time;

package body Driver is

   type Index is mod 4;

   Pattern : array (Index) of User_LED := (Orange, Red, Blue, Green);
   --  The LEDs are not physically laid out "consecutively" in such a way that
   --  we can simply go in enumeral order to get circular rotation. Thus we
   --  define this mapping, using a consecutive index to get the physical LED
   --  blinking order desired.

   task body Controller is
      Period       : Time_Span;
      Next_Release : Time := Clock;
      Next_LED     : Index := 0;
   begin
      Initialize_LEDs;
      All_LEDs_Off;

      loop
         Period := Milliseconds (75);  -- arbitrary
         for K in 1 .. 40 loop   -- arbitrary
            Turn_Off (Pattern (Next_LED));

            if Button.Current_Direction = Counterclockwise then
               Next_LED := Next_LED - 1;
            else
               Next_LED := Next_LED + 1;
            end if;

            Turn_On (Pattern (Next_LED));

            Next_Release := Next_Release + Period;
            delay until Next_Release;
         end loop;

         Period := Milliseconds (50);  -- arbitrary
         for K in 1 .. 5 loop   -- arbitrary
            All_LEDs_On;
            Next_Release := Next_Release + Period;
            delay until Next_Release;

            All_LEDs_Off;
            Next_Release := Next_Release + Period;
            delay until Next_Release;
         end loop;
      end loop;
   end Controller;

end Driver;
