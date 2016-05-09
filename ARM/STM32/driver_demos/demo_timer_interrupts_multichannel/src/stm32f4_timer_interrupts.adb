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

with STM32.Board;  use STM32.Board;
with STM32.GPIO;   use STM32.GPIO;

package body STM32F4_Timer_Interrupts is

   -------------
   -- Handler --
   -------------

   protected body Handler is

      -----------------
      -- IRQ_Handler --
      -----------------

      procedure IRQ_Handler is
         Current : Short;
      begin
         if Status (Timer_3, Timer_CC1_Indicated) then
            Clear_Pending_Interrupt (Timer_3, Timer_CC1_Interrupt);

            Toggle (Blue);

            Current := Current_Capture_Value (Timer_3, Channel_1);
            Set_Compare_Value (Timer_3, Channel_1, Current + Channel_1_Period);
         end if;

         if Status (Timer_3, Timer_CC2_Indicated) then
            Clear_Pending_Interrupt (Timer_3, Timer_CC2_Interrupt);

            Toggle (Green);

            Current := Current_Capture_Value (Timer_3, Channel_2);
            Set_Compare_Value (Timer_3, Channel_2, Current + Channel_2_Period);
         end if;

         if Status (Timer_3, Timer_CC3_Indicated) then
            Clear_Pending_Interrupt (Timer_3, Timer_CC3_Interrupt);

            Toggle (Orange);

            Current := Current_Capture_Value (Timer_3, Channel_3);
            Set_Compare_Value (Timer_3, Channel_3, Current + Channel_3_Period);
         end if;

         if Status (Timer_3, Timer_CC4_Indicated) then
            Clear_Pending_Interrupt (Timer_3, Timer_CC4_Interrupt);

            Toggle (Red);

            Current := Current_Capture_Value (Timer_3, Channel_4);
            Set_Compare_Value (Timer_3, Channel_4, Current + Channel_4_Period);
         end if;
      end IRQ_Handler;

   end Handler;

end STM32F4_Timer_Interrupts;
