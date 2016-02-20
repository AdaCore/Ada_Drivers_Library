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

--  This package provides an interface to the on-board "independent watchdog"
--  provided by the STM32F4 family.

package STM32.IWDG is -- the Independent Watchdog

   subtype Countdown_Value is UInt12;

   type Prescalers is
     (Divider_4,
      Divider_8,
      Divider_16,
      Divider_32,
      Divider_64,
      Divider_128,
      Divider_256)
     with Size => 3;
   --  These are the values used to control the rate at which the watchdog
   --  countdown timer counts down to zero. The clock driving the watchdog
   --  downcounter is approximately 32KHz. Thus, for example, dividing by
   --  32 gives about 1 millisecond per count.

   procedure Initialize_Watchdog
     (Prescaler : Prescalers;
      Count     : Countdown_Value);
   --  Sets the watchdog operating parameters. See the ST Micro RM0090
   --  Reference Manual, table 106 (pg 691) for the minimum and maximum timeout
   --  values possible for the range of countdown values, given any specific
   --  prescalar value.
   --
   --  Note that the counter does not begin counting as a result of calling
   --  this routine.

   procedure Start_Watchdog;
   --  Starts the counter counting down to zero. At zero the interrupt is
   --  generated to reset the board.

   procedure Reset_Watchdog;
   --  Reloads the countdown value so that the hardware reset interrupt
   --  is not generated.

end STM32.IWDG;
