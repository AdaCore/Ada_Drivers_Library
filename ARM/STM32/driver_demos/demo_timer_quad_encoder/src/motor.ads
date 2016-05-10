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

--  This package acts as if it is a motor containing a quadrature encoder. As
--  such, one can query the current wheel rotation direction, which is based
--  on the internal encoder inputs.

--  It is this package that contains and manages the timer that is in quad
--  encoder mode, taking the two signals as inputs and providing the counts
--  and direction information. In effect is is decoding the hardware signals
--  produced by the encoder hardware, but they are not known as "decoders" in
--  the industry.

--  The set up and use of the timer in this "encoder mode" is the purpose of
--  the demonstration.

with STM32.GPIO;         use STM32.GPIO;
with STM32.Timers;       use STM32.Timers;
with STM32.Device;       use STM32.Device;
with HAL;                use HAL;

package Motor is
   pragma Elaborate_Body;

   function Encoder_Count return Word;

   type Direction is (Forward, Backward);

   function Current_Direction return Direction;

private

   Encoder_Tach0 : constant GPIO_Point := PA8;
   Encoder_Tach1 : constant GPIO_Point := PE11;

   Encoder_Timer : Timer renames Timer_1;

   Encoder_AF : constant GPIO_Alternate_Function := GPIO_AF_TIM1;

end Motor;
