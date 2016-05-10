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

package body Motor is

   procedure Initialize;

   -----------------------
   -- Current_Direction --
   -----------------------

   function Current_Direction return Direction is
   begin
      case Current_Counter_Mode (Encoder_Timer) is
         when Up     => return Forward;
         when Down   => return Backward;
         when others => raise Program_Error;
      end case;
   end Current_Direction;

   -------------------
   -- Encoder_Count --
   -------------------

   function Encoder_Count return Word is
   begin
      return Current_Counter (Encoder_Timer);
   end Encoder_Count;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (Encoder_Tach0);
      Enable_Clock (Encoder_Tach1);

      Enable_Clock (Encoder_Timer);

      Configuration.Mode := Mode_AF;
      Configuration.Output_Type := Push_Pull;
      Configuration.Resistors := Pull_Up;
      Configuration.Speed := Speed_100MHz;

      Configure_IO (Encoder_Tach0, Configuration);
      Configure_Alternate_Function (Encoder_Tach0, Encoder_AF);

      Configure_IO (Encoder_Tach1, Configuration);
      Configure_Alternate_Function (Encoder_Tach1, Encoder_AF);

      Configure_Encoder_Interface
        (Encoder_Timer,
         Mode         => Encoder_Mode_TI1_TI2,
         IC1_Polarity => Rising,
         IC2_Polarity => Rising);

      Configure
        (Encoder_Timer,
         Prescaler     => 0,
         Period        => Word (Short'Last),
         Clock_Divisor => Div1,
         Counter_Mode  => Up);

      Configure_Channel_Input
        (Encoder_Timer,
         Channel   => Channel_1,
         Polarity  => Rising,
         Selection => Direct_TI,
         Prescaler => Div1,
         Filter    => 0);

      Configure_Channel_Input
        (Encoder_Timer,
         Channel   => Channel_2,
         Polarity  => Rising,
         Selection => Direct_TI,
         Prescaler => Div1,
         Filter    => 0);

      Enable_Channel (Encoder_Timer, Channel_1);
      Enable_Channel (Encoder_Timer, Channel_2);

      Set_Counter (Encoder_Timer, Short'(0));

      Enable (Encoder_Timer);
   end Initialize;

begin
   Initialize;
end Motor;
