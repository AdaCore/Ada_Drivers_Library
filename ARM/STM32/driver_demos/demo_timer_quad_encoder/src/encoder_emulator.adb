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

package body Encoder_Emulator is

   procedure Initialize;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Enable_Channel (Emulator_Timer, Channel_1);
      Enable_Channel (Emulator_Timer, Channel_2);
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Disable_Channel (Emulator_Timer, Channel_1);
      Disable_Channel (Emulator_Timer, Channel_2);
   end Stop;

   -------------------------------
   -- Emulate_Forward_Direction --
   -------------------------------

   procedure Emulate_Forward_Direction is
   begin
      Configure_Channel_Output
        (Emulator_Timer,
         Channel  => Channel_1,
         Mode     => Toggle,
         State    => Disable,
         Pulse    => Emulator_Period / 4,
         Polarity => Low);

      Configure_Channel_Output
        (Emulator_Timer,
         Channel  => Channel_2,
         Mode     => Toggle,
         State    => Disable,
         Pulse    => (Emulator_Period * 3) / 4,
         Polarity => Low);
   end Emulate_Forward_Direction;

   --------------------------------
   -- Emulate_Backward_Direction --
   --------------------------------

   procedure Emulate_Backward_Direction is
   begin
      Configure_Channel_Output
        (Emulator_Timer,
         Channel  => Channel_1,
         Mode     => Toggle,
         State    => Disable,
         Pulse    => (Emulator_Period * 3) / 4,
         Polarity => Low);

      Configure_Channel_Output
        (Emulator_Timer,
         Channel  => Channel_2,
         Mode     => Toggle,
         State    => Disable,
         Pulse    => Emulator_Period / 4,
         Polarity => Low);
   end Emulate_Backward_Direction;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (Emulator_Points);

      Enable_Clock (Emulator_Timer);

      Configuration.Mode := Mode_AF;
      Configuration.Output_Type := Push_Pull;
      Configuration.Resistors := Pull_Up;
      Configuration.Speed := Speed_100MHz;

      Configure_IO (Emulator_Points, Configuration);
      Configure_Alternate_Function (Emulator_Points, Emulator_AF);

      Configure
        (Emulator_Timer,
         Prescaler     => 0,
         Period        => Emulator_Period,
         Clock_Divisor => Div1,
         Counter_Mode  => Up);

      Enable (Emulator_Timer);

      Emulate_Forward_Direction;
   end Initialize;

begin
   Initialize;
end Encoder_Emulator;
