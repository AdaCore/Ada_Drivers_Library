------------------------------------------------------------------------------
--                                                                          --
--          Copyright (C) 2017-2018, AdaCore and other contributors         --
--                                                                          --
--      See github.com/AdaCore/Ada_Drivers_Library/graphs/contributors      --
--                           for more information                           --
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

with FE310;
with HiFive1.LEDs; use HiFive1.LEDs;
with FE310.Time; use FE310.Time;

procedure Main is

begin
   --  The SPI flash clock divider should be as small as possible to increase
   --  the execution speed of instructions that are not yet in the instruction
   --  cache.
   FE310.Set_SPI_Flash_Clock_Divider (2);

   --  Load the internal oscillator factory calibration to be sure it
   --  oscillates at a known frequency.
   FE310.Load_Internal_Oscilator_Calibration;

   --  Use the HiFive1 16 MHz crystal oscillator which is more acurate than the
   --  internal oscillator.
   FE310.Use_Crystal_Oscillator;

   HiFive1.LEDs.Initialize;

   --  Blinky!
   loop
      Turn_On (Red_LED);
      Delay_S (1);
      Turn_Off (Red_LED);

      Turn_On (Green_LED);
      Delay_S (1);
      Turn_Off (Green_LED);

      Turn_On (Blue_LED);
      Delay_S (1);
      Turn_Off (Blue_LED);
   end loop;
end Main;
