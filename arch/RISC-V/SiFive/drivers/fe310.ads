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

with HAL; use HAL;
with FE310_SVD.PRIC;

package FE310 is

   LF_Clock_Frequency : constant := 32768;

   function CPU_Frequency return UInt32;
   --  Compute CPU frequency

   procedure Load_Internal_Oscilator_Calibration;
   --  Read the calibration setting from the OTP memory ant write it to the
   --  oscillator configuration register.
   --  After execution of this procedure, the (undivided) internal oscillator
   --  frequency should be about 72 MHz

   subtype PLL_Output_Divider is Integer range 1 .. 128;

   procedure Use_Crystal_Oscillator (Divider : PLL_Output_Divider := 1)
     with Pre => (Divider = 1) or (Divider rem 2 = 0);

   subtype Internal_Oscillator_Divider is Integer range 1 .. 64;

   procedure Use_Internal_Oscillator (Divider : Internal_Oscillator_Divider := 5);

   type PLL_Reference is new FE310_SVD.PRIC.PLLCFG_REFSEL_Field;

   subtype PLL_R is Integer range 1 .. 4;
   subtype PLL_F is Integer range 2 .. 128;

   type PLL_Q is (Div_By_2, Div_By_4, Div_By_8);

   procedure Use_PLL (Reference : PLL_Reference;
                      Internal_Osc_Div : Internal_Oscillator_Divider  := 5;
                      R_Div : PLL_R;
                      F_Mul : PLL_F;
                      Q_Div : PLL_Q;
                      Output_Div : PLL_Output_Divider)
     with Pre => ((Internal_Osc_Div >= 2) and (Internal_Osc_Div <= 12)) and
                 (F_Mul rem 2 = 0);


   subtype SPI_Clock_Divider is Integer range 2 .. 8192;

   procedure Set_SPI_Flash_Clock_Divider (Divider : SPI_Clock_Divider)
     with Pre => Divider rem 2 = 0;

   function SPI_Flash_Clock_Divider return SPI_Clock_Divider;

private

   for PLL_Q use (Div_By_2 => 1, Div_By_4 => 2, Div_By_8 => 3);

end FE310;

