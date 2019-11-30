------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

with SAM.Main_Clock;
with SAM.Clock_Generator;        use SAM.Clock_Generator;
with SAM.Clock_Generator.IDs;    use SAM.Clock_Generator.IDs;
with SAM.Oscillators_Controller; use SAM.Oscillators_Controller;

package body SAM.Clock_Setup_120Mhz is

   -----------------------
   -- Initialize_Clocks --
   -----------------------

   procedure Initialize_Clocks is
   begin
      --  Turn on the oscilator controller
      SAM.Main_Clock.OSCCTRL_On;

      --  OSCULP32K is enabled at reset

      --  32.768 Khz from the Ultra Low Power Internal Oscilator (OSCULP32K)
      Configure_Generator (Id             => Clk_32Khz,
                           Source         => OSCULP32K,
                           DIV_Select     => False,
                           DIV            => 1,
                           Run_In_Standby => False,
                           Output_Enable  => True);


      --  Temporarly set the CPU clock generator to 32Khz while we change the
      --  other configurations.
      Configure_Generator (Id             => Clk_CPU,
                           Source         => OSCULP32K,
                           DIV_Select     => False,
                           DIV            => 1,
                           Run_In_Standby => False,
                           Output_Enable  => True);

      --  Set DFLL48 input to generator at 32.768 Khz
      Configure_Periph_Channel (IDs.OSCCTRL_DFLL48, Clk_32Khz);


      --  Configure DFLL to produce a 48Mhz
      Configure_DFLL (On_Demand_Control     => False,
                      Run_On_Standby        => False,
                      USB_Clock_Recovery    => True,
                      Wait_Lock             => True,
                      Bypass_Coarse_Lock    => False,
                      Quick_Lock_Disable    => False,
                      Chill_Cycle_Disable   => True,
                      Lose_Lock_After_Wake  => False,
                      Stable_DFLL_Frequency => False,
                      Operating_Mode        => Open_Loop_Mode,
                      Coarse_Maximum_Step   => 1,
                      Fine_Maximum_Step     => 1,
                      Multiply_Factor       => 0);


      --  48 Mhz from DFLL
      Configure_Generator (Id             => Clk_48Mhz,
                           Source         => DFLL,
                           DIV_Select     => False,
                           DIV            => 1,
                           Run_In_Standby => False,
                           Output_Enable  => True);

      --  2Mhz from 48Mhz DFLL devided by 24
      Configure_Generator (Id             => Clk_2Mhz,
                           Source         => DFLL,
                           DIV_Select     => False,
                           DIV            => 24,
                           Run_In_Standby => False,
                           Output_Enable  => True);

      --  Set DPLL0 input to generator at 2Mhz
      Configure_Periph_Channel (IDs.OSCCTRL_FDPLL0, Clk_2Mhz);

      --  Configure DPLL0 to produce a 120Mhz clock
      Configure_DPLL (DPLL                    => 0,
                      On_Demand_Control       => False,
                      Run_On_Standby          => False,
                      Loop_Divider_Fractional => 0,
                      Loop_Divider_Integer    => 59,
                      Clock_Divider           => 0,
                      DCO_Filter_Enable       => False,
                      Sigma_Delta_DCO_Filter  => 0,
                      Lock_Bypass             => False,
                      Lock_Time               => Default,
                      Reference_Clock         => GCLK_Ref,
                      Wakeup_Fast             => False,
                      Prop_Integral_Filter    => 0);

      --  Set CPU clock to 120Mhz from the DPLL0
      Configure_Generator (Id             => Clk_CPU,
                           Source         => DPLL0,
                           DIV_Select     => False,
                           DIV            => 1,
                           Run_In_Standby => False,
                           Output_Enable  => True);
   end Initialize_Clocks;

end SAM.Clock_Setup_120Mhz;
