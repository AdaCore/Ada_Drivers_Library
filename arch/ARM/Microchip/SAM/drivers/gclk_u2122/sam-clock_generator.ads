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

with HAL; use HAL;

package SAM.Clock_Generator is

   type Clock_Source is (XOSC0,     -- 0 oscillator output
                         XOSC1,     -- 1 oscillator output
                         GCLK_IN,   -- Generator input pad (GCLK_IO)
                         GCLK_GEN1, -- Generic clock generator 1 output
                         OSCULP32K, -- OSCULP32K oscillator output
                         XOSC32K,   -- XOSC32K oscillator output
                         DFLL,      -- DFLL oscillator output
                         DPLL0,     -- DPLL0 output
                         DPLL1      -- DPLL1 output
                        );

   type Generator_Id is range 0 .. 11;

   type Peripheral_Id is range 0 .. 47;

   procedure Configure_Generator (Id             : Generator_Id;
                                  Source         : Clock_Source;
                                  DIV_Select     : Boolean;
                                  DIV            : UInt16;
                                  Run_In_Standby : Boolean;
                                  Output_Enable  : Boolean)
     with Pre => (if Id /= 1 then DIV <= 512);
   --  Also enabled the generator

   procedure Disable_Generator (Id : Generator_Id);

   procedure Configure_Periph_Channel (Periph : Peripheral_Id;
                                       Gen    : Generator_Id);

   procedure Disable_Periph_Channel (Periph : Peripheral_Id);

end SAM.Clock_Generator;
