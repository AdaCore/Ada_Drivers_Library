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

with Cortex_M_SVD.SysTick; use Cortex_M_SVD.SysTick;

with HAL; use HAL;

package body Cortex_M.Systick is

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Source             : Clock_Source;
      Generate_Interrupt : Boolean;
      Reload_Value       : HAL.UInt24)
   is
   begin
      SysTick_Periph.CSR.CLKSOURCE :=
        (case Source is
            when CPU_Clock      => Cpu_Clk,
            when External_Clock => External_Clk);

      SysTick_Periph.CSR.TICKINT :=
        (if Generate_Interrupt then Enable else Disable);

      SysTick_Periph.RVR.RELOAD := Reload_Value;
      SysTick_Periph.CVR.CURRENT := Counter;
   end Configure;

   ------------
   -- Enable --
   ------------

   procedure Enable is
   begin
      SysTick_Periph.CSR.ENABLE := Enable;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable is
   begin
      SysTick_Periph.CSR.ENABLE := Disable;
   end Disable;

   ---------------------
   -- Counted_To_Zero --
   ---------------------

   function Counted_To_Zero return Boolean is
   begin
      return SysTick_Periph.CSR.COUNTFLAG;
   end Counted_To_Zero;

   -------------
   -- Counter --
   -------------

   function Counter return HAL.UInt24 is
   begin
      return SysTick_Periph.CVR.CURRENT;
   end Counter;

end Cortex_M.Systick;
