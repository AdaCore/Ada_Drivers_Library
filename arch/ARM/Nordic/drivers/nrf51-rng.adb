------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

with NRF51_SVD.RNG; use NRF51_SVD.RNG;

package body nRF51.RNG is

   -------------------------------------
   -- Enable_Digital_Error_Correction --
   -------------------------------------

   procedure Enable_Digital_Error_Correction is
   begin
      RNG_Periph.CONFIG.DERCEN := Enabled;
   end Enable_Digital_Error_Correction;

   --------------------------------------
   -- Disable_Digital_Error_Correction --
   --------------------------------------

   procedure Disable_Digital_Error_Correction is
   begin
      RNG_Periph.CONFIG.DERCEN := Disabled;
   end Disable_Digital_Error_Correction;

   ----------
   -- Read --
   ----------

   function Read return UInt8 is
   begin
      --  Clear event
      RNG_Periph.EVENTS_VALRDY := 0;

      --  Start random numnber generator
      RNG_Periph.TASKS_START := 1;

      while RNG_Periph.EVENTS_VALRDY = 0 loop
         null;
      end loop;

      return RNG_Periph.VALUE.VALUE;
   end Read;

end nRF51.RNG;
