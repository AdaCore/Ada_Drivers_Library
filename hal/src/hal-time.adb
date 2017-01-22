------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

package body HAL.Time is

   Global_Delay_Provider : Any_Delay_Provider := null;

   --------------------
   -- Delay_Provided --
   --------------------

   function Delay_Provided return Boolean is
     (Global_Delay_Provider /= null);

   -------------------
   -- Delay_Seconds --
   -------------------

   procedure Delay_Seconds (S : Natural) is
   begin
      if Global_Delay_Provider /= null then
         Global_Delay_Provider.Delay_Seconds (S);
      end if;
   end Delay_Seconds;

   ------------------------
   -- Delay_Milliseconds --
   ------------------------

   procedure Delay_Milliseconds (Ms : Natural) is
   begin
      if Global_Delay_Provider /= null then
         Global_Delay_Provider.Delay_Milliseconds (Ms);
      end if;
   end Delay_Milliseconds;

   ------------------------
   -- Delay_Microseconds --
   ------------------------

   procedure Delay_Microseconds (Us : Natural) is
   begin
      if Global_Delay_Provider /= null then
         Global_Delay_Provider.Delay_Microseconds (Us);
      end if;
   end Delay_Microseconds;

   -------------------------------
   -- Set_Global_Delay_Provider --
   -------------------------------

   procedure Set_Global_Delay_Provider
     (Provider : not null Any_Delay_Provider)
   is
   begin
      Global_Delay_Provider := Provider;
   end Set_Global_Delay_Provider;

end HAL.Time;
