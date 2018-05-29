------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2018, AdaCore and other contributors            --
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

with FE310_SVD.CLINT; use FE310_SVD.CLINT;

package body FE310.CLINT is

   ------------------
   -- Machine_Time --
   ------------------

   function Machine_Time return Machine_Time_Value is
      High : UInt32;
      Low : UInt32;
   begin
      High := CLINT_Periph.MTIME_HI;
      Low := CLINT_Periph.MTIME_LO;

      --  Handle the case where the timer registers were read during the
      --  incrementation of the high part
      if CLINT_Periph.MTIME_HI /= High then
         High := High + 1;
         Low := 0;
      end if;

      return Machine_Time_Value (High) * 2**32 + Machine_Time_Value (Low);
   end Machine_Time;

   ------------------------------
   -- Set_Machine_Time_Compare --
   ------------------------------

   procedure Set_Machine_Time_Compare (Value : Machine_Time_Value) is
   begin
      CLINT_Periph.MTIMECMP_LO := UInt32'Last;
      CLINT_Periph.MTIMECMP_HI := UInt32 (Value / 2**32);
      CLINT_Periph.MTIMECMP_LO := UInt32 (Value rem 2**32);
   end Set_Machine_Time_Compare;

   --------------------------
   -- Machine_Time_Compare --
   --------------------------

   function Machine_Time_Compare return Machine_Time_Value is
   begin
      return Machine_Time_Value (CLINT_Periph.MTIMECMP_HI) * 2**32 +
             Machine_Time_Value (CLINT_Periph.MTIMECMP_LO);
   end Machine_Time_Compare;


end FE310.CLINT;
