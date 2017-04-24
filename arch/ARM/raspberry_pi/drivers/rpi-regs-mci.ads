------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

package RPi.Regs.MCI is

   type Host_Config_Register is record
      null;
   end record;

   type MCI_Peripheral is record
      Command     : UInt32;
      Argument    : UInt32;
      Timeout     : UInt32;
      Clk_Div     : UInt32;
      Response_0  : UInt32;
      Response_1  : UInt32;
      Response_2  : UInt32;
      Response_3  : UInt32;
      Status      : UInt32;

      VDD         : UInt32;
      EDM         : UInt32;
      Host_Config : UInt32;

      HBCT        : UInt32;
      Data        : UInt32;
      HBLC        : UInt32;
   end record with Volatile;

   for MCI_Peripheral use record
      Command     at 16#00# range 0 .. 31;
      Argument    at 16#04# range 0 .. 31;
      Timeout     at 16#08# range 0 .. 31;
      Clk_Div     at 16#0C# range 0 .. 31;
      Response_0  at 16#10# range 0 .. 31;
      Response_1  at 16#14# range 0 .. 31;
      Response_2  at 16#18# range 0 .. 31;
      Response_3  at 16#1C# range 0 .. 31;
      Status      at 16#20# range 0 .. 31;

      VDD         at 16#30# range 0 .. 31;
      EDM         at 16#34# range 0 .. 31;
      Host_Config at 16#38# range 0 .. 31;

      HBCT        at 16#3C# range 0 .. 31;
      Data        at 16#40# range 0 .. 31;
      HBLC        at 16#50# range 0 .. 31;
   end record;

end RPi.Regs.MCI;
