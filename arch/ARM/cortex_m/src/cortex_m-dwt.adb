------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017, AdaCore                           --
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

with Cortex_M_SVD.Debug;
with Cortex_M_SVD.DWT;
with Ada.Unchecked_Conversion;

package body Cortex_M.DWT is

   ---------------------
   -- Enable_DWT_Unit --
   ---------------------

   procedure Enable_DWT_Unit is
      use Cortex_M_SVD.Debug;
   begin
      Debug_Periph.DEMCR.TRCENA := False;
      Debug_Periph.DEMCR.TRCENA := True;
   end Enable_DWT_Unit;

   ----------------------
   -- Disable_DWT_Unit --
   ----------------------

   procedure Disable_DWT_Unit is
      use Cortex_M_SVD.Debug;
   begin
      Debug_Periph.DEMCR.TRCENA := False;
   end Disable_DWT_Unit;

   ----------------------
   -- DWT_Unit_Enabled --
   ----------------------

   function DWT_Unit_Enabled return Boolean is
      use Cortex_M_SVD.Debug;
   begin
      return Debug_Periph.DEMCR.TRCENA;
   end DWT_Unit_Enabled;

   ---------------------
   -- DWT_Reset_Value --
   ---------------------

   function DWT_Reset_Value return UInt32 is
      use Cortex_M_SVD.DWT;

      function As_UInt32 is new Ada.Unchecked_Conversion
        (Source => CTRL_Register, Target => UInt32);

   begin
      return As_UInt32 (DWT_Periph.CTRL);
   end DWT_Reset_Value;

end Cortex_M.DWT;
