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

with FE310_SVD.RTC;

package FE310.RTC is

   subtype Count_Value is UInt48;

   function Count return Count_Value;

   procedure Set_Count (Value : Count_Value);

   subtype Scaled_Value is UInt32;

   function Scaled_Counter return Scaled_Value;

   -- Enable --

   procedure Enable;

   procedure Disable;

   -- Configuration --

   procedure Set_Scale (Scale : FE310_SVD.RTC.CONFIG_SCALE_Field);

   -- Compare Value --

   subtype Compare_Value is UInt32;

   procedure Set_Compare (Value : Compare_Value);

   function Compare return Compare_Value;

   -- Interrupts --

   function Interrupt_Pending return Boolean;

end FE310.RTC;
