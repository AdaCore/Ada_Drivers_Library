------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
--                       Copyright (C) 2020, Monadnock Systems Ltd.         --
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

package nRF.ADC is

   type Bits_Resolution is (Res_8bit, Res_10bit, Res_12bit);

   type Analog_Pin is range 0 .. 7;

   type Pin_Input_Selection is (Pin_Quadruple, Pin_Double, Pin_Full, Pin_Half,
                                Pin_One_Third, Pin_One_Forth, Pin_One_Fifth,
                                Pin_One_Sixth);
   type VDD_Input_Selection is (VDD_One_Fifth, VDD_One_Sixth);

   type Reference_Selection is (Internal_0V6, VDD_One_Forth);

   function Do_Pin_Conversion
     (Pin   : Analog_Pin;
      Input : Pin_Input_Selection;
      Ref   : Reference_Selection;
      Res   : Bits_Resolution) return UInt16;

   function Do_VDD_Conversion
     (Input : VDD_Input_Selection;
      Ref   : Reference_Selection;
      Res   : Bits_Resolution) return UInt16;

   function Busy return Boolean;

end nRF.ADC;
