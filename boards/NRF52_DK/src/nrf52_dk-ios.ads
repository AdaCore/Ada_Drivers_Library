------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017-2020, AdaCore                      --
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

with nRF.Device; use nRF.Device;
with nRF.GPIO; use nRF.GPIO;

package NRF52_DK.IOs is

   type Pin_Id is range 0 .. 31;

   type IO_Features is (Digital, Analog);

   function Supports (Pin : Pin_Id; Feature : IO_Features) return Boolean is
     (case Feature is
         when Digital => (case Pin is
                             when 0 .. 2 | 5 .. 27 => True,
                             when others           => False),
         when Analog  => (case Pin is
                             when 3 .. 4 | 28 .. 31 => True,
                             when others            => False));

   procedure Set (Pin : Pin_Id; Value : Boolean)
     with Pre => Supports (Pin, Digital);

   function Set (Pin : Pin_Id) return Boolean
     with Pre => Supports (Pin, Digital);

   type Analog_Value is range 0 .. 4095;

   procedure Set_Analog_Period_Us (Period : Natural);
   --  Set the period (in microseconds) of the PWM signal for all analog output
   --  pins.

   procedure Write (Pin : Pin_Id; Value : Analog_Value)
     with Pre => Supports (Pin, Analog);

   function Analog (Pin : Pin_Id) return Analog_Value
     with Pre => Supports (Pin, Analog);
   --  Read the voltagle applied to the pin. 0 means 0V 1023 means 3.3V

private

   --  Mapping between pin id and GPIO_Points

   Points : array (Pin_Id) of GPIO_Point :=
     (0  => P00,
      1  => P01,
      2  => P02,
      3  => P03,
      4  => P04,
      5  => P05,
      6  => P06,
      7  => P07,
      8  => P08,
      9  => P09,
      10 => P10,
      11 => P11,
      12 => P12,
      13 => P13,
      14 => P14,
      15 => P15,
      16 => P16,
      17 => P17,
      18 => P18,
      19 => P19,
      20 => P20,
      21 => P21,
      22 => P22,
      23 => P23,
      24 => P24,
      25 => P25,
      26 => P26,
      27 => P27,
      28 => P28,
      29 => P29,
      30 => P30,
      31 => P31
     );

end NRF52_DK.IOs;
