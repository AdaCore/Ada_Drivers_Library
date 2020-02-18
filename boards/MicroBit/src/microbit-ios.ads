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

with nRF.GPIO; use nRF.GPIO;

package MicroBit.IOs is

   type Pin_Id is range 0 .. 20;

   type IO_Features is (Digital, Analog, Touch);

   function Supports (Pin : Pin_Id; Feature : IO_Features) return Boolean is
     (case Feature is
         when Digital => (case Pin is
                             when 0 .. 16 | 19 .. 20 => True,
                             when others             => False),
         when Analog  => (case Pin is
                             when 0 .. 4 | 10 => True,
                             when others                 => False),
         when Touch   => (case Pin is
                             when 0 | 1 | 2 => True,
                             when others    => False));

   procedure Set (Pin : Pin_Id; Value : Boolean)
     with Pre => Supports (Pin, Digital);

   function Set (Pin : Pin_Id) return Boolean
     with Pre => Supports (Pin, Digital);

   type Analog_Value is range 0 .. 1023;

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
     (0  => MB_P0,
      1  => MB_P1,
      2  => MB_P2,
      3  => MB_P3,
      4  => MB_P4,
      5  => MB_P5,
      6  => MB_P6,
      7  => MB_P7,
      8  => MB_P8,
      9  => MB_P9,
      10 => MB_P10,
      11 => MB_P11,
      12 => MB_P12,
      13 => MB_P13,
      14 => MB_P14,
      15 => MB_P15,
      16 => MB_P16,
      17 => MB_P0,  --  There's no pin17, using P0 to fill in...
      18 => MB_P0,  --  There's no pin18, using P0 to fill in...
      19 => MB_P19,
      20 => MB_P20);

end MicroBit.IOs;
