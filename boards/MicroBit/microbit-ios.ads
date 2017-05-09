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

   procedure Write (Pin : Pin_Id; Value : Analog_Value)
     with Pre => Supports (Pin, Analog);

   function Analog (Pin : Pin_Id) return Analog_Value
     with Pre => Supports (Pin, Analog);

   function Touched (Pin : Pin_Id) return Boolean
     with Pre => Supports (Pin, Touch);

end MicroBit.IOs;
