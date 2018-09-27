------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

package body MicroBit.Servos is

   Servo_0 : constant := 1023 * 500 / 20_000;

   function To_PWM (SP : Servo_Set_Point) return Analog_Value is
      (Servo_0 + Analog_Value (1023 * Integer (SP) / 1800));

   Analog_Period_Set : Boolean := False;

   ----------
   -- Stop --
   ----------

   procedure Stop (Pin : Servo_Pin_Id) is
   begin
      if Supports (Pin, Digital) then
         Set (Pin, False);
      else
         Write (Pin, 0);
      end if;
   end Stop;

   --------
   -- Go --
   --------

   procedure Go (Pin : Servo_Pin_Id; Setpoint : Servo_Set_Point) is
   begin
      if not Analog_Period_Set then
         --  Set PWM period to 20_000 µs (50 Hz)

         Set_Analog_Period_Us (20_000);
         Analog_Period_Set := True;
      end if;

      Write (Pin, To_PWM (Setpoint));
   end Go;

end MicroBit.Servos;
