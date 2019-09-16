------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2018-2019, AdaCore                      --
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

with MicroBit.Console; use MicroBit.Console;
with MicroBit.Display; use MicroBit.Display;
with MicroBit.IOs; use MicroBit.IOs;
with MicroBit.Servos; use MicroBit.Servos;
with MicroBit.Buttons; use MicroBit.Buttons;
with MicroBit.Time; use MicroBit.Time;

procedure Main is

   subtype Servo_Pin_Id is Pin_Id range 1 .. 2;
   type Servo_Pin_State (Active : Boolean := False) is record
      case Active is
         when True => Setpoint : Servo_Set_Point;
         when False => null;
      end case;
   end record;
   type Servo_Pin_Array is array (Servo_Pin_Id) of Servo_Pin_State;

   Servo_Pins, Cur_Servo_Pins : Servo_Pin_Array := (others => (Active => False));
   Code : Character := ' ';
   Button_AB : Boolean;
   Starting : Boolean := False;
begin
   Put_Line ("Start");

   loop
      --  Update PWM pulse size

      if Starting or else Cur_Servo_Pins /= Servo_Pins then
         Starting := False;
         Clear;
         Display (Code);
         for J in Servo_Pins'Range loop
            if Servo_Pins (J).Active then
               Go (J, Servo_Pins (J).Setpoint);
            else
               Stop (J);
            end if;
         end loop;
         Cur_Servo_Pins := Servo_Pins;
      end if;

      --  Check buttons

      if State (Button_A) = Released and then State (Button_B) = Released then
         --  Reset double press latch

         Button_AB := False;

      elsif State (Button_A) = Pressed and then State (Button_B) = Pressed then
         Servo_Pins := (others => (Active => False));
         Code := '0';

         --  Latch double press mode so that when one button is released, we
         --  ignore the other.

         Button_AB := True;

      elsif Button_AB then
         --  After double press, ignore single button state until both buttons
         --  are released.

         null;

      elsif State (Button_A) = Pressed then
         Servo_Pins := (1 => (Active => True, Setpoint => 0),
                        2 => (Active => True, Setpoint => 180));
         Code := 'A';

      elsif State (Button_B) = Pressed then
         Servo_Pins := (1 => (Active => True, Setpoint => 180),
                        2 => (Active => True, Setpoint => 0));
         Code := 'R';
      end if;

      --  Delay for at least 1 PWM frame

      Delay_Ms (20);
   end loop;
end Main;
