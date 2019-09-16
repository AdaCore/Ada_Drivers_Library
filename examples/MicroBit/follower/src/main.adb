------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with NeoPixel; use NeoPixel;
with HMI; use HMI;

procedure Main is

   type Side is (Left, Right);

   -----------
   -- Input --
   -----------

   --  P15 right sensor
   --  P16 left sensor

   type Optical_Sensor is record
      Pin       : Pin_Id;
      --  Sensor pin (digital input, True when on light background)

      LED_State : Integer;
      LED_Color : LED_Values;
      --  Index and color for LED pixel reflecting sensor state

      State : Boolean;
      --  Current state
   end record;

   Sensors : array (Side) of Optical_Sensor :=
     (Left => (16, 4, Purple, False), Right => (15, 0, Orange, False));

   ------------
   -- Output --
   ------------

   --  P0  NeoPixel strip
   --  P1  right servo
   --  P2  left servo

   type Motor is record
      Pin         : Servo_Pin_Id;
      --  Servo pin (PWM output, see MicroBit.Servos)

      LED_Running : Integer;
      --  Index for LED pixel reflecting motor state:
      --    Green = running at nominal speed
      --    Blue  = running at reduced speed (Slow)
      --    off   = stopped

      Slow : Boolean;
      --  Set True when motor must run at reduced speed
      --  to compensate drift.
   end record;

   Motors : array (Side) of Motor :=
     (Left => (Pin         => 2,
               LED_Running => 3,
               Slow        => False),
      Right => (Pin         => 1,
                LED_Running => 1,
                Slow        => False));

   function Set_Point
     (Motor_Side : Side; Slow : Boolean) return Servo_Set_Point
   --  Compute the servo set point for the given motor
   --  (left motor runs counter-clockwise, right motor runs clockwise).

   is
     (90 + (if Motor_Side = Left then 1 else -1) * (if Slow then 10 else 90));

   Code : Character := ' ';
   --  Character to be displayed on LED array

   Discard : Boolean;

   Motor_Color : LED_Values;
   Run : Boolean := True;

begin
   Put_Line ("Start");
   Discard := MicroBit.Buttons.Subscribe (Button_CB'Access);

   loop
      --  Check sensors

      for Side in Sensors'Range loop
         Sensors (Side).State := Set (Sensors (Side).Pin);
         Set_Color
           (Strip, Sensors (Side).LED_State,
            (if Sensors (Side).State then Sensors (Side).LED_Color else Black));
      end loop;

      --  Determine action based on sensor readings

      --  On track: black strip is centered between sensors, steam ahead!

      Run := True;
      if Sensors (Left).State and Sensors (Right).State then
         Motors (Left).Slow  := False;
         Motors (Right).Slow := False;
         Code := '|';

      --  Left sensor goes dark: we are too far right,
      --  slow left motor to move left.

      elsif Sensors (Right).State then
         Motors (Right).Slow := False;
         Motors (Left).Slow  := True;
         Code := '>';

      --  Right sensor goes dark: we are too far left,
      --  slow right motor to move right.

      elsif Sensors (Left).State then
         Motors (Right).Slow := True;
         Motors (Left).Slow  := False;
         Code := '<';

      --  Both sensors dark: stop

      else
         Code := '-';
         Run := False;
      end if;

      if not Motors_Enable then
         Code := '0';
      end if;

      Clear;
      Display (Code);

      --  Drive servos based on steering decision

      Set_Color (Strip, 2, (if Motors_Enable then Green else Red));

      for M in Motors'Range loop
         if Run then
            Motor_Color := (if Motors (M).Slow then Blue else Green);
         else
            Motor_Color := Red;
         end if;

         if Motors_Enable and Run then
            Set_Color (Strip, Motors (M).LED_Running, Motor_Color);
            Go (Motors (M).Pin, Set_Point (M, Motors (M).Slow));
         else
            Set_Color (Strip, Motors (M).LED_Running, Black);
            Stop (Motors (M).Pin);
         end if;
      end loop;

      Show (Strip, Kitronik_Write'Access);

      Delay_Ms (10);
   end loop;
end Main;
