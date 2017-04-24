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

with HAL.GPIO;

with RPi.Regs.GPIO;

package RPi.GPIO is

   type GPIO_Point is new HAL.GPIO.GPIO_Point with record
      Pin : RPi.Regs.GPIO.GPIO_Pin;
   end record;

   type GPIO_Points is array (Positive range <>) of GPIO_Point;

   type Pin_Mode is
     (Mode_In,
      Mode_Out,
      Mode_AF0,
      Mode_AF1,
      Mode_AF2,
      Mode_AF3,
      Mode_AF4,
      Mode_AF5);

   type Pin_Resistance is
     (Open_Drain,
      Pull_Up,
      Pull_Down);

   type GPIO_Trigger is mod 2 ** 6;
   Trigger_None                      : constant GPIO_Trigger := 2#000000#;
   Trigger_Rising_Edge_Detect        : constant GPIO_Trigger := 2#000001#;
   Trigger_Falling_Edge_Detect       : constant GPIO_Trigger := 2#000010#;
   Trigger_High_Detect               : constant GPIO_Trigger := 2#000100#;
   Trigger_Low_Detect                : constant GPIO_Trigger := 2#001000#;
   Trigger_Async_Rising_Edge_Detect  : constant GPIO_Trigger := 2#010000#;
   Trigger_Async_Falling_Edge_Detect : constant GPIO_Trigger := 2#100000#;

   overriding function Mode (This : GPIO_Point) return HAL.GPIO.GPIO_Mode is
     (HAL.GPIO.GPIO_Mode'First);
   --  The reference manual says that there's no way to retrieve the GPIO mode
   --  once set. It is thus the responsibility of the application to remember
   --  the mode set

   overriding function Set_Mode (This : in out GPIO_Point;
                      Mode : HAL.GPIO.GPIO_Config_Mode) return Boolean is
     (False);
   --  The RPi mode enum is incompatible with the base HAL GPIO mode

   overriding function Pull_Resistor
     (This : GPIO_Point) return HAL.GPIO.GPIO_Pull_Resistor is
     (HAL.GPIO.GPIO_Pull_Resistor'First);
   --  Same as the Mode function above

   overriding function Set_Pull_Resistor
     (This : in out GPIO_Point;
      Pull : HAL.GPIO.GPIO_Pull_Resistor) return Boolean;
   --  Return False if pull is not available for this GPIO point

   procedure Configure
     (This     : GPIO_Point;
      Mode     : Pin_Mode);

   procedure Configure
     (This     : GPIO_Points;
      Mode     : Pin_Mode);

   procedure Configure
     (This     : GPIO_Point;
      Resistor : Pin_Resistance);

   procedure Configure
     (This     : GPIO_Points;
      Resistor : Pin_Resistance);

   procedure Configure
     (This    : GPIO_Point;
      Trigger : GPIO_Trigger);

   overriding function Set (This : GPIO_Point) return Boolean;

   overriding procedure Set (This : in out GPIO_Point);

   overriding procedure Clear (This : in out GPIO_Point);

   overriding procedure Toggle (This : in out GPIO_Point);

   function Event_Detected (This : GPIO_Point) return Boolean;
   --  According to the Triggers configured for this Pin (see the Trigger
   --  Configure procedure), this function will tell if one of the event
   --  has been triggered.

   procedure Clear_Event (This : GPIO_Point);
   --  Clear the Event detected flag

end RPi.GPIO;
