------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

package nRF51.GPIO is

   subtype GPIO_Pin_Index is Natural range 0 .. 31;

   type Pin_IO_Modes is (Mode_In, Mode_Out);

   type Pin_Resistors is (Pull_Up, Pull_Down, No_Pull);

   type Pin_Drive is (Drive_S0S1,
                      Drive_H0S1,
                      Drive_S0H1,
                      Drive_H0H1,
                      Drive_D0S1,
                      Drive_D0H1,
                      Drive_S0D1,
                      Drive_H0D1);

   type Pin_Sense_Mode is (Sense_Disabled,
                           Sense_For_High_Level,
                           Sense_For_Low_Level);

   type Pin_Input_Buffer_Mode is (Input_Buffer_Connect,
                                  Input_Buffer_Disconnect);

   type GPIO_Configuration is record
      Mode         : Pin_IO_Modes;
      Resistors    : Pin_Resistors;
      Input_Buffer : Pin_Input_Buffer_Mode := Input_Buffer_Disconnect;
      Drive        : Pin_Drive             := Drive_S0S1;
      Sense        : Pin_Sense_Mode        := Sense_Disabled;
   end record;

   type GPIO_Point is new HAL.GPIO.GPIO_Point with record
      Pin : GPIO_Pin_Index;
   end record;

   overriding
   function Mode (This : GPIO_Point) return HAL.GPIO.GPIO_Mode;

   overriding
   function Set_Mode (This : in out GPIO_Point;
                      Mode : HAL.GPIO.GPIO_Config_Mode) return Boolean;

   overriding
   function Pull_Resistor (This : GPIO_Point)
                           return HAL.GPIO.GPIO_Pull_Resistor;

   overriding
   function Set_Pull_Resistor (This : in out GPIO_Point;
                               Pull : HAL.GPIO.GPIO_Pull_Resistor)
                               return Boolean;

   overriding
   function Set (This : GPIO_Point) return Boolean with
     Inline;
   --  Returns True if the bit specified by This.Pin is set (not zero) in the
   --  input data register of This.Port.all; returns False otherwise.

   overriding
   procedure Set (This : in out GPIO_Point) with
     Inline;
   --  For This.Port.all, sets the output data register bit specified by
   --  This.Pin to one. Other pins are unaffected.

   overriding
   procedure Clear (This : in out GPIO_Point) with
     Inline;
   --  For This.Port.all, sets the output data register bit specified by
   --  This.Pin to zero. Other pins are unaffected.

   overriding
   procedure Toggle (This : in out GPIO_Point) with
     Inline;
   --  For This.Port.all, negates the output data register bit specified by
   --  This.Pin (one becomes zero and vice versa). Other pins are unaffected.

   procedure Configure_IO
     (This   : GPIO_Point;
      Config : GPIO_Configuration);
   --  Configures the characteristics specified by Config

end nRF51.GPIO;
