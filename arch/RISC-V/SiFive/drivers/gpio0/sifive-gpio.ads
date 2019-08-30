------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

with System.Storage_Elements;
with HAL.GPIO;

package SiFive.GPIO is

   type GPIO_Pin_Index is range 0 .. 31;
   --  Indentifier of

   type GPIO_Pin_Number is range 1 .. 31;
   --  Number of GPIO pins available on a controller

   type GPIO_Controller
     (Base_Address : System.Storage_Elements.Integer_Address)
   is private;

   type Any_GPIO_Controller is access all GPIO_Controller;

   type GPIO_Point (Controller : not null Any_GPIO_Controller;
                    Pin        : GPIO_Pin_Index)
   is new HAL.GPIO.GPIO_Point
     with
     private;

   procedure Invert (This    : in out GPIO_Point;
                     Enabled : Boolean := True);
   --  Invert the output level

   function Inverted (This : GPIO_Point) return Boolean;

   ---------------
   --  HAL.GPIO --
   ---------------

   overriding
   function Support (This : GPIO_Point;
                     Capa : HAL.GPIO.Capability)
                     return Boolean
   is (case Capa is
          when HAL.GPIO.Pull_Down => False,
          when others             => True);

   overriding
   function Mode (This : GPIO_Point) return HAL.GPIO.GPIO_Mode;

   overriding
   procedure Set_Mode (This : in out GPIO_Point;
                       Mode : HAL.GPIO.GPIO_Config_Mode);

   overriding
   function Pull_Resistor (This : GPIO_Point)
                           return HAL.GPIO.GPIO_Pull_Resistor;

   overriding
   procedure Set_Pull_Resistor (This : in out GPIO_Point;
                                Pull : HAL.GPIO.GPIO_Pull_Resistor);

   overriding
   function Set (This : GPIO_Point) return Boolean;

   overriding
   procedure Set (This : in out GPIO_Point);

   overriding
   procedure Clear (This : in out GPIO_Point);

   overriding
   procedure Toggle (This : in out GPIO_Point);

private

   type GPIO_Controller
     (Base_Address : System.Storage_Elements.Integer_Address)
   is null record;

   type GPIO_Point (Controller : not null Any_GPIO_Controller;
                    Pin        : GPIO_Pin_Index)
   is new HAL.GPIO.GPIO_Point
     with null record;

end SiFive.GPIO;
