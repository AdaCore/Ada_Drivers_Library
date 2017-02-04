------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

package HAL.GPIO is

   type GPIO_Mode is (Unknown, Input, Output);
   --  Possible modes for a GPIO point. Unknown means that the point is
   --  configured in a mode that is not described in this interface. For
   --  instance alternate function mode on an STM32 micro-controller.

   subtype GPIO_Config_Mode is GPIO_Mode range Input .. Output;
   --  Modes a GPIO point can be configured in

   type GPIO_Pull_Resistor is (Floating, Pull_Up, Pull_Down);

   type GPIO_Point is limited interface;

   type Any_GPIO_Point is access all GPIO_Point'Class;

   function Mode (This : GPIO_Point) return GPIO_Mode is abstract;

   function Set_Mode (This : in out GPIO_Point;
                      Mode : GPIO_Config_Mode) return Boolean is abstract;
   --  Return False if the mode is not available

   function Pull_Resistor (This : GPIO_Point)
                           return GPIO_Pull_Resistor is abstract;

   function Set_Pull_Resistor (This : in out GPIO_Point;
                               Pull : GPIO_Pull_Resistor)
                               return Boolean is abstract;
   --  Return False if pull is not available for this GPIO point

   function Set (This : GPIO_Point) return Boolean is abstract;
   --  Read actual state of the GPIO_Point.
   --
   --  So far all the GPIO supported by this library have the ability to read
   --  the state even when they are configured as output.


   --  For the output control procedures below, depending on configuration
   --  and/or other devices conected to the IO line, these procedures may have
   --  no actual effect on the line. For example trying to set the IO when
   --  another device is pulling the line to low.

   procedure Set (This : in out GPIO_Point) is abstract
     with Pre'Class => This.Mode = Output;

   procedure Clear (This : in out GPIO_Point) is abstract
     with Pre'Class => This.Mode = Output;

   procedure Toggle (This : in out GPIO_Point) is abstract
     with Pre'Class => This.Mode = Output;

end HAL.GPIO;
