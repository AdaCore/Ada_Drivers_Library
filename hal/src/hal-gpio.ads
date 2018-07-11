------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2018, AdaCore                     --
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

   type Capability is (Unknown_Mode, Input, Output,   -- Mode
                       Floating, Pull_Up, Pull_Down); -- Resistor

   subtype GPIO_Mode is Capability range Unknown_Mode .. Output;
   --  Possible modes for a GPIO point. Unknown_Mode means that the point
   --  is configured in a mode that is not described in this interface. For
   --  instance alternate function mode on an STM32 micro-controller.

   subtype GPIO_Config_Mode is GPIO_Mode range Input .. Output;
   --  Modes a GPIO point can be configured in

   subtype GPIO_Pull_Resistor is Capability range Floating .. Pull_Down;

   type GPIO_Point is limited interface;

   type Any_GPIO_Point is access all GPIO_Point'Class;

   function Support (This : GPIO_Point;
                     Capa : Capability)
                     return Boolean
                     is abstract;
   --  Return True if the GPIO_Point supports the given capability

   function Mode (This : GPIO_Point) return GPIO_Mode is abstract;
   --  Return the current mode of the GPIO_Point

   procedure Set_Mode (This : in out GPIO_Point;
                       Mode : GPIO_Config_Mode)
   is abstract
     with Pre'Class => This.Support (Mode);
   --  Set the mode of the GPIO_Point, iff the mode is supported

   function Pull_Resistor (This : GPIO_Point)
                           return GPIO_Pull_Resistor is abstract;
   --  Return the current pull resistor mode

   procedure Set_Pull_Resistor (This : in out GPIO_Point;
                                Pull : GPIO_Pull_Resistor)
   is abstract
     with Pre'Class => This.Support (Pull);
   --  Set the pull resistor of the GPIO_Point, iff the pull mode is supported

   function Set (This : GPIO_Point) return Boolean is abstract;
   --  Read actual state of the GPIO_Point.
   --
   --  So far all the GPIO supported by this library have the ability to read
   --  the state even when they are configured as output.


   --  For the output control procedures below, depending on configuration
   --  and/or other devices conected to the IO line, these procedures may have
   --  no actual effect on the line. For example trying to set the IO when
   --  another device is pulling the line to low.

   procedure Set (This : in out GPIO_Point) is abstract;

   procedure Clear (This : in out GPIO_Point) is abstract;

   procedure Toggle (This : in out GPIO_Point) is abstract
     with Pre'Class => This.Mode = Output;

end HAL.GPIO;
