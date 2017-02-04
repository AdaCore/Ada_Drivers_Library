------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

--  This package provides a simulation of digital signals on an electric wire.
--  It can be used to test algorithms using GPIO_Point like protocol bit
--  banging.

with HAL.GPIO; use HAL.GPIO;

package Wire_Simulation is

   Unknown_State : exception;
   Invalid_Configuration : exception;

   type Virtual_Wire (Default_Pull : GPIO_Pull_Resistor;
                      Max_Points   : Positive) is
     tagged limited private;

   type Any_Virtual_Wire is access all Virtual_Wire'Class;

   function Point (This : in out Virtual_Wire;
                   Id   : Positive)
                   return Any_GPIO_Point
     with Pre => Id <= This.Max_Points;
   --  Return the GPIO_Point coresponding to the Id

private

   type Wire_State is (High, Low, Unknown);

   type Wire_Point is new HAL.GPIO.GPIO_Point with record
      Current_Mode  : GPIO_Mode := Input;
      Current_Pull  : GPIO_Pull_Resistor := Floating;
      Current_State : Boolean := False;
      Wire          : Any_Virtual_Wire := null;
   end record;

   overriding
   function Mode (This : Wire_Point) return GPIO_Mode is (This.Current_Mode);

   overriding
   function Set_Mode (This : in out Wire_Point;
                      Mode : GPIO_Config_Mode) return Boolean;
   --  Return False if the mode is not available

   overriding
   function Pull_Resistor (This : Wire_Point)
                           return GPIO_Pull_Resistor is (This.Current_Pull);

   overriding
   function Set_Pull_Resistor (This : in out Wire_Point;
                               Pull : GPIO_Pull_Resistor)
                               return Boolean;

   overriding
   function Set (This : Wire_Point) return Boolean;

   overriding
   procedure Set (This : in out Wire_Point);

   overriding
   procedure Clear (This : in out Wire_Point);

   overriding
   procedure Toggle (This : in out Wire_Point);


   type Wire_Point_Array is array (Natural range <>) of aliased Wire_Point;

   type Virtual_Wire (Default_Pull : GPIO_Pull_Resistor;
                      Max_Points   : Positive) is
     tagged limited record

      State  : Wire_State := (case Default_Pull is
                                 when Pull_Down => Low,
                                 when Pull_Up   => High,
                                 when Floating  => Unknown);
      Points : aliased Wire_Point_Array (1 .. Max_Points);
   end record;

   procedure Update_Wire_State (This : in out Virtual_Wire);

   function At_Least_One_Output (This : Virtual_Wire) return Boolean;
   --  Return True if at least one GPIO point is configured as output

   function At_Least_One_Pull_Up (This : Virtual_Wire) return Boolean;
   --  Return True if at least one GPIO point is configured as pull up

   function At_Least_One_Pull_Down (This : Virtual_Wire) return Boolean;
   --  Return True if at least one GPIO point is configured as pull down

end Wire_Simulation;
