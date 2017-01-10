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

package HAL.Touch_Panel is

   type TP_Touch_State is record
      X      : Natural;
      Y      : Natural;
      Weight : Natural;
   end record;

   Null_Touch_State : constant TP_Touch_State := (0, 0, 0);

   type Swap_State is new UInt3;

   Invert_X : constant Swap_State;
   Invert_Y : constant Swap_State;
   Swap_XY  : constant Swap_State;

   subtype Touch_Identifier is Natural range 0 .. 10;

   type TP_State is array (Touch_Identifier range <>) of TP_Touch_State;

   type Touch_Panel_Device is limited interface;

   type Any_Touch_Panel is access all Touch_Panel_Device'Class;

   procedure Set_Bounds (This   : in out Touch_Panel_Device;
                         Width  : Natural;
                         Height : Natural;
                         Swap   : Swap_State) is abstract;
   --  Set screen bounds. Touch_State must should stay within screen bounds

   function Active_Touch_Points (This : in out Touch_Panel_Device)
                                 return Touch_Identifier is abstract;
   --  Retrieve the number of active touch points

   function Get_Touch_Point (This     : in out Touch_Panel_Device;
                             Touch_Id : Touch_Identifier)
                             return TP_Touch_State is abstract;
   --  Retrieves the position and pressure information of the specified
   --  touch

   function Get_All_Touch_Points
     (This     : in out Touch_Panel_Device)
      return TP_State is abstract;
   --  Retrieves the position and pressure information of every active touch
   --  points

private

   Invert_X : constant Swap_State := 2#001#;
   Invert_Y : constant Swap_State := 2#010#;
   Swap_XY  : constant Swap_State := 2#100#;

end HAL.Touch_Panel;
