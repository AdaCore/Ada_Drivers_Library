package HAL.Touch_Panel is

   type TP_Touch_State is record
      X      : Natural;
      Y      : Natural;
      Weight : Natural;
   end record;

   type Swap_State is new UInt3;

   Invert_X : constant Swap_State;
   Invert_Y : constant Swap_State;
   Swap_XY  : constant Swap_State;

   subtype Touch_Identifier is Natural range 0 .. 10;
   type TP_State is array (Touch_Identifier range <>) of TP_Touch_State;

   type Touch_Panel_Device is limited interface;
   type Touch_Panel_Ref is access all Touch_Panel_Device'Class;

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
