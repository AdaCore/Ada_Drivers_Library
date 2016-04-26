package HAL.Touch_Panel is

   type TP_Touch_State is record
      X      : Natural;
      Y      : Natural;
      Weight : Natural;
   end record;

   subtype Touch_Identifier is Natural range 0 .. 10;
   type TP_State is array (Touch_Identifier range <>) of TP_Touch_State;

   type Touch_Panel_Device is limited interface;

   function Initialize (This : in out Touch_Panel_Device) return Boolean
     is abstract;

   procedure Initialize (This : in out Touch_Panel_Device) is abstract;

   procedure Set_Bounds (This   : in out Touch_Panel_Device;
                         Width  : Natural;
                         Height : Natural) is abstract;
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

   function Swap_Points (This : Touch_Panel_Device) return Boolean is abstract;

end HAL.Touch_Panel;
