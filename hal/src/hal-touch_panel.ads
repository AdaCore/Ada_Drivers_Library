package HAL.Touch_Panel is

   type TP_Touch_State is record
      X      : Natural;
      Y      : Natural;
      Weight : Natural;
   end record;

   type TP_State is array (Natural range <>) of TP_Touch_State;

   subtype Touch_Identifier is Natural range 0 .. 10;

   type Touch_Panel_Device is limited interface;
   type Touch_Panel_Device_Ref is not null access all Touch_Panel_Device'Class;

   procedure Set_Bounds (This   : in out Touch_Panel_Device;
                         Width  : Natural;
                         Height : Natural) is abstract;
   --  Set screen bounds. Touch_State must should stay within screen bounds

   function Active_Touch_Points (This : in out Touch_Panel_Device)
                                 return Touch_Identifier is abstract;
   --  Retrieve the number of active touch points

   function Get_Touch_Point (This     : in out Touch_Panel_Device;
                             Touch_Id : Touch_Identifier;
                             SwapXY   : Boolean := False)
                             return TP_Touch_State is abstract;
   --  Retrieves the position and pressure information of the specified
   --  touch

   function Get_All_Touch_Points
     (This     : in out Touch_Panel_Device;
      SwapXY   : Boolean := False)
      return TP_State is abstract;
   --  Retrieves the position and pressure information of every active touch
   --  points

end HAL.Touch_Panel;
