package HAL.Touch_Panel is

   type TP_Touch_State is record
      X      : Natural;
      Y      : Natural;
      Weight : Natural;
   end record;

   type TP_State is array (Natural range <>) of TP_Touch_State;

   type Touch_Panel_Device is limited interface;
   type Touch_Panel_Device_Ref is not null access all Touch_Panel_Device'Class;

   function Detect_Touch (Dev : in out Touch_Panel_Device)
                          return Natural is abstract;
   --  Detects the number of touches

   function Get_State (Dev : in out Touch_Panel_Device)
                       return TP_State is abstract;
   --  The current state of the touch panel
end HAL.Touch_Panel;
