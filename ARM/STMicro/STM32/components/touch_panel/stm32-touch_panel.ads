package STM32.Touch_Panel is

   type TP_Touch_State is record
      X      : Natural;
      Y      : Natural;
      Weight : Natural;
   end record;

   type TP_State is array (Natural range <>) of TP_Touch_State;

   function Initialize return Boolean;
   --  Initializes the LCD touch panel

   procedure Initialize;
   --  Initializes the LCD touch panel

   function Detect_Touch return Natural;
   --  Detects the number of touches

   function Get_State return TP_State;
   --  The current state of the touch panel

end STM32.Touch_Panel;
