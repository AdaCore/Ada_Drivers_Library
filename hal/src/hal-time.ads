package HAL.Time is

   type Delays is limited interface;

   type Delays_Ref is access all Delays'Class;

   procedure Delay_Microseconds (This : in out Delays;
                                 Us : Integer) is abstract;

   procedure Delay_Milliseconds (This : in out Delays;
                                 Ms : Integer) is abstract;

   procedure Delay_Seconds      (This : in out Delays;
                                 S  : Integer) is abstract;
end HAL.Time;
