package HAL.GPIO is

   type GPIO_Point is limited interface;

   type GPIO_Point_Ref is access all GPIO_Point'Class;

   function Set (This : GPIO_Point) return Boolean is abstract;

   procedure Set (This : in out GPIO_Point) is abstract;

   procedure Clear (This : in out GPIO_Point) is abstract;

   procedure Toggle (This : in out GPIO_Point) is abstract;

end HAL.GPIO;
