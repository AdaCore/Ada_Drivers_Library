--  Generic driver for the FT5336 touch panel

with HAL;             use HAL;
with HAL.I2C;         use HAL.I2C;
with HAL.Touch_Panel; use HAL.Touch_Panel;

package FT5336 is

   type FT5336_Device (Port     : not null I2C_Port_Ref;
                       I2C_Addr : I2C_Address) is
     limited new Touch_Panel_Device with private;

   function Check_Id (This : in out FT5336_Device) return Boolean;
   --  Checks the ID of the touch panel controller, returns false if not found
   --  or invalid.

   procedure TP_Set_Use_Interrupts (This : in out FT5336_Device;
                                    Enabled : Boolean);
   --  Whether the data is retrieved upon interrupt or by polling by the
   --  software.

   overriding
   procedure Set_Bounds (This   : in out FT5336_Device;
                         Width  : Natural;
                         Height : Natural;
                         Swap   : HAL.Touch_Panel.Swap_State);
   --  Set screen bounds. Touch_State must should stay within screen bounds

   overriding
   function Active_Touch_Points (This : in out FT5336_Device)
                                 return HAL.Touch_Panel.Touch_Identifier;
   --  Retrieve the number of active touch points

   overriding
   function Get_Touch_Point (This     : in out FT5336_Device;
                             Touch_Id : HAL.Touch_Panel.Touch_Identifier)
                             return HAL.Touch_Panel.TP_Touch_State;
   --  Retrieves the position and pressure information of the specified
   --  touch

   overriding
   function Get_All_Touch_Points
     (This     : in out FT5336_Device)
      return HAL.Touch_Panel.TP_State;
   --  Retrieves the position and pressure information of every active touch
   --  points

private

   type FT5336_Device (Port     : not null I2C_Port_Ref;
                       I2C_Addr : I2C_Address) is
     limited new HAL.Touch_Panel.Touch_Panel_Device with record
      LCD_Natural_Width  : Natural := 0;
      LCD_Natural_Height : Natural := 0;
      Swap               : Swap_State := 0;
   end record;

   function I2C_Read (This   : in out FT5336_Device;
                      Reg    : Byte;
                      Status : out Boolean)
                      return Byte;

   procedure I2C_Write (This   : in out FT5336_Device;
                        Reg    : Byte;
                        Data   : Byte;
                        Status : out Boolean);

end FT5336;
