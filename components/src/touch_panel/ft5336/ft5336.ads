--  Generic driver for the FT5336 touch panel

with Interfaces.Bit_Types; use Interfaces.Bit_Types;
with HAL.I2C;
with HAL.Touch_Panel;

package FT5336 is

   subtype Touch_Identifier is Natural range 0 .. 10;

   type FT5336_Device (Port     : HAL.I2C.I2C_Port_Ref;
                       I2C_Addr : HAL.I2C.I2C_Address) is
     tagged private;

   function Check_Id (This : in out FT5336_Device) return Boolean;
   --  Checks the ID of the touch panel controller, returns false if not found
   --  or invalid.

   procedure TP_Set_Use_Interrupts (This : in out FT5336_Device;
                                    Enabled : Boolean);
   --  Whether the data is retrieved upon interrupt or by polling by the
   --  software.

   function Active_Touch_Points (This : in out FT5336_Device)
                                 return Touch_Identifier;
   --  Retrieve the number of active touch points

   function Get_Touch_Point (This     : in out FT5336_Device;
                             Touch_Id : Touch_Identifier)
                             return HAL.Touch_Panel.TP_Touch_State;
   --  Retrieves the position and pressure information of the specified
   --  touch

private

   type FT5336_Device (Port     : HAL.I2C.I2C_Port_Ref;
                       I2C_Addr : HAL.I2C.I2C_Address) is
     tagged null record;

   function I2C_Read (This   : in out FT5336_Device;
                      Reg    : Byte;
                      Status : out Boolean)
                      return Byte;

   procedure I2C_Write (This   : in out FT5336_Device;
                        Reg    : Byte;
                        Data   : Byte;
                        Status : out Boolean);

end FT5336;
