--  Generic driver for the FT5336 touch panel

with Interfaces; use Interfaces;

generic

   with function IO_Read
     (Reg    : Unsigned_8;
      Status : out Boolean) return Unsigned_8;
   --  Reads a Touch Panel register value

   with procedure IO_Write
     (Reg    : Unsigned_8;
      Data   : Unsigned_8;
      Status : out Boolean);

package FT5336 is

   type Touch_Point is record
      X      : Unsigned_16;
      Y      : Unsigned_16;
      Weight : Unsigned_8;
   end record;

   subtype Touch_Identifier is Natural range 0 .. 10;

   function Check_Id return Boolean;
   --  Checks the ID of the touch panel controller, returns false if not found
   --  or invalid.

   procedure TP_Set_Use_Interrupts (Enabled : Boolean);
   --  Whether the data is retrieved upon interrupt or by polling by the
   --  software.

   function Active_Touch_Points return Touch_Identifier;
   --  Retrieve the number of active touch points

   function Get_Touch_Point (Touch_Id : Touch_Identifier) return Touch_Point;

end FT5336;
