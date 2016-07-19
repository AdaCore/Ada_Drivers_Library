with Interfaces; use Interfaces;

package body MCP23x08 is

   procedure Loc_IO_Write
     (This      : in out MCP23x08_Device;
      WriteAddr : Register_Address;
      Value     : Byte)
     with Inline_Always;

   procedure Loc_IO_Read
     (This     : MCP23x08_Device;
      ReadAddr : Register_Address;
      Value    : out Byte)
     with Inline_Always;

   procedure Set_Bit
     (This     : in out MCP23x08_Device;
      RegAddr  : Register_Address;
      Pin      : MCP23x08_Pin);

   procedure Clear_Bit
     (This     : in out MCP23x08_Device;
      RegAddr  : Register_Address;
      Pin      : MCP23x08_Pin);

   ------------------
   -- Loc_IO_Write --
   ------------------

   procedure Loc_IO_Write
     (This      : in out MCP23x08_Device;
      WriteAddr : Register_Address;
      Value     : Byte)
   is

   begin
      IO_Write (MCP23x08_Device'Class (This),
                WriteAddr,
                Value);
   end Loc_IO_Write;

   -----------------
   -- Loc_IO_Read --
   -----------------

   procedure Loc_IO_Read
     (This     : MCP23x08_Device;
      ReadAddr : Register_Address;
      Value    : out Byte)
      is
   begin
      IO_Read (MCP23x08_Device'Class (This),
               ReadAddr,
               Value);
   end Loc_IO_Read;

   -------------
   -- Set_Bit --
   -------------

   procedure Set_Bit
     (This     : in out MCP23x08_Device;
      RegAddr  : Register_Address;
      Pin      : MCP23x08_Pin)
   is
      Prev, Next : Byte;
   begin
      Loc_IO_Read (This, RegAddr, Prev);
      Next := Prev or Pin'Enum_Rep;
      if Next /= Prev then
         Loc_IO_Write (This, RegAddr, Next);
      end if;
   end Set_Bit;

   ---------------
   -- Clear_Bit --
   ---------------

   procedure Clear_Bit
     (This     : in out MCP23x08_Device;
      RegAddr : Register_Address;
      Pin      : MCP23x08_Pin)
   is
      Prev, Next : Byte;
   begin
      Loc_IO_Read (This, RegAddr, Prev);
      Next := Prev and (not  Pin'Enum_Rep);
      if Next /= Prev then
         Loc_IO_Write (This, RegAddr, Next);
      end if;
   end Clear_Bit;

   ---------------
   -- Configure --
   ---------------

   procedure Configure (This    : in out MCP23x08_Device;
                        Pin     : MCP23x08_Pin;
                        Output  : Boolean;
                        Pull_Up : Boolean)
   is
   begin
      if Output then
         Clear_Bit (This, IO_DIRECTION_REG, Pin);
      else
         Set_Bit (This, IO_DIRECTION_REG, Pin);
      end if;

      if Pull_Up then
         Set_Bit (This, PULL_UP_REG, Pin);
      else
         Clear_Bit (This, PULL_UP_REG, Pin);
      end if;
   end Configure;

   ---------
   -- Set --
   ---------

   function Set (This  : MCP23x08_Device;
                 Pin   : MCP23x08_Pin) return Boolean
   is
      Val : Byte;
   begin
      Loc_IO_Read (This, LOGIC_LEVLEL_REG, Val);
      return (Pin'Enum_Rep and Val) /= 0;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (This  : in out MCP23x08_Device;
                  Pin   : MCP23x08_Pin)
   is
   begin
      Set_Bit (This, LOGIC_LEVLEL_REG, Pin);
   end Set;

   -----------
   -- Clear --
   -----------

   procedure Clear (This  : in out MCP23x08_Device;
                    Pin   : MCP23x08_Pin)
   is
   begin
      Clear_Bit (This, LOGIC_LEVLEL_REG, Pin);
   end Clear;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (This  : in out MCP23x08_Device;
                     Pin   : MCP23x08_Pin)
   is
   begin
      if This.Set (Pin) then
         This.Clear (Pin);
      else
         This.Set (Pin);
      end if;
   end Toggle;

   --------------------
   -- Get_GPIO_Point --
   --------------------

   function Get_GPIO_Point (This : in out MCP23x08_Device;
                            Pin  : MCP23x08_Pin)
                            return not null HAL.GPIO.GPIO_Point_Ref
   is
   begin
      This.Points (Pin) := (Device => This'Unchecked_Access,
                            Pin    => Pin);
      return This.Points (Pin)'Unchecked_Access;
   end Get_GPIO_Point;

   ---------
   -- Set --
   ---------

   overriding
   function Set (Point : MCP23_GPIO_Point) return Boolean is
   begin
      return Point.Device.Set (Point.Pin);
   end Set;

   ---------
   -- Set --
   ---------

   overriding
   procedure Set (Point : in out MCP23_GPIO_Point) is
   begin
      Point.Device.Set (Point.Pin);
   end Set;

   -----------
   -- Clear --
   -----------

   overriding
   procedure Clear (Point : in out MCP23_GPIO_Point) is
   begin
      Point.Device.Clear (Point.Pin);
   end Clear;

   ------------
   -- Toggle --
   ------------

   overriding
   procedure Toggle (Point : in out MCP23_GPIO_Point) is
   begin
      Point.Device.Toggle (Point.Pin);
   end Toggle;

end MCP23x08;
