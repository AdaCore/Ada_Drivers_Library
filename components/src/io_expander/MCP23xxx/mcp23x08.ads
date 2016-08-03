with HAL; use HAL;
with HAL.GPIO;

package MCP23x08 is

   type MCP23x08_Pin is (Pin_0, Pin_1, Pin_2, Pin_3,
                         Pin_4, Pin_5, Pin_6, Pin_7);

   type MCP23x08_Device is abstract tagged private;

   procedure Configure (This    : in out MCP23x08_Device;
                        Pin     : MCP23x08_Pin;
                        Output  : Boolean;
                        Pull_Up : Boolean);

   function Set (This  : MCP23x08_Device;
                 Pin   : MCP23x08_Pin) return Boolean;

   procedure Set (This  : in out MCP23x08_Device;
                  Pin   : MCP23x08_Pin);

   procedure Clear (This  : in out MCP23x08_Device;
                    Pin   : MCP23x08_Pin);

   procedure Toggle (This  : in out MCP23x08_Device;
                     Pin   : MCP23x08_Pin);


   type ALl_IO_Array is array (MCP23x08_Pin) of Boolean
     with Pack, Size => 8;

   function Get_All_IO (This : in out MCP23x08_Device) return ALl_IO_Array;

   procedure Set_All_IO (This : in out MCP23x08_Device; IOs : ALl_IO_Array);

   function Get_GPIO_Point (This : in out MCP23x08_Device;
                            Pin  : MCP23x08_Pin)
                            return not null HAL.GPIO.GPIO_Point_Ref;

   -----------------------------
   -- IO abstract subprograms --
   -----------------------------

   type Register_Address is new Byte;

   procedure IO_Write
     (This      : in out MCP23x08_Device;
      WriteAddr : Register_Address;
      Value     : Byte) is abstract;

   procedure IO_Read
     (This     : MCP23x08_Device;
      ReadAddr : Register_Address;
      Value    : out Byte) is abstract;

private

   for MCP23x08_Pin use
     (Pin_0  => 16#0001#,
      Pin_1  => 16#0002#,
      Pin_2  => 16#0004#,
      Pin_3  => 16#0008#,
      Pin_4  => 16#0010#,
      Pin_5  => 16#0020#,
      Pin_6  => 16#0040#,
      Pin_7  => 16#0080#);

   type MCP23_GPIO_Point is new HAL.GPIO.GPIO_Point with record
      Device : access MCP23x08_Device;
      Pin    : MCP23x08_Pin;
   end record;
   --  Internal implementation of HAL.GPIO interface

   type MCP23_GPIO_Point_Array is array (MCP23x08_Pin) of
     aliased MCP23_GPIO_Point;

   type MCP23x08_Device is abstract tagged record
      Points : MCP23_GPIO_Point_Array;
   end record;

   overriding
   function Set (Point : MCP23_GPIO_Point) return Boolean;

   overriding
   procedure Set (Point : MCP23_GPIO_Point);

   overriding
   procedure Clear (Point : MCP23_GPIO_Point);

   overriding
   procedure Toggle (Point : MCP23_GPIO_Point);

   IO_DIRECTION_REG     : constant Register_Address := 16#00#;
   INPUT_POLARITY_REG   : constant Register_Address := 16#01#;
   INTERRUPT_ENABLE_REG : constant Register_Address := 16#02#;
   DEFAULT_VALUE_REG    : constant Register_Address := 16#03#;
   INTERRUPT_CTRL_REG   : constant Register_Address := 16#04#;
   CONFIGURATION_REG    : constant Register_Address := 16#05#;
   PULL_UP_REG          : constant Register_Address := 16#06#;
   INTERRUPT_FLAG_REG   : constant Register_Address := 16#07#;
   INTERRUPT_CAPT_REG   : constant Register_Address := 16#08#;
   LOGIC_LEVLEL_REG     : constant Register_Address := 16#09#;
   OUTPUT_LATCH_REG     : constant Register_Address := 16#0A#;
end MCP23x08;
