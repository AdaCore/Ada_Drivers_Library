private with SAM_SVD.PIO;
private with System;

with HAL.GPIO;

package SAM.GPIO is

   GPIO_Exception : exception;

   type GPIO_Port is limited private;

   subtype GPIO_Pin is Integer range 0 .. 31;

   type Pin_IO_Modes is (Mode_In, Mode_Out, Mode_AF);

   type Pin_AF_Mode is (Periph_A, Periph_B, Periph_C, Periph_D);

   type Pin_Output_Types is (Push_Pull, Open_Drain);

   type Pin_Input_Filter_Type is (None, Glitch, Debounce);

   type GPIO_Port_Configuration (Mode : Pin_IO_Modes := Mode_In) is record
      Resistors : HAL.GPIO.GPIO_Pull_Resistor := HAL.GPIO.Floating;
      case Mode is
         when Mode_In =>
            Filter_Type : Pin_Input_Filter_Type := None;
            Filter_Time : UInt14 := 5;
            Schmitt_Trigger : Boolean := False;
         when Mode_Out =>
            Output_Type    : Pin_Output_Types := Push_Pull;
         when Mode_AF =>
            AF_Output_Type : Pin_Output_Types := Push_Pull;
            AF             : Pin_AF_Mode;
      end case;
   end record;

   type GPIO_Point is new HAL.GPIO.GPIO_Point with record
      Periph : access GPIO_Port;
      Pin    : GPIO_Pin;
   end record;

   type GPIO_Group is
     (A, B, C, D, E);

   function Group (Port : GPIO_Port) return GPIO_Group;

   function Group (GPIO : GPIO_Point) return GPIO_Group is
      (Group (Port => GPIO.Periph.all));

   function Group_ID (Group : GPIO_Group) return Natural is
     (case Group is
         when A => PIOA_ID,
         when B => PIOB_ID,
         when C => PIOC_ID,
         when D => PIOD_ID,
         when E => PIOE_ID);

   function Group_ID (GPIO : GPIO_Point) return Natural is
     (Group_ID (Group => Group (GPIO => GPIO)));

   function Group_ID (Port : GPIO_Port) return Natural is
     (Group_ID (Group => Group (Port => Port)));

   overriding
   function Mode (This : GPIO_Point) return HAL.GPIO.GPIO_Mode;

   overriding
   function Set_Mode (This : in out GPIO_Point;
                      Mode : HAL.GPIO.GPIO_Config_Mode) return Boolean;

   overriding
   function Pull_Resistor (This : GPIO_Point)
                              return HAL.GPIO.GPIO_Pull_Resistor;

   overriding
   function Set_Pull_Resistor (This : in out GPIO_Point;
                               Pull : HAL.GPIO.GPIO_Pull_Resistor)
                                  return Boolean;

   overriding
   function Set (This : GPIO_Point) return Boolean with
     Pre => Pin_IO_Mode (This) /= Mode_AF,
     Inline;
   --  Returns True if the bit specified by This.Pin is set (not zero) in the
   --  input data register of This.Port.all; returns False otherwise.

   overriding
   procedure Set (This : in out GPIO_Point) with
     Inline;
   --  For This.Port.all, sets the output data register bit specified by
   --  This.Pin to one. Other pins are unaffected.

   overriding
   procedure Clear (This : in out GPIO_Point) with
     Inline;
   --  For This.Port.all, sets the output data register bit specified by
   --  This.Pin to zero. Other pins are unaffected.

   overriding
   procedure Toggle (This : in out GPIO_Point) with
     Inline;
   --  For This.Port.all, negates the output data register bit specified by
   --  This.Pin (one becomes zero and vice versa). Other pins are unaffected.

   procedure Drive (This : in out GPIO_Point; Condition : Boolean) with
      Post => (This.Set = Condition),
      Inline;
   --  Drives This high or low (set or clear) based on the value of Condition

   function Locked (This : GPIO_Point) return Boolean
     with Inline;

   procedure Configure_IO
     (This   : in out GPIO_Point;
      Config : GPIO_Port_Configuration);
   --  For Point.Pin on the Point.Port.all, configures the
   --  characteristics specified by Config

   function Pin_IO_Mode (This : GPIO_Point) return Pin_IO_Modes with Inline;

   procedure Enable_Clock (Port : GPIO_Port);
   procedure Enable_Clock (Point : GPIO_Point);
   procedure Disable_Clock (Port : GPIO_Port);
   procedure Disable_Clock (Point : GPIO_Point);

   type Interrupt_Trigger_Type is
     (Any_Edge, Rising_Edge, Falling_Edge, Low, High);

   procedure Enable_Interrupt (This : GPIO_Point;
                               Trigger : Interrupt_Trigger_Type);
   procedure Disable_Interrupt (This : GPIO_Point);

   function Read_Interrupt_Status (This : GPIO_Point) return Boolean;
private

   type GPIO_Port is new SAM_SVD.PIO.PIO_Peripheral;

   use System;

   Unknown_GPIO_Port : exception;

   function Group (Port : GPIO_Port) return GPIO_Group is
     (if Port'Address = SAM_SVD.PIOA_Base then
         A
      elsif Port'Address = SAM_SVD.PIOB_Base then
         B
      elsif Port'Address = SAM_SVD.PIOC_Base then
         C
      elsif Port'Address = SAM_SVD.PIOD_Base then
         D
      elsif Port'Address = SAM_SVD.PIOE_Base then
         E
      else
         raise Unknown_GPIO_Port);

end SAM.GPIO;
