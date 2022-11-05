------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2022, AdaCore                           --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

--  The MCP23x17 are similar to the MCP23x08.  They have two ports
--  (banks) of 8 pins (GPA0..GPA7, GPB0..GPB7) instead of just one.


with HAL; use HAL;
with HAL.GPIO;

package MCP23x17 is

   type MCP23x17_Pin_Number is range 0 .. 7;

   type MCP23x17_Port is (A, B);

   type MCP23x17_Pin is record
      Port   : MCP23x17_Port;
      Pin_Nr : MCP23x17_Pin_Number;
   end record;

   A0 : constant MCP23x17_Pin := (A, 0);
   A1 : constant MCP23x17_Pin := (A, 1);
   A2 : constant MCP23x17_Pin := (A, 2);
   A3 : constant MCP23x17_Pin := (A, 3);
   A4 : constant MCP23x17_Pin := (A, 4);
   A5 : constant MCP23x17_Pin := (A, 5);
   A6 : constant MCP23x17_Pin := (A, 6);
   A7 : constant MCP23x17_Pin := (A, 7);
   B0 : constant MCP23x17_Pin := (B, 0);
   B1 : constant MCP23x17_Pin := (B, 1);
   B2 : constant MCP23x17_Pin := (B, 2);
   B3 : constant MCP23x17_Pin := (B, 3);
   B4 : constant MCP23x17_Pin := (B, 4);
   B5 : constant MCP23x17_Pin := (B, 5);
   B6 : constant MCP23x17_Pin := (B, 6);
   B7 : constant MCP23x17_Pin := (B, 7);

   type MCP23x17_IO_Expander is abstract tagged private;

   procedure Configure (This         : in out MCP23x17_IO_Expander;
                        Pin          :        MCP23x17_Pin;
                        Output       :        Boolean;
                        Pull_Up      :        Boolean;
                        Invert_Input :        Boolean := False);
   --  Configure single pin as input or output and optionally enable
   --  the pull-up resistor. Input logic is inverted if Invert_Input
   --  is true.

   procedure Configure_Mode (This    : in out MCP23x17_IO_Expander;
                             Pin     : MCP23x17_Pin;
                             Output  : Boolean);
   --  Configure single pin as input or output

   procedure Configure_Mode (This    : in out MCP23x17_IO_Expander;
                             Port    : MCP23x17_Port;
                             Output  : Boolean);
   --  Configure all pins of a port as input or output

   function Is_Output (This : in out MCP23x17_IO_Expander;
                       Pin  : MCP23x17_Pin)
                       return Boolean;

   procedure Configure_Pull_Up (This    : in out MCP23x17_IO_Expander;
                                Pin     : MCP23x17_Pin;
                                Pull_Up : Boolean);
   --  Configure single pin pull up resistor

   procedure Configure_Pull_Up (This    : in out MCP23x17_IO_Expander;
                                Port    : MCP23x17_Port;
                                Pull_Up : Boolean);
   --  Configure pull-up resistor for all pins of a port

   function Has_Pull_Up (This : MCP23x17_IO_Expander;
                         Pin  : MCP23x17_Pin) return Boolean;
   --  Return True if the internal pull-up resistor in enabled

   procedure Configure_Polarity (This         : in out MCP23x17_IO_Expander;
                                 Pin          : MCP23x17_Pin;
                                 Invert_Input : Boolean);
   --  Configure input polarity of input pin.

   procedure Configure_Polarity (This         : in out MCP23x17_IO_Expander;
                                 Port         : MCP23x17_Port;
                                 Invert_Input : Boolean);
   --  Configure input polarity of all pins of a port.

   function Input_Is_Inverted (This : MCP23x17_IO_Expander;
                               Pin  : MCP23x17_Pin) return Boolean;
   --  Return True if the input polarity is inverted.

   function Is_Set (This : MCP23x17_IO_Expander;
                    Pin  : MCP23x17_Pin) return Boolean;
   --  Return the curent status of Pin

   procedure Set (This  : in out MCP23x17_IO_Expander;
                  Pin   : MCP23x17_Pin;
                  Value : Boolean := True);
   --  Change status of Pin to Value (True = logic high)

   procedure Clear (This  : in out MCP23x17_IO_Expander;
                    Pin   : MCP23x17_Pin); -- is (Set (This, Pin, False));
   --  Change status of Pin to logic low

   procedure Toggle (This  : in out MCP23x17_IO_Expander;
                     Pin   : MCP23x17_Pin);
   --  Toggle status of Pin

   type Port_IO_Array is array (MCP23x17_Pin_Number) of Boolean
     with Pack, Size => 8;

   function Get_IO (This : in out MCP23x17_IO_Expander;
                    Port : MCP23x17_Port) return Port_IO_Array;
   --  Return the current status of all pins of a given port in an array

   procedure Get_IO (This : in out MCP23x17_IO_Expander;
                     Port : MCP23x17_Port;
                     IOs  : out Port_IO_Array);
   --  Return the current status of selected pins in an array

   procedure Set_IO (This : in out MCP23x17_IO_Expander;
                     Port : MCP23x17_Port;
                     IOs  : Port_IO_Array);
   --  Set the current status of selected pins with an array

   function As_GPIO_Point (This : in out MCP23x17_IO_Expander;
                           Pin  : MCP23x17_Pin)
                           return not null HAL.GPIO.Any_GPIO_Point;
   --  Return the HAL.GPIO interface conresponding to Pin

   --  The MCP23x17 family has the ability to operate in Byte mode or
   --  Sequential mode (SEQOP).
   type Configuration_Bits is (Unimplemented, --  Read as ‘0’
                               INTPOL,        --  polarity of the INT output pin
                               ODR,           --  INT pin as open-drain output
                               HAEN,          --  h/w address enable for SPI
                               DISSLW,        --  slew rate control for SDA output
                               SEQOP,         --  sequential operation mode
                               MIRROR,        --  INT pins mirror
                               BANK);         --  mode of addressing registers

   type Module_Configuration is array (Configuration_Bits) of Boolean
     with Pack, Size => 8;

   procedure Set_Module_Configuration (This : in out MCP23x17_IO_Expander;
                                       Conf : Module_Configuration);
   --  set the module configuration

   procedure Get_Module_Configuration (This : in out MCP23x17_IO_Expander;
                                       Conf : out Module_Configuration);
   --  return the current module configuration.

private

   type MCP23x17_GPIO_Point is new HAL.GPIO.GPIO_Point with record
      Device : access MCP23x17_IO_Expander;
      Port   : MCP23x17_Port;
      Pin_Nr : MCP23x17_Pin_Number;
   end record;
   --  Internal implementation of HAL.GPIO interface

   overriding
   function Support (This : MCP23x17_GPIO_Point;
                     Capa : HAL.GPIO.Capability)
                     return Boolean
   is (case Capa is
          when HAL.GPIO.Pull_Down => False,
          when others             => True);

   overriding
   function Mode (This : MCP23x17_GPIO_Point) return HAL.GPIO.GPIO_Mode;

   overriding
   procedure Set_Mode (This : in out MCP23x17_GPIO_Point;
                       Mode : HAL.GPIO.GPIO_Config_Mode);

   overriding
   function Pull_Resistor (This : MCP23x17_GPIO_Point)
                           return HAL.GPIO.GPIO_Pull_Resistor;

   overriding
   procedure Set_Pull_Resistor (This : in out MCP23x17_GPIO_Point;
                                Pull : HAL.GPIO.GPIO_Pull_Resistor);

   overriding
   function Set (This : MCP23x17_GPIO_Point) return Boolean;

   overriding
   procedure Set (This : in out MCP23x17_GPIO_Point);

   overriding
   procedure Clear (This : in out MCP23x17_GPIO_Point);

   overriding
   procedure Toggle (This : in out MCP23x17_GPIO_Point);

   type MCP23x17_GPIO_Point_Array is array (MCP23x17_Pin_Number) of
     aliased MCP23x17_GPIO_Point;

   --------------------------
   -- MCP23x17_IO_Expander --
   --------------------------

   type MCP23x17_IO_Expander is abstract tagged record
      A_Points : MCP23x17_GPIO_Point_Array;
      B_Points : MCP23x17_GPIO_Point_Array;
   end record;

   ----------------------------
   -- IO private subprograms --
   ----------------------------

   type Register_Address is new UInt8 range 16#00# .. 16#1A#;

   procedure IO_Write
     (This  : in out MCP23x17_IO_Expander;
      Addr  : Register_Address;
      Value : UInt8) is null;
   --  This procedure must be overridden by the I2C or SPI implementation

   procedure IO_Read
     (This  : MCP23x17_IO_Expander;
      Addr  : Register_Address;
      Value : out UInt8) is null;
   --  This procedure must be overridden by the I2C or SPI implementation

   --  Register addresses for IO_Con.Bank = False (0)
   IO_Dir_A    : constant Register_Address := 16#00#;
   IO_Dir_B    : constant Register_Address := 16#01#;
   I_Pol_A     : constant Register_Address := 16#02#;
   I_Pol_B     : constant Register_Address := 16#03#;
   GP_Int_En_A : constant Register_Address := 16#04#;
   GP_Int_En_B : constant Register_Address := 16#05#;
   Def_Val_A   : constant Register_Address := 16#06#;
   Def_Val_B   : constant Register_Address := 16#07#;
   Int_Con_A   : constant Register_Address := 16#08#;
   Int_Con_B   : constant Register_Address := 16#09#;
   IO_Con      : constant Register_Address := 16#0A#; --  16#0B#
   GP_Pu_A     : constant Register_Address := 16#0C#;
   GP_Pu_B     : constant Register_Address := 16#0D#;
   Int_F_A     : constant Register_Address := 16#0E#;
   Int_F_B     : constant Register_Address := 16#0F#;
   Int_Cap_A   : constant Register_Address := 16#10#;
   Int_Cap_B   : constant Register_Address := 16#11#;
   GP_IO_A     : constant Register_Address := 16#12#;
   GP_IO_B     : constant Register_Address := 16#13#;
   O_Lat_A     : constant Register_Address := 16#14#;
   O_Lat_B     : constant Register_Address := 16#15#;

   --  Register addresses for IO_Con.Bank = True (1)
   --  IO_Dir_A    : constant Register_Address := 16#00#;
   --  IO_Dir_B    : constant Register_Address := 16#10#;
   --  I_Pol_A     : constant Register_Address := 16#01#;
   --  I_Pol_B     : constant Register_Address := 16#11#;
   --  GP_Int_En_A : constant Register_Address := 16#02#;
   --  GP_Int_En_B : constant Register_Address := 16#12#;
   --  Def_Val_A   : constant Register_Address := 16#03#;
   --  Def_Val_B   : constant Register_Address := 16#13#;
   --  Int_Con_A   : constant Register_Address := 16#04#;
   --  Int_Con_B   : constant Register_Address := 16#14#;
   --  IO_Con      : constant Register_Address := 16#05#; --  16#15#
   --  GP_Pu_A     : constant Register_Address := 16#06#;
   --  GP_Pu_B     : constant Register_Address := 16#16#;
   --  Int_F_A     : constant Register_Address := 16#07#;
   --  Int_F_B     : constant Register_Address := 16#17#;
   --  Int_Cap_A   : constant Register_Address := 16#08#;
   --  Int_Cap_B   : constant Register_Address := 16#18#;
   --  GP_IO_A     : constant Register_Address := 16#09#;
   --  GP_IO_B     : constant Register_Address := 16#19#;
   --  O_Lat_A     : constant Register_Address := 16#0A#;
   --  O_Lat_B     : constant Register_Address := 16#1A#;

end MCP23x17;
