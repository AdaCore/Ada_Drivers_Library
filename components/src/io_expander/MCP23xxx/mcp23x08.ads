------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015-2018, AdaCore                      --
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

--  The MCP23008 and MCP23S08 are two versions of the same chip, respectively
--  with an I2C and SPI interface. This package provides an abstract
--  implementation of the features common to both chips.
--
--  Please use the MCP23008 (or MCP23x008.I2C) package for the I2C interface.
--  The SPI interface is not implemented yet.

with HAL; use HAL;
with HAL.GPIO;

package MCP23x08 is

   type MCP23x08_Pin is (Pin_0, Pin_1, Pin_2, Pin_3,
                         Pin_4, Pin_5, Pin_6, Pin_7);

   type MCP23x08_IO_Expander is abstract tagged private;

   procedure Configure (This    : in out MCP23x08_IO_Expander;
                        Pin     : MCP23x08_Pin;
                        Output  : Boolean;
                        Pull_Up : Boolean);
   --  Configure single pin as input or output and optinaly enable the pull-up
   --  resistor.

   procedure Configure_Mode (This    : in out MCP23x08_IO_Expander;
                             Pin     : MCP23x08_Pin;
                             Output  : Boolean);
   --  Configure single pin as input or output

   function Is_Output (This : in out MCP23x08_IO_Expander;
                       Pin  : MCP23x08_Pin)
                       return Boolean;

   procedure Configure_Pull (This    : in out MCP23x08_IO_Expander;
                             Pin     : MCP23x08_Pin;
                             Pull_Up : Boolean);
   --  Configure single pin pull up resistor

   function Pull_Up (This : MCP23x08_IO_Expander;
                     Pin  : MCP23x08_Pin) return Boolean;
   --  Return True if the internal pull-up resistor in enabled

   function Set (This  : MCP23x08_IO_Expander;
                 Pin   : MCP23x08_Pin) return Boolean;
   --  Return the curent status of Pin

   procedure Set (This  : in out MCP23x08_IO_Expander;
                  Pin   : MCP23x08_Pin);
   --  Change status of Pin to logic high

   procedure Clear (This  : in out MCP23x08_IO_Expander;
                    Pin   : MCP23x08_Pin);
   --  Change status of Pin to logic low

   procedure Toggle (This  : in out MCP23x08_IO_Expander;
                     Pin   : MCP23x08_Pin);
   --  Toggle status of Pin

   type ALl_IO_Array is array (MCP23x08_Pin) of Boolean
     with Pack, Size => 8;

   function All_IO (This : in out MCP23x08_IO_Expander) return ALl_IO_Array;
   --  Return the current status of all pins in an array

   procedure Set_All_IO (This : in out MCP23x08_IO_Expander;
                         IOs  : ALl_IO_Array);
   --  Set the current status of all pins with an array

   function As_GPIO_Point (This : in out MCP23x08_IO_Expander;
                           Pin  : MCP23x08_Pin)
                           return not null HAL.GPIO.Any_GPIO_Point;
   --  Return the HAL.GPIO interface conresponding to Pin

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
      Device : access MCP23x08_IO_Expander;
      Pin    : MCP23x08_Pin;
   end record;
   --  Internal implementation of HAL.GPIO interface

   overriding
   function Support (This : MCP23_GPIO_Point;
                     Capa : HAL.GPIO.Capability)
                     return Boolean
   is (case Capa is
          when HAL.GPIO.Pull_Down => False,
          when others             => True);

   overriding
   function Mode (This : MCP23_GPIO_Point) return HAL.GPIO.GPIO_Mode;

   overriding
   procedure Set_Mode (This : in out MCP23_GPIO_Point;
                       Mode : HAL.GPIO.GPIO_Config_Mode);

   overriding
   function Pull_Resistor (This : MCP23_GPIO_Point)
                           return HAL.GPIO.GPIO_Pull_Resistor;

   overriding
   procedure Set_Pull_Resistor (This : in out MCP23_GPIO_Point;
                                Pull : HAL.GPIO.GPIO_Pull_Resistor);

   overriding
   function Set (This : MCP23_GPIO_Point) return Boolean;

   overriding
   procedure Set (This : in out MCP23_GPIO_Point);

   overriding
   procedure Clear (This : in out MCP23_GPIO_Point);

   overriding
   procedure Toggle (This : in out MCP23_GPIO_Point);

   type MCP23_GPIO_Point_Array is array (MCP23x08_Pin) of
     aliased MCP23_GPIO_Point;

   --------------------------
   -- MCP23x08_IO_Expander --
   --------------------------

   type MCP23x08_IO_Expander is abstract tagged record
      Points : MCP23_GPIO_Point_Array;
   end record;

   ----------------------------
   -- IO private subprograms --
   ----------------------------

   type Register_Address is new UInt8;

   procedure IO_Write
     (This      : in out MCP23x08_IO_Expander;
      WriteAddr : Register_Address;
      Value     : UInt8) is null;
   --  This procedure must be overridden by the I2C or SPI implementation

   procedure IO_Read
     (This     : MCP23x08_IO_Expander;
      ReadAddr : Register_Address;
      Value    : out UInt8) is null;
   --  This procedure must be overridden by the I2C or SPI implementation

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
