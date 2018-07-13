------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017-2018, AdaCore                      --
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

with HAL.I2C;
with HAL.GPIO;

package STMPE1600 is

   type STMPE1600_Expander (Port : not null HAL.I2C.Any_I2C_Port;
                            Addr : HAL.I2C.I2C_Address) is limited private;
   type Any_STMPE1600_Expander is access all STMPE1600_Expander;

   type STMPE1600_Pin_Polarity is (Low, High) with Size => 1;
   type STMPE1600_Pin_Direction is (Input, Output) with Size => 1;
   type STMPE1600_Pin_Number is range 0 .. 15;

   type STMPE1600_Pins is array (STMPE1600_Pin_Number) of Boolean
     with Pack, Size => 16;

   type STMPE1600_Pins_Direction is
     array (STMPE1600_Pin_Number) of STMPE1600_Pin_Direction
     with Pack, Size => 16;

   procedure Check_Id
     (This   : in out STMPE1600_Expander;
      Status : out Boolean);

   procedure Set_Interrupt_Enable
     (This     : in out STMPE1600_Expander;
      Enable   : Boolean;
      Polarity : STMPE1600_Pin_Polarity;
      Status   : out Boolean);
   --  Enables/Disables the interrupt to the host.
   --  Allows also to set whether the interrupt is active High or Low.

   procedure Set_Interrupt_Mask
     (This : STMPE1600_Expander;
      Mask : STMPE1600_Pins;
      Status : out Boolean);
   --  Enables the interrupt from a particular GPIO interrupt source to the
   --  host.

   function Interrupt_Mask
     (This : STMPE1600_Expander) return STMPE1600_Pins;
   --  Returns the current value of the interrupt mask.

   function Interrupt_State
     (This : STMPE1600_Expander) return STMPE1600_Pins;
   --  Tells if pins have changed since last query. Reading this value
   --  clears the state.
   --  The state is set regardless of the interrupt mask.

   function Pins_State
     (This : in out STMPE1600_Expander) return STMPE1600_Pins;

   function Pin_State
     (This : in out STMPE1600_Expander;
      Pin  : STMPE1600_Pin_Number) return Boolean;

   procedure Set_Pins_State
     (This : in out STMPE1600_Expander;
      Pins : STMPE1600_Pins);

   procedure Set_Pin_State
     (This  : in out STMPE1600_Expander;
      Pin   : STMPE1600_Pin_Number;
      State : Boolean);

   procedure Set_Pins_Direction
     (This      : STMPE1600_Expander;
      Pins      : STMPE1600_Pins_Direction);

   procedure Set_Pin_Direction
     (This      : STMPE1600_Expander;
      Pin       : STMPE1600_Pin_Number;
      Direction : STMPE1600_Pin_Direction);

   function Pin_Direction
     (This : STMPE1600_Expander;
      Pin  : STMPE1600_Pin_Number)
      return STMPE1600_Pin_Direction;

   procedure Set_Pin_Polarity_Inversion
     (This            : STMPE1600_Expander;
      Pin             : STMPE1600_Pin_Number;
      Inversion_State : Boolean);
   --  Enables or disables the polarity inversion of the given input pin

   function As_GPIO_Point
     (This : in out STMPE1600_Expander;
      Pin  : STMPE1600_Pin_Number) return HAL.GPIO.Any_GPIO_Point;


private

   use type HAL.GPIO.GPIO_Pull_Resistor;

   type STMPE1600_SYS_CTRL is record
      INT_Polarity : STMPE1600_Pin_Polarity := Low;
      Reserved_1   : HAL.Bit := 0;
      INT_Enable   : Boolean := False;
      Reserved_3_4 : HAL.UInt2 := 0;
      Wakeup_En    : Boolean := False;
      I2C_Shdn     : Boolean := False;
      Soft_Reset   : Boolean := False;
   end record with Size => 8;

   for STMPE1600_SYS_CTRL use record
      INT_Polarity at 0 range 0 .. 0;
      Reserved_1   at 0 range 1 .. 1;
      INT_Enable   at 0 range 2 .. 2;
      Reserved_3_4 at 0 range 3 .. 4;
      Wakeup_En    at 0 range 5 .. 5;
      I2C_Shdn     at 0 range 6 .. 6;
      Soft_Reset   at 0 range 7 .. 7;
   end record;


   type STMPE1600_Pin is new HAL.GPIO.GPIO_Point with record
      Port : Any_STMPE1600_Expander;
      Pin  : STMPE1600_Pin_Number;
   end record;

   type STMPE1600_Pin_Array is
     array (STMPE1600_Pin_Number) of aliased STMPE1600_Pin;

   overriding
   function Support (This : STMPE1600_Pin;
                     Capa : HAL.GPIO.Capability)
                     return Boolean
   is (case Capa is
          when HAL.GPIO.Pull_Up | HAL.GPIO.Pull_Down => False,
          when others                                => True);
   --  STMPE1600 doesn't have internal pull resistors

   overriding
   function Mode (This : STMPE1600_Pin) return HAL.GPIO.GPIO_Mode;

   overriding
   procedure Set_Mode (This : in out STMPE1600_Pin;
                       Mode : HAL.GPIO.GPIO_Config_Mode);

   overriding
   function Pull_Resistor (This : STMPE1600_Pin)
                           return HAL.GPIO.GPIO_Pull_Resistor is
     (HAL.GPIO.Floating);

   overriding
   procedure Set_Pull_Resistor (This : in out STMPE1600_Pin;
                                Pull : HAL.GPIO.GPIO_Pull_Resistor)
   is null;
   --  STMPE1600 doesn't have internal pull resistors

   overriding
   function Set (This : STMPE1600_Pin) return Boolean;

   overriding
   procedure Set (This : in out STMPE1600_Pin);

   overriding
   procedure Clear (This : in out STMPE1600_Pin);

   overriding
   procedure Toggle (This : in out STMPE1600_Pin);

   type STMPE1600_Expander (Port : not null HAL.I2C.Any_I2C_Port;
                            Addr : HAL.I2C.I2C_Address)
   is limited record
      Points : STMPE1600_Pin_Array;
   end record;

   STMPE1600_REG_ChipID      : constant := 16#00#;
   STMPE1600_REG_Version_ID  : constant := 16#02#;
   STMPE1600_REG_System_Ctrl : constant := 16#03#;
   STMPE1600_REG_IEGPIOR_0   : constant := 16#08#;
   STMPE1600_REG_IEGPIOR_1   : constant := 16#09#;
   STMPE1600_REG_ISGPIOR_0   : constant := 16#0A#;
   STMPE1600_REG_ISGPIOR_1   : constant := 16#0B#;
   STMPE1600_REG_GPMR_0      : constant := 16#10#;
   STMPE1600_REG_GPMR_1      : constant := 16#11#;
   STMPE1600_REG_GPSR_0      : constant := 16#12#;
   STMPE1600_REG_GPSR_1      : constant := 16#13#;
   STMPE1600_REG_GPDR_0      : constant := 16#14#;
   STMPE1600_REG_GPDR_1      : constant := 16#15#;
   STMPE1600_REG_GPPIR_0     : constant := 16#16#;
   STMPE1600_REG_GPPIR_1     : constant := 16#17#;

   procedure Read
     (This   : STMPE1600_Expander;
      Reg    : HAL.UInt8;
      Data   : out HAL.UInt8_Array;
      Status : out Boolean);

   procedure Write
     (This   : STMPE1600_Expander;
      Reg    : HAL.UInt8;
      Data   : HAL.UInt8_Array;
      Status : out Boolean);

end STMPE1600;
