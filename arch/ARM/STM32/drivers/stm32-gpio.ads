------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2018, AdaCore                         --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32f4xx_hal_gpio.h                                          --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   Header file of GPIO HAL module.                               --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides definitions for the GPIO ports on the STM32F4 (ARM
--  Cortex M4F) microcontrollers from ST Microelectronics.

private with STM32_SVD.GPIO;
with STM32.EXTI;
with HAL.GPIO;

package STM32.GPIO is

   type GPIO_Port is limited private;

   type GPIO_Pin is
     (Pin_0, Pin_1, Pin_2,  Pin_3,  Pin_4,  Pin_5,  Pin_6,  Pin_7,
      Pin_8, Pin_9, Pin_10, Pin_11, Pin_12, Pin_13, Pin_14, Pin_15);

   for GPIO_Pin use
     (Pin_0  => 16#0001#,
      Pin_1  => 16#0002#,
      Pin_2  => 16#0004#,
      Pin_3  => 16#0008#,
      Pin_4  => 16#0010#,
      Pin_5  => 16#0020#,
      Pin_6  => 16#0040#,
      Pin_7  => 16#0080#,
      Pin_8  => 16#0100#,
      Pin_9  => 16#0200#,
      Pin_10 => 16#0400#,
      Pin_11 => 16#0800#,
      Pin_12 => 16#1000#,
      Pin_13 => 16#2000#,
      Pin_14 => 16#4000#,
      Pin_15 => 16#8000#);

   for GPIO_Pin'Size use 16;
   --  for compatibility with hardware registers

   type GPIO_Pins is array (Positive range <>) of GPIO_Pin;
   --  Note that, in addition to aggregates, the language-defined catenation
   --  operator "&" is available for types GPIO_Pin and GPIO_Pins, allowing one
   --  to construct GPIO_Pins values conveniently

   All_Pins : constant GPIO_Pins :=
                (Pin_0, Pin_1, Pin_2, Pin_3, Pin_4, Pin_5, Pin_6, Pin_7,
                 Pin_8, Pin_9, Pin_10, Pin_11, Pin_12, Pin_13, Pin_14, Pin_15);

   type Pin_IO_Modes is (Mode_In, Mode_Out, Mode_AF, Mode_Analog)
     with Size => 2;

   for Pin_IO_Modes use
     (Mode_In     => 0,
      Mode_Out    => 1,
      Mode_AF     => 2,
      Mode_Analog => 3);

   type Pin_Output_Types is (Push_Pull, Open_Drain)
     with Size => 1;

   for Pin_Output_Types use (Push_Pull => 0, Open_Drain => 1);

   type Pin_Output_Speeds is (Speed_2MHz,  Speed_25MHz, Speed_50MHz, Speed_100MHz)
     with Size => 2;

   for Pin_Output_Speeds use
     (Speed_2MHz   => 0,  -- low
      Speed_25MHz  => 1,  -- medium
      Speed_50MHz  => 2,  -- high
      Speed_100MHz => 3); -- very high

   Speed_Low       : Pin_Output_Speeds renames Speed_2MHz;
   Speed_Medium    : Pin_Output_Speeds renames Speed_25MHz;
   Speed_High      : Pin_Output_Speeds renames Speed_50MHz;
   Speed_Very_High : Pin_Output_Speeds renames Speed_100MHz;

   type Internal_Pin_Resistors is (Floating, Pull_Up, Pull_Down)
     with Size => 2;

   for Internal_Pin_Resistors use (Floating  => 0,
                                   Pull_Up   => 1,
                                   Pull_Down => 2);

   --  see the Reference Manual, table 35, section 8.3
   type GPIO_Port_Configuration (Mode : Pin_IO_Modes := Mode_In) is record
      Resistors : Internal_Pin_Resistors;
      case Mode is
         when Mode_In | Mode_Analog =>
            null;
         when Mode_Out =>
            Output_Type : Pin_Output_Types;
            Speed       : Pin_Output_Speeds;
         when Mode_AF =>
            AF_Output_Type : Pin_Output_Types;
            AF_Speed       : Pin_Output_Speeds;
            AF             : GPIO_Alternate_Function;
      end case;
   end record;

   type GPIO_Point is new HAL.GPIO.GPIO_Point with record
      Periph : access GPIO_Port;
      --  Port should be a not null access, but this raises an exception
      --  during elaboration.
      Pin    : GPIO_Pin;
   end record;

   overriding
   function Support (This : GPIO_Point;
                     Capa : HAL.GPIO.Capability)
                     return Boolean
   is (case Capa is
          when others => True);
   --  STM32 supports all GPIO capabilities

   overriding
   function Mode (This : GPIO_Point) return HAL.GPIO.GPIO_Mode;

   overriding
   procedure Set_Mode (This : in out GPIO_Point;
                       Mode : HAL.GPIO.GPIO_Config_Mode);

   overriding
   function Pull_Resistor (This : GPIO_Point)
                           return HAL.GPIO.GPIO_Pull_Resistor;

   overriding
   procedure Set_Pull_Resistor (This : in out GPIO_Point;
                                Pull : HAL.GPIO.GPIO_Pull_Resistor);

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

   procedure Lock (This : GPIO_Point) with
     Pre  => not Locked (This),
     Post => Locked (This);
   --  For the given GPIO port, locks the current configuration of Pin until
   --  the MCU is reset.

   function Locked (This : GPIO_Point) return Boolean
     with Inline;

   procedure Configure_IO
     (This   : GPIO_Point;
      Config : GPIO_Port_Configuration);
   --  For Point.Pin on the Point.Port.all, configures the
   --  characteristics specified by Config

   function Pin_IO_Mode (This : GPIO_Point) return Pin_IO_Modes with Inline;

   function Interrupt_Line_Number
     (This : GPIO_Point) return EXTI.External_Line_Number;
   --  Returns the external interrupt line number that corresponds to the
   --  GPIO point.

   procedure Configure_Trigger
     (This    : GPIO_Point;
      Trigger : EXTI.External_Triggers);
   --  For Point.Pin on Point.Port.all, connects the external line and enables
   --  the external Trigger.  Enables the SYSCFG clock.

   procedure Configure_Alternate_Function
     (This : GPIO_Point;
      AF   : GPIO_Alternate_Function);
   --  For Point.Pin on Point.Port.all, sets the alternate function
   --  specified by AF

   type GPIO_Points is array (Positive range <>) of GPIO_Point;

   function Any_Set (Pins : GPIO_Points) return Boolean with
     Inline;
   --  Returns True if any one of the bits specified by Pins is set (not zero)
   --  in the Port input data register; returns False otherwise.

   function Set (Pins : GPIO_Points) return Boolean
     renames Any_Set;
   --  Defined for readability when only one pin is specified in GPIO_Pins

   function All_Set (Pins : GPIO_Points) return Boolean with
     Inline;
   --  Returns True iff all of the bits specified by Pins are set (not zero) in
   --  the Port input data register; returns False otherwise.

   procedure Set (Pins : in out GPIO_Points) with
     Inline;
   --  For the given GPIO port, sets all of the output data register bits
   --  specified by Pins to one. Other pins are unaffected.

   procedure Clear (Pins : in out GPIO_Points) with
     Inline;
   --  For the given GPIO port, sets of all of the output data register bits
   --  specified by Pins to zero. Other pins are unaffected.

   procedure Toggle (Points : in out GPIO_Points) with Inline;
   --  For the given GPIO ports, negates all of the output data register bis
   --  specified by Pins (ones become zeros and vice versa). Other pins are
   --  unaffected.

   procedure Lock (Points : GPIO_Points);
   --  For the given GPIO port, locks the current configuration of Pin until
   --  the MCU is reset.

   procedure Configure_IO
     (Points : GPIO_Points;
      Config : GPIO_Port_Configuration);
   --  For Point.Pin on the Point.Port.all, configures the
   --  characteristics specified by Config

   procedure Configure_Trigger
     (Points  : GPIO_Points;
      Trigger : EXTI.External_Triggers);
   --  For Point.Pin on Point.Port.all, configures the
   --  characteristics specified by Trigger

   procedure Configure_Alternate_Function
     (Points : GPIO_Points;
      AF     : GPIO_Alternate_Function);
   --  For Point.Pin on Point.Port.all, sets the alternate function
   --  specified by AF

private

   type GPIO_Port is new STM32_SVD.GPIO.GPIO_Peripheral;

   LCCK : constant UInt32 := 16#0001_0000#;
   --  As per the Reference Manual (RM0090; Doc ID 018909 Rev 6) pg 282,
   --  this is the "Lock Key" used to control the locking of port/pin
   --  configurations. It is bit 16 in the lock register (LCKR) of any
   --  given port, thus the first bit of the upper 16 bits of the word.

end STM32.GPIO;
