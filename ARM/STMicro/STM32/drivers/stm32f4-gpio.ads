------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2016, AdaCore                        --
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

pragma Restrictions (No_Elaboration_Code);

with STM32F4.EXTI;

package STM32F4.GPIO is

   type GPIO_Port is limited private;

   function Current_Input (Port : GPIO_Port) return Half_Word with Inline;
   --  Returns the value of the Port's input data register

   function Current_Output (Port : GPIO_Port) return Half_Word with Inline;
   --  Returns the value of the Port's output data register

   procedure Write_Output (Port : in out GPIO_Port; Data : Half_Word) with Inline;
   --  Sets the value of the Port's output data register to Data.  All bits
   --  in the register are affected.

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

   type GPIO_Point is record
      Port : access GPIO_Port;
      Pin  : GPIO_Pin;
   end record;

   type GPIO_Pins is array (Positive range <>) of GPIO_Pin;
   --  Note that, in addition to aggregates, the language-defined catenation
   --  operator "&" is available for types GPIO_Pin and GPIO_Pins, allowing one
   --  to construct GPIO_Pins values conveniently

   All_Pins : constant GPIO_Pins :=
                (Pin_0, Pin_1, Pin_2, Pin_3, Pin_4, Pin_5, Pin_6, Pin_7,
                 Pin_8, Pin_9, Pin_10, Pin_11, Pin_12, Pin_13, Pin_14, Pin_15);

   function Any_Set (Port : GPIO_Port;  Pins : GPIO_Pins) return Boolean with Inline;
   --  Returns True if any one of the bits specified by Pins is set (not zero)
   --  in the Port input data register; returns False otherwise.

   function Set (Port : GPIO_Port;  Pins : GPIO_Pins) return Boolean
     renames Any_Set;
   --  Defined for readability when only one pin is specified in GPIO_Pins

   function Set (Port : GPIO_Port;  Pin : GPIO_Pin) return Boolean with Inline;
   --  Returns True if the bit specified by Pin is set (not zero) in the Port
   --  input data register; returns False otherwise.

   function Set (This : GPIO_Point) return Boolean with Inline;
   --  Returns True if the bit specified by This.Pin is set (not zero) in the
   --  input data register of This.Port.all; returns False otherwise.

   function All_Set (Port : GPIO_Port;  Pins : GPIO_Pins) return Boolean with Inline;
   --  Returns True iff all of the bits specified by Pins are set (not zero) in
   --  the Port input data register; returns False otherwise.

   procedure Set (Port : in out GPIO_Port;  Pin : GPIO_Pin) with Inline;
   --  For the given GPIO port, sets the output data register bit specified by
   --  Pin to one. Other pins are unaffected.

   procedure Set (Port : in out GPIO_Port;  Pins : GPIO_Pins) with Inline;
   --  For the given GPIO port, sets all of the output data register bits
   --  specified by Pins to one. Other pins are unaffected.

   procedure Set (This : in out GPIO_Point) with Inline;
   --  For This.Port.all, sets the output data register bit specified by
   --  This.Pin to one. Other pins are unaffected.

   procedure Clear (Port : in out GPIO_Port;  Pin : GPIO_Pin) with Inline;
   --  For the given GPIO port, sets the output data register bit specified by
   --  Pin to zero. Other pins are unaffected.

   procedure Clear (Port : in out GPIO_Port;  Pins : GPIO_Pins) with
     Inline;
   --  For the given GPIO port, sets of all of the output data register bits
   --  specified by Pins to zero. Other pins are unaffected.

   procedure Clear (This : in out GPIO_Point) with Inline;
   --  For This.Port.all, sets the output data register bit specified by
   --  This.Pin to zero. Other pins are unaffected.

   procedure Toggle (Port : in out GPIO_Port;  Pin : GPIO_Pin) with Inline;
   --  For the given GPIO port, negates the output data register bit specified
   --  by Pin (one becomes zero and vice versa). Other pins are unaffected.

   procedure Toggle (Port : in out GPIO_Port;  Pins : GPIO_Pins) with Inline;
   --  For the given GPIO port, negates all of the output data register bis
   --  specified by Pins (ones become zeros and vice versa). Other pins are
   --  unaffected.

   procedure Toggle (This : in out GPIO_Point) with Inline;
   --  For This.Port.all, negates the output data register bit specified by
   --  This.Pin (one becomes zero and vice versa). Other pins are unaffected.

   procedure Lock (Port : in out GPIO_Port;  Pin : GPIO_Pin) with
     Pre  => not Locked (Port, Pin),
     Post => Locked (Port, Pin);
   --  For the given GPIO port, locks the current configuration of Pin until
   --  the MCU is reset.

   function Locked (Port : GPIO_Port;  Pin : GPIO_Pin) return Boolean with Inline;

   procedure Lock (Port : in out GPIO_Port;  Pins : GPIO_Pins) with
     Pre  => (for all Pin of Pins => not Locked (Port, Pin)),
     Post => (for all Pin of Pins => Locked (Port, Pin));
   --  Locks the current configuration of the specified pins on the given port
   --  until the MCU is reset.

   procedure Lock (Point : GPIO_Point) with
     Pre  => not Locked (Point),
     Post => Locked (Point);
   --  Locks the current configuration of the given port/pin pair until the MCU
   --  is reset.

   function Locked (Point : GPIO_Point) return Boolean with Inline;

   type Pin_IO_Modes is (Mode_In, Mode_Out, Mode_AF, Mode_Analog);

   for Pin_IO_Modes use
     (Mode_In     => 0,
      Mode_Out    => 1,
      Mode_AF     => 2,
      Mode_Analog => 3);
   for Pin_IO_Modes'Size use 2;

   type Pin_Output_Types is (Push_Pull, Open_Drain);

   for Pin_Output_Types use (Push_Pull => 0, Open_Drain => 1);
   for Pin_Output_Types'Size use 1;

   type Pin_Output_Speeds is
     (Speed_2MHz,  Speed_25MHz, Speed_50MHz, Speed_100MHz);

   for Pin_Output_Speeds use
     (Speed_2MHz   => 0,  -- low
      Speed_25MHz  => 1,  -- medium
      Speed_50MHz  => 2,  -- high
      Speed_100MHz => 3); -- very high
   for Pin_Output_Speeds'Size use 2;

   Speed_Low       : Pin_Output_Speeds renames Speed_2MHz;
   Speed_Medium    : Pin_Output_Speeds renames Speed_25MHz;
   Speed_High      : Pin_Output_Speeds renames Speed_50MHz;
   Speed_Very_High : Pin_Output_Speeds renames Speed_100MHz;

   type Internal_Pin_Resistors is (Floating, Pull_Up, Pull_Down);

   for Internal_Pin_Resistors use (Floating => 0, Pull_Up => 1, Pull_Down => 2);
   for Internal_Pin_Resistors'Size use 2;

   type GPIO_Port_Configuration is record
      Mode        : Pin_IO_Modes;
      Output_Type : Pin_Output_Types;
      Speed       : Pin_Output_Speeds;
      Resistors   : Internal_Pin_Resistors;
   end record;

   procedure Configure_IO
     (Port   : in out GPIO_Port;
      Pin    : GPIO_Pin;
      Config : GPIO_Port_Configuration);
   --  For Pin on the specified Port, configures the
   --  characteristics specified by Config

   procedure Configure_IO
     (Port   : in out GPIO_Port;
      Pins   : GPIO_Pins;
      Config : GPIO_Port_Configuration);
   --  For each pin of Pins on the specified Port, configures the
   --  characteristics specified by Config

   procedure Configure_IO
     (Point  : GPIO_Point;
      Config : GPIO_Port_Configuration);
   --  For Point.Pin on the Point.Port.all, configures the
   --  characteristics specified by Config

   procedure Configure_Trigger
     (Port    : in out GPIO_Port;
      Pin     : GPIO_Pin;
      Trigger : EXTI.External_Triggers);
   --  Connects the external line for Port and Pin, and enables the external
   --  Trigger.  Enables the SYSCFG clock.

   procedure Configure_Trigger
     (Port    : in out GPIO_Port;
      Pins    : GPIO_Pins;
      Trigger : EXTI.External_Triggers);
   --  For each pin of Pins on the specified Port, connects the external line
   --  and enables the external Trigger.  Enables the SYSCFG clock.

   procedure Configure_Trigger
     (Point   : GPIO_Point;
      Trigger : EXTI.External_Triggers);
   --  For Point.Pin on Point.Port.all, connects the external line and enables
   --  the external Trigger.  Enables the SYSCFG clock.

   type GPIO_Alternate_Function is private;

   procedure Configure_Alternate_Function
     (Port : in out GPIO_Port;
      Pin  : GPIO_Pin;
      AF   : GPIO_Alternate_Function);
   --  For Pin on the specified Port, sets the alternate function
   --  specified by AF

   procedure Configure_Alternate_Function
     (Port : in out GPIO_Port;
      Pins : GPIO_Pins;
      AF   : GPIO_Alternate_Function);
   --  For each pin of Pins on the specified Port, sets the alternate function
   --  specified by AF

   procedure Configure_Alternate_Function
     (Point : GPIO_Point;
      AF    : GPIO_Alternate_Function);
   --  For Point.Pin on Point.Port.all, sets the alternate function
   --  specified by AF

   GPIO_AF_RTC_50Hz  : constant GPIO_Alternate_Function;
   GPIO_AF_MCO       : constant GPIO_Alternate_Function;
   GPIO_AF_TAMPER    : constant GPIO_Alternate_Function;
   GPIO_AF_SWJ       : constant GPIO_Alternate_Function;
   GPIO_AF_TRACE     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM1      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM2      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM3      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM4      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM5      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM8      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM9      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM10     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM11     : constant GPIO_Alternate_Function;
   GPIO_AF_I2C1      : constant GPIO_Alternate_Function;
   GPIO_AF_I2C2      : constant GPIO_Alternate_Function;
   GPIO_AF_I2C3      : constant GPIO_Alternate_Function;
   GPIO_AF_SPI1      : constant GPIO_Alternate_Function;
   GPIO_AF_SPI2      : constant GPIO_Alternate_Function;
   GPIO_AF5_I2S3ext  : constant GPIO_Alternate_Function;
   GPIO_AF_SPI4      : constant GPIO_Alternate_Function;
   GPIO_AF_SPI5      : constant GPIO_Alternate_Function;
   GPIO_AF_SPI6      : constant GPIO_Alternate_Function;
   GPIO_AF_SPI3      : constant GPIO_Alternate_Function;
   GPIO_AF_I2S2ext   : constant GPIO_Alternate_Function;
   GPIO_AF_USART1    : constant GPIO_Alternate_Function;
   GPIO_AF_USART2    : constant GPIO_Alternate_Function;
   GPIO_AF_USART3    : constant GPIO_Alternate_Function;
   GPIO_AF7_I2S3ext  : constant GPIO_Alternate_Function;
   GPIO_AF7_SPI3     : constant GPIO_Alternate_Function;
   GPIO_AF_USART4    : constant GPIO_Alternate_Function;
   GPIO_AF_USART5    : constant GPIO_Alternate_Function;
   GPIO_AF_USART6    : constant GPIO_Alternate_Function;
   GPIO_AF_CAN1      : constant GPIO_Alternate_Function;
   GPIO_AF_CAN2      : constant GPIO_Alternate_Function;
   GPIO_AF_TIM12     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM13     : constant GPIO_Alternate_Function;
   GPIO_AF_TIM14     : constant GPIO_Alternate_Function;
   GPIO_AF_LTDC_2    : constant GPIO_Alternate_Function;
   GPIO_AF_OTG_FS    : constant GPIO_Alternate_Function;
   GPIO_AF_OTG_HS    : constant GPIO_Alternate_Function;
   GPIO_AF_ETH       : constant GPIO_Alternate_Function;
   GPIO_AF_FSMC      : constant GPIO_Alternate_Function;
   GPIO_AF_OTG_HS_FS : constant GPIO_Alternate_Function;
   GPIO_AF_SDIO      : constant GPIO_Alternate_Function;
   GPIO_AF_FMC       : constant GPIO_Alternate_Function;
   GPIO_AF_DCMI      : constant GPIO_Alternate_Function;
   GPIO_AF_LTDC      : constant GPIO_Alternate_Function;
   GPIO_AF_EVENTOUT  : constant GPIO_Alternate_Function;

private

   type Reserved_246X32 is array (1 ..  246) of Word
     with Component_Size => 32, Size => 246*32;

   type Bits_16x4 is array (0 ..  15) of Bits_4
     with Component_Size => 4, Size => 64;

   type Pin_Modes_Register is array (0 .. 15) of Pin_IO_Modes;
   for Pin_Modes_Register'Component_Size use Pin_IO_Modes'Size;

   type Output_Types_Register is array (0 .. 15) of Pin_Output_Types;
   for Output_Types_Register'Component_Size use Pin_Output_Types'Size;

   type Output_Speeds_Register is array (0 .. 15) of Pin_Output_Speeds;
   for Output_Speeds_Register'Component_Size use Pin_Output_Speeds'Size;

   type Resistors_Register is array (0 .. 15) of Internal_Pin_Resistors;
   for Resistors_Register'Component_Size use Internal_Pin_Resistors'Size;

   type GPIO_Port is limited record
      MODER      : Pin_Modes_Register;
      OTYPER     : Output_Types_Register;
      Reserved_1 : Half_Word;
      OSPEEDR    : Output_Speeds_Register;
      PUPDR      : Resistors_Register;
      IDR        : Half_Word;       --  input data register
      Reserved_2 : Half_Word;
      ODR        : Half_Word;       --  output data register
      Reserved_3 : Half_Word;
      BSRR_Set   : Half_Word;       --  bit set register
      BSRR_Reset : Half_Word;       --  bit reset register
      LCKR       : Word with Atomic;
      AFR        : Bits_16x4;       --  alternate function registers
      Reserved_4 : Reserved_246x32;
   end record with
     Volatile,
     Size => 16#400# * 8;

   for GPIO_Port use record
      MODER      at 0  range 0 .. 31;
      OTYPER     at 4  range 0 .. 15;
      Reserved_1 at 6  range 0 .. 15;
      OSPEEDR    at 8  range 0 .. 31;
      PUPDR      at 12 range 0 .. 31;
      IDR        at 16 range 0 .. 15;
      Reserved_2 at 18 range 0 .. 15;
      ODR        at 20 range 0 .. 15;
      Reserved_3 at 22 range 0 .. 15;
      BSRR_Set   at 24 range 0 .. 15;
      BSRR_Reset at 26 range 0 .. 15;
      LCKR       at 28 range 0 .. 31;
      AFR        at 32 range 0 .. 63;
      Reserved_4 at 40 range 0 .. 7871;
   end record;

   LCCK : constant Word := 16#0001_0000#;
   --  As per the Reference Manual (RM0090; Doc ID 018909 Rev 6) pg 282,
   --  this is the "Lock Key" used to control the locking of port/pin
   --  configurations. It is bit 16 in the lock register (LCKR) of any
   --  given port, thus the first bit of the upper 16 bits of the word.

   type GPIO_Alternate_Function is new Bits_4;

   --  We cannot use an enumeration type because there are duplicate binary
   --  values

   GPIO_AF_RTC_50Hz  : constant GPIO_Alternate_Function := 0;
   GPIO_AF_MCO       : constant GPIO_Alternate_Function := 0;
   GPIO_AF_TAMPER    : constant GPIO_Alternate_Function := 0;
   GPIO_AF_SWJ       : constant GPIO_Alternate_Function := 0;
   GPIO_AF_TRACE     : constant GPIO_Alternate_Function := 0;
   GPIO_AF_TIM1      : constant GPIO_Alternate_Function := 1;
   GPIO_AF_TIM2      : constant GPIO_Alternate_Function := 1;
   GPIO_AF_TIM3      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_TIM4      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_TIM5      : constant GPIO_Alternate_Function := 2;
   GPIO_AF_TIM8      : constant GPIO_Alternate_Function := 3;
   GPIO_AF_TIM9      : constant GPIO_Alternate_Function := 3;
   GPIO_AF_TIM10     : constant GPIO_Alternate_Function := 3;
   GPIO_AF_TIM11     : constant GPIO_Alternate_Function := 3;
   GPIO_AF_I2C1      : constant GPIO_Alternate_Function := 4;
   GPIO_AF_I2C2      : constant GPIO_Alternate_Function := 4;
   GPIO_AF_I2C3      : constant GPIO_Alternate_Function := 4;
   GPIO_AF_SPI1      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_SPI2      : constant GPIO_Alternate_Function := 5;
   GPIO_AF5_I2S3ext  : constant GPIO_Alternate_Function := 5;
   GPIO_AF_SPI4      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_SPI5      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_SPI6      : constant GPIO_Alternate_Function := 5;
   GPIO_AF_SPI3      : constant GPIO_Alternate_Function := 6;
   GPIO_AF_I2S2ext   : constant GPIO_Alternate_Function := 6;
   GPIO_AF_USART1    : constant GPIO_Alternate_Function := 7;
   GPIO_AF_USART2    : constant GPIO_Alternate_Function := 7;
   GPIO_AF_USART3    : constant GPIO_Alternate_Function := 7;
   GPIO_AF7_I2S3ext  : constant GPIO_Alternate_Function := 7;
   GPIO_AF7_SPI3     : constant GPIO_Alternate_Function := 7;
   GPIO_AF_USART4    : constant GPIO_Alternate_Function := 8;
   GPIO_AF_USART5    : constant GPIO_Alternate_Function := 8;
   GPIO_AF_USART6    : constant GPIO_Alternate_Function := 8;
   GPIO_AF_CAN1      : constant GPIO_Alternate_Function := 9;
   GPIO_AF_CAN2      : constant GPIO_Alternate_Function := 9;
   GPIO_AF_TIM12     : constant GPIO_Alternate_Function := 9;
   GPIO_AF_TIM13     : constant GPIO_Alternate_Function := 9;
   GPIO_AF_TIM14     : constant GPIO_Alternate_Function := 9;
   GPIO_AF_LTDC_2    : constant GPIO_Alternate_Function := 9;
   GPIO_AF_OTG_FS    : constant GPIO_Alternate_Function := 10;
   GPIO_AF_OTG_HS    : constant GPIO_Alternate_Function := 10;
   GPIO_AF_ETH       : constant GPIO_Alternate_Function := 11;
   GPIO_AF_FSMC      : constant GPIO_Alternate_Function := 12;
   GPIO_AF_OTG_HS_FS : constant GPIO_Alternate_Function := 12;
   GPIO_AF_SDIO      : constant GPIO_Alternate_Function := 12;
   GPIO_AF_FMC       : constant GPIO_Alternate_Function := 12;
   GPIO_AF_DCMI      : constant GPIO_Alternate_Function := 13;
   GPIO_AF_LTDC      : constant GPIO_Alternate_Function := 14;
   GPIO_AF_EVENTOUT  : constant GPIO_Alternate_Function := 15;

end STM32F4.GPIO;
