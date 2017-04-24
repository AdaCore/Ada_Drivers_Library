------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with System;

package RPi.Regs.GPIO is

   type GPIO_Pin is range 0 .. 53;

   type Pins_0_31 is array (GPIO_Pin range 0 .. 31) of Boolean with Pack;
   type Pins_32_53 is array (GPIO_Pin range 32 .. 53) of Boolean with Pack;

   --  GPFSELn registers: Alternate functions selectors

   type AF_Selector is
     (Input,
      Output,
      AF5,
      AF4,
      AF0,
      AF1,
      AF2,
      AF3)
     with Size => 3;
   for AF_Selector use
     (Input  => 0,
      Output => 1,
      AF5    => 2,
      AF4    => 3,
      AF0    => 4,
      AF1    => 5,
      AF2    => 6,
      AF3    => 7);

   type Function_Selector_0 is array (GPIO_Pin range 0 .. 9) of AF_Selector
     with Pack;
   type Function_Selector_1 is array (GPIO_Pin range 10 .. 19) of AF_Selector
     with Pack;
   type Function_Selector_2 is array (GPIO_Pin range 20 .. 29) of AF_Selector
     with Pack;
   type Function_Selector_3 is array (GPIO_Pin range 30 .. 39) of AF_Selector
     with Pack;
   type Function_Selector_4 is array (GPIO_Pin range 40 .. 49) of AF_Selector
     with Pack;
   type Function_Selector_5 is array (GPIO_Pin range 50 .. 53) of AF_Selector
     with Pack;

   --  The function select registers are used to define the operation of the
   --  general-purpose I/O pins. Each of the 54 GPIO pins has at least two
   --  alternative functions. The FSEL (n) field determines the functionality
   --  of the nth GPIO pin. All unused alternative function lines are tied to
   --  ground and will output a "0" if selected. All pins reset to normal GPIO
   --  input operation.
   type GPFSEL0_Register is record
      FSel           : Function_Selector_0 := (others => Input);
      Reserved_30_31 : UInt2 := 0;
   end record with Size => 32, Volatile_Full_Access;

   for GPFSEL0_Register use record
      FSel           at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  The function select registers are used to define the operation of the
   --  general-purpose I/O pins. Each of the 54 GPIO pins has at least two
   --  alternative functions. The FSEL (n) field determines the functionality
   --  of the nth GPIO pin. All unused alternative function lines are tied to
   --  ground and will output a "0" if selected. All pins reset to normal GPIO
   --  input operation.
   type GPFSEL1_Register is record
      FSel           : Function_Selector_1 := (others => Input);
      Reserved_30_31 : UInt2 := 0;
   end record with Size => 32, Volatile_Full_Access;

   for GPFSEL1_Register use record
      FSel           at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  The function select registers are used to define the operation of the
   --  general-purpose I/O pins. Each of the 54 GPIO pins has at least two
   --  alternative functions. The FSEL (n) field determines the functionality
   --  of the nth GPIO pin. All unused alternative function lines are tied to
   --  ground and will output a "0" if selected. All pins reset to normal GPIO
   --  input operation.
   type GPFSEL2_Register is record
      FSel           : Function_Selector_2 := (others => Input);
      Reserved_30_31 : UInt2 := 0;
   end record with Size => 32, Volatile_Full_Access;

   for GPFSEL2_Register use record
      FSel           at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  The function select registers are used to define the operation of the
   --  general-purpose I/O pins. Each of the 54 GPIO pins has at least two
   --  alternative functions. The FSEL (n) field determines the functionality
   --  of the nth GPIO pin. All unused alternative function lines are tied to
   --  ground and will output a "0" if selected. All pins reset to normal GPIO
   --  input operation.
   type GPFSEL3_Register is record
      FSel           : Function_Selector_3 := (others => Input);
      Reserved_30_31 : UInt2 := 0;
   end record with Size => 32, Volatile_Full_Access;

   for GPFSEL3_Register use record
      FSel           at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  The function select registers are used to define the operation of the
   --  general-purpose I/O pins. Each of the 54 GPIO pins has at least two
   --  alternative functions. The FSEL (n) field determines the functionality
   --  of the nth GPIO pin. All unused alternative function lines are tied to
   --  ground and will output a "0" if selected. All pins reset to normal GPIO
   --  input operation.
   type GPFSEL4_Register is record
      FSel           : Function_Selector_4 := (others => Input);
      Reserved_30_31 : UInt2 := 0;
   end record with Size => 32, Volatile_Full_Access;

   for GPFSEL4_Register use record
      FSel           at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  The function select registers are used to define the operation of the
   --  general-purpose I/O pins. Each of the 54 GPIO pins has at least two
   --  alternative functions. The FSEL (n) field determines the functionality
   --  of the nth GPIO pin. All unused alternative function lines are tied to
   --  ground and will output a "0" if selected. All pins reset to normal GPIO
   --  input operation.
   type GPFSEL5_Register is record
      FSel           : Function_Selector_5 := (others => Input);
      Reserved_12_31 : UInt20 := 0;
   end record with Size => 32, Volatile_Full_Access;

   for GPFSEL5_Register use record
      FSel           at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  GPSETn: GPIO Pin Output Set Registers

   --  The output set registers are used to set a GPIO pin. The SET (n) field
   --  defines the respective GPIO pin to set, writing a False to the field
   --  has no effect. If the GPIO pin is being used as in input (by default)
   --  then the value in the SET (n) field is ignored. However, if the pin is
   --  subsequently defined as an output then the bit will be set according
   --  to the last set/clear operation. Separating the set and clear functions
   --  removes the need for read-modify-write operations
   type GPSET0_Register is record
      SET : Pins_0_31 := (others => False);
   end record with Size => 32, Volatile;

   type GPSET1_Register is record
      SET            : Pins_32_53 := (others => False);
      Reserved_22_31 : UInt9 := 0;
   end record with Size => 32, Volatile;

   for GPSET1_Register use record
      SET            at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  GPCLRn: GPIO Pin Output Clear Registers

   --  The output clear registers are used to clear a GPIO pin. The CLR (n)
   --  field defines the respective GPIO pin to clear, writing a False to
   --  the field has no effect. If the GPIO pin is being used as in input (by
   --  default) then the value in the CLR (n) field is ignored. However, if
   --  the pin is subsequently defined as an output then the bit will be set
   --  according to the last set/clear operation. Separating the set and clear
   --  functions removes the need for read-modify-write operations.
   type GPCLR0_Register is record
      CLR : Pins_0_31 := (others => False);
   end record with Size => 32, Volatile;

   --  The output clear registers are used to clear a GPIO pin. The CLR (n)
   --  field defines the respective GPIO pin to clear, writing a False to
   --  the field has no effect. If the GPIO pin is being used as in input (by
   --  default) then the value in the CLR (n) field is ignored. However, if
   --  the pin is subsequently defined as an output then the bit will be set
   --  according to the last set/clear operation. Separating the set and clear
   --  functions removes the need for read-modify-write operations.
   type GPCLR1_Register is record
      CLR            : Pins_32_53 := (others => False);
      Reserved_22_31 : UInt9 := 0;
   end record with Size => 32, Volatile;

   for GPCLR1_Register use record
      CLR            at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  GPLEVn: GPIO Pin level

   --  The pin level registers return the actual value of the pin. The LEV (n)
   --  field gives the value of the respective GPIO pin.
   type GPLEV0_Register is record
      LEV : Pins_0_31; --  read-only
   end record with Size => 32, Volatile_Full_Access;

   --  The pin level registers return the actual value of the pin. The LEV (n)
   --  field gives the value of the respective GPIO pin.
   type GPLEV1_Register is record
      LEV            : Pins_32_53; --  read-only
      Reserved_22_31 : UInt9;      --  ignore
   end record with Size => 32, Volatile_Full_Access;

   for GPLEV1_Register use record
      LEV            at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  GPEDSn: GPIO Event Detect Status

   --  The event detect status registers are used to record level and edge
   --  events on the GPIO pins. The relevant bit in the event detect status
   --  registers is set whenever: 1) an edge is detected that matches the type
   --  of edge programmed in the rising/falling edge detect enable registers,
   --  or 2) a level is detected that matches the type of level programmed
   --  in the high/low level detect enable registers. The bit is cleared by
   --  writing a "1" to the relevant bit.
   --
   --  The interrupt controller can be programmed to interrupt the processor
   --  when any of the status bits are set. The GPIO peripheral has three
   --  dedicated interrupt lines. Each GPIO bank can generate an independent
   --  interrupt. The third line generates a single interrupt whenever any bit
   --  is set.
   type GPEDS0_Register is record
      EDS : Pins_0_31; --  read-only
   end record with Size => 32, Volatile_Full_Access;

   --  The event detect status registers are used to record level and edge
   --  events on the GPIO pins. The relevant bit in the event detect status
   --  registers is set whenever: 1) an edge is detected that matches the type
   --  of edge programmed in the rising/falling edge detect enable registers,
   --  or 2) a level is detected that matches the type of level programmed
   --  in the high/low level detect enable registers. The bit is cleared by
   --  writing a "1" to the relevant bit.
   --
   --  The interrupt controller can be programmed to interrupt the processor
   --  when any of the status bits are set. The GPIO peripheral has three
   --  dedicated interrupt lines. Each GPIO bank can generate an independent
   --  interrupt. The third line generates a single interrupt whenever any bit
   --  is set.
   type GPEDS1_Register is record
      EDS            : Pins_32_53; --  read-only
      Reserved_22_31 : UInt9;      --  ignore
   end record with Size => 32, Volatile_Full_Access;

   for GPEDS1_Register use record
      EDS            at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  GPRENn: GPIO Rising Edge Detect Enable Registers

   --  The rising edge detect enable registers define the pins for which a
   --  rising edge transition sets a bit in the event detect status registers
   --  (GPEDSn). When the relevant bits are set in both the GPRENn and GPFENn
   --  registers, any transition (1 to 0 and 0 to 1) will set a bit in the
   --  GPEDSn registers. The GPRENn registers use synchronous edge detection.
   --  This means the input signal is sampled using the system clock and then
   --  it is looking for a "011" pattern on the sampled signal. This has
   --  the effect of suppressing glitches.
   type GPREN0_Register is record
      REN : Pins_0_31; --  read-only
   end record with Size => 32, Volatile_Full_Access;

   --  The rising edge detect enable registers define the pins for which a
   --  rising edge transition sets a bit in the event detect status registers
   --  (GPEDSn). When the relevant bits are set in both the GPRENn and GPFENn
   --  registers, any transition (1 to 0 and 0 to 1) will set a bit in the
   --  GPEDSn registers. The GPRENn registers use synchronous edge detection.
   --  This means the input signal is sampled using the system clock and then
   --  it is looking for a "011" pattern on the sampled signal. This has
   --  the effect of suppressing glitches.
   type GPREN1_Register is record
      REN            : Pins_32_53; --  read-only
      Reserved_22_31 : UInt9;      --  ignore
   end record with Size => 32, Volatile_Full_Access;

   for GPREN1_Register use record
      REN            at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  GPFENn: GPIO Falling Edge Detect Enable Registers

   --  The falling edge detect enable registers define the pins for which a
   --  falling edge transition sets a bit in the event detect status registers
   --  (GPEDSn). When the relevant bits are set in both the GPRENn and GPFENn
   --  registers, any transition (1 to 0 and 0 to 1) will set a bit in the
   --  GPEDSn registers. The GPFENn registers use synchronous edge detection.
   --  This means the input signal is sampled using the system clock and then
   --  it is looking for a "100" pattern on the sampled signal. This has
   --  the effect of suppressing glitches.
   type GPFEN0_Register is record
      FEN : Pins_0_31; --  read-only
   end record with Size => 32, Volatile_Full_Access;

   --  The falling edge detect enable registers define the pins for which a
   --  falling edge transition sets a bit in the event detect status registers
   --  (GPEDSn). When the relevant bits are set in both the GPRENn and GPFENn
   --  registers, any transition (1 to 0 and 0 to 1) will set a bit in the
   --  GPEDSn registers. The GPFENn registers use synchronous edge detection.
   --  This means the input signal is sampled using the system clock and then
   --  it is looking for a "100" pattern on the sampled signal. This has
   --  the effect of suppressing glitches.
   type GPFEN1_Register is record
      FEN            : Pins_32_53; --  read-only
      Reserved_22_31 : UInt9;      --  ignore
   end record with Size => 32, Volatile_Full_Access;

   for GPFEN1_Register use record
      FEN            at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  GPHENn: GPIO High Level Detect Enable Registers

   --  The high level detect enable registers define the pins for which a high
   --  level sets a bit in the event detect status register (GPEDSn). If the
   --  pin is still high when an attempt is made to clear the status bit in
   --  GPEDSn then the status bit will remain set.
   type GPHEN0_Register is record
      HEN : Pins_0_31; --  read-only
   end record with Size => 32, Volatile_Full_Access;

   --  The high level detect enable registers define the pins for which a high
   --  level sets a bit in the event detect status register (GPEDSn). If the
   --  pin is still high when an attempt is made to clear the status bit in
   --  GPEDSn then the status bit will remain set.
   type GPHEN1_Register is record
      HEN            : Pins_32_53; --  read-only
      Reserved_22_31 : UInt9;      --  ignore
   end record with Size => 32, Volatile_Full_Access;

   for GPHEN1_Register use record
      HEN            at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  GPLENn: GPIO Low Level Detect Enable Registers

   --  The low level detect enable registers define the pins for which a low
   --  level sets a bit in the event detect status register (GPEDSn). If the
   --  pin is still low when an attempt is made to clear the status bit in
   --  GPEDSn then the status bit will remain set.
   type GPLEN0_Register is record
      LEN : Pins_0_31; --  read-only
   end record with Size => 32, Volatile_Full_Access;

   --  The low level detect enable registers define the pins for which a low
   --  level sets a bit in the event detect status register (GPEDSn). If the
   --  pin is still low when an attempt is made to clear the status bit in
   --  GPEDSn then the status bit will remain set.
   type GPLEN1_Register is record
      LEN            : Pins_32_53; --  read-only
      Reserved_22_31 : UInt9;      --  ignore
   end record with Size => 32, Volatile_Full_Access;

   for GPLEN1_Register use record
      LEN            at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  GPARENn: GPIO Async Falling Edge Detect Enable Registers

   --  The asynchronous rising edge detect enable registers define the pins
   --  for which a asynchronous rising edge transition sets a bit in the event
   --  detect status registers (GPEDSn).
   --
   --  Asynchronous means the incoming signal is not sampled by the system
   --  clock. As such rising edges of very short duration can be detected.
   type GPAREN0_Register is record
      AREN : Pins_0_31; --  read-only
   end record with Size => 32, Volatile_Full_Access;

   --  The asynchronous rising edge detect enable registers define the pins
   --  for which a asynchronous rising edge transition sets a bit in the event
   --  detect status registers (GPEDSn).
   --
   --  Asynchronous means the incoming signal is not sampled by the system
   --  clock. As such rising edges of very short duration can be detected.
   type GPAREN1_Register is record
      AREN            : Pins_32_53; --  read-only
      Reserved_22_31 : UInt9;      --  ignore
   end record with Size => 32, Volatile_Full_Access;

   for GPAREN1_Register use record
      AREN            at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  GPAFENn: GPIO Async Rising Edge Detect Enable Registers

   --  The asynchronous falling edge detect enable registers define the pins
   --  for which a asynchronous falling edge transition sets a bit in the event
   --  detect status registers (GPEDSn). Asynchronous means the incoming signal
   --  is not sampled by the system clock. As such falling edges of very short
   --  duration can be detected.
   --
   --  Asynchronous means the incoming signal is not sampled by the system
   --  clock. As such rising edges of very short duration can be detected.
   type GPAFEN0_Register is record
      AFEN : Pins_0_31; --  read-only
   end record with Size => 32, Volatile_Full_Access;

   --  The asynchronous falling edge detect enable registers define the pins
   --  for which a asynchronous falling edge transition sets a bit in the event
   --  detect status registers (GPEDSn). Asynchronous means the incoming signal
   --  is not sampled by the system clock. As such falling edges of very short
   --  duration can be detected.
   --
   --  Asynchronous means the incoming signal is not sampled by the system
   --  clock. As such rising edges of very short duration can be detected.
   type GPAFEN1_Register is record
      AFEN            : Pins_32_53; --  read-only
      Reserved_22_31 : UInt9;      --  ignore
   end record with Size => 32, Volatile_Full_Access;

   for GPAFEN1_Register use record
      AFEN            at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  GPPUD: GPIO Pull-up/down register

   type Pull_Up_Down_Control is
     (Off,
      --  Use in conjunction with GPPUDCLK0/1
      Pull_Down,
      --  Use in conjunction with GPPUDCLK0/1
      Pull_Up)
     with Size => 2;

   --  The GPIO Pull-up/down Register controls the actuation of the internal
   --  pull-up/down control line to ALL the GPIO pins. This register must be
   --  used in conjunction with the 2 GPPUDCLKn registers.
   --
   --  Note that it is not possible to read back the current Pull-up/down
   --  settings and so it is the users' responsibility to 'remember' which
   --  pull-up/downs are active. The reason for this is that GPIO pull-ups
   --  are maintained even in power-down mode when the core is off, when all
   --  register contents is lost.
   --
   --  The Alternate function table also has the pull state which is applied
   --  after a power down.
   type GPPUD_Register is record
      PUD      : Pull_Up_Down_Control := Off;
      Reserved : UInt30 := 0;
   end record;

   --  The GPIO Pull-up/down Clock Registers control the actuation of internal
   --  pull-downs on the respective GPIO pins. These registers must be used in
   --  conjunction with the GPPUD register to effect GPIO Pull-up/down changes.
   --  The following sequence of events is required:
   --
   --  1. Write to GPPUD to set the required control signal (i.e. Pull-up or
   --     Pull-Down or neither to remove the current Pull-up/down)
   --  2. Wait 150 cycles - this provides the required set-up time for the
   --     control signal
   --  3. Write to GPPUDCLK0/1 to clock the control signal into the GPIO pads
   --     you wish to modify - NOTE only the pads which receive a clock will be
   --     modified, all others will retain their previous state.
   --  4. Wait 150 cycles - this provides the required hold time for the
   --     control signal
   --  5. Write to GPPUD to remove the control signal
   --  6. Write to GPPUDCLK0/1 to remove the clock
   type GPPUDCLK0_Register is record
      PUDCLK : Pins_0_31; --  read-only
   end record with Size => 32, Volatile_Full_Access;

   --  The GPIO Pull-up/down Clock Registers control the actuation of internal
   --  pull-downs on the respective GPIO pins. These registers must be used in
   --  conjunction with the GPPUD register to effect GPIO Pull-up/down changes.
   --  The following sequence of events is required:
   --
   --  1. Write to GPPUD to set the required control signal (i.e. Pull-up or
   --     Pull-Down or neither to remove the current Pull-up/down)
   --  2. Wait 150 cycles - this provides the required set-up time for the
   --     control signal
   --  3. Write to GPPUDCLK0/1 to clock the control signal into the GPIO pads
   --     you wish to modify - NOTE only the pads which receive a clock will be
   --     modified, all others will retain their previous state.
   --  4. Wait 150 cycles - this provides the required hold time for the
   --     control signal
   --  5. Write to GPPUD to remove the control signal
   --  6. Write to GPPUDCLK0/1 to remove the clock
   type GPPUDCLK1_Register is record
      PUDCLK            : Pins_32_53; --  read-only
      Reserved_22_31 : UInt9;      --  ignore
   end record with Size => 32, Volatile_Full_Access;

   for GPPUDCLK1_Register use record
      PUDCLK            at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   type GPIO_Peripheral is record
      GPFSEL0   : GPFSEL0_Register;
      GPFSEL1   : GPFSEL1_Register;
      GPFSEL2   : GPFSEL2_Register;
      GPFSEL3   : GPFSEL3_Register;
      GPFSEL4   : GPFSEL4_Register;
      GPFSEL5   : GPFSEL5_Register;
      Pad_18    : UInt32;

      GPSET0    : GPSET0_Register;
      GPSET1    : GPSET1_Register;
      Pad_24    : UInt32;

      GPCLR0    : GPCLR0_Register;
      GPCLR1    : GPCLR1_Register;
      Pad_30    : UInt32;

      GPLEV0    : GPLEV0_Register;
      GPLEV1    : GPLEV1_Register;
      Pad_3c    : UInt32;

      GPEDS0    : GPEDS0_Register;
      GPEDS1    : GPEDS1_Register;
      Pad_48    : UInt32;

      GPREN0    : GPREN0_Register;
      GPREN1    : GPREN1_Register;
      Pad_54    : UInt32;

      GPFEN0    : GPFEN0_Register;
      GPFEN1    : GPFEN1_Register;
      Pad_60    : UInt32;

      GPHEN0    : GPHEN0_Register;
      GPHEN1    : GPHEN1_Register;
      Pad_6c    : UInt32;

      GPLEN0    : GPLEN0_Register;
      GPLEN1    : GPLEN1_Register;
      Pad_78    : UInt32;

      GPAREN0   : GPAREN0_Register;
      GPAREN1   : GPAREN1_Register;
      Pad_84    : UInt32;

      GPAFEN0   : GPAFEN0_Register;
      GPAFEN1   : GPAFEN1_Register;
      Pad_90    : UInt32;

      GPPUD     : GPPUD_Register;
      GPPUDCLK0 : GPPUDCLK0_Register;
      GPPUDCLK1 : GPPUDCLK1_Register;
   end record;

   GPIO_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address (GPIO_Base);

end RPi.Regs.GPIO;
