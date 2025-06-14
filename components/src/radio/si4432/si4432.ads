------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2025, AdaCore                             --
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

--  Driver for Si4432 chip, actualy on Si4430/31/32 Rev B1

with HAL;       use HAL;
with HAL.GPIO;  use HAL.GPIO;
with HAL.SPI;   use HAL.SPI;

package Si4432 is
   pragma Extensions_Allowed (On);

   type Register is new UInt8;

   type On_Off is (Off, On);
   type Chip_State is (Idle, RX, TX) with Size => UInt2'Size;

   type Frequency is delta 0.1 range 240.0 .. 960.0
     with Small => 0.1; -- in MHz

   type Frequency_Offset                is range -16#1FF# .. 16#1FF#;
   type Frequency_Deviation_Hz          is range        0 .. 319375
     with Dynamic_Predicate => Frequency_Deviation_Hz rem 625 = 0; -- in Hz
   type Wake_Up_Timer_Period            is range        1 .. 16#FFFF#;
   type Clock_Recovery_Offset           is range        0 .. 16#FF7FF#;
   type Clock_Recovery_Oversampling     is range        0 .. 16#7FF#;
   type Clock_Recovery_Timing_Loop_Gain is range        0 .. 16#7FF#;
   type OOK_Counter                     is range        0 .. 16#7FF#;
   type AFC_Correction                  is range        0 .. 16#3FF#;
   type FIFO_Threshold                  is range        0 .. 16#3F#;
   type Low_Duty_Cycle_Mode_Duration    is range        1 .. 16#FF#;
   type ADC8_Control                    is range        0 .. 16#20#;
   type ADC_Sensor_Amplifier_Offset     is range       -7 .. 7;
   type Wake_Up_Timer_Exponent          is range        0 .. 20;

   type Modulation_Mode is
     (Unmodulated_Carrier,
      OOK,
      FSK,
      GFSK --  (enable TX Data CLK (TX_Data_Clock_Configuration) when direct
      --        mode is used)
     ) with Size => UInt2'Size;

   type Modulation_Source is
     (Direct_GPIO,
      --  Direct Mode using TX_Data function via the GPIO pin (one of the
      --  GPIO's should be programmed accordingly as well)
      Direct_SDI,
      --  Direct Mode using TX_Data function via the SDI pin (only
      --  when nSEL is high)
      FIFO,
      --  FIFO Mode
      PN9
      --  PN9 (internally generated)
     ) with Size => UInt2'Size;

   type Modulation_TX_Data_Clock is
     (No_CLK,
      --  No TX Data CLK is available (asynchronous mode - Can only work
      --  with modulations FSK or OOK).
      CLK_GPIO,
      --  TX Data CLK is available via the GPIO (one of the GPIO-s should
      --  be programmed as well).
      CLK_SDO,
      --  TX Data CLK is available via the SDO pin.
      CLK_nIRQ
      --  TX Data CLK is available via the nIRQ pin.
     ) with Size => UInt2'Size;

   type Synchronization_Word_Length is
     (Word_3, Word_3_2, Word_3_2_1, Word_3_2_1_0) with Size => UInt2'Size;

   for Synchronization_Word_Length use
     (Word_3       => 2#00#,
      Word_3_2     => 2#01#,
      Word_3_2_1   => 2#10#,
      Word_3_2_1_0 => 2#11#);

   type Header_Length is
     (No_Header, Header_3, Header_3_2, Header_3_2_1, Header_3_2_1_0)
     with Size => UInt3'Size;

   for Header_Length use
     (No_Header      => 2#000#,
      Header_3       => 2#001#,
      Header_3_2     => 2#010#,
      Header_3_2_1   => 2#011#,
      Header_3_2_1_0 => 2#100#);

   type CRC_Polynomial is
     (CCITT,
      CRC_16, --  (IBM)
      IEC_16,
      Biacheva) with Size => UInt2'Size;

   type GPIO_0_Function is
     (Power_On_Reset,
      Wake_Up_Timer,
      Low_Battery_Detect,
      Direct_Digital_Input,
      Interrupt_Falling_Edge,
      Interrupt_Rising_Edge,
      Interrupt_State_Change,
      ADC_Analog_Input,
      Reserved_Analog_Test_N_Input,
      Reserved_Analog_Test_P_Input,
      Direct_Digital_Output,
      Reserved_Digital_Test_Output,
      Reserved_Analog_Test_N_Output,
      Reserved_Analog_Test_P_Output,
      Reference_Voltage,
      Data_CLK_Output,
      TX_Data_Input,
      External_Retransmission_Request,
      TX_State,
      TX_FIFO_Almost_Full,
      RX_Data,
      RX_State,
      RX_FIFO_Almost_Full,
      Antenna_1_Switch,
      Antenna_2_Switch,
      Valid_Preamble_Detected,
      Invalid_Preamble_Detected,
      Sync_Word_Detected,
      Clear_Channel_Assessment,
      VDD,
      GND0, GND1
     ) with Size => UInt5'Size;

   type GPIO_1_Function is
     (Inverted_Power_On_Reset,
      Wake_Up_Timer,
      Low_Battery_Detect,
      Direct_Digital_Input,
      Interrupt_Falling_Edge,
      Interrupt_Rising_Edge,
      Interrupt_State_Change,
      ADC_Analog_Input,
      Reserved_Analog_Test_N_Input,
      Reserved_Analog_Test_P_Input,
      Direct_Digital_Output,
      Reserved_Digital_Test_Output,
      Reserved_Analog_Test_N_Output,
      Reserved_Analog_Test_P_Output,
      Reference_Voltage,
      Data_CLK_Output,
      TX_Data_Input,
      External_Retransmission_Request,
      TX_State,
      TX_FIFO_Almost_Full,
      RX_Data,
      RX_State,
      RX_FIFO_Almost_Full,
      Antenna_1_Switch,
      Antenna_2_Switch,
      Valid_Preamble_Detected,
      Invalid_Preamble_Detected,
      Sync_Word_Detected,
      Clear_Channel_Assessment,
      VDD,
      GND0, GND1
     ) with Size => UInt5'Size;

   type GPIO_2_Function is
     (Microcontroller_Clocks,
      Wake_Up_Timer,
      Low_Battery_Detect,
      Direct_Digital_Input,
      Interrupt_Falling_Edge,
      Interrupt_Rising_Edge,
      Interrupt_State_Change,
      ADC_Analog_Input,
      Reserved_Analog_Test_N_Input,
      Reserved_Analog_Test_P_Input,
      Direct_Digital_Output,
      Reserved_Digital_Test_Output,
      Reserved_Analog_Test_N_Output,
      Reserved_Analog_Test_P_Output,
      Reference_Voltage,
      Data_CLK_Output,
      TX_Data_Input,
      External_Retransmission_Request,
      TX_State,
      TX_FIFO_Almost_Full,
      RX_Data,
      RX_State,
      RX_FIFO_Almost_Full,
      Antenna_1_Switch,
      Antenna_2_Switch,
      Valid_Preamble_Detected,
      Invalid_Preamble_Detected,
      Sync_Word_Detected,
      Clear_Channel_Assessment,
      VDD,
      GND0, GND1
     ) with Size => UInt5'Size;

   type AGC_Override_Gain is range 0 .. 24 with
     Static_Predicate => AGC_Override_Gain in 0 | 3 | 6 | 12 | 24;

   type Crystal_Oscillator is (RC_Oscillator, Crystal_32kHz)
     with Size => Bit'Size;
   for Crystal_Oscillator use
     (RC_Oscillator => 0, Crystal_32kHz => 1);

   type Microcontroller_Clock is
     (c30MHz, c15MHz, c10MHz, c4MHz, c3MHz, c2MHz, c1MHz, c32kHz)
     with Size => UInt3'Size;

   type Clock_Tail is range 0 .. 512 with
     Static_Predicate => Clock_Tail in 0 | 128 | 256 | 512;

   type ADC_Input_Source is
     (Internal_Temperature_Sensor,
      GPIO0_Single_Ended,
      GPIO1_Single_Ended,
      GPIO2_Single_Ended,
      GPIO0_GPIO1_differential, --  GPIO0(+) - GPIO1(-), differential
      GPIO1_GPIO2_differential, --  GPIO1(+) - GPIO2(-), differential
      GPIO0_GPIO2_differential, --  GPIO0(+) - GPIO2(-), differential
      GND) with Size => UInt3'Size;

   type Temperature_Sensor_Range is
     (From_Minus_64_To_64C,
      --  -64 to 64C (full operating range), with 0.5C resolution
      --    (1 LSB in the 8-bit ADC)
      From_Minus_64_To_192C,
      --  -64 to 192C, with 1C resolution (1 LSB in the 8-bit ADC)
      From_Minus_40_To_216F,
      --  -40 to 216F, with 1F resolution (1 LSB in the 8-bit ADC)
      From_0_To_12C
      --   0 to 12C, with 0.5C resolution (1 LSB in the 8-bit ADC)
     ) with Size => UInt2'Size;

   type Internal_Chip_State is (LP, RDY, TX, Tune, RX)
     with Size => UInt3'Size;

   subtype Interrupt_Statuses is HAL.SPI.SPI_Data_8b (1 .. 2);

   -------------------
   -- Si4432_Driver --
   -------------------

   type Si4432_Driver is limited private;

   procedure Initialize
     (This    : out Si4432_Driver;
      CSN_Pin : HAL.GPIO.Any_GPIO_Point;
      SDN_Pin : HAL.GPIO.Any_GPIO_Point;
      SPI     : HAL.SPI.Any_SPI_Port);

   function Device_Type_Code (This : Si4432_Driver) return UInt8;
   --  Default 16#8#

   function Version_Code (This : Si4432_Driver) return UInt8;
   --  Code indicating the version of the chip.
   --  Si4430/31/32 Rev B1: 16#6#.
   --  Si100x Rev C, Si101x Rev A, Si102x/3x Rev A: 16#6#.
   --  Si100x Rev E, Si101x Rev B: Si102x/3x Rev B: 16#7#.

   procedure Set_Power
     (This : Si4432_Driver;
      Mode : On_Off);

   procedure Set_State
     (This  : Si4432_Driver;
      State : Chip_State);

   procedure Set_TX_Power
     (This         : Si4432_Driver;
      Output_Power : UInt3;
      --  Default 0
      --  The output power is configurable from +13 dBm to -8 dBm (Si4430/31),
      --  and from +20 dBM to -1 dBM (Si4432) in ~3 dB steps. 0 corresponds to
      --  min output power, while 7 corresponds to max output power.

      LNA_Switch   : Boolean);
      --  Default True
      --  This determines when internal MOS switches at the LNA input(s) are
      --  invoked. When False, these switches open. When True, these switches
      --  are closed in TX mode and open at all other times. This bit MUST
      --  be set for proper operation in any Direct Tie application.

   procedure Get_TX_Power
     (This         : Si4432_Driver;
      Output_Power : out UInt3;
      LNA_Switch   : out Boolean);

   procedure Get_Device_Status
     (This                 : Si4432_Driver;
      Power                : out Chip_State;
      Frequency_Error      : out Boolean;
      --  The programmed frequency is outside of the operating range. The actual frequency is
      --  saturated to the max/min value.

      Header_Error         : out Boolean;
      --  Indicates if the received packet has a header check error.

      RX_FIFO_Empty        : out Boolean;
      RX_TX_FIFO_Underflow : out Boolean;
      RX_TX_FIFO_Overflow  : out Boolean);

   procedure Get_EZMAC_Status
     (This                  : Si4432_Driver;
      Packet_Sent           : out Boolean;
      --  When True a packet has been sent by the radio. (Same bit as in
      --  Interrupt_Status_1, but reading it does not reset the IRQ)

      Packet_Transmitting   : out Boolean;
      --  When True the radio is currently transmitting a packet.

      CRC_Error             : out Boolean;
      --  When True a Cyclic Redundancy Check error has been detected. (Same
      --  bit as in Interrupt_Status_1, but reading it does not reset the IRQ)

      Valid_Packet_Received : out Boolean;
      --  When True a valid packet has been received by the receiver. (Same
      --  bit as in Interrupt_Status_1, but reading it does not reset the IRQ)

      Packet_Receiving      : out Boolean;
      --  When True the radio is currently receiving a valid packet.

      Packet_Searching      : out Boolean;
      --  When True the radio is searching for a valid packet.

      Underflow_CRC         : out Boolean);
      --  May indicated Transmitter underflow in case of CRC error.

   procedure Software_Reset (This : Si4432_Driver);
   --  The user should wait until the CHIPRDY status flag/interrupt is issued
   --  before sending further SPI commands to the chip.

   ----------------
   -- Interrupts --
   ----------------

   function Get_Interrupt_Statuses
     (This : Si4432_Driver)
      return Interrupt_Statuses;
   --  Returns Interrupt_Statuses
   --            (1 => Interrupt_Status_1, 2 => Interrupt_Status_2)

   function Get_Interrupt_Status_1
     (This : Si4432_Driver)
      return UInt8;
   --  Reading statuses clears the interrupt flags and resets the NIRQ output
   --  When any of the Interrupt/Status 1 register bits change state from 0
   --  to 1 the device will notify the microcontroller by setting the nIRQ pin
   --  LOW = 0 if the corresponding enable bit is set in the Interrupt Enable 1
   --  register. The nIRQ pin will go to HIGH and all the enabled interrupt
   --  bits will be cleared when the microcontroller reads this address. If any
   --  of these bits are not enabled in the Interrupt Enable 1 register, they
   --  then become status signals that can be read at any time. They WILL NOT
   --  be cleared by reading the register.

   Status_CRC_Error               : constant := 2#00000001#;
   --  When set to 1 the cyclic redundancy check is failed.
   Status_Valid_Packet_Received   : constant := 2#00000010#;
   --  When set to 1 a valid packet has been received.
   Status_Packet_Sent             : constant := 2#00000100#;
   --  When set to1 a valid packet has been transmitted.
   Status_External_Interrupt      : constant := 2#00001000#;
   --  When set to 1 an interrupt occurred on one of the GPIO's if it is
   --  programmed so. The status can be checked in register 0Eh. See
   --  GPIOx Configuration section for the details.
   Status_RX_FIFO_Almost_Full     : constant := 2#00010000#;
   --  When set to 1 the RX FIFO has met its almost full threshold and needs
   --  to be read by the microcontroller.
   Status_TX_FIFO_Almost_Empty    : constant := 2#00100000#;
   --  When set to 1 the TX FIFO is almost empty and needs to be filled.
   Status_TX_FIFO_Almost_Full     : constant := 2#01000000#;
   --  When set to 1 the TX FIFO has met its almost full threshold and
   --  needs to be transmitted.
   Status_FIFO_Underflow_Overflow : constant := 2#10000000#;
   --  When set to 1 the TX or RX FIFO has overflowed or underflowed.

   function Get_Interrupt_Status_2
     (This : Si4432_Driver)
      return UInt8;
   --  Reading statuses clears the interrupt flags and resets the NIRQ output

   Status_Power_On_Reset            : constant := 2#00000001#;
   --  When the chip detects a Power on Reset above the desired setting
   --  this bit will be set to 1.
   Status_Chip_Ready                : constant := 2#00000010#;
   --  When a chip ready event has been detected this bit will be set to 1.
   Status_Low_Battery_Detect        : constant := 2#00000100#;
   --  When a low battery event has been detected this bit will be set to 1.
   --  This interrupt event is saved even if it is not enabled by the mask
   --  register bit and causes an interrupt after it is enabled.
   Status_Wake_Up_Timer             : constant := 2#00001000#;
   --  On the expiration of programmed wake-up timer this bit will be set
   --  to 1.
   Status_RSSI                      : constant := 2#00010000#;
   --  When RSSI level exceeds the programmed threshold this bit will be
   --  set to 1.
   Status_Invalid_Preamble_Detected : constant := 2#00100000#;
   --  When the preamble is not found within a period of time set by the
   --  invalid preamble detection threshold in Register 60h, this bit will
   --  be set to 1.
   Status_Valid_Preamble_Detected   : constant := 2#01000000#;
   --  When a preamble is detected this bit will be set to 1.
   Status_Sync_Word_Detected        : constant := 2#10000000#;
   --  When a sync word is detected this bit will be set to 1.

   procedure Clear_Interrupts (This : Si4432_Driver);

   procedure Set_Interrupt_Enable
     (This                    : Si4432_Driver;
      CRC_Error               : Boolean := False;
      Valid_Packet_Received   : Boolean := False;
      Packet_Sent             : Boolean := False;
      External_Interrupt      : Boolean := False;
      RX_FIFO_Almost_Full     : Boolean := False;
      TX_FIFO_Almost_Empty    : Boolean := False;
      TX_FIFO_Almost_Full     : Boolean := False;
      FIFO_Underflow_Overflow : Boolean := False;
      POR                     : Boolean := True;
      Chip_Ready              : Boolean := True;
      Low_Battery             : Boolean := False;
      Wake_Up_Timer           : Boolean := False;
      RSSI                    : Boolean := False;
      Invalid_Preamble        : Boolean := False;
      Valid_Preamble          : Boolean := False;
      Sync_Word               : Boolean := False);
   --  Uses Set_Interrupt_Enable_1 and Set_Interrupt_Enable_2. See below.

   procedure Set_Interrupt_Enable_1
     (This                    : Si4432_Driver;
      CRC_Error               : Boolean;
      --  When set to True the CRC Error interrupt will be enabled.
      Valid_Packet_Received   : Boolean;
      --  When True the Valid Packet Received Interrupt will be enabled.
      Packet_Sent             : Boolean;
      --  When True the Packet Sense Interrupt will be enabled.
      External_Interrupt      : Boolean;
      --  When set to True the External Interrupt will be enabled.
      RX_FIFO_Almost_Full     : Boolean;
      --  When set to True the RX FIFO Almost Full interrupt will be enabled.
      TX_FIFO_Almost_Empty    : Boolean;
      --  When set to True the TX FIFO Almost Empty interrupt will be enabled.
      TX_FIFO_Almost_Full     : Boolean;
      --  When set to True the TX FIFO Almost Full interrupt will be enabled.
      FIFO_Underflow_Overflow : Boolean);
      --  When set to True the FIFO Underflow/Overflow interrupt will be
      --  enabled.
      --  Default: False all

   procedure Get_Interrupt_Enable_1
     (This                    : Si4432_Driver;
      CRC_Error               : out Boolean;
      Valid_Packet_Received   : out Boolean;
      Packet_Sent             : out Boolean;
      External_Interrupt      : out Boolean;
      RX_FIFO_Almost_Full     : out Boolean;
      TX_FIFO_Almost_Empty    : out Boolean;
      TX_FIFO_Almost_Full     : out Boolean;
      FIFO_Underflow_Overflow : out Boolean);

   procedure Set_Interrupt_Enable_2
     (This             : Si4432_Driver;
      POR              : Boolean;
      --  When set to True the POR interrupt will be enabled.
      Chip_Ready       : Boolean;
      --  When set to True the Chip Ready interrupt will be enabled.
      Low_Battery      : Boolean;
      --  When set to True the Low Battery Detect interrupt will be enabled.
      Wake_Up_Timer    : Boolean;
      --  When set to True the Wake-Up Timer interrupt will be enabled.
      RSSI             : Boolean;
      --  When set to True the RSSI Interrupt will be enabled.
      Invalid_Preamble : Boolean;
      --  When set to True the Invalid Preamble Detected Interrupt will be
      --  enabled.
      Valid_Preamble   : Boolean;
      --  When set to True the Valid Preamble Detected Interrupt will be
      --  enabled.
      Sync_Word        : Boolean);
      --  When set to True the Syn Word Detected Interrupt will be enabled.
   --  Default: POR & Chip Read are True

   procedure Get_Interrupt_Enable_2
     (This             : Si4432_Driver;
      POR              : out Boolean;
      Chip_Ready       : out Boolean;
      Low_Battery      : out Boolean;
      Wake_Up_Timer    : out Boolean;
      RSSI             : out Boolean;
      Invalid_Preamble : out Boolean;
      Valid_Preamble   : out Boolean;
      Sync_Word        : out Boolean);

   -------------------------------
   --  The RF carrier frequency --
   -------------------------------

   procedure Set_Frequency
     (This  : Si4432_Driver;
      Value : Frequency);
   --  Calculate (formula below) and set frequency by setting Frequency_Band
   --    and Nominal_Carrier_Frequency.
   --  Assumes that Frequency_Hopping_Channel_Select = 0.

   --  The RF carrier frequency can be calculated as follows:
   --  fcarrier = (Frequency_Band.Band + 24 +
   --               (Nominal_Carrier_Frequency + Frequency_Offset) / 64000) x
   --    10000 x (Frequency_Band.High_Band + 1) +
   --    (Frequency_Hopping_Channel_Select x Frequency_Hopping_Step_Size x 10)
   --  [kHz]

   procedure Set_Frequency_Band
     (This      : Si4432_Driver;
      Band      : UInt5;
      --  Default 16#15#
      --  Every increment corresponds to a 10 MHz increase in frequency
      --  (when not High_Band) or a 20 MHz increase in frequency
      --  (when High_Band).
      --  Example: Setting Band=0 will result in tuning within the 240-250 MHz
      --  frequency range (for not High_Band) or within the 480-500 MHz
      --  frequency range (for High_Band). Setting Band=1 will result in tuning
      --  within the 250-260 MHz frequency range (not High_Band) or 500-520 MHz
      --  range (High_Band), and so on.

      High_Band : Boolean;
      --  Default True
      --  Setting High_Band will choose the frequency range from 480-960 MHz
      --  (high bands). Setting High_Band=False will choose the frequency range
      --  from 240-479.9 MHz (low bands).

      Side_Band : Boolean);
      --  Default True
      --  Setting Side_Band (recommended setting) will result in tuning the
      --  RX LO below the desired channel frequency in RX mode (low-side
      --  injection) such that the high-side sideband is selected. Note that
      --  setting Side_Band = False will result in positioning the RX LO above
      --  the desired tuned frequency (high-side injection), but will NOT
      --  additionally flip the processing of the complex (I + jQ) signals in
      --  the IF chain necessary to select the lower sideband as the desired
      --  signal.

   procedure Get_Frequency_Band
     (This      : Si4432_Driver;
      Band      : out UInt5;
      High_Band : out Boolean;
      Side_Band : out Boolean);

   procedure Set_Nominal_Carrier_Frequency
     (This  : Si4432_Driver;
      Value : UInt16);
   --  See formula above.
   --  Default 16#BB80#

   function Get_Nominal_Carrier_Frequency
     (This : Si4432_Driver)
      return UInt16;

   procedure Set_Frequency_Offset
     (This  : Si4432_Driver;
      Value : Frequency_Offset);
   --  The frequency offset can be calculated as
   --    Offset = 156.25 Hz x (Frequency_Band.High_Band + 1) x Frequency_Offset
   --  Default 16#0#

   function Get_Frequency_Offset
     (This : Si4432_Driver)
      return Frequency_Offset;

   procedure Set_Frequency_Hopping_Channel_Select
     (This  : Si4432_Driver;
      Value : UInt8);
   --  Frequency Hopping Step Size in 10 kHz Increments.
   --  See formula for the nominal carrier frequency - fcarrier.
   --  Important: The EZHop method of frequency programming only works while
   --  remaining entirely within one of the following defined frequency
   --  sub-bands: 240-320 MHz, 320-480 MHz, 480-640 MHz, and 640-960 MHz.
   --  It is not allowed to define a base frequency that falls in one
   --  sub-band while the selected channel number falls in another sub-band.
   --  Default 16#0#

   function Get_Frequency_Hopping_Channel_Select
     (This : Si4432_Driver)
      return UInt8;

   procedure Set_Frequency_Hopping_Step_Size
     (This  : Si4432_Driver;
      Value : UInt8);
   --  Frequency Hopping Step Size in 10 kHz Increments.
   --  See formula for the nominal carrier frequency - fcarrier.
   --  Important: The EZHop method of frequency programming only works while
   --  remaining entirely within one of the following defined frequency
   --  sub-bands: 240-320 MHz, 320-480 MHz, 480-640 MHz, and 640-960 MHz.
   --  It is not allowed to define a base frequency that falls in one sub-band
   --  while the selected channel number falls in another sub-band.
   --  Default 16#0#

   function Get_Frequency_Hopping_Step_Size
     (This : Si4432_Driver)
      return UInt8;

   procedure Set_Frequency_Deviation_Hz
     (This  : Si4432_Driver;
      Value : Frequency_Deviation_Hz);
   --  Calculate (formula below) and set frequency deviation

   procedure Set_Frequency_Deviation
     (This  : Si4432_Driver;
      Value : UInt9);
      --  Default: 16#20#
      --  The frequency deviation can be calculated:
      --    FD = 625 Hz x Frequency_Deviation.
      --  Note: It's recommended to use modulation index of 1 or higher
      --   (maximum allowable modulation index is 32). The modulation index
      --   is defined by 2FD/RB were FD is the deviation and RB is the data
      --   rate. When Manchester coding is enabled the modulation index is
      --   defined by FD/RB.

   function Get_Frequency_Deviation
     (This : Si4432_Driver)
      return UInt9;

   ---------------
   -- Data_Rate --
   ---------------

   procedure Set_TX_Data_Rate
     (This  : Si4432_Driver;
      Value : UInt16);
   --  The data rate can be calculated as:
   --   106 x TX_Data_Rate/2^16 [bps] (if Data_Rates_Below is False) or
   --   106 x TX_Data_Rate/2^21 [bps] (if Data_Rates_Below is True)
   --  Defaults = 16#A3D# (40 kbps)

   function Get_TX_Data_Rate
     (This : Si4432_Driver)
      return UInt16;

   procedure Set_Data_Rates_Below
     (This  : Si4432_Driver;
      Value : Boolean);
   --  See Modulation_Mode_Control_1.Data_Rates_Below

   --------------
   -- Preamble --
   --------------

   procedure Set_Preamble_Length
     (This  : Si4432_Driver;
      Value : UInt9);
   --  Preamble Length. Default 16#8#
   --  The value corresponds to the number of nibbles (4 bits) in the packet.
   --  For example '000001000' corresponds to a preamble length of 32 bits
   --  (8 x 4bits) or 4 bytes. The maximum preamble length is '111111111'
   --  which corresponds to a 255 bytes Preamble. Writing 0 will have the
   --  same result as if writing 1, which corresponds to one single nibble
   --  of preamble.

   function Get_Preamble_Length
     (This : Si4432_Driver)
      return UInt9;

   procedure Set_Preamble_Detection_Control
     (This                : Si4432_Driver;
      RSSI_Offset         : UInt3;
      --  Default 16#2#
      --  Value added as offset to RSSI calculation. Every increment in this
      --  register results in an increment of +4 dB in the RSSI.

      Detection_Threshold : UInt5);
      --  Default 16#5#
      --  Preamble Detection Threshold. The value corresponds to the number
      --  of nibbles (4 bits) of preamble pattern (i.e., 01010...) that must
      --  be received correctly, before a PREAMBLE_VALID signal is issued.
      --  This threshold helps guard against false preamble detection upon
      --  noise.

   procedure Get_Preamble_Detection_Control
     (This                : Si4432_Driver;
      RSSI_Offset         : out UInt3;
      Detection_Threshold : out UInt5);

   --------------------------
   -- Synchronization_Word --
   --------------------------

   procedure Set_Synchronization_Word
     (This  : Si4432_Driver;
      Index : UInt2;
      Value : UInt8);
   --  Default:
   --  3 : 16#2D#
   --  2 : 16#D4#
   --  1 : 16#00#
   --  0 : 16#00#

   function Get_Synchronization_Word
     (This  : Si4432_Driver;
      Index : UInt2)
      return UInt8;

   procedure Set_Data_Access_Control
     (This                              : Si4432_Driver;
      CRC_Selection                     : CRC_Polynomial;
      --  Default CRC_16

      CRC_Enable                        : Boolean;
      --  Default True
      --    Cyclic Redundancy Check generation is enabled if this bit is set.

      Enable_Packet_TX_Handling         : Boolean;
      --  Default True
      --    If FIFO Mode (Modulation_Mode_Control_2.Modulation_Source = FIFO)
      --    is being used automatic packet handling may be enabled. Setting
      --    True will enable automatic packet handling in the TX path.
      --    Register 30-4D allow for various configurations of the packet
      --    structure. Setting False will not do any packet handling in the
      --    TX path. It will only transmit what is loaded to the FIFO.

      Skip_2nd_Phase_Preamble_Detection : Boolean;
      --  Default False
      --    If set, we skip the second phase of the preamble detection (under
      --    certain conditions) if antenna diversity is enabled.

      CRC_Data_Only_Enable              : Boolean;
      --  Default False
      --    When this bit is set to 1 the CRC is calculated on and checked
      --    against the packet data fields only.

      LSB_First_Enable                  : Boolean;
      --  Default False
      --    The LSB of the data will be transmitted/received first if this bit
      --    is set.

      Enable_Packet_RX_Handling         : Boolean);
      --  Default True
      --    If FIFO Mode (Modulation_Mode_Control_2.Modulation_Source = FIFO)
      --    is being used automatic packet handling may be enabled. Setting
      --    True will enable automatic packet handling in the RX path. Register
      --    30-4D allow for various configurations of the packet structure.
      --    Setting False will not do any packet handling in the RX path. It
      --    will only receive everything after the sync word and fill up the
      --    RX FIFO.

   procedure Get_Data_Access_Control
     (This                              : Si4432_Driver;
      CRC_Selection                     : out CRC_Polynomial;
      CRC_Enable                        : out Boolean;
      Enable_Packet_TX_Handling         : out Boolean;
      Skip_2nd_Phase_Preamble_Detection : out Boolean;
      CRC_Data_Only_Enable              : out Boolean;
      LSB_First_Enable                  : out Boolean;
      Enable_Packet_RX_Handling         : out Boolean);

   -------------
   -- Headers --
   -------------

   procedure Set_Transmit_Header
     (This  : Si4432_Driver;
      Index : UInt2;
      Value : UInt8);
   --  Default:
   --  3 : 16#00#
   --  2 : 16#00#
   --  1 : 16#00#
   --  0 : 16#00#

   function Get_Transmit_Header
     (This  : Si4432_Driver;
      Index : UInt2)
      return UInt8;

   procedure Set_Check_Header
     (This  : Si4432_Driver;
      Index : UInt2;
      Value : UInt8);
   --  Check Header bytes 3 to 0 are checked against the corresponding bytes
   --  in the Received Header if the check is enabled.
   --  Default:
   --  3 : 16#00#
   --  2 : 16#00#
   --  1 : 16#00#
   --  0 : 16#00#

   function Get_Check_Header
     (This  : Si4432_Driver;
      Index : UInt2)
      return UInt8;

   procedure Set_Header_Enable
     (This  : Si4432_Driver;
      Index : UInt2;
      Value : UInt8);
   --  Header Enable bytes 3 to 0 control which bits of the Check Header bytes
   --  are checked against the corresponding bits in the Received Header.
   --  Only those bits are compared where the enable bits are set to 1.
   --  Default:
   --  3 : 16#FF#
   --  2 : 16#FF#
   --  1 : 16#FF#
   --  0 : 16#FF#

   function Get_Header_Enable
     (This  : Si4432_Driver;
      Index : UInt2)
      return UInt8;

   function Get_Received_Header
     (This  : Si4432_Driver;
      Index : UInt2)
      return UInt8;

   ----------
   -- GPIO --
   ----------

   procedure Set_GPIO0_Configuration
     (This                   : Si4432_Driver;
      Pin_Function           : GPIO_0_Function;
      --  Default Power_On_Reset

      Pullup_Resistor_Enable : Boolean;
      --  Default False
      --    When set to 1 a 200 kOm resistor is connected internally between
      --    VDD and the pin if the GPIO is configured as a digital input.

      Driving_Capability     : UInt2);
      --  Default 16#0#

   procedure Get_GPIO0_Configuration
     (This                   : Si4432_Driver;
      Pin_Function           : out GPIO_0_Function;
      Pullup_Resistor_Enable : out Boolean;
      Driving_Capability     : out UInt2);

   procedure Set_GPIO1_Configuration
     (This                   : Si4432_Driver;
      Pin_Function           : GPIO_1_Function;
      --  Default Inverted_Power_On_Reset

      Pullup_Resistor_Enable : Boolean;
      --  Default False
      --    When set to 1 a 200 kOm resistor is connected internally between
      --    VDD and the pin if the GPIO is configured as a digital input.

      Driving_Capability     : UInt2);
      --  Default 16#0#

   procedure Get_GPIO1_Configuration
     (This                   : Si4432_Driver;
      Pin_Function           : out GPIO_1_Function;
      Pullup_Resistor_Enable : out Boolean;
      Driving_Capability     : out UInt2);

   procedure Set_GPIO2_Configuration
     (This                   : Si4432_Driver;
      Pin_Function           : GPIO_2_Function;
      --  Default Microcontroller_Clocks

      Pullup_Resistor_Enable : Boolean;
      --  Default False
      --    When set to 1 a 200 kOm resistor is connected internally between
      --    VDD and the pin if the GPIO is configured as a digital input.

      Driving_Capability     : UInt2);
      --  Default 16#0#

   procedure Get_GPIO2_Configuration
     (This                   : Si4432_Driver;
      Pin_Function           : out GPIO_2_Function;
      Pullup_Resistor_Enable : out Boolean;
      Driving_Capability     : out UInt2);

   ---------------
   --  Settings --
   ---------------

   procedure Set_Modulation_Mode_Control_1
     (This                         : Si4432_Driver;
      Data_Whitening               : Boolean;
      --  Default False

      Manchester_Coding            : Boolean;
      --  Default False
      --    What Manchester coding does is to replace a single
      --    high bit (1) with two bits starting with low followed by high (01)
      --    and a low bit (0) with a high bit followed by a low bit (10). When
      --    Manchester is enabled, please configure as well the
      --    Manchester_Data_Inversion since it influences the Manchester
      --    encoding/decoding process.

      Manchester_Data_Inversion    : Boolean;
      --  Default True
      --    When False a 10 pair is considered a Manchester 0, and a
      --    01 pair as a Manchester 1. By setting this, do the opposite: every
      --    10 will be considered as a 1, and every 01 will be considered as
      --    a 0. This function is relevant only if the Manchester mode is
      --    enabled.

      Manchester_Preamble_Polarity : Boolean;
      --  Default True
      --    Will transmit a series of 1 if set, or series of 0 if reset. This
      --    bit affects only the transmitter side, not the receiver. This is
      --    valid only if Manchester Mode is enabled.

      Packet_Handler_Down          : Boolean;
      --  Default False
      --    If set, the Packet Handler will be powered down when chip is in
      --    low power mode.

      Data_Rates_Below             : Boolean);
      --  Default False
      --    This bit should be set for TX_Data_Rate below 30 kbps.

   procedure Get_Modulation_Mode_Control_1
     (This                         : Si4432_Driver;
      Data_Whitening               : out Boolean;
      Manchester_Coding            : out Boolean;
      Manchester_Data_Inversion    : out Boolean;
      Manchester_Preamble_Polarity : out Boolean;
      Packet_Handler_Down          : out Boolean;
      Data_Rates_Below             : out Boolean);

   procedure Set_Modulation_Mode_Control_2
     (This         : Si4432_Driver;
      Modulation   : Modulation_Mode;
      --  Default Unmodulated carrier

      Invert_TX_RX  : Boolean;
      --  Default False
      --    Invert TX and RX Data.

      Source        : Modulation_Source;
      --  Default Direct_GPIO

      TX_Data_Clock : Modulation_TX_Data_Clock);
      --  Default No_CLK

   procedure Get_Modulation_Mode_Control_2
     (This          : Si4432_Driver;
      Modulation    : out Modulation_Mode;
      Invert_TX_RX  : out Boolean;
      Source        : out Modulation_Source;
      TX_Data_Clock : out Modulation_TX_Data_Clock);

   procedure Set_Header_Control
     (This                    : Si4432_Driver;
      Header_Bytes_Check      : UInt4 := 16#C#;
      Broadcast_Address_Check : UInt4 := 0;
      Synchronization         : Synchronization_Word_Length := Word_3_2;
      Fix_TR_Packet_Length    : Boolean := False;
      Header                  : Header_Length := Header_3_2;
      Skipsyn                 : Boolean := False);
   --  Uses Set_Header_Control_1 and Set_Header_Control_2

   procedure Set_Header_Control_1
     (This                    : Si4432_Driver;
      Header_Bytes_Check      : UInt4;
      --  Default 16#C#
      --    Received Header Bytes to be Checked Against the Check Header Bytes.
      --    One hot encoding. The receiver will use Header to know the position
      --    of the Header Bytes.
      --      0:No Received Header check
      --      1:Received Header check for byte 0.
      --      2:Received Header check for bytes 1.
      --      3:Received header check for bytes 0 & 1 and so on.

      Broadcast_Address_Check : UInt4);
      --  Default 16#0#
      --    Broadcast Address (FFh) Check Enable.
      --    If it is enabled together with Header Byte Check then the header
      --    check is OK if the incoming header byte equals with the appropriate
      --    check byte or FFh). One hot encoding.
      --      0:No broadcast address enable.
      --      1:Broadcast address enable for header byte 0.
      --      2:Broadcast address enable for header byte 1.
      --      3:Broadcast address enable for header bytes 0 & 1 and so on.

   procedure Get_Header_Control_1
     (This                    : Si4432_Driver;
      Header_Bytes_Check      : out UInt4;
      Broadcast_Address_Check : out UInt4);

   procedure Set_Header_Control_2
     (This                 : Si4432_Driver;
      Synchronization      : Synchronization_Word_Length;
      --  Default Word_3_2
      --    The value in this register corresponds to the number of bytes used
      --    in the Synchronization Word. The synchronization word bytes are
      --    transmitted in descending order.

      Fix_TR_Packet_Length : Boolean;
      --  Default False
      --    When True the packet length (Packet_Length) is not included in the
      --    transmit header. When False the packet length is included in the
      --    transmit header. In receive mode, if this bit is set the packet
      --    length is obtained from the Packet_Length; otherwise the packet
      --    length is obtained from the received header packet length byte.

      Header               : Header_Length;
      --  Default Header_3_2
      --    Transmit/Receive Header Length. Length of header used if packet
      --    handler is enabled for TX/RX (enpactx/rx). Headers are
      --    transmitted/received in descending order.

      Skipsyn              : Boolean);
      --  Default False
      --    Skip Sync Word search timeout. If high, the system will ignore the
      --    search timeout period when failing to find Sync Word and will not
      --    return to searching for Preamble. Setting this bit does not
      --    eliminate the search for Sync Word. Proper detection of Sync Word
      --    remains necessary in FIFO mode in order to determine the start of
      --    the Payload field and to thus store the correct bytes in the RX
      --    FIFO.

   procedure Get_Header_Control_2
     (This                 : Si4432_Driver;
      Synchronization      : out Synchronization_Word_Length;
      Fix_TR_Packet_Length : out Boolean;
      Header               : out Header_Length;
      Skipsyn              : out Boolean);

   procedure Set_Operating_Mode_And_Function_Control
     (This                   : Si4432_Driver;
      READY_Mode             : Boolean := True;
      TUNE_Mode              : Boolean := False;
      RX_On                  : Boolean := False;
      TX_On                  : Boolean := False;
      Oscillator_Select      : Crystal_Oscillator := RC_Oscillator;
      Wake_Up_Timer          : Boolean := False;
      Low_Battery_Detect     : Boolean := False;
      TX_FIFO_Reset          : Boolean := False;
      RX_FIFO_Reset          : Boolean := False;
      Low_Duty_Cycle_Mode    : Boolean := False;
      Automatic_Transmission : Boolean := False;
      RX_Multi_Packet        : Boolean := False;
      Antenna_Diversity      : UInt3 := 0);
   --  Uses Set_Operating_Mode_And_Function_Control_1 and
   --   Set_Operating_Mode_And_Function_Control_2. See below.

   procedure Set_Operating_Mode_And_Function_Control_1
     (This               : Si4432_Driver;
      READY_Mode         : Boolean;
      --  Default True
      --  READY Mode (Xtal is ON).

      TUNE_Mode          : Boolean;
      --  Default False
      --  TUNE Mode (PLL is ON).
      --  When True the PLL will remain enabled in Idle State. This allows for
      --  faster turn-around time at the cost of increased current consumption
      --  in Idle State.

      RX_On              : Boolean;
      --  Default False
      --  RX on in Manual Receiver Mode.
      --  Automatically cleared if Multiple Packets config. is disabled and
      --  a valid packet received.

      TX_On              : Boolean;
      --  Default False
      --  TX on in Manual Transmit Mode.
      --  Automatically cleared in FIFO mode once the packet is sent.

      Oscillator_Select  : Crystal_Oscillator;
      --  Default RC_Oscillator

      Wake_Up_Timer      : Boolean;
      --  Default False
      --  Enable Wake-Up-Timer.
      --  Enabled when True. If the Wake-up-Timer function is enabled it will
      --  operate in any mode and notify the microcontroller through the
      --  GPIO interrupt when the timer expires.

      Low_Battery_Detect : Boolean;
      --  Default False
      --  Enable Low Battery Detect.
      --  When this bit is set to True the Low Battery Detector circuit and
      --  threshold comparison will be enabled.

      Software_Reset     : Boolean);
      --  Software Register Reset Bit.
      --  This bit may be used to reset all registers simultaneously to a
      --  DEFAULT state, without the need for sequentially writing to each
      --  individual register. The RESET is accomplished by setting True. This
      --  bit will be automatically cleared. The user should wait until the
      --  CHIPRDY status flag/interrupt is issued before sending further SPI
      --  commands to the chip.

   procedure Get_Operating_Mode_And_Function_Control_1
     (This               : Si4432_Driver;
      READY_Mode         : out Boolean;
      TUNE_Mode          : out Boolean;
      RX_On              : out Boolean;
      TX_On              : out Boolean;
      Oscillator_Select  : out Crystal_Oscillator;
      Wake_Up_Timer      : out Boolean;
      Low_Battery_Detect : out Boolean);

   procedure Set_Operating_Mode_And_Function_Control_2
     (This                   : Si4432_Driver;
      TX_FIFO_Reset          : Boolean;
      --  Default False
      --  TX FIFO Reset/Clear.
      --  This has to be a two writes operation: Setting True followed by
      --  False will clear the contents of the TX FIFO.

      RX_FIFO_Reset          : Boolean;
      --  Default False
      --  RX FIFO Reset/Clear.
      --  This has to be a two writes operation: Setting True followed by
      --  False will clear the contents of the RX FIFO.

      Low_Duty_Cycle_Mode    : Boolean;
      --  Default False
      --  Enable Low Duty Cycle Mode.
      --  If this bit is set to True then the chip turns on the RX regularly.
      --  The frequency should be set in the Wake-Up Timer Period register,
      --  while the minimum ON time should be set in the Low-Duty Cycle Mode
      --  Duration register. The FIFO mode should be enabled also.

      Automatic_Transmission : Boolean;
      --  Default False
      --  Automatic Transmission.
      --  When True the transceiver will enter automatically TX State when
      --  the FIFO is almost full. When the FIFO is empty it will automatically
      --  return to the Idle State.

      RX_Multi_Packet        : Boolean;
      --  Default False
      --  RX Multi Packet.
      --  When the chip is selected to use FIFO Mode
      --  (Modulation_Mode_Control_2.Modulation Source) and RX Packet Handling
      --  (Data_Access_Control.Packet_RX_Handling) then it will fill up the
      --  FIFO with multiple valid packets if this bit is set, otherwise the
      --  transceiver will automatically leave the RX State after the first
      --  valid packet has been received.

      Antenna_Diversity      : UInt3);
   --  Default 0
   --  Enable Antenna Diversity.
   --  The GPIO must be configured for Antenna Diversity for the algorithm to
   --  work properly.

   procedure Get_Operating_Mode_And_Function_Control_2
     (This                   : Si4432_Driver;
      TX_FIFO_Reset          : out Boolean;
      RX_FIFO_Reset          : out Boolean;
      Low_Duty_Cycle_Mode    : out Boolean;
      Automatic_Transmission : out Boolean;
      RX_Multi_Packet        : out Boolean;
      Antenna_Diversity      : out UInt3);

   procedure Clear_RX_FIFO (This : Si4432_Driver);
   procedure Clear_TX_FIFO (This : Si4432_Driver);
   --  Set TX_FIFO_Reset or RX_FIFO_Reset

   procedure Set_Crystal_Oscillator_Load_Capacitance
     (This               : Si4432_Driver;
      Tuning_Capacitance : UInt7;
      --  Default 16#7F#
      --    Tuning Capacitance for the 30 MHz XTAL.

      Shft               : Bit);
      --  Default 16#0#
      --    Additional capacitance to coarse shift the frequency if
      --    Tuning_Capacitance is not sufficient. Not binary with
      --    Tuning_Capacitance.

   procedure Get_Crystal_Oscillator_Load_Capacitance
     (This               : Si4432_Driver;
      Tuning_Capacitance : out UInt7;
      Shft               : out Bit);

   procedure Set_IF_Filter_Bandwidth
     (This             : Si4432_Driver;
      Coefficient      : UInt4;
      --  Default: 1
      --  Selects one of 15 pre-calculated sets of digital FIR filter tap
      --  coefficients. Along with the decimation ratios selected by
      --  Bypass_Decimate and Decimation_Rates, the filter coefficients
      --  determine the bandwidth of the IF filter.

      Decimation_Rates : UInt3;
      --  Default: 0
      --  The oversampled data in the receive data path is decimated by a
      --  factor of 2^Decimation_Rates. A higher decimation factor (i.e.,
      --  larger value of ndec_exp) results in a lower IF filter bandwidth.

      Bypass_Decimate  : Boolean);
      --  Default: False
      --  If set, results in bypassing a decimate-by-3 stage in the path of
      --  the oversampled data path for the digital filter for the IF
      --  bandwidth.

   procedure Get_IF_Filter_Bandwidth
     (This             : Si4432_Driver;
      Coefficient      : out UInt4;
      Decimation_Rates : out UInt3;
      Bypass_Decimate  : out Boolean);

   procedure Set_Clock_Recovery_Oversampling_Rate
     (This  : Si4432_Driver;
      Value : Clock_Recovery_Oversampling);
   --  3 LSBs are the fraction, default = 16#64# = 12.5 clock cycles per
   --  data bit.
   --  The oversampling rate can be calculated as Oversampling =
   --  500 kHz/(2 IF_Filter_Bandwidth.Decimation_Rates x RX_DR). The
   --  IF_Filter_Bandwidth.Decimation_Rates and the Bypass_Decimate values
   --  together with the receive data rate (Rb) are the parameters needed
   --  to calculate Oversampling:
   --
   --    Oversampling = (500 x (1 + 2xBypass_Decimate)) /
   --                     (2^Decimation_Rates-3 x Rb x (1 + Manchester_Coding))
   --
   --  The Rb unit used in this equation is in kbps.
   --  The Manchester_Coding is the Manchester_Coding parameter
   --  (see Modulation_Mode_Control_1.Manchester_Coding is 1 when enabled and
   --  0 when disabled). The number found in the equation should be rounded to
   --  an integer. The integer can be translated to a hexadecimal. For optimal
   --  modem performance it is recommended to set the Oversampling to at
   --  least 8. A higher Oversampling can be obtained by choosing a lower
   --  value for IF_Filter_Bandwidth.Decimation_Rates or enable
   --  IF_Filter_Bandwidth.Bypass_Decimate. A correction in filset might be
   --  needed to correct the channel select bandwidth to the desired value.
   --  Note that when Decimation_Rates or Bypass_Decimate are changed the
   --  related parameters (Oversampling,
   --  Clock_Recovery_Offset.NCO_Offset and
   --  Clock_Recovery_Timing_Loop_Gain.Gain) need to be updated.

   function Get_Clock_Recovery_Oversampling_Rate
     (This  : Si4432_Driver)
      return Clock_Recovery_Oversampling;

   procedure Set_Clock_Recovery_Offset
     (This           : Si4432_Driver;
      NCO_Offset     : Clock_Recovery_Offset;
      --  Default 16#147AE#
      --  See formula above.

      Skip_2nd_Phase : Boolean);
      --  Default False
      --  Skip 2nd Phase Ant Div Threshold. Threshold for skipping the 2nd
      --  phase of RSSI detection during antenna diversity algorithm.
      --  False=16 dB (default), True=11 dB.
      --  NOT RECOMMENDED FOR USER CONFIGURATION.

   --  The offset can be calculated as follows:
   --    NCO_Offset =
   --     (Rb x (1 + Modulation_Mode_Control_1.Manchester_Coding) x
   --        2^IF_Filter_Bandwidth.Decimation_Rates) /
   --     (500 x (1 + 2 x IF_Filter_Bandwidth.Bypass_Decimate))

   --  The default values for Clock_Recovery_Oversampling_Rate and
   --  Clock_Recovery_Offset gives 40 kbps RX_DR with Manchester coding is
   --  disabled.

   procedure Get_Clock_Recovery_Offset
     (This           : Si4432_Driver;
      NCO_Offset     : out Clock_Recovery_Offset;
      Skip_2nd_Phase : out Boolean);

   procedure Set_Clock_Recovery_Timing_Loop_Gain
     (This                : Si4432_Driver;
      Gain                : Clock_Recovery_Timing_Loop_Gain;
      --  Default 16#28F#
      --  Clock Recovery Timing Loop Gain.

      Multiplying_By_2    : Boolean;
      --  Default False
      --  Multiplying the CR Gain by 2.

      Compensation_Enable : Boolean);
      --  Default False
      --  Receive Compensation Enable for High Data Rate Offset.

   --  The loop gain can be calculated as follows:
   --    Gain = 2 +
   --      ((2^16 x (1 + Modulation_Mode_Control_1.Manchester Coding) x Rb) /
   --        (Clock_Recovery_Oversampling_Rate x Fd))

   procedure Get_Clock_Recovery_Timing_Loop_Gain
     (This                : Si4432_Driver;
      Gain                : out Clock_Recovery_Timing_Loop_Gain;
      Multiplying_By_2    : out Boolean;
      Compensation_Enable : out Boolean);

   procedure Set_AFC_Loop_Gearshift_Override
     (This           : Si4432_Driver;
      Reset_Preamble : Boolean;
      --  If False (default), we will reset the Preamble detector if there
      --  are 5 consecutive zero phases. If True, the reset will happen after
      --  3 consecutive zero phases.

      Taps           : Boolean;
      --  Number of taps for moving average filter during Antenna Diversity
      --  RSSI evaluation. Allows for reduced noise variation on measured
      --  RSSI value but with slower update rate. If True, filter tap
      --  length = 8*Tb. If False (default), filter tap length = 8*Tb prior
      --  to first PREAMBLE_VALID, and 4*Tb thereafter.

      Bypass         : Boolean;
      --  If True, select 0dB bias for the second phase antenna selection,
      --  if False, select 1.5 dB. The default is True, selecting 0 dB.

      AFC_High_Gear  : UInt3;
      --  AFC High Gear Setting. Feedback loop gain during AFC setting
      --  process is proportional to 2^(-AFC_High_Gear).
      AFC            : Boolean;
      --  AFC Enable.

      AFC_Wideband   : Boolean);
      --  AFC Wideband Enable (active True). If set, the IF filter bandwidth
      --  is reduced after preamble detection, in order to optimize RX
      --  sensitivity. The IF filter bandwidth used during preamble detection
      --  is programmed by the FILSET, NDEC, and DWN3BYPASS parameters in
      --  IF_Filter_Bandwidth. After preamble detection, the chip automatically
      --  selects the next lower IF filter bandwidth by internally decreasing
      --  the FILSET parameter by 1. The resulting filter bandwidth may be
      --  determined from the bandwidth table provided under the description
      --  for IF_Filter_Bandwidth.

   procedure Get_AFC_Loop_Gearshift_Override
     (This           : Si4432_Driver;
      Reset_Preamble : out Boolean;
      Taps           : out Boolean;
      Bypass         : out Boolean;
      AFC_High_Gear  : out UInt3;
      AFC            : out Boolean;
      AFC_Wideband   : out Boolean);

   procedure Set_AFC_Limiter
     (This  : Si4432_Driver;
      Value : UInt8);
   --  Default 16#0#
   --  AFC limiter value.

   function Get_AFC_Limiter
     (This : Si4432_Driver)
      return UInt8;

   procedure Set_AGC_Override
     (This                   : Si4432_Driver;
      Gain_Override          : AGC_Override_Gain;
      --  Default 0

      LNA_Gain_Select        : Boolean;
      --  Default False
      --  Inagain=LNA Gain select.
      --  False - min. gain = 5 dB
      --  True - max. gain = 25 dB

      Automatic_Gain_Control : Boolean;
      --  Default True
      --  Automatic Gain Control enable. When is set then the result of the
      --  control can be read out from Gain_Override, otherwise the gain can
      --  be controlled manually by writing into Gain_Override.
      Sgin                   : Boolean);
      --  AGC stop increasing gain override bit (active low). When False
      --  (default), AGC gain increases during signal reductions are prevented.
      --  When True, AGC gain increases during signal reductions are allowed.
      --  Only effective during Preamble, prior to detection of
      --  PREAMBLE_VALID signal.

   procedure Get_AGC_Override
     (This                   : Si4432_Driver;
      Gain_Override          : out AGC_Override_Gain;
      LNA_Gain_Select        : out Boolean;
      Automatic_Gain_Control : out Boolean;
      Sgin                   : out Boolean);

   procedure Set_Microcontroller_Output_Clock
     (This                : Si4432_Driver;
      Clock               : Microcontroller_Clock;
      --  Different clock frequencies may be selected for configurable GPIO
      --  clock output. All clock frequencies are created by dividing the XTAL
      --  except for the 32 kHz clock which comes directly from the 32 kHz
      --  RC Oscillator. The setting is only valid when
      --  Operating_Mode_And_Function_Control_1.READY_Mode except the 32 kHz.

      Low_Frequency_Clock : Boolean;
      --  When True and the chip is in Sleep mode then the 32.768 kHz clock
      --  will be provided to the microcontroller no matter what the selection
      --  of Microcontroller_Clock is. For example if
      --  Microcontroller_Clock = 30 MHz will be available through the GPIO
      --  to output to the microcontroller in all Idle, TX, or RX states.
      --  When the chip is commanded to Sleep mode the 30 MHz clock will
      --  become 32.768 kHz.

      Tail                : Clock_Tail);
      --  If Low_Frequency_Clock = False then it can be useful to provide a
      --  few extra cycles for the microcontroller to complete its operation.
      --  Setting the value will provide the addition cycles of the clock
      --  before it shuts off.

   procedure Get_Microcontroller_Output_Clock
     (This                : Si4432_Driver;
      Clock               : out Microcontroller_Clock;
      Low_Frequency_Clock : out Boolean;
      Tail                : out Clock_Tail);

   procedure Set_IO_Port_Configuration
     (This                 : Si4432_Driver;
      Direct_GPIO0         : Boolean;
      --  Default False
      --  Direct I/O for GPIO0.
      --  If the GPIO0 is configured to be a direct output then the value on
      --  the GPIO pin can be set here. If the GPIO0 is configured to be a
      --  direct input then the value of the pin can be read here.

      Direct_GPIO1         : Boolean;
      --  Default False
      --  Direct I/O for GPIO1.
      --  If the GPIO1 is configured to be a direct output then the value on
      --  the GPIO pin can be set here. If the GPIO1 is configured to be a
      --  direct input then the value of the pin can be read here.

      Direct_GPIO2         : Boolean;
      --  Default False
      --  Direct I/O for GPIO2.
      --  If the GPIO2 is configured to be a direct output then the value on
      --  the GPIO pin can be set here. If the GPIO2 is configured to be a
      --  direct input then the value of the pin can be read here.

      Interrupt_Output_SDO : Boolean);
      --  Default False
      --  Interrupt Request Output on the SDO Pin.
      --  nIRQ output is present on the SDO pin if this bit is set and the
      --  nSEL input is inactive (high).

   procedure Get_IO_Port_Configuration
     (This                 : Si4432_Driver;
      Direct_GPIO0         : out Boolean;
      Direct_GPIO1         : out Boolean;
      Direct_GPIO2         : out Boolean;
      Interrupt_Output_SDO : out Boolean;
      External_0_Interrupt : out Boolean;
      --  External Interrupt Status.
      --  If the GPIO0 is programmed to be an external interrupt source then
      --  the status can be read here.

      External_1_Interrupt : out Boolean;
      --  External Interrupt Status.
      --  If the GPIO1 is programmed to be an external interrupt source then
      --  the status can be read here.

      External_2_Interrupt : out Boolean);
      --  External Interrupt Status.
      --  If the GPIO2 is programmed to be an external interrupt source then
      --  the status can be read here.

   procedure Set_ADC_Configuration
     (This                  : Si4432_Driver;
      Sensor_Amplifier_Gain : UInt2;
      --  Default 0
      --  The full scale range of the internal 8-bit ADC in differential mode
      --  (see Input_Source)

      Reference_Voltage     : UInt2;
      --  Default 0
      --  The reference voltage of the internal 8-bit ADC can be selected
      --  as follows:
      --    0X:bandgap voltage (1.2 V)
      --    10:VDD/3
      --    11:VDD/2

      Input_Source          : ADC_Input_Source;
      --  Default 0
      --  The internal 8-bit ADC input source can be selected

      Measurement_Start     : Boolean);
      --  Default False
      --  Set this True starts the ADC measurement process. This bit
      --  self-clears during the measurement cycle and returns high when the
      --  measurement is complete. The conversion process is fast; reading
      --  this bit may always appear to return a True.

   --  Reference_Voltage[00] = FS=0.014 x (Sensor_Amplifier_Gain + 1) x VDD
   --  Reference_Voltage[01] = FS=0.021 x (Sensor_Amplifier_Gain + 1) x VDD

   procedure Get_ADC_Configuration
     (This                  : Si4432_Driver;
      Sensor_Amplifier_Gain : out UInt2;
      Reference_Voltage     : out UInt2;
      Input_Source          : out ADC_Input_Source;
      Measurement_Start     : out Boolean);

   procedure Set_ADC_Sensor_Amplifier_Offset
     (This             : Si4432_Driver;
      Amplifier_Offset : ADC_Sensor_Amplifier_Offset);
   --  *Note: The offset can be calculated as
   --     Offset = Amplifier_Offset x VDD/1000;

   function Get_ADC_Sensor_Amplifier_Offset
     (This : Si4432_Driver)
      return ADC_Sensor_Amplifier_Offset;

   function Get_ADC_Value
     (This : Si4432_Driver)
      return UInt8;
   --  Internal 8 bit ADC Output Value

   procedure Set_Temperature_Sensor_Calibration
     (This         : Si4432_Driver;
      Trim_Value   : UInt4;
      --  Temperature Sensor Trim Value.

      Trim         : Boolean;
      --  Temperature Sensor Trim Enable.

      Offset       : Boolean;
      --  Temperature Sensor Offset to Convert from K to C. Default is True.
      --  Test mode only, to use set Sensor_Range = From_Minus_64_To_64C and
      --  Offset to False.

      Sensor_Range : Temperature_Sensor_Range);
      --  Temperature Sensor Range Selection. (FS range is 0-1024 mV)

   procedure Get_Temperature_Sensor_Calibration
     (This         : Si4432_Driver;
      Trim_Value   : out UInt4;
      Trim         : out Boolean;
      Offset       : out Boolean;
      Sensor_Range : out Temperature_Sensor_Range);

   procedure Set_Temperature_Value_Offset
     (This  : Si4432_Driver;
      Value : UInt8);
   --  Default 0
   --  This value is added to the measured temperature value.

   function Get_Temperature_Value_Offset
     (This : Si4432_Driver)
      return UInt8;

   procedure Set_Low_Battery_Detector_Threshold
     (This  : Si4432_Driver;
      Value : UInt5);
   --  This threshold is compared to Battery Voltage Level. If the Battery
   --  Voltage is less than the threshold the Low Battery Interrupt is set.
   --  Default = 2.7 V. Note: The threshold can be calculated as:
   --    Vthreshold = 1.7 + Value x 50 mV.

   function Get_Low_Battery_Detector_Threshold
     (This : Si4432_Driver)
      return UInt5;

   function Get_Battery_Voltage_Level
     (This : Si4432_Driver)
      return UInt5;
   --  The battery voltage is converted by a 5 bit ADC if the
   --  Operating_Mode_And_Function_Control_1.Low_Battery_Detect is also set.
   --  In Sleep Mode the register is updated in every 1 s. In other states
   --  it measures continuously. The measured voltage is calculated by the
   --  following formula:
   --    Vbat_meas=1.7V + Battery_Voltage_Level x 50 mV

   procedure Set_AFC_Timing_Control
     (This        : Si4432_Driver;
      Anwait      : UInt3;
      --  Antenna switching wait time. Number of bit periods between toggling
      --  selection of antennas in AntDiv mode, prior to reception of first
      --  PREAMBLE_VALID.
      --  Number of bit periods = ( Anwait + 2 ) x 4 +3 (when AFC = enabled)
      --  Number of bit periods = ( Anwait + 2 ) x 2 +3 (when AFC = disabled)
      --  Default value = 2#010# = 19 bit periods (AFC = enabled).

      Shwait      : UInt3;
      --  Shwait wait periods after AFC correction used before preamble is
      --  detected. Short wait=(RegValue+1) x 2Tb. If set to 0 then no AFC
      --  correction will occur before preamble detect, i.e., AFC will be
      --  disabled. Default value = 2#001#

      Swant_Timer : UInt2);
      --  Additional number of bit periods to wait for RSSI value to stabilize
      --  during Antenna Diversity 2nd phase antenna evaluation. If
      --  AFC_Loop_Gearshift_Override.Taps = False, total wait time =
      --   8 x Tb + Swant_Timer. If AFC_Loop_Gearshift_Override.Taps = True,
      --  total wait time = 12 * Tb + Swant_Timer.
      --  Effective only during Antenna Diversity.

   procedure Get_AFC_Timing_Control
     (This        : Si4432_Driver;
      Anwait      : out UInt3;
      Shwait      : out UInt3;
      Swant_Timer : out UInt2);

   procedure Set_Clock_Recovery_Gearshift_Override
     (This : Si4432_Driver;
      Slow : UInt3;
      Fast : UInt3);
   --  The gear-shift register controls BCR loop gain. Before the preamble
   --  is detected, BCR loop gain is as follows:
   --    BCRLoopGain = crgain / 2^Fast
   --
   --  Once the preamble is detected, internal state machine automatically
   --  shift BCR loop gain to the following:
   --    BCRLoopGain = crgain / 2^Slow
   --
   --  Fast = 0 and Slow = 2#101# are recommended for most applications.
   --  The value of "slow" should be greater than "fast".

   procedure Get_Clock_Recovery_Gearshift_Override
     (This : Si4432_Driver;
      Slow : out UInt3;
      Fast : out UInt3);

   function Get_AFC_Correction
     (This : Si4432_Driver)
      return AFC_Correction;
   --  AFC loop correction values. Values are updated once, after sync word
   --  is found during receiving.

   procedure Set_OOK_Counter
     (This          : Si4432_Driver;
      Value         : OOK_Counter;
      --  OOK counter value. This counter value will affect the
      --  OOK AGC's decay time. Use the following equation:
      --    ook_cnt_val = 3 x 500[kHz] /
      --       Rb x (Modulation_Mode_Control_1.Manchester_Coding)
      --  Where Rb's unit is in kHz. Therefore, the minimal data rate that this
      --  register can support without Manchester is 0.366 kbps.

      MA            : Boolean;
      --  When True (default), Moving Average Detector for OOK Modem is
      --  enabled. Provides best sensitivity, but requires DC-balanced data
      --  (e.g., Manchester data) and is more sensitive to co-channel
      --  interference. Peak Detector output is logically AND'ed with Moving
      --  Average Detector output.

      Peak_Detector : Boolean;
      --  When True (default), Peak Detector for OOK Modem is enabled. Provides
      --  improved performance in presence of co-channel interferers, at slight
      --  reduction of sensitivity. Peak Detector output is logically AND'ed
      --  with Moving Average Detector output.

      OOK_Freeze    : Boolean);
   --  when False (default), AGC and OOK Moving Average Detector threshold
   --  operate continuously. When True, AGC and OOK MA Detector threshold
   --  operate until PREAMBLE_VALID signal is detected; values are frozen
   --  thereafter. Recommended for use with non-Manchestered payload data.

   procedure Get_OOK_Counter
     (This          : Si4432_Driver;
      Value         : out OOK_Counter;
      MA            : out Boolean;
      Peak_Detector : out Boolean;
      OOK_Freeze    : out Boolean);

   procedure Set_Slicer_Peak_Holder
     (This   : Si4432_Driver;
      Decay  : UInt4;
      --  OOK Peak Detector decay time. Peak detector value discharges at
      --  rate proportional to 2^Decay. OOK slicing threshold is set 6 dB
      --  below peak detector value. Effective only when OOK Peak Detector
      --  is enabled.

      Attack : UInt3);
      --  OOK Peak Detector attack time. Peak detector value charges up at
      --  rate proportional to 2^Attack. OOK slicing threshold is set 6 dB
      --  below peak detector value. Effective only when OOK Peak Detector
      --  is enabled.

   procedure Get_Slicer_Peak_Holder
     (This   : Si4432_Driver;
      Decay  : out UInt4;
      Attack : out UInt3);

   procedure Set_ADC8_Control
     (This  : Si4432_Driver;
      Value : ADC8_Control);
   --  Default: 16#10#

   function Get_ADC8_Control
     (This : Si4432_Driver)
      return ADC8_Control;

   procedure Set_Channel_Filter_Coefficient_Address
     (This  : Si4432_Driver;
      Value : UInt4);
   --  Default: 16#0#
   --  This configures (in nibbles) for how long we will search for preamble.
   --  If during this time the preamble is not detected, we will send a signal
   --  (which can be configured as interrupt) and restart looking for the
   --  preamble again.

   function Get_Channel_Filter_Coefficient_Address
     (This : Si4432_Driver)
      return UInt4;

   procedure Set_Crystal_Oscillator
     (This                           : Si4432_Driver;
      Output_Buffer                  : Boolean;
      --  Default: False
      --  This bit is active only if the Output_Buffer_Override is set to True.

      Output_Buffer_Override         : Boolean;
      --  Default: False
      --  If set to True then the Output_Buffer controls the output buffer.
      --  False: output buffer is controlled by the state machine.
      --  True:  output buffer is controlled by the Output_Buffer.

      Two_Times_Higher_Amplification : Boolean;
      --  Default: True

      Two_Times_Higher_Bias_Current  : Boolean;
      --  Default: False

      Clock_Hysteresis               : Boolean);
      --  Default: False

   procedure Get_Crystal_Oscillator
     (This                           : Si4432_Driver;
      Output_Buffer                  : out Boolean;
      Output_Buffer_Override         : out Boolean;
      Two_Times_Higher_Amplification : out Boolean;
      Two_Times_Higher_Bias_Current  : out Boolean;
      Clock_Hysteresis               : out Boolean;
      Internal_Power_State           : out Internal_Chip_State);

   -------------------
   -- Wake Up Timer --
   -------------------

   --  Note: If a new configuration is needed (e.g., for the WUT or the LDC),
   --  proper functionality is required. The function must first be disabled,
   --  then the settings changed, then enabled back on.

   procedure Set_Wake_Up_Timer_Period
     (This  : Si4432_Driver;
      Value : Wake_Up_Timer_Period);
   --  Default 1

   function Get_Wake_Up_Timer_Period
     (This : Si4432_Driver)
      return Wake_Up_Timer_Period;

   procedure Set_Wake_Up_Timer_Exponent
     (This  : Si4432_Driver;
      Value : Wake_Up_Timer_Exponent);
   --  Default 3
   --  The period of the wake-up timer can be calculated as
   --    TWUT = (4 x M x 2^Wake_Up_Timer_Exponent)/32.768 ms.
   --  Wake_Up_Timer_Exponent = 0 is allowed, and the maximum is decimal 20.

   function Get_Wake_Up_Timer_Exponent
     (This  : Si4432_Driver)
      return Wake_Up_Timer_Exponent;

   function Get_Wake_Up_Timer_Current_Value
     (This : Si4432_Driver)
      return UInt16;
   --  The value reflects the current count value of the timer.

   procedure Set_Low_Duty_Cycle_Mode_Duration
     (This  : Si4432_Driver;
      Value : Low_Duty_Cycle_Mode_Duration);
   --  If enabled, the LDC will start together when the WUT is supposed to
   --  start, and the duration of the LDC is specified by the value and the
   --  equation that goes with it. In order for the LDC to work, the LDC value
   --  has to be smaller than the value specified in Wake_Up_Timer_Period.

   --  *Note: The period of the low-duty cycle ON time can be calculated as
   --    TLDC_ON = (4 x LDC x 2^R)/32.768 ms. R is the Wake_Up_Timer_Exponent.
   --  The LDC works in conjunction with the WUT. The LDC period must be
   --  specified to be smaller than the WUT period. (i.e., the LDC register
   --  must be smaller than the Wake_Up_Timer_Period).

   function Get_Low_Duty_Cycle_Mode_Duration
     (This : Si4432_Driver)
      return Low_Duty_Cycle_Mode_Duration;

   -------------------
   -- Packet_Length --
   -------------------

   procedure Set_Packet_Length
     (This   : Si4432_Driver;
      Length : UInt8);
   --  Default 0
   --  The value in this register corresponds directly to the number of bytes
   --  in the Packet. For example '00001000' corresponds to a packet length of
   --  8 bytes. The maximum packet length is '11111111', a 255 byte packet.
   --  Writing 0 is possible, in this case we do not send any data in the
   --  packet. During RX, if Header_Control_2.Fix_TR_Packet_Length = True, this
   --  will specify also the Packet_Length for RX mode.
   --  Use Get_Received_Packet_Length if Header_Control_2.
   --    Fix_TR_Packet_Length = False to get received length.

   function Get_Packet_Length
     (This : Si4432_Driver)
      return UInt8;

   function Get_Received_Packet_Length
     (This : Si4432_Driver)
      return UInt8;
   --  Length Byte of the Received Packet during Header_Control_2.
   --  Fix_TR_Packet_Length = False. This register specifies the number of
   --  Data bytes in the last received packet, and reflects the value of the
   --  packet length byte in the received header. This is relevant ONLY if the
   --  Fix_TR_Packet_Length is False. If the Fix_TR_Packet_Length is set,
   --  then the expected number of received Data bytes must be programmed
   --  into the Packet_Length.

   ----------
   -- RSSI --
   ----------

   function Get_Received_Signal_Strength_Indicator
     (This : Si4432_Driver)
      return UInt8;

   procedure Set_RSSI_Threshold_For_Clear_Channel_Indicator
     (This  : Si4432_Driver;
      Value : UInt8);
   --  Interrupt is set if the RSSI value is above this threshold.

   function Get_RSSI_Threshold_For_Clear_Channel_Indicator
     (This : Si4432_Driver)
      return UInt8;

   function Get_Antenna_Diversity_1
     (This : Si4432_Driver)
      return UInt8;
   --  Measured RSSI Value on Antenna 1.

   function Get_Antenna_Diversity_2
     (This : Si4432_Driver)
      return UInt8;
   --  Measured RSSI Value on Antenna 2.

   ----------
   -- FIFO --
   ----------

   procedure Send
     (This : Si4432_Driver;
      Data : SPI_Data_8b);
   --  Send data as a variable-length packet. It set count of bytes by
   --  Packet_Length, fill data with Set_FIFO_Data and turn TX on by
   --  Set_TX_Mode.

   procedure Get_Received
     (This : Si4432_Driver;
      Data : out SPI_Data_8b);
   --  Get received data. Lenght of the data can be get by
   --    Get_Received_Packet_Length. Also clears the RX FIFO with
   --    Clear_RX_FIFO if all data was copied to the Data

   procedure Set_TX_FIFO_Almost_Full
     (This  : Si4432_Driver;
      Value : FIFO_Threshold);
   --  Default 16#37#
   --  This specifies the threshold value at which the TXFFAFULL status
   --  bit/interrupt will be generated, as data bytes are stored into the
   --  TX FIFO for later transmission. This value should be programmed to
   --  1 byte less than the desired threshold value. Example: A value of
   --  0x3C=60d will not generate an interrupt if 60 bytes (or less) are
   --  written to the TX FIFO, but will generate an interrupt when 61 bytes
   --  (or more) are written to the TX FIFO.

   function Get_TX_FIFO_Almost_Full
     (This : Si4432_Driver)
      return FIFO_Threshold;

   procedure Set_TX_FIFO_Almost_Empty
     (This  : Si4432_Driver;
      Value : FIFO_Threshold);
   --  Default 16#4#
   --  This register specifies the threshold value at which the TXFFAEM status
   --  bit/interrupt will be generated, as data bytes are pulled from the
   --  TX FIFO and transmitted. This value should be programmed to 1 byte less
   --  than the desired threshold value. Example: A value of 0x05 will generate
   --  an interrupt when 6 bytes remain in the TX FIFO.

   function Get_TX_FIFO_Almost_Empty
     (This : Si4432_Driver)
      return FIFO_Threshold;

   procedure Set_RX_FIFO_Almost_Full
     (This  : Si4432_Driver;
      Value : FIFO_Threshold);
   --  Default 16#37#
   --  This register specifies the threshold value at which the RXFFAFULL
   --  status bit/interrupt will be generated, as data bytes are received and
   --  stored into the RX FIFO for later retrieval. This value should be
   --  programmed to 1 byte less than the desired threshold value.
   --  Example: A value of 0x3C=60d will not generate an interrupt if 60
   --  bytes (or less) are received and stored to the RX FIFO, but will
   --  generate an interrupt when 61 bytes (or more) are received and stored
   --  to the RX FIFO.

   function Get_RX_FIFO_Almost_Full
     (This : Si4432_Driver)
      return FIFO_Threshold;

private

   type Register_Address is range 16#00# .. 16#7F# with Size => UInt7'Size;

   type Operation_Type is (Read, Write) with Size => Bit'Size;
   for Operation_Type use (Read => 0, Write => 1);

   ----------------
   --  Registers --
   ----------------

   type Device_Type_Code_Register is record
      dt         : UInt5;      --  R
      Reserved_R : UInt3 := 0; --  R
   end record with Size => Register'Size;
   --  Reset value = 00001000

   for Device_Type_Code_Register use record
      dt         at 0 range 0 .. 4;
      Reserved_R at 0 range 5 .. 7;
   end record;

   type Version_Code_Register is record
      vc         : UInt5;      --  R
      Reserved_R : UInt3 := 0; --  R
   end record with Size => Register'Size;

   for Version_Code_Register use record
      vc         at 0 range 0 .. 4;
      Reserved_R at 0 range 5 .. 7;
   end record;

   for Chip_State use (Idle => 0, RX => 1, TX => 2);

   type Device_Status_Register is record
      cps        : Chip_State;  --  R
      Reserved_R : Bit;  --  R
      freqerr    : Bit;  --  R
      --  Frequency Error Status.
      --  The programmed frequency is outside of the operating range.
      --  The actual frequency is saturated to the max/min value.

      headerr    : Bit;  --  R
      --  Header Error Status.
      --  Indicates if the received packet has a header check error.

      rxffem     : Bit;  --  R
      --  RX FIFO Empty Status.

      ffunfl     : Bit;  --  R
      --  RX/TX FIFO Underflow Status.

      ffovfl     : Bit;  --  R
      --  RX/TX FIFO Overflow Status.
   end record with Size => Register'Size;
   --  Reset value = xxxxxxxx

   for Device_Status_Register use record
      cps        at 0 range 0 .. 1;
      Reserved_R at 0 range 2 .. 2;
      freqerr    at 0 range 3 .. 3;
      headerr    at 0 range 4 .. 4;
      rxffem     at 0 range 5 .. 5;
      ffunfl     at 0 range 6 .. 6;
      ffovfl     at 0 range 7 .. 7;
   end record;

   type Interrupt_Status_1_Register is record
      icrerror   : Bit;  --  R
      --  CRC Error.
      --  When set to 1 the cyclic redundancy check is failed.

      ipkvalid   : Bit;  --  R
      --  Valid Packet Received. When set to 1 a valid packet has
      --  been received.

      ipksent    : Bit;  --  R
      --  Packet Sent Interrupt.
      --  When set to1 a valid packet has been transmitted.

      iext       : Bit;  --  R
      --  External Interrupt.
      --  When set to 1 an interrupt occurred on one of the GPIO's if it is
      --  programmed so. The status can be checked in register 0Eh. See
      --  GPIOx Configuration section for the details.

      irxffafull : Bit;  --  R
      --  RX FIFO Almost Full.When set to 1 the RX FIFO has met its almost
      --  full threshold and needs to be read by the microcontroller.

      ixtffaem   : Bit;  --  R
      --  TX FIFO Almost Empty.
      --  When set to 1 the TX FIFO is almost empty and needs to be filled.

      itxffafull : Bit;  --  R
      --  TX FIFO Almost Full.
      --  When set to 1 the TX FIFO has met its almost full threshold and
      --  needs to be transmitted.

      ifferr     : Bit;  --  R
      --  FIFO Underflow/Overflow Error.
      --  When set to 1 the TX or RX FIFO has overflowed or underflowed.
   end record with Size => Register'Size;
   --  Reset value = xxxxxxxx

   for Interrupt_Status_1_Register use record
      icrerror   at 0 range 0 .. 0;
      ipkvalid   at 0 range 1 .. 1;
      ipksent    at 0 range 2 .. 2;
      iext       at 0 range 3 .. 3;
      irxffafull at 0 range 4 .. 4;
      ixtffaem   at 0 range 5 .. 5;
      itxffafull at 0 range 6 .. 6;
      ifferr     at 0 range 7 .. 7;
   end record;

   type Interrupt_Status_2_Register is record
      ipor       : Bit;  --  R
      --  Power-on-Reset (POR).
      --  When the chip detects a Power on Reset above the desired setting
      --  this bit will be set to 1.

      ichiprdy   : Bit;  --  R
      --  Chip Ready (XTAL).
      --  When a chip ready event has been detected this bit will be set to 1.

      ilbd       : Bit;  --  R
      --  Low Battery Detect.
      --  When a low battery event has been detected this bit will be set to 1.
      --  This interrupt event is saved even if it is not enabled by the mask
      --  register bit and causes an interrupt after it is enabled.

      iwut       : Bit;  --  R
      --  Wake-Up-Timer.
      --  On the expiration of programmed wake-up timer this bit will be set
      --  to 1.

      irssi      : Bit;  --  R
      --  RSSI.
      --  When RSSI level exceeds the programmed threshold this bit will be
      --  set to 1.

      ipreainval : Bit;  --  R
      --  Invalid Preamble Detected.
      --  When the preamble is not found within a period of time set by the
      --  invalid preamble detection threshold in Register 60h, this bit will
      --  be set to 1.

      ipreaval   : Bit;  --  R
      --  Valid Preamble Detected.
      --  When a preamble is detected this bit will be set to 1.

      iswdet     : Bit;  --  R
      --  Sync Word Detected.
      --  When a sync word is detected this bit will be set to 1.
   end record with Size => Register'Size;
   --  Reset value = xxxxxxxx

   for Interrupt_Status_2_Register use record
      ipor       at 0 range 0 .. 0;
      ichiprdy   at 0 range 1 .. 1;
      ilbd       at 0 range 2 .. 2;
      iwut       at 0 range 3 .. 3;
      irssi      at 0 range 4 .. 4;
      ipreainval at 0 range 5 .. 5;
      ipreaval   at 0 range 6 .. 6;
      iswdet     at 0 range 7 .. 7;
   end record;

   type Interrupt_Enable_1_Register is record
      encrcerror  : Bit;  --  R/W
      --  Enable CRC Error.
      --  When set to 1 the CRC Error interrupt will be enabled.

      enpkvalid   : Bit;  --  R/W
      --  Enable Valid Packet Received.
      --  When ipkvalid = 1 the Valid Packet Received Interrupt will be enabled.

      enpksent    : Bit;  --  R/W
      --  Enable Packet Sent.
      --  When ipksent =1 the Packet Sense Interrupt will be enabled.

      enext       : Bit;  --  R/W
      --  Enable External Interrupt.
      --  When set to 1 the External Interrupt will be enabled.

      enrxffafull : Bit;  --  R/W
      --  Enable RX FIFO Almost Full.
      --  When set to 1 the RX FIFO Almost Full interrupt will be enabled.

      entxffaem   : Bit;  --  R/W
      --  Enable TX FIFO Almost Empty.
      --  When set to 1 the TX FIFO Almost Empty interrupt will be enabled.

      entxffafull : Bit;  --  R/W
      --  Enable TX FIFO Almost Full.
      --  When set to 1 the TX FIFO Almost Full interrupt will be enabled.

      enfferr     : Bit;  --  R/W
      --  Enable FIFO Underflow/Overflow.
      --  When set to 1 the FIFO Underflow/Overflow interrupt will be enabled.
   end record with Size => Register'Size;
   --  Reset value = 00000000

   for Interrupt_Enable_1_Register use record
      encrcerror  at 0 range 0 .. 0;
      enpkvalid   at 0 range 1 .. 1;
      enpksent    at 0 range 2 .. 2;
      enext       at 0 range 3 .. 3;
      enrxffafull at 0 range 4 .. 4;
      entxffaem   at 0 range 5 .. 5;
      entxffafull at 0 range 6 .. 6;
      enfferr     at 0 range 7 .. 7;
   end record;

   type Interrupt_Enable_2_Register is record
      enpor       : Bit;  --  R/W
      --  Enable POR.
      --  When set to 1 the POR interrupt will be enabled.

      enchiprdy   : Bit;  --  R/W
      --  Enable Chip Ready (XTAL).
      --  When set to 1 the Chip Ready interrupt will be enabled.

      enlbd       : Bit;  --  R/W
      --  Enable Low Battery Detect.
      --  When set to 1 the Low Battery Detect interrupt will be enabled.

      enwut       : Bit;  --  R/W
      --  Enable Wake-Up Timer.
      --  When set to 1 the Wake-Up Timer interrupt will be enabled.

      enrssi      : Bit;  --  R/W
      --  Enable RSSI.
      --  When set to 1 the RSSI Interrupt will be enabled.

      enpreainval : Bit;  --  R/W
      --  Enable Invalid Preamble Detected.
      --  When set to 1 the Invalid Preamble Detected Interrupt will be enabled.

      enpreaval   : Bit;  --  R/W
      --  Enable Valid Preamble Detected.
      --  When set to 1 the Valid Preamble Detected Interrupt will be enabled.

      enswdet     : Bit;  --  R/W
      --  Enable Sync Word Detected.
      --  When set to 1 the Syn Word Detected Interrupt will be enabled.
   end record with Size => Register'Size;
   --  Reset value = 00000011

   for Interrupt_Enable_2_Register use record
      enpor       at 0 range 0 .. 0;
      enchiprdy   at 0 range 1 .. 1;
      enlbd       at 0 range 2 .. 2;
      enwut       at 0 range 3 .. 3;
      enrssi      at 0 range 4 .. 4;
      enpreainval at 0 range 5 .. 5;
      enpreaval   at 0 range 6 .. 6;
      enswdet     at 0 range 7 .. 7;
   end record;

   type Operating_Mode_And_Function_Control_1_Register is record
      xton    : Bit;  --  R/W
      --  READY Mode (Xtal is ON).

      pllon   : Bit;  --  R/W
      --  TUNE Mode (PLL is ON).
      --  When pllon = 1 the PLL will remain enabled in Idle State. This allows
      --  for faster turn-around time at the cost of increased current
      --  consumption in Idle State.

      rxon    : Bit;  --  R/W
      --  RX on in Manual Receiver Mode.
      --  Automatically cleared if Multiple Packets config. is disabled and
      --  a valid packet received.

      txon    : Bit;  --  R/W
      --  TX on in Manual Transmit Mode.
      --  Automatically cleared in FIFO mode once the packet is sent.

      x32ksel : Crystal_Oscillator;  --  R/W
      --  32,768 kHz Crystal Oscillator Select.
      --  0: RC oscillator
      --  1: 32 kHz crystal

      enwt    : Bit;  --  R/W
      --  Enable Wake-Up-Timer.
      --  Enabled when enwt = 1. If the Wake-up-Timer function is enabled it
      --  will operate in any mode and notify the microcontroller through the
      --  GPIO interrupt when the timer expires.

      enlbd   : Bit;  --  R/W
      --  Enable Low Battery Detect.
      --  When this bit is set to 1 the Low Battery Detector circuit and
      --  threshold comparison will be enabled.

      swres   : Bit;  --  R/W
      --  Software Register Reset Bit.
      --  This bit may be used to reset all registers simultaneously to
      --  a DEFAULT state, without the need for sequentially writing to each
      --  individual register. The RESET is accomplished by setting swres = 1.
      --  This bit will be automatically cleared. The user should wait until
      --  the CHIPRDY status flag/interrupt is issued before sending further
      --  SPI commands to the chip.
   end record with Size => Register'Size;
   --  Reset value = 00000001

   for Operating_Mode_And_Function_Control_1_Register use record
      xton    at 0 range 0 .. 0;
      pllon   at 0 range 1 .. 1;
      rxon    at 0 range 2 .. 2;
      txon    at 0 range 3 .. 3;
      x32ksel at 0 range 4 .. 4;
      enwt    at 0 range 5 .. 5;
      enlbd   at 0 range 6 .. 6;
      swres   at 0 range 7 .. 7;
   end record;

   type Operating_Mode_And_Function_Control_2_Register is record
      ffclrtx : Bit;  --  R/W
      --  TX FIFO Reset/Clear.
      --  This has to be a two writes operation: Setting ffclrtx =1 followed
      --  by ffclrtx = 0 will clear the contents of the TX FIFO.

      ffclrrx : Bit;  --  R/W
      --  RX FIFO Reset/Clear.
      --  This has to be a two writes operation: Setting ffclrrx =1 followed
      --  by ffclrrx = 0 will clear the contents of the RX FIFO.

      enldm   : Bit;  --  R/W
      --  Enable Low Duty Cycle Mode.
      --  If this bit is set to 1 then the chip turns on the RX regularly.
      --  The frequency should be set in the Wake-Up Timer Period register,
      --  while the minimum ON time should be set in the Low-Duty Cycle Mode
      --  Duration register. The FIFO mode should be enabled also.

      autotx  : Bit;  --  R/W
      --  Automatic Transmission.
      --  When autotx = 1 the transceiver will enter automatically TX State
      --  when the FIFO is almost full. When the FIFO is empty it will
      --  automatically return to the Idle State.

      rxmpk   : Bit;  --  R/W
      --  RX Multi Packet.
      --  When the chip is selected to use FIFO Mode (dtmod[1:0]) and RX Packet
      --  Handling (enpacrx) then it will fill up the FIFO with multiple valid
      --  packets if this bit is set, otherwise the transceiver will
      --  automatically leave the RX State after the first valid packet has
      --  been received.

      antdiv  : UInt3;  --  R/W
      --  Enable Antenna Diversity.
      --  The GPIO must be configured for Antenna Diversity for the algorithm
      --  to work properly.
   end record with Size => Register'Size;
   --  Reset value = 00000000

   for Operating_Mode_And_Function_Control_2_Register use record
      ffclrtx at 0 range 0 .. 0;
      ffclrrx at 0 range 1 .. 1;
      enldm   at 0 range 2 .. 2;
      autotx  at 0 range 3 .. 3;
      rxmpk   at 0 range 4 .. 4;
      antdiv  at 0 range 5 .. 7;
   end record;

   type Crystal_Oscillator_Load_Capacitance_Register is record
      xlc      : UInt7;  --  R/W
      --  Tuning Capacitance for the 30 MHz XTAL.

      xtalshft : Bit;  --  R/W
      --  Additional capacitance to coarse shift the frequency if xlc is not
      --  sufficient. Not binary with xlc
   end record with Size => Register'Size;
   --  Reset value = 01111111

   for Crystal_Oscillator_Load_Capacitance_Register use record
      xlc      at 0 range 0 .. 6;
      xtalshft at 0 range 7 .. 7;
   end record;

   for Microcontroller_Clock use
     (c30MHz => 0,      c15MHz => 2#001#, c10MHz => 2#010#, c4MHz  => 2#011#,
      c3MHz  => 2#100#, c2MHz  => 2#101#, c1MHz  => 2#110#, c32kHz => 2#111#);

   type Microcontroller_Output_Clock_Register is record
      mclk     : Microcontroller_Clock;  --  R/W
      --  Microcontroller Clock.
      --  Different clock frequencies may be selected for configurable GPIO
      --  clock output. All clock frequencies are created by dividing the XTAL
      --  except for the 32 kHz clock which comes directly from the 32 kHz
      --  RC Oscillator. The mclk[2:0] setting is only valid when xton = 1
      --  except the 111.

      enlfc    : Bit;  --  R/W
      --  Enable Low Frequency Clock.
      --  When enlfc = 1 and the chip is in Sleep mode then the 32.768 kHz
      --  clock will be provided to the microcontroller no matter what the
      --  selection of mclk[2:0] is. For example if mclk = 000, 30 MHz will
      --  be available through the GPIO to output to the microcontroller in
      --  all Idle, TX, or RX states. When the chip is commanded to Sleep mode
      --  the 30 MHz clock will become 32.768 kHz.

      clkt     : UInt2;  --  R/W
      --  Clock Tail.
      --  If enlfc = 0 then it can be useful to provide a few extra cycles for
      --  the microcontroller to complete its operation. Setting the clkt
      --  register will provide the addition cycles of the clock before it
      --  shuts off.

      Reserved : UInt2;  --  R
   end record with Size => Register'Size;
   --  Reset value = xx000110

   for Microcontroller_Output_Clock_Register use record
      mclk     at 0 range 0 .. 2;
      enlfc    at 0 range 3 .. 3;
      clkt     at 0 range 4 .. 5;
      Reserved at 0 range 6 .. 7;
   end record;

   for GPIO_0_Function use
     (Power_On_Reset                  => 2#00000#, --  (output)
      Wake_Up_Timer                   => 2#00001#,
      --  1 when WUT has expired (output)
      Low_Battery_Detect              => 2#00010#,
      --  1 when battery is below threshold setting (output)
      Direct_Digital_Input            => 2#00011#,
      Interrupt_Falling_Edge          => 2#00100#, --  (input)
      Interrupt_Rising_Edge           => 2#00101#, --  (input)
      Interrupt_State_Change          => 2#00110#, --  (input)
      ADC_Analog_Input                => 2#00111#,
      Reserved_Analog_Test_N_Input    => 2#01000#,
      Reserved_Analog_Test_P_Input    => 2#01001#,
      Direct_Digital_Output           => 2#01010#,
      Reserved_Digital_Test_Output    => 2#01011#,
      Reserved_Analog_Test_N_Output   => 2#01100#,
      Reserved_Analog_Test_P_Output   => 2#01101#,
      Reference_Voltage               => 2#01110#, --  (output)
      Data_CLK_Output                 => 2#01111#,
      --  TX/RX Data CLK output to be used in conjunction with TX/RX
      --   Data pin (output)
      TX_Data_Input                   => 2#10000#,
      --  TX Data input for direct modulation (input)
      External_Retransmission_Request => 2#10001#, --  (input)
      TX_State                        => 2#10010#, --  (output)
      TX_FIFO_Almost_Full             => 2#10011#, --  (output)
      RX_Data                         => 2#10100#, --  (output)
      RX_State                        => 2#10101#, --  (output)
      RX_FIFO_Almost_Full             => 2#10110#, --  (output)
      Antenna_1_Switch                => 2#10111#,
      --  Antenna 1 Switch used for antenna diversity (output)
      Antenna_2_Switch                => 2#11000#,
      --  Antenna 2 Switch used for antenna diversity (output)
      Valid_Preamble_Detected         => 2#11001#, --  (output)
      Invalid_Preamble_Detected       => 2#11010#, --  (output)
      Sync_Word_Detected              => 2#11011#, --  (output)
      Clear_Channel_Assessment        => 2#11100#, --  (output)
      VDD                             => 2#11101#,
      GND0                            => 2#11110#,
      GND1                            => 2#11111#);

   type GPIO0_Configuration_Register is record
      gpio    : GPIO_0_Function;  --  R/W
      --  Pin Function Select, see below

      pup     : Bit;  --  R/W
      --  Pullup Resistor Enable on GPIO0.
      --  When set to 1 a 200 kOm resistor is connected internally between
      --  VDD and the pin if the GPIO is configured as a digital input.

      gpiodrv : UInt2;  --  R/W
      --  GPIO Driving Capability Setting.
   end record with Size => Register'Size;
   --  Reset value = 00000000

   for GPIO0_Configuration_Register use record
      gpio    at 0 range 0 .. 4;
      pup     at 0 range 5 .. 5;
      gpiodrv at 0 range 6 .. 7;
   end record;

   for GPIO_1_Function use
     (Inverted_Power_On_Reset         => 2#00000#, --  (output)
      Wake_Up_Timer                   => 2#00001#,
      --  1 when WUT has expired (output)
      Low_Battery_Detect              => 2#00010#,
      --  1 when battery is below threshold setting (output)
      Direct_Digital_Input            => 2#00011#,
      Interrupt_Falling_Edge          => 2#00100#, --  (input)
      Interrupt_Rising_Edge           => 2#00101#, --  (input)
      Interrupt_State_Change          => 2#00110#, --  (input)
      ADC_Analog_Input                => 2#00111#, --  (input)
      Reserved_Analog_Test_N_Input    => 2#01000#,
      Reserved_Analog_Test_P_Input    => 2#01001#,
      Direct_Digital_Output           => 2#01010#,
      Reserved_Digital_Test_Output    => 2#01011#,
      Reserved_Analog_Test_N_Output   => 2#01100#,
      Reserved_Analog_Test_P_Output   => 2#01101#,
      Reference_Voltage               => 2#01110#, --  (output)
      Data_CLK_Output                 => 2#01111#,
      --  TX/RX Data CLK output to be used in conjunction with TX/RX
      --   Data pin (output)
      TX_Data_Input                   => 2#10000#,
      --  TX Data input for direct modulation (input)
      External_Retransmission_Request => 2#10001#, --  (input)
      TX_State                        => 2#10010#, --  (output)
      TX_FIFO_Almost_Full             => 2#10011#, --  (output)
      RX_Data                         => 2#10100#, --  (output)
      RX_State                        => 2#10101#, --  (output)
      RX_FIFO_Almost_Full             => 2#10110#, --  (output)
      Antenna_1_Switch                => 2#10111#,
      --  Antenna 1 Switch used for antenna diversity (output)
      Antenna_2_Switch                => 2#11000#,
      --  Antenna 2 Switch used for antenna diversity (output)
      Valid_Preamble_Detected         => 2#11001#, --  (output)
      Invalid_Preamble_Detected       => 2#11010#, --  (output)
      Sync_Word_Detected              => 2#11011#, --  (output)
      Clear_Channel_Assessment        => 2#11100#, --  (output)
      VDD                             => 2#11101#,
      GND0                            => 2#11110#,
      GND1                            => 2#11111#);

   type GPIO1_Configuration_Register is record
      gpio    : GPIO_1_Function;  --  R/W
      --  Pin Function Select, see below

      pup     : Bit;  --  R/W
      --  Pullup Resistor Enable on GPIO0.
      --  When set to 1 a 200 kOm resistor is connected internally between
      --  VDD and the pin if the GPIO is configured as a digital input.

      gpiodrv : UInt2;  --  R/W
      --  GPIO Driving Capability Setting.
   end record with Size => Register'Size;
   --  Reset value = 00000000

   for GPIO1_Configuration_Register use record
      gpio    at 0 range 0 .. 4;
      pup     at 0 range 5 .. 5;
      gpiodrv at 0 range 6 .. 7;
   end record;

   for GPIO_2_Function use
     (Microcontroller_Clocks          => 2#00000#,
      Wake_Up_Timer                   => 2#00001#,
      --  1 when WUT has expired (output)
      Low_Battery_Detect              => 2#00010#,
      --  1 when battery is below threshold setting (output)
      Direct_Digital_Input            => 2#00011#,
      Interrupt_Falling_Edge          => 2#00100#, --  (input)
      Interrupt_Rising_Edge           => 2#00101#, --  (input)
      Interrupt_State_Change          => 2#00110#, --  (input)
      ADC_Analog_Input                => 2#00111#, --  (input)
      Reserved_Analog_Test_N_Input    => 2#01000#,
      Reserved_Analog_Test_P_Input    => 2#01001#,
      Direct_Digital_Output           => 2#01010#,
      Reserved_Digital_Test_Output    => 2#01011#,
      Reserved_Analog_Test_N_Output   => 2#01100#,
      Reserved_Analog_Test_P_Output   => 2#01101#,
      Reference_Voltage               => 2#01110#, --  (output)
      Data_CLK_Output                 => 2#01111#,
      --  TX/RX Data CLK output to be used in conjunction with TX/RX
      --   Data pin (output)
      TX_Data_Input                   => 2#10000#,
      --  TX Data input for direct modulation (input)
      External_Retransmission_Request => 2#10001#, --  (input)
      TX_State                        => 2#10010#, --  (output)
      TX_FIFO_Almost_Full             => 2#10011#, --  (output)
      RX_Data                         => 2#10100#, --  (output)
      RX_State                        => 2#10101#, --  (output)
      RX_FIFO_Almost_Full             => 2#10110#, --  (output)
      Antenna_1_Switch                => 2#10111#,
      --  Antenna 1 Switch used for antenna diversity (output)
      Antenna_2_Switch                => 2#11000#,
      --  Antenna 2 Switch used for antenna diversity (output)
      Valid_Preamble_Detected         => 2#11001#, --  (output)
      Invalid_Preamble_Detected       => 2#11010#, --  (output)
      Sync_Word_Detected              => 2#11011#, --  (output)
      Clear_Channel_Assessment        => 2#11100#, --  (output)
      VDD                             => 2#11101#,
      GND0                            => 2#11110#,
      GND1                            => 2#11111#);

   type GPIO2_Configuration_Register is record
      gpio    : GPIO_2_Function;  --  R/W
      --  Pin Function Select, see below

      pup     : Bit;  --  R/W
      --  Pullup Resistor Enable on GPIO0.
      --  When set to 1 a 200 kOm resistor is connected internally between
      --  VDD and the pin if the GPIO is configured as a digital input.

      gpiodrv : UInt2;  --  R/W
      --  GPIO Driving Capability Setting.
   end record with Size => Register'Size;
   --  Reset value = 00000000

   for GPIO2_Configuration_Register use record
      gpio    at 0 range 0 .. 4;
      pup     at 0 range 5 .. 5;
      gpiodrv at 0 range 6 .. 7;
   end record;

   type IO_Port_Configuration_Register is record
      dio0     : Bit;  --  R/W
      --  Direct I/O for GPIO0.
      --  If the GPIO0 is configured to be a direct output then the value on
      --  the GPIO pin can be set here. If the GPIO0 is configured to be a
      --  direct input then the value of the pin can be read here.

      dio1     : Bit;  --  R/W
      --  Direct I/O for GPIO1.
      --  If the GPIO1 is configured to be a direct output then the value on
      --  the GPIO pin can be set here. If the GPIO1 is configured to be a
      --  direct input then the value of the pin can be read here.

      dio2     : Bit;  --  R/W
      --  Direct I/O for GPIO2.
      --  If the GPIO2 is configured to be a direct output then the value on
      --  the GPIO pin can be set here. If the GPIO2 is configured to be a
      --  direct input then the value of the pin can be read here.

      itsdo    : Bit;  --  R/W
      --  Interrupt Request Output on the SDO Pin.
      --  nIRQ output is present on the SDO pin if this bit is set and the
      --  nSEL input is inactive (high).

      extitst0 : Bit;  --  R
      --  External Interrupt Status.
      --  If the GPIO0 is programmed to be an external interrupt source then
      --  the status can be read here.

      extitst1 : Bit;  --  R
      --  External Interrupt Status.
      --  If the GPIO1 is programmed to be an external interrupt source then
      --  the status can be read here.

      extitst2 : Bit;  --  R
      --  External Interrupt Status.
      --  If the GPIO2 is programmed to be an external interrupt source then
      --  the status can be read here.
      Reserved : Bit;  --  R
   end record with Size => Register'Size;
   --  Reset value = 00000000

   for IO_Port_Configuration_Register use record
      dio0     at 0 range 0 .. 0;
      dio1     at 0 range 1 .. 1;
      dio2     at 0 range 2 .. 2;
      itsdo    at 0 range 3 .. 3;
      extitst0 at 0 range 4 .. 4;
      extitst1 at 0 range 5 .. 5;
      extitst2 at 0 range 6 .. 6;
      Reserved at 0 range 7 .. 7;
   end record;

   for ADC_Input_Source use
     (Internal_Temperature_Sensor => 2#000#,
      GPIO0_Single_Ended          => 2#001#,
      GPIO1_Single_Ended          => 2#010#,
      GPIO2_Single_Ended          => 2#011#,
      GPIO0_GPIO1_differential    => 2#100#,
      GPIO1_GPIO2_differential    => 2#101#,
      GPIO0_GPIO2_differential    => 2#110#,
      GND                         => 2#111#);

   type ADC_Configuration_Register is record
      adcgain  : UInt2;  --  R/W
      --  ADC Sensor Amplifier Gain Selection.
      --  The full scale range of the internal 8-bit ADC in differential mode
      --  (see adcsel)

      adcref   : UInt2;  --  R/W
      --  ADC Reference Voltage Selection.
      --  The reference voltage of the internal 8-bit ADC can be selected

      adcsel   : ADC_Input_Source;  --  R/W
      --  ADC Input Source Selection.
      --  The internal 8-bit ADC input source can be selected

      adcstart : Bit;  --  R/W
      --  ADC Measurement Start Bit.
      --  Set this bit=1 starts the ADC measurement process. This bit self-
      --  clears during the measurement cycle and returns high when the
      --  measurement is complete. The conversion process is fast; reading
      --  this bit may always appear to return a 1.
   end record with Size => Register'Size;
   --  Reset value = 00000000

   for ADC_Configuration_Register use record
      adcgain  at 0 range 0 .. 1;
      adcref   at 0 range 2 .. 3;
      adcsel   at 0 range 4 .. 6;
      adcstart at 0 range 7 .. 7;
   end record;

   type ADC_Sensor_Amplifier_Offset_Register is record
      adcoffs  : UInt4;  --  R/W
      --  *Note: The offset can be calculated as
      --    Offset = adcoffs[2:0] x VDD/1000; MSB = adcoffs[3] = Sign bit.
      Reserved : UInt4;  --  R
   end record;

   for ADC_Sensor_Amplifier_Offset_Register use record
      adcoffs  at 0 range 0 .. 3;
      Reserved at 0 range 4 .. 7;
   end record;

   subtype ADC_Value_Register is UInt8;  --  R
   --  Internal 8 bit ADC Output Value.
   --  Reset value = xxxxxxxx

   for Temperature_Sensor_Range use
     (From_Minus_64_To_64C  => 2#00#,
      From_Minus_64_To_192C => 2#01#,
      From_Minus_40_To_216F => 2#10#,
      From_0_To_12C         => 2#11#);

   type Temperature_Sensor_Calibration_Register is record
      tstrim   : UInt4;  --  R/W
      --  Temperature Sensor Trim Value.

      entstrim : Bit;  --  R/W
      --  Temperature Sensor Trim Enable.

      entsoffs : Bit;  --  R/W
      --  Temperature Sensor Offset to Convert from K to ºC. Default is 1.
      --  Test mode only, to use set tsrange and entsoffs to 0.

      tsrange  : Temperature_Sensor_Range;  --  R/W
      --  Temperature Sensor Range Selection (FS range is 0-1024 mV)
   end record with Size => Register'Size;
   --  Reset value = 00100000

   for Temperature_Sensor_Calibration_Register use record
      tstrim   at 0 range 0 .. 3;
      entstrim at 0 range 4 .. 4;
      entsoffs at 0 range 5 .. 5;
      tsrange  at 0 range 6 .. 7;
   end record;

   subtype Temperature_Value_Offset_Register is UInt8;  --  R/W
   --  This value is added to the measured temperature value.
   --    (MSB, tvoffs[8]: sign bit).
   --  Reset value = 00000000

   type Wake_Up_Timer_Period_1_Register is record
      wtr      : UInt4;  --  R/W
      --  Wake Up Timer Exponent (R) Value*.
      --  Maximum value for R is decimal 20. A value greater than 20 will
      --  yield a result as if 20 were written. R Value = 0 can be written
      --  here.
      --  *Note: The period of the wake-up timer can be calculated as
      --  TWUT = (4 x M x 2R)/32.768 ms. R = 0 is allowed, and the maximum
      --  value for R is decimal 20. A value greater than 20 will result in
      --  the same as if 20 was written.

      Reserved : UInt3;  --  R/W
   end record with Size => Register'Size;
   --  Reset value = xxx00011

   --  Note: If a new configuration is needed (e.g., for the WUT or the LDC),
   --  proper functionality is required. The function must first be disabled,
   --  then the settings changed, then enabled back on.

   for Wake_Up_Timer_Period_1_Register use record
      wtr      at 0 range 0 .. 3;
      Reserved at 0 range 4 .. 7;
   end record;

   subtype Wake_Up_Timer_Period_Register is UInt8;  --  R/W
   --  wtm2[15:8] wtm3[7:0]
   --  *Note: The period of the wake-up timer can be calculated as
   --    TWUT = (4 x M x 2R)/32.768 ms.
   --  Reset value = 00000000 00000001

   subtype Wake_Up_Timer_Value_Register is UInt8;  --  R
   --  wtv1[15:8] wtv2[7:0]
   --  Wake Up Timer Current Mantissa (M) Value. The value in wtv[15:0]
   --  reflects the current count value of the timer.
   --  Reset value = xxxxxxxx

   subtype Low_Duty_Cycle_Mode_Duration_Register is UInt8;  --  R/W
   --  Low-Duty Cycle Mode Duration (LDC)*.
   --  If enabled, the LDC will start together when the WUT is supposed to
   --  start, and the duration of the LDC is specified by the address 19h and
   --  the equation that goes with it. In order for the LDC to work, the LDC
   --  value has to be smaller than the M value specified in registers 15h
   --  and 16h.
   --  LDC = 0 is not allowed here. Write at least decimal 1.
   --  *Note: The period of the low-duty cycle ON time can be calculated as
   --  TLDC_ON = (4 x LDC x 2R)/32.768 ms. R is the same as in the wake-up
   --  timer setting in "Register 14h. Wake-Up Timer Period 1". The LDC works
   --  in conjunction with the WUT. The LDC period must be specified to be
   --  smaller than the WUT period. (i.e., the LDC register must be smaller
   --  than the M register). The LDC may not be programmed to 0.
   --  Reset value = 00000001

   type Low_Battery_Detector_Threshold_Register is record
      lbdt     : UInt5;  --  R/W
      --  Low Battery Detector Threshold.
      --  This threshold is compared to Battery Voltage Level. If the Battery
      --  Voltage is less than the threshold the Low Battery Interrupt is set.
      --  Default = 2.7 V.*
      Reserved : UInt3;  --  R
   end record with Size => Register'Size;
   --  Reset value = xxx10100
   --  *Note: The threshold can be calculated as Vthreshold =
   --    1.7 + lbdt x 50 mV.

   for Low_Battery_Detector_Threshold_Register use record
      lbdt     at 0 range 0 .. 4;
      Reserved at 0 range 5 .. 7;
   end record;

   type Battery_Voltage_Level_Register is record
      vbat     : UInt5;  --  R
      --  Battery Voltage Level.
      --  The battery voltage is converted by a 5 bit ADC if the LBD bit D6 of
      --  Reg 07h is also set. In Sleep Mode the register is updated in every
      --  1 s. In other states it measures continuously. The measured voltage
      --  is calculated by the following formula:
      --    Vbat_meas=1.7V + vbat[4:0] x 50 mV
      Reserved : UInt3;  --  R
   end record with Size => Register'Size;
   --  Reset value = xxxxxxxx

   for Battery_Voltage_Level_Register use record
      vbat     at 0 range 0 .. 4;
      Reserved at 0 range 5 .. 7;
   end record;

   type IF_Filter_Bandwidth_Register is record
      filset      : UInt4;  --  R/W
      --  IF Filter Coefficient Sets.
      --  Selects one of 15 pre-calculated sets of digital FIR filter tap
      --  coefficients. Along with the decimation ratios selected by
      --  dwn3_bypass and ndec_exp, the filter coefficients determine the
      --  bandwidth of the IF filter.

      ndec_exp    : UInt3;  --  R/W
      --  IF Filter Decimation Rates.
      --  The oversampled data in the receive data path is decimated by a
      --  factor of 2^ndec_exp. A higher decimation factor (i.e., larger value
      --  of ndec_exp) results in a lower IF filter bandwidth.

      dwn3_bypass : Bit;  --  R/W
      --  Bypass Decimate-by-3 Stage.
      --  If set, results in bypassing a decimate-by-3 stage in the path of
      --  the oversampled data path for the digital filter for the IF
      --  bandwidth.
   end record with Size => Register'Size;
   --  Reset value = 00000001

   for IF_Filter_Bandwidth_Register use record
      filset      at 0 range 0 .. 3;
      ndec_exp    at 0 range 4 .. 6;
      dwn3_bypass at 0 range 7 .. 7;
   end record;

   type AFC_Loop_Gearshift_Override_Register is record
      ph0size  : Bit;  --  R/W
      --  If low, we will reset the Preamble detector if there are 5
      --  consecutive zero phases. If high, the reset will happen after 3
      --  consecutive zero phases.

      matap    : Bit;  --  R/W
      --  Number of taps for moving average filter during Antenna Diversity
      --  RSSI evaluation. Allows for reduced noise variation on measured RSSI
      --  value but with slower update rate. If high (1), filter tap
      --  length = 8*Tb. If low (0=default), filter tap length = 8*Tb prior to
      --  first PREAMBLE_VALID, and 4*Tb thereafter.

      p5bypass : Bit;  --  R/W
      --  If high (1), select 0dB bias for the second phase antenna selection,
      --  if low (0), select 1.5 dB. The default is (1), selecting 0 dB.

      afcgearh : UInt3;  --  R/W
      --  AFC High Gear Setting. Feedback loop gain during AFC setting process
      --  is proportional to 2^(-afcgearh[2:0]).

      enafc    : Bit;  --  R/W
      --  AFC Enable.

      afcbd    : Bit;  --  R/W
      --  AFC Wideband Enable (active high). If set, the IF filter bandwidth
      --  is reduced after preamble detection, in order to optimize RX
      --  sensitivity. The IF filter bandwidth used during preamble detection
      --  is programmed by the FILSET, NDEC, and DWN3BYPASS parameters in SPI
      --  Register 1CH. After preamble detection, the chip automatically
      --  selects the next lower IF filter bandwidth by internally decreasing
      --  the FILSET parameter by 1. The resulting filter bandwidth may be
      --  determined from the bandwidth table provided under the description
      --  for SPI Register 1CH.
   end record with Size => Register'Size;
   --  Reset value = 01000100

   for AFC_Loop_Gearshift_Override_Register use record
      ph0size  at 0 range 0 .. 0;
      matap    at 0 range 1 .. 1;
      p5bypass at 0 range 2 .. 2;
      afcgearh at 0 range 3 .. 5;
      enafc    at 0 range 6 .. 6;
      afcbd    at 0 range 7 .. 7;
   end record;

   type AFC_Timing_Control_Register is record
      anwait      : UInt3;  --  R/W
      --  Antenna switching wait time. Number of bit periods between toggling
      --  selection of antennas in AntDiv mode, prior to reception of first
      --  PREAMBLE_VALID.
      --  Number of bit periods = (anwait + 2) x 4 + 3 (when AFC = enabled)
      --  Number of bit periods = (anwait + 2) x 2 + 3 (when AFC = disabled)
      --  Default value = 3'b010 = 19 bit periods (AFC = enabled).

      shwait      : UInt3;  --  R/W
      --  Short wait periods after AFC correction used before preamble is
      --  detected. Short wait=(RegValue+1) x 2Tb. If set to 0 then no AFC
      --  correction will occur before preamble detect, i.e., AFC will be
      --  disabled.

      swant_timer : UInt2;  --  R/W
      --  Additional number of bit periods to wait for RSSI value to stabilize
      --  during Antenna Diversity 2nd phase antenna evaluation. If matap=0,
      --  total wait time=8 x Tb+swant_timer[1:0]. If matap=1, total wait
      --  time=12*Tb+swant_timer{1:0]. Effective only during Antenna Diversity.
   end record with Size => Register'Size;
   --  Reset value = xx001010

   for AFC_Timing_Control_Register use record
      anwait      at 0 range 0 .. 2;
      shwait      at 0 range 3 .. 5;
      swant_timer at 0 range 6 .. 7;
   end record;

   type Clock_Recovery_Gearshift_Override_Register is record
      crslow   : UInt3;  --  R/W
      --  Clock Recovery Slow Gearshift Value.

      crfast   : UInt3;  --  R/W
      --  Clock Recovery Fast Gearshift Value.

      Reserved : UInt2;  --  R/W
   end record with Size => Register'Size;
   --  Reset value = 00000011

   for Clock_Recovery_Gearshift_Override_Register use record
      crslow   at 0 range 0 .. 2;
      crfast   at 0 range 3 .. 5;
      Reserved at 0 range 6 .. 7;
   end record;

   subtype Clock_Recovery_Oversampling_Rate_Register is UInt8;  --  R/W
   --  Oversampling Rate.
   --  3 LSBs are the fraction, default = 0110 0100 = 12.5 clock cycles
   --  per data bit
   --  Reset value = 01100100

   type Clock_Recovery_Offset_2_Register is record
      ncoff     : UInt4;  --  R/W
      --  ncoff[19:16]
      --  NCO Offset.

      skip2phth : Bit;    --  R/W
      --  Skip 2nd Phase Ant Div Threshold. Threshold for skipping the 2nd
      --  phase of RSSI detection during antenna diversity algorithm. 0=16 dB
      --  (default), 1=11 dB. NOT RECOMMENDED FOR USER CONFIGURATION.

      rxosr     : UInt3;  --  R/W
      --  rxosr[10:8]
      --  Oversampling Rate. Upper bits.
   end record with Size => Register'Size;
   --  Reset value = 00000011

   for Clock_Recovery_Offset_2_Register use record
      ncoff     at 0 range 0 .. 3;
      skip2phth at 0 range 4 .. 4;
      rxosr     at 0 range 5 .. 7;
   end record;

   subtype Clock_Recovery_Offset_Register is UInt8;  --  R/W
   --  ncoff1[15:8] ncoff0[7:0]
   --  Reset value = 01000111 10101110

   type Clock_Recovery_Timing_Loop_Gain_1_Register is record
      crgain    : UInt3;  --  R/W
      --  crgain[10:8]
      --  Clock Recovery Timing Loop Gain.

      cgainx2   : Bit;    --  R/W
      --  Multiplying the CR Gain by 2.

      rxncocomp : Bit;  --  R/W
      --  Receive Compensation Enable for High Data Rate Offset.

      Reserved  : UInt3;  --  R/W
   end record with Size => Register'Size;
   --  Reset value = 00000010

   for Clock_Recovery_Timing_Loop_Gain_1_Register use record
      crgain    at 0 range 0 .. 2;
      cgainx2   at 0 range 3 .. 3;
      rxncocomp at 0 range 4 .. 4;
      Reserved  at 0 range 5 .. 7;
   end record;

   subtype Clock_Recovery_Timing_Loop_Gain_0_Register is UInt8;  --  R/W
   --  crgain[7:0]
   --  Clock Recovery Timing Loop Gain.
   --  Reset value = 10001111

   subtype Received_Signal_Strength_Indicator_Register is UInt8;  --  R
   --  Reset value = xxxxxxxx

   subtype RSSI_Threshold_for_Clear_Channel_Indicator_Register is UInt8;
   --  R/W
   --  Interrupt is set if the RSSI value is above this threshold.
   --  Reset value = 00011110

   subtype Antenna_Diversity_Register is UInt8;  --  R
   --  Measured RSSI Value on Antenna 1 and 2.
   --  Reset value = xxxxxxxx

   subtype AFC_Limiter_Register is UInt8;  --  R/W
   --  AFC limiter value.
   --  Reset value = 00000000

   subtype AFC_Correction_Register is UInt8;  --  R
   --  afc_corr[9:2]
   --  AFC loop correction values [9:2] (MSBs only). Values are updated once,
   --  after sync word is found during receiving. See also address 2Ch.
   --  Reset value = xxxxxxxx

   type OOK_Counter_Value_1_Register is record
      ookcnt    : UInt3;  --  R/W
      --  ookcnt[10:8]
      --  OOK counter [10:8] =OOK counter Value MSBs. This counter value will
      --  affect the OOK AGC's decay time.

      madeten   : Bit;    --  R/W
      --  MA_Enable.
      --  madeten= when '1' (default), Moving Average Detector for OOK Modem
      --  is enabled. Provides best sensitivity, but requires DC-balanced data
      --  (e.g., Manchester data) and is more sensitive to co-channel
      --  interference. Peak Detector output is logically AND'ed with Moving
      --  Average Detector output.

      peakdeten : Bit;  --  R/W
      --  Peak Detector Enable.
      --  peakdeten= when '1' (default), Peak Detector for OOK Modem is
      --  enabled. Provides improved performance in presence of co-channel
      --  interferers, at slight reduction of sensitivity. Peak Detector output
      --  is logically AND'ed with Moving Average Detector output.

      ookfrzen  : Bit;  --  R/W
      --  OOK Freeze.
      --  ookfrzen= when '0' (default), AGC and OOK Moving Average Detector
      --  threshold operate continuously. When '1', AGC and OOK MA Detector
      --  threshold operate until PREAMBLE_VALID signal is detected; values
      --  are frozen thereafter. Recommended for use with non-Manchestered
      --  payload data.

      afc_corr  : UInt2; -- R
      --  afc_corr[1:0]
      --  AFC Correction Values.
      --  AFC loop correction values [1:0] (LSBs). Values are updated once,
      --  after sync word is found during receiving. See also address 2Bh.
   end record with Size => Register'Size;
   --  Reset value = 00011000

   for OOK_Counter_Value_1_Register use record
      ookcnt    at 0 range 0 .. 2;
      madeten   at 0 range 3 .. 3;
      peakdeten at 0 range 4 .. 4;
      ookfrzen  at 0 range 5 .. 5;
      afc_corr  at 0 range 6 .. 7;
   end record;

   subtype OOK_Counter_Value_2_Register is UInt8;  --  R/W
   --  ookcnt[7:0]
   --  OOK counter value LSBs. This counter value will affect the OOK AGC's
   --  decay time.
   --  Reset value = 10111100

   type Slicer_Peak_Holder_Register is record
      decay    : UInt4;  --  R/W
      --  Decay. decay[3:0]=OOK Peak Detector decay time. Peak detector value
      --  discharges at rate proportional to 2^(-decay[3:0]). OOK slicing
      --  threshold is set 6 dB below peak detector value. Effective only when
      --  OOK Peak Detector is enabled.

      attack   : UInt3;    --  R/W
      --  Attack. attack [2:0}=OOK Peak Detector attack time. Peak detector
      --  value charges up at rate proportional to 2^(-attack[2:0]). OOK
      --  slicing threshold is set 6 dB below peak detector value. Effective
      --  only when OOK Peak Detector is enabled.

      Reserved : Bit;  --  R/W
   end record with Size => Register'Size;
   --  Reset value = 00101100

   for Slicer_Peak_Holder_Register use record
      decay    at 0 range 0 .. 3;
      attack   at 0 range 4 .. 6;
      Reserved at 0 range 7 .. 7;
   end record;

   for CRC_Polynomial use
     (CCITT    => 2#00#,
      CRC_16   => 2#01#,
      IEC_16   => 2#10#,
      Biacheva => 2#11#);

   type Data_Access_Control_Register is record
      crc      : CRC_Polynomial;  --  R/W

      encrc    : Bit;    --  R/W
      --  CRC Enable.
      --  Cyclic Redundancy Check generation is enabled if this bit is set.

      enpactx  : Bit;  --  R/W
      --  Enable Packet TX Handling.
      --  If FIFO Mode (dtmod = 10) is being used automatic packet handling
      --  may be enabled. Setting enpactx = 1 will enable automatic packet
      --  handling in the TX path. Register 30-4D allow for various
      --  configurations of the packet structure. Setting enpactx = 0 will
      --  not do any packet handling in the TX path. It will only transmit
      --  what is loaded to the FIFO.

      skip2ph  : Bit;  --  R/W
      --  Skip 2nd Phase of Preamble Detection.
      --  If set, we skip the second phase of the preamble detection
      --  (under certain conditions) if antenna diversity is enabled.

      crcdonly : Bit;  --  R/W
      --  CRC Data Only Enable.
      --  When this bit is set to 1 the CRC is calculated on and checked
      --  against the packet data fields only.

      lsbfrst  : Bit;  --  R/W
      --  LSB First Enable.
      --  The LSB of the data will be transmitted/received first if this
      --  bit is set.

      enpacrx  : Bit;  --  R/W
      --  Enable Packet RX Handling.
      --  If FIFO Mode (dtmod = 10) is being used automatic packet handling
      --  may be enabled. Setting enpacrx = 1 will enable automatic packet
      --  handling in the RX path. Register 30-4D allow for various
      --  configurations of the packet structure. Setting enpacrx = 0 will
      --  not do any packet handling in the RX path. It will only receive
      --  everything after the sync word and fill up the RX FIFO.
   end record with Size => Register'Size;
   --  Reset value = 10001101

   for Data_Access_Control_Register use record
      crc      at 0 range 0 .. 1;
      encrc    at 0 range 2 .. 2;
      enpactx  at 0 range 3 .. 3;
      skip2ph  at 0 range 4 .. 4;
      crcdonly at 0 range 5 .. 5;
      lsbfrst  at 0 range 6 .. 6;
      enpacrx  at 0 range 7 .. 7;
   end record;

   type EZMAC_Status_Register is record
      pksent   : Bit;  --  R
      --  Packet Sent.
      --  A pksent = 1 a packet has been sent by the radio. (Same bit as in
      --  register 03, but reading it does not reset the IRQ)

      pktx     : Bit;  --  R
      --  Packet Transmitting.
      --  When pktx = 1 the radio is currently transmitting a packet.

      crcerror : Bit;  --  R
      --  CRC Error.
      --  When crcerror = 1 a Cyclic Redundancy Check error has been detected.
      --  (Same bit as in register 03, but reading it does not reset the IRQ)

      pkvalid  : Bit;  --  R
      --  Valid Packet Received.
      --  When a pkvalid = 1 a valid packet has been received by the receiver.
      --  (Same bit as in register 03, but reading it does not reset the IRQ)

      pkrx     : Bit;  --  R
      --  Packet Receiving.
      --  When pkrx = 1 the radio is currently receiving a valid packet.

      pksrch   : Bit;  --  R
      --  Packet Searching.
      --  When pksrch = 1 the radio is searching for a valid packet.

      rxcrc1   : Bit;  --  R
      --  If high, it indicates the last CRC received is all ones.
      --  May indicated Transmitter underflow in case of CRC error.

      Reserved : Bit;
   end record with Size => Register'Size;
   --  Reset value = 00000000

   for EZMAC_Status_Register use record
      pksent   at 0 range 0 .. 0;
      pktx     at 0 range 1 .. 1;
      crcerror at 0 range 2 .. 2;
      pkvalid  at 0 range 3 .. 3;
      pkrx     at 0 range 4 .. 4;
      pksrch   at 0 range 5 .. 5;
      rxcrc1   at 0 range 6 .. 6;
      Reserved at 0 range 7 .. 7;
   end record;

   type Header_Control_1_Register is record
      hdch   : UInt4;  --  R/W
      --  Received Header Bytes to be Checked Against the Check Header Bytes.
      --  One hot encoding. The receiver will use hdch[2:0] to know the
      --  position of the Header Bytes.
      --  0000:No Received Header check
      --  0001:Received Header check for byte 0.
      --  0010:Received Header check for bytes 1.
      --  0011:Received header check for bytes 0 & 1.
      --  0100:...
      bcen   : UInt4;  --  R/W
      --  Broadcast Address (FFh) Check Enable.
      --  If it is enabled together with Header Byte Check then the header
      --  check is OK if the incoming header byte equals with the appropriate
      --  check byte or FFh). One hot encoding.
      --  0000:No broadcast address enable.
      --  0001:Broadcast address enable for header byte 0.
      --  0010:Broadcast address enable for header byte 1.
      --  0011:Broadcast address enable for header bytes 0 & 1.
      --  0100:...
   end record with Size => Register'Size;
   --  Reset value = 00001100

   for Header_Control_1_Register use record
      hdch at 0 range 0 .. 3;
      bcen at 0 range 4 .. 7;
   end record;

   type Header_Control_2_Register is record
      prealen  : Bit;  --  R/W
      --  prealen[8]
      --  MSB of Preamble Length.
      --  See register Preamble Length.

      synclen  : Synchronization_Word_Length;  --  R/W
      --  Synchronization Word Length.
      --  The value in this register corresponds to the number of bytes used
      --  in the Synchronization Word. The synchronization word bytes are
      --  transmitted in descending order.
      --  00: Synchronization Word 3
      --  01: Synchronization Word 3 and 2
      --  10: Synchronization Word 3 and 2 and 1
      --  11: Synchronization Word 3 and 2 and 1 and 0

      fixpklen : Bit;  --  R/W
      --  Fix Transmit/Receive Packet Length.
      --  When fixpklen = 1 the packet length (pklen[7:0]) is not included in
      --  the transmit header. When fixpklen = 0 the packet length is included
      --  in the transmit header. In receive mode, if this bit is set the
      --  packet length is obtained from the pklen[7:0] field in Reg 3Eh;
      --  otherwise the packet length is obtained from the received header
      --  packet length byte.

      hdlen    : Header_Length;  --  R/W
      --  Header Length.
      --  Transmit/Receive Header Length. Length of header used if packet
      --  handler is enabled for TX/RX (enpactx/rx). Headers are
      --  transmitted/received in descending order.
      --  000: No TX/RX header
      --  001: Header 3
      --  010: Header 3 and 2
      --  011: Header 3 and 2 and 1
      --  100: Header 3 and 2 and 1 and 0

      skipsyn  : Bit;  --  R/W
      --  Skipsyn.
      --  Skip Sync Word search timeout. If high, the system will ignore the
      --  search timeout period when failing to find Sync Word and will not
      --  return to searching for Preamble. Setting this bit does not
      --  eliminate the search for Sync Word. Proper detection of Sync Word
      --  remains necessary in FIFO mode in order to determine the start of
      --  the Payload field and to thus store the correct bytes in the RX FIFO.
   end record with Size => Register'Size;
   --  Reset value = 00100010

   for Header_Control_2_Register use record
      prealen  at 0 range 0 .. 0;
      synclen  at 0 range 1 .. 2;
      fixpklen at 0 range 3 .. 3;
      hdlen    at 0 range 4 .. 6;
      skipsyn  at 0 range 7 .. 7;
   end record;

   subtype Preamble_Length_Register is UInt8;  --  R/W
   --  Preamble Length.
   --  The value in the prealen[8:0] register corresponds to the number of
   --  nibbles (4 bits) in the packet. For example prealen[8:0] = '000001000'
   --  corresponds to a preamble length of 32 bits (8 x 4bits) or 4 bytes.
   --  The maximum preamble length is prealen[8:0] = 111111111 which
   --  corresponds to a 255 bytes Preamble. Writing 0 will have the same result
   --  as if writing 1, which corresponds to one single nibble of preamble.
   --  Reset value = 00001000

   type Preamble_Detection_Control_1_Register is record
      rssi_offset : UInt3;  --  R/W
      --  Value added as offset to RSSI calculation. Every increment in this
      --  register results in an increment of +4 dB in the RSSI.

      preath      : UInt5;  --  R/W
      --  Preamble Detection Threshold. The value in the preath[4:0] register
      --  corresponds to the number of nibbles (4 bits) of preamble pattern
      --  (i.e., 01010...) that must be received correctly, before a
      --  PREAMBLE_VALID signal is issued. This threshold helps guard against
      --  false preamble detection upon noise.
   end record with Size => Register'Size;
   --  Reset value = 00101010

   for Preamble_Detection_Control_1_Register use record
      rssi_offset at 0 range 0 .. 2;
      preath      at 0 range 3 .. 7;
   end record;

   subtype Synchronization_Word_Register is UInt8;  --  R/W
   --  sync3[31:24] sync2[23:16] sync1[15:8] sync0[7:0]
   --  Reset value = 00101101 11010100 00000000 00000000

   subtype Transmit_Header_Register is UInt8;  --  R/W
   --  txhd3[31:24] txhd2[23:16] txhd1[15:8] txhd0[7:0]
   --  Reset value = 00000000

   subtype Packet_Length_Register is UInt8;  --  R/W
   --  Packet Length.
   --  The value in the pklen[7:0] register corresponds directly to the number
   --  of bytes in the Packet. For example pklen[7:0] = '00001000' corresponds
   --  to a packet length of 8 bytes. The maximum packet length is
   --  pklen[7:0] = '11111111', a 255 byte packet. Writing 0 is possible, in
   --  this case we do not send any data in the packet. During RX, if
   --  fixpklen = 1, this will specify also the Packet Length for RX mode.
   --  Reset value = 00000000

   subtype Check_Header_Register is UInt8;  --  R/W
   --  chhd3[31:24] chhd2[23:16] chhd1[15:8] chhd0[7:0]
   --  Reset value = 00000000

   subtype Header_Enable_Register is UInt8;  --  R/W
   --  hden3[31:24] hden2[23:16] hden1[15:8] hden0[7:0]
   --  Reset value = 11111111 11111111 11111111 11111111

   subtype Received_Header_Register is UInt8;  --  R
   --  rxhd3[31:24] rxhd2[23:16] rxhd1[15:8] rxhd0[7:0]
   --  Reset value = 00000000 00000000 00000000 00000000

   subtype Received_Packet_Length_Register is UInt8;  --  R
   --  Length Byte of the Received Packet during fixpklen = 0.
   --  This register specifies the number of Data bytes in the last received
   --  packet, and reflects the value of the packet length byte in the received
   --  header. This is relevant ONLY if the fixpklen bit D3 of Reg 33h is
   --  cleared. If the fixpklen bit is set, then the expected number of
   --  received Data bytes must be programmed into the pklen[7:0] field in
   --  Reg 3Eh.
   --  Reset value = xxxxxxxx

   type ADC8_Control_Register is record
      adc8     : UInt6;  --  R/W
      Reserved : UInt2;  --  R/W
   end record with Size => Register'Size;
   --  Reset value = 00010000

   for ADC8_Control_Register use record
      adc8     at 0 range 0 .. 5;
      Reserved at 0 range 6 .. 7;
   end record;

   type Channel_Filter_Coefficient_Address_Register is record
      Reserved                   : UInt4;  --  R/W
      invalid_preamble_threshold : UInt4;  --  R/W
      --  Invalid Preamble Threshold.
      --  invalid_preamble_threshold[3:0}=This configures (in nibbles) for how
      --  long we will search for preamble. If during this time the preamble
      --  is not detected, we will send a signal (which can be configured as
      --  interrupt) and restart looking for the preamble again. The interval
      --  between each interrupt is given by the formula below.
   end record with Size => Register'Size;
   --  Reset value = 00000000

   for Channel_Filter_Coefficient_Address_Register use record
      Reserved                   at 0 range 0 .. 3;
      invalid_preamble_threshold at 0 range 4 .. 7;
   end record;

   for Internal_Chip_State use
     (LP   => 2#000#,
      RDY  => 2#001#,
      TX   => 2#010#,
      Tune => 2#011#,
      RX   => 2#111#);

   type Crystal_Oscillator_Register is record
      enbuf    : Bit;  --  R/W
      --  Output Buffer Enable.
      --  This bit is active only if the bufovr bit is set to 1.

      bufovr   : Bit;  --  R/W
      --  Output Buffer Enable Override.
      --  If set to 1 then the enbuf bit controls the output buffer.
      --  0: output buffer is controlled by the state machine.
      --  1: output buffer is controlled by the enbuf bit.

      enamp2x  : Bit;  --  R/W
      --  2 Times Higher Amplification Enable.

      enbias2x : Bit;  --  R/W
      --  2 Times Higher Bias Current Enable.

      clkhyst  : Bit;  --  R/W
      --  Clock Hysteresis Setting.

      pwst     : Internal_Chip_State;  --  R
      --  Internal Power States of the Chip.
   end record with Size => Register'Size;
   --  Reset value = xxx00100

   for Crystal_Oscillator_Register use record
      enbuf    at 0 range 0 .. 0;
      bufovr   at 0 range 1 .. 1;
      enamp2x  at 0 range 2 .. 2;
      enbias2x at 0 range 3 .. 3;
      clkhyst  at 0 range 4 .. 4;
      pwst     at 0 range 5 .. 7;
   end record;

   type AGC_Override_Register is record
      pga      : UInt4;  --  R/W
      --  PGA Gain Override Value.
      --  0000: 0 dB
      --  0001: 3 dB
      --  0010: 6 dB
      --  ...
      --  1000: 24 dB max.

      lnagain  : Bit;  --  R/W
      --  LNA Gain Select.
      --  Inagain=LNA Gain select.
      --  0 - min. gain = 5 dB
      --  1 - max. gain = 25 dB

      agcen    : Bit;  --  R/W
      --  Automatic Gain Control Enable.
      --  agcen=Automatic Gain Control enable. When this bit is set then the
      --  result of the control can be read out from bits [4:0], otherwise
      --  the gain can be controlled manually by writing into bits [4:0].

      sgin     : Bit;  --  R/W
      --  sgin=AGC stop increasing gain override bit (active low). When '0'
      --  (default), AGC gain increases during signal reductions are prevented.
      --  When '1', AGC gain increases during signal reductions are allowed.
      --  Only effective during Preamble, prior to detection of PREAMBLE_VALID
      --  signal.

      Reserved : Bit;  --  R
   end record with Size => Register'Size;
   --  Reset value = 00100000

   for AGC_Override_Register use record
      pga      at 0 range 0 .. 3;
      lnagain  at 0 range 4 .. 4;
      agcen    at 0 range 5 .. 5;
      sgin     at 0 range 6 .. 6;
      Reserved at 0 range 7 .. 7;
   end record;

   type TX_Power_Register is record
      txpow    : UInt3;  --  R/W
      --  TX Output Power.
      --  The output power is configurable from +13 dBm to -8 dBm (Si4430/31),
      --  and from +20 dBM to -1 dBM (Si4432) in -3 dB steps. txpow[2:0]=000
      --  corresponds to min output power, while txpow[2:0]=111 corresponds
      --  to max output power.

      lna_sw   : Bit;  --  R/W
      --  LNA Switch Controller.
      --  This bit determines when internal MOS switches at the LNA input(s)
      --  are invoked. When lna_sw=0, these switches open. When lna_sw=1,
      --  these switches are closed in TX mode and open at all other times.
      --  This bit MUST be set for proper operation in any Direct Tie
      --  application.

      Reserved : UInt4;  --  R
   end record with Size => Register'Size;
   --  Reset value = 00100000

   for TX_Power_Register use record
      txpow    at 0 range 0 .. 2;
      lna_sw   at 0 range 3 .. 3;
      Reserved at 0 range 4 .. 7;
   end record;

   subtype TX_Data_Rate_Register is UInt8;  --  R/W
   --  txdr1[15:8] txdr0[7:0]
   --  Reset value = 00001010 00111101
   --  Defaults = 40 kbps.

   type Modulation_Mode_Control_1_Register is record
      enwhite     : Bit;  --  R/W
      --  Data Whitening is Enabled if this bit is set.

      enmanch     : Bit;  --  R/W
      --  Manchester Coding is Enabled if this bit is set.
      --  What Manchester coding does is to replace a single high bit (1) with
      --  two bits starting with low followed by high (01) and a low bit (0)
      --  with a high bit followed by a low bit (10). When Manchester is
      --  enabled, please configure as well the enmaninv at 70h bit [2] since
      --  it influences the Manchester encoding/decoding process.

      enmaninv    : Bit;  --  R/W
      --  Manchester Data Inversion is Enabled if this bit is set.
      --  When this bit is low, a 10 pair is considered a Manchester 0, and
      --  a 01 pair as a Manchester 1. By setting this bit, do the opposite:
      --  every 10 will be considered as a 1, and every 01 will be considered
      --  as a 0. This function is relevant only if the Manchester mode is
      --  enabled.

      manppol     : Bit;  --  R/W
      --  Manchester Preamble Polarity (will transmit a series of 1 if set,
      --  or series of 0 if reset).
      --  This bit affects only the transmitter side, not the receiver. This
      --  is valid only if Manchester Mode is enabled.

      enphpwdn    : Bit;  --  R/W
      --  If set, the Packet Handler will be powered down when chip is in
      --  low power mode.

      txdtrtscale : Bit;  --  R/W
      --  This bit should be set for Data Rates below 30 kbps.

      Reserved    : UInt2;  --  R
   end record with Size => Register'Size;
   --  Reset value = 00001100

   for Modulation_Mode_Control_1_Register use record
      enwhite     at 0 range 0 .. 0;
      enmanch     at 0 range 1 .. 1;
      enmaninv    at 0 range 2 .. 2;
      manppol     at 0 range 3 .. 3;
      enphpwdn    at 0 range 4 .. 4;
      txdtrtscale at 0 range 5 .. 5;
      Reserved    at 0 range 6 .. 7;
   end record;

   for Modulation_Mode use
     (Unmodulated_Carrier => 2#00#,
      OOK                 => 2#01#,
      FSK                 => 2#10#,
      GFSK                => 2#11#);

   for Modulation_Source use
     (Direct_GPIO => 2#00#,
      Direct_SDI  => 2#01#,
      FIFO        => 2#10#,
      PN9         => 2#11#);

   for Modulation_TX_Data_Clock use
     (No_CLK   => 2#00#,
      CLK_GPIO => 2#01#,
      CLK_SDO  => 2#10#,
      CLK_nIRQ => 2#11#);

   type Modulation_Mode_Control_2_Register is record
      modtyp : Modulation_Mode;  --  R/W
      --  Modulation Type.

      fd     : Bit;  --  R/W
      --  fd[8]
      --  MSB of Frequency Deviation Setting, see
      --  "Register 72h. Frequency Deviation".

      eninv  : Bit;  --  R/W
      --  Invert TX and RX Data.

      dtmod  : Modulation_Source;  --  R/W
      --  Modulation Source.

      trclk  : Modulation_TX_Data_Clock;  --  R/W
      --  TX Data Clock Configuration.
   end record with Size => Register'Size;
   --  Reset value = 00000000

   for Modulation_Mode_Control_2_Register use record
      modtyp at 0 range 0 .. 1;
      fd     at 0 range 2 .. 2;
      eninv  at 0 range 3 .. 3;
      dtmod  at 0 range 4 .. 5;
      trclk  at 0 range 6 .. 7;
   end record;

   subtype Frequency_Deviation_Register is UInt8;  --  R/W
   --  fd[7:0]
   --  Reset value = 00100000

   subtype Frequency_Offset_1_Register is UInt8;  --  R/W
   --  fo[7:0]
   --  Frequency Offset Setting.
   --  The frequency offset can be calculated as Offset = 156.25 Hz x
   --  (hbsel + 1) x fo[7:0].
   --  fo[9:0] is a twos complement value.
   --  Reset value = 00000000

   type Frequency_Offset_2_Register is record
      fo       : UInt2;  --  R/W
      --  fo[9:8]
      --  Upper Bits of the Frequency Offset Setting.
      --  fo[9] is the sign bit. The frequency offset can be calculated as
      --  Offset = 156.25 Hz x (hbsel + 1) x fo[7:0]. fo[9:0] is a twos
      --  complement value.

      Reserved : UInt6;  --  R
   end record with Size => Register'Size;
   --  Reset value = 00000000

   for Frequency_Offset_2_Register use record
      fo       at 0 range 0 .. 1;
      Reserved at 0 range 2 .. 7;
   end record;

   type Frequency_Band_Select_Register is record
      fb       : UInt5;  --  R/W
      --  Frequency Band Select.
      --  Every increment corresponds to a 10 MHz increase in frequency
      --  (when hbsel=0) or a 20 MHz increase in frequency (when hbsel=1).
      --  Example: Setting fb[4:0]=00000 will result inn tuning within the
      --  240-250 MHz frequency range (for hbsel=0) or within the 480-500 MHz
      --  frequency range (for hbsel=1). Setting fb[4:0]=00001 will result in
      --  tuning within the 250-260 MHz frequency range (hbsel=0) or
      --  500-520 MHz range (hbsel=1), and so on.

      hbsel    : Bit;
      --  High Band Select.
      --  Setting hbsel = 1 will choose the frequency range from 480-960 MHz
      --  (high bands). Setting hbsel = 0 will choose the frequency range
      --  from 240-479.9 MHz (low bands).

      sbsel    : Bit;
      --  Side Band Select.
      --  Setting sbsel = 1 (recommended setting) will result in tuning the
      --  RX LO below the desired channel frequency in RX mode
      --  (low-side injection) such that the high-side sideband is selected.
      --  Note that setting sbsel = 0 will result in positioning the RX LO
      --  above the desired tuned frequency (high-side injection), but will
      --  NOT additionally flip the processing of the complex (I + jQ) signals
      --  in the IF chain necessary to select the lower sideband as the
      --  desired signal.

      Reserved : Bit;  --  R
   end record with Size => Register'Size;
   --  Reset value = 01110101

   for Frequency_Band_Select_Register use record
      fb       at 0 range 0 .. 4;
      hbsel    at 0 range 5 .. 5;
      sbsel    at 0 range 6 .. 6;
      Reserved at 0 range 7 .. 7;
   end record;

   subtype Nominal_Carrier_Frequency_Register is UInt8;  --  R/W
   --  fc1[15:8] fc0[7:0]
   --  Reset value = 10111011 10000000

   subtype Frequency_Hopping_Channel_Select_Register is UInt8;  --  R/W
   --  Reset value = 00000000

   subtype Frequency_Hopping_Step_Size_Register is UInt8;  --  R/W
   --  Frequency Hopping Step Size in 10 kHz Increments.
   --  See formula for the nominal carrier frequency at "Register 76h. Nominal
   --  Carrier Frequency". Important: The EZHop method of frequency programming
   --  only works while remaining entirely within one of the following defined
   --  frequency sub-bands: 240-320 MHz, 320-480 MHz, 480-640 MHz, and
   --  640-960 MHz. It is not allowed to define a base frequency that falls
   --  in one sub-band while the selected channel number falls in another
   --  sub-band.
   --  Reset value = 00000000

   type TX_FIFO_Control_1_Register is record
      txafthr  : UInt6;  --  R/W
      --  TX FIFO Almost Full Threshold.
      --  This register specifies the threshold value at which the TXFFAFULL
      --  status bit/interrupt will be generated, as data bytes are stored
      --  into the TX FIFO for later transmission. This value should be
      --  programmed to 1 byte less than the desired threshold value.
      --  Example: A value of 0x3C=60d will not generate an interrupt if 60
      --  bytes (or less) are written to the TX FIFO, but will generate an
      --  interrupt when 61 bytes (or more) are written to the TX FIFO.

      Reserved : UInt2;  --  R
   end record with Size => Register'Size;
   --  Reset value = 00110111

   for TX_FIFO_Control_1_Register use record
      txafthr  at 0 range 0 .. 5;
      Reserved at 0 range 6 .. 7;
   end record;

   type TX_FIFO_Control_2_Register is record
      txfaethr  : UInt6;  --  R/W
      --  TX FIFO Almost Empty Threshold.
      --  This register specifies the threshold value at which the TXFFAEM
      --  status bit/interrupt will be generated, as data bytes are pulled
      --  from the TX FIFO and transmitted. This value should be programmed
      --  to 1 byte less than the desired threshold value. Example: A value
      --  of 0x05 will generate an interrupt when 6 bytes remain in the
      --  TX FIFO.

      Reserved : UInt2;  --  R
   end record with Size => Register'Size;
   --  Reset value = 00000100

   for TX_FIFO_Control_2_Register use record
      txfaethr at 0 range 0 .. 5;
      Reserved at 0 range 6 .. 7;
   end record;

   type RX_FIFO_Control_Register is record
      rxafthr  : UInt6;  --  R/W
      --  RX FIFO Almost Full Threshold.
      --  This register specifies the threshold value at which the RXFFAFULL
      --  status bit/interrupt will be generated, as data bytes are received
      --  and stored into the RX FIFO for later retrieval. This value should
      --  be programmed to 1 byte less than the desired threshold value.
      --  Example: A value of 0x3C=60d will not generate an interrupt if 60
      --  bytes (or less) are received and stored to the RX FIFO, but will
      --  generate an interrupt when 61 bytes (or more) are received and
      --  stored to the RX FIFO.

      Reserved : UInt2;  --  R
   end record with Size => Register'Size;
   --  Reset value = 00110111

   for RX_FIFO_Control_Register use record
      rxafthr  at 0 range 0 .. 5;
      Reserved at 0 range 6 .. 7;
   end record;

   subtype FIFO_Access_Register is UInt8;  --  R/W
   --  FIFO Data.
   --  A Write (R/W = 1) to this Address will begin a Burst Write to the TX
   --  FIFO. The FIFO will be loaded in the same manner as a Burst SPI Write
   --  but the SPI address will not be incremented. To conclude the TX FIFO
   --  Write the SEL pin should be brought HIGH. A Read (R/W = 0) to this
   --  address will begin a burst read of the RX FIFO, in the same manner.
   --  Reset value = NA

   -- Register_Name --

   type Register_Name is
     (Device_Type_Code_Name,
      Version_Code_Name,
      Device_Status_Name,
      Interrupt_Status_1_Name,
      Interrupt_Status_2_Name,
      Interrupt_Enable_1_Name,
      Interrupt_Enable_2_Name,
      Operating_Mode_And_Function_Control_1_Name,
      Operating_Mode_And_Function_Control_2_Name,
      Crystal_Oscillator_Load_Capacitance_Name,
      Microcontroller_Output_Clock_Name,
      GPIO0_Configuration_Name,
      GPIO1_Configuration_Name,
      GPIO2_Configuration_Name,
      IO_Port_Configuration_Name,
      ADC_Configuration_Name,
      ADC_Sensor_Amplifier_Offset_Name,
      ADC_Value_Name,
      Temperature_Sensor_Calibration_Name,
      Temperature_Value_Offset_Name,
      Wake_Up_Timer_Period_1_Name,
      Wake_Up_Timer_Period_2_Name,
      Wake_Up_Timer_Period_3_Name,
      Wake_Up_Timer_Value_1_Name,
      Wake_Up_Timer_Value_2_Name,
      Low_Duty_Cycle_Mode_Duration_Name,
      Low_Battery_Detector_Threshold_Name,
      Battery_Voltage_Level_Name,
      IF_Filter_Bandwidth_Name,
      AFC_Loop_Gearshift_Override_Name,
      AFC_Timing_Control_Name,
      Clock_Recovery_Gearshift_Override_Name,
      Clock_Recovery_Oversampling_Rate_Name,
      Clock_Recovery_Offset_2_Name,
      Clock_Recovery_Offset_1_Name,
      Clock_Recovery_Offset_0_Name,
      Clock_Recovery_Timing_Loop_Gain_1_Name,
      Clock_Recovery_Timing_Loop_Gain_0_Name,
      Received_Signal_Strength_Indicator_Name,
      RSSI_Threshold_For_Clear_Channel_Indicator_Name,
      Antenna_Diversity_1_Name,
      Antenna_Diversity_2_Name,
      AFC_Limiter_Name,
      AFC_Correction_Name,
      OOK_Counter_Value_1_Name,
      OOK_Counter_Value_2_Name,
      Slicer_Peak_Holder_Name,
      Data_Access_Control_Name,
      EZMAC_Status_Name,
      Header_Control_1_Name,
      Header_Control_2_Name,
      Preamble_Length_Name,
      Preamble_Detection_Control_1_Name,
      Synchronization_Word_3_Name,
      Synchronization_Word_2_Name,
      Synchronization_Word_1_Name,
      Synchronization_Word_0_Name,
      Transmit_Header_3_Name,
      Transmit_Header_2_Name,
      Transmit_Header_1_Name,
      Transmit_Header_0_Name,
      Packet_Length_Name,
      Check_Header_3_Name,
      Check_Header_2_Name,
      Check_Header_1_Name,
      Check_Header_0_Name,
      Header_Enable_3_Name,
      Header_Enable_2_Name,
      Header_Enable_1_Name,
      Header_Enable_0_Name,
      Received_Header_3_Name,
      Received_Header_2_Name,
      Received_Header_1_Name,
      Received_Header_0_Name,
      Received_Packet_Length_Name,
      ADC8_Control_Name,
      Channel_Filter_Coefficient_Address_Name,
      Crystal_Oscillator_Name,
      AGC_Override_Name,
      TX_Power_Name,
      TX_Data_Rate_1_Name,
      TX_Data_Rate_0_Name,
      Modulation_Mode_Control_1_Name,
      Modulation_Mode_Control_2_Name,
      Frequency_Deviation_Name,
      Frequency_Offset_1_Name,
      Frequency_Offset_2_Name,
      Frequency_Band_Select_Name,
      Nominal_Carrier_Frequency_1_Name,
      Nominal_Carrier_Frequency_0_Name,
      Frequency_Hopping_Channel_Select_Name,
      Frequency_Hopping_Step_Size_Name,
      TX_FIFO_Control_1_Name,
      TX_FIFO_Control_2_Name,
      RX_FIFO_Control_Name,
      FIFO_Access_Name
     );

   Registers_Addressses : constant array (Register_Name) of
     Register_Address :=
       [Device_Type_Code_Name                            => 16#00#,
        Version_Code_Name                                => 16#01#,
        Device_Status_Name                               => 16#02#,
        Interrupt_Status_1_Name                          => 16#03#,
        Interrupt_Status_2_Name                          => 16#04#,
        Interrupt_Enable_1_Name                          => 16#05#,
        Interrupt_Enable_2_Name                          => 16#06#,
        Operating_Mode_And_Function_Control_1_Name       => 16#07#,
        Operating_Mode_And_Function_Control_2_Name       => 16#08#,
        Crystal_Oscillator_Load_Capacitance_Name         => 16#09#,
        Microcontroller_Output_Clock_Name                => 16#0A#,
        GPIO0_Configuration_Name                         => 16#0B#,
        GPIO1_Configuration_Name                         => 16#0C#,
        GPIO2_Configuration_Name                         => 16#0D#,
        IO_Port_Configuration_Name                       => 16#0E#,
        ADC_Configuration_Name                           => 16#0F#,
        ADC_Sensor_Amplifier_Offset_Name                 => 16#10#,
        ADC_Value_Name                                   => 16#11#,
        Temperature_Sensor_Calibration_Name              => 16#12#,
        Temperature_Value_Offset_Name                    => 16#13#,
        Wake_Up_Timer_Period_1_Name                      => 16#14#,
        Wake_Up_Timer_Period_2_Name                      => 16#15#,
        Wake_Up_Timer_Period_3_Name                      => 16#16#,
        Wake_Up_Timer_Value_1_Name                       => 16#17#,
        Wake_Up_Timer_Value_2_Name                       => 16#18#,
        Low_Duty_Cycle_Mode_Duration_Name                => 16#19#,
        Low_Battery_Detector_Threshold_Name              => 16#1A#,
        Battery_Voltage_Level_Name                       => 16#1B#,
        IF_Filter_Bandwidth_Name                         => 16#1C#,
        AFC_Loop_Gearshift_Override_Name                 => 16#1D#,
        AFC_Timing_Control_Name                          => 16#1E#,
        Clock_Recovery_Gearshift_Override_Name           => 16#1F#,
        Clock_Recovery_Oversampling_Rate_Name            => 16#20#,
        Clock_Recovery_Offset_2_Name                     => 16#21#,
        Clock_Recovery_Offset_1_Name                     => 16#22#,
        Clock_Recovery_Offset_0_Name                     => 16#23#,
        Clock_Recovery_Timing_Loop_Gain_1_Name           => 16#24#,
        Clock_Recovery_Timing_Loop_Gain_0_Name           => 16#25#,
        Received_Signal_Strength_Indicator_Name          => 16#26#,
        RSSI_Threshold_For_Clear_Channel_Indicator_Name  => 16#27#,
        Antenna_Diversity_1_Name                         => 16#28#,
        Antenna_Diversity_2_Name                         => 16#29#,
        AFC_Limiter_Name                                 => 16#2A#,
        AFC_Correction_Name                              => 16#2B#,
        OOK_Counter_Value_1_Name                         => 16#2C#,
        OOK_Counter_Value_2_Name                         => 16#2D#,
        Slicer_Peak_Holder_Name                          => 16#2E#,
        Data_Access_Control_Name                         => 16#30#,
        EZMAC_Status_Name                                => 16#31#,
        Header_Control_1_Name                            => 16#32#,
        Header_Control_2_Name                            => 16#33#,
        Preamble_Length_Name                             => 16#34#,
        Preamble_Detection_Control_1_Name                => 16#35#,
        Synchronization_Word_3_Name                      => 16#36#,
        Synchronization_Word_2_Name                      => 16#37#,
        Synchronization_Word_1_Name                      => 16#38#,
        Synchronization_Word_0_Name                      => 16#39#,
        Transmit_Header_3_Name                           => 16#3A#,
        Transmit_Header_2_Name                           => 16#3B#,
        Transmit_Header_1_Name                           => 16#3C#,
        Transmit_Header_0_Name                           => 16#3D#,
        Packet_Length_Name                               => 16#3E#,
        Check_Header_3_Name                              => 16#3F#,
        Check_Header_2_Name                              => 16#40#,
        Check_Header_1_Name                              => 16#41#,
        Check_Header_0_Name                              => 16#42#,
        Header_Enable_3_Name                             => 16#43#,
        Header_Enable_2_Name                             => 16#44#,
        Header_Enable_1_Name                             => 16#45#,
        Header_Enable_0_Name                             => 16#46#,
        Received_Header_3_Name                           => 16#47#,
        Received_Header_2_Name                           => 16#48#,
        Received_Header_1_Name                           => 16#49#,
        Received_Header_0_Name                           => 16#4A#,
        Received_Packet_Length_Name                      => 16#4B#,
        ADC8_Control_Name                                => 16#4F#,
        Channel_Filter_Coefficient_Address_Name          => 16#60#,
        Crystal_Oscillator_Name                          => 16#62#,
        AGC_Override_Name                                => 16#69#,
        TX_Power_Name                                    => 16#6D#,
        TX_Data_Rate_1_Name                              => 16#6E#,
        TX_Data_Rate_0_Name                              => 16#6F#,
        Modulation_Mode_Control_1_Name                   => 16#70#,
        Modulation_Mode_Control_2_Name                   => 16#71#,
        Frequency_Deviation_Name                         => 16#72#,
        Frequency_Offset_1_Name                          => 16#73#,
        Frequency_Offset_2_Name                          => 16#74#,
        Frequency_Band_Select_Name                       => 16#75#,
        Nominal_Carrier_Frequency_1_Name                 => 16#76#,
        Nominal_Carrier_Frequency_0_Name                 => 16#77#,
        Frequency_Hopping_Channel_Select_Name            => 16#79#,
        Frequency_Hopping_Step_Size_Name                 => 16#7A#,
        TX_FIFO_Control_1_Name                           => 16#7C#,
        TX_FIFO_Control_2_Name                           => 16#7D#,
        RX_FIFO_Control_Name                             => 16#7E#,
        FIFO_Access_Name                                 => 16#7F#
       ];

   --------------
   -- Commands --
   --------------

   type Command is new UInt8;
   --  Base type for all commands

   type Command_Register is record
      Address   : Register_Address;
      Operation : Operation_Type;
   end record with Size => Command'Size;
   --  Read registers command

   for Command_Register use record
      Address   at 0 range 0 .. 6;
      Operation at 0 range 7 .. 7;
   end record;

   ----------------------
   -- NRF24L01P_Driver --
   ----------------------

   type Holder_Type_Kind is (Hardware, Software);

   type Holder_Type
     (Kind : Holder_Type_Kind := Software) is
      record
         SPI     : HAL.SPI.Any_SPI_Port;
         SDN_Pin : HAL.GPIO.Any_GPIO_Point;

         case Kind is
            when Hardware =>
               null;
            when Software =>
               CSN_Pin : HAL.GPIO.Any_GPIO_Point;
               --  Used for "selecting" chip vis SPI
         end case;
      end record;

   type Si4432_Driver is limited record
      Holder : Holder_Type;
   end record;

   function Read_Register
     (This : Si4432_Driver;
      Name : Register_Name)
      return Register;

   procedure Read_Register
     (This : Si4432_Driver;
      Name : Register_Name;
      Data : out HAL.SPI.SPI_Data_8b);

   procedure Write_Register
     (This : Si4432_Driver;
      Name : Register_Name;
      Data : Register);

   procedure Write_Register
     (This : Si4432_Driver;
      Name : Register_Name;
      Data : HAL.SPI.SPI_Data_8b);

   procedure Write
     (This  : Si4432_Driver;
      Data  : HAL.SPI.SPI_Data_8b);

   procedure Write_And_Read
     (This    : Si4432_Driver;
      Command : HAL.SPI.SPI_Data_8b;
      Data    : out HAL.SPI.SPI_Data_8b);

   procedure CSN_High (This : Si4432_Driver);
   procedure CSN_Low  (This : Si4432_Driver);
   --  High/Low CSN pin in SOFTWARE slave selection mode.

   Null_Interrupt_Status_1_Register : constant Interrupt_Status_1_Register :=
     (others => 0);
   Null_Interrupt_Status_2_Register : constant Interrupt_Status_2_Register :=
     (others => 0);

end Si4432;
