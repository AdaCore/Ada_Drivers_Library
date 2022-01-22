------------------------------------------------------------------------------
--                                                                          --
--                      Copyright (C) 2022 AdaCore                          --
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
--   This file is based on STSW-IMG009, issue 3.5.1/UM2150 Rev 4.
--                                                                          --
--   COPYRIGHT(c) 2021 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System;

with HAL;

package body VL53L1X is

   package Registers is
      pragma Style_Checks (Off);
      pragma Warnings (Off, "is not referenced");

      --  These values are copied from those derived from vl53l1x_api.h,
      --  except that the encoding of the C double-underscore as "_u_"
      --  has been replaced by a single underscore.

      VL53L1X_IMPLEMENTATION_VER_MAJOR : constant := 3;
      VL53L1X_IMPLEMENTATION_VER_MINOR : constant := 5;
      VL53L1X_IMPLEMENTATION_VER_SUB : constant := 1;
      VL53L1X_IMPLEMENTATION_VER_REVISION : constant := 8#000#;

      SOFT_RESET : constant := 16#0000#;
      VL53L1_I2C_SLAVE_DEVICE_ADDRESS : constant := 16#0001#;
      VL53L1_VHV_CONFIG_TIMEOUT_MACROP_LOOP_BOUND : constant := 16#0008#;
      ALGO_CROSSTALK_COMPENSATION_PLANE_OFFSET_KCPS : constant := 16#0016#;
      ALGO_CROSSTALK_COMPENSATION_X_PLANE_GRADIENT_KCPS : constant := 16#0018#;
      ALGO_CROSSTALK_COMPENSATION_Y_PLANE_GRADIENT_KCPS : constant := 16#001A#;
      ALGO_PART_TO_PART_RANGE_OFFSET_MM : constant := 16#001E#;
      MM_CONFIG_INNER_OFFSET_MM : constant := 16#0020#;
      MM_CONFIG_OUTER_OFFSET_MM : constant := 16#0022#;
      GPIO_HV_MUX_CTRL : constant := 16#0030#;
      GPIO_TIO_HV_STATUS : constant := 16#0031#;
      SYSTEM_INTERRUPT_CONFIG_GPIO : constant := 16#0046#;
      PHASECAL_CONFIG_TIMEOUT_MACROP : constant := 16#004B#;
      RANGE_CONFIG_TIMEOUT_MACROP_A_HI : constant := 16#005E#;
      RANGE_CONFIG_VCSEL_PERIOD_A : constant := 16#0060#;
      RANGE_CONFIG_VCSEL_PERIOD_B : constant := 16#0063#;
      RANGE_CONFIG_TIMEOUT_MACROP_B_HI : constant := 16#0061#;
      RANGE_CONFIG_TIMEOUT_MACROP_B_LO : constant := 16#0062#;
      RANGE_CONFIG_SIGMA_THRESH : constant := 16#0064#;
      RANGE_CONFIG_MIN_COUNT_RATE_RTN_LIMIT_MCPS : constant := 16#0066#;
      RANGE_CONFIG_VALID_PHASE_HIGH : constant := 16#0069#;
      VL53L1_SYSTEM_INTERMEASUREMENT_PERIOD : constant := 16#006C#;
      SYSTEM_THRESH_HIGH : constant := 16#0072#;
      SYSTEM_THRESH_LOW : constant := 16#0074#;
      SD_CONFIG_WOI_SD0 : constant := 16#0078#;
      SD_CONFIG_INITIAL_PHASE_SD0 : constant := 16#007A#;
      ROI_CONFIG_USER_ROI_CENTRE_SPAD : constant := 16#007F#;
      ROI_CONFIG_USER_ROI_REQUESTED_GLOBAL_XY_SIZE : constant := 16#0080#;
      SYSTEM_SEQUENCE_CONFIG : constant := 16#0081#;
      VL53L1_SYSTEM_GROUPED_PARAMETER_HOLD : constant := 16#0082#;
      SYSTEM_INTERRUPT_CLEAR : constant := 16#0086#;
      SYSTEM_MODE_START : constant := 16#0087#;
      VL53L1_RESULT_RANGE_STATUS : constant := 16#0089#;
      VL53L1_RESULT_DSS_ACTUAL_EFFECTIVE_SPADS_SD0 : constant := 16#008C#;
      RESULT_AMBIENT_COUNT_RATE_MCPS_SD : constant := 16#0090#;
      VL53L1_RESULT_FINAL_CROSSTALK_CORRECTED_RANGE_MM_SD0 : constant := 16#0096#;
      VL53L1_RESULT_PEAK_SIGNAL_COUNT_RATE_CROSSTALK_CORRECTED_MCPS_SD0 : constant := 16#0098#;
      VL53L1_RESULT_OSC_CALIBRATE_VAL : constant := 16#00DE#;
      VL53L1_FIRMWARE_SYSTEM_STATUS : constant := 16#00E5#;
      VL53L1_IDENTIFICATION_MODEL_ID : constant := 16#010F#;
      VL53L1_ROI_CONFIG_MODE_ROI_CENTRE_SPAD : constant := 16#013E#;

      pragma Warnings (On, "is not referenced");
      pragma Style_Checks (On);
   end Registers;
   use Registers;

   --  Scaling factor used in Get/Set_Inter_Measurement_Time.
   PLL_Factor : constant := 1.0466;

   --  Low-level device access procedures.

   procedure Read (This  : in out VL53L1X_Ranging_Sensor;
                   Index :        HAL.UInt16;
                   Data  :    out HAL.I2C.I2C_Data);

   procedure Read (This  : in out VL53L1X_Ranging_Sensor;
                   Index :        HAL.UInt16;
                   Data  :    out HAL.UInt16);

   procedure Read (This  : in out VL53L1X_Ranging_Sensor;
                   Index :        HAL.UInt16;
                   Data  :    out HAL.UInt32);

   procedure Write (This  : in out VL53L1X_Ranging_Sensor;
                    Index :        HAL.UInt16;
                    Data  :        HAL.I2C.I2C_Data);

   procedure Write (This  : in out VL53L1X_Ranging_Sensor;
                    Index :        HAL.UInt16;
                    Data  :        HAL.UInt16);

   procedure Write (This  : in out VL53L1X_Ranging_Sensor;
                    Index :        HAL.UInt16;
                    Data  :        HAL.UInt32);

   --  The VL53L1X is a big-endian device. Register addresses are two
   --  bytes wide. The data is mainly (arrays of) single bytes, but
   --  some is two bytes wide, some 4.

   subtype Two_Byte_Array is HAL.UInt8_Array (1 .. 2);
   function To_Device (Value : HAL.UInt16) return Two_Byte_Array;
   function From_Device (Value : Two_Byte_Array) return HAL.UInt16;

   subtype Four_Byte_Array is HAL.UInt8_Array (1 .. 4);
   function To_Device (Value : HAL.UInt32) return Four_Byte_Array;
   function From_Device (Value : Four_Byte_Array) return HAL.UInt32;

   -----------------
   -- Boot_Device --
   -----------------

   procedure Boot_Device
     (This             : in out VL53L1X_Ranging_Sensor;
      Loop_Interval_Ms :        Positive := 10;
      Status           :    out Boot_Status)
   is
      I2C_Status : HAL.I2C.I2C_Status;
      use all type HAL.I2C.I2C_Status;
   begin
      --  Allow the VL53L1X to do its internal initialization.
      This.Timing.Delay_Milliseconds (100);

      Get_Device_Status :
      for J in 1 .. 10 loop
         --  We're going to do a low-level read using HAL.I2C
         --  directly, because we don't want to raise an exception if
         --  there's a failure.
         HAL.I2C.Master_Transmit
           (This    => This.Port.all,
            Addr    => This.I2C_Address,
            Data    => To_Device (HAL.UInt16'(VL53L1_FIRMWARE_SYSTEM_STATUS)),
            Status  => I2C_Status);
         exit Get_Device_Status when I2C_Status /= Ok;

         declare
            Buffer : HAL.UInt8_Array (1 .. 1);
            use type HAL.UInt8;
         begin
            HAL.I2C.Master_Receive
              (This    => This.Port.all,
               Addr    => This.I2C_Address,
               Data    => Buffer,
               Status  => I2C_Status);
            if I2C_Status = Ok and then Buffer (1) = 3 then
               --  '3' is undocumented; UM2150 says 1.
               This.State := Booted;
               exit Get_Device_Status;
            end if;
         end;

         This.Timing.Delay_Milliseconds (Loop_Interval_Ms);
      end loop Get_Device_Status;

      Status := (case I2C_Status is
                    when Ok          => Ok,
                    when Err_Error   => I2C_Error,
                    when Err_Timeout => I2C_Timeout,
                    when Busy        => I2C_Busy);
   end Boot_Device;

   ------------------------
   -- Set_Device_Address --
   ------------------------

   procedure Set_Device_Address
     (This : in out VL53L1X_Ranging_Sensor;
      Addr :        HAL.I2C.I2C_Address)
   is
      use all type HAL.UInt8;
   begin
      Write (This,
             Index => VL53L1_I2C_SLAVE_DEVICE_ADDRESS,
             Data  => (1 => Shift_Right (HAL.UInt8 (Addr), 1)));
      This.I2C_Address := Addr;
   end Set_Device_Address;

   -----------------
   -- Sensor_Init --
   -----------------

   procedure Sensor_Init
     (This : in out VL53L1X_Ranging_Sensor)
   is
      pragma Style_Checks (Off); -- the continuation comment lines

      VL51L1X_DEFAULT_CONFIGURATION :
        constant HAL.I2C.I2C_Data (16#2d# .. 16#87#) :=
          (
           16#00#, -- 0x2d : set bit 2 and 5 to 1 for fast plus mode
                   -- (1MHz I2C), else don't touch
           16#00#, -- 0x2e : bit 0 if I2C pulled up at 1.8V, else set bit
                   -- 0 to 1 (pull up at AVDD)
           16#00#, -- 0x2f : bit 0 if GPIO pulled up at 1.8V, else set
                   -- bit 0 to 1 (pull up at AVDD)
           16#01#, -- 0x30 : set bit 4 to 0 for active high interrupt and
                   -- 1 for active low (bits 3:0 must be 0x1), use
                   -- SetInterruptPolarity()
           16#02#, -- 0x31 : bit 1 = interrupt depending on the polarity,
                   -- use CheckForDataReady()
           16#00#, -- 0x32 : not user-modifiable
           16#02#, -- 0x33 : not user-modifiable
           16#08#, -- 0x34 : not user-modifiable
           16#00#, -- 0x35 : not user-modifiable
           16#08#, -- 0x36 : not user-modifiable
           16#10#, -- 0x37 : not user-modifiable
           16#01#, -- 0x38 : not user-modifiable
           16#01#, -- 0x39 : not user-modifiable
           16#00#, -- 0x3a : not user-modifiable
           16#00#, -- 0x3b : not user-modifiable
           16#00#, -- 0x3c : not user-modifiable
           16#00#, -- 0x3d : not user-modifiable
           16#ff#, -- 0x3e : not user-modifiable
           16#00#, -- 0x3f : not user-modifiable
           16#0F#, -- 0x40 : not user-modifiable
           16#00#, -- 0x41 : not user-modifiable
           16#00#, -- 0x42 : not user-modifiable
           16#00#, -- 0x43 : not user-modifiable
           16#00#, -- 0x44 : not user-modifiable
           16#00#, -- 0x45 : not user-modifiable
           16#20#, -- 0x46 : interrupt configuration 0->level low
                   -- detection, 1-> level high, 2-> Out of window, 3->In
                   -- window, 0x20-> New sample ready , TBC
           16#0b#, -- 0x47 : not user-modifiable
           16#00#, -- 0x48 : not user-modifiable
           16#00#, -- 0x49 : not user-modifiable
           16#02#, -- 0x4a : not user-modifiable
           16#0a#, -- 0x4b : not user-modifiable
           16#21#, -- 0x4c : not user-modifiable
           16#00#, -- 0x4d : not user-modifiable
           16#00#, -- 0x4e : not user-modifiable
           16#05#, -- 0x4f : not user-modifiable
           16#00#, -- 0x50 : not user-modifiable
           16#00#, -- 0x51 : not user-modifiable
           16#00#, -- 0x52 : not user-modifiable
           16#00#, -- 0x53 : not user-modifiable
           16#c8#, -- 0x54 : not user-modifiable
           16#00#, -- 0x55 : not user-modifiable
           16#00#, -- 0x56 : not user-modifiable
           16#38#, -- 0x57 : not user-modifiable
           16#ff#, -- 0x58 : not user-modifiable
           16#01#, -- 0x59 : not user-modifiable
           16#00#, -- 0x5a : not user-modifiable
           16#08#, -- 0x5b : not user-modifiable
           16#00#, -- 0x5c : not user-modifiable
           16#00#, -- 0x5d : not user-modifiable
           16#01#, -- 0x5e : not user-modifiable
           16#cc#, -- 0x5f : not user-modifiable
           16#0f#, -- 0x60 : not user-modifiable
           16#01#, -- 0x61 : not user-modifiable
           16#f1#, -- 0x62 : not user-modifiable
           16#0d#, -- 0x63 : not user-modifiable
           16#01#, -- 0x64 : Sigma threshold MSB (mm in 14.2 format for
                   -- MSB+LSB), use SetSigmaThreshold(), default value 90
                   -- mm
           16#68#, -- 0x65 : Sigma threshold LSB
           16#00#, -- 0x66 : Min count Rate MSB (MCPS in 9.7 format for
                   -- MSB+LSB), use SetSignalThreshold()
           16#80#, -- 0x67 : Min count Rate LSB
           16#08#, -- 0x68 : not user-modifiable
           16#b8#, -- 0x69 : not user-modifiable
           16#00#, -- 0x6a : not user-modifiable
           16#00#, -- 0x6b : not user-modifiable
           16#00#, -- 0x6c : Intermeasurement period MSB, 32 bits
                   -- register, use SetIntermeasurementInMs()
           16#00#, -- 0x6d : Intermeasurement period
           16#0f#, -- 0x6e : Intermeasurement period
           16#89#, -- 0x6f : Intermeasurement period LSB
           16#00#, -- 0x70 : not user-modifiable
           16#00#, -- 0x71 : not user-modifiable
           16#00#, -- 0x72 : distance threshold high MSB (in mm,
                   -- MSB+LSB), use SetD:tanceThreshold()
           16#00#, -- 0x73 : distance threshold high LSB
           16#00#, -- 0x74 : distance threshold low MSB ( in mm,
                   -- MSB+LSB), use SetD:tanceThreshold()
           16#00#, -- 0x75 : distance threshold low LSB
           16#00#, -- 0x76 : not user-modifiable
           16#01#, -- 0x77 : not user-modifiable
           16#0f#, -- 0x78 : not user-modifiable
           16#0d#, -- 0x79 : not user-modifiable
           16#0e#, -- 0x7a : not user-modifiable
           16#0e#, -- 0x7b : not user-modifiable
           16#00#, -- 0x7c : not user-modifiable
           16#00#, -- 0x7d : not user-modifiable
           16#02#, -- 0x7e : not user-modifiable
           16#c7#, -- 0x7f : ROI center, use SetROI()
           16#ff#, -- 0x80 : XY ROI (X=Width, Y=Height), use SetROI()
           16#9B#, -- 0x81 : not user-modifiable
           16#00#, -- 0x82 : not user-modifiable
           16#00#, -- 0x83 : not user-modifiable
           16#00#, -- 0x84 : not user-modifiable
           16#01#, -- 0x85 : not user-modifiable
           16#00#, -- 0x86 : clear interrupt, use ClearInterrupt()
           16#00#  -- 0x87 : start ranging, use StartRanging() or
                   -- StopRanging(), If you want an automatic start after
                   -- VL53L1X_init() call, put 0x40 in location 0x87
          );

      pragma Style_Checks (On);
   begin
      Write (This,
             Index => HAL.UInt16 (VL51L1X_DEFAULT_CONFIGURATION'First),
             Data  => VL51L1X_DEFAULT_CONFIGURATION);

      This.State := Initialized;  -- needed here to do the first measurement.

      --  Discard the first measurement.
      Start_Ranging (This);
      Wait_For_Measurement (This);
      Clear_Interrupt (This);
      Stop_Ranging (This);

      --  I think this is to do with temperature?
      Write (This,
             Index => VL53L1_VHV_CONFIG_TIMEOUT_MACROP_LOOP_BOUND,
             Data  => (1 => 16#09#));
      Write (This,
             Index => 16#0b#,
             Data  => (1 => 16#00#));
   end Sensor_Init;

   -----------------------
   -- Get_Distance_Mode --
   -----------------------

   function Get_Distance_Mode
     (This : in out VL53L1X_Ranging_Sensor) return Distance_Mode
   is
      Buffer : HAL.I2C.I2C_Data (1 .. 1);
   begin
      Read (This,
            Index => PHASECAL_CONFIG_TIMEOUT_MACROP,
            Data  => Buffer);
      return (case Buffer (1) is
                 when 16#14# => Short,
                 when 16#0a# => Long,
                 when others =>
                    raise VL53L1X_Error with "invalid distance mode value");
   end Get_Distance_Mode;

   -----------------------
   -- Set_Distance_Mode --
   -----------------------

   procedure Set_Distance_Mode
     (This : in out VL53L1X_Ranging_Sensor;
      Mode :        Distance_Mode := Long)
   is
      Budget : constant Measurement_Budget := Get_Measurement_Budget (This);
   begin
      case Mode is
         when Short =>
            Write (This,
                   Index => PHASECAL_CONFIG_TIMEOUT_MACROP,
                   Data  => (1 => 16#14#));
            Write (This,
                   Index => RANGE_CONFIG_VCSEL_PERIOD_A,
                   Data  => (1 => 16#07#));
            Write (This,
                   Index => RANGE_CONFIG_VCSEL_PERIOD_B,
                   Data  => (1 => 16#05#));
            Write (This,
                   Index => RANGE_CONFIG_VALID_PHASE_HIGH,
                   Data  => (1 => 16#38#));
            Write (This,
                   Index => SD_CONFIG_WOI_SD0,
                   Data  => HAL.UInt16'(16#0705#));
            Write (This,
                   Index => SD_CONFIG_INITIAL_PHASE_SD0,
                   Data => HAL.UInt16'(16#0606#));
         when Long =>
            Write (This,
                   Index => PHASECAL_CONFIG_TIMEOUT_MACROP,
                   Data  => (1 => 16#0a#));
            Write (This,
                   Index => RANGE_CONFIG_VCSEL_PERIOD_A,
                   Data  => (1 => 16#0f#));
            Write (This,
                   Index => RANGE_CONFIG_VCSEL_PERIOD_B,
                   Data  => (1 => 16#0d#));
            Write (This,
                   Index => RANGE_CONFIG_VALID_PHASE_HIGH,
                   Data  => (1 => 16#b8#));
            Write (This,
                   Index => SD_CONFIG_WOI_SD0,
                   Data  => HAL.UInt16'(16#0f0d#));
            Write (This,
                   Index => SD_CONFIG_INITIAL_PHASE_SD0,
                   Data => HAL.UInt16'(16#0e0e#));
      end case;
      Set_Measurement_Budget (This, Budget);
   end Set_Distance_Mode;

   --------------------------------
   -- Get_Inter_Measurement_Time --
   --------------------------------

   function Get_Inter_Measurement_Time
     (This : in out VL53L1X_Ranging_Sensor) return Milliseconds
   is
      Tmp          : HAL.UInt32;
      Raw_Interval : Float;
      Clock_PLL    : HAL.UInt16;
      use all type HAL.UInt16;
   begin
      Read (This,
            Index => VL53L1_SYSTEM_INTERMEASUREMENT_PERIOD,
            Data  => Tmp);
      Raw_Interval := Float (Tmp);
      Read (This,
            Index => VL53L1_RESULT_OSC_CALIBRATE_VAL,
            Data  => Clock_PLL);
      return Natural (Raw_Interval
                        / (Float (Clock_PLL and 16#03ff#) * PLL_Factor));
   end Get_Inter_Measurement_Time;

   ----------------------------
   -- Get_Measurement_Budget --
   ----------------------------

   function Get_Measurement_Budget
     (This : in out VL53L1X_Ranging_Sensor) return Measurement_Budget
   is
      Raw : HAL.UInt16;
   begin
      Read (This,
            Index => RANGE_CONFIG_TIMEOUT_MACROP_A_HI,
            Data  => Raw);
      return (case Raw is
                 when 16#001d#            => 15,
                 when 16#0051# | 16#001e# => 20,
                 when 16#00d6# | 16#0060# => 33,
                 when 16#01ae# | 16#00ad# => 50,
                 when 16#02e1# | 16#01cc# => 100,
                 when 16#03e1# | 16#02d9# => 200,
                 when 16#0591# | 16#048f# => 500,
                 when others =>
                    raise VL53L1X_Error
                        with "invalid measurement budget" & Raw'Image);
   end Get_Measurement_Budget;

   --------------------------------
   -- Set_Inter_Measurement_Time --
   --------------------------------

   procedure Set_Inter_Measurement_Time
     (This     : in out VL53L1X_Ranging_Sensor;
      Interval :        Milliseconds)
   is
      Clock_PLL : HAL.UInt16;
      Raw_Interval : HAL.UInt32;
      use all type HAL.UInt16;
   begin
      Read (This,
            Index => VL53L1_RESULT_OSC_CALIBRATE_VAL,
            Data  => Clock_PLL);
      Raw_Interval := HAL.UInt32 (Float (Clock_PLL and 16#03ff#)
                                    * Float (Interval)
                                    * PLL_Factor);
      Write (This,
             Index => VL53L1_SYSTEM_INTERMEASUREMENT_PERIOD,
             Data  => Raw_Interval);
   end Set_Inter_Measurement_Time;

   ----------------------------
   -- Set_Measurement_Budget --
   ----------------------------

   procedure Set_Measurement_Budget
     (This   : in out VL53L1X_Ranging_Sensor;
      Budget :        Measurement_Budget := 100)
   is
      --  The next two declarations make it easier to implement the
      --  limitation that the measurement budget can't be 15 in
      --  Long distance mode.
      subtype Measurement_Budget_For_Long
        is Measurement_Budget range 20 .. 500;
      Long_Measurement_Budget_Ms : constant Measurement_Budget_For_Long
        := Measurement_Budget_For_Long (Budget);
   begin
      case Get_Distance_Mode (This) is
         when Short =>
            Write (This,
                   Index => RANGE_CONFIG_TIMEOUT_MACROP_A_HI,
                   Data  =>
                     HAL.UInt16'(case Budget is
                                    when 15  => 16#001d#,
                                    when 20  => 16#0051#,
                                    when 33  => 16#00d6#,
                                    when 50  => 16#01ae#,
                                    when 100 => 16#02e1#,
                                    when 200 => 16#03e1#,
                                    when 500 => 16#0591#));
            Write (This,
                   Index => RANGE_CONFIG_TIMEOUT_MACROP_B_HI,
                   Data  =>
                     HAL.UInt16'(case Budget is
                                    when 15  => 16#0027#,
                                    when 20  => 16#006e#,
                                    when 33  => 16#006e#,
                                    when 50  => 16#01e8#,
                                    when 100 => 16#0388#,
                                    when 200 => 16#0496#,
                                    when 500 => 16#05c1#));
         when Long =>
            Write (This,
                   Index => RANGE_CONFIG_TIMEOUT_MACROP_A_HI,
                   Data  =>
                     HAL.UInt16'(case Long_Measurement_Budget_Ms is
                                    when 20  => 16#001e#,
                                    when 33  => 16#0060#,
                                    when 50  => 16#00ad#,
                                    when 100 => 16#01cc#,
                                    when 200 => 16#02d9#,
                                    when 500 => 16#048f#));
            Write (This,
                   Index => RANGE_CONFIG_TIMEOUT_MACROP_B_HI,
                   Data  =>
                     HAL.UInt16'(case Long_Measurement_Budget_Ms is
                                    when 20  => 16#0022#,
                                    when 33  => 16#006e#,
                                    when 50  => 16#00c6#,
                                    when 100 => 16#01ea#,
                                    when 200 => 16#02f8#,
                                    when 500 => 16#04a4#));
      end case;
   end Set_Measurement_Budget;

   -------------------
   -- Start_Ranging --
   -------------------

   procedure Start_Ranging
     (This : in out VL53L1X_Ranging_Sensor)
   is
   begin
      Write (This,
             Index => SYSTEM_MODE_START,
             Data  => (1 => 16#40#));
      This.State := Ranging;
   end Start_Ranging;

   --------------------------
   -- Wait_For_Measurement --
   --------------------------

   procedure Wait_For_Measurement
     (This             : in out VL53L1X_Ranging_Sensor;
      Loop_Interval_Ms :        Positive := 10)
   is
   begin
      loop
         exit when Is_Measurement_Ready (This);
         This.Timing.Delay_Milliseconds (Loop_Interval_Ms);
      end loop;
   end Wait_For_Measurement;

   --------------------------
   -- Is_Measurement_Ready --
   --------------------------

   function Is_Measurement_Ready
     (This : in out VL53L1X_Ranging_Sensor) return Boolean
   is
      --  We need to compare the data status and the interrupt
      --  polarity; haven't provided any interface to interrupt
      --  polarity yet, so do it hre.
      Buffer : HAL.I2C.I2C_Data (1 .. 1);
      Polarity : Boolean;
      Availability : Boolean;
      use type HAL.UInt8;
   begin
      Read (This,
            Index => GPIO_HV_MUX_CTRL,
            Data  => Buffer);
      Polarity := (Buffer (1) and 16#10#) = 0;
      Read (This,
            Index => GPIO_TIO_HV_STATUS,
            Data  => Buffer);
      Availability := (Buffer (1) and 16#01#) = 1;
      return Availability = Polarity;
   end Is_Measurement_Ready;

   ---------------------
   -- Get_Measurement --
   ---------------------

   function Get_Measurement
     (This : in out VL53L1X_Ranging_Sensor) return Measurement
   is
      Buffer : HAL.I2C.I2C_Data (0 .. 16);
      use all type HAL.UInt8;
      Status : Ranging_Status;
   begin
      Read (This,
            Index => VL53L1_RESULT_RANGE_STATUS,
            Data  => Buffer);

      Buffer (0) := Buffer (0) and 16#1f#;
      Status := (case Buffer (0) is
                    when 4 => Signal_Failure,
                    when 5 => Out_Of_Bounds,
                    when 6 => Sigma_Failure,
                    when 7 => Wraparound,
                    when 9 => Ok,
                    when others => raise VL53L1X_Error
                           with "invalid status " & Buffer (0)'Image);

      return Result : Measurement (Status => Status) do
         if Status = Ok then
            Result.Distance :=
              Millimetres (HAL.UInt16'(From_Device (Buffer (13 .. 14))));
         end if;
      end return;
   end Get_Measurement;

   -------------------------------
   -- Clear_Interrupt --
   -------------------------------

   procedure Clear_Interrupt
     (This : in out VL53L1X_Ranging_Sensor)
   is
   begin
      Write (This,
             Index => SYSTEM_INTERRUPT_CLEAR,
             Data  => (1 => 16#01#));
   end Clear_Interrupt;

   ------------------
   -- Stop_Ranging --
   ------------------

   procedure Stop_Ranging
     (This : in out VL53L1X_Ranging_Sensor)
   is
   begin
      Write (This,
             Index => SYSTEM_MODE_START,
             Data  => (1 => 16#00#));
      This.State := Initialized;
   end Stop_Ranging;

   --  Local stuff.

   ----------
   -- Read --
   ----------

   procedure Read (This  : in out VL53L1X_Ranging_Sensor;
                   Index :        HAL.UInt16;
                   Data  :    out HAL.I2C.I2C_Data)
   is
      I2C_Status : HAL.I2C.I2C_Status;
      use all type HAL.I2C.I2C_Status;
   begin
      HAL.I2C.Master_Transmit
        (This    => This.Port.all,
         Addr    => This.I2C_Address,
         Data    => To_Device (Index),
         Status  => I2C_Status);
      if I2C_Status /= Ok then
         raise VL53L1X_Error
           with "I2C write error during read: " & I2C_Status'Image;
      end if;
      HAL.I2C.Master_Receive
        (This    => This.Port.all,
         Addr    => This.I2C_Address,
         Data    => Data,
         Status  => I2C_Status);
      if I2C_Status /= Ok then
         raise VL53L1X_Error
           with "I2C read error: " & I2C_Status'Image;
      end if;
   end Read;

   procedure Read (This  : in out VL53L1X_Ranging_Sensor;
                   Index :        HAL.UInt16;
                   Data  :    out HAL.UInt16)
   is
      Buffer : HAL.I2C.I2C_Data (1 .. 2);
   begin
      Read (This,
            Index => Index,
            Data  => Buffer);
      Data := From_Device (Buffer);
   end Read;

   procedure Read (This  : in out VL53L1X_Ranging_Sensor;
                   Index :        HAL.UInt16;
                   Data  :    out HAL.UInt32)
   is
      Buffer : HAL.I2C.I2C_Data (1 .. 4);
   begin
      Read (This,
            Index => Index,
            Data  => Buffer);
      Data := From_Device (Buffer);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (This  : in out VL53L1X_Ranging_Sensor;
                    Index :        HAL.UInt16;
                    Data  :        HAL.I2C.I2C_Data)
   is
      use type HAL.I2C.I2C_Data;
      Buffer : constant HAL.I2C.I2C_Data (1 .. Data'Length + 2)
        := To_Device (Index) & Data;
      I2C_Status : HAL.I2C.I2C_Status;
      use all type HAL.I2C.I2C_Status;
   begin
      HAL.I2C.Master_Transmit
        (This    => This.Port.all,
         Addr    => This.I2C_Address,
         Data    => Buffer,
         Status  => I2C_Status);
      if I2C_Status /= Ok then
         raise VL53L1X_Error with "I2C write error: " & I2C_Status'Image;
      end if;
   end Write;

   procedure Write (This  : in out VL53L1X_Ranging_Sensor;
                    Index :        HAL.UInt16;
                    Data  :        HAL.UInt16)
   is
   begin
      Write (This,
             Index => Index,
             Data => To_Device (Data));
   end Write;

   procedure Write (This  : in out VL53L1X_Ranging_Sensor;
                    Index :        HAL.UInt16;
                    Data  :        HAL.UInt32)
   is
   begin
      Write (This,
             Index => Index,
             Data => To_Device (Data));
   end Write;

   ---------------
   -- To_Device --
   ---------------

   function To_Device (Value : HAL.UInt16) return Two_Byte_Array
   is
      As_Bytes : Two_Byte_Array with Address => Value'Address;
   begin
      case System.Default_Bit_Order is
         when System.High_Order_First =>
            return As_Bytes;
         when System.Low_Order_First =>
            return (1 => As_Bytes (2),
                    2 => As_Bytes (1));
      end case;
   end To_Device;

   function To_Device (Value : HAL.UInt32) return Four_Byte_Array
   is
      As_Bytes : Four_Byte_Array with Address => Value'Address;
   begin
      case System.Default_Bit_Order is
         when System.High_Order_First =>
            return As_Bytes;
         when System.Low_Order_First =>
            return (1 => As_Bytes (4),
                    2 => As_Bytes (3),
                    3 => As_Bytes (2),
                    4 => As_Bytes (1));
      end case;
   end To_Device;

   -----------------
   -- From_Device --
   -----------------

   function From_Device (Value : Two_Byte_Array) return HAL.UInt16
   is
      function Convert
      is new Ada.Unchecked_Conversion (Two_Byte_Array, HAL.UInt16);
   begin
      case System.Default_Bit_Order is
         when System.High_Order_First =>
            return Convert (Value);
         when System.Low_Order_First =>
            return Convert ((1 => Value (2),
                             2 => Value (1)));
      end case;
   end From_Device;

   function From_Device (Value : Four_Byte_Array) return HAL.UInt32
   is
      function Convert
      is new Ada.Unchecked_Conversion (Four_Byte_Array, HAL.UInt32);
   begin
      case System.Default_Bit_Order is
         when System.High_Order_First =>
            return Convert (Value);
         when System.Low_Order_First =>
            return Convert ((1 => Value (4),
                             2 => Value (3),
                             3 => Value (2),
                             4 => Value (1)));
      end case;
   end From_Device;

end VL53L1X;
