------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
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

--  This package provides an interface for the Bosch 9-DOF Absolute
--  Orientation IMU, integrating a triaxial 14-bit accelerometer, a triaxial
--  16-bit gyroscope, a triaxial geomagnetic sensor and a 32-bit cortex M0+
--  microcontroller running Bosch Sensortec sensor fusion software.
--
--  See https://www.adafruit.com/products/2472
--
--  See the Datasheet: "BNO055 Intelligent 9-axis absolute orientation sensor"
--  by Bosch Sensortec.
--  Document number BST-BNO055-DS000-13  (BST_BNO055_DS000_13-838248.pdf)
--  Version 1.3 or higher, although version 1.2 is likely close enough.

--  Give the supplied package for I2C-based communication (BNO055_I2C_IO),
--  and a procedure for delaying a given number of milliseconds, one could
--  instantiate the package as follows (for example):
--
--      with BNO055_I2C_IO;  use BNO055_I2C_IO;
--      with HAL.I2C;        use HAL.I2C;
--
--      with Bosch_BNO055;
--      with Delay_Milliseconds;
--
--      package BNO055_I2C is new Bosch_BNO055
--        (IO_Port            => IO_Port,
--         Any_IO_Port        => Any_IO_Port,
--         Sensor_Data_Buffer => I2C_Data);
--
--  In the above we are specifying the generic actual types from the
--  BNO055_I2C_IO package and the HAL.I2C package, in that order, and taking
--  the defaults for the generic formal I/O subprograms made visible via the
--  use-clause for the BNO055_I2C_IO package. Note that a USART-based
--  instantiation could look very similar; given similar naming only the
--  first two lines would change.
--
--  Similarly, the with-clause on procedure Delay_Milliseconds makes a
--  procedure matching the formal subprogram's profile directly visible
--  (and only one), so the default can be taken there as well.

pragma Restrictions (No_Streams);

with System;
with Interfaces; use Interfaces;
with HAL.I2C;    use HAL.I2C;

use HAL;

generic

   --  Driving the BNO055 is accomplished by writing to, and reading from,
   --  8-bit registers on the device. This internal communication is in
   --  terms of either I2C or serial (UART) input/output, specified as
   --  actual parameters for the following generic formal parameters.
   --
   --  Note that selection of the protocol is configured by the hardware
   --  "protocol select" pins PS0 and PS1, described in section 4.5 of the
   --  Bosch manual. These are explicitly marked header pins on the AdaFruit
   --  breakout board, for example. The protocol selected via the pins must
   --  match the generic actual parameters used to instantiate this generic
   --  unit. Thus, for example, if you use the pins to select the I2C-based
   --  protocol you must instantiate the generic unit with an I2C-based
   --  software abstraction via these parameters.

   type IO_Port (<>) is abstract tagged limited private;

   type Any_IO_Port is access all IO_Port'Class;

   with procedure Read
     (This     : Any_IO_Port;
      Register : UInt8;
      Value    : out UInt8) is <>;
   --  Get the Value at the address specified in Register via This port.

   with procedure Write
     (This     : Any_IO_Port;
      Register : UInt8;
      Value    : UInt8) is <>;
   --  Write the Value to the address specified in Register via This port.

   type Sensor_Data_Buffer is array (Natural range <>) of UInt8;

   with procedure Read_Buffer
     (This     : Any_IO_Port;
      Register : UInt8;
      Value    : out Sensor_Data_Buffer) is <>;
   --  Get the multiple Values at the address specified in Register via This
   --  port.

   with procedure Delay_Milliseconds (Count : Positive) is <>;
   --  Issue a relative delay for Count milliseconds. This formal subprogram
   --  removes the dependency on Ada.Real_Time so that this driver can be used
   --  with runtimes that do not have that language-defined facility.

package Bosch_BNO055 is

   type BNO055_9DOF_IMU (Port : not null Any_IO_Port) is tagged limited private;
   --  The BNO055 is a System in Package (SiP) integrating a triaxial 14-bit
   --  accelerometer, a triaxial 16-bit gyroscope, a triaxial geomagnetic
   --  sensor and a 32-bit Cortex M0+ microcontroller running Bosch Sensortec
   --  sensor fusion software providing fused orientation data. The designated
   --  IO_Port is used to communicate with the sensor, and will be a concrete
   --  instance of either an I2C port or a USART port.

   function Device_Id (This : in out BNO055_9DOF_IMU) return UInt8;
   --  Interacts with the device to read a device-class specific identifier
   --  that can be checked for a BNO055-specific value

   I_Am_BNO055 : constant := 16#A0#;
   --  The value expected to be returned from Device_Id

   --  These are the possible I2C addresses for the BNO055. Note that I2C is
   --  one of two options for communicating with the BNO055 device (the other
   --  being serial, ie USART).
   BNO055_Primary_Address   : constant I2C_Address := 16#28# * 2;
   BNO055_Alternate_Address : constant I2C_Address := 16#29# * 2;
   --  shift left one bit since we're using 7-bit addresses.

   BNO055_Min_Sample_Interval : constant := 10;
   --  According to the Datasheet, when using the internal oscillator the max
   --  data rate is 100Hz for the data-fusion configurations. The AdaFruit
   --  breakout has an external crystal available so this rate could be higher.
   --  See Datasheet Table 0-2 in section 1.2, and especially Table 3-14 in
   --  section 3.6.3. Rates for the Compass and M4G configurations are lower.
   --  The procedure Configure has a parameter named Use_External_Crystal to
   --  control that option.

   type Power_Modes is
     (Power_Mode_Normal,
      Power_Mode_Lowpower,
      Power_Mode_Suspend);

   for Power_Modes use
     (Power_Mode_Normal   => 16#00#,
      Power_Mode_Lowpower => 16#01#,
      Power_Mode_Suspend  => 16#02#);

   --  see section 3.3 of the Datasheet
   type Operating_Modes is
     (Operating_Mode_Config,
      Operating_Mode_Acc_Only,
      Operating_Mode_Mag_Only,
      Operating_Mode_Gyro_Only,
      Operating_Mode_Acc_Mag,
      Operating_Mode_Acc_Gyro,
      Operating_Mode_Mag_Gyro,
      Operating_Mode_AMG,
      Operating_Mode_IMU,
      Operating_Mode_Compass,
      Operating_Mode_M4G,
      Operating_Mode_NDOF_FMC_Off,
      Operating_Mode_NDOF);

   for Operating_Modes use
     (Operating_Mode_Config       => 16#00#,
      Operating_Mode_Acc_Only     => 16#01#,
      Operating_Mode_Mag_Only     => 16#02#,
      Operating_Mode_Gyro_Only    => 16#03#,
      Operating_Mode_Acc_Mag      => 16#04#,
      Operating_Mode_Acc_Gyro     => 16#05#,
      Operating_Mode_Mag_Gyro     => 16#06#,
      Operating_Mode_AMG          => 16#07#,
      Operating_Mode_IMU          => 16#08#,
      Operating_Mode_Compass      => 16#09#,
      Operating_Mode_M4G          => 16#0A#,
      Operating_Mode_NDOF_FMC_Off => 16#0B#,
      Operating_Mode_NDOF         => 16#0C#);

   function Current_Mode (This : in out BNO055_9DOF_IMU) return Operating_Modes;
   --  After power up reset, returns Operating_Mode_Config

   procedure Configure
     (This                 : in out BNO055_9DOF_IMU;
      Operating_Mode       : Operating_Modes := Operating_Mode_NDOF;
      Power_Mode           : Power_Modes     := Power_Mode_Normal;
      Use_External_Crystal : Boolean         := True)
     with Post => Current_Mode (This) = Operating_Mode;

   procedure Set_Mode
     (This : in out BNO055_9DOF_IMU;
      Mode : Operating_Modes)
     with Post => Current_Mode (This) = Mode;

   type Acceleration_Units is (Meters_Second_Squared, Milligravity);
   --  linear and gravity vector

   for Acceleration_Units use
     (Meters_Second_Squared => 2#00000000#,
      Milligravity          => 2#00000001#);

   procedure Set_Acceleration_Units
     (This  : in out BNO055_9DOF_IMU;
      Units : Acceleration_Units)
     with Post => Selected_Acceleration_Units (This) = Units;

   type Angular_Rate_Units is (Degrees_Second, Radians_Second);
   --  for gyro

   for Angular_Rate_Units use
     (Degrees_Second => 2#00000000#,
      Radians_Second => 2#00000010#);

   procedure Set_Angular_Rate_Units
     (This  : in out BNO055_9DOF_IMU;
      Units : Angular_Rate_Units);

   type Euler_Angle_Units is (Degrees, Radians);

   for Euler_Angle_Units use
     (Degrees => 2#00000000#,
      Radians => 2#00000100#);

   procedure Set_Euler_Angle_Units
     (This  : in out BNO055_9DOF_IMU;
      Units : Euler_Angle_Units)
   with Post => Selected_Euler_Units (This) = Units;

   type Temperature_Units is (Celsius, Fahrenheit);

   for Temperature_Units use
     (Celsius    => 2#00000000#,
      Fahrenheit => 2#00010000#);

   procedure Set_Temperature_Units
     (This  : in out BNO055_9DOF_IMU;
      Units : Temperature_Units)
     with Post => Selected_Temperature_Units (This) = Units;

   type Pitch_Rotation_Conventions is (Clockwise_Increasing, Clockwise_Decreasing);
   --  See the Datasheet, Table 3-12, in section 3.6.2 "Data output format"
   --  in which "Android" and "Windows" are used for these possible values.
   --  Nobody seems to know why they are used so we give them different names.
   --  Clockwise_Increasing corresponds to "Windows" and Clockwise_Decreasing
   --  corresponds to "Android" in that table. Clockwise_Increasing is the
   --  default on power-up.
   --
   --  The effect is only applied to Pitch angle values, as follows:
   --    Clockwise_Increasing => -180 to +180 degrees
   --    Clockwise_Decreasing => +180 to -180 degrees

   for Pitch_Rotation_Conventions use
     (Clockwise_Increasing => 2#00000000#,
      Clockwise_Decreasing => 2#10000000#);

   procedure Set_Pitch_Rotation
     (This  : in out BNO055_9DOF_IMU;
      Convention : Pitch_Rotation_Conventions);

   function Selected_Euler_Units (This : in out BNO055_9DOF_IMU)
     return Euler_Angle_Units;

   function Selected_Acceleration_Units (This : in out BNO055_9DOF_IMU)
     return Acceleration_Units;

   function Selected_Temperature_Units (This : in out BNO055_9DOF_IMU)
     return Temperature_Units;

   procedure Reset (This : in out BNO055_9DOF_IMU);
   --  Issues a software-reset command to the device, as opposed to a
   --  hardware-based reset that requires a physical connection.

   type Revision_Information is record
      Accelerometer : UInt8;
      Magnetometer  : UInt8;
      Gyroscope     : UInt8;
      Software      : UInt16;
      Bootloader    : UInt8;
   end record;

   procedure Get_Revision_Info
     (This : in out BNO055_9DOF_IMU;
      Info : out Revision_Information);

   --  see section 4.3.58
   type System_Status_Values is
     (Idle,
      System_Error,
      Initializing_Peripherals,
      System_Initalization,
      Executing_Self_Test,
      Sensor_Fusion_Algorithm_Running,
      System_Running_Without_Fusion_Algorithms)
     with Size => 8;

   for System_Status_Values use  -- confirming
     (Idle                                     => 0,
      System_Error                             => 1,
      Initializing_Peripherals                 => 2,
      System_Initalization                     => 3,
      Executing_Self_Test                      => 4,
      Sensor_Fusion_Algorithm_Running          => 5,
      System_Running_Without_Fusion_Algorithms => 6);

   --  see section 3.8
   type Self_Test_Results is record
      Accelerometer_Passed : Boolean;
      Magnetometer_Passed  : Boolean;
      Gyroscope_Passed     : Boolean;
      MCU_Passed           : Boolean;
   end record
     with Size => 8, Alignment => 1, Bit_Order => System.Low_Order_First;

   for Self_Test_Results use record
      Accelerometer_Passed at 0 range 0 .. 0;
      Magnetometer_Passed  at 0 range 1 .. 1;
      Gyroscope_Passed     at 0 range 2 .. 2;
      MCU_Passed           at 0 range 3 .. 3;
   end record;

   All_Tests_Passed : constant Self_Test_Results := (others => True);

   --  see section 4.3.59
   type System_Error_Values is
     (No_Error,
      Peripheral_Initialization_Error,
      System_Initialization_Error,
      Self_Test_Result_Failed,
      Register_Map_Value_Range_Error,
      Register_Map_Address_Range_Error,
      Register_Map_Write_Error,
      BNO_Low_Power_Mode_Not_Available,
      Accelerometer_Power_Mode_Not_Available,
      Fusion_Algorithm_Configuration_Error,
      Sensor_Configuration_Error)
     with Size => 8;

   for System_Error_Values use  -- confirming
     (No_Error                               => 0,
      Peripheral_Initialization_Error        => 1,
      System_Initialization_Error            => 2,
      Self_Test_Result_Failed                => 3,
      Register_Map_Value_Range_Error         => 4,
      Register_Map_Address_Range_Error       => 5,
      Register_Map_Write_Error               => 6,
      BNO_Low_Power_Mode_Not_Available       => 7,
      Accelerometer_Power_Mode_Not_Available => 8,
      Fusion_Algorithm_Configuration_Error   => 9,
      Sensor_Configuration_Error             => 10);

   procedure Get_Status
     (This          : in out BNO055_9DOF_IMU;
      System_Status : out System_Status_Values;
      Self_Test     : out Self_Test_Results;
      System_Error  : out System_Error_Values);

   subtype Calibration_Level    is UInt8 range 0 .. 3;
   subtype Uncalibrated         is Calibration_Level range 0 .. 0;
   subtype Partially_Calibrated is Calibration_Level range 1 .. 2;
   subtype Fully_Calibrated     is Calibration_Level range 3 .. 3;

   type Calibration_States is record
      Platform      : Calibration_Level;
      Gyroscope     : Calibration_Level;
      Accelerometer : Calibration_Level;
      Magnetometer  : Calibration_Level;
   end record;
   --  NB: The "platform" component ("system" in the Datasheet) can indicate
   --  that the platform is (fully) calibrated, but that does not mean that
   --  all sensors are individually fully calibrated. We define function
   --  Calibration_Complete for determining the state based on the three
   --  (or fewer, if so selected) sensors.

   function Sensor_Calibration (This : in out BNO055_9DOF_IMU) return Calibration_States;
   --  Note that you can select individual values if you don't want the
   --  full record result. For example, given an object of type BNO055_9DOF_IMU
   --  named "IMU" we could write any of the following:
   --
   --     Calibration_Data : Calibration_Values := IMU.Calibration;
   --  or
   --     Gyro_Calibration : UInt8 := IMU.Calibration.Gyroscope;
   --  or
   --     if IMU.Calibration.Gyroscope in Fully_Calibrated then

   type Sensor_Id is (Accelerometer, Gyroscope, Magnetometer);

   for Sensor_Id use (Accelerometer => 0, Gyroscope => 1, Magnetometer => 2);
      --  confirming, but these are the required values so we make it explicit

   type Sensors_Selection is array (Positive range <>) of Sensor_Id;

   All_Sensors : constant Sensors_Selection := (Accelerometer, Magnetometer, Gyroscope);

   function Calibration_Complete
     (This      : in out BNO055_9DOF_IMU;
      Selection : Sensors_Selection := All_Sensors)
      return Boolean
     with Pre => Selection'Length in 1 .. 3;
   --  Returns True iff all the selected sensors in Selection are in the
   --  Fully_Calibrated state. Sensors not specified are ignored. For example,
   --  to see if the gyroscope and magnetometer are fully configured, and
   --  ignore the accelerometer:
   --
   --     IMU.Calibration_Complete ((Gyroscope, Magnetometer));
   --
   --  but to check all three:
   --
   --     IMU.Calibration_Complete;

   type Axis is (X, Y, Z);

   type Sensor_Data is array (Axis) of Float;

   type Sensor_Data_Kinds is
     (Accelerometer_Data,
      Magnetometer_Data,
      Gyroscope_Data,
      Euler_Orientation,
      Linear_Acceleration_Data,
      Gravity_Data);

   function Output
     (This : in out BNO055_9DOF_IMU;
      Kind : Sensor_Data_Kinds)
      return Sensor_Data;
   --  This is one of the two primary data access functions, the other being
   --  function Quaternion_Orientation.

   type Quaternion is array (1 .. 4) of Float;
   --  W value is in the first indexed value
   --  X value is in the second indexed value
   --  Y value is in the third indexed value
   --  Z value is in the fourth indexed value
   --
   --  We define this type here, rather than using the full Quaternions package
   --  and type, because this facility does not manipulate quaternions itself,
   --  it just provides the data. In addition, we use a numeric type for the
   --  index so that this type will support direct conversion to the full
   --  Quaternion type without requiring UC or a user-defined function. An
   --  enumeration type for the index would not allow direct type conversion.

   function Quaternion_Orientation (This : in out BNO055_9DOF_IMU) return Quaternion;

   subtype Temperature_Source is Sensor_Id
     with Static_Predicate => Temperature_Source in Accelerometer | Gyroscope;
     --  The magnetometer's temperature is not available, per section 3.6.5.8
     --  of the Datasheet.

   function Sensor_Temperature
     (This   : in out BNO055_9DOF_IMU;
      Source : Temperature_Source)
      return Integer_8;
   --  Returns the temperature of the selected on-board device, for the sake
   --  of calibrating the unit as it warms up. NB: this is NOT the ambiant air
   --  temperature. By default, units are Celsius.

   type Sensor_Offset_Values is record
      Accel_Offset_X : Integer_16;
      Accel_Offset_Y : Integer_16;
      Accel_Offset_Z : Integer_16;
      Gyro_Offset_X  : Integer_16;
      Gyro_Offset_Y  : Integer_16;
      Gyro_Offset_Z  : Integer_16;
      Mag_Offset_X   : Integer_16;
      Mag_Offset_Y   : Integer_16;
      Mag_Offset_Z   : Integer_16;
      Accel_Radius   : Integer_16;
      Mag_Radius     : Integer_16;
   end record;

   function Sensor_Offsets (This : in out BNO055_9DOF_IMU) return Sensor_Offset_Values
     with Pre => This.Calibration_Complete (All_Sensors);
   --  The precondition reflects the Datasheet, section 3.11.4, under the
   --  "Reading Calibration profile " subsection.

   procedure Set_Sensor_Offsets
     (This    : in out BNO055_9DOF_IMU;
      Offsets : Sensor_Offset_Values);

   type Axis_Remapping_Selections is (Remap_To_X, Remap_To_Y, Remap_To_Z)
     with Size => 2;

   for Axis_Remapping_Selections use
     (Remap_To_X => 0, Remap_To_Y => 1, Remap_To_Z => 2); -- confirming

   type Axes_Remapping is array (Axis) of Axis_Remapping_Selections;

   --  see section 3.4 of the Datasheet
   procedure Remap_Axes
     (This : in out BNO055_9DOF_IMU;
      Map  : Axes_Remapping);
   --
   --  sample call:
   --  Remap_Axes (Map => (X => Remap_To_Z, Y => Remap_To_X, Z => Remap_To_Y));

   type Axis_Sign_Selections is (Remap_To_Positive, Remap_To_Negative)
     with Size => 2;

   for Axis_Sign_Selections use
     (Remap_To_Positive => 0, Remap_To_Negative => 1); -- confirming

   type Axes_Sign_Remapping is array (Axis) of Axis_Sign_Selections;

   --  see section 3.4 of the Datasheet
   procedure Remap_Axes_Signs
     (This : in out BNO055_9DOF_IMU;
      Map  : Axes_Sign_Remapping);

private

   type BNO055_9DOF_IMU (Port : not null Any_IO_Port) is
   tagged limited record
      Mode : Operating_Modes := Operating_Mode_Config;
   end record;

   --  masks for unit selections with the BNO055_UNIT_SEL register
   Acceleration_Units_Mask        : constant := 2#0000_0001#;
   Angular_Rate_Units_Mask        : constant := 2#0000_0010#;
   Euler_Angle_Units_Mask         : constant := 2#0000_0100#;
   Temperature_Units_Mask         : constant := 2#0001_0000#;
   Pitch_Rotation_Convention_Mask : constant := 2#1000_0000#;

   Operating_Mode_Mask     : constant := 2#00001111#;

   --  Page0 register definition start
   BNO055_CHIP_ID_ADDR       : constant := 16#00#;
   BNO055_ACCEL_REV_ID_ADDR  : constant := 16#01#;
   BNO055_MAG_REV_ID_ADDR    : constant := 16#02#;
   BNO055_GYRO_REV_ID_ADDR   : constant := 16#03#;
   BNO055_SW_REV_ID_LSB_ADDR : constant := 16#04#;
   BNO055_SW_REV_ID_MSB_ADDR : constant := 16#05#;
   BNO055_BL_REV_ID_ADDR     : constant := 16#06#;

   --  Page id register definition
   BNO055_PAGE_ID_ADDR : constant := 16#07#;

   --  Accel data register
   BNO055_ACCEL_DATA_X_LSB_ADDR : constant := 16#08#;
   BNO055_ACCEL_DATA_X_MSB_ADDR : constant := 16#09#;
   BNO055_ACCEL_DATA_Y_LSB_ADDR : constant := 16#0A#;
   BNO055_ACCEL_DATA_Y_MSB_ADDR : constant := 16#0B#;
   BNO055_ACCEL_DATA_Z_LSB_ADDR : constant := 16#0C#;
   BNO055_ACCEL_DATA_Z_MSB_ADDR : constant := 16#0D#;

   --   Mag data register
   BNO055_MAG_DATA_X_LSB_ADDR : constant := 16#0E#;
   BNO055_MAG_DATA_X_MSB_ADDR : constant := 16#0F#;
   BNO055_MAG_DATA_Y_LSB_ADDR : constant := 16#10#;
   BNO055_MAG_DATA_Y_MSB_ADDR : constant := 16#11#;
   BNO055_MAG_DATA_Z_LSB_ADDR : constant := 16#12#;
   BNO055_MAG_DATA_Z_MSB_ADDR : constant := 16#13#;

   --   Gyro data registers
   BNO055_GYRO_DATA_X_LSB_ADDR : constant := 16#14#;
   BNO055_GYRO_DATA_X_MSB_ADDR : constant := 16#15#;
   BNO055_GYRO_DATA_Y_LSB_ADDR : constant := 16#16#;
   BNO055_GYRO_DATA_Y_MSB_ADDR : constant := 16#17#;
   BNO055_GYRO_DATA_Z_LSB_ADDR : constant := 16#18#;
   BNO055_GYRO_DATA_Z_MSB_ADDR : constant := 16#19#;

   --   Euler data registers
   BNO055_EULER_H_LSB_ADDR : constant := 16#1A#;
   BNO055_EULER_H_MSB_ADDR : constant := 16#1B#;
   BNO055_EULER_R_LSB_ADDR : constant := 16#1C#;
   BNO055_EULER_R_MSB_ADDR : constant := 16#1D#;
   BNO055_EULER_P_LSB_ADDR : constant := 16#1E#;
   BNO055_EULER_P_MSB_ADDR : constant := 16#1F#;

   --   Quaternion data registers
   BNO055_QUATERNION_DATA_W_LSB_ADDR : constant := 16#20#;
   BNO055_QUATERNION_DATA_W_MSB_ADDR : constant := 16#21#;
   BNO055_QUATERNION_DATA_X_LSB_ADDR : constant := 16#22#;
   BNO055_QUATERNION_DATA_X_MSB_ADDR : constant := 16#23#;
   BNO055_QUATERNION_DATA_Y_LSB_ADDR : constant := 16#24#;
   BNO055_QUATERNION_DATA_Y_MSB_ADDR : constant := 16#25#;
   BNO055_QUATERNION_DATA_Z_LSB_ADDR : constant := 16#26#;
   BNO055_QUATERNION_DATA_Z_MSB_ADDR : constant := 16#27#;

   --   Linear acceleration data registers
   BNO055_LINEAR_ACCEL_DATA_X_LSB_ADDR : constant := 16#28#;
   BNO055_LINEAR_ACCEL_DATA_X_MSB_ADDR : constant := 16#29#;
   BNO055_LINEAR_ACCEL_DATA_Y_LSB_ADDR : constant := 16#2A#;
   BNO055_LINEAR_ACCEL_DATA_Y_MSB_ADDR : constant := 16#2B#;
   BNO055_LINEAR_ACCEL_DATA_Z_LSB_ADDR : constant := 16#2C#;
   BNO055_LINEAR_ACCEL_DATA_Z_MSB_ADDR : constant := 16#2D#;

   --   Gravity data registers
   BNO055_GRAVITY_DATA_X_LSB_ADDR : constant := 16#2E#;
   BNO055_GRAVITY_DATA_X_MSB_ADDR : constant := 16#2F#;
   BNO055_GRAVITY_DATA_Y_LSB_ADDR : constant := 16#30#;
   BNO055_GRAVITY_DATA_Y_MSB_ADDR : constant := 16#31#;
   BNO055_GRAVITY_DATA_Z_LSB_ADDR : constant := 16#32#;
   BNO055_GRAVITY_DATA_Z_MSB_ADDR : constant := 16#33#;

   --   Temperature data register
   BNO055_TEMP_ADDR : constant := 16#34#;

   --   Status registers
   BNO055_CALIB_STAT_ADDR      : constant := 16#35#;
   BNO055_SELFTEST_RESULT_ADDR : constant := 16#36#;
   BNO055_INTR_STAT_ADDR       : constant := 16#37#;

   BNO055_SYS_CLK_STAT_ADDR : constant := 16#38#;
   BNO055_SYS_STAT_ADDR     : constant := 16#39#;
   BNO055_SYS_ERR_ADDR      : constant := 16#3A#;

   --   Unit selection register
   BNO055_UNIT_SEL_ADDR    : constant := 16#3B#;
   BNO055_DATA_SELECT_ADDR : constant := 16#3C#;

   --   Mode registers
   BNO055_OPR_MODE_ADDR : constant := 16#3D#;
   BNO055_PWR_MODE_ADDR : constant := 16#3E#;

   BNO055_SYS_TRIGGER_ADDR : constant := 16#3F#;
   BNO055_TEMP_SOURCE_ADDR : constant := 16#40#;

   --   Axis remap registers
   BNO055_AXIS_MAP_CONFIG_ADDR : constant := 16#41#;
   BNO055_AXIS_MAP_SIGN_ADDR   : constant := 16#42#;

   --   SIC registers
   BNO055_SIC_MATRIX_0_LSB_ADDR : constant := 16#43#;
   BNO055_SIC_MATRIX_0_MSB_ADDR : constant := 16#44#;
   BNO055_SIC_MATRIX_1_LSB_ADDR : constant := 16#45#;
   BNO055_SIC_MATRIX_1_MSB_ADDR : constant := 16#46#;
   BNO055_SIC_MATRIX_2_LSB_ADDR : constant := 16#47#;
   BNO055_SIC_MATRIX_2_MSB_ADDR : constant := 16#48#;
   BNO055_SIC_MATRIX_3_LSB_ADDR : constant := 16#49#;
   BNO055_SIC_MATRIX_3_MSB_ADDR : constant := 16#4A#;
   BNO055_SIC_MATRIX_4_LSB_ADDR : constant := 16#4B#;
   BNO055_SIC_MATRIX_4_MSB_ADDR : constant := 16#4C#;
   BNO055_SIC_MATRIX_5_LSB_ADDR : constant := 16#4D#;
   BNO055_SIC_MATRIX_5_MSB_ADDR : constant := 16#4E#;
   BNO055_SIC_MATRIX_6_LSB_ADDR : constant := 16#4F#;
   BNO055_SIC_MATRIX_6_MSB_ADDR : constant := 16#50#;
   BNO055_SIC_MATRIX_7_LSB_ADDR : constant := 16#51#;
   BNO055_SIC_MATRIX_7_MSB_ADDR : constant := 16#52#;
   BNO055_SIC_MATRIX_8_LSB_ADDR : constant := 16#53#;
   BNO055_SIC_MATRIX_8_MSB_ADDR : constant := 16#54#;

   --   Accelerometer Offset registers
   ACCEL_OFFSET_X_LSB_ADDR : constant := 16#55#;
   ACCEL_OFFSET_X_MSB_ADDR : constant := 16#56#;
   ACCEL_OFFSET_Y_LSB_ADDR : constant := 16#57#;
   ACCEL_OFFSET_Y_MSB_ADDR : constant := 16#58#;
   ACCEL_OFFSET_Z_LSB_ADDR : constant := 16#59#;
   ACCEL_OFFSET_Z_MSB_ADDR : constant := 16#5A#;

   --   Magnetometer Offset registers
   MAG_OFFSET_X_LSB_ADDR : constant := 16#5B#;
   MAG_OFFSET_X_MSB_ADDR : constant := 16#5C#;
   MAG_OFFSET_Y_LSB_ADDR : constant := 16#5D#;
   MAG_OFFSET_Y_MSB_ADDR : constant := 16#5E#;
   MAG_OFFSET_Z_LSB_ADDR : constant := 16#5F#;
   MAG_OFFSET_Z_MSB_ADDR : constant := 16#60#;

   --   Gyroscope Offset registers
   GYRO_OFFSET_X_LSB_ADDR : constant := 16#61#;
   GYRO_OFFSET_X_MSB_ADDR : constant := 16#62#;
   GYRO_OFFSET_Y_LSB_ADDR : constant := 16#63#;
   GYRO_OFFSET_Y_MSB_ADDR : constant := 16#64#;
   GYRO_OFFSET_Z_LSB_ADDR : constant := 16#65#;
   GYRO_OFFSET_Z_MSB_ADDR : constant := 16#66#;

   --   Radius registers
   ACCEL_RADIUS_LSB_ADDR : constant := 16#67#;
   ACCEL_RADIUS_MSB_ADDR : constant := 16#68#;
   MAG_RADIUS_LSB_ADDR   : constant := 16#69#;
   MAG_RADIUS_MSB_ADDR   : constant := 16#6A#;

end Bosch_BNO055;
