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

with Ada.Unchecked_Conversion;

package body Bosch_BNO055 is

   subtype Register_Address is UInt8;

   function Value_At
     (This : in out BNO055_9DOF_IMU;  MSB, LSB : Register_Address)
      return UInt16 with Inline;
   --  MSB and LSB are the *register addresses* from which to get the values,
   --  not the values themselves

   function Value_At
     (This : in out BNO055_9DOF_IMU;  MSB, LSB : Register_Address)
      return Integer_16 with Inline;
   --  MSB and LSB are the *register addresses* from which to get the values,
   --  not the values themselves

   function To_Integer_16 (LSB, MSB : UInt8) return Integer_16 with Inline;
   --  returns a signed value, possibly negative based on high-order bit of MSB

   function As_Self_Test_Results is new Ada.Unchecked_Conversion
     (Source => UInt8, Target => Self_Test_Results);

   procedure Set_Units_Register
     (This       : in out BNO055_9DOF_IMU;
      New_Value  : UInt8;
      Units_Mask : UInt8);
   --  An internal utility routine. Sets the units bits within the UNIT_SEL
   --  register to the New_Value. Other bits are not altered.

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in  out BNO055_9DOF_IMU) is
      Reset_Bit : constant UInt8 := 16#20#;
   begin
      Write (This.Port, BNO055_SYS_TRIGGER_ADDR, Value => Reset_Bit);
      Delay_Milliseconds (650);
   end Reset;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This                 : in out BNO055_9DOF_IMU;
      Operating_Mode       : Operating_Modes := Operating_Mode_NDOF;
      Power_Mode           : Power_Modes     := Power_Mode_Normal;
      Use_External_Crystal : Boolean         := True)
   is
   begin
      Set_Mode (This, Operating_Mode_Config);

      --  select register map page zero
      Write (This.Port, BNO055_PAGE_ID_ADDR, 0);
      Delay_Milliseconds (10);

      Write (This.Port, BNO055_PWR_MODE_ADDR, Power_Mode'Enum_Rep);
      Delay_Milliseconds (10);

      --  clear interrupt, self-test, and reset bits, conditionally set
      --  external oscillator bit
      if Use_External_Crystal then
         Write (This.Port, BNO055_SYS_TRIGGER_ADDR, Value => 16#80#);
      else
         Write (This.Port, BNO055_SYS_TRIGGER_ADDR, Value => 0);
      end if;
      Delay_Milliseconds (10);

      Set_Mode (This, Operating_Mode);
      --  Note that Set_Mode automatically does a delay at the end so we don't
      --  need to do it here
   end Configure;

   ----------------------------
   -- Set_Acceleration_Units --
   ----------------------------

   procedure Set_Acceleration_Units
     (This  : in out BNO055_9DOF_IMU;
      Units : Acceleration_Units)
   is
   begin
      Set_Units_Register (This, Units'Enum_Rep, Acceleration_Units_Mask);
   end Set_Acceleration_Units;

   ----------------------------
   -- Set_Angular_Rate_Units --
   ----------------------------

   procedure Set_Angular_Rate_Units
     (This  : in out BNO055_9DOF_IMU;
      Units : Angular_Rate_Units)
   is
   begin
      Set_Units_Register (This, Units'Enum_Rep, Angular_Rate_Units_Mask);
   end Set_Angular_Rate_Units;

   ---------------------------
   -- Set_Euler_Angle_Units --
   ---------------------------

   procedure Set_Euler_Angle_Units
     (This  : in out BNO055_9DOF_IMU;
      Units : Euler_Angle_Units)
   is
   begin
      Set_Units_Register (This, Units'Enum_Rep, Euler_Angle_Units_Mask);
   end Set_Euler_Angle_Units;

   ---------------------------
   -- Set_Temperature_Units --
   ---------------------------

   procedure Set_Temperature_Units
     (This  : in out BNO055_9DOF_IMU;
      Units : Temperature_Units)
   is
   begin
      Set_Units_Register (This, Units'Enum_Rep, Temperature_Units_Mask);
   end Set_Temperature_Units;

   ------------------------
   -- Set_Pitch_Rotation --
   ------------------------

   procedure Set_Pitch_Rotation
     (This  : in out BNO055_9DOF_IMU;
      Convention : Pitch_Rotation_Conventions)
   is
   begin
      Set_Units_Register (This, Convention'Enum_Rep, Pitch_Rotation_Convention_Mask);
   end Set_Pitch_Rotation;

   --------------------------
   -- Selected_Euler_Units --
   --------------------------

   function Selected_Euler_Units
     (This  : in out BNO055_9DOF_IMU)
      return Euler_Angle_Units
   is
      Value : UInt8;
   begin
      Read (This.Port, BNO055_UNIT_SEL_ADDR, Value);
      if (Value and Euler_Angle_Units_Mask) = Degrees'Enum_Rep then
         return Degrees;
      else
         return Radians;
      end if;
   end Selected_Euler_Units;

   ---------------------------------
   -- Selected_Acceleration_Units --
   ---------------------------------

   function Selected_Acceleration_Units (This : in out BNO055_9DOF_IMU)
     return Acceleration_Units
   is
      Value : UInt8;
   begin
      Read (This.Port, BNO055_UNIT_SEL_ADDR, Value);
      if (Value and Acceleration_Units_Mask) = Meters_Second_Squared'Enum_Rep then
         return Meters_Second_Squared;
      else
         return Milligravity;
      end if;
   end Selected_Acceleration_Units;

   --------------------------------
   -- Selected_Temperature_Units --
   --------------------------------

   function Selected_Temperature_Units (This : in out BNO055_9DOF_IMU)
     return Temperature_Units
   is
      Value : UInt8;
   begin
      Read (This.Port, BNO055_UNIT_SEL_ADDR, Value);
      if (Value and Temperature_Units_Mask) = Fahrenheit'Enum_Rep then
         return Fahrenheit;
      else
         return Celsius;
      end if;
   end Selected_Temperature_Units;

   ---------------
   -- Device_Id --
   ---------------

   function Device_Id
     (This : in out BNO055_9DOF_IMU)
      return UInt8
   is
      Result : UInt8;
   begin
      Read (This.Port, BNO055_CHIP_ID_ADDR, Value => Result);
      return Result;
   end Device_Id;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (This : in out BNO055_9DOF_IMU;
      Mode : Operating_Modes)
   is
      --  Table 3-6 of the datasheet + 1ms as margin
      Switching_From_Config_Mode : constant := 8; -- milliseconds
      Switching_To_Config_Mode   : constant := 20; -- milliseconds

   begin
      if This.Mode = Operating_Mode_Config then
         Write (This.Port, BNO055_OPR_MODE_ADDR, Value => Mode'Enum_Rep);
         Delay_Milliseconds (Switching_From_Config_Mode);
      else
         Write (This.Port, BNO055_OPR_MODE_ADDR, Value => Operating_Mode_Config'Enum_Rep);
         Delay_Milliseconds (Switching_To_Config_Mode);

         Write (This.Port, BNO055_OPR_MODE_ADDR, Value => Mode'Enum_Rep);
         Delay_Milliseconds (Switching_From_Config_Mode);
      end if;

      This.Mode := Mode;
   end Set_Mode;

   ------------------
   -- Current_Mode --
   ------------------

   function Current_Mode (This : in out BNO055_9DOF_IMU) return Operating_Modes is
      Result : UInt8 := 0;
   begin
      Write (This.Port, BNO055_PAGE_ID_ADDR, 0);    -- x7, x0
      Delay_Milliseconds (10);

      Read (This.Port, BNO055_OPR_MODE_ADDR, Value => Result);
      return Operating_Modes'Val (Result and Operating_Mode_Mask);
   end Current_Mode;

   -----------------------
   -- Get_Revision_Info --
   -----------------------

   procedure Get_Revision_Info
     (This : in out BNO055_9DOF_IMU;
      Info : out Revision_Information)
   is
   begin
      Read (This.Port, BNO055_ACCEL_REV_ID_ADDR, Info.Accelerometer);
      Read (This.Port, BNO055_MAG_REV_ID_ADDR, Info.Magnetometer);
      Read (This.Port, BNO055_GYRO_REV_ID_ADDR, Info.Gyroscope);
      Read (This.Port, BNO055_BL_REV_ID_ADDR, Info.Bootloader);
      Info.Software := Value_At (This, MSB => BNO055_SW_REV_ID_MSB_ADDR, LSB => BNO055_SW_REV_ID_LSB_ADDR);
   end Get_Revision_Info;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (This          : in out BNO055_9DOF_IMU;
      System_Status : out System_Status_Values;
      Self_Test     : out Self_Test_Results;
      System_Error  : out System_Error_Values)
   is
      Value : UInt8;
   begin
      Read (This.Port, BNO055_SYS_STAT_ADDR, Value);
      System_Status := System_Status_Values'Val (Value);
      Read (This.Port, BNO055_SELFTEST_RESULT_ADDR, Value);
      Self_Test := As_Self_Test_Results (Value);
      Read (This.Port, BNO055_SYS_ERR_ADDR, Value);
      System_Error := System_Error_Values'Val (Value);
   end Get_Status;

   ------------------------
   -- Sensor_Calibration --
   ------------------------

   function Sensor_Calibration (This : in out BNO055_9DOF_IMU) return Calibration_States is
      Data   : UInt8;
      Mask   : constant := 2#00000011#;
      Result : Calibration_States;
   begin
      Read (This.Port, BNO055_CALIB_STAT_ADDR, Data);
      Result.Platform := Shift_Right (Data, 6) and Mask;
      Result.Gyroscope := Shift_Right (Data, 4) and Mask;
      Result.Accelerometer := Shift_Right (Data, 2) and Mask;
      Result.Magnetometer := Data and Mask;
      return Result;
   end Sensor_Calibration;

   --------------------------
   -- Calibration_Complete --
   --------------------------

   function Calibration_Complete
     (This      : in out BNO055_9DOF_IMU;
      Selection : Sensors_Selection := All_Sensors)
      return Boolean
   is
      States : constant Calibration_States := Sensor_Calibration (This);
   begin
      for Sensor of Selection loop
         case Sensor is
            when Gyroscope =>
               if States.Gyroscope not in Fully_Calibrated then
                  return False;
               end if;
            when Accelerometer =>
               if States.Accelerometer not in Fully_Calibrated then
                  return False;
               end if;
            when Magnetometer =>
               if States.Magnetometer not in Fully_Calibrated then
                  return False;
               end if;
         end case;
      end loop;

      return True;
   end Calibration_Complete;

   ----------
   -- Data --
   ----------

   function Output
     (This : in out BNO055_9DOF_IMU;
      Kind : Sensor_Data_Kinds)
      return Sensor_Data
   is
      Result   : Sensor_Data;
      Buffer   : Sensor_Data_Buffer (0 .. 5);
      New_X    : Integer_16;
      New_Y    : Integer_16;
      New_Z    : Integer_16;
      LSB      : Float;
      Source   : Register_Address;
   begin
      case Kind is
         when Accelerometer_Data =>
            Source := BNO055_ACCEL_DATA_X_LSB_ADDR;

         when Magnetometer_Data =>
            Source := BNO055_MAG_DATA_X_LSB_ADDR;

         when Gyroscope_Data =>
            Source := BNO055_GYRO_DATA_X_LSB_ADDR;

         when Euler_Orientation =>
            Source := BNO055_EULER_H_LSB_ADDR;

         when Linear_Acceleration_Data =>
            Source := BNO055_LINEAR_ACCEL_DATA_X_LSB_ADDR;

         when Gravity_Data =>
            Source := BNO055_GRAVITY_DATA_X_LSB_ADDR;
      end case;

      Read_Buffer (This.Port, Source, Buffer);
      --  By reading multiple UInt8s, the device ensures data consistency,
      --  whereas reading single UInt8s individually does not have that
      --  guarantee. See the Datasheet, section 3.7 "Data register shadowing"

      New_X := To_Integer_16 (LSB => Buffer (0), MSB => Buffer (1));
      New_Y := To_Integer_16 (LSB => Buffer (2), MSB => Buffer (3));
      New_Z := To_Integer_16 (LSB => Buffer (4), MSB => Buffer (5));

      case Kind is
         when Magnetometer_Data =>
            LSB := 16.0;
         when Euler_Orientation =>
            if Selected_Euler_Units (This) = Degrees then
               LSB := 16.0;
            else  -- radians
               LSB := 900.0;
            end if;
         when Gyroscope_Data =>
            LSB := 900.0;
         when Accelerometer_Data =>
            if Selected_Acceleration_Units (This) = Milligravity then
               LSB := 1.0;
            else -- Meters_Second_Squared
               LSB := 100.0;
            end if;
         when Linear_Acceleration_Data | Gravity_Data =>
            LSB := 100.0;
      end case;
      Result (X) := Float (New_X) / LSB;
      Result (Y) := Float (New_Y) / LSB;
      Result (Z) := Float (New_Z) / LSB;

      return Result;
   end Output;

   ----------------------------
   -- Quaternion_Orientation --
   ----------------------------

   function Quaternion_Orientation (This : in out BNO055_9DOF_IMU)
      return Quaternion
   is
      Buffer : Sensor_Data_Buffer (0 .. 7);
      New_W  : Integer_16;
      New_X  : Integer_16;
      New_Y  : Integer_16;
      New_Z  : Integer_16;
      Result : Quaternion;
      Scale  : constant Float := 1.0 / Float (2 ** 14); -- see section 3.6.5.5
   begin
      Read_Buffer (This.Port, BNO055_QUATERNION_DATA_W_LSB_ADDR, Buffer);
      --  By reading multiple UInt8s, the device ensures data consistency,
      --  whereas reading single UInt8s individually does not have that
      --  guarantee. See the Datasheet, section 3.7 "Data register shadowing"

      New_W := To_Integer_16 (MSB => Buffer (1), LSB => Buffer (0));
      New_X := To_Integer_16 (MSB => Buffer (3), LSB => Buffer (2));
      New_Y := To_Integer_16 (MSB => Buffer (5), LSB => Buffer (4));
      New_Z := To_Integer_16 (MSB => Buffer (7), LSB => Buffer (6));

      Result (1) := Float (New_W) * Scale;
      Result (2) := Float (New_X) * Scale;
      Result (3) := Float (New_Y) * Scale;
      Result (4) := Float (New_Z) * Scale;

      return Result;
   end Quaternion_Orientation;

   ------------------------
   -- Sensor_Temperature --
   ------------------------

   function Sensor_Temperature
     (This   : in out BNO055_9DOF_IMU;
      Source : Temperature_Source)
      return Integer_8
   is
      Result : UInt8;
   begin
      --  see Datasheet section 3.6.5.8
      Write (This.Port, BNO055_TEMP_SOURCE_ADDR, Source'Enum_Rep);
      Read (This.Port, BNO055_TEMP_ADDR, Value => Result);
      if Selected_Temperature_Units (This) = Fahrenheit then
         Result := Result * 2;  -- LSB is 2 for Fahrenheit, 1 for Celcius
      end if;
      return Integer_8 (Result);
   end Sensor_Temperature;

   ------------------------
   -- Get_Sensor_Offsets --
   ------------------------

   function Sensor_Offsets (This : in out BNO055_9DOF_IMU) return Sensor_Offset_Values
   is
      Previous_Mode : constant Operating_Modes := This.Mode;
      Offsets : Sensor_Offset_Values;
   begin
      Set_Mode (This, Operating_Mode_Config);
      Delay_Milliseconds (25);

      Offsets.Accel_Offset_X := Value_At (This, MSB => ACCEL_OFFSET_X_MSB_ADDR, LSB => ACCEL_OFFSET_X_LSB_ADDR);
      Offsets.Accel_Offset_Y := Value_At (This, MSB => ACCEL_OFFSET_Y_MSB_ADDR, LSB => ACCEL_OFFSET_Y_LSB_ADDR);
      Offsets.Accel_Offset_Z := Value_At (This, MSB => ACCEL_OFFSET_Z_MSB_ADDR, LSB => ACCEL_OFFSET_Z_LSB_ADDR);

      Offsets.Gyro_Offset_X := Value_At (This, MSB => GYRO_OFFSET_X_MSB_ADDR, LSB => GYRO_OFFSET_X_LSB_ADDR);
      Offsets.Gyro_Offset_Y := Value_At (This, MSB => GYRO_OFFSET_Y_MSB_ADDR, LSB => GYRO_OFFSET_Y_LSB_ADDR);
      Offsets.Gyro_Offset_Z := Value_At (This, MSB => GYRO_OFFSET_Z_MSB_ADDR, LSB => GYRO_OFFSET_Z_LSB_ADDR);

      Offsets.Mag_Offset_X := Value_At (This, MSB => MAG_OFFSET_X_MSB_ADDR, LSB => MAG_OFFSET_X_LSB_ADDR);
      Offsets.Mag_Offset_Y := Value_At (This, MSB => MAG_OFFSET_Y_MSB_ADDR, LSB => MAG_OFFSET_Y_LSB_ADDR);
      Offsets.Mag_Offset_Z := Value_At (This, MSB => MAG_OFFSET_Z_MSB_ADDR, LSB => MAG_OFFSET_Z_LSB_ADDR);

      Offsets.Accel_Radius := Value_At (This, MSB => ACCEL_RADIUS_MSB_ADDR, LSB => ACCEL_RADIUS_LSB_ADDR);
      Offsets.Mag_Radius   := Value_At (This, MSB => MAG_RADIUS_MSB_ADDR,   LSB => MAG_RADIUS_LSB_ADDR);

      Set_Mode (This, Previous_Mode);

      return Offsets;
   end Sensor_Offsets;

   ------------------------
   -- Set_Sensor_Offsets --
   ------------------------

   procedure Set_Sensor_Offsets
     (This    : in out BNO055_9DOF_IMU;
      Offsets : Sensor_Offset_Values)
   is
      Previous_Mode : constant Operating_Modes := This.Mode;
      Outgoing      : UInt16;
   begin
      Set_Mode (This, Operating_Mode_Config);
      Delay_Milliseconds (25);

      Outgoing := UInt16'Mod (Offsets.Accel_Offset_X);
      Write (This.Port, ACCEL_OFFSET_X_LSB_ADDR, Value => UInt8 (Outgoing and 16#FF#));
      Write (This.Port, ACCEL_OFFSET_X_MSB_ADDR, Value => UInt8 (Shift_Right (Outgoing, 8)));

      Outgoing := UInt16'Mod (Offsets.Accel_Offset_Y);
      Write (This.Port, ACCEL_OFFSET_Y_LSB_ADDR, Value => UInt8 (Outgoing and 16#FF#));
      Write (This.Port, ACCEL_OFFSET_Y_MSB_ADDR, Value => UInt8 (Shift_Right (Outgoing, 8)));

      Outgoing := UInt16'Mod (Offsets.Accel_Offset_Z);
      Write (This.Port, ACCEL_OFFSET_Z_LSB_ADDR, Value => UInt8 (Outgoing and 16#FF#));
      Write (This.Port, ACCEL_OFFSET_Z_MSB_ADDR, Value => UInt8 (Shift_Right (Outgoing, 8)));
      --  The Datasheet, section 3.6.4.1 "Accelerometer offset" says the
      --  configuration only occurs when the MSB of the Z offset is written

      Outgoing := UInt16'Mod (Offsets.Gyro_Offset_X);
      Write (This.Port, GYRO_OFFSET_X_LSB_ADDR, Value => UInt8 (Outgoing and 16#FF#));
      Write (This.Port, GYRO_OFFSET_X_MSB_ADDR, Value => UInt8 (Shift_Right (Outgoing, 8)));

      Outgoing := UInt16'Mod (Offsets.Gyro_Offset_Y);
      Write (This.Port, GYRO_OFFSET_Y_LSB_ADDR, Value => UInt8 (Outgoing and 16#FF#));
      Write (This.Port, GYRO_OFFSET_Y_MSB_ADDR, Value => UInt8 (Shift_Right (Outgoing, 8)));

      Outgoing := UInt16'Mod (Offsets.Gyro_Offset_Z);
      Write (This.Port, GYRO_OFFSET_Z_LSB_ADDR, Value => UInt8 (Outgoing and 16#FF#));
      Write (This.Port, GYRO_OFFSET_Z_MSB_ADDR, Value => UInt8 (Shift_Right (Outgoing, 8)));
      --  The Datasheet, section 3.6.4.3 "Gyroscope offset" says the
      --  configuration only occurs when the MSB of the Z offset is written

      Outgoing := UInt16'Mod (Offsets.Mag_Offset_X);
      Write (This.Port, MAG_OFFSET_X_LSB_ADDR, Value => UInt8 (Outgoing and 16#FF#));
      Write (This.Port, MAG_OFFSET_X_MSB_ADDR, Value => UInt8 (Shift_Right (Outgoing, 8)));

      Outgoing := UInt16'Mod (Offsets.Mag_Offset_Y);
      Write (This.Port, MAG_OFFSET_Y_LSB_ADDR, Value => UInt8 (Outgoing and 16#FF#));
      Write (This.Port, MAG_OFFSET_Y_MSB_ADDR, Value => UInt8 (Shift_Right (Outgoing, 8)));

      Outgoing := UInt16'Mod (Offsets.Mag_Offset_Z);
      Write (This.Port, MAG_OFFSET_Z_LSB_ADDR, Value => UInt8 (Outgoing and 16#FF#));
      Write (This.Port, MAG_OFFSET_Z_MSB_ADDR, Value => UInt8 (Shift_Right (Outgoing, 8)));
      --  The Datasheet, section 3.6.4.2 "Magnetometer offset" says the
      --  configuration only occurs when the MSB of the Z offset is written

      Outgoing := UInt16'Mod (Offsets.Accel_Radius);
      Write (This.Port, ACCEL_RADIUS_LSB_ADDR, Value => UInt8 (Outgoing and 16#FF#));
      Write (This.Port, ACCEL_RADIUS_MSB_ADDR, Value => UInt8 (Shift_Right (Outgoing, 8)));
      --  The Datasheet, section 3.6.4.4 "Radius" says the
      --  configuration only occurs when the MSB of the radius is written

      Outgoing := UInt16'Mod (Offsets.Mag_Radius);
      Write (This.Port, MAG_RADIUS_LSB_ADDR, Value => UInt8 (Outgoing and 16#FF#));
      Write (This.Port, MAG_RADIUS_MSB_ADDR, Value => UInt8 (Shift_Right (Outgoing, 8)));
      --  The Datasheet, section 3.6.4.4 "Radius" says the
      --  configuration only occurs when the MSB of the radius is written

      Set_Mode (This, Previous_Mode);
   end Set_Sensor_Offsets;

   ------------------------
   -- Set_Units_Register --
   ------------------------

   procedure Set_Units_Register
     (This       : in out BNO055_9DOF_IMU;
      New_Value  : UInt8;
      Units_Mask : UInt8)
   is
      Value         : UInt8;
      Previous_Mode : constant Operating_Modes := This.Mode;
   begin
      if Previous_Mode /= Operating_Mode_Config then
         Set_Mode (This, Operating_Mode_Config);
      end if;

      --  we don't set all the bits within this register in this routine, so we
      --  read the current value in order to preserve those that are unchanged
      Read (This.Port, BNO055_UNIT_SEL_ADDR, Value);
      Value := Value and not Units_Mask; -- clear the bits we're working with here
      Value := Value or New_Value'Enum_Rep;  -- set or clear the bits per the value
      Write (This.Port, BNO055_UNIT_SEL_ADDR, Value);
      Delay_Milliseconds (10);

      if Previous_Mode /= Operating_Mode_Config then
         Set_Mode (This, Previous_Mode);
      end if;
   end Set_Units_Register;

   ----------------
   -- Remap_Axes --
   ----------------

   procedure Remap_Axes
     (This : in out BNO055_9DOF_IMU;
      Map  : Axes_Remapping)
   is
      New_Mapping : UInt8 := 0;
   begin
      New_Mapping := New_Mapping or Axis_Remapping_Selections'Enum_Rep (Map (Z));
      New_Mapping := Shift_Left (New_Mapping, 2);
      New_Mapping := New_Mapping or Axis_Remapping_Selections'Enum_Rep (Map (Y));
      New_Mapping := Shift_Left (New_Mapping, 2);
      New_Mapping := New_Mapping or Axis_Remapping_Selections'Enum_Rep (Map (X));

      Write (This.Port, BNO055_AXIS_MAP_CONFIG_ADDR, Value => New_Mapping);
      Delay_Milliseconds (10);
   end Remap_Axes;

   ----------------------
   -- Remap_Axes_Signs --
   ----------------------

   procedure Remap_Axes_Signs
     (This : in out BNO055_9DOF_IMU;
      Map  : Axes_Sign_Remapping)
   is
      New_Mapping : UInt8 := 0;
   begin
      New_Mapping := New_Mapping or Axis_Sign_Selections'Enum_Rep (Map (X));
      New_Mapping := Shift_Left (New_Mapping, 1);

      New_Mapping := New_Mapping or Axis_Sign_Selections'Enum_Rep (Map (Y));
      New_Mapping := Shift_Left (New_Mapping, 1);

      New_Mapping := New_Mapping or Axis_Sign_Selections'Enum_Rep (Map (Z));

      Write (This.Port, BNO055_AXIS_MAP_SIGN_ADDR, Value => New_Mapping);
      Delay_Milliseconds (10);
   end Remap_Axes_Signs;

   --------------
   -- Value_At --
   --------------

   function Value_At (This : in out BNO055_9DOF_IMU; MSB, LSB : Register_Address) return Integer_16 is
      High, Low : UInt8;
   begin
      Read (This.Port, Register => MSB, Value => High);
      Read (This.Port, Register => LSB, Value => Low);
      return To_Integer_16 (LSB => Low, MSB => High);
   end Value_At;

   --------------
   -- Value_At --
   --------------

   function Value_At (This : in out BNO055_9DOF_IMU; MSB, LSB : Register_Address) return UInt16 is
      High, Low : UInt8;
      Result    : UInt16;
   begin
      Read (This.Port, Register => MSB, Value => High);
      Read (This.Port, Register => LSB, Value => Low);
      Result := UInt16 (High);
      Result := Shift_Left (Result, 8);
      Result := Result or UInt16 (Low);
      return Result;
   end Value_At;

   -------------------
   -- To_Integer_16 --
   -------------------

   function To_Integer_16 (LSB : UInt8;  MSB : UInt8) return Integer_16 is
      Result : Integer_32;
   begin
      Result := Integer_32 (MSB) * 256;
      Result := Result + Integer_32 (LSB);
      if (MSB and 16#80#) /= 0 then
         Result := -((16#FFFF# - Result) + 1);
      end if;
      return Integer_16 (Result);
   end To_Integer_16;

end Bosch_BNO055;
