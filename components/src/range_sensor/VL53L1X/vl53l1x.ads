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
--   This file is based on STSW-IMG009, issue 3.5.1/UM2150 Rev 4.
--                                                                          --
--   COPYRIGHT(c) 2021 STMicroelectronics                                   --
------------------------------------------------------------------------------

with HAL.I2C;
with HAL.Time;

package VL53L1X is

   VL53L1X_Error : exception;

   type VL53L1X_Ranging_Sensor
     (Port   : not null HAL.I2C.Any_I2C_Port;
      Timing : not null HAL.Time.Any_Delays) is limited private;

   function Is_Booted (This : VL53L1X_Ranging_Sensor) return Boolean;

   type Boot_Status is (Ok, I2C_Error, I2C_Timeout, I2C_Busy);

   procedure Boot_Device
     (This             : in out VL53L1X_Ranging_Sensor;
      Loop_Interval_Ms :        Positive := 10;
      Status           :    out Boot_Status)
   with
     Pre => not Is_Booted (This),
     Post => Is_Booted (This) = (Status = Ok);

   procedure Set_Device_Address
     (This : in out VL53L1X_Ranging_Sensor;
      Addr :        HAL.I2C.I2C_Address)
   with
     Pre => Is_Booted (This);

   function Sensor_Initialized (This : VL53L1X_Ranging_Sensor) return Boolean;

   procedure Sensor_Init
     (This : in out VL53L1X_Ranging_Sensor)
   with
     Pre => Is_Booted (This),
     Post => Sensor_Initialized (This);

   type Distance_Mode is (Short, Long);

   function Get_Distance_Mode
     (This : in out VL53L1X_Ranging_Sensor) return Distance_Mode
   with Pre => Sensor_Initialized (This);

   procedure Set_Distance_Mode
     (This : in out VL53L1X_Ranging_Sensor;
      Mode :        Distance_Mode := Long)
   with Pre => Sensor_Initialized (This);
   --  The defaulted mode is what the device initializes to.

   subtype Milliseconds is Natural;
   subtype Measurement_Budget is Milliseconds
   with Predicate => Measurement_Budget in 15 | 20 | 33 | 50 | 100 | 200 | 500;

   function Get_Inter_Measurement_Time
     (This : in out VL53L1X_Ranging_Sensor) return Milliseconds
   with Pre => Sensor_Initialized (This);

   function Get_Measurement_Budget
     (This : in out VL53L1X_Ranging_Sensor) return Measurement_Budget
   with Pre => Sensor_Initialized (This);

   procedure Set_Inter_Measurement_Time
     (This     : in out VL53L1X_Ranging_Sensor;
      Interval :        Milliseconds)
   with Pre =>
     Sensor_Initialized (This) and not Ranging_Started (This);

   procedure Set_Measurement_Budget
     (This   : in out VL53L1X_Ranging_Sensor;
      Budget :        Measurement_Budget := 100)
   with Pre =>
     Sensor_Initialized (This) and not Ranging_Started (This);

   function Ranging_Started (This : VL53L1X_Ranging_Sensor) return Boolean;

   procedure Start_Ranging
     (This : in out VL53L1X_Ranging_Sensor)
   with
     Pre => Sensor_Initialized (This),
     Post => Ranging_Started (This);

   procedure Wait_For_Measurement
     (This             : in out VL53L1X_Ranging_Sensor;
      Loop_Interval_Ms :        Positive := 10)
   with
     Pre => Ranging_Started (This);

   function Is_Measurement_Ready
     (This : in out VL53L1X_Ranging_Sensor) return Boolean
   with
     Pre => Ranging_Started (This);

   type Ranging_Status is
     (Ok, Sigma_Failure, Signal_Failure, Out_Of_Bounds, Wraparound);
   --  Sigma_Failure: the repeatability or standard deviation of the
   --  measurement is bad due to a decreasing signal-to-noise
   --  ratio. Increasing the timing budget can improve the standard
   --  deviation and avoid this problem.
   --
   --  Signal_Failure: the return signal is too weak to return a good
   --  answer. The reason may be that the target is too far, or the
   --  target is not reflective enough, or the target is too
   --  small. Increasing the timing buget might help, but there may
   --  simply be no target available.
   --
   --  Out_Of_Bounds: the sensor is ranging in a “non-appropriated”
   --  zone and the measured result may be inconsistent. This status
   --  is considered as a warning but, in general, it happens when a
   --  target is at the maximum distance possible from the sensor,
   --  i.e. around 5 m. However, this is only for very bright targets.
   --
   --  Wraparound: may occur when the target is very reflective and
   --  the distance to the target is longer than the physical limited
   --  distance measurable by the sensor. Such distances include
   --  approximately 5 m when the senor is in Long distance mode and
   --  approximately 1.3 m when the sensor is in Short distance
   --  mode. Example: a traffic sign located at 6 m can be seen by the
   --  sensor and returns a range of 1 m. This is due to “radar
   --  aliasing”: if only an approximate distance is required, we may
   --  add 6 m to the distance returned. However, that is a very
   --  approximate estimation.

   subtype Millimetres is Natural;

   type Measurement (Status : Ranging_Status := Ok) is record
      case Status is
         when Ok     => Distance : Millimetres;
         when others => null;
      end case;
   end record;

   function Get_Measurement
     (This : in out VL53L1X_Ranging_Sensor) return Measurement
   with
     Pre => Ranging_Started (This);

   procedure Clear_Interrupt
     (This : in out VL53L1X_Ranging_Sensor)
   with
     Pre => Ranging_Started (This);

   procedure Stop_Ranging
     (This : in out VL53L1X_Ranging_Sensor)
   with
     Pre => Sensor_Initialized (This),
     Post => not Ranging_Started (This);

private

   type Sensor_State is (Unbooted, Booted, Initialized, Ranging);

   type VL53L1X_Ranging_Sensor (Port   : not null HAL.I2C.Any_I2C_Port;
                                Timing : not null HAL.Time.Any_Delays)
      is limited record
         State : Sensor_State := Unbooted;

         --  Default address: can be changed by software
         I2C_Address : HAL.I2C.I2C_Address := 16#52#;
      end record;

   function Is_Booted (This : VL53L1X_Ranging_Sensor) return Boolean
   is (This.State >= Booted);

   function Sensor_Initialized (This : VL53L1X_Ranging_Sensor) return Boolean
   is (This.State >= Initialized);

   function Ranging_Started (This : VL53L1X_Ranging_Sensor) return Boolean
   is (This.State = Ranging);

end VL53L1X;
