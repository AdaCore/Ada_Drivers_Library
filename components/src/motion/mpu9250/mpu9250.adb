------------------------------------------------------------------------------
--                              Certyflie                                   --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

package body MPU9250 is

   --  Evaluate the self test and print the result of this evluation
   --  with the given string prepended
   function MPU9250_Evaluate_Self_Test
     (Low          : Float;
      High         : Float;
      Value        : Float;
      Debug_String : String;
      Do_Report    : Boolean;
      Reporter     : Test_Reporter) return Boolean;

   --  Public procedures and functions

   ------------------
   -- MPU9250_Init --
   ------------------

   procedure MPU9250_Init (Device : in out MPU9250_Device)
   is
   begin
      if Device.Is_Init then
         return;
      end if;

      --  Wait for MPU9250 startup
      delay until Time_First + MPU9250_STARTUP_TIME_MS;

      --  Set the device address
      Device.Address :=
        (case Device.I2C_AD0_Pin is
            when High => UInt10 (MPU9250_ADDRESS_AD0_HIGH) * 2,
            when Low  => UInt10 (MPU9250_ADDRESS_AD0_LOW) * 2);

      --        MPU9250_Configure_I2C;
      Device.Is_Init := True;
   end MPU9250_Init;

--     --------------------------------
--     -- MPU9250_Init_Control_Lines --
--     --------------------------------
--
--     procedure MPU9250_Init_Control_Lines
--     is
--        GPIO_Conf : GPIO_Port_Configuration;
--        Pins      : constant GPIO_Points :=
--                      (MPU9250_SCL_Pin, MPU9250_SDA_Pin);
--     begin
--        Enable_Clock (Pins);
--
--        Enable_Clock (MPU9250_I2C_PORT);
--
--        Reset (MPU9250_I2C_PORT);
--
--        Enable_Clock (MPU9250_I2C_PORT);
--
--        Configure_Alternate_Function
--          (MPU9250_SCL_Pin, MPU9250_SCL_AF);
--        Configure_Alternate_Function
--          (MPU9250_SDA_Pin, MPU9250_SDA_AF);
--
--        GPIO_Conf.Speed       := Speed_25MHz;
--        GPIO_Conf.Mode        := Mode_AF;
--        GPIO_Conf.Output_Type := Open_Drain;
--        GPIO_Conf.Resistors   := Pull_Up;
--
--        Configure_IO (Pins, GPIO_Conf);
--     end MPU9250_Init_Control_Lines;

   ---------------------------
   -- MPU9250_Configure_I2C --
   ---------------------------

--     procedure MPU9250_Configure_I2C is
--     begin
--        Reset (MPU9250_I2C_PORT);
--
--        Configure
--          (Handle => MPU9250_I2C_PORT,
--           Conf   =>
--             (Clock_Speed => 100_000,
--              Mode        => I2C_Mode,
--              Duty_Cycle  => DutyCycle_2,
--              Addressing_Mode => Addressing_Mode_7bit,
--              Own_Address     => 0,
--              others => <>));
--
--        Set_State (MPU9250_I2C_PORT, True);
--     end MPU9250_Configure_I2C;

   ------------------
   -- MPU9250_Test --
   ------------------

   function MPU9250_Test (Device : MPU9250_Device) return Boolean is
   begin
      return Device.Is_Init and then MPU9250_Test_Connection (Device);
   end MPU9250_Test;

   -----------------------------
   -- MPU9250_Test_Connection --
   -----------------------------

   function MPU9250_Test_Connection (Device : MPU9250_Device) return Boolean
   is
      Who_Am_I : Byte;
   begin
      MPU9250_Read_Byte_At_Register
        (Device   => Device,
         Reg_Addr => MPU9250_RA_WHO_AM_I,
         Data     => Who_Am_I);

      return Who_Am_I = MPU9250_DEVICE_ID;
   end MPU9250_Test_Connection;

   -----------------------
   -- MPU9250_Self_Test --
   -----------------------

   function MPU9250_Self_Test
     (Device    : in out MPU9250_Device;
      Do_Report : Boolean;
      Reporter  : Test_Reporter) return Boolean
   is
      type T_Int32_Array_3 is array (1 .. 3) of Integer_32;
      type T_Int32_Array_6 is array (1 .. 6) of Integer_32;
      type Float_Array_3 is array (1 .. 3) of Float;

      Raw_Data    : I2C_Data (1 .. 6) := (others => 0);
      Saved_Reg   : I2C_Data (1 .. 5) := (others => 0);
      Self_Test   : I2C_Data (1 .. 6) := (others => 0);
      Acc_Avg     : T_Int32_Array_3 := (others => 0);
      Gyro_Avg    : T_Int32_Array_3 := (others => 0);
      Acc_ST_Avg  : T_Int32_Array_3 := (others => 0);
      Gyro_ST_Avg : T_Int32_Array_3 := (others => 0);

      Factory_Trim : T_Int32_Array_6 := (others => 0);
      Acc_Diff     : Float_Array_3;
      Gyro_Diff    : Float_Array_3;
      FS           : constant Natural := 0;

      Next_Period : Time;
      Test_Status : Boolean;
   begin
      --  Save old configuration
      MPU9250_Read_Byte_At_Register
        (Device, MPU9250_RA_SMPLRT_DIV, Saved_Reg (1));
      MPU9250_Read_Byte_At_Register
        (Device, MPU9250_RA_CONFIG, Saved_Reg (2));
      MPU9250_Read_Byte_At_Register
        (Device, MPU9250_RA_GYRO_CONFIG, Saved_Reg (3));
      MPU9250_Read_Byte_At_Register
        (Device, MPU9250_RA_ACCEL_CONFIG_2, Saved_Reg (4));
      MPU9250_Read_Byte_At_Register
        (Device, MPU9250_RA_ACCEL_CONFIG, Saved_Reg (5));

      --  Write test configuration
      MPU9250_Write_Byte_At_Register
        (Device, MPU9250_RA_SMPLRT_DIV, 16#00#);
      MPU9250_Write_Byte_At_Register
        (Device, MPU9250_RA_CONFIG, 16#02#);
      MPU9250_Write_Byte_At_Register
        (Device, MPU9250_RA_GYRO_CONFIG, Shift_Left (FS, 3));
      MPU9250_Write_Byte_At_Register
        (Device, MPU9250_RA_ACCEL_CONFIG_2, 16#02#);
      MPU9250_Write_Byte_At_Register
        (Device, MPU9250_RA_ACCEL_CONFIG, Shift_Left (FS, 3));

      --  Get average current values of gyro and accelerometer
      for I in 1 .. 200 loop
         MPU9250_Read_Register (Device, MPU9250_RA_ACCEL_XOUT_H, Raw_Data);
         Acc_Avg (1) :=
           Acc_Avg (1) +
           Integer_32 (Fuse_Low_And_High_Register_Parts
                       (Raw_Data (1), Raw_Data (2)));
         Acc_Avg (2) :=
           Acc_Avg (2) +
           Integer_32 (Fuse_Low_And_High_Register_Parts
                       (Raw_Data (3), Raw_Data (4)));
         Acc_Avg (3) :=
           Acc_Avg (3) +
           Integer_32 (Fuse_Low_And_High_Register_Parts
                       (Raw_Data (5), Raw_Data (6)));

         MPU9250_Read_Register (Device, MPU9250_RA_GYRO_XOUT_H, Raw_Data);
         Gyro_Avg (1) :=
           Gyro_Avg (1) +
           Integer_32 (Fuse_Low_And_High_Register_Parts
                       (Raw_Data (1), Raw_Data (2)));
         Gyro_Avg (2) :=
           Gyro_Avg (2) +
           Integer_32 (Fuse_Low_And_High_Register_Parts
                       (Raw_Data (3), Raw_Data (4)));
         Gyro_Avg (3) :=
           Gyro_Avg (3) +
           Integer_32 (Fuse_Low_And_High_Register_Parts
                       (Raw_Data (5), Raw_Data (6)));
      end loop;

      --  Get average of 200 values and store as average current readings
      for I in T_Int32_Array_3'Range loop
         Acc_Avg (I) := Acc_Avg (I) / 200;
         Gyro_Avg (I) := Gyro_Avg (I) / 200;
      end loop;

      --  Configure the acceleromter for self test
      MPU9250_Write_Byte_At_Register
        (Device, MPU9250_RA_ACCEL_CONFIG, 16#E0#);
      MPU9250_Write_Byte_At_Register
        (Device, MPU9250_RA_GYRO_CONFIG, 16#E0#);

      --  Delay a while to let the device stabilize
      Next_Period := Clock + Milliseconds (25);
      delay until Next_Period;

      --  Get average self-test values of gyro and accelerometer
      for I in 1 .. 200 loop
         MPU9250_Read_Register (Device, MPU9250_RA_ACCEL_XOUT_H, Raw_Data);
         Acc_ST_Avg (1) :=
           Acc_ST_Avg (1) +
           Integer_32 (Fuse_Low_And_High_Register_Parts
                       (Raw_Data (1), Raw_Data (2)));
         Acc_ST_Avg (2) :=
           Acc_ST_Avg (2) +
           Integer_32 (Fuse_Low_And_High_Register_Parts
                       (Raw_Data (3), Raw_Data (4)));
         Acc_ST_Avg (3) :=
           Acc_ST_Avg (3) +
           Integer_32 (Fuse_Low_And_High_Register_Parts
                       (Raw_Data (5), Raw_Data (6)));

         MPU9250_Read_Register (Device, MPU9250_RA_GYRO_XOUT_H, Raw_Data);
         Gyro_ST_Avg (1) :=
           Gyro_ST_Avg (1) +
           Integer_32 (Fuse_Low_And_High_Register_Parts
                       (Raw_Data (1), Raw_Data (2)));
         Gyro_ST_Avg (2) :=
           Gyro_ST_Avg (2) +
           Integer_32 (Fuse_Low_And_High_Register_Parts
                       (Raw_Data (3), Raw_Data (4)));
         Gyro_ST_Avg (3) :=
           Gyro_ST_Avg (3) +
           Integer_32 (Fuse_Low_And_High_Register_Parts
                       (Raw_Data (5), Raw_Data (6)));
      end loop;

      --  Get average of 200 values and store as average self-test readings
      for I in T_Int32_Array_3'Range loop
         Acc_ST_Avg (I) := Acc_ST_Avg (I) / 200;
         Gyro_ST_Avg (I) := Gyro_ST_Avg (I) / 200;
      end loop;

      --  Configure the gyro and accelerometer for normal operation
      MPU9250_Write_Byte_At_Register
        (Device, MPU9250_RA_ACCEL_CONFIG, 16#00#);
      MPU9250_Write_Byte_At_Register
        (Device, MPU9250_RA_GYRO_CONFIG, 16#00#);

      --  Delay a while to let the device stabilize
      Next_Period := Clock + Milliseconds (25);
      delay until Next_Period;

      --  Retrieve Accelerometer and Gyro Factory Self - Test Code From USR_Reg
      MPU9250_Read_Byte_At_Register
        (Device, MPU9250_RA_ST_X_ACCEL, Self_Test (1));
      MPU9250_Read_Byte_At_Register
        (Device, MPU9250_RA_ST_Y_ACCEL, Self_Test (2));
      MPU9250_Read_Byte_At_Register
        (Device, MPU9250_RA_ST_Z_ACCEL, Self_Test (3));
      MPU9250_Read_Byte_At_Register
        (Device, MPU9250_RA_ST_X_GYRO, Self_Test (4));
      MPU9250_Read_Byte_At_Register
        (Device, MPU9250_RA_ST_Y_GYRO, Self_Test (5));
      MPU9250_Read_Byte_At_Register
        (Device, MPU9250_RA_ST_Z_GYRO, Self_Test (6));

      for I in 1 .. 6 loop
         if Self_Test (I) /= 0 then
            Factory_Trim (I) := Integer_32
              (MPU9250_ST_TB (Integer (Self_Test (I))));
         else
            Factory_Trim (I) := 0;
         end if;
      end loop;

      --  Report results as a ratio of (STR - FT)/FT; the change from
      --  Factory Trim of the Self - Test Response
      --  To get percent, must multiply by 100

      for I in 1 .. 3 loop
         Acc_Diff (I) :=
           100.0 * (Float (Acc_ST_Avg (I) - Acc_Avg (I) - Factory_Trim (I)) /
                      Float (Factory_Trim (I)));
         Gyro_Diff (I) :=
           100.0 * (Float (Gyro_ST_Avg (I) - Gyro_Avg (I) -
                      Factory_Trim (I + 3)) /
                      Float (Factory_Trim (I + 3)));
      end loop;

      --  Restore old configuration
      MPU9250_Write_Byte_At_Register
        (Device, MPU9250_RA_SMPLRT_DIV, Saved_Reg (1));
      MPU9250_Write_Byte_At_Register
        (Device, MPU9250_RA_CONFIG, Saved_Reg (2));
      MPU9250_Write_Byte_At_Register
        (Device, MPU9250_RA_GYRO_CONFIG, Saved_Reg (3));
      MPU9250_Write_Byte_At_Register
        (Device, MPU9250_RA_ACCEL_CONFIG_2, Saved_Reg (4));
      MPU9250_Write_Byte_At_Register
        (Device, MPU9250_RA_ACCEL_CONFIG, Saved_Reg (5));

      --  Check result
      Test_Status := MPU9250_Evaluate_Self_Test
        (MPU9250_ST_GYRO_LOW, MPU9250_ST_GYRO_HIGH,
         Gyro_Diff (1), "gyro X",
         Do_Report, Reporter);
      Test_Status := Test_Status and
        MPU9250_Evaluate_Self_Test
          (MPU9250_ST_GYRO_LOW, MPU9250_ST_GYRO_HIGH,
           Gyro_Diff (2), "gyro Y",
           Do_Report, Reporter);
      Test_Status := Test_Status and
        MPU9250_Evaluate_Self_Test
          (MPU9250_ST_GYRO_LOW, MPU9250_ST_GYRO_HIGH,
           Gyro_Diff (3), "gyro Z",
           Do_Report, Reporter);
      Test_Status := Test_Status and
        MPU9250_Evaluate_Self_Test
          (MPU9250_ST_ACCEL_LOW, MPU9250_ST_ACCEL_HIGH,
           Acc_Diff (1), "acc X",
           Do_Report, Reporter);
      Test_Status := Test_Status and
        MPU9250_Evaluate_Self_Test
          (MPU9250_ST_ACCEL_LOW, MPU9250_ST_ACCEL_HIGH,
           Acc_Diff (2), "acc Y",
           Do_Report, Reporter);
      Test_Status := Test_Status and
        MPU9250_Evaluate_Self_Test
          (MPU9250_ST_ACCEL_LOW, MPU9250_ST_ACCEL_HIGH,
           Acc_Diff (3), "acc Z",
           Do_Report, Reporter);

      return Test_Status;
   end MPU9250_Self_Test;

   -------------------
   -- MPU9250_Reset --
   -------------------

   procedure MPU9250_Reset (Device : in out MPU9250_Device) is
   begin
      MPU9250_Write_Bit_At_Register
        (Device    => Device,
         Reg_Addr  => MPU9250_RA_PWR_MGMT_1,
         Bit_Pos   => MPU9250_PWR1_DEVICE_RESET_BIT,
         Bit_Value => True);
   end MPU9250_Reset;

   --------------------------
   -- MPU9250_Get_Motion_6 --
   --------------------------

   procedure MPU9250_Get_Motion_6
     (Device : MPU9250_Device;
      Acc_X  : out Integer_16;
      Acc_Y  : out Integer_16;
      Acc_Z  : out Integer_16;
      Gyro_X : out Integer_16;
      Gyro_Y : out Integer_16;
      Gyro_Z : out Integer_16)
   is
      Raw_Data : I2C_Data (1 .. 14);
   begin
      MPU9250_Read_Register
        (Device   => Device,
         Reg_Addr => MPU9250_RA_ACCEL_XOUT_H,
         Data     => Raw_Data);

      Acc_X :=
        Fuse_Low_And_High_Register_Parts (Raw_Data (1), Raw_Data (2));
      Acc_Y :=
        Fuse_Low_And_High_Register_Parts (Raw_Data (3), Raw_Data (4));
      Acc_Z :=
        Fuse_Low_And_High_Register_Parts (Raw_Data (5), Raw_Data (6));

      Gyro_X :=
        Fuse_Low_And_High_Register_Parts (Raw_Data (9), Raw_Data (10));
      Gyro_Y :=
        Fuse_Low_And_High_Register_Parts (Raw_Data (11), Raw_Data (12));
      Gyro_Z :=
        Fuse_Low_And_High_Register_Parts (Raw_Data (13), Raw_Data (14));
   end MPU9250_Get_Motion_6;

   ------------------------------
   -- MPU9250_Set_Clock_Source --
   ------------------------------

   procedure MPU9250_Set_Clock_Source
     (Device       : in out MPU9250_Device;
      Clock_Source : MPU9250_Clock_Source)
   is
   begin
      MPU9250_Write_Bits_At_Register
        (Device        => Device,
         Reg_Addr      => MPU9250_RA_PWR_MGMT_1,
         Start_Bit_Pos => MPU9250_PWR1_CLKSEL_BIT,
         Data          => MPU9250_Clock_Source'Enum_Rep (Clock_Source),
         Length        => MPU9250_PWR1_CLKSEL_LENGTH);
   end MPU9250_Set_Clock_Source;

   ---------------------------
   -- MPU9250_Set_DLPF_Mode --
   ---------------------------

   procedure MPU9250_Set_DLPF_Mode
     (Device    : in out MPU9250_Device;
      DLPF_Mode : MPU9250_DLPF_Bandwidth_Mode)
   is
   begin
      MPU9250_Write_Bits_At_Register
        (Device        => Device,
         Reg_Addr      => MPU9250_RA_CONFIG,
         Start_Bit_Pos => MPU9250_CFG_DLPF_CFG_BIT,
         Data          => MPU9250_DLPF_Bandwidth_Mode'Enum_Rep (DLPF_Mode),
         Length        => MPU9250_CFG_DLPF_CFG_LENGTH);
   end MPU9250_Set_DLPF_Mode;

   ---------------------------------------
   -- MPU9250_Set_Full_Scale_Gyro_Range --
   ---------------------------------------

   procedure MPU9250_Set_Full_Scale_Gyro_Range
     (Device   : in out MPU9250_Device;
      FS_Range : MPU9250_FS_Gyro_Range)
   is
   begin
      MPU9250_Write_Bits_At_Register
        (Device        => Device,
         Reg_Addr      => MPU9250_RA_GYRO_CONFIG,
         Start_Bit_Pos => MPU9250_GCONFIG_FS_SEL_BIT,
         Data          => MPU9250_FS_Gyro_Range'Enum_Rep (FS_Range),
         Length        => MPU9250_GCONFIG_FS_SEL_LENGTH);
   end MPU9250_Set_Full_Scale_Gyro_Range;

   ----------------------------------------
   -- MPU9250_Set_Full_Scale_Accel_Range --
   ----------------------------------------

   procedure MPU9250_Set_Full_Scale_Accel_Range
     (Device   : in out MPU9250_Device;
      FS_Range : MPU9250_FS_Accel_Range)
   is
   begin
      MPU9250_Write_Bits_At_Register
        (Device        => Device,
         Reg_Addr      => MPU9250_RA_ACCEL_CONFIG,
         Start_Bit_Pos => MPU9250_ACONFIG_AFS_SEL_BIT,
         Data          => MPU9250_FS_Accel_Range'Enum_Rep (FS_Range),
         Length        => MPU9250_ACONFIG_AFS_SEL_LENGTH);
   end MPU9250_Set_Full_Scale_Accel_Range;

   ------------------------------------
   -- MPU9250_Set_I2C_Bypass_Enabled --
   ------------------------------------

   procedure MPU9250_Set_I2C_Bypass_Enabled
     (Device : in out MPU9250_Device;
      Value  : Boolean)
   is
   begin
      MPU9250_Write_Bit_At_Register
        (Device    => Device,
         Reg_Addr  => MPU9250_RA_INT_PIN_CFG,
         Bit_Pos   => MPU9250_INTCFG_I2C_BYPASS_EN_BIT,
         Bit_Value => Value);
   end MPU9250_Set_I2C_Bypass_Enabled;

   -----------------------------
   -- MPU9250_Set_Int_Enabled --
   -----------------------------

   procedure MPU9250_Set_Int_Enabled
     (Device : in out MPU9250_Device;
      Value  : Boolean)
   is
   begin
      --  Full register byte for all interrupts, for quick reading.
      --  Each bit should be set 0 for disabled, 1 for enabled.
      if Value then
         MPU9250_Write_Byte_At_Register
           (Device   => Device,
            Reg_Addr => MPU9250_RA_INT_ENABLE,
            Data     => 16#FF#);
      else
         MPU9250_Write_Byte_At_Register
           (Device   => Device,
            Reg_Addr => MPU9250_RA_INT_ENABLE,
            Data     => 16#00#);
      end if;
   end MPU9250_Set_Int_Enabled;

   ----------------------
   -- MPU9250_Set_Rate --
   ----------------------

   procedure MPU9250_Set_Rate
     (Device   : in out MPU9250_Device;
      Rate_Div : Byte)
   is
   begin
      MPU9250_Write_Byte_At_Register
        (Device   => Device,
         Reg_Addr => MPU9250_RA_SMPLRT_DIV,
         Data     => Rate_Div);
   end MPU9250_Set_Rate;

   -------------------------------
   -- MPU9250_Set_Sleep_Enabled --
   -------------------------------

   procedure MPU9250_Set_Sleep_Enabled
     (Device : in out MPU9250_Device;
      Value  : Boolean)
   is
   begin
      MPU9250_Write_Bit_At_Register
        (Device    => Device,
         Reg_Addr  => MPU9250_RA_PWR_MGMT_1,
         Bit_Pos   => MPU9250_PWR1_SLEEP_BIT,
         Bit_Value => Value);
   end MPU9250_Set_Sleep_Enabled;

   -------------------------------------
   -- MPU9250_Set_Temp_Sensor_Enabled --
   -------------------------------------

   procedure MPU9250_Set_Temp_Sensor_Enabled
     (Device : in out MPU9250_Device;
      Value  : Boolean)
   is
   begin
      --  True value for this bit actually disables it.
      MPU9250_Write_Bit_At_Register
        (Device    => Device,
         Reg_Addr  => MPU9250_RA_PWR_MGMT_1,
         Bit_Pos   => MPU9250_PWR1_TEMP_DIS_BIT,
         Bit_Value => not Value);
   end MPU9250_Set_Temp_Sensor_Enabled;

   -------------------------------------
   -- MPU9250_Get_Temp_Sensor_Enabled --
   -------------------------------------

   function MPU9250_Get_Temp_Sensor_Enabled
     (Device : MPU9250_Device) return Boolean
   is
   begin
      --  False value for this bit means that it is enabled
      return not MPU9250_Read_Bit_At_Register
        (Device   => Device,
         Reg_Addr => MPU9250_RA_PWR_MGMT_1,
         Bit_Pos  => MPU9250_PWR1_TEMP_DIS_BIT);
   end MPU9250_Get_Temp_Sensor_Enabled;

   --  Private procedures and functions

   --------------------------------
   -- MPU9250_Evaluate_Self_Test --
   --------------------------------

   function MPU9250_Evaluate_Self_Test
     (Low          : Float;
      High         : Float;
      Value        : Float;
      Debug_String : String;
      Do_Report    : Boolean;
      Reporter     : Test_Reporter) return Boolean
   is
      Has_Succeed : Boolean;

   begin
      if Value not in Low .. High then
         if Do_Report then
            Reporter
              ("Self test " & Debug_String & "[FAIL]" & ASCII.LF,
               Has_Succeed);
         end if;

         return False;
      else
         return True;
      end if;
   end MPU9250_Evaluate_Self_Test;

   ---------------------------
   -- MPU9250_Read_Register --
   ---------------------------

   procedure MPU9250_Read_Register
     (Device   : MPU9250_Device;
      Reg_Addr : Byte;
      Data     : in out I2C_Data)
   is
      Status : I2C_Status;
   begin
      Device.Port.Mem_Read
        (Addr          => Device.Address,
         Mem_Addr      => Short (Reg_Addr),
         Mem_Addr_Size => Memory_Size_8b,
         Data          => Data,
         Status        => Status);
   end MPU9250_Read_Register;

   -----------------------------------
   -- MPU9250_Read_Byte_At_Register --
   -----------------------------------

   procedure MPU9250_Read_Byte_At_Register
     (Device   : MPU9250_Device;
      Reg_Addr : Byte;
      Data     : out Byte)
   is
      Status : I2C_Status;
      I_Data : I2C_Data (1 .. 1);
   begin
      Device.Port.Mem_Read
        (Addr          => Device.Address,
         Mem_Addr      => Short (Reg_Addr),
         Mem_Addr_Size => Memory_Size_8b,
         Data          => I_Data,
         Status        => Status);
      Data := I_Data (1);
   end MPU9250_Read_Byte_At_Register;

   ----------------------------------
   -- MPU9250_Read_Bit_At_Register --
   ----------------------------------

   function MPU9250_Read_Bit_At_Register
     (Device   : MPU9250_Device;
      Reg_Addr : Byte;
      Bit_Pos  : T_Bit_Pos_8) return Boolean
   is
      Register_Value : Byte;
   begin
      MPU9250_Read_Byte_At_Register (Device, Reg_Addr, Register_Value);

      return (Register_Value and Shift_Left (1, Bit_Pos)) /= 0;
   end MPU9250_Read_Bit_At_Register;

   ----------------------------
   -- MPU9250_Write_Register --
   ----------------------------

   procedure MPU9250_Write_Register
     (Device   : MPU9250_Device;
      Reg_Addr : Byte;
      Data     : I2C_Data)
   is
      Status : I2C_Status;
   begin
      Device.Port.Mem_Write
        (Addr          => Device.Address,
         Mem_Addr      => Short (Reg_Addr),
         Mem_Addr_Size => Memory_Size_8b,
         Data          => Data,
         Status        => Status);
   end MPU9250_Write_Register;

   ------------------------------------
   -- MPU9250_Write_Byte_At_Register --
   ------------------------------------

   procedure MPU9250_Write_Byte_At_Register
     (Device   : MPU9250_Device;
      Reg_Addr : Byte;
      Data     : Byte)
   is
      Status : I2C_Status;
   begin
      Device.Port.Mem_Write
        (Addr          => Device.Address,
         Mem_Addr      => Short (Reg_Addr),
         Mem_Addr_Size => Memory_Size_8b,
         Data          => (1 => Data),
         Status        => Status);
   end MPU9250_Write_Byte_At_Register;

   -----------------------------------
   -- MPU9250_Write_Bit_At_Register --
   -----------------------------------

   procedure MPU9250_Write_Bit_At_Register
     (Device    : MPU9250_Device;
      Reg_Addr  : Byte;
      Bit_Pos   : T_Bit_Pos_8;
      Bit_Value : Boolean)
   is
      Register_Value : Byte;
   begin
      MPU9250_Read_Byte_At_Register (Device, Reg_Addr, Register_Value);

      Register_Value := (if Bit_Value then
                            Register_Value or (Shift_Left (1, Bit_Pos))
                         else
                            Register_Value and not (Shift_Left (1, Bit_Pos)));

      MPU9250_Write_Byte_At_Register (Device, Reg_Addr, Register_Value);
   end MPU9250_Write_Bit_At_Register;

   ------------------------------------
   -- MPU9250_Write_Bits_At_Register --
   ------------------------------------

   procedure MPU9250_Write_Bits_At_Register
     (Device        : MPU9250_Device;
      Reg_Addr      : Byte;
      Start_Bit_Pos : T_Bit_Pos_8;
      Data          : Byte;
      Length        : T_Bit_Pos_8)
   is
      Register_Value : Byte;
      Mask           : Byte;
      Data_Aux       : Byte := Data;
   begin
      MPU9250_Read_Byte_At_Register (Device, Reg_Addr, Register_Value);

      Mask := Shift_Left
        ((Shift_Left (1, Length) - 1), Start_Bit_Pos - Length + 1);
      Data_Aux := Shift_Left
        (Data_Aux, Start_Bit_Pos - Length + 1);
      Data_Aux := Data_Aux and Mask;
      Register_Value := Register_Value and not Mask;
      Register_Value := Register_Value or Data_Aux;

      MPU9250_Write_Byte_At_Register (Device, Reg_Addr, Register_Value);
   end MPU9250_Write_Bits_At_Register;

   --------------------------------------
   -- Fuse_Low_And_High_Register_Parts --
   --------------------------------------

   function Fuse_Low_And_High_Register_Parts
     (High : Byte;
      Low  : Byte) return Integer_16
   is
      ---------------------
      -- Uint16_To_Int16 --
      ---------------------

      function Uint16_To_Int16 is new Ada.Unchecked_Conversion
        (Unsigned_16, Integer_16);

      Register : Unsigned_16;
   begin
      Register := Shift_Left (Unsigned_16 (High), 8);
      Register := Register or Unsigned_16 (Low);

      return Uint16_To_Int16 (Register);
   end Fuse_Low_And_High_Register_Parts;

end MPU9250;
