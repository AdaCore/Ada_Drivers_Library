------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2016, AdaCore                          --
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
--   @file    lis3dsh.h                                                     --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   This file contains all the functions prototypes for the       --
--            lis3dsh.c firmware driver.                                    --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This package provides an interface to the LIS3DSH accelerometer chip used
--  on later versions of the STM32F4 Discovery boards.

with Interfaces;
with HAL; use HAL;

package LIS3DSH is

   type Three_Axis_Accelerometer is abstract tagged limited private;

   --  This device can be connected through I2C or SPI. This package implements
   --  an abstract driver with abstact IO subprograms, child packages will
   --  provide I2C or SPI specific implementation.

   type Axis_Acceleration is new Interfaces.Integer_16;

   type Axes_Accelerations is record
      X, Y, Z : Axis_Acceleration;
   end record;

   procedure Get_Accelerations
     (This : Three_Axis_Accelerometer;
      Axes : out Axes_Accelerations)
     with Pre => Configured (This);

   --  Note that the axes are orientated relative to the accelerometer chip
   --  itself, of course. According to the LIS3DSH datasheet (Doc ID 022405
   --  Rev 1, figure 2, page 10), pin 1 on the chip is marked on the top of
   --  the package with a white/gray dot. That dot will be in the upper left
   --  quadrant of the chip package when the board is oriented such that the
   --  text on the board is right-side up and can be read normally, with the
   --  power/debug USB connector CN1 at the top and the blue User button on the
   --  left. Given that board orientation, the X axis runs left-right through
   --  the User and Reset buttons. The Y axis is then orthogonal to that. The Z
   --  axis runs through the chip itself, orthogonal to the plane of the board,
   --  and will indicate 1G when the board is resting level.

   function Device_Id (This : Three_Axis_Accelerometer) return UInt8;

   I_Am_LIS3DSH : constant := 16#3F#;

   --  For the values assigned to the enumerals in the following types,
   --  see the LIS3DSH datasheet DM00040962. The values are their numerical
   --  bit patterns, shifted into the proper bit positions to be used for
   --  bit-masking into the control registers.
   --
   --  We assume nobody will want to use these as array type indexes, nor
   --  will they want to use them as the type of for-loop parameters.

   type Full_Scale_Selection is
     (Fullscale_2g,   --  2 g
      Fullscale_4g,   --  4 g
      Fullscale_6g,   --  6 g
      Fullscale_8g,   --  8 g
      Fullscale_16g)  --  16 g
   with Size => UInt8'Size;

   for Full_Scale_Selection use -- bits 3..5 of CTRL5
     (Fullscale_2g  => 16#00#,
      Fullscale_4g  => 16#08#,
      Fullscale_6g  => 16#10#,
      Fullscale_8g  => 16#18#,
      Fullscale_16g => 16#20#);

   type Data_Rate_Power_Mode_Selection is
     (Data_Rate_Powerdown, --  Power Down Mode
      Data_Rate_3_125Hz,   --  3.125 Hz Normal Mode
      Data_Rate_6_25Hz,    --  6.25  Hz Normal Mode
      Data_Rate_12_5Hz,    --  12.5  Hz Normal Mode
      Data_Rate_25Hz,      --  25    Hz Normal Mode
      Data_Rate_50Hz,      --  50    Hz Normal Mode
      Data_Rate_100Hz,     --  100   Hz Normal Mode
      Data_Rate_400Hz,     --  400   Hz Normal Mode
      Data_Rate_800Hz,     --  800   Hz Normal Mode
      Data_Rate_1600Hz);   --  1600  Hz Normal Mode

   for Data_Rate_Power_Mode_Selection use -- bits 4..6 of CTRL4
     (Data_Rate_Powerdown => 16#00#,
      Data_Rate_3_125Hz   => 16#10#,
      Data_Rate_6_25Hz    => 16#20#,
      Data_Rate_12_5Hz    => 16#30#,
      Data_Rate_25Hz      => 16#40#,
      Data_Rate_50Hz      => 16#50#,
      Data_Rate_100Hz     => 16#60#,
      Data_Rate_400Hz     => 16#70#,
      Data_Rate_800Hz     => 16#80#,
      Data_Rate_1600Hz    => 16#90#);

   type Anti_Aliasing_Filter_Bandwidth is
     (Filter_800Hz,
      Filter_400Hz,
      Filter_200Hz,
      Filter_50Hz);

   for Anti_Aliasing_Filter_Bandwidth use -- bits 6..7 of CTRL5
     (Filter_800Hz => 16#00#,
      Filter_400Hz => 16#08#,
      Filter_200Hz => 16#10#,
      Filter_50Hz  => 16#18#);

   type Self_Test_Selection is
     (Self_Test_Normal,
      Self_Test_Positive,
      Self_Test_Negative);

   for Self_Test_Selection use -- bits 1..2 of CTRL5
     (Self_Test_Normal   => 16#00#,
      Self_Test_Positive => 16#02#,
      Self_Test_Negative => 16#04#);

   type Direction_XYZ_Selection is
     (X_Enabled,
      Y_Enabled,
      Z_Enabled,
      XYZ_Enabled);

   for Direction_XYZ_Selection use -- bits 0..2 of CTRL4
     (X_Enabled   => 16#01#,
      Y_Enabled   => 16#02#,
      Z_Enabled   => 16#04#,
      XYZ_Enabled => 16#07#);

   type SPI_Serial_Interface_Mode_Selection is
     (Serial_Interface_4Wire,
      Serial_Interface_3Wire);

   for SPI_Serial_Interface_Mode_Selection use -- bit 0 of CTRL5
     (Serial_Interface_4Wire => 16#00#,
      Serial_Interface_3Wire => 16#01#);

   procedure Configure
     (This            : in out Three_Axis_Accelerometer;
      Output_DataRate : Data_Rate_Power_Mode_Selection;
      Axes_Enable     : Direction_XYZ_Selection;
      SPI_Wire        : SPI_Serial_Interface_Mode_Selection;
      Self_Test       : Self_Test_Selection;
      Full_Scale      : Full_Scale_Selection;
      Filter_BW       : Anti_Aliasing_Filter_Bandwidth)
     with Post => Configured (This);

   procedure Set_Full_Scale
     (This  : in out Three_Axis_Accelerometer;
      Scale : Full_Scale_Selection)
     with Pre => Configured (This);

   procedure Set_Low_Power
     (This : in out Three_Axis_Accelerometer;
      Mode : Data_Rate_Power_Mode_Selection)
     with Pre => Configured (This);

   procedure Set_Data_Rate
     (This     : in out Three_Axis_Accelerometer;
      DataRate : Data_Rate_Power_Mode_Selection)
     with Pre => Configured (This);

   function Selected_Sensitivity (This : Three_Axis_Accelerometer) return Float
     with Pre => Configured (This);
   --  The value determined by the current setting of Full_Scale, as set by
   --  a prior call to Configure_Accelerometer.

   --  Values return by function Selected_Sensitivity will be one of the
   --  following:
   Sensitivity_0_06mg : constant := 0.06;  -- 0.06 mg/digit
   Sensitivity_0_12mg : constant := 0.12;  -- 0.12 mg/digit
   Sensitivity_0_18mg : constant := 0.18;  -- 0.18 mg/digit
   Sensitivity_0_24mg : constant := 0.24;  -- 0.24 mg/digit
   Sensitivity_0_73mg : constant := 0.73;  -- 0.73 mg/digit

   procedure Reboot (This : in out Three_Axis_Accelerometer);

   type State_Machine_Routed_Interrupt is
     (SM_INT1,
      SM_INT2);

   for State_Machine_Routed_Interrupt use -- bit 3 of CTRL1/CTRL2
     (SM_INT1 => 2#0000_0000#,
      SM_INT2 => 2#0000_1000#);

   type Interrupt_Request_Selection is
     (Interrupt_Request_Latched,
      Interrupt_Request_Pulsed);

   for Interrupt_Request_Selection use -- bit 5 of CTRL3
     (Interrupt_Request_Latched => 16#00#,
      Interrupt_Request_Pulsed  => 16#20#);

   type Interrupt_Selection_Enablers is
     (Interrupt_2_Enable,
      Interrupt_1_Enable,
      Interrupt_1_2_Enable);

   for Interrupt_Selection_Enablers use -- values in bits 3..4 of CTRL3
     (Interrupt_1_Enable   => 2#1000_1000#,
      --  Also enables Data Ready Enable bit in bit 7
      Interrupt_2_Enable   => 2#0001_0000#,
      Interrupt_1_2_Enable => 2#1001_1000#);
      --  Also enables Data Ready Enable bit in bit 7
      --  NB lis3dsh.h uses different values???

   type Interrupt_Signal_Active_Selection is
     (Interrupt_Signal_Low,
      Interrupt_Signal_High);

   for Interrupt_Signal_Active_Selection use -- values in bit 6 of CTRL3
     (Interrupt_Signal_Low  => 2#0000_0000#,
      Interrupt_Signal_High => 2#0100_0000#);

   procedure Configure_Interrupts
     (This                       : in out Three_Axis_Accelerometer;
      Interrupt_Request          : Interrupt_Request_Selection;
      Interrupt_Selection_Enable : Interrupt_Selection_Enablers;
      Interrupt_Signal           : Interrupt_Signal_Active_Selection;
      State_Machine1_Enable      : Boolean;
      State_Machine1_Interrupt   : State_Machine_Routed_Interrupt;
      State_Machine2_Enable      : Boolean;
      State_Machine2_Interrupt   : State_Machine_Routed_Interrupt)
     with Pre => Configured (This);

   procedure Configure_Click_Interrupt (This : in out Three_Axis_Accelerometer)
     with Pre => Configured (This);

   function Temperature (This : Three_Axis_Accelerometer) return UInt8;
   --   Zero corresponds to approx. 25 degrees Celsius

   function Configured (This : Three_Axis_Accelerometer) return Boolean;

   type Register_Address is new UInt8;
   --  Prevent accidentally mixing addresses and data in I/O calls

   -----------------------------
   -- IO abstract subprograms --
   -----------------------------

   procedure IO_Write
     (This      : in out Three_Axis_Accelerometer;
      Value     : UInt8;
      WriteAddr : Register_Address) is abstract;

   procedure IO_Read
     (This     : Three_Axis_Accelerometer;
      Value    : out UInt8;
      ReadAddr : Register_Address) is abstract;

private

   type Three_Axis_Accelerometer is abstract tagged limited record
      --  Ensures the device has been configured via some prior call to
      --  Configure_Accelerometer. In addition, this flag also ensures that
      --  the I/O facility has been initialized because Configure_Accelerometer
      --  does that initialization too. Those routines that do NOT require
      --  prior configuration explicitly initialize the I/O facility.
      Device_Configured : Boolean := False;

      --  The value determined by the current setting of Full_Scale, as set
      --  by a prior call to Configure_Accelerometer. Storing this value is
      --  an optimization over querying the chip, which is what the function
      --  Selected_Sensitivity actually does. This component should always
      --  provide the same value as that function.
      Sensitivity : Float;
   end record;

   ----------------------------------------------------------------------------
   --  OUT_T: Temperature Output Register
   --  Read only register
   --  Default value: 0x21
   --  7:0 Temp7-Temp0: Temperature output Data
   ----------------------------------------------------------------------------
   Out_T : constant Register_Address := 16#0C#;

   ----------------------------------------------------------------------------
   --  INFO1 : Information Register 1
   --  Read only register
   --  Default value: 0x21
   ----------------------------------------------------------------------------
   INFO1 : constant Register_Address := 16#0D#;

   ----------------------------------------------------------------------------
   --  INFO2 : Information Register 2
   --  Read only register
   --  Default value: 0x00
   ----------------------------------------------------------------------------
   INFO2 : constant Register_Address := 16#0E#;

   ----------------------------------------------------------------------------
   --  WHO_AM_I : Device Identification Register
   --  Read only register
   --  Default value: 0x3F
   ----------------------------------------------------------------------------
   WHO_AM_I : constant Register_Address := 16#0F#;

   ----------------------------------------------------------------------------
   --  OFF_X : X-axis Offset Compensation Register
   --  Read Write register
   --  Default value: 0x00
   --  7:0 OFFx_7-OFFx_0: X-axis Offset Compensation Value
   ----------------------------------------------------------------------------
   OFF_X : constant Register_Address := 16#10#;

   ----------------------------------------------------------------------------
   --  OFF_Y : Y-axis Offset Compensation Register
   --  Read Write register
   --  Default value: 0x00
   --  7:0 OFFy_7-OFFy_0: Y-axis Offset Compensation Value
   ----------------------------------------------------------------------------
   OFF_Y : constant Register_Address := 16#11#;

   ----------------------------------------------------------------------------
   --  OFF_Z : Z-axis Offset Compensation Register
   --  Read Write register
   --  Default value: 0x00
   --  7:0 OFFz_7-OFFz_0: Z-axis Offset Compensation Value
   ----------------------------------------------------------------------------
   OFF_Z : constant Register_Address := 16#12#;

   ----------------------------------------------------------------------------
   --  CS_X : X-axis Constant Shift Register
   --  Read Write register
   --  Default value: 0x00
   --  7:0 CS_7-CS_0: X-axis Constant Shift Value
   ----------------------------------------------------------------------------
   CS_X : constant Register_Address := 16#13#;

   ----------------------------------------------------------------------------
   --  CS_Y : Y-axis Constant Shift Register
   --  Read Write register
   --  Default value: 0x00
   --  7:0 CS_7-CS_0: Y-axis Constant Shift Value
   ----------------------------------------------------------------------------
   CS_Y : constant Register_Address := 16#14#;

   ----------------------------------------------------------------------------
   --  CS_Z : Z-axis Constant Shift Value Register
   --  Read Write register
   --  Default value: 0x00
   --  7:0 CS_7-CS_0: Z-axis Constant Shift Value
   ----------------------------------------------------------------------------
   CS_Z : constant Register_Address := 16#15#;

   ----------------------------------------------------------------------------
   --  LC_L : Long Counter Low Register
   --  Read Write register
   --  Default value: 0x01
   --  7:0 LC_L_7-LC_L_0: Long Counter Low Value
   ----------------------------------------------------------------------------
   LC_L : constant Register_Address := 16#16#;

   ----------------------------------------------------------------------------
   --  LC_H : Long Counter High Register
   --  Read Write register
   --  Default value: 0x00
   --  7:0 LC_H_7-LC_H_0: Long Counter Low Value
   ----------------------------------------------------------------------------
   LC_H : constant Register_Address := 16#17#;

   ----------------------------------------------------------------------------
   --  STAT : State Machine Register
   --  Read only register
   --  Default value: 0x00
   --  7 LONG: LONG flag common to both State Machines
   --          0 - no interrupt
   --          1 - LongCounter interrupt flag
   --  6 SYNCW: Common information for OUTW host action waiting
   --           0 - no action waiting from Host
   --           1 - Host action is waiting after OUTW command
   --  5 SYNC1:
   --           0 - State Machine 1 running normally
   --           1 - SM1 stopped and waiting for restart request from SM2
   --  4 SYNC2:
   --           0 - State Machine 2 running normally
   --           1 - SM2 stopped and waiting for restart request from SM1
   --  3 INT_SM1: Interrupt signal on SM1 is reset when OUTS1 register is read
   --             0 - no interrupt on State Machine 1
   --             1 - State Machine 1 interrupt happened
   --  2 INT_SM2: Interrupt signal on SM2 is reset when OUTS2 register is read
   --             0 - no interrupt on State Machine 2
   --             1 - State Machine 2 interrupt happened
   --  1 DOR: Data OverRun bit
   --         0 - no overrun
   --         1 - data overrun
   --  0 DRDY: New data are ready in output registers
   --         0 - data not ready
   --         1 - data ready
   ----------------------------------------------------------------------------
   STAT : constant Register_Address := 16#18#;

   ----------------------------------------------------------------------------
   --  PEAK1 : Peak 1 Register
   --  Read only register
   --  Default value: 0x00
   --  7:0 PKx_7-PKx_0: Peak 1 Value for SM1
   ----------------------------------------------------------------------------
   PEAK1 : constant Register_Address := 16#19#;

   ----------------------------------------------------------------------------
   --  PEAK2 : Peak 2 Register
   --  Read only register
   --  Default value: 0x00
   --  7:0 PKx_7-PKx_0: Peak 2 value for SM2
   ----------------------------------------------------------------------------
   PEAK2 : constant Register_Address := 16#1A#;

   ----------------------------------------------------------------------------
   --  VFC_1 : Vector Filter Coefficient 1 Register
   --  Read Write register
   --  Default value: 0x00
   --  7:0 VFC1_7-VFC1_0: Vector Filter Coefficient 1 Value
   ----------------------------------------------------------------------------
   VFC_1 : constant Register_Address := 16#1B#;

   ----------------------------------------------------------------------------
   --  VFC_2 : Vector Filter Coefficient 2 Register
   --  Read Write register
   --  Default value: 0x00
   --  7:0 VFC2_7-VFC2_0: Vector Filter Coefficient 2 Value
   ----------------------------------------------------------------------------
   VFC_2 : constant Register_Address := 16#1C#;

   ----------------------------------------------------------------------------
   --  VFC_3 : Vector Filter Coefficient 3 Register
   --  Read Write register
   --  Default value: 0x00
   --  7:0 VFC3_7-VFC3_0: Vector Filter Coefficient 3 Value
   ----------------------------------------------------------------------------
   VFC_3 : constant Register_Address := 16#1D#;

   ----------------------------------------------------------------------------
   --  VFC_4 : Vector Filter Coefficient 4 Register
   --  Read Write register
   --  Default value: 0x00
   --  7:0 VFC4_7-VFC4_0: Vector Filter Coefficient 4 Value
   ----------------------------------------------------------------------------
   VFC_4 : constant Register_Address := 16#1E#;

   ----------------------------------------------------------------------------
   --  THRS3 : Threshold Value 3 Register
   --  Read Write register
   --  Default value: 0x00
   --  7:0 THRS3_7-THRS3_0: Common Threshold for Overrun Detection Value
   ----------------------------------------------------------------------------
   THRS3 : constant Register_Address := 16#1F#;

   ----------------------------------------------------------------------------
   --  CTRL_REG4 : Control Register 4
   --  Read Write register
   --  Default value: 0x07
   --  7:4 ODR3-ODR0: Data rate selection
   --            ODR3 | ODR2 | ODR1 | ORD0 | ORD Selection
   --            -------------------------------------------
   --              0  |  0   |  0   |  0   | Power Down (Default)
   --              0  |  0   |  0   |  1   | 3.125 Hz
   --              0  |  0   |  1   |  0   | 6.25 Hz
   --              0  |  0   |  1   |  1   | 12.5 Hz
   --              0  |  1   |  0   |  0   | 25 Hz
   --              0  |  1   |  0   |  1   | 50 Hz
   --              0  |  1   |  1   |  0   | 100 Hz
   --              0  |  1   |  1   |  1   | 400 Hz
   --              1  |  0   |  0   |  0   | 800 Hz
   --              1  |  0   |  0   |  1   | 1600 Hz
   --  *
   --  3 BDU: Block data update
   --         0: Output register not updated until High and Low reading
   --                                                                 (Default)
   --         1: Continuous update
   --  2 ZEN:
   --         0: Z-axis disable (Default)
   --         1: Z-axis enable
   --  1 YEN:
   --         0: Y-axis disable (Default)
   --         1: Y-axis enable
   --  0 XEN:
   --         0: Y-axis disable (Default)
   --         1: Y-axis enable
   ----------------------------------------------------------------------------
   CTRL_REG4 : constant Register_Address := 16#20#;

   ----------------------------------------------------------------------------
   --  CTRL_REG1 : Control Register 1 (SM1 interrupt configuration register)
   --  Read Write register
   --  Default value: 0x00
   --  7:5 HYST1_2-HYST1_0: Hysteresis which is added or subtracted from the
   --                       threshold values (THRS1_1 and THRS2_1) of SM1.
   --                       000 = 0 (Default)
   --                       111 = 7 (maximum Hysteresis)
   --  4 Reserved
   --  3 SM1_INT:
   --            0: State Machine 1 interrupt routed to INT1 (Default)
   --            1: State Machine 1 interrupt routed to INT2
   --  2 Reserved
   --  1 Reserved
   --  0 SM1_EN:
   --           0: State Machine 1 disabled. Temporary memories and registers
   --              related to this State Machine are left intact. (Default)
   --           1: State Machine 1 enabled.
   ----------------------------------------------------------------------------
   CTRL_REG1 : constant Register_Address := 16#21#;

   ----------------------------------------------------------------------------
   --  CTRL_REG2 : Control Register 2 (SM2 interrupt configuration register)
   --  Read Write register
   --  Default value: 0x00
   --  7:5 HYST2_2-HYST2_0: Hysteresis which is added or subtracted from the
   --                       threshold values (THRS1_2 and THRS2_2) of SM1.
   --                       000 = 0 (Default)
   --                       111 = 7 (maximum Hysteresis)
   --  4 Reserved
   --  3 SM2_INT:
   --            0: State Machine 2 interrupt routed to INT1 (Default)
   --            1: State Machine 2 interrupt routed to INT2
   --  2 Reserved
   --  1 Reserved
   --  0 SM2_EN:
   --           0: State Machine 2 disabled. Temporary memories and registers
   --              related to this State Machine are left intact. (Default)
   --           1: State Machine 2 enabled.
   ----------------------------------------------------------------------------
   CTRL_REG2 : constant Register_Address := 16#22#;

   ----------------------------------------------------------------------------
   --  CTRL_REG3 : Control Register 3 Read Write register Default value: 0x00 7
   --  DR_EN: Data-ready interrupt
   --          0 - Data-ready interrupt disabled (Default)
   --          1 - Data-ready interrupt enabled and routed to INT1
   --  6 IEA:
   --         0 - Interrupt signal active LOW (Default)
   --         1 - Interrupt signal active HIGH
   --  5 IEL:
   --         0 - Interrupt latched (Default)
   --         1 - Interrupt pulsed
   --  4 INT2_EN:
   --             0 - INT2 signal disabled (High-Z state) (Default)
   --             1 - INT2 signal enabled (signal pin fully functional)
   --  3 INT1_EN:
   --             0 - INT1 (DRDY) signal disabled (High-Z state) (Default)
   --             1 - INT1 (DRDY) signal enabled (signal pin fully functional)
   --                 DR_EN bit in CTRL_REG3 should be taken into account too
   --  2 VLIFT:
   --           0 - Vector filter disabled (Default)
   --           1 - Vector filter enabled
   --  1 Reserved 0 STRT: Soft Reset
   --          0 - (Default)
   --          1 - it resets the whole internal logic circuitry.
   ----------------------------------------------------------------------------
   CTRL_REG3 : constant Register_Address := 16#23#;

   ----------------------------------------------------------------------------
   --  CTRL_REG5 : Control Register 5
   --  Read Write register
   --  Default value: 0x00
   --  7:6 BW2-BW1: Anti aliasing filter bandwidth
   --            BW2 | BW1 | BW Selection
   --            -------------------------
   --             0  |  0  | 800 Hz (Default)
   --             0  |  1  | 40 Hz
   --             1  |  0  | 200 Hz
   --             1  |  1  | 50 Hz
   --  *
   --  5:3 FSCALE2-FSCALE0: Full scale selection
   --            FSCALE2 | FSCALE1 | FSCALE0 | Full scale selection
   --            --------------------------------------------------
   --               0    |    0    |    0    | +/-2g (Default)
   --               0    |    0    |    1    | +/-4g
   --               0    |    1    |    0    | +/-6g
   --               0    |    1    |    1    | +/-8g
   --               1    |    0    |    0    | +/-16g
   --  *
   --  2:1 ST2_ST1: Self-test Enable
   --            ST2 | ST1 | ST Selection
   --            -------------------------
   --             0  |  0  | Normal Mode (Default)
   --             0  |  1  | Positive sign self-test
   --             1  |  0  | Negative sign-test
   --             1  |  1  | Not Allowed
   --  *
   --  0 SIM: SPI serial internal interface mode selection
   --         0: 4-wire interface (Default)
   --         1: 3-wire interface
   ----------------------------------------------------------------------------
   CTRL_REG5 : constant Register_Address := 16#24#;

   ----------------------------------------------------------------------------
   --  CTRL_REG6 : Control Register 6
   --  Read Write register
   --  Default value: 0x00
   --  7 BOOT: Force reboot, cleared as soon as the reboot is finished. Active
   --          High.
   --  6 FIFO_EN: FIFO Enable
   --             0: disable (Default)
   --             1: enable

   --             0: disable (Default)
   --             1: enable
   --  4 IF_ADD_INC: Register address automatically increased during a
   --                multiple UInt8 access with a serial interface (I2C or SPI)
   --                0: disable (Default)
   --                1: enable
   --  3 I1_EMPTY: Enable FIFO Empty indication on INT1 pin.
   --              0: disable (Default)
   --              1: enable
   --  2 I1_WTM: FIFO Watermark interrupt on INT1 pin.
   --            0: disable (Default)
   --            1: enable
   --  1 I1_OVERRUN: FIFO Overrun interrupt on INT1 pin.
   --                0: disable (Default)
   --                1: enable
   --  0 I2_BOOT: BOOT interrupt on INT2 pin.
   --                0: disable (Default)
   --                1: enable
   ----------------------------------------------------------------------------
   CTRL_REG6 : constant Register_Address := 16#25#;

   ----------------------------------------------------------------------------
   --  STATUS : Status Data Register
   --  Read only register
   --  Default value: 0x00
   --  7 ZYXOR: X, Y and Z-axis Data Overrun.
   --           0: no Overrun has occurred (Default)
   --           1: a new set of data has overwritten the previous ones
   --  6 ZOR: Z-axis Data Overrun.
   --         0: no Overrun has occurred (Default)
   --         1: a new data for the Z-axis has overwritten the previous ones
   --  5 YOR: Y-axis Data Overrun.
   --         0: no Overrun has occurred (Default)
   --         1: a new data for the Y-axis has overwritten the previous ones
   --  4 XOR: X-axis Data Overrun.
   --         0: no Overrun has occurred (Default)
   --         1: a new data for the X-axis has overwritten the previous ones
   --  3 ZYXDA: X, Y and Z-axis new Data Available.
   --           0: a new set of data is not yet available (Default)
   --           1: a new set of data is available
   --  2 ZDA: Z-axis new data available.
   --         0: a new data for the Z-axis is not yet available (Default)
   --         1: a new data for Z-axis is available
   --  1 YDA: Y-axis new data available.
   --         0: a new data for the Y-axis is not yet available (Default)
   --         1: a new data for Y-axis is available
   --  0 XDA: X-axis new data available.
   --         0: a new data for the X-axis is not yet available (Default)
   --         1: a new data for X-axis is available
   ----------------------------------------------------------------------------
   STATUS : constant Register_Address := 16#27#;

   ----------------------------------------------------------------------------
   --  OUT_X_L : X-axis Output Acceleration Low Data
   --  Read only register
   --  Default value: output
   --  7:0 XD7-XD0: X-axis output Data
   ----------------------------------------------------------------------------
   OUT_X_L : constant Register_Address := 16#28#;

   ----------------------------------------------------------------------------
   --  OUT_X_H : X-axis Output Acceleration High Data
   --  Read only register
   --  Default value: output
   --  15:8 XD15-XD8: X-axis output Data
   ----------------------------------------------------------------------------
   OUT_X_H : constant Register_Address := 16#29#;

   ----------------------------------------------------------------------------
   --  OUT_Y_L : Y-axis Output Acceleration Low Data
   --  Read only register
   --  Default value: output
   --  7:0 YD7-YD0: Y-axis output Data
   ----------------------------------------------------------------------------
   OUT_Y_L : constant Register_Address := 16#2A#;

   ----------------------------------------------------------------------------
   --  OUT_Y_H : Y-axis Output Acceleration High Data
   --  Read only register
   --  Default value: output
   --  15:8 YD15-YD8: Y-axis output Data
   ----------------------------------------------------------------------------
   OUT_Y_H : constant Register_Address := 16#2B#;

   ----------------------------------------------------------------------------
   --  OUT_Z_L : Z-axis Output Acceleration Low Data
   --  Read only register
   --  Default value: output
   --  7:0 ZD7-ZD0: Z-axis output Data
   ----------------------------------------------------------------------------
   OUT_Z_L : constant Register_Address := 16#2C#;

   ----------------------------------------------------------------------------
   --  OUT_Z_H : Z-axis Output Acceleration High Data
   --  Read only register
   --  Default value: output
   --  15:8 ZD15-ZD8: Z-axis output Data
   ----------------------------------------------------------------------------
   OUT_Z_H : constant Register_Address := 16#2D#;

   ----------------------------------------------------------------------------
   --  FIFO_CTRL : FIFO Control Register
   --  Read/Write register
   --  Default value: 0x00
   --  7:5 FMODE2-FMODE0: FIFO mode
   --        FMODE2 | FMODE1 | FMODE0 | Mode description
   --        --------------------------------------------------
   --          0    |    0   |    0   | Bypass mode. FIFO turned off. (Default)
   --          0    |    0   |    1   | FIFO mode. Stop collecting data
   --                                              when FIFO is full.
   --          0    |    1   |    0   | Stream mode. If the FIFO is full, the
   --                                   new sample overwrites the older one
   --                                   (circular buffer).
   --          0    |    1   |    1   | Stream mode until trigger is
   --                                   de-asserted, then FIFO mode.
   --          1    |    0   |    0   | Bypass mode until trigger is
   --                                   de-asserted, then Stream mode.
   --          1    |    0   |    1   | Not to use.
   --          1    |    1   |    0   | Not to use.
   --          1    |    1   |    1   | Bypass mode until trigger is
   --                                   de-asserted, then FIFO mode.
   --  *
   --  4:0 WTMP4-WTMP0: FIFO Watermark pointer. It is the FIFO depth when the
   --                   Watermark is enabled
   ----------------------------------------------------------------------------
   FIFO_CTRL : constant Register_Address := 16#2E#;

   ----------------------------------------------------------------------------
   --  FIFO_SRC : FIFO Source Register
   --  Read only register
   --  Default value: 0x00
   --  7 WTM: Watermark status.
   --         0: FIFO filling is lower than WTM level (Default)
   --         1: FIFO filling is equal or higher than WTM level
   --  6 OVRN_FIFO: Overrun bit status.
   --               0: FIFO is not completely filled (Default)
   --               1: FIFO is completely filled
   --  5 EMPTY: Overrun bit status.
   --           0: FIFO not empty (Default)
   --           1: FIFO empty
   --  4:0 FSS: Number of samples stored in the FIFO - 1
   ----------------------------------------------------------------------------
   FIFO_SRC : constant Register_Address := 16#2F#;

   ----------------------------------------------------------------------------
   --  ST1_X : State Machine 1 Code Registers
   --  Write only register
   --  Default value: 0x00
   --  7:0 ST1_7-ST1_0: State Machine 1 Code Registers
   ----------------------------------------------------------------------------
   ST1_1  : constant Register_Address := 16#40#;
   ST1_2  : constant Register_Address := 16#41#;
   ST1_3  : constant Register_Address := 16#42#;
   ST1_4  : constant Register_Address := 16#43#;
   ST1_5  : constant Register_Address := 16#44#;
   ST1_6  : constant Register_Address := 16#45#;
   ST1_7  : constant Register_Address := 16#46#;
   ST1_8  : constant Register_Address := 16#47#;
   ST1_9  : constant Register_Address := 16#48#;
   ST1_10 : constant Register_Address := 16#49#;
   ST1_11 : constant Register_Address := 16#4A#;
   ST1_12 : constant Register_Address := 16#4B#;
   ST1_13 : constant Register_Address := 16#4C#;
   ST1_14 : constant Register_Address := 16#4D#;
   ST1_15 : constant Register_Address := 16#4E#;
   ST1_16 : constant Register_Address := 16#4F#;

   ----------------------------------------------------------------------------
   --  TIM4_1 : SM1 General Timer 4 Register
   --  Write only register
   --  Default value: 0x00
   --  7:0 TM_7-TM_0: SM1 Timer 4 Counter 1 Value
   ----------------------------------------------------------------------------
   TIM4_1 : constant Register_Address := 16#50#;

   ----------------------------------------------------------------------------
   --  TIM3_1 : SM1 General Timer 3 Register
   --  Write only register
   --  Default value: 0x00
   --  7:0 TM_7-TM_0: SM1 Timer 3 Counter 1 Value
   ----------------------------------------------------------------------------
   TIM3_1 : constant Register_Address := 16#51#;

   ----------------------------------------------------------------------------
   --  TIM2_1_L : SM1 General Timer 2 Low Register
   --  Write only register
   --  Default value: 0x00
   --  7:0 TM_7-TM_0: SM1 Timer 2 Counter 1 Low Value
   ----------------------------------------------------------------------------
   TIM2_1_L : constant Register_Address := 16#52#;

   ----------------------------------------------------------------------------
   --  TIM2_1_H : SM1 General Timer 2 High Register
   --  Write only register
   --  Default value: 0x00
   --  15:8 TM_15-TM_8: SM1 Timer 2 Counter 1 High Value
   ----------------------------------------------------------------------------
   TIM2_1_H : constant Register_Address := 16#53#;

   ----------------------------------------------------------------------------
   --  TIM1_1_L : SM1 General Timer 1 Low Register
   --  Write only register
   --  Default value: 0x00
   --  7:0 TM_7-TM_0: SM1 Timer 1 Counter 1 Low Value
   ----------------------------------------------------------------------------
   TIM1_1_L : constant Register_Address := 16#54#;

   ----------------------------------------------------------------------------
   --  TIM1_1_H : SM1 General Timer 1 High Register
   --  Write only register
   --  Default value: 0x00
   --  15:8 TM_15-TM_8: SM1 Timer 1 Counter 1 High Value
   ----------------------------------------------------------------------------
   TIM1_1_H : constant Register_Address := 16#55#;

   ----------------------------------------------------------------------------
   --  THRS2_1 : SM1 Threshold Value 1 Register
   --  Write only register
   --  Default value: 0x00
   --  7:0 THS7-THS0: SM1 Threshold Value 1
   ----------------------------------------------------------------------------
   THRS2_1 : constant Register_Address := 16#56#;

   ----------------------------------------------------------------------------
   --  THRS1_1 : SM1 Threshold Value 2 Register
   --  Write only register
   --  Default value: 0x00
   --  7:0 THS7-THS0: SM1 Threshold Value 2
   ----------------------------------------------------------------------------
   THRS1_1 : constant Register_Address := 16#57#;

   ----------------------------------------------------------------------------
   --  MASK1_B : SM1 Swap Axis and Sign Mask Register
   --  Write only register
   --  Default value: 0x00
   --  7 P_X: X-Axis Positive Motion Detection
   --         0: X+ disabled
   --         1: X+ enabled
   --  6 N_X: X-Axis Negative Motion Detection
   --         0: X- disabled
   --         1: X- enabled
   --  5 P_Y: Y-Axis Positive Motion Detection
   --         0: Y+ disabled
   --         1: Y+ enabled
   --  4 N_Y: Y-Axis Negative Motion Detection
   --         0: Y- disabled
   --         1: Y- enabled
   --  3 P_Z: X-Axis Positive Motion Detection
   --         0: Z+ disabled
   --         1: Z+ enabled
   --  2 N_Z: X-Axis Negative Motion Detection
   --         0: Z- disabled
   --         1: Z- enabled
   --  1 P_V:
   --         0: V+ disabled
   --         1: V+ enabled
   --  0 N_V:
   --         0: V- disabled
   --         1: V- enabled
   ----------------------------------------------------------------------------
   MASK1_B : constant Register_Address := 16#59#;

   ----------------------------------------------------------------------------
   --  MASK1_A : SM1 Default Axis and Sign Mask Register
   --  Write only register
   --  Default value: 0x00
   --  7 P_X: X-Axis Positive Motion Detection
   --         0: X+ disabled
   --         1: X+ enabled
   --  6 N_X: X-Axis Negative Motion Detection
   --         0: X- disabled
   --         1: X- enabled
   --  5 P_Y: Y-Axis Positive Motion Detection
   --         0: Y+ disabled
   --         1: Y+ enabled
   --  4 N_Y: Y-Axis Negative Motion Detection
   --         0: Y- disabled
   --         1: Y- enabled
   --  3 P_Z: X-Axis Positive Motion Detection
   --         0: Z+ disabled
   --         1: Z+ enabled
   --  2 N_Z: X-Axis Negative Motion Detection
   --         0: Z- disabled
   --         1: Z- enabled
   --  1 P_V:
   --         0: V+ disabled
   --         1: V+ enabled
   --  0 N_V:
   --         0: V- disabled
   --         1: V- enabled
   ----------------------------------------------------------------------------
   MASK1_A : constant Register_Address := 16#5A#;

   ----------------------------------------------------------------------------
   --  SETT1 : SM1 Detection Settings Register
   --  Write only register
   --  Default value: 0x00
   --  7 P_DET: SM1 peak detection bit
   --           0: peak detection disabled (Default)
   --           1: peak detection enabled
   --  6 THR3_SA:
   --             0: no action (Default)
   --             1: threshold 3 enabled for axis and sign mask reset (MASK1_B)
   --  5 ABS:
   --         0: unsigned thresholds THRSx (Default)
   --         1: signed thresholds THRSx
   --  4 Reserved
   --  3 Reserved
   --  2 THR3_MA:
   --             0: no action (Default)
   --             1: threshold 3 enabled for axis and sign mask reset (MASK1_A)
   --  1 R_TAM: Next condition validation flag
   --           0: mask frozen on axis that triggers the condition (Default)
   --           1: standard mask always evaluated
   --  0 SITR:
   --          0: no actions (Default)
   --          1: STOP and CONT commands generate an interrupt and perform
   --             output actions as OUTC command.
   ----------------------------------------------------------------------------
   SETT1 : constant Register_Address := 16#5B#;

   ----------------------------------------------------------------------------
   --  PR1 : SM1 Program and Reset Pointers Register
   --  Read only register
   --  Default value: 0x00
   --  7:4 PP3-PP0: SM1 program pointer address
   --  3:0 RP3-RP0: SM1 reset pointer address
   ----------------------------------------------------------------------------
   PR1 : constant Register_Address := 16#5C#;

   ----------------------------------------------------------------------------
   --  TC1_L : SM1 General Timer Counter Low Register
   --  Read only register
   --  Default value: 0x00
   --  7:0 TC1_7-TC1_0: SM1 General Timer Counter Low Value
   ----------------------------------------------------------------------------
   TC1_L : constant Register_Address := 16#5D#;

   ----------------------------------------------------------------------------
   --  TC1_H : SM1 General Timer Counter High Register
   --  Read only register
   --  Default value: 0x00
   --  15:8 TC1_15-TC1_8: SM1 General Timer Counter High Value
   ----------------------------------------------------------------------------
   TC1_H : constant Register_Address := 16#5E#;

   ----------------------------------------------------------------------------
   --  OUTS1 : SM1 Output Set Flag Register
   --  Read only register
   --  Default value: 0x00
   --  7 P_X:
   --         0: X+ noshow
   --         1: X+ show
   --  6 N_X:
   --         0: X- noshow
   --         1: X- show
   --  5 P_Y:
   --         0: Y+ noshow
   --         1: Y+ show
   --  4 N_Y:
   --         0: Y- noshow
   --         1: Y- show
   --  3 P_Z:
   --         0: Z+ noshow
   --         1: Z+ show
   --  2 N_Z:
   --         0: Z- noshow
   --         1: Z- show
   --  1 P_V:
   --         0: V+ noshow
   --         1: V+ show
   --  0 N_V:
   --         0: V- noshow
   --         1: V- show
   ----------------------------------------------------------------------------
   OUTS1 : constant Register_Address := 16#5F#;

   ----------------------------------------------------------------------------
   --  ST2_X : State Machine 2 Code Registers
   --  Write only register
   --  Default value: 0x00
   --  7:0 ST2_7-ST2_0: State Machine 2 Code Registers
   ----------------------------------------------------------------------------
   ST2_1  : constant Register_Address := 16#60#;
   ST2_2  : constant Register_Address := 16#61#;
   ST2_3  : constant Register_Address := 16#62#;
   ST2_4  : constant Register_Address := 16#63#;
   ST2_5  : constant Register_Address := 16#64#;
   ST2_6  : constant Register_Address := 16#65#;
   ST2_7  : constant Register_Address := 16#66#;
   ST2_8  : constant Register_Address := 16#67#;
   ST2_9  : constant Register_Address := 16#68#;
   ST2_10 : constant Register_Address := 16#69#;
   ST2_11 : constant Register_Address := 16#6A#;
   ST2_12 : constant Register_Address := 16#6B#;
   ST2_13 : constant Register_Address := 16#6C#;
   ST2_14 : constant Register_Address := 16#6D#;
   ST2_15 : constant Register_Address := 16#6E#;
   ST2_16 : constant Register_Address := 16#6F#;

   ----------------------------------------------------------------------------
   --  TIM4_2 : SM2 General Timer 4 Register
   --  Write only register
   --  Default value: 0x00
   --  7:0 TM_7-TM_0: SM2 Timer 4 Counter 1 Value
   ----------------------------------------------------------------------------
   TIM4_2 : constant Register_Address := 16#70#;

   ----------------------------------------------------------------------------
   --  TIM3_2 : SM2 General Timer 3 Register
   --  Write only register
   --  Default value: 0x00
   --  7:0 TM_7-TM_0: SM2 Timer 3 Counter 2 Value
   ----------------------------------------------------------------------------
   TIM3_2 : constant Register_Address := 16#71#;

   ----------------------------------------------------------------------------
   --  TIM2_2_L : SM2 General Timer 2 Low Register
   --  Write only register
   --  Default value: 0x00
   --  7:0 TM_7-TM_0: SM2 Timer 2 Counter 2 Low Value
   ----------------------------------------------------------------------------
   TIM2_2_L : constant Register_Address := 16#72#;

   ----------------------------------------------------------------------------
   --  TIM2_2_H : SM2 General Timer 2 High Register
   --  Write only register
   --  Default value: 0x00
   --  15:8 TM_15-TM_8: SM2 Timer 2 Counter 2 High Value
   ----------------------------------------------------------------------------
   TIM2_2_H : constant Register_Address := 16#73#;

   ----------------------------------------------------------------------------
   --  TIM1_2_L : SM2 General Timer 1 Low Register
   --  Write only register
   --  Default value: 0x00
   --  7:0 TM_7-TM_0: SM2 Timer 1 Counter 2 Low Value
   ----------------------------------------------------------------------------
   TIM1_2_L : constant Register_Address := 16#74#;

   ----------------------------------------------------------------------------
   --  TIM1_2_H : SM2 General Timer 1 High Register
   --  Write only register
   --  Default value: 0x00
   --  15:8 TM_15-TM_8: SM2 Timer 1 Counter 2 High Value
   ----------------------------------------------------------------------------
   TIM1_2_H : constant Register_Address := 16#75#;

   ----------------------------------------------------------------------------
   --  THRS2_2 : SM2 Threshold Value 1 Register
   --  Write only register
   --  Default value: 0x00
   --  7:0 THS7-THS0: SM2 Threshold Value
   ----------------------------------------------------------------------------
   THRS2_2 : constant Register_Address := 16#76#;

   ----------------------------------------------------------------------------
   --  THRS1_2 : SM2 Threshold Value 2 Register
   --  Write only register
   --  Default value: 0x00
   --  7:0 THS7-THS0: SM2 Threshold Value
   ----------------------------------------------------------------------------
   THRS1_2 : constant Register_Address := 16#77#;

   ----------------------------------------------------------------------------
   --  DES2 : SM2 Decimation Counter Value Register
   --  Write only register
   --  Default value: 0x00
   --  7:0 D7-D0: SM2 Decimation Counter Value
   ----------------------------------------------------------------------------
   DES2 : constant Register_Address := 16#78#;

   ----------------------------------------------------------------------------
   --  MASK2_B : SM2 Axis and Sign Mask Register
   --  Write only register
   --  Default value: 0x00
   --  7 P_X: X-Axis Positive Motion Detection
   --         0: X+ disabled
   --         1: X+ enabled
   --  6 N_X: X-Axis Negative Motion Detection
   --         0: X- disabled
   --         1: X- enabled
   --  5 P_Y: Y-Axis Positive Motion Detection
   --         0: Y+ disabled
   --         1: Y+ enabled
   --  4 N_Y: Y-Axis Negative Motion Detection
   --         0: Y- disabled
   --         1: Y- enabled
   --  3 P_Z: X-Axis Positive Motion Detection
   --         0: Z+ disabled
   --         1: Z+ enabled
   --  2 N_Z: X-Axis Negative Motion Detection
   --         0: Z- disabled
   --         1: Z- enabled
   --  1 P_V:
   --         0: V+ disabled
   --         1: V+ enabled
   --  0 N_V:
   --         0: V- disabled
   --         1: V- enabled
   ----------------------------------------------------------------------------
   MASK2_B : constant Register_Address := 16#79#;

   ----------------------------------------------------------------------------
   --  MASK2_A : SM2 Axis and Sign Mask Register
   --  Write only register
   --  Default value: 0x00
   --  7 P_X: X-Axis Positive Motion Detection
   --         0: X+ disabled
   --         1: X+ enabled
   --  6 N_X: X-Axis Negative Motion Detection
   --         0: X- disabled
   --         1: X- enabled
   --  5 P_Y: Y-Axis Positive Motion Detection
   --         0: Y+ disabled
   --         1: Y+ enabled
   --  4 N_Y: Y-Axis Negative Motion Detection
   --         0: Y- disabled
   --         1: Y- enabled
   --  3 P_Z: X-Axis Positive Motion Detection
   --         0: Z+ disabled
   --         1: Z+ enabled
   --  2 N_Z: X-Axis Negative Motion Detection
   --         0: Z- disabled
   --         1: Z- enabled
   --  1 P_V:
   --         0: V+ disabled
   --         1: V+ enabled
   --  0 N_V:
   --         0: V- disabled
   --         1: V- enabled
   ----------------------------------------------------------------------------
   MASK2_A : constant Register_Address := 16#7A#;

   ----------------------------------------------------------------------------
   --  SETT2 : SM2 Detection Settings Register
   --  Write only register
   --  Default value: 0x00
   --  7 P_DET: SM2 peak detection
   --           0: peak detection disabled (Default)
   --           1: peak detection enabled
   --  6 THR3_SA:
   --             0: no action (Default)
   --             1: threshold 3 limit value for axis and sign mask reset
   --                (MASK2_B)
   --  5 ABS:
   --         0: unsigned thresholds (Default)
   --         1: signed thresholds
   --  4 RADI:
   --          0: raw data
   --          1: diff data for State Machine 2
   --  3 D_CS:
   --          0: DIFF2 enabled (difference between current data and previous
   --                            data)
   --          1: constant shift enabled (difference between current data and
   --                                     constant values)
   --  2 THR3_MA:
   --             0: no action (Default)
   --             1: threshold 3 enabled for axis and sign mask reset (MASK2_A)
   --  1 R_TAM: Next condition validation flag
   --           0: mask frozen on the axis that triggers the condition
   --              (Default)
   --           1: standard mask always evaluated
   --  0 SITR:
   --          0: no actions (Default)
   --          1: STOP and CONT commands generate an interrupt and perform
   --             output actions as OUTC command.
   ----------------------------------------------------------------------------
   SETT2 : constant Register_Address := 16#7B#;

   ----------------------------------------------------------------------------
   --  PR2 : SM2 Program and Reset Pointers Register
   --  Read only register
   --  Default value: 0x00
   --  7:4 PP3-PP0: SM1 program pointer address
   --  3:0 RP3-RP0: SM1 reset pointer address
   ----------------------------------------------------------------------------
   PR2 : constant Register_Address := 16#7C#;

   ----------------------------------------------------------------------------
   --  TC2_L : SM2 General Timer Counter Low Register
   --  Read only register
   --  Default value: 0x00
   --  7:0 TC2_7-TC2_0: SM2 General Timer Counter Low Value
   ----------------------------------------------------------------------------
   TC2_L : constant Register_Address := 16#7D#;

   ----------------------------------------------------------------------------
   --  TC2_H : SM2 General Timer Counter High Register
   --  Read only register
   --  Default value: 0x00
   --  15:8 TC2_15-TC2_8: SM2 General Timer Counter High Value
   ----------------------------------------------------------------------------
   TC2_H : constant Register_Address := 16#7E#;

   ----------------------------------------------------------------------------
   --  OUTS2 : SM2 Output Set Flag Register
   --  Read only register
   --  Default value: 0x00
   --  7 P_X:
   --         0: X+ noshow
   --         1: X+ show
   --  6 N_X:
   --         0: X- noshow
   --         1: X- show
   --  5 P_Y:
   --         0: Y+ noshow
   --         1: Y+ show
   --  4 N_Y:
   --         0: Y- noshow
   --         1: Y- show
   --  3 P_Z:
   --         0: Z+ noshow
   --         1: Z+ show
   --  2 N_Z:
   --         0: Z- noshow
   --         1: Z- show
   --  1 P_V:
   --         0: V+ noshow
   --         1: V+ show
   --  0 N_V:
   --         0: V- noshow
   --         1: V- show
   ----------------------------------------------------------------------------
   OUTS2 : constant Register_Address := 16#7F#;

   --  During SPI communication, the most significant bit of the register
   --  address specifies if the operation is read or write.
   SPI_Read_Flag  : constant Register_Address := 16#80#;
   SPI_Write_Flag : constant Register_Address := 16#00#;
end LIS3DSH;
