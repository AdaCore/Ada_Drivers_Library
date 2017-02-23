------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--   @file    l3gd20.h                                                      --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   This file contains all the functions prototypes for the       --
--            l3gd20.c driver                                               --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides a driver for the L3GD20 gyroscope from ST
--  Microelectronics.

--  See datasheet "L3GD20 MEMS motion sensor: three-axis digital output
--  gyroscope" DocID022116 Rev 2, dated February 2013, file number DM00036465

with Interfaces; use Interfaces;
with HAL;        use HAL;
with HAL.SPI;    use HAL.SPI;
with HAL.GPIO;   use HAL.GPIO;

package L3GD20 is
   pragma Elaborate_Body;

   type Three_Axis_Gyroscope is tagged limited private;

   procedure Initialize
     (This        : in out Three_Axis_Gyroscope;
      Port        : Any_SPI_Port;
      Chip_Select : Any_GPIO_Point);
   --  Does device-specific initialization. Must be called prior to use.
   --
   --  NB: does NOT init/configure SPI and GPIO IO, which must be done
   --  (elsewhere) prior to calling this routine.

   type Power_Mode_Selection is
     (L3GD20_Mode_Powerdown,
      L3GD20_Mode_Active)
     with Size => 8;

   for Power_Mode_Selection use
     (L3GD20_Mode_Powerdown => 16#00#,
      L3GD20_Mode_Active    => 16#08#);

   type Output_Data_Rate_Selection is
     (L3GD20_Output_Data_Rate_95Hz,
      L3GD20_Output_Data_Rate_190Hz,
      L3GD20_Output_Data_Rate_380Hz,
      L3GD20_Output_Data_Rate_760Hz)
     with Size => 8;

   for Output_Data_Rate_Selection use
     (L3GD20_Output_Data_Rate_95Hz  => 16#00#,
      L3GD20_Output_Data_Rate_190Hz => 16#40#,
      L3GD20_Output_Data_Rate_380Hz => 16#80#,
      L3GD20_Output_Data_Rate_760Hz => 16#C0#);

   --  See App Note 4505, pg 8, Table 3.
   function Data_Rate_Hertz (Selection : Output_Data_Rate_Selection) return Float is
     (case Selection is
         when L3GD20_Output_Data_Rate_95Hz  =>  95.0,
         when L3GD20_Output_Data_Rate_190Hz => 190.0,
         when L3GD20_Output_Data_Rate_380Hz => 380.0,
         when L3GD20_Output_Data_Rate_760Hz => 760.0);

   type Axes_Selection is
     (L3GD20_Axes_Disable,
      L3GD20_Y_Enable,
      L3GD20_X_Enable,
      L3GD20_Z_Enable,
      L3GD20_Axes_Enable)
     with Size => 8;

   for Axes_Selection use
     (L3GD20_X_Enable     => 16#02#,
      L3GD20_Y_Enable     => 16#01#,
      L3GD20_Z_Enable     => 16#04#,
      L3GD20_Axes_Enable  => 16#07#,
      L3GD20_Axes_Disable => 16#00#);

   --  See App Note 4505, pg 8, Table 3.
   type Bandwidth_Selection is
     (L3GD20_Bandwidth_1,
      L3GD20_Bandwidth_2,
      L3GD20_Bandwidth_3,
      L3GD20_Bandwidth_4)
     with Size => 8;

   for Bandwidth_Selection use
     (L3GD20_Bandwidth_1 => 16#00#,
      L3GD20_Bandwidth_2 => 16#10#,
      L3GD20_Bandwidth_3 => 16#20#,
      L3GD20_Bandwidth_4 => 16#30#);

   type Full_Scale_Selection is
     (L3GD20_Fullscale_250,
      L3GD20_Fullscale_500,
      L3GD20_Fullscale_2000)
     with Size => 8;

   for Full_Scale_Selection use
     (L3GD20_Fullscale_250  => 16#00#,
      L3GD20_Fullscale_500  => 16#10#,
      L3GD20_Fullscale_2000 => 16#20#);

   type Block_Data_Update_Selection is
     (L3GD20_BlockDataUpdate_Continous,
      L3GD20_BlockDataUpdate_Single)
     with Size => 8;

   for Block_Data_Update_Selection use
     (L3GD20_BlockDataUpdate_Continous => 16#00#,
      L3GD20_BlockDataUpdate_Single    => 16#80#);

   type Endian_Data_Selection is
     (L3GD20_Little_Endian,
      L3GD20_Big_Endian)
     with Size => 8;

   for Endian_Data_Selection use
     (L3GD20_Little_Endian => 16#00#,
      L3GD20_Big_Endian    => 16#40#);

   --  Set up the gyro behavior.  Must be called prior to using the gyro.
   procedure Configure
     (This             : in out Three_Axis_Gyroscope;
      Power_Mode       : Power_Mode_Selection;
      Output_Data_Rate : Output_Data_Rate_Selection;
      Axes_Enable      : Axes_Selection;
      Bandwidth        : Bandwidth_Selection;
      BlockData_Update : Block_Data_Update_Selection;
      Endianness       : Endian_Data_Selection;
      Full_Scale       : Full_Scale_Selection);

   procedure Enable_Low_Pass_Filter (This : in out Three_Axis_Gyroscope);
   --  See App Note 4505, pg 15, Figure 5 and Table 9 for the concept. Enables
   --  the low pass filter LPF2 in the diagrams, for output to the data
   --  registers and the FIFO. The value of LPF2 is determined by the output
   --  data rate and bandwidth selections specified when calling procedure
   --  Configure. These values are shown in the App Note, pg 16, Table 11.
   --  The filter allows frequencies lower than the LPF2 "cutoff" value, and
   --  attenuates those at or above the cutoff. Note that the high pass filter
   --  is independent of the low pass filter.

   procedure Disable_Low_Pass_Filter (This : in out Three_Axis_Gyroscope);

   function Full_Scale_Sensitivity (This : Three_Axis_Gyroscope) return Float;
   --  Returns the sensitivity per LSB based on the Full_Scale_Selection
   --  specified to procedure Configure. Used to scale raw angle rates. Note
   --  that the values are already in terms of millidegrees per second per
   --  digit, so to use these values you should multiply, rather than divide.
   --  NB: These values are not measured on the specific device on the board
   --  in use. Instead, they are statistical averages determined at the factory
   --  for this component family, so you will likely need to tweak them a bit.

   --  High Pass Filtering functionality

   --  See App Note 4505, pg 17, Table 14.
   type High_Pass_Filter_Mode is
     (L3GD20_HPM_Normal_Mode_Reset,
      --  filter is reset by reading the Reference register

      L3GD20_HPM_Reference_Signal,
      --  output data is input - Reference register

      L3GD20_HPM_Normal_Mode,
      --  filter is reset by reading the Reference register

      L3GD20_HPM_Autoreset_Int)
      --  filter is auto-reset on configured interrupt
     with Size => 8;

   for High_Pass_Filter_Mode use
     (L3GD20_HPM_Normal_Mode_Reset => 16#00#,
      L3GD20_HPM_Reference_Signal  => 16#10#,
      L3GD20_HPM_Normal_Mode       => 16#20#,
      L3GD20_HPM_Autoreset_Int     => 16#30#);

   --  See App Note 4505, pg 17, Table 13 for the resulting frequencies. As
   --  shown in that table, the frequencies selected by these enumerals are
   --  a function of the configured output data rate selected elsewhere.
   type High_Pass_Cutoff_Frequency is
     (L3GD20_HPFCF_0,
      L3GD20_HPFCF_1,
      L3GD20_HPFCF_2,
      L3GD20_HPFCF_3,
      L3GD20_HPFCF_4,
      L3GD20_HPFCF_5,
      L3GD20_HPFCF_6,
      L3GD20_HPFCF_7,
      L3GD20_HPFCF_8,
      L3GD20_HPFCF_9)
     with Size => 8;

   for High_Pass_Cutoff_Frequency use  -- confirming
     (L3GD20_HPFCF_0 => 16#00#,
      L3GD20_HPFCF_1 => 16#01#,
      L3GD20_HPFCF_2 => 16#02#,
      L3GD20_HPFCF_3 => 16#03#,
      L3GD20_HPFCF_4 => 16#04#,
      L3GD20_HPFCF_5 => 16#05#,
      L3GD20_HPFCF_6 => 16#06#,
      L3GD20_HPFCF_7 => 16#07#,
      L3GD20_HPFCF_8 => 16#08#,
      L3GD20_HPFCF_9 => 16#09#);

   procedure Configure_High_Pass_Filter
     (This             : in out Three_Axis_Gyroscope;
      Mode_Selection   : High_Pass_Filter_Mode;
      Cutoff_Frequency : High_Pass_Cutoff_Frequency);

   procedure Enable_High_Pass_Filter (This : in out Three_Axis_Gyroscope);
   --  Allows frequencies higher than the cutoff specified to procedure
   --  Configure_High_Pass_Filter, and attenuates those at or below it.

   procedure Disable_High_Pass_Filter (This : in out Three_Axis_Gyroscope);

   function Reference_Value
     (This : Three_Axis_Gyroscope)
      return UInt8
     with Inline;

   procedure Set_Reference
     (This : in out Three_Axis_Gyroscope; Value : UInt8)
     with Inline;

   --  Basic data access functionality

   type Gyro_Data_Status is record
      ZYX_Overrun   : Boolean;
      Z_Overrun     : Boolean;
      Y_Overrun     : Boolean;
      X_Overrun     : Boolean;
      ZYX_Available : Boolean;
      Z_Available   : Boolean;
      Y_Available   : Boolean;
      X_Available   : Boolean;
   end record
     with Size => 8;

   for Gyro_Data_Status use record
      ZYX_Overrun   at 0 range 7 .. 7;
      Z_Overrun     at 0 range 6 .. 6;
      Y_Overrun     at 0 range 5 .. 5;
      X_Overrun     at 0 range 4 .. 4;
      ZYX_Available at 0 range 3 .. 3;
      Z_Available   at 0 range 2 .. 2;
      Y_Available   at 0 range 1 .. 1;
      X_Available   at 0 range 0 .. 0;
   end record;

   function Data_Status (This : Three_Axis_Gyroscope) return Gyro_Data_Status
     with Inline;

   type Angle_Rate is new Integer_16;

   type Angle_Rates is record
      X : Angle_Rate;  -- pitch, per Figure 2, pg 7 of the Datasheet
      Y : Angle_Rate;  -- roll
      Z : Angle_Rate;  -- yaw
   end record with Size => 3 * 16;

   for Angle_Rates use record
      X at 0 range 0 .. 15;
      Y at 2 range 0 .. 15;
      Z at 4 range 0 .. 15;
   end record;
   --  confirming, but required, eg for matching FIFO content format

   procedure Get_Raw_Angle_Rates
     (This  : Three_Axis_Gyroscope;
      Rates : out Angle_Rates);
   --  Returns the latest data, swapping UInt8s if required by the endianess
   --  selected by a previous call to Configure.
   --  NB: does NOT check whether the gyro status indicates data are ready.
   --  NB: does NOT apply any sensitity scaling.

   --  FIFO functionality

   type FIFO_Modes is
     (L3GD20_Bypass_Mode,
      L3GD20_FIFO_Mode,
      L3GD20_Stream_Mode,
      L3GD20_Stream_To_FIFO_Mode,
      L3GD20_Bypass_To_Stream_Mode)
     with Size => 8;

   for FIFO_Modes use  -- confirming
     (L3GD20_Bypass_Mode           => 2#0000_0000#,
      L3GD20_FIFO_Mode             => 2#0010_0000#,
      L3GD20_Stream_Mode           => 2#0100_0000#,
      L3GD20_Stream_To_FIFO_Mode   => 2#0110_0000#,
      L3GD20_Bypass_To_Stream_Mode => 2#1000_0000#);

   procedure Set_FIFO_Mode (This : in out Three_Axis_Gyroscope;
                            Mode : FIFO_Modes);

   procedure Enable_FIFO (This : in out Three_Axis_Gyroscope);

   procedure Disable_FIFO (This : in out Three_Axis_Gyroscope);

   subtype FIFO_Level is Integer range 0 .. 31;

   procedure Set_FIFO_Watermark (This : in out Three_Axis_Gyroscope;
                                 Level : FIFO_Level);

   type Angle_Rates_FIFO_Buffer is array (FIFO_Level range <>) of Angle_Rates
     with Alignment => Angle_Rate'Alignment;

   procedure Get_Raw_Angle_Rates_FIFO
     (This   : in out Three_Axis_Gyroscope;
      Buffer : out Angle_Rates_FIFO_Buffer);
   --  Returns the latest raw data in the FIFO, swapping UInt8s if required by
   --  the endianess selected by a previous call to Configure. Fills the entire
   --  buffer passed.
   --  NB: does NOT apply any sensitity scaling.

   function Current_FIFO_Depth (This : Three_Axis_Gyroscope) return FIFO_Level;
   --  The number of unread entries currently in the hardware FIFO

   function FIFO_Below_Watermark (This : Three_Axis_Gyroscope) return Boolean;

   function FIFO_Empty (This : Three_Axis_Gyroscope) return Boolean;

   function FIFO_Overrun (This : Three_Axis_Gyroscope) return Boolean;

   --  Interrupt 2 controls

   procedure Disable_Int2_Interrupts (This : in out Three_Axis_Gyroscope);

   procedure Enable_Data_Ready_Interrupt  (This : in out Three_Axis_Gyroscope);

   procedure Disable_Data_Ready_Interrupt (This : in out Three_Axis_Gyroscope);

   procedure Enable_FIFO_Watermark_Interrupt
     (This : in out Three_Axis_Gyroscope);

   procedure Disable_FIFO_Watermark_Interrupt
     (This : in out Three_Axis_Gyroscope);

   procedure Enable_FIFO_Overrun_Interrupt (This : in out Three_Axis_Gyroscope);

   procedure Disable_FIFO_Overrun_Interrupt (This : in out Three_Axis_Gyroscope);

   procedure Enable_FIFO_Empty_Interrupt  (This : in out Three_Axis_Gyroscope);

   procedure Disable_FIFO_Empty_Interrupt (This : in out Three_Axis_Gyroscope);

   --  Interrupt 1 facilities

   procedure Disable_Int1_Interrupts (This : in out Three_Axis_Gyroscope);

   type Sample_Counter is mod 2 ** 6;

   procedure Set_Duration_Counter
     (This  : in out Three_Axis_Gyroscope;
      Value : Sample_Counter);
   --  Loads the INT1_DURATION register with the specified Value and enables
   --  the "wait" option so that the interrupt is not generated unless any
   --  sample value is beyond a threshold for the corresponding amount of time.
   --  The effective time is dependent on the data rate selected elsewhere.

   type Axes_Interrupts is
     (Z_High_Interrupt,
      Z_Low_Interrupt,
      Y_High_Interrupt,
      Y_Low_Interrupt,
      X_High_Interrupt,
      X_Low_Interrupt);

   procedure Enable_Event
     (This  : in out Three_Axis_Gyroscope;
      Event : Axes_Interrupts);

   procedure Disable_Event
     (This  : in out Three_Axis_Gyroscope;
      Event : Axes_Interrupts);

   type Axis_Sample_Threshold is new UInt16 range 0 .. 2 ** 15;
   --  Threshold values do not use the high-order bit

   procedure Set_Threshold
     (This  : in out Three_Axis_Gyroscope;
      Event : Axes_Interrupts;
      Value : Axis_Sample_Threshold);
   --  Writes the two-UInt8 threshold value into the two threshold registers for
   --  the specified interrupt event.  Does not enable the event itself.

   type Threshold_Event is record
      Axis      : Axes_Interrupts;
      Threshold : Axis_Sample_Threshold;
   end record;

   type Threshold_Event_List is
     array (Positive range <>) of Threshold_Event;

   type Interrupt1_Active_Edge is
     (L3GD20_Interrupt1_High_Edge,
      L3GD20_Interrupt1_Low_Edge);

   for Interrupt1_Active_Edge use
     (L3GD20_Interrupt1_Low_Edge  => 16#20#,
      L3GD20_Interrupt1_High_Edge => 16#00#);

   procedure Configure_Interrupt1
     (This           : in out Three_Axis_Gyroscope;
      Triggers       : Threshold_Event_List;
      Latched        : Boolean := False;
      Active_Edge    : Interrupt1_Active_Edge := L3GD20_Interrupt1_High_Edge;
      Combine_Events : Boolean := False;
      Sample_Count   : Sample_Counter := 0);
   --
   --  If Latched is False (the hardware default value), the interrupt signal
   --  goes high when the interrupt condition is satisfied and returns low
   --  immediately if the interrupt condition is no longer verified. Otherwise,
   --  if Latched is True, the interrupt signal remains high even if the
   --  condition returns to a non-interrupt status, until a read of the
   --  INT1_SRC register is performed.
   --
   --  If Combine_Events is True, all events' criteria must be
   --  safisfied for the interrupt to be generated; otherwise any one is
   --  sufficient.
   --
   --  If Sample_Count is not zero, passes Sample_Count to a call to
   --  Set_Duration_Counter
   --
   --  For every event and threshold specified in Triggers, sets the threshold
   --  value and enables the corresponding high or low threshold interrupt for
   --  that axis. For example, the following call enables the interrupt when
   --  the sampled value for the X axis is greater than 10 or the sample for
   --  the Z axis is greater than 20:
   --
   --      Configure_Interrupt_1
   --        (Latched     => ... ,
   --         Active_Edge => ... ,
   --         Triggers    => ((X_High_Interrupt, Threshold => 10),
   --                         (Z_High_Interrupt, Threshold => 20)));
   --
   --  For that example call, if Combine_Events is True, the interrupt is only
   --  generated if both thresholds are exceeded. Otherwise either one is
   --  sufficient for the interrupt to be generated.

   procedure Enable_Interrupt1 (This : in out Three_Axis_Gyroscope);

   procedure Disable_Interrupt1 (This : in out Three_Axis_Gyroscope);

   type Pin_Modes is (Push_Pull, Open_Drain);

   for Pin_Modes use (Push_Pull => 0,  Open_Drain => 2#0001_0000#);

   procedure Set_Interrupt_Pin_Mode
     (This : in out Three_Axis_Gyroscope;
      Mode : Pin_Modes);

   type Interrupt1_Sources is record
      Interrupts_Active : Boolean;
      Z_High_Interrupt  : Boolean;
      Z_Low_Interrupt   : Boolean;
      Y_High_Interrupt  : Boolean;
      Y_Low_Interrupt   : Boolean;
      X_High_Interrupt  : Boolean;
      X_Low_Interrupt   : Boolean;
   end record
   with Size => 8;

   for Interrupt1_Sources use record
      Interrupts_Active at 0 range 6 .. 6;
      Z_High_Interrupt  at 0 range 5 .. 5;
      Z_Low_Interrupt   at 0 range 4 .. 4;
      Y_High_Interrupt  at 0 range 3 .. 3;
      Y_Low_Interrupt   at 0 range 2 .. 2;
      X_High_Interrupt  at 0 range 1 .. 1;
      X_Low_Interrupt   at 0 range 0 .. 0;
   end record;

   function Interrupt1_Source (This : Three_Axis_Gyroscope) return Interrupt1_Sources;

   --  Miscellaneous functionality

   procedure Sleep (This : in out Three_Axis_Gyroscope);
   --  See App Note 4505, pg 9

   procedure Reboot (This : Three_Axis_Gyroscope);

   procedure Reset (This : in out Three_Axis_Gyroscope);
   --  writes default values to all writable registers

   I_Am_L3GD20 : constant := 16#D4#;

   function Device_Id (This : Three_Axis_Gyroscope) return UInt8;
   --  Will return I_Am_L3GD20 when functioning properly

   function Temperature (This : Three_Axis_Gyroscope) return UInt8;
   --  the temperature of the chip itself

private

   type Three_Axis_Gyroscope is tagged limited record
      Port : Any_SPI_Port;
      CS   : Any_GPIO_Point;
   end record;

   type Register is new UInt8;

   procedure Write
     (This : Three_Axis_Gyroscope;
      Addr : Register;
      Data : UInt8)
     with Inline;
   --  Writes Data to the specified register within the gyro chip

   procedure Read
     (This : Three_Axis_Gyroscope;
      Addr : Register;
      Data : out UInt8)
     with Inline;
   --  Reads Data from the specified register within the gyro chip

   procedure Read_UInt8s
     (This   : Three_Axis_Gyroscope;
      Addr   : Register;
      Buffer : out SPI_Data_8b;
      Count  : Natural);
   --  Reads multiple Data from the specified register within the gyro chip

   --  L3GD20 Registers

   Who_Am_I      : constant Register := 16#0F#; --  device identification
   CTRL_REG1     : constant Register := 16#20#; --  Control register 1
   CTRL_REG2     : constant Register := 16#21#; --  Control register 2
   CTRL_REG3     : constant Register := 16#22#; --  Control register 3
   CTRL_REG4     : constant Register := 16#23#; --  Control register 4
   CTRL_REG5     : constant Register := 16#24#; --  Control register 5
   Reference     : constant Register := 16#25#; --  Reference
   OUT_Temp      : constant Register := 16#26#; --  Temperature
   Status        : constant Register := 16#27#; --  Status
   OUT_X_L       : constant Register := 16#28#; --  Output X low UInt8
   OUT_X_H       : constant Register := 16#29#; --  Output X high UInt8
   OUT_Y_L       : constant Register := 16#2A#; --  Output Y low UInt8
   OUT_Y_H       : constant Register := 16#2B#; --  Output Y high UInt8
   OUT_Z_L       : constant Register := 16#2C#; --  Output Z low UInt8
   OUT_Z_H       : constant Register := 16#2D#; --  Output Z high UInt8
   FIFO_CTRL     : constant Register := 16#2E#; --  FIFO control
   FIFO_SRC      : constant Register := 16#2F#; --  FIFO src
   INT1_CFG      : constant Register := 16#30#; --  Interrupt 1 configuration
   INT1_SRC      : constant Register := 16#31#; --  Interrupt 1 source
   INT1_TSH_XH   : constant Register := 16#32#; --  Interrupt 1 Threshold X
   INT1_TSH_XL   : constant Register := 16#33#; --  Interrupt 1 Threshold X
   INT1_TSH_YH   : constant Register := 16#34#; --  Interrupt 1 Threshold Y
   INT1_TSH_YL   : constant Register := 16#35#; --  Interrupt 1 Threshold Y
   INT1_TSH_ZH   : constant Register := 16#36#; --  Interrupt 1 Threshold Z
   INT1_TSH_ZL   : constant Register := 16#37#; --  Interrupt 1 Threshold Z
   INT1_Duration : constant Register := 16#38#; --  Interrupt 1 Duration

end L3GD20;
