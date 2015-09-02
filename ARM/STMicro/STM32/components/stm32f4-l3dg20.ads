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
--   @brief   Header file of DMA HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides a driver for the L3DG20 gyroscope from ST
--  Microelectronics.

--  See datasheet "L3DG20 MEMS motion sensor: three-axis digital output
--  gyroscope" DocID022116 Rev 2, dated February 2013, file number DM00036465

with STM32F4.SPI;  use STM32F4.SPI;
with STM32F4.GPIO; use STM32F4.GPIO;
with STM32F4.RCC;

with Interfaces;   use Interfaces;

with Ada.Interrupts.Names; use Ada.Interrupts.Names;
use Ada.Interrupts;

package STM32F4.L3DG20 is

   type Power_Mode_Selection is
     (L3GD20_Mode_Powerdown,
      L3GD20_Mode_Active)
     with Size => 8;

   for Power_Mode_Selection use
     (L3GD20_Mode_Powerdown => 16#00#,
      L3GD20_Mode_Active    => 16#08#);

   type Output_DataRate_Selection is
     (L3GD20_Output_DataRate_1,
      L3GD20_Output_DataRate_2,
      L3GD20_Output_DataRate_3,
      L3GD20_Output_DataRate_4)
     with Size => 8;

   for Output_DataRate_Selection use
     (L3GD20_Output_DataRate_1 => 16#00#,
      L3GD20_Output_DataRate_2 => 16#40#,
      L3GD20_Output_DataRate_3 => 16#80#,
      L3GD20_Output_DataRate_4 => 16#C0#);

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

   type BandWidth_Selection is
     (L3GD20_Bandwidth_1,
      L3GD20_Bandwidth_2,
      L3GD20_Bandwidth_3,
      L3GD20_Bandwidth_4)
     with Size => 8;

   for BandWidth_Selection use
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
     (L3GD20_BLE_LSB,
      L3GD20_BLE_MSB)
     with Size => 8;

   for Endian_Data_Selection use
     (L3GD20_BLE_LSB => 16#00#,
      L3GD20_BLE_MSB => 16#40#);

   procedure Configure
     (Power_Mode       : Power_Mode_Selection;
      Output_DataRate  : Output_DataRate_Selection;
      Axes_Enable      : Axes_Selection;
      Band_Width       : BandWidth_Selection;
      BlockData_Update : Block_Data_Update_Selection;
      Endianness       : Endian_Data_Selection;
      Full_Scale       : Full_Scale_Selection);

   type High_Pass_Filter_Mode is
     (L3GD20_HPM_Normal_Mode_Res,
      L3GD20_HPM_Ref_Signal,
      L3GD20_HPM_Normal_Mode,
      L3GD20_HPM_Autoreset_Int)
     with Size => 8;

   for High_Pass_Filter_Mode use
     (L3GD20_HPM_Normal_Mode_Res => 16#00#,
      L3GD20_HPM_Ref_Signal      => 16#10#,
      L3GD20_HPM_Normal_Mode     => 16#20#,
      L3GD20_HPM_Autoreset_Int   => 16#30#);

   type High_Pass_Cut_Off_Frequency is
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

   for High_Pass_Cut_Off_Frequency use
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

   procedure Configure_Filter
     (Mode_Selection   : High_Pass_Filter_Mode;
      CutOff_Frequency : High_Pass_Cut_Off_Frequency);

   procedure Enable_Filter;

   procedure Disable_Filter;

   type Sample_Counter is mod 2 ** 6;

   procedure Set_Duration_Counter (Value : Sample_Counter);
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

   procedure Enable_Event (Event : Axes_Interrupts);

   procedure Disable_Event (Event : Axes_Interrupts);

   type Axis_Sample_Threshold is new Half_Word range 0 .. 2 ** 15;
   --  Threshold values do not use the high-order bit

   procedure Set_Threshold
     (Event : Axes_Interrupts;
      Value : Axis_Sample_Threshold);
   --  Writes the two-byte threshold value into the two threshold registers for
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
     (Triggers       : Threshold_Event_List;
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

   procedure Enable_Interrupt1;

   procedure Disable_Interrupt1;

   type Pin_Modes is (Push_Pull, Open_Drain);

   for Pin_Modes use (Push_Pull => 0,  Open_Drain => 2#0001_0000#);

   procedure Set_Interrupt_Pin_Mode (Mode : Pin_Modes);

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

   function Interrupt1_Source return Interrupt1_Sources;

   I_Am_L3GD20 : constant := 16#D4#;

   function Device_Id return Byte;
   --  Will return I_Am_L3GD20 when functioning properly

   function Data_Status return Byte;

   type Raw_Angle_Rates is record
      X : Integer_16;  -- roll
      Y : Integer_16;  -- pitch
      Z : Integer_16;  -- yaw
   end record;

   procedure Get_Raw_Angle_Rates (Rates : out Raw_Angle_Rates);
   --  Poll for the latest data. Raises Program_Error if the status indicates
   --  no data available after a certain number of attempts. Does NOT apply the
   --  specified sensitity scaling as determined by the Full_Scale_Selection
   --  passed to procedure Configure.

   function Selected_Sensitivity return Float;
   --  Returns the value resulting from the Full_Scale_Selection specified to
   --  procedure Configure. Can be used to manually scale unscaled results,
   --  which is done automatically in procedure Get_Angle_Rates but not by
   --  Get_Unscaled_Angle_Rates

   procedure Reboot;

   type FIFO_Modes is
     (L3GD20_Bypass_Mode,
      L3GD20_FIFO_Mode,
      L3GD20_Stream_Mode,
      L3GD20_Bypass_To_Stream_Mode,
      L3GD20_Stream_To_FIFO_Mode)
     with Size => 8;

   for FIFO_Modes use  -- confirming
     (L3GD20_Bypass_Mode           => 2#000#,
      L3GD20_FIFO_Mode             => 2#001#,
      L3GD20_Stream_Mode           => 2#010#,
      L3GD20_Bypass_To_Stream_Mode => 2#011#,
      L3GD20_Stream_To_FIFO_Mode   => 2#100#);

   procedure Enable_FIFO (Mode : FIFO_Modes);
   --  Enables the FIFO and sets the specified mode

   procedure Disable_FIFO;

   FIFO_Depth : constant := 32;
   --  the number of 16-bit quantities that the hardware FIFO can contain

   type FIFO_Level is new Byte range 0 .. FIFO_Depth - 1;

   procedure Set_Watermark (Level : FIFO_Level);

   function Current_FIFO_Depth return FIFO_Level;
   --  The number of entries currently in the hardware FIFO

   function FIFO_Below_Watermark return Boolean;

   function FIFO_Empty return Boolean;

   function FIFO_Overrun return Boolean;

   --  Interrupt 2 controls

   procedure Enable_Data_Ready_Interrupt;
   procedure Disable_Data_Ready_Interrupt;

   procedure Enable_Watermark_Interrupt;
   procedure Disable_Watermark_Interrupt;

   procedure Enable_Overrun_Interrupt;
   procedure Disable_Overrun_Interrupt;

   procedure Enable_FIFO_Empty_Interrupt;
   procedure Disable_FIFO_Empty_Interrupt;

   --  The following are the interrupt definitions necessary for client-defined
   --  interrupt handlers:

   Int1_Pin : constant GPIO_Pin := Pin_1;
   Int2_Pin : constant GPIO_Pin := Pin_2;

   Int1_Interrupt : constant Interrupt_ID := EXTI1_Interrupt;
   --  NB: the line number 'n' in EXTIn_Interrupt must match the GPIO pin
   --  number associated with the interrupt, so pin 1 uses EXTI1 etc.

private

   type Register is new Byte;

   procedure Write (Addr : Register;  Data : Byte);
   --  Writes Data to the specified register within the gyro chip

   procedure Read (Addr : Register; Data : out Byte);
   --  Reads Data from the specified register within the gyro chip

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
   OUT_X_L       : constant Register := 16#28#; --  Output X low byte
   OUT_X_H       : constant Register := 16#29#; --  Output X high byte
   OUT_Y_L       : constant Register := 16#2A#; --  Output Y low byte
   OUT_Y_H       : constant Register := 16#2B#; --  Output Y high byte
   OUT_Z_L       : constant Register := 16#2C#; --  Output Z low byte
   OUT_Z_H       : constant Register := 16#2D#; --  Output Z high byte
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

   --  SPI and GPIO definitions. We use the addresses defined by the STM32F4
   --  root package to declare the GPIO and SPI devices rather than simply
   --  referencing those declared in the board-specific *_Discovery packages
   --  to get the device declarations because this is a component-level driver
   --  that could conceivably be used on any given board, not just a Discovery
   --  or evaluation board. For the same reason, we reference the clock enabler
   --  procedures defined in the RCC package rathet than more convenient
   --  routines defined in the *_Discovery packages. Of course, use within an
   --  application requires the developer to be cognizant of the devices and
   --  pins in use here, so we put them in the package declaration (albeit in
   --  the private part).

   L3GD20_SPI : SPI_Port with Address => SPI5_Base;

   SPI_GPIO_AF : constant GPIO_Alternate_Function := GPIO_AF_SPI5;
   -- must match the selected SPI port

   procedure Enable_SPI_Clock renames RCC.SPI5_Clock_Enable;
   --  must match selected SPI port

   SPI_GPIO : GPIO_Port with Address => GPIOF_Base;

   procedure Enable_SPI_GPIO_Clock renames RCC.GPIOF_Clock_Enable;
   --  must match selected SPI_GPIO port

   SCK_Pin  : constant GPIO_Pin := Pin_7;
   MISO_Pin : constant GPIO_Pin := Pin_8;
   MOSI_Pin : constant GPIO_Pin := Pin_9;

   CS_GPIO : GPIO_Port with Address => GPIOC_Base;
   CS_Pin  : constant GPIO_Pin := Pin_1;

   procedure Enable_Chip_Select_Clock renames RCC.GPIOC_Clock_Enable;
   --  must match selected CS_GPIO port

   Int_GPIO : GPIO_Port with Address => GPIOA_Base;

   procedure Enable_GPIO_Interrupt_Clock renames RCC.GPIOA_Clock_Enable;
   --  must match selected Int_GPIO port

end STM32F4.L3DG20;
