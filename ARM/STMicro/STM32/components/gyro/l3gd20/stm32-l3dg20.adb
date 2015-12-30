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
--   @file    l3gd20.c                                                      --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   This file provides a set of functions needed to manage the    --
--            L3GD20, ST MEMS motion sensor,  3-axis digital output         --
--            gyroscope.                                                    --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with STM32.EXTI;    use STM32.EXTI;
with STM32.SYSCFG;  use STM32.SYSCFG;

with STM32.Device;  use STM32.Device;

package body STM32.L3DG20 is

   --  the following are per Table 4 of the L3DG20 Datasheet, pg 9.
   --  the values are millidegrees per second, so we scale accordingly
   Sensitivity_250dps  : constant := 8.75 * 0.001; -- mdps/digit
   Sensitivity_500dps  : constant := 17.5 * 0.001; -- mdps/digit
   Sensitivity_2000dps : constant := 70.0 * 0.001; -- mdps/digit

   ReadWrite_CMD : constant := 16#80#;

   --  bit definitions for the CTRL_REG3 register

   INT1_Interrupt_Enable            : constant := 2#1000_0000#;
   Interrupt_Pin_Mode_Bit           : constant := 2#0001_0000#;
   INT2_Data_Ready_Interrupt_Enable : constant := 2#0000_1000#;
   INT2_Watermark_Interrupt_Enable  : constant := 2#0000_0100#;
   INT2_Overrun_Interrupt_Enable    : constant := 2#0000_0010#;
   INT2_FIFO_Empty_Interrupt_Enable : constant := 2#0000_0001#;

   --  bit definitions for the CTRL_REG4 register

   Endian_Selection_Mask    : constant := 2#0100_0000#;
   Fullscale_Selection_Bits : constant := 2#0011_0000#;

   --  bit definitions for the CTRL_REG5 register

   Boot_Bit               : constant := 2#1000_0000#;
   FIFO_Enable_Bit        : constant := 2#0100_0000#;
   HighPass_Filter_Enable : constant := 2#0001_0000#;

   --  bit definitions for the FIFO_CTRL register

   FIFO_Mode_Bits           : constant := 2#1110_0000#;
   Watermark_Threshold_Bits : constant := 2#0001_1111#;

   --  bit definitions for the FIFO_SRC register

   Watermark_Status_Bit : constant := 2#1000_0000#;
   FIFO_Overrun_Bit     : constant := 2#0100_0000#;
   FIFO_Empty_Bit       : constant := 2#0010_0000#;
   FIFO_Depth_Bits      : constant := 2#0001_1111#;

   --  bit definitions for the INT1_CFG register

   Logically_And_Or_Events_Bit : constant := 2#1000_0000#;
   Int1_Latch_Enable_Bit       : constant := 2#0100_0000#;

   Axes_Interrupt_Enablers : constant array (Axes_Interrupts) of Byte :=
     (Z_High_Interrupt => 2#0010_0000#,
      Z_Low_Interrupt  => 2#0001_0000#,
      Y_High_Interrupt => 2#0000_1000#,
      Y_Low_Interrupt  => 2#0000_0100#,
      X_High_Interrupt => 2#0000_0010#,
      X_Low_Interrupt  => 2#0000_0001#);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => High_Pass_Filter_Mode, Target => Byte);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => High_Pass_Cutoff_Frequency, Target => Byte);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => Power_Mode_Selection, Target => Byte);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => Output_Data_Rate_Selection, Target => Byte);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => Axes_Selection, Target => Byte);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => Bandwidth_Selection, Target => Byte);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => Block_Data_Update_Selection, Target => Byte);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => Endian_Data_Selection, Target => Byte);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => Full_Scale_Selection, Target => Byte);

   procedure Initialize_Device_IO (This : in out Three_Axis_Gyroscope);

   ------------------------------
   -- Initialize_Gyro_Hardware --
   ------------------------------

   procedure Initialize_Gyro_Hardware
     (This                        : out Three_Axis_Gyroscope;
      L3GD20_SPI                  : access SPI_Port;
      SPI_GPIO_AF                 : GPIO_Alternate_Function;
      SCK_Pin                     : GPIO_Point;
      MISO_Pin                    : GPIO_Point;
      MOSI_Pin                    : GPIO_Point;
      CS_Pin                      : GPIO_Point;
      Int1_Pin                    : GPIO_Point;
      Int2_Pin                    : GPIO_Point)
   is
   begin
      This.L3GD20_SPI  := L3GD20_SPI;
      This.SPI_GPIO_AF := SPI_GPIO_AF;
      This.SCK_Pin     := SCK_Pin;
      This.MISO_Pin    := MISO_Pin;
      This.MOSI_Pin    := MOSI_Pin;
      This.CS_Pin      := CS_Pin;
      This.Int1_Pin    := Int1_Pin;
      This.Int2_Pin    := Int2_Pin;
      Initialize_Device_IO (This);
   end Initialize_Gyro_Hardware;

   -----------------
   -- Chip_Select --
   -----------------

   procedure Chip_Select (This : Three_Axis_Gyroscope; Enabled : Boolean) is
      --  When the pin is low (cleared), the device is in SPI mode.
      --  When the pin is high (set), the device is in I2C mode.
      --  We want SPI mode communication, so Enabled, when True,
      --  means we must drive the pin low.
   begin
      if Enabled then
         GPIO.Clear (This.CS_Pin);
      else
         GPIO.Set (This.CS_Pin);
      end if;
   end Chip_Select;

   -----------
   -- Write --
   -----------

   procedure Write (This : Three_Axis_Gyroscope; Addr : Register;  Data : Byte) is
   begin
      Chip_Select (This, Enabled => True);
      SPI.Transmit (This.L3GD20_SPI.all, Byte (Addr));
      SPI.Transmit (This.L3GD20_SPI.all, Data);
      Chip_Select (This, Enabled => False);
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read (This : Three_Axis_Gyroscope; Addr : Register; Data : out Byte) is
   begin
      Chip_Select (This, Enabled => True);
      SPI.Transmit (This.L3GD20_SPI.all, Byte (Addr) or READWRITE_CMD);
      SPI.Receive (This.L3GD20_SPI.all, Data);
      Chip_Select (This, Enabled => False);
   end Read;

   ----------------------
   -- Init_Chip_Select --
   ----------------------

   procedure Init_Chip_Select (This : in out Three_Axis_Gyroscope) is
      GPIO_Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (This.CS_Pin);

      GPIO_Conf.Speed := Speed_25MHz;
      GPIO_Conf.Mode := Mode_OUT;
      GPIO_Conf.Output_Type := Push_Pull;
      GPIO_Conf.Resistors := Pull_Up;

      Configure_IO (This.CS_Pin, GPIO_Conf);

      Lock (This.CS_Pin);
   end Init_Chip_Select;

   ----------------------
   -- Init_SPI_IO_Pins --
   ----------------------

   procedure Init_SPI_IO_Pins (This : in out Three_Axis_Gyroscope) is
      GPIO_Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (This.SCK_Pin & This.MISO_Pin & This.MOSI_Pin);

      GPIO_Conf.Speed       := Speed_100MHz;
      GPIO_Conf.Mode        := Mode_AF;
      GPIO_Conf.Output_Type := Push_Pull;
      GPIO_Conf.Resistors   := Floating;

      Configure_IO (This.SCK_Pin & This.MISO_Pin & This.MOSI_Pin, GPIO_Conf);

      Configure_Alternate_Function
        (This.SCK_Pin & This.MISO_Pin & This.MOSI_Pin,
         This.SPI_GPIO_AF);

      Lock (This.SCK_Pin & This.MISO_Pin & This.MOSI_Pin);
   end Init_SPI_IO_Pins;

   --------------
   -- Init_SPI --
   --------------

   procedure Init_SPI (This : in out Three_Axis_Gyroscope) is
      SPI_Conf  : SPI_Configuration;
   begin
      Init_SPI_IO_Pins (This);

      SPI.Disable (This.L3GD20_SPI.all);

      SPI_Conf.Direction           := D2Lines_FullDuplex;
      SPI_Conf.Mode                := Master;
      SPI_Conf.Data_Size           := Data_8;
      SPI_Conf.Clock_Polarity      := Low;
      SPI_Conf.Clock_Phase         := P1Edge;
      SPI_Conf.Slave_Management    := Software_Managed;
      SPI_Conf.Baud_Rate_Prescaler := BRP_32;
      SPI_Conf.First_Bit           := MSB;
      SPI_Conf.CRC_Poly            := 7;

      SPI.Configure (This.L3GD20_SPI.all, SPI_Conf);

      SPI.Enable (This.L3GD20_SPI.all);
   end Init_SPI;

   --------------------------
   -- Initialize_Device_IO --
   --------------------------

   procedure Initialize_Device_IO (This : in out Three_Axis_Gyroscope) is
   begin
      Init_Chip_Select (This);
      Chip_Select (This, Enabled => False);
      Init_SPI (This);
   end Initialize_Device_IO;

   ------------------------------
   -- Configure_Interrupt_Pins --
   ------------------------------

   procedure Configure_Interrupt_Pins (This : in out Three_Axis_Gyroscope) is
      GPIO_Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (This.Int1_Pin & This.Int2_Pin);
      GPIO_Conf.Speed := Speed_50MHz;
      GPIO_Conf.Mode := Mode_In;
      GPIO_Conf.Output_Type := Push_Pull;
      GPIO_Conf.Resistors := Floating;

      Configure_IO (This.Int1_Pin & This.Int2_Pin, GPIO_Conf);
      Lock (This.Int1_Pin & This.Int2_Pin);
   end Configure_Interrupt_Pins;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This             : in out Three_Axis_Gyroscope;
      Power_Mode       : Power_Mode_Selection;
      Output_Data_Rate : Output_Data_Rate_Selection;
      Axes_Enable      : Axes_Selection;
      Bandwidth        : Bandwidth_Selection;
      BlockData_Update : Block_Data_Update_Selection;
      Endianness       : Endian_Data_Selection;
      Full_Scale       : Full_Scale_Selection)
   is
      Ctrl1 : Byte;
      Ctrl4 : Byte;
   begin
      Ctrl1 := As_Byte (Power_Mode)       or
               As_Byte (Output_Data_Rate) or
               As_Byte (Axes_Enable)      or
               As_Byte (Bandwidth);

      Ctrl4 := As_Byte (BlockData_Update) or
               As_Byte (Endianness)       or
               As_Byte (Full_Scale);

      Write (This, CTRL_REG1, Ctrl1);
      Write (This, CTRL_REG4, Ctrl4);

      Configure_Interrupt_Pins (This);
   end Configure;

   --------------------------------
   -- Configure_High_Pass_Filter --
   --------------------------------

   procedure Configure_High_Pass_Filter
     (This             : in out Three_Axis_Gyroscope;
      Mode_Selection   : High_Pass_Filter_Mode;
      Cutoff_Frequency : High_Pass_Cutoff_Frequency)
   is
      Ctrl2 : Byte;
   begin
      --  note that the two high-order bits must remain zero, per the datasheet
      Ctrl2 := As_Byte (Mode_Selection) or As_Byte (Cutoff_Frequency);
      Write (This, CTRL_REG2, Ctrl2);
   end Configure_High_Pass_Filter;

   -----------------------------
   -- Enable_High_Pass_Filter --
   -----------------------------

   procedure Enable_High_Pass_Filter (This : in out Three_Axis_Gyroscope) is
      Ctrl5 : Byte;
   begin
      Read (This, CTRL_REG5, Ctrl5);
      --  set HPen bit
      Ctrl5 := Ctrl5 or HighPass_Filter_Enable;
      Write (This, CTRL_REG5, Ctrl5);
   end Enable_High_Pass_Filter;

   ------------------------------
   -- Disable_High_Pass_Filter --
   ------------------------------

   procedure Disable_High_Pass_Filter (This : in out Three_Axis_Gyroscope) is
      Ctrl5 : Byte;
   begin
      Read (This, CTRL_REG5, Ctrl5);
      --  clear HPen bit
      Ctrl5 := Ctrl5 and (not HighPass_Filter_Enable);
      Write (This, CTRL_REG5, Ctrl5);
   end Disable_High_Pass_Filter;

   ---------------------
   -- Reference_Value --
   ---------------------

   function Reference_Value (This : Three_Axis_Gyroscope) return Byte is
      Result : Byte;
   begin
      Read (This, Reference, Result);
      return Result;
   end Reference_Value;

   -----------------
   -- Data_Status --
   -----------------

   function Data_Status (This : Three_Axis_Gyroscope) return Gyro_Data_Status is
      Result : Byte;
      function As_Gyro_Data_Status is
        new Ada.Unchecked_Conversion (Source => Byte, Target => Gyro_Data_Status);
   begin
      Read (This, Status, Result);
      return As_Gyro_Data_Status (Result);
   end Data_Status;

   ---------------
   -- Device_Id --
   ---------------

   function Device_Id (This : Three_Axis_Gyroscope) return Byte is
      Result : Byte;
   begin
      Read (This, Who_Am_I, Result);
      return Result;
   end Device_Id;

   ------------
   -- Reboot --
   ------------

   procedure Reboot (This : Three_Axis_Gyroscope) is
      Ctrl5 : Byte;
   begin
      Read (This, CTRL_REG5, Ctrl5);
      --  set the boot bit
      Ctrl5 := Ctrl5 or Boot_Bit;
      Write (This, CTRL_REG5, Ctrl5);
   end Reboot;

   ----------------------------
   -- Full_Scale_Sensitivity --
   ----------------------------

   function Full_Scale_Sensitivity (This : Three_Axis_Gyroscope) return Float is
      Ctrl4  : Byte;
      Result : Float;
      Fullscale_Selection : Byte;
   begin
      Read (This, CTRL_REG4, Ctrl4);

      Fullscale_Selection := Ctrl4 and Fullscale_Selection_Bits;

      if Fullscale_Selection = L3GD20_Fullscale_250'Enum_Rep then
         Result := Sensitivity_250dps;
      elsif Fullscale_Selection = L3GD20_Fullscale_500'Enum_Rep then
         Result := Sensitivity_500dps;
      else
         Result := Sensitivity_2000dps;
      end if;

      return Result;
   end Full_Scale_Sensitivity;

   -------------------------
   -- Get_Raw_Angle_Rates --
   -------------------------

   procedure Get_Raw_Angle_Rates
     (This  : Three_Axis_Gyroscope;
      Rates : out Angle_Rates)
   is
      Ctrl4 : Byte;

      type Buffer is array (0 .. 5) of Byte with Component_Size => 8;

      Received : Buffer with Alignment => Angle_Rate'Alignment;

      type Angle_Rate_Pointer is access all Angle_Rate with Storage_Size => 0;

      function As_Angle_Rate_Pointer is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Angle_Rate_Pointer);
      --  So that we can treat the address of a byte as a pointer to a two-byte
      --  sequence representing a signed integer quantity. That's also why the
      --  alignment of Received is specified.

      function Swap_Bytes (Input : Angle_Rate) return Angle_Rate;
      pragma Import (Intrinsic, Swap_Bytes, "__builtin_bswap16");
   begin
      Read (This, CTRL_REG4, Ctrl4);

      Read (This, OUT_X_L, Received (0));
      Read (This, OUT_X_H, Received (1));
      Read (This, OUT_Y_L, Received (2));
      Read (This, OUT_Y_H, Received (3));
      Read (This, OUT_Z_L, Received (4));
      Read (This, OUT_Z_H, Received (5));

      Rates.X := As_Angle_Rate_Pointer (Received (0)'Address).all;
      Rates.Y := As_Angle_Rate_Pointer (Received (2)'Address).all;
      Rates.Z := As_Angle_Rate_Pointer (Received (4)'Address).all;

      if (Ctrl4 and Endian_Selection_Mask) = L3GD20_Big_Endian'Enum_Rep then
         Rates.X := Swap_Bytes (Rates.X);
         Rates.Y := Swap_Bytes (Rates.Y);
         Rates.Z := Swap_Bytes (Rates.Z);
      end if;
   end Get_Raw_Angle_Rates;

   --------------------------
   -- Configure_Interrupt1 --
   --------------------------

   procedure Configure_Interrupt1
     (This : in out Three_Axis_Gyroscope;
      Triggers       : Threshold_Event_List;
      Latched        : Boolean := False;
      Active_Edge    : Interrupt1_Active_Edge := L3GD20_Interrupt1_High_Edge;
      Combine_Events : Boolean := False;
      Sample_Count   : Sample_Counter := 0)
   is
      Config : Byte;
      Ctrl3  : Byte;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and 16#DF#;
      Ctrl3 := Ctrl3 or Active_Edge'Enum_Rep;
      Ctrl3 := Ctrl3 or INT1_Interrupt_Enable;
      Write (This, CTRL_REG3, Ctrl3);

      if Sample_Count > 0 then
         Set_Duration_Counter (This, Sample_Count);
      end if;

      Config := 0;

      if Latched then
         Config := Config or Int1_Latch_Enable_Bit;
      end if;

      if Combine_Events then
         Config := Config or Logically_And_Or_Events_Bit;
      end if;

      for Event of Triggers loop
         Config := Config or Axes_Interrupt_Enablers (Event.Axis);
      end loop;

      Write (This, INT1_CFG, Config);

      for Event of Triggers loop
         Set_Threshold (This, Event.Axis, Event.Threshold);
      end loop;
   end Configure_Interrupt1;

   ----------------------------
   -- Set_Interrupt_Pin_Mode --
   ----------------------------

   procedure Set_Interrupt_Pin_Mode
     (This : in out Three_Axis_Gyroscope;
      Mode : Pin_Modes)
   is
      Ctrl3 : Byte;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not Interrupt_Pin_Mode_Bit);
      Ctrl3 := Ctrl3 or Mode'Enum_Rep;
      Write (This, CTRL_REG3, Ctrl3);
   end Set_Interrupt_Pin_Mode;

   -----------------------
   -- Enable_Interrupt1 --
   -----------------------

   procedure Enable_Interrupt1 (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : Byte;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 or INT1_Interrupt_Enable;
      Write (This, CTRL_REG3, Ctrl3);

      --  should these calls really be done here, rather than in a higher level
      --  abstraction (eg a "Gyro" ADT), or by the client???
      Connect_External_Interrupt (This.Int1_Pin);

      Configure_Trigger (This.Int1_Pin, Interrupt_Falling_Edge);
      -- hardcoded edge, should use ActiveEdge ???
   end Enable_Interrupt1;

   ------------------------
   -- Disable_Interrupt1 --
   ------------------------

   procedure Disable_Interrupt1 (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : Byte;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not INT1_Interrupt_Enable);
      Write (This, CTRL_REG3, Ctrl3);
   end Disable_Interrupt1;

   -----------------------
   -- Interrupt1_Status --
   -----------------------

   function Interrupt1_Source (This : Three_Axis_Gyroscope) return Interrupt1_Sources is
      Result : Byte;
      function As_Interrupt_Source is new Ada.Unchecked_Conversion
        (Source => Byte, Target => Interrupt1_Sources);
   begin
      Read (This, INT1_SRC, Result);
      return As_Interrupt_Source (Result);
   end Interrupt1_Source;

   --------------------------
   -- Set_Duration_Counter --
   --------------------------

   procedure Set_Duration_Counter
     (This  : in out Three_Axis_Gyroscope;
      Value : Sample_Counter)
   is
      Wait_Bit : constant := 2#1000_0000#;
   begin
      Write (This, INT1_Duration, Byte (Value) or Wait_Bit);
   end Set_Duration_Counter;

   ------------------
   -- Enable_Event --
   ------------------

   procedure Enable_Event
     (This  : in out Three_Axis_Gyroscope;
      Event : Axes_Interrupts)
   is
      Config : Byte;
   begin
      Read (This, INT1_CFG, Config);
      Config := Config or Axes_Interrupt_Enablers (Event);
      Write (This, INT1_CFG, Config);
   end Enable_Event;

   -------------------
   -- Disable_Event --
   -------------------

   procedure Disable_Event
     (This  : in out Three_Axis_Gyroscope;
      Event : Axes_Interrupts)
   is
      Config : Byte;
   begin
      Read (This, INT1_CFG, Config);
      Config := Config and (not Axes_Interrupt_Enablers (Event));
      Write (This, INT1_CFG, Config);
   end Disable_Event;

   -------------------
   -- Set_Threshold --
   -------------------

   procedure Set_Threshold
     (This  : in out Three_Axis_Gyroscope;
      Event : Axes_Interrupts;
      Value : Axis_Sample_Threshold)
   is
   begin
      case Event is
         when Z_High_Interrupt | Z_Low_Interrupt =>
            Write (This, INT1_TSH_ZL, Byte (Value));
            Write (This, INT1_TSH_ZH, Byte (Shift_Right (Value, 8)));

         when Y_High_Interrupt | Y_Low_Interrupt =>
            Write (This, INT1_TSH_YL, Byte (Value));
            Write (This, INT1_TSH_YH, Byte (Shift_Right (Value, 8)));

         when X_High_Interrupt | X_Low_Interrupt =>
            Write (This, INT1_TSH_XL, Byte (Value));
            Write (This, INT1_TSH_XH, Byte (Shift_Right (Value, 8)));
      end case;
   end Set_Threshold;

   -----------------
   -- Enable_FIFO --
   -----------------

   procedure Enable_FIFO
     (This : in out Three_Axis_Gyroscope;
      Mode : FIFO_Modes)
   is
      Ctrl5 : Byte;
      FIFO : Byte;
   begin
      Read (This, CTRL_REG5, Ctrl5);
      Ctrl5 := Ctrl5 or FIFO_Enable_Bit;
      Write (This, CTRL_REG5, Ctrl5);

      Read (This, FIFO_CTRL, FIFO);
      FIFO := FIFO and (not FIFO_Mode_Bits);  -- clear the current bits
      FIFO := FIFO or Mode'Enum_Rep;
      Write (This, FIFO_CTRL, FIFO);
   end Enable_FIFO;

   ------------------
   -- Disable_FIFO --
   ------------------

   procedure Disable_FIFO (This : in out Three_Axis_Gyroscope) is
      Ctrl5 : Byte;
   begin
      Read (This, CTRL_REG5, Ctrl5);
      Ctrl5 := Ctrl5 and (not FIFO_Enable_Bit);
      Write (This, CTRL_REG5, Ctrl5);
   end Disable_FIFO;

   -------------------
   -- Set_Watermark --
   -------------------

   procedure Set_Watermark
     (This  : in out Three_Axis_Gyroscope;
      Level : FIFO_Level)
   is
      Value : Byte;
   begin
      Read (This, FIFO_CTRL, Value);
      Value := Value and (not Watermark_Threshold_Bits); -- clear the bits
      Value := Value or Byte (Level);
      Write (This, FIFO_CTRL, Value);
   end Set_Watermark;

   ------------------------
   -- Current_FIFO_Depth --
   ------------------------

   function Current_FIFO_Depth (This : Three_Axis_Gyroscope) return FIFO_Level is
      Result : Byte;
   begin
      Read (This, FIFO_SRC, Result);
      Result := Result and FIFO_Depth_Bits;
      return FIFO_Level (Result);
   end Current_FIFO_Depth;

   --------------------------
   -- FIFO_Below_Watermark --
   --------------------------

   function FIFO_Below_Watermark (This : Three_Axis_Gyroscope) return Boolean is
      Result : Byte;
   begin
      Read (This, FIFO_SRC, Result);
      Result := Result and Watermark_Status_Bit;
      return Result = 0;
   end FIFO_Below_Watermark;

   ----------------
   -- FIFO_Empty --
   ----------------

   function FIFO_Empty (This : Three_Axis_Gyroscope) return Boolean is
      Result : Byte;
   begin
      Read (This, FIFO_SRC, Result);
      Result := Result and FIFO_Empty_Bit;
      return Result = 1;
   end FIFO_Empty;

   ------------------
   -- FIFO_Overrun --
   ------------------

   function FIFO_Overrun (This : Three_Axis_Gyroscope) return Boolean is
      Result : Byte;
   begin
      Read (This, FIFO_SRC, Result);
      Result := Result and FIFO_Overrun_Bit;
      return Result = 1;
   end FIFO_Overrun;

   ---------------------------------
   -- Enable_Data_Ready_Interrupt --
   ---------------------------------

   procedure Enable_Data_Ready_Interrupt (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : Byte;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 or INT2_Data_Ready_Interrupt_Enable;
      Write (This, CTRL_REG3, Ctrl3);
   end Enable_Data_Ready_Interrupt;

   ----------------------------------
   -- Disable_Data_Ready_Interrupt --
   ----------------------------------

   procedure Disable_Data_Ready_Interrupt (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : Byte;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not INT2_Data_Ready_Interrupt_Enable);
      Write (This, CTRL_REG3, Ctrl3);
   end Disable_Data_Ready_Interrupt;

   --------------------------------
   -- Enable_Watermark_Interrupt --
   --------------------------------

   procedure Enable_Watermark_Interrupt (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : Byte;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 or INT2_Watermark_Interrupt_Enable;
      Write (This, CTRL_REG3, Ctrl3);
   end Enable_Watermark_Interrupt;

   ---------------------------------
   -- Disable_Watermark_Interrupt --
   ---------------------------------

   procedure Disable_Watermark_Interrupt (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : Byte;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not INT2_Watermark_Interrupt_Enable);
      Write (This, CTRL_REG3, Ctrl3);
   end Disable_Watermark_Interrupt;

   ------------------------------
   -- Enable_Overrun_Interrupt --
   ------------------------------

   procedure Enable_Overrun_Interrupt (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : Byte;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 or INT2_Overrun_Interrupt_Enable;
      Write (This, CTRL_REG3, Ctrl3);
   end Enable_Overrun_Interrupt;

   -------------------------------
   -- Disable_Overrun_Interrupt --
   -------------------------------

   procedure Disable_Overrun_Interrupt (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : Byte;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not INT2_Overrun_Interrupt_Enable);
      Write (This, CTRL_REG3, Ctrl3);
   end Disable_Overrun_Interrupt;

   ---------------------------------
   -- Enable_FIFO_Empty_Interrupt --
   ---------------------------------

   procedure Enable_FIFO_Empty_Interrupt (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : Byte;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 or INT2_FIFO_Empty_Interrupt_Enable;
      Write (This, CTRL_REG3, Ctrl3);
   end Enable_FIFO_Empty_Interrupt;

   ----------------------------------
   -- Disable_FIFO_Empty_Interrupt --
   ----------------------------------

   procedure Disable_FIFO_Empty_Interrupt (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : Byte;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not INT2_FIFO_Empty_Interrupt_Enable);
      Write (This, CTRL_REG3, Ctrl3);
   end Disable_FIFO_Empty_Interrupt;

end STM32.L3DG20;
