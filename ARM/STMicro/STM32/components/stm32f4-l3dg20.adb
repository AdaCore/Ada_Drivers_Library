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
--   @brief   Header file of DMA HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with GNAT.Byte_Swapping; use GNAT.Byte_Swapping;
with STM32F4.SYSCFG;     use STM32F4.SYSCFG;

package body STM32F4.L3DG20 is

   Sensitivity_250dps  : constant := 8.75 * 0.001; -- mdps/digit
   Sensitivity_500dps  : constant := 17.5 * 0.001; -- mdps/digit
   Sensitivity_2000dps : constant := 70.0 * 0.001; -- mdps/digit

   ReadWrite_CMD : constant := 16#80#;

   --  bit definitions for the CTRL_REG3 register

   INT1_Interrupt_Enable            : constant := 2#1000_0000#;
   Interrupt_Pin_Mode_Bit           : constant := 2#0001_0000#;  -- PP_OD
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
   HighPass_Filter_Enable : constant := 2#0001_0000#; -- 16#10#;

   --  bit definitions for the STATUS register

   XYZ_Data_Available : constant := 2#0000_1000#;

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
     (Source => High_Pass_Cut_Off_Frequency, Target => Byte);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => Power_Mode_Selection, Target => Byte);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => Output_DataRate_Selection, Target => Byte);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => Axes_Selection, Target => Byte);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => BandWidth_Selection, Target => Byte);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => Block_Data_Update_Selection, Target => Byte);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => Endian_Data_Selection, Target => Byte);

   function As_Byte is new Ada.Unchecked_Conversion
     (Source => Full_Scale_Selection, Target => Byte);

   -----------------
   -- Chip_Select --
   -----------------

   procedure Chip_Select (Enabled : Boolean) is
      --  When the pin is low (cleared), the device is in SPI mode.
      --  When the pin is high (set), the device is in I2C mode.
      --  We want SPI mode communication, so Enabled, when True,
      --  means we must drive the pin low.
   begin
      if Enabled then
         GPIO.Clear (CS_GPIO, CS_Pin);
      else
         GPIO.Set (CS_GPIO, CS_Pin);
      end if;
   end Chip_Select;

   -----------
   -- Write --
   -----------

   procedure Write (Addr : Register;  Data : Byte) is
   begin
      Chip_Select (Enabled => True);
      SPI.Transmit (L3GD20_SPI, Byte (Addr));
      SPI.Transmit (L3GD20_SPI, Data);
      Chip_Select (Enabled => False);
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read (Addr : Register; Data : out Byte) is
   begin
      Chip_Select (Enabled => True);
      SPI.Transmit (L3GD20_SPI, Byte (Addr) or READWRITE_CMD);
      SPI.Receive (L3GD20_SPI, Data);
      Chip_Select (Enabled => False);
   end Read;

   ----------------------
   -- Init_Chip_Select --
   ----------------------

   procedure Init_Chip_Select is
      GPIO_Conf : GPIO_Port_Configuration;
   begin
      Enable_Chip_Select_Clock;

      GPIO_Conf.Speed := Speed_25MHz;
      GPIO_Conf.Mode := Mode_OUT;
      GPIO_Conf.Output_Type := Push_Pull;
      GPIO_Conf.Resistors := Pull_Up;
--        GPIO_Conf.Locked := True;

      Configure_IO (CS_GPIO, CS_Pin, GPIO_Conf);
   end Init_Chip_Select;

   ----------------------
   -- Init_SPI_IO_Pins --
   ----------------------

   procedure Init_SPI_IO_Pins is
      GPIO_Conf : GPIO_Port_Configuration;
   begin
      Enable_SPI_GPIO_Clock;

      GPIO_Conf.Speed       := Speed_100MHz;
      GPIO_Conf.Mode        := Mode_AF;
      GPIO_Conf.Output_Type := Push_Pull;
      GPIO_Conf.Resistors   := Floating;  -- pull_down ???
--        GPIO_Conf.Locked := True;

      Configure_IO (SPI_GPIO, SCK_Pin & MISO_Pin & MOSI_Pin, GPIO_Conf);

      Configure_Alternate_Function
        (SPI_GPIO,
         SCK_Pin & MISO_Pin & MOSI_Pin,
         SPI_GPIO_AF);
   end Init_SPI_IO_Pins;

   --------------
   -- Init_SPI --
   --------------

   procedure Init_SPI is
      SPI_Conf  : SPI_Configuration;
   begin
      Enable_SPI_Clock;

      Init_SPI_IO_Pins;

      SPI.Disable (L3GD20_SPI);

      SPI_Conf.Direction           := D2Lines_FullDuplex;
      SPI_Conf.Mode                := Master;
      SPI_Conf.Data_Size           := Data_8;
      SPI_Conf.Clock_Polarity      := Low;
      SPI_Conf.Clock_Phase         := P1Edge;
      SPI_Conf.Slave_Management    := Software_Managed;
      SPI_Conf.Baud_Rate_Prescaler := BRP_32;
      SPI_Conf.First_Bit           := MSB;
      SPI_Conf.CRC_Poly            := 7;

      SPI.Configure (L3GD20_SPI, SPI_Conf);

      SPI.Enable (L3GD20_SPI);
   end Init_SPI;

   --------------------------
   -- Initialize_Device_IO --
   --------------------------

   procedure Initialize_Device_IO is
   begin
      Init_Chip_Select;
      Chip_Select (Enabled => False);
      Init_SPI;
   end Initialize_Device_IO;

   ------------------------------
   -- Configure_Interrupt_Pins --
   ------------------------------

   procedure Configure_Interrupt_Pins is
      GPIO_Conf : GPIO_Port_Configuration;
   begin
      Enable_GPIO_Interrupt_Clock;

      GPIO_Conf.Speed := Speed_50MHz;
      GPIO_Conf.Mode := Mode_In;
      GPIO_Conf.Output_Type := Push_Pull;
      GPIO_Conf.Resistors := Floating;
--        GPIO_Conf.Locked := True;

      Configure_IO (Int_GPIO, Int1_Pin & Int2_Pin, GPIO_Conf);
   end Configure_Interrupt_Pins;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Power_Mode       : Power_Mode_Selection;
      Output_DataRate  : Output_DataRate_Selection;
      Axes_Enable      : Axes_Selection;
      Band_Width       : BandWidth_Selection;
      BlockData_Update : Block_Data_Update_Selection;
      Endianness       : Endian_Data_Selection;
      Full_Scale       : Full_Scale_Selection)
   is
      Ctrl1 : Byte;
      Ctrl4 : Byte;
   begin
      Initialize_Device_IO;

      Ctrl1 := As_Byte (Power_Mode)      or
               As_Byte (Output_DataRate) or
               As_Byte (Axes_Enable)     or
               As_Byte (Band_Width);

      Ctrl4 := As_Byte (BlockData_Update) or
               As_Byte (Endianness)       or
               As_Byte (Full_Scale);

      Write (CTRL_REG1, Ctrl1);
      Write (CTRL_REG4, Ctrl4);

      Configure_Interrupt_Pins;
   end Configure;

   ----------------------
   -- Configure_Filter --
   ----------------------

   procedure Configure_Filter
     (Mode_Selection   : High_Pass_Filter_Mode;
      CutOff_Frequency : High_Pass_Cut_Off_Frequency)
   is
      Ctrl2 : Byte;
   begin
      --  note that the two high-order bits must remain zero, per the datasheet
      Ctrl2 := As_Byte (Mode_Selection) or As_Byte (CutOff_Frequency);
      Write (CTRL_REG2, Ctrl2);
   end Configure_Filter;

   -------------------
   -- Enable_Filter --
   -------------------

   procedure Enable_Filter is
      Ctrl5 : Byte;
   begin
      Read (CTRL_REG5, Ctrl5);
      --  set HPen bit
      Ctrl5 := Ctrl5 or HighPass_Filter_Enable;
      Write (CTRL_REG5, Ctrl5);
   end Enable_Filter;

   --------------------
   -- Disable_Filter --
   --------------------

   procedure Disable_Filter is
      Ctrl5 : Byte;
   begin
      Read (CTRL_REG5, Ctrl5);
      --  clear HPen bit
      Ctrl5 := Ctrl5 and (not HighPass_Filter_Enable);
      Write (CTRL_REG5, Ctrl5);
   end Disable_Filter;

   -----------------
   -- Data_Status --
   -----------------

   function Data_Status return Byte is
      Result : Byte;
   begin
      Read (STATUS, Result);
      return Result;
   end Data_Status;

   ---------------
   -- Device_Id --
   ---------------

   function Device_Id return Byte is
      Result : Byte;
   begin
      Read (Who_Am_I, Result);
      return Result;
   end Device_Id;

   ------------
   -- Reboot --
   ------------

   procedure Reboot is
      Ctrl5 : Byte;
   begin
      Read (CTRL_REG5, Ctrl5);
      --  set the boot bit
      Ctrl5 := Ctrl5 or Boot_Bit;
      Write (CTRL_REG5, Ctrl5);
   end Reboot;

   --------------------------
   -- Selected_Sensitivity --
   --------------------------

   function Selected_Sensitivity return Float is
      Ctrl4 : Byte;
      Result : Float;
      Fullscale_Selection : Byte;
   begin
      --  we could store this value within the package, so that we would not
      --  need to query the register again...

      Read (CTRL_REG4, Ctrl4);

      Fullscale_Selection := Ctrl4 and Fullscale_Selection_Bits;

      if Fullscale_Selection = L3GD20_Fullscale_250'Enum_Rep then
         Result := Sensitivity_250dps;
      elsif Fullscale_Selection = L3GD20_Fullscale_500'Enum_Rep then
         Result := Sensitivity_500dps;
      else
         Result := Sensitivity_2000dps;
      end if;

      return Result;
   end Selected_Sensitivity;

   -------------------------
   -- Get_Raw_Angle_Rates --
   -------------------------

   procedure Get_Raw_Angle_Rates (Rates : out Raw_Angle_Rates) is
      Ctrl4       : Byte;
      Status      : Byte;
      Received    : array (0 .. 5) of Byte with Alignment => Integer_16'Alignment;
      Unscaled    : array (0 .. 2) of Integer_16;

      type Integer16_Pointer is access all Integer_16 with Storage_Size => 0;

      function As_Pointer is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Integer16_Pointer);
      --  So that we can treat the address of a byte as a pointer to a two-byte
      --  sequence representing a signed Integer_16 quantity. That's why the
      --  alignment of Reg_Data is set as well.

      Max_Status_Attempts : constant := 1_000;  -- semi-arbitrary
   begin
      for K in 1 .. Max_Status_Attempts loop
         Status := Data_Status;
         exit when (Status and XYZ_Data_Available) /= 0;
         if K = Max_Status_Attempts then
            raise Program_Error with "Timeout getting data status";
         end if;
      end loop;

      Read (CTRL_REG4, Ctrl4);

      Read (OUT_X_L, Received (0));
      Read (OUT_X_H, Received (1));
      Read (OUT_Y_L, Received (2));
      Read (OUT_Y_H, Received (3));
      Read (OUT_Z_L, Received (4));
      Read (OUT_Z_H, Received (5));

      --  convert received data into integer_16 quantities, taking selected
      --  endianess into account
      Unscaled (0) := As_Pointer (Received (0)'Address).all;
      Unscaled (1) := As_Pointer (Received (2)'Address).all;
      Unscaled (2) := As_Pointer (Received (4)'Address).all;
      if (Ctrl4 and Endian_Selection_Mask) = L3GD20_BLE_MSB'Enum_Rep then
         Swap2 (Unscaled (0)'Address);
         Swap2 (Unscaled (1)'Address);
         Swap2 (Unscaled (2)'Address);
      end if;

      if Unscaled (0) = 16#FF#
        or Unscaled (1) = 16#FF#
        or Unscaled (2) = 16#FF#
      then
         Rates := (others => 0);
      else
         Rates.X := Unscaled (0);
         Rates.Y := Unscaled (1);
         Rates.Z := Unscaled (2);
      end if;
   end Get_Raw_Angle_Rates;

   --------------------------
   -- Configure_Interrupt1 --
   --------------------------

   procedure Configure_Interrupt1
     (Triggers       : Threshold_Event_List;
      Latched        : Boolean := False;
      Active_Edge    : Interrupt1_Active_Edge := L3GD20_Interrupt1_High_Edge;
      Combine_Events : Boolean := False;
      Sample_Count   : Sample_Counter := 0)
   is
      Config : Byte;
      Ctrl3  : Byte;
   begin
      Read (CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and 16#DF#;
      Ctrl3 := Ctrl3 or Active_Edge'Enum_Rep;
      Ctrl3 := Ctrl3 or INT1_Interrupt_Enable;
      Write (CTRL_REG3, Ctrl3);

      if Sample_Count > 0 then
         Set_Duration_Counter (Sample_Count);
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

      Write (INT1_CFG, Config);

      for Event of Triggers loop
         Set_Threshold (Event.Axis, Event.Threshold);
      end loop;
   end Configure_Interrupt1;

   ----------------------------
   -- Set_Interrupt_Pin_Mode --
   ----------------------------

   procedure Set_Interrupt_Pin_Mode (Mode : Pin_Modes) is
      Ctrl3 : Byte;
   begin
      Read (CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not Interrupt_Pin_Mode_Bit);
      Ctrl3 := Ctrl3 or Mode'Enum_Rep;
      Write (CTRL_REG3, Ctrl3);
   end Set_Interrupt_Pin_Mode;

   -----------------------
   -- Enable_Interrupt1 --
   -----------------------

   procedure Enable_Interrupt1 is
      Ctrl3 : Byte;
   begin
      Read (CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 or INT1_Interrupt_Enable;
      Write (CTRL_REG3, Ctrl3);

      --  should these calls really be done here, rather than in a higher level
      --  abstraction (eg a "Gyro" ADT), or by the client???
      Connect_External_Interrupt (Int_GPIO, Int1_Pin);

      Configure_Trigger (Int_GPIO, Int1_Pin, Interrupt_Falling_Edge);
      -- hardcoded edge, should use ActiveEdge ???
   end Enable_Interrupt1;

   ------------------------
   -- Disable_Interrupt1 --
   ------------------------

   procedure Disable_Interrupt1 is
      Ctrl3 : Byte;
   begin
      Read (CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not INT1_Interrupt_Enable);
      Write (CTRL_REG3, Ctrl3);
   end Disable_Interrupt1;

   -----------------------
   -- Interrupt1_Status --
   -----------------------

   function Interrupt1_Source return Interrupt1_Sources is
      Result : Byte;
      function As_Interrupt_Source is new Ada.Unchecked_Conversion
        (Source => Byte, Target => Interrupt1_Sources);
   begin
      Read (INT1_SRC, Result);
      return As_Interrupt_Source (Result);
   end Interrupt1_Source;

   --------------------------
   -- Set_Duration_Counter --
   --------------------------

   procedure Set_Duration_Counter (Value : Sample_Counter) is
      Wait_Bit : constant := 2#1000_0000#;
   begin
      Write (INT1_Duration, Byte (Value) or Wait_Bit);
   end Set_Duration_Counter;

   ------------------
   -- Enable_Event --
   ------------------

   procedure Enable_Event (Event : Axes_Interrupts) is
      Config : Byte;
   begin
      Read (INT1_CFG, Config);
      Config := Config or Axes_Interrupt_Enablers (Event);
      Write (INT1_CFG, Config);
   end Enable_Event;

   -------------------
   -- Disable_Event --
   -------------------

   procedure Disable_Event (Event : Axes_Interrupts) is
      Config : Byte;
   begin
      Read (INT1_CFG, Config);
      Config := Config and (not Axes_Interrupt_Enablers (Event));
      Write (INT1_CFG, Config);
   end Disable_Event;

   -------------------
   -- Set_Threshold --
   -------------------

   procedure Set_Threshold
     (Event : Axes_Interrupts;
      Value : Axis_Sample_Threshold)
   is
   begin
      case Event is
         when Z_High_Interrupt | Z_Low_Interrupt =>
            Write (INT1_TSH_ZL, Byte (Value));
            Write (INT1_TSH_ZH, Byte (Shift_Right (Value, 8)));

         when Y_High_Interrupt | Y_Low_Interrupt =>
            Write (INT1_TSH_YL, Byte (Value));
            Write (INT1_TSH_YH, Byte (Shift_Right (Value, 8)));

         when X_High_Interrupt | X_Low_Interrupt =>
            Write (INT1_TSH_XL, Byte (Value));
            Write (INT1_TSH_XH, Byte (Shift_Right (Value, 8)));
      end case;
   end Set_Threshold;

   -----------------
   -- Enable_FIFO --
   -----------------

   procedure Enable_FIFO (Mode : FIFO_Modes) is
      Ctrl5 : Byte;
      FIFO : Byte;
   begin
      Read (CTRL_REG5, Ctrl5);
      Ctrl5 := Ctrl5 or FIFO_Enable_Bit;
      Write (CTRL_REG5, Ctrl5);

      Read (FIFO_CTRL, FIFO);
      FIFO := FIFO and (not FIFO_Mode_Bits);  -- clear the current bits
      FIFO := FIFO or Mode'Enum_Rep;
      Write (FIFO_CTRL, FIFO);
   end Enable_FIFO;

   ------------------
   -- Disable_FIFO --
   ------------------

   procedure Disable_FIFO is
      Ctrl5 : Byte;
   begin
      Read (CTRL_REG5, Ctrl5);
      Ctrl5 := Ctrl5 and (not FIFO_Enable_Bit);
      Write (CTRL_REG5, Ctrl5);
   end Disable_FIFO;

   -------------------
   -- Set_Watermark --
   -------------------

   procedure Set_Watermark (Level : FIFO_Level) is
      Value : Byte;
   begin
      Read (FIFO_CTRL, Value);
      Value := Value and (not Watermark_Threshold_Bits); -- clear the bits
      Value := Value or Byte (Level);
      Write (FIFO_CTRL, Value);
   end Set_Watermark;

   ------------------------
   -- Current_FIFO_Depth --
   ------------------------

   function Current_FIFO_Depth return FIFO_Level is
      Result : Byte;
   begin
      Read (FIFO_SRC, Result);
      Result := Result and FIFO_Depth_Bits;
      return FIFO_Level (Result);
   end Current_FIFO_Depth;

   --------------------------
   -- FIFO_Below_Watermark --
   --------------------------

   function FIFO_Below_Watermark return Boolean is
      Result : Byte;
   begin
      Read (FIFO_SRC, Result);
      Result := Result and Watermark_Status_Bit;
      return Result = 0;
   end FIFO_Below_Watermark;

   ----------------
   -- FIFO_Empty --
   ----------------

   function FIFO_Empty return Boolean is
      Result : Byte;
   begin
      Read (FIFO_SRC, Result);
      Result := Result and FIFO_Empty_Bit;
      return Result = 1;
   end FIFO_Empty;

   ------------------
   -- FIFO_Overrun --
   ------------------

   function FIFO_Overrun return Boolean is
      Result : Byte;
   begin
      Read (FIFO_SRC, Result);
      Result := Result and FIFO_Overrun_Bit;
      return Result = 1;
   end FIFO_Overrun;

   ---------------------------------
   -- Enable_Data_Ready_Interrupt --
   ---------------------------------

   procedure Enable_Data_Ready_Interrupt is
      Ctrl3 : Byte;
   begin
      Read (CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 or INT2_Data_Ready_Interrupt_Enable;
      Write (CTRL_REG3, Ctrl3);
   end Enable_Data_Ready_Interrupt;

   ----------------------------------
   -- Disable_Data_Ready_Interrupt --
   ----------------------------------

   procedure Disable_Data_Ready_Interrupt is
      Ctrl3 : Byte;
   begin
      Read (CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not INT2_Data_Ready_Interrupt_Enable);
      Write (CTRL_REG3, Ctrl3);
   end Disable_Data_Ready_Interrupt;

   --------------------------------
   -- Enable_Watermark_Interrupt --
   --------------------------------

   procedure Enable_Watermark_Interrupt is
      Ctrl3 : Byte;
   begin
      Read (CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 or INT2_Watermark_Interrupt_Enable;
      Write (CTRL_REG3, Ctrl3);
   end Enable_Watermark_Interrupt;

   ---------------------------------
   -- Disable_Watermark_Interrupt --
   ---------------------------------

   procedure Disable_Watermark_Interrupt is
      Ctrl3 : Byte;
   begin
      Read (CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not INT2_Watermark_Interrupt_Enable);
      Write (CTRL_REG3, Ctrl3);
   end Disable_Watermark_Interrupt;

   ------------------------------
   -- Enable_Overrun_Interrupt --
   ------------------------------

   procedure Enable_Overrun_Interrupt is
      Ctrl3 : Byte;
   begin
      Read (CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 or INT2_Overrun_Interrupt_Enable;
      Write (CTRL_REG3, Ctrl3);
   end Enable_Overrun_Interrupt;

   -------------------------------
   -- Disable_Overrun_Interrupt --
   -------------------------------

   procedure Disable_Overrun_Interrupt is
      Ctrl3 : Byte;
   begin
      Read (CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not INT2_Overrun_Interrupt_Enable);
      Write (CTRL_REG3, Ctrl3);
   end Disable_Overrun_Interrupt;

   ---------------------------------
   -- Enable_FIFO_Empty_Interrupt --
   ---------------------------------

   procedure Enable_FIFO_Empty_Interrupt is
      Ctrl3 : Byte;
   begin
      Read (CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 or INT2_FIFO_Empty_Interrupt_Enable;
      Write (CTRL_REG3, Ctrl3);
   end Enable_FIFO_Empty_Interrupt;

   ----------------------------------
   -- Disable_FIFO_Empty_Interrupt --
   ----------------------------------

   procedure Disable_FIFO_Empty_Interrupt is
      Ctrl3 : Byte;
   begin
      Read (CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not INT2_FIFO_Empty_Interrupt_Enable);
      Write (CTRL_REG3, Ctrl3);
   end Disable_FIFO_Empty_Interrupt;

end STM32F4.L3DG20;
