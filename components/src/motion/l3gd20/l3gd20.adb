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
with System;

package body L3GD20 is

   --  the following are per Table 4 of the L3GD20 Datasheet, pg 9.
   --  the values are millidegrees per second, so we scale accordingly
   Sensitivity_250dps  : constant := 8.75 * 0.001; -- mdps/digit
   Sensitivity_500dps  : constant := 17.5 * 0.001; -- mdps/digit
   Sensitivity_2000dps : constant := 70.0 * 0.001; -- mdps/digit

   ReadWrite_CMD : constant := 16#80#;
   MultiUInt8_CMD       : constant := 16#40#;

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
   LowPass_Filter_Enable  : constant := 2#0000_0010#;

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

   Axes_Interrupt_Enablers : constant array (Axes_Interrupts) of UInt8 :=
     (Z_High_Interrupt => 2#0010_0000#,
      Z_Low_Interrupt  => 2#0001_0000#,
      Y_High_Interrupt => 2#0000_1000#,
      Y_Low_Interrupt  => 2#0000_0100#,
      X_High_Interrupt => 2#0000_0010#,
      X_Low_Interrupt  => 2#0000_0001#);

   function As_UInt8 is new Ada.Unchecked_Conversion
     (Source => High_Pass_Filter_Mode, Target => UInt8);

   function As_UInt8 is new Ada.Unchecked_Conversion
     (Source => High_Pass_Cutoff_Frequency, Target => UInt8);

   function As_UInt8 is new Ada.Unchecked_Conversion
     (Source => Power_Mode_Selection, Target => UInt8);

   function As_UInt8 is new Ada.Unchecked_Conversion
     (Source => Output_Data_Rate_Selection, Target => UInt8);

   function As_UInt8 is new Ada.Unchecked_Conversion
     (Source => Axes_Selection, Target => UInt8);

   function As_UInt8 is new Ada.Unchecked_Conversion
     (Source => Bandwidth_Selection, Target => UInt8);

   function As_UInt8 is new Ada.Unchecked_Conversion
     (Source => Block_Data_Update_Selection, Target => UInt8);

   function As_UInt8 is new Ada.Unchecked_Conversion
     (Source => Endian_Data_Selection, Target => UInt8);

   function As_UInt8 is new Ada.Unchecked_Conversion
     (Source => Full_Scale_Selection, Target => UInt8);

   type Angle_Rate_Pointer is access all Angle_Rate with Storage_Size => 0;

   function As_Angle_Rate_Pointer is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Angle_Rate_Pointer);
   --  So that we can treat the address of a UInt8 as a pointer to a two-UInt8
   --  sequence representing a signed integer quantity.

   procedure Swap2 (Location : System.Address) with Inline;
   --  Swaps the two UInt8s at Location and Location+1

   procedure SPI_Mode (This : Three_Axis_Gyroscope; Enabled : Boolean);
   --  Enable or disable SPI mode communication with the device. This is named
   --  "chip select" in other demos/examples.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This        : in out Three_Axis_Gyroscope;
      Port        : Any_SPI_Port;
      Chip_Select : Any_GPIO_Point)
   is
   begin
      This.Port := Port;
      This.CS := Chip_Select;

      SPI_Mode (This, Enabled => False);
   end Initialize;

   -----------------
   -- SPI_Mode --
   -----------------

   procedure SPI_Mode (This : Three_Axis_Gyroscope; Enabled : Boolean) is
      --  When the pin is low (cleared), the device is in SPI mode.
      --  When the pin is high (set), the device is in I2C mode.
      --  We want SPI mode communication, so Enabled, when True,
      --  means we must drive the pin low.
   begin
      if Enabled then
         This.CS.Clear;
      else
         This.CS.Set;
      end if;
   end SPI_Mode;

   -----------
   -- Write --
   -----------

   procedure Write (This : Three_Axis_Gyroscope; Addr : Register;  Data : UInt8)
   is
      Status : SPI_Status;
   begin
      SPI_Mode (This, Enabled => True);

      This.Port.Transmit (SPI_Data_8b'(1 => UInt8 (Addr)), Status);
      if Status /= Ok then
         raise Program_Error;
      end if;

      This.Port.Transmit (SPI_Data_8b'(1 => Data), Status);
      if Status /= Ok then
         raise Program_Error;
      end if;

      SPI_Mode (This, Enabled => False);
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (This : Three_Axis_Gyroscope;
      Addr : Register;
      Data : out UInt8)
   is
      Status : SPI_Status;
      Tmp_Data : SPI_Data_8b (1 .. 1);
   begin
      SPI_Mode (This, Enabled => True);

      This.Port.Transmit (SPI_Data_8b'(1 => UInt8 (Addr) or ReadWrite_CMD),
                         Status);
      if Status /= Ok then
         raise Program_Error;
      end if;

      This.Port.Receive (Tmp_Data, Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
      Data := Tmp_Data (Tmp_Data'First);

      SPI_Mode (This, Enabled => False);
   end Read;

   ----------------
   -- Read_UInt8s --
   ----------------

   procedure Read_UInt8s
     (This   : Three_Axis_Gyroscope;
      Addr   : Register;
      Buffer : out SPI_Data_8b;
      Count  : Natural)
   is
      Index : Natural := Buffer'First;
      Status : SPI_Status;
      Tmp_Data : SPI_Data_8b (1 .. 1);
   begin
      SPI_Mode (This, Enabled => True);

      This.Port.Transmit
        (SPI_Data_8b'(1 => UInt8 (Addr) or ReadWrite_CMD or MultiUInt8_CMD),
         Status);

      if Status /= Ok then
         raise Program_Error;
      end if;

      for K in 1 .. Count loop
         This.Port.Receive (Tmp_Data, Status);
         if Status /= Ok then
            raise Program_Error;
         end if;
         Buffer (Index) := Tmp_Data (Tmp_Data'First);
         Index := Index + 1;
      end loop;

      SPI_Mode (This, Enabled => False);
   end Read_UInt8s;

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
      Ctrl1 : UInt8;
      Ctrl4 : UInt8;
   begin
      Ctrl1 := As_UInt8 (Power_Mode)       or
               As_UInt8 (Output_Data_Rate) or
               As_UInt8 (Axes_Enable)      or
               As_UInt8 (Bandwidth);

      Ctrl4 := As_UInt8 (BlockData_Update) or
               As_UInt8 (Endianness)       or
               As_UInt8 (Full_Scale);

      Write (This, CTRL_REG1, Ctrl1);
      Write (This, CTRL_REG4, Ctrl4);
   end Configure;

   -----------
   -- Sleep --
   -----------

   procedure Sleep (This : in out Three_Axis_Gyroscope) is
      Ctrl1      : UInt8;
      Sleep_Mode : constant := 2#1000#;  -- per the Datasheet, Table 22, pg 32
   begin
      Read (This, CTRL_REG1, Ctrl1);
      Ctrl1 := Ctrl1 or Sleep_Mode;
      Write (This, CTRL_REG1, Ctrl1);
   end Sleep;

   --------------------------------
   -- Configure_High_Pass_Filter --
   --------------------------------

   procedure Configure_High_Pass_Filter
     (This             : in out Three_Axis_Gyroscope;
      Mode_Selection   : High_Pass_Filter_Mode;
      Cutoff_Frequency : High_Pass_Cutoff_Frequency)
   is
      Ctrl2 : UInt8;
   begin
      --  note that the two high-order bits must remain zero, per the datasheet
      Ctrl2 := As_UInt8 (Mode_Selection) or As_UInt8 (Cutoff_Frequency);
      Write (This, CTRL_REG2, Ctrl2);
   end Configure_High_Pass_Filter;

   -----------------------------
   -- Enable_High_Pass_Filter --
   -----------------------------

   procedure Enable_High_Pass_Filter (This : in out Three_Axis_Gyroscope) is
      Ctrl5 : UInt8;
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
      Ctrl5 : UInt8;
   begin
      Read (This, CTRL_REG5, Ctrl5);
      --  clear HPen bit
      Ctrl5 := Ctrl5 and (not HighPass_Filter_Enable);
      Write (This, CTRL_REG5, Ctrl5);
   end Disable_High_Pass_Filter;

   ----------------------------
   -- Enable_Low_Pass_Filter --
   ----------------------------

   procedure Enable_Low_Pass_Filter (This : in out Three_Axis_Gyroscope) is
      Ctrl5 : UInt8;
   begin
      Read (This, CTRL_REG5, Ctrl5);
      Ctrl5 := Ctrl5 or LowPass_Filter_Enable;
      Write (This, CTRL_REG5, Ctrl5);
   end Enable_Low_Pass_Filter;

   -----------------------------
   -- Disable_Low_Pass_Filter --
   -----------------------------

   procedure Disable_Low_Pass_Filter (This : in out Three_Axis_Gyroscope) is
      Ctrl5 : UInt8;
   begin
      Read (This, CTRL_REG5, Ctrl5);
      --  clear HPen bit
      Ctrl5 := Ctrl5 and (not LowPass_Filter_Enable);
      Write (This, CTRL_REG5, Ctrl5);
   end Disable_Low_Pass_Filter;

   ---------------------
   -- Reference_Value --
   ---------------------

   function Reference_Value (This : Three_Axis_Gyroscope) return UInt8 is
      Result : UInt8;
   begin
      Read (This, Reference, Result);
      return Result;
   end Reference_Value;

   -------------------
   -- Set_Reference --
   -------------------

   procedure Set_Reference (This : in out Three_Axis_Gyroscope; Value : UInt8) is
   begin
      Write (This, Reference, Value);
   end Set_Reference;

   -----------------
   -- Data_Status --
   -----------------

   function Data_Status (This : Three_Axis_Gyroscope) return Gyro_Data_Status is
      Result : UInt8;
      function As_Gyro_Data_Status is
        new Ada.Unchecked_Conversion (Source => UInt8,
                                      Target => Gyro_Data_Status);
   begin
      Read (This, Status, Result);
      return As_Gyro_Data_Status (Result);
   end Data_Status;

   ---------------
   -- Device_Id --
   ---------------

   function Device_Id (This : Three_Axis_Gyroscope) return UInt8 is
      Result : UInt8;
   begin
      Read (This, Who_Am_I, Result);
      return Result;
   end Device_Id;

   -----------------
   -- Temperature --
   -----------------

   function Temperature (This : Three_Axis_Gyroscope) return UInt8 is
      Result : UInt8;
   begin
      Read (This, OUT_Temp, Result);
      return Result;
   end Temperature;

   ------------
   -- Reboot --
   ------------

   procedure Reboot (This : Three_Axis_Gyroscope) is
      Ctrl5 : UInt8;
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
      Ctrl4  : UInt8;
      Result : Float;
      Fullscale_Selection : UInt8;
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

      Ctrl4 : UInt8;

      UInt8s_To_Read : constant Integer := 6;  -- ie, three 2-UInt8 integers
      --  the number of UInt8s in an Angle_Rates record object

      Received : SPI_Data_8b (1 .. UInt8s_To_Read);

   begin
      Read (This, CTRL_REG4, Ctrl4);

      Read_UInt8s (This, OUT_X_L, Received, UInt8s_To_Read);
      --  The above has the effect of separate reads, as follows:
      --    Read (This, OUT_X_L, Received (1));
      --    Read (This, OUT_X_H, Received (2));
      --    Read (This, OUT_Y_L, Received (3));
      --    Read (This, OUT_Y_H, Received (4));
      --    Read (This, OUT_Z_L, Received (5));
      --    Read (This, OUT_Z_H, Received (6));

      if (Ctrl4 and Endian_Selection_Mask) = L3GD20_Big_Endian'Enum_Rep then
         Swap2 (Received (1)'Address);
         Swap2 (Received (3)'Address);
         Swap2 (Received (5)'Address);
      end if;

      Rates.X := As_Angle_Rate_Pointer (Received (1)'Address).all;
      Rates.Y := As_Angle_Rate_Pointer (Received (3)'Address).all;
      Rates.Z := As_Angle_Rate_Pointer (Received (5)'Address).all;
   end Get_Raw_Angle_Rates;

   --------------------------
   -- Configure_Interrupt1 --
   --------------------------

   procedure Configure_Interrupt1
     (This           : in out Three_Axis_Gyroscope;
      Triggers       : Threshold_Event_List;
      Latched        : Boolean := False;
      Active_Edge    : Interrupt1_Active_Edge := L3GD20_Interrupt1_High_Edge;
      Combine_Events : Boolean := False;
      Sample_Count   : Sample_Counter := 0)
   is
      Config : UInt8;
      Ctrl3  : UInt8;
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
      Ctrl3 : UInt8;
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
      Ctrl3 : UInt8;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 or INT1_Interrupt_Enable;
      Write (This, CTRL_REG3, Ctrl3);
   end Enable_Interrupt1;

   ------------------------
   -- Disable_Interrupt1 --
   ------------------------

   procedure Disable_Interrupt1 (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : UInt8;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not INT1_Interrupt_Enable);
      Write (This, CTRL_REG3, Ctrl3);
   end Disable_Interrupt1;

   -----------------------
   -- Interrupt1_Status --
   -----------------------

   function Interrupt1_Source (This : Three_Axis_Gyroscope) return Interrupt1_Sources is
      Result : UInt8;
      function As_Interrupt_Source is new Ada.Unchecked_Conversion
        (Source => UInt8, Target => Interrupt1_Sources);
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
      Write (This, INT1_Duration, UInt8 (Value) or Wait_Bit);
   end Set_Duration_Counter;

   ------------------
   -- Enable_Event --
   ------------------

   procedure Enable_Event
     (This  : in out Three_Axis_Gyroscope;
      Event : Axes_Interrupts)
   is
      Config : UInt8;
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
      Config : UInt8;
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
            Write (This, INT1_TSH_ZL, UInt8 (Value));
            Write (This, INT1_TSH_ZH, UInt8 (Shift_Right (Value, 8)));

         when Y_High_Interrupt | Y_Low_Interrupt =>
            Write (This, INT1_TSH_YL, UInt8 (Value));
            Write (This, INT1_TSH_YH, UInt8 (Shift_Right (Value, 8)));

         when X_High_Interrupt | X_Low_Interrupt =>
            Write (This, INT1_TSH_XL, UInt8 (Value));
            Write (This, INT1_TSH_XH, UInt8 (Shift_Right (Value, 8)));
      end case;
   end Set_Threshold;

   -------------------
   -- Set_FIFO_Mode --
   -------------------

   procedure Set_FIFO_Mode
     (This : in out Three_Axis_Gyroscope;
      Mode : FIFO_Modes)
   is
      FIFO  : UInt8;
   begin
      Read (This, FIFO_CTRL, FIFO);
      FIFO := FIFO and (not FIFO_Mode_Bits);  -- clear the current bits
      FIFO := FIFO or Mode'Enum_Rep;
      Write (This, FIFO_CTRL, FIFO);
   end Set_FIFO_Mode;

   -----------------
   -- Enable_FIFO --
   -----------------

   procedure Enable_FIFO (This : in out Three_Axis_Gyroscope) is
      Ctrl5 : UInt8;
   begin
      Read (This, CTRL_REG5, Ctrl5);
      Ctrl5 := Ctrl5 or FIFO_Enable_Bit;
      Write (This, CTRL_REG5, Ctrl5);
   end Enable_FIFO;

   ------------------
   -- Disable_FIFO --
   ------------------

   procedure Disable_FIFO (This : in out Three_Axis_Gyroscope) is
      Ctrl5 : UInt8;
   begin
      Read (This, CTRL_REG5, Ctrl5);
      Ctrl5 := Ctrl5 and (not FIFO_Enable_Bit);
      Write (This, CTRL_REG5, Ctrl5);
   end Disable_FIFO;

   ------------------------
   -- Set_FIFO_Watermark --
   ------------------------

   procedure Set_FIFO_Watermark
     (This  : in out Three_Axis_Gyroscope;
      Level : FIFO_Level)
   is
      Value : UInt8;
   begin
      Read (This, FIFO_CTRL, Value);
      Value := Value and (not Watermark_Threshold_Bits); -- clear the bits
      Value := Value or UInt8 (Level);
      Write (This, FIFO_CTRL, Value);
   end Set_FIFO_Watermark;

   ------------------------------
   -- Get_Raw_Angle_Rates_FIFO --
   ------------------------------

   procedure Get_Raw_Angle_Rates_FIFO
     (This   : in out Three_Axis_Gyroscope;
      Buffer : out Angle_Rates_FIFO_Buffer)
   is
      Ctrl4 : UInt8;

      Angle_Rate_Size : constant Integer := 6;  -- UInt8s
      UInt8s_To_Read   : constant Integer := Buffer'Length * Angle_Rate_Size;
      Received        : SPI_Data_8b (0 .. UInt8s_To_Read - 1)
        with Alignment => 2;
   begin
      Read (This, CTRL_REG4, Ctrl4);

      Read_UInt8s (This, OUT_X_L, Received, UInt8s_To_Read);

      if (Ctrl4 and Endian_Selection_Mask) = L3GD20_Big_Endian'Enum_Rep then
         declare
            J : Integer := 0;
         begin
            for K in Received'First .. Received'Last / 2 loop
               Swap2 (Received (J)'Address);
               J := J + 2;
            end loop;
         end;
      end if;

      declare
         J : Integer := 0;
      begin
         for K in Buffer'Range loop
            Buffer (K).X := As_Angle_Rate_Pointer (Received (J)'Address).all;
            J := J + 2;
            Buffer (K).Y := As_Angle_Rate_Pointer (Received (J)'Address).all;
            J := J + 2;
            Buffer (K).Z := As_Angle_Rate_Pointer (Received (J)'Address).all;
            J := J + 2;
         end loop;
      end;
   end Get_Raw_Angle_Rates_FIFO;

   ------------------------
   -- Current_FIFO_Depth --
   ------------------------

   function Current_FIFO_Depth (This : Three_Axis_Gyroscope) return FIFO_Level is
      Result : UInt8;
   begin
      Read (This, FIFO_SRC, Result);
      Result := Result and FIFO_Depth_Bits;
      return FIFO_Level (Result);
   end Current_FIFO_Depth;

   --------------------------
   -- FIFO_Below_Watermark --
   --------------------------

   function FIFO_Below_Watermark (This : Three_Axis_Gyroscope) return Boolean is
      Result : UInt8;
   begin
      Read (This, FIFO_SRC, Result);
      Result := Result and Watermark_Status_Bit;
      return Result = 0;
   end FIFO_Below_Watermark;

   ----------------
   -- FIFO_Empty --
   ----------------

   function FIFO_Empty (This : Three_Axis_Gyroscope) return Boolean is
      Result : UInt8;
   begin
      Read (This, FIFO_SRC, Result);
      Result := Result and FIFO_Empty_Bit;
      return Result = FIFO_Empty_Bit;
   end FIFO_Empty;

   ------------------
   -- FIFO_Overrun --
   ------------------

   function FIFO_Overrun (This : Three_Axis_Gyroscope) return Boolean is
      Result : UInt8;
   begin
      Read (This, FIFO_SRC, Result);
      Result := Result and FIFO_Overrun_Bit;
      return Result = FIFO_Overrun_Bit;
   end FIFO_Overrun;

   ---------------------------------
   -- Enable_Data_Ready_Interrupt --
   ---------------------------------

   procedure Enable_Data_Ready_Interrupt (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : UInt8;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 or INT2_Data_Ready_Interrupt_Enable;
      Write (This, CTRL_REG3, Ctrl3);
   end Enable_Data_Ready_Interrupt;

   ----------------------------------
   -- Disable_Data_Ready_Interrupt --
   ----------------------------------

   procedure Disable_Data_Ready_Interrupt (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : UInt8;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not INT2_Data_Ready_Interrupt_Enable);
      Write (This, CTRL_REG3, Ctrl3);
   end Disable_Data_Ready_Interrupt;

   -------------------------------------
   -- Enable_FIFO_Watermark_Interrupt --
   -------------------------------------

   procedure Enable_FIFO_Watermark_Interrupt (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : UInt8;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 or INT2_Watermark_Interrupt_Enable;
      Write (This, CTRL_REG3, Ctrl3);
   end Enable_FIFO_Watermark_Interrupt;

   -----------------------------
   -- Disable_Int1_Interrupts --
   -----------------------------

   procedure Disable_Int1_Interrupts (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : UInt8;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and 2#0001_1111#;
      Write (This, CTRL_REG3, Ctrl3);
   end Disable_Int1_Interrupts;

   -----------------------------
   -- Disable_Int2_Interrupts --
   -----------------------------

   procedure Disable_Int2_Interrupts (This : in out Three_Axis_Gyroscope) is
      Ctrl3 : UInt8;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and 2#1111_0000#;
      Write (This, CTRL_REG3, Ctrl3);
   end Disable_Int2_Interrupts;

   --------------------------------------
   -- Disable_FIFO_Watermark_Interrupt --
   --------------------------------------

   procedure Disable_FIFO_Watermark_Interrupt
     (This : in out Three_Axis_Gyroscope)
   is
      Ctrl3 : UInt8;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not INT2_Watermark_Interrupt_Enable);
      Write (This, CTRL_REG3, Ctrl3);
   end Disable_FIFO_Watermark_Interrupt;

   -----------------------------------
   -- Enable_FIFO_Overrun_Interrupt --
   -----------------------------------

   procedure Enable_FIFO_Overrun_Interrupt
     (This : in out Three_Axis_Gyroscope)
   is
      Ctrl3 : UInt8;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 or INT2_Overrun_Interrupt_Enable;
      Write (This, CTRL_REG3, Ctrl3);
   end Enable_FIFO_Overrun_Interrupt;

   ------------------------------------
   -- Disable_FIFO_Overrun_Interrupt --
   ------------------------------------

   procedure Disable_FIFO_Overrun_Interrupt
     (This : in out Three_Axis_Gyroscope)
   is
      Ctrl3 : UInt8;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not INT2_Overrun_Interrupt_Enable);
      Write (This, CTRL_REG3, Ctrl3);
   end Disable_FIFO_Overrun_Interrupt;

   ---------------------------------
   -- Enable_FIFO_Empty_Interrupt --
   ---------------------------------

   procedure Enable_FIFO_Empty_Interrupt
     (This : in out Three_Axis_Gyroscope)
   is
      Ctrl3 : UInt8;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 or INT2_FIFO_Empty_Interrupt_Enable;
      Write (This, CTRL_REG3, Ctrl3);
   end Enable_FIFO_Empty_Interrupt;

   ----------------------------------
   -- Disable_FIFO_Empty_Interrupt --
   ----------------------------------

   procedure Disable_FIFO_Empty_Interrupt
     (This : in out Three_Axis_Gyroscope)
   is
      Ctrl3 : UInt8;
   begin
      Read (This, CTRL_REG3, Ctrl3);
      Ctrl3 := Ctrl3 and (not INT2_FIFO_Empty_Interrupt_Enable);
      Write (This, CTRL_REG3, Ctrl3);
   end Disable_FIFO_Empty_Interrupt;

   -----------
   -- Swap2 --
   -----------

   procedure Swap2 (Location : System.Address) is
      X : UInt16;
      for X'Address use Location;
      function Bswap_16 (X : UInt16) return UInt16;
      pragma Import (Intrinsic, Bswap_16, "__builtin_bswap16");
   begin
      X := Bswap_16 (X);
   end Swap2;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Three_Axis_Gyroscope) is
   begin
      --  per the Datasheet, Table 17, pg 29
      Write (This, CTRL_REG1, 2#0000_0111#);
      Write (This, CTRL_REG2, 0);
      Write (This, CTRL_REG3, 0);
      Write (This, CTRL_REG4, 0);
      Write (This, CTRL_REG5, 0);
      Write (This, Reference, 0);
      Write (This, FIFO_CTRL, 0);
      Write (This, INT1_CFG, 0);
      Write (This, INT1_TSH_ZL, 0);
      Write (This, INT1_TSH_ZH, 0);
      Write (This, INT1_TSH_YL, 0);
      Write (This, INT1_TSH_YH, 0);
      Write (This, INT1_TSH_XL, 0);
      Write (This, INT1_TSH_XH, 0);
      Write (This, INT1_Duration, 0);
   end Reset;

end L3GD20;

