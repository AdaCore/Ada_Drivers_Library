------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2017, 2020, AdaCore                        --
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
--  This file is based on X-CUBE-53L0A1 STM32Cube expansion, with some      --
--  input from Crazyflie source.                                            --
--                                                                          --
--   COPYRIGHT(c) 2016 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with HAL;        use HAL;

package body VL53L0X is

   Start_Overhead       : constant := 1910;
   End_Overhead         : constant := 960;
   Msrc_Overhead        : constant := 660;
   Tcc_Overhead         : constant := 590;
   Dss_Overhead         : constant := 690;
   Pre_Range_Overhead   : constant := 660;
   Final_Range_Overhead : constant := 550;

   function Decode_Timeout (Encoded : UInt16) return UInt32;
   function Encode_Timeout (Timeout : UInt32) return UInt16;

   function To_Timeout_Microseconds
     (Timeout_Period_Mclks : UInt32;
      VCSel_Period_Pclks   : UInt8) return UInt32;

   function To_Timeout_Mclks
     (Timeout_Period_us  : UInt32;
      VCSel_Period_Pclks : UInt8) return UInt32;

   procedure Perform_Single_Ref_Calibration
     (This     : VL53L0X_Ranging_Sensor;
      VHV_Init : UInt8;
      Status   : out Boolean);

   ----------------------------
   -- Get_GPIO_Functionality --
   ----------------------------

   function Get_GPIO_Functionality (This : VL53L0X_Ranging_Sensor)
                                   return VL53L0X_GPIO_Functionality
   is
      Data   : UInt8;
      Status : Boolean;
      Result : VL53L0X_GPIO_Functionality := No_Interrupt;
   begin
      Read (This, REG_SYSTEM_INTERRUPT_CONFIG_GPIO, Data, Status);
      if Status and then Data in 1 .. 4 then
         case Data is
            when 1 => Result := Level_Low;
            when 2 => Result := Level_High;
            when 3 => Result := Out_Of_Window;
            when 4 => Result := New_Sample_Ready;
            when others => null;
         end case;
      end if;
      return Result;
   end Get_GPIO_Functionality;

   ---------------
   -- I2C_Write --
   ---------------

   procedure I2C_Write
     (This   : VL53L0X_Ranging_Sensor;
      Data   : HAL.UInt8_Array;
      Status : out Boolean)
   is
      use type HAL.I2C.I2C_Status;
      Ret : HAL.I2C.I2C_Status;
   begin
      HAL.I2C.Master_Transmit
        (This    => This.Port.all,
         Addr    => This.I2C_Address,
         Data    => Data,
         Status  => Ret);
      Status := Ret = HAL.I2C.Ok;
   end I2C_Write;

   --------------
   -- I2C_Read --
   --------------

   procedure I2C_Read
     (This   : VL53L0X_Ranging_Sensor;
      Data   : out HAL.UInt8_Array;
      Status : out Boolean)
   is
      use type HAL.I2C.I2C_Status;
      Ret : HAL.I2C.I2C_Status;
   begin
      HAL.I2C.Master_Receive
        (This    => This.Port.all,
         Addr    => This.I2C_Address,
         Data    => Data,
         Status  => Ret);
      Status := Ret = HAL.I2C.Ok;
   end I2C_Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (This   : VL53L0X_Ranging_Sensor;
      Index  : HAL.UInt8;
      Data   : HAL.UInt8_Array;
      Status : out Boolean)
   is
      Buf : constant HAL.UInt8_Array := (1 => Index) & Data;
   begin
      I2C_Write (This, Buf, Status);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (This   : VL53L0X_Ranging_Sensor;
      Index  : HAL.UInt8;
      Data   : HAL.UInt8;
      Status : out Boolean)
   is
   begin
      I2C_Write (This, (Index, Data), Status);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (This   : VL53L0X_Ranging_Sensor;
      Index  : HAL.UInt8;
      Data   : HAL.UInt16;
      Status : out Boolean)
   is
   begin
      I2C_Write
        (This,
         (Index,
          HAL.UInt8 (Shift_Right (Data, 8)),
          HAL.UInt8 (Data and 16#FF#)),
         Status);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (This   : VL53L0X_Ranging_Sensor;
      Index  : HAL.UInt8;
      Data   : HAL.UInt32;
      Status : out Boolean)
   is
   begin
      I2C_Write
        (This,
         (Index,
          HAL.UInt8 (Shift_Right (Data, 24)),
          HAL.UInt8 (Shift_Right (Data, 16) and 16#FF#),
          HAL.UInt8 (Shift_Right (Data, 8) and 16#FF#),
          HAL.UInt8 (Data and 16#FF#)),
         Status);
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (This   : VL53L0X_Ranging_Sensor;
      Index  : HAL.UInt8;
      Data   : out HAL.UInt8_Array;
      Status : out Boolean)
   is
   begin
      I2C_Write (This, (1 => Index), Status);
      if Status then
         I2C_Read (This, Data, Status);
      end if;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (This   : VL53L0X_Ranging_Sensor;
      Index  : HAL.UInt8;
      Data   : out HAL.UInt8;
      Status : out Boolean)
   is
      Buf : UInt8_Array (1 .. 1);
   begin
      I2C_Write (This, (1 => Index), Status);
      if Status then
         I2C_Read (This, Buf, Status);
         Data := Buf (1);
      end if;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (This   : VL53L0X_Ranging_Sensor;
      Index  : HAL.UInt8;
      Data   : out HAL.UInt16;
      Status : out Boolean)
   is
      Buf : UInt8_Array (1 .. 2);
   begin
      I2C_Write (This, (1 => Index), Status);
      if Status then
         I2C_Read (This, Buf, Status);
         Data := Shift_Left (UInt16 (Buf (1)), 8) or UInt16 (Buf (2));
      end if;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (This   : VL53L0X_Ranging_Sensor;
      Index  : HAL.UInt8;
      Data   : out HAL.UInt32;
      Status : out Boolean)
   is
      Buf : UInt8_Array (1 .. 4);
   begin
      I2C_Write (This, (1 => Index), Status);
      if Status then
         I2C_Read (This, Buf, Status);
         Data := Shift_Left (UInt32 (Buf (1)), 24) or
           Shift_Left (UInt32 (Buf (2)), 16) or
           Shift_Left (UInt32 (Buf (3)), 8) or
           UInt32 (Buf (4));
      end if;
   end Read;

   --------------------
   -- Decode_Timeout --
   --------------------

   function Decode_Timeout (Encoded : UInt16) return UInt32
   is
      LSByte : constant UInt32 := UInt32 (Encoded and 16#00_FF#);
      MSByte : constant Natural :=
                 Natural (Shift_Right (Encoded and 16#FF_00#, 8));
   begin
      return LSByte * 2 ** MSByte + 1;
   end Decode_Timeout;

   --------------------
   -- Encode_Timeout --
   --------------------

   function Encode_Timeout (Timeout : UInt32) return UInt16
   is
      LSByte : UInt32;
      MSByte : UInt32 := 0;
   begin
      LSByte := Timeout - 1;
      while (LSByte and 16#FFFF_FF00#) > 0 loop
         LSByte := Shift_Right (LSByte, 1);
         MSByte := MSByte + 1;
      end loop;

      return UInt16 (Shift_Left (MSByte, 8) or LSByte);
   end Encode_Timeout;

   ----------------------
   -- To_Timeout_Mclks --
   ----------------------

   function To_Timeout_Mclks
     (Timeout_Period_us  : UInt32;
      VCSel_Period_Pclks : UInt8) return UInt32
   is
      PLL_Period_Ps        : constant := 1655;
      Macro_Period_Vclks   : constant := 2304;
      Macro_Period_Ps      : UInt32;
      Macro_Period_Ns      : UInt32;

   begin
      Macro_Period_Ps :=
        UInt32 (VCSel_Period_Pclks) * PLL_Period_Ps * Macro_Period_Vclks;
      Macro_Period_Ns := (Macro_Period_Ps + 500) / 1000;

      return
        (Timeout_Period_us * 1000 + Macro_Period_Ns / 2) / Macro_Period_Ns;
   end To_Timeout_Mclks;

   -----------------------------
   -- To_Timeout_Microseconds --
   -----------------------------

   function To_Timeout_Microseconds
     (Timeout_Period_Mclks : UInt32;
      VCSel_Period_Pclks   : UInt8) return UInt32
   is
      PLL_Period_Ps        : constant := 1655;
      Macro_Period_Vclks   : constant := 2304;
      Macro_Period_Ps      : UInt32;
      Macro_Period_Ns      : UInt32;

   begin
      Macro_Period_Ps :=
        UInt32 (VCSel_Period_Pclks) * PLL_Period_Ps * Macro_Period_Vclks;
      Macro_Period_Ns := (Macro_Period_Ps + 500) / 1000;

      return (Timeout_Period_Mclks * Macro_Period_Ns + 500) / 1000;
   end To_Timeout_Microseconds;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : in out VL53L0X_Ranging_Sensor)
   is
   begin
      This.I2C_Address := 16#52#;
   end Initialize;

   -------------
   -- Read_Id --
   -------------

   function Read_Id (This : VL53L0X_Ranging_Sensor) return HAL.UInt16
   is
      Ret    : UInt16;
      Status : Boolean;
   begin
      Read (This, REG_IDENTIFICATION_MODEL_ID, Ret, Status);

      if not Status then
         return 0;
      else
         return Ret;
      end if;
   end Read_Id;

   -------------------
   -- Read_Revision --
   -------------------

   function Read_Revision (This : VL53L0X_Ranging_Sensor) return HAL.UInt8
   is
      Ret    : UInt8;
      Status : Boolean;
   begin
      Read (This, REG_IDENTIFICATION_REVISION_ID, Ret, Status);

      if not Status then
         return 0;
      else
         return Ret;
      end if;
   end Read_Revision;

   ------------------------
   -- Set_Device_Address --
   ------------------------

   procedure Set_Device_Address
     (This   : in out VL53L0X_Ranging_Sensor;
      Addr   : HAL.I2C.I2C_Address;
      Status : out Boolean)
   is
   begin
      Write (This, REG_I2C_SLAVE_DEVICE_ADDRESS, UInt8 (Addr / 2), Status);
      if Status then
         This.I2C_Address := Addr;
      end if;
   end Set_Device_Address;

   ---------------
   -- Data_Init --
   ---------------

   procedure Data_Init
     (This   : in out VL53L0X_Ranging_Sensor;
      Status : out Boolean)
   is
      Regval : UInt8;
   begin
      --  Set I2C Standard mode
      Write (This, 16#88#, UInt8'(16#00#), Status);

      if not Status then
         return;
      end if;

--        This.Device_Specific_Params.Read_Data_From_Device_Done := False;
--
--        --  Set default static parameters:
--        --  set first temporary value 9.44MHz * 65536 = 618_660
--        This.Device_Specific_Params.Osc_Frequency := 618_660;
--
--        --  Set default cross talk compenstation rate to 0
--        This.Device_Params.X_Talk_Compensation_Rate_Mcps := 0.0;
--
--        This.Device_Params.Device_Mode := Single_Ranging;
--        This.Device_Params.Histogram_Mode := Disabled;

      --  TODO: Sigma estimator variable
      if Status then
         Write (This, 16#80#, UInt8'(16#01#), Status);
      end if;
      if Status then
         Write (This, 16#FF#, UInt8'(16#01#), Status);
      end if;
      if Status then
         Write (This, 16#00#, UInt8'(16#00#), Status);
      end if;
      if Status then
         Read (This, 16#91#, This.Stop_Variable, Status);
      end if;
      if Status then
         Write (This, 16#00#, UInt8'(16#01#), Status);
      end if;
      if Status then
         Write (This, 16#FF#, UInt8'(16#00#), Status);
      end if;
      if Status then
         Write (This, 16#80#, UInt8'(16#00#), Status);
      end if;

      --  disable SIGNAL_RATE_MSRC (bit 1) and SIGNAL_RATE_PRE_RANGE (bit 4)
      --  limit checks
      if Status then
         Read (This, REG_MSRC_CONFIG_CONTROL, Regval, Status);
      end if;

      if Status then
         Write (This, REG_MSRC_CONFIG_CONTROL, Regval or 16#12#, Status);
      end if;

      if Status then
         --  Set final range signal rate limit to 0.25 MCPS
         Status := Set_Signal_Rate_Limit (This, 0.25);
      end if;

      if Status then
         Write (This, REG_SYSTEM_SEQUENCE_CONFIG, UInt8'(16#FF#), Status);
      end if;
   end Data_Init;

   -----------------
   -- Static_Init --
   -----------------

   procedure Static_Init
     (This          : in out VL53L0X_Ranging_Sensor;
      GPIO_Function : VL53L0X_GPIO_Functionality;
      Status        : out Boolean)
   is
      type SPAD_Map is array (UInt8 range 1 .. 48) of Boolean
        with Pack, Size => 48;
      subtype SPAD_Map_Bytes is UInt8_Array (1 .. 6);
      function To_Map is new Ada.Unchecked_Conversion
        (SPAD_Map_Bytes, SPAD_Map);
      function To_Bytes is new Ada.Unchecked_Conversion
        (SPAD_Map, SPAD_Map_Bytes);

      SPAD_Count         : UInt8;
      SPAD_Is_Aperture   : Boolean;
      Ref_SPAD_Map_Bytes : SPAD_Map_Bytes;
      Ref_SPAD_Map       : SPAD_Map;
      First_SPAD         : UInt8;
      SPADS_Enabled      : UInt8;
      Timing_Budget      : UInt32;

   begin
      Status := SPAD_Info (This, SPAD_Count, SPAD_Is_Aperture);

      if not Status then
         return;
      end if;

      Read
        (This, REG_GLOBAL_CONFIG_SPAD_ENABLES_REF_0,
         Ref_SPAD_Map_Bytes, Status);
      Ref_SPAD_Map := To_Map (Ref_SPAD_Map_Bytes);

      --  Set reference spads
      if Status then
         Write (This, 16#FF#, UInt8'(16#01#), Status);
      end if;
      if Status then
         Write (This, REG_DYNAMIC_SPAD_REF_EN_START_OFFSET,
                UInt8'(16#00#), Status);
      end if;
      if Status then
         Write (This, REG_DYNAMIC_SPAD_NUM_REQUESTED_REF_SPAD,
                UInt8'(16#2C#), Status);
      end if;
      if Status then
         Write (This, 16#FF#, UInt8'(16#00#), Status);
      end if;
      if Status then
         Write (This, REG_GLOBAL_CONFIG_REF_EN_START_SELECT,
                UInt8'(16#B4#), Status);
      end if;

      if Status then
         if SPAD_Is_Aperture then
            First_SPAD := 13;
         else
            First_SPAD := 1;
         end if;

         SPADS_Enabled := 0;

         for J in UInt8 range 1 .. 48 loop

            if J < First_SPAD or else SPADS_Enabled = SPAD_Count then
               --  This bit is lower than the first one that should be enabled,
               --  or SPAD_Count bits have already been enabled, so zero this
               --  bit
               Ref_SPAD_Map (J) := False;

            elsif Ref_SPAD_Map (J) then
               SPADS_Enabled := SPADS_Enabled + 1;
            end if;
         end loop;
      end if;

      if Status then
         Ref_SPAD_Map_Bytes := To_Bytes (Ref_SPAD_Map);
         Write (This, REG_GLOBAL_CONFIG_SPAD_ENABLES_REF_0,
                Ref_SPAD_Map_Bytes, Status);
      end if;

      --  Load tuning Settings
      --  default tuning settings from vl53l0x_tuning.h
      if Status then
         Write (This, 16#FF#, UInt8'(16#01#), Status);
         Write (This, 16#00#, UInt8'(16#00#), Status);

         Write (This, 16#FF#, UInt8'(16#00#), Status);
         Write (This, 16#09#, UInt8'(16#00#), Status);
         Write (This, 16#10#, UInt8'(16#00#), Status);
         Write (This, 16#11#, UInt8'(16#00#), Status);

         Write (This, 16#24#, UInt8'(16#01#), Status);
         Write (This, 16#25#, UInt8'(16#FF#), Status);
         Write (This, 16#75#, UInt8'(16#00#), Status);

         Write (This, 16#FF#, UInt8'(16#01#), Status);
         Write (This, 16#4E#, UInt8'(16#2C#), Status);
         Write (This, 16#48#, UInt8'(16#00#), Status);
         Write (This, 16#30#, UInt8'(16#20#), Status);

         Write (This, 16#FF#, UInt8'(16#00#), Status);
         Write (This, 16#30#, UInt8'(16#09#), Status);
         Write (This, 16#54#, UInt8'(16#00#), Status);
         Write (This, 16#31#, UInt8'(16#04#), Status);
         Write (This, 16#32#, UInt8'(16#03#), Status);
         Write (This, 16#40#, UInt8'(16#83#), Status);
         Write (This, 16#46#, UInt8'(16#25#), Status);
         Write (This, 16#60#, UInt8'(16#00#), Status);
         Write (This, 16#27#, UInt8'(16#00#), Status);
         Write (This, 16#50#, UInt8'(16#06#), Status);
         Write (This, 16#51#, UInt8'(16#00#), Status);
         Write (This, 16#52#, UInt8'(16#96#), Status);
         Write (This, 16#56#, UInt8'(16#08#), Status);
         Write (This, 16#57#, UInt8'(16#30#), Status);
         Write (This, 16#61#, UInt8'(16#00#), Status);
         Write (This, 16#62#, UInt8'(16#00#), Status);
         Write (This, 16#64#, UInt8'(16#00#), Status);
         Write (This, 16#65#, UInt8'(16#00#), Status);
         Write (This, 16#66#, UInt8'(16#A0#), Status);

         Write (This, 16#FF#, UInt8'(16#01#), Status);
         Write (This, 16#22#, UInt8'(16#32#), Status);
         Write (This, 16#47#, UInt8'(16#14#), Status);
         Write (This, 16#49#, UInt8'(16#FF#), Status);
         Write (This, 16#4A#, UInt8'(16#00#), Status);

         Write (This, 16#FF#, UInt8'(16#00#), Status);
         Write (This, 16#7A#, UInt8'(16#0A#), Status);
         Write (This, 16#7B#, UInt8'(16#00#), Status);
         Write (This, 16#78#, UInt8'(16#21#), Status);

         Write (This, 16#FF#, UInt8'(16#01#), Status);
         Write (This, 16#23#, UInt8'(16#34#), Status);
         Write (This, 16#42#, UInt8'(16#00#), Status);
         Write (This, 16#44#, UInt8'(16#FF#), Status);
         Write (This, 16#45#, UInt8'(16#26#), Status);
         Write (This, 16#46#, UInt8'(16#05#), Status);
         Write (This, 16#40#, UInt8'(16#40#), Status);
         Write (This, 16#0E#, UInt8'(16#06#), Status);
         Write (This, 16#20#, UInt8'(16#1A#), Status);
         Write (This, 16#43#, UInt8'(16#40#), Status);

         Write (This, 16#FF#, UInt8'(16#00#), Status);
         Write (This, 16#34#, UInt8'(16#03#), Status);
         Write (This, 16#35#, UInt8'(16#44#), Status);

         Write (This, 16#FF#, UInt8'(16#01#), Status);
         Write (This, 16#31#, UInt8'(16#04#), Status);
         Write (This, 16#4B#, UInt8'(16#09#), Status);
         Write (This, 16#4C#, UInt8'(16#05#), Status);
         Write (This, 16#4D#, UInt8'(16#04#), Status);

         Write (This, 16#FF#, UInt8'(16#00#), Status);
         Write (This, 16#44#, UInt8'(16#00#), Status);
         Write (This, 16#45#, UInt8'(16#20#), Status);
         Write (This, 16#47#, UInt8'(16#08#), Status);
         Write (This, 16#48#, UInt8'(16#28#), Status);
         Write (This, 16#67#, UInt8'(16#00#), Status);
         Write (This, 16#70#, UInt8'(16#04#), Status);
         Write (This, 16#71#, UInt8'(16#01#), Status);
         Write (This, 16#72#, UInt8'(16#FE#), Status);
         Write (This, 16#76#, UInt8'(16#00#), Status);
         Write (This, 16#77#, UInt8'(16#00#), Status);

         Write (This, 16#FF#, UInt8'(16#01#), Status);
         Write (This, 16#0D#, UInt8'(16#01#), Status);

         Write (This, 16#FF#, UInt8'(16#00#), Status);
         Write (This, 16#80#, UInt8'(16#01#), Status);
         Write (This, 16#01#, UInt8'(16#F8#), Status);

         Write (This, 16#FF#, UInt8'(16#01#), Status);
         Write (This, 16#8E#, UInt8'(16#01#), Status);
         Write (This, 16#00#, UInt8'(16#01#), Status);
         Write (This, 16#FF#, UInt8'(16#00#), Status);
         Write (This, 16#80#, UInt8'(16#00#), Status);
      end if;

      Set_GPIO_Config (This, GPIO_Function, Polarity_High, Status);

      if Status then
         Timing_Budget := Measurement_Timing_Budget (This);

         --  Disable MSRC and TCC by default
         --  MSRC = Minimum Signal Rate Check
         --  TCC  = Target CenterCheck

         Write (This, REG_SYSTEM_SEQUENCE_CONFIG, UInt8'(16#E8#), Status);
      end if;

      --  Recalculate the timing Budget
      if Status then
         Set_Measurement_Timing_Budget (This, Timing_Budget, Status);
      end if;
   end Static_Init;

   ------------------------------------
   -- Perform_Single_Ref_Calibration --
   ------------------------------------

   procedure Perform_Single_Ref_Calibration
     (This     : VL53L0X_Ranging_Sensor;
      VHV_Init : UInt8;
      Status   : out Boolean)
   is
      Val    : UInt8;

   begin
      Write (This, REG_SYSRANGE_START, VHV_Init or 16#01#, Status);

      if not Status then
         return;
      end if;

      loop
         Read (This, REG_RESULT_INTERRUPT_STATUS, Val, Status);
         exit when not Status;
         exit when (Val and 16#07#) /= 0;
      end loop;

      if not Status then
         return;
      end if;

      Write (This, REG_SYSTEM_INTERRUPT_CLEAR, UInt8'(16#01#), Status);
      if not Status then
         return;
      end if;

      Write (This, REG_SYSRANGE_START, UInt8'(16#00#), Status);
   end Perform_Single_Ref_Calibration;

   -----------------------------
   -- Perform_Ref_Calibration --
   -----------------------------

   procedure Perform_Ref_Calibration
     (This   : in out VL53L0X_Ranging_Sensor;
      Status : out Boolean)
   is
   begin
      --  VHV calibration
      Write (This, REG_SYSTEM_SEQUENCE_CONFIG, UInt8'(16#01#), Status);

      if Status then
         Perform_Single_Ref_Calibration (This, 16#40#, Status);
      end if;

      --  Phase calibration
      if Status then
         Write (This, REG_SYSTEM_SEQUENCE_CONFIG, UInt8'(16#02#), Status);
      end if;

      if Status then
         Perform_Single_Ref_Calibration (This, 16#00#, Status);
      end if;

      --  Restore the sequence config
      if Status then
         Write (This, REG_SYSTEM_SEQUENCE_CONFIG, UInt8'(16#E8#), Status);
      end if;
   end Perform_Ref_Calibration;

   ----------------------
   -- Start_Continuous --
   ----------------------

   procedure Start_Continuous (This : VL53L0X.VL53L0X_Ranging_Sensor;
                               Period_Ms : HAL.UInt32;
                               Status : out Boolean) is
      --  From vl53l0xStartContinuous() in
      --  crazyflie-firmware/src/drivers/src/vl53l0x.c
   begin
      Write (This, 16#80#, UInt8'(16#01#), Status);
      if Status then
         Write (This, 16#ff#, UInt8'(16#01#), Status);
      end if;
      if Status then
         Write (This, 16#00#, UInt8'(16#00#), Status);
      end if;
      if Status then
         Write (This, 16#91#, UInt8'(This.Stop_Variable), Status);
      end if;
      if Status then
         Write (This, 16#00#, UInt8'(16#01#), Status);
      end if;
      if Status then
         Write (This, 16#ff#, UInt8'(16#00#), Status);
      end if;
      if Status then
         Write (This, 16#80#, UInt8'(16#00#), Status);
      end if;

      if not Status then
         return;
      end if;
      if Period_Ms /= 0 then
         --  continuous timed mode
         declare
            procedure Set_Inter_Measurement_Period_Milliseconds;
            --  The Crazyflie code indicates that this is a missing
            --  function that they've inlined.
            procedure Set_Inter_Measurement_Period_Milliseconds is
               Osc_Calibrate_Val : UInt16;
               Period : UInt32 := Period_Ms;
            begin
               Read (This, REG_OSC_CALIBRATE_VAL, Osc_Calibrate_Val, Status);
               if Status and then Osc_Calibrate_Val /= 0 then
                  Period := Period * UInt32 (Osc_Calibrate_Val);
               end if;
               if Status then
                  Write
                    (This, REG_SYSTEM_INTERMEASUREMENT_PERIOD, Period, Status);
               end if;
            end Set_Inter_Measurement_Period_Milliseconds;
         begin
            Set_Inter_Measurement_Period_Milliseconds;
            if Status then
               Write (This,
                      REG_SYSRANGE_START,
                      UInt8'(REG_SYSRANGE_MODE_TIMED),
                      Status);
            end if;
         end;
      else
         --  continuous back-to-back mode
         Write (This,
                REG_SYSRANGE_START,
                UInt8'(REG_SYSRANGE_MODE_BACKTOBACK),
                Status);
      end if;
   end Start_Continuous;

   ------------------------------------
   -- Start_Range_Single_Millimeters --
   ------------------------------------

   procedure Start_Range_Single_Millimeters
     (This   : VL53L0X_Ranging_Sensor;
      Status : out Boolean)
   is
      Val    : UInt8;
   begin
      Write (This, 16#80#, UInt8'(16#01#), Status);
      if Status then
         Write (This, 16#FF#, UInt8'(16#01#), Status);
      end if;
      if Status then
         Write (This, 16#00#, UInt8'(16#00#), Status);
      end if;
      if Status then
         Write (This, 16#91#, This.Stop_Variable, Status);
      end if;
      if Status then
         Write (This, 16#00#, UInt8'(16#01#), Status);
      end if;
      if Status then
         Write (This, 16#FF#, UInt8'(16#00#), Status);
      end if;
      if Status then
         Write (This, 16#80#, UInt8'(16#00#), Status);
      end if;

      if Status then
         Write (This, REG_SYSRANGE_START, UInt8'(16#01#), Status);
      end if;

      if not Status then
         return;
      end if;

      loop
         Read (This, REG_SYSRANGE_START, Val, Status);
         exit when not Status;
         exit when (Val and 16#01#) = 0;
      end loop;
   end Start_Range_Single_Millimeters;

   ---------------------------
   -- Range_Value_Available --
   ---------------------------

   function Range_Value_Available
     (This : VL53L0X_Ranging_Sensor) return Boolean
   is
      Status : Boolean;
      Val    : UInt8;
   begin
      Read (This, REG_RESULT_INTERRUPT_STATUS, Val, Status);
      return (Val and 16#07#) /= 0;
   end Range_Value_Available;

   ----------------------------
   -- Read_Range_Millimeters --
   ----------------------------

   function Read_Range_Millimeters
     (This : VL53L0X_Ranging_Sensor) return HAL.UInt16
   is
      Status : Boolean;
      Ret    : HAL.UInt16;
   begin
      Read (This, REG_RESULT_RANGE_STATUS + 10, Ret, Status);
      Write (This, REG_SYSTEM_INTERRUPT_CLEAR, UInt8'(16#01#), Status);

      return Ret;
   end Read_Range_Millimeters;

   -----------------------------------
   -- Read_Range_Single_Millimeters --
   -----------------------------------

   function Read_Range_Single_Millimeters
     (This : VL53L0X_Ranging_Sensor) return HAL.UInt16
   is
      Status : Boolean;
   begin
      Start_Range_Single_Millimeters (This, Status);
      if not Status then
         return 4000;
      end if;

      while not Range_Value_Available (This) loop
         This.Timing.Delay_Milliseconds (1);
      end loop;

      return Read_Range_Millimeters (This);
   end Read_Range_Single_Millimeters;

   ---------------------
   -- Set_GPIO_Config --
   ---------------------

   procedure Set_GPIO_Config
     (This          : in out VL53L0X_Ranging_Sensor;
      Functionality : VL53L0X_GPIO_Functionality;
      Polarity      : VL53L0X_Interrupt_Polarity;
      Status        : out Boolean)
   is
      Data   : UInt8;
      Tmp    : UInt8;
   begin
      case Functionality is
         when No_Interrupt =>
            Data := 0;
         when Level_Low =>
            Data := 1;
         when Level_High =>
            Data := 2;
         when Out_Of_Window =>
            Data := 3;
         when New_Sample_Ready =>
            Data := 4;
      end case;

      --  16#04#: interrupt on new measure ready
      Write (This, REG_SYSTEM_INTERRUPT_CONFIG_GPIO, Data, Status);

      --  Interrupt polarity
      if Status then
         case Polarity is
            when Polarity_Low =>
               Data := 16#10#;
            when Polarity_High =>
               Data := 16#00#;
         end case;

         Read (This, REG_GPIO_HV_MUX_ACTIVE_HIGH, Tmp, Status);
         Tmp := (Tmp and 16#EF#) or Data;
         Write (This, REG_GPIO_HV_MUX_ACTIVE_HIGH, Tmp, Status);
      end if;

      if Status then
         Clear_Interrupt_Mask (This);
      end if;
   end Set_GPIO_Config;

   --------------------------
   -- Clear_Interrupt_Mask --
   --------------------------

   procedure Clear_Interrupt_Mask
     (This : VL53L0X_Ranging_Sensor)
   is
      Status : Boolean;
--        Tmp    : UInt8;
   begin
--        for J in 1 .. 3 loop
         Write (This, REG_SYSTEM_INTERRUPT_CLEAR, UInt8'(16#01#), Status);
--           exit when not Status;
--           Write (This, REG_SYSTEM_INTERRUPT_CLEAR, UInt8'(16#00#), Status);
--           exit when not Status;
--           Read (This, REG_RESULT_INTERRUPT_STATUS, Tmp, Status);
--           exit when not Status;
--           exit when (Tmp and 16#07#) /= 0;
--        end loop;
   end Clear_Interrupt_Mask;

   ---------------------------
   -- Sequence_Step_Enabled --
   ---------------------------

   function Sequence_Step_Enabled
     (This : VL53L0X_Ranging_Sensor) return VL53L0x_Sequence_Step_Enabled
   is
      Sequence_Steps  : VL53L0x_Sequence_Step_Enabled;
      Sequence_Config : UInt8 := 0;
      Status          : Boolean;

      function Sequence_Step_Enabled
        (Step            : VL53L0x_Sequence_Step;
         Sequence_Config : UInt8) return Boolean;

      ---------------------------
      -- Sequence_Step_Enabled --
      ---------------------------

      function Sequence_Step_Enabled
        (Step            : VL53L0x_Sequence_Step;
         Sequence_Config : UInt8) return Boolean
      is
      begin
         case Step is
            when TCC =>
               return (Sequence_Config and 16#10#) /= 0;
            when DSS =>
               return (Sequence_Config and 16#08#) /= 0;
            when MSRC =>
               return (Sequence_Config and 16#04#) /= 0;
            when Pre_Range =>
               return (Sequence_Config and 16#40#) /= 0;
            when Final_Range =>
               return (Sequence_Config and 16#80#) /= 0;
         end case;
      end Sequence_Step_Enabled;

   begin
      Read (This, REG_SYSTEM_SEQUENCE_CONFIG, Sequence_Config, Status);

      if not Status then
         return (others => False);
      end if;

      for Step in Sequence_Steps'Range loop
         Sequence_Steps (Step) :=
           Sequence_Step_Enabled (Step, Sequence_Config);
      end loop;

      return Sequence_Steps;
   end Sequence_Step_Enabled;

   ---------------------------
   -- Sequence_Step_Timeout --
   ---------------------------

   function Sequence_Step_Timeout
     (This     : VL53L0X_Ranging_Sensor;
      Step     : VL53L0x_Sequence_Step;
      As_Mclks : Boolean := False) return UInt32
   is
      VCSel_Pulse_Period_Pclk : UInt8;
      Encoded_UInt8           : UInt8;
      Encoded_UInt16          : UInt16;
      Status                  : Boolean;
      Timeout_Mclks           : UInt32;
      Sequence_Steps          : VL53L0x_Sequence_Step_Enabled;

   begin
      case Step is
         when TCC | DSS | MSRC =>
            Read (This, REG_MSRC_CONFIG_TIMEOUT_MACROP,
                  Encoded_UInt8, Status);
            if Status then
               Timeout_Mclks := Decode_Timeout (UInt16 (Encoded_UInt8));
            end if;

            if As_Mclks then
               return Timeout_Mclks;
            else
               VCSel_Pulse_Period_Pclk :=
                 VCSel_Pulse_Period (This, Pre_Range);

               return To_Timeout_Microseconds
                 (Timeout_Mclks, VCSel_Pulse_Period_Pclk);
            end if;

         when Pre_Range =>
            Read (This, REG_PRE_RANGE_CONFIG_TIMEOUT_MACROP_HI,
                  Encoded_UInt16, Status);

            if Status then
               Timeout_Mclks := Decode_Timeout (Encoded_UInt16);
            end if;

            if As_Mclks then
               return Timeout_Mclks;
            else
               VCSel_Pulse_Period_Pclk :=
                 VCSel_Pulse_Period (This, Pre_Range);

               return To_Timeout_Microseconds
                 (Timeout_Mclks, VCSel_Pulse_Period_Pclk);
            end if;

         when Final_Range =>
            Sequence_Steps := Sequence_Step_Enabled (This);

            if Sequence_Steps (Pre_Range) then
               VCSel_Pulse_Period_Pclk :=
                 VCSel_Pulse_Period (This, Pre_Range);

               Read (This, REG_PRE_RANGE_CONFIG_TIMEOUT_MACROP_HI,
                     Encoded_UInt16, Status);

               if Status then
                  Timeout_Mclks := Decode_Timeout (Encoded_UInt16);
               end if;
            else
               Timeout_Mclks := 0;
            end if;

            VCSel_Pulse_Period_Pclk :=
              VCSel_Pulse_Period (This, Final_Range);

            Read (This, REG_FINAL_RANGE_CONFIG_TIMEOUT_MACROP_HI,
                  Encoded_UInt16, Status);

            Timeout_Mclks :=
              Decode_Timeout (Encoded_UInt16) - Timeout_Mclks;

            if As_Mclks then
               return Timeout_Mclks;
            else
               return To_Timeout_Microseconds
                 (Timeout_Mclks, VCSel_Pulse_Period_Pclk);
            end if;
      end case;
   end Sequence_Step_Timeout;

   -------------------------------
   -- Measurement_Timing_Budget --
   -------------------------------

   function Measurement_Timing_Budget
        (This : VL53L0X_Ranging_Sensor) return HAL.UInt32
   is
      Ret                  : UInt32;
      Pre_Range_Timeout    : UInt32;
      Final_Range_Timeout  : UInt32;
      Sequence_Steps       : VL53L0x_Sequence_Step_Enabled;
      Msrc_Dcc_Tcc_Timeout : UInt32;

   begin
      Ret := Start_Overhead + End_Overhead;

      Sequence_Steps := Sequence_Step_Enabled (This);

      if Sequence_Steps (TCC)
        or else Sequence_Steps (MSRC)
        or else Sequence_Steps (DSS)
      then
         Msrc_Dcc_Tcc_Timeout := Sequence_Step_Timeout (This, MSRC);
         if Sequence_Steps (TCC) then
            Ret := Ret + Msrc_Dcc_Tcc_Timeout + Tcc_Overhead;
         end if;
         if Sequence_Steps (DSS) then
            Ret := Ret + 2 * (Msrc_Dcc_Tcc_Timeout + Dss_Overhead);
         elsif Sequence_Steps (MSRC) then
            Ret := Ret + Msrc_Dcc_Tcc_Timeout + Msrc_Overhead;
         end if;
      end if;

      if Sequence_Steps (Pre_Range) then
         Pre_Range_Timeout := Sequence_Step_Timeout (This, Pre_Range);
         Ret := Ret + Pre_Range_Timeout + Pre_Range_Overhead;
      end if;

      if Sequence_Steps (Final_Range) then
         Final_Range_Timeout := Sequence_Step_Timeout (This, Final_Range);
         Ret := Ret + Final_Range_Timeout + Final_Range_Overhead;
      end if;

      return Ret;
   end Measurement_Timing_Budget;

   -----------------------------------
   -- Set_Measurement_Timing_Budget --
   -----------------------------------

   procedure Set_Measurement_Timing_Budget
     (This                 : VL53L0X_Ranging_Sensor;
      Budget_Micro_Seconds : HAL.UInt32;
      Status               : out Boolean)
   is
      Final_Range_Timing_Budget_us : UInt32;
      Sequence_Steps               : VL53L0x_Sequence_Step_Enabled;
      Pre_Range_Timeout_us         : UInt32 := 0;
      Sub_Timeout                  : UInt32 := 0;
      Msrc_Dcc_Tcc_Timeout         : UInt32;

   begin
      Status := True;

      Final_Range_Timing_Budget_us :=
        Budget_Micro_Seconds - Start_Overhead - End_Overhead
          - Final_Range_Overhead;

      Sequence_Steps := Sequence_Step_Enabled (This);

      if not Sequence_Steps (Final_Range) then
         return;
      end if;

      if Sequence_Steps (TCC)
        or else Sequence_Steps (MSRC)
        or else Sequence_Steps (DSS)
      then
         Msrc_Dcc_Tcc_Timeout := Sequence_Step_Timeout (This, MSRC);

         if Sequence_Steps (TCC) then
            Sub_Timeout := Msrc_Dcc_Tcc_Timeout + Tcc_Overhead;
         end if;

         if Sequence_Steps (DSS) then
            Sub_Timeout :=
              Sub_Timeout + 2 * (Msrc_Dcc_Tcc_Timeout + Dss_Overhead);

         elsif Sequence_Steps (MSRC) then
            Sub_Timeout := Sub_Timeout + Msrc_Dcc_Tcc_Timeout + Msrc_Overhead;
         end if;
      end if;

      if Sequence_Steps (Pre_Range) then
         Pre_Range_Timeout_us := Sequence_Step_Timeout (This, Pre_Range);

         Sub_Timeout :=
           Sub_Timeout + Pre_Range_Timeout_us + Pre_Range_Overhead;
      end if;

      if Sub_Timeout < Final_Range_Timing_Budget_us then
         Final_Range_Timing_Budget_us :=
           Final_Range_Timing_Budget_us - Sub_Timeout;

      else
         --  Requested timeout too big
         Status := False;

         return;
      end if;

      declare
         VCSel_Pulse_Period_Pclk : UInt8;
         Encoded_UInt16          : UInt16;
         Timeout_Mclks           : UInt32;
      begin
         if Sequence_Steps (Pre_Range) then
            VCSel_Pulse_Period_Pclk :=
              VCSel_Pulse_Period (This, Pre_Range);

            Read (This, REG_PRE_RANGE_CONFIG_TIMEOUT_MACROP_HI,
                  Encoded_UInt16, Status);

            if Status then
               Timeout_Mclks := Decode_Timeout (Encoded_UInt16);
            end if;
         else
            Timeout_Mclks := 0;
         end if;

         VCSel_Pulse_Period_Pclk :=
           VCSel_Pulse_Period (This, Final_Range);

         Timeout_Mclks := Timeout_Mclks + To_Timeout_Mclks
           (Final_Range_Timing_Budget_us,
            VCSel_Pulse_Period_Pclk);

         Write (This, REG_FINAL_RANGE_CONFIG_TIMEOUT_MACROP_HI,
                Encode_Timeout (Timeout_Mclks), Status);
      end;

      return;
   end Set_Measurement_Timing_Budget;

   ---------------------------
   -- Set_Signal_Rate_Limit --
   ---------------------------

   function Set_Signal_Rate_Limit
     (This       : VL53L0X_Ranging_Sensor;
      Limit_Mcps : Fix_Point_16_16) return Boolean
   is
      function To_U32 is new Ada.Unchecked_Conversion
        (Fix_Point_16_16, UInt32);
      Val    : UInt16;
      Status : Boolean;
   begin
      --  Expecting Fixed Point 9.7
      Val := UInt16 (Shift_Right (To_U32 (Limit_Mcps), 9) and 16#FF_FF#);
      Write (This,
             REG_FINAL_RANGE_CONFIG_MIN_COUNT_RATE_RTN_LIMIT,
             Val,
             Status);
      return Status;
   end Set_Signal_Rate_Limit;

   ---------------
   -- SPAD_Info --
   ---------------

   function SPAD_Info
     (This        : VL53L0X_Ranging_Sensor;
      SPAD_Count  : out HAL.UInt8;
      Is_Aperture : out Boolean) return Boolean
   is
      Status : Boolean;
      Tmp    : UInt8;
   begin
      Write (This, 16#80#, UInt8'(16#01#), Status);
      if Status then
         Write (This, 16#FF#, UInt8'(16#01#), Status);
      end if;
      if Status then
         Write (This, 16#00#, UInt8'(16#00#), Status);
      end if;

      if Status then
         Write (This, 16#FF#, UInt8'(16#06#), Status);
      end if;
      if Status then
         Read (This, 16#83#, Tmp, Status);
      end if;
      if Status then
         Write (This, 16#83#, Tmp or 16#04#, Status);
      end if;

      if Status then
         Write (This, 16#FF#, UInt8'(16#07#), Status);
      end if;
      if Status then
         Write (This, 16#81#, UInt8'(16#01#), Status);
      end if;

      if Status then
         Write (This, 16#80#, UInt8'(16#01#), Status);
      end if;

      if Status then
         Write (This, 16#94#, UInt8'(16#6B#), Status);
      end if;
      if Status then
         Write (This, 16#83#, UInt8'(16#00#), Status);
      end if;

      loop
         exit when not Status;
         Read (This, 16#83#, Tmp, Status);
         exit when Tmp /= 0;
      end loop;

      if Status then
         Write (This, 16#83#, UInt8'(16#01#), Status);
      end if;
      if Status then
         Read (This, 16#92#, Tmp, Status);
      end if;

      if Status then
         SPAD_Count := Tmp and 16#7F#;
         Is_Aperture := (Tmp and 16#80#) /= 0;

         Write (This, 16#81#, UInt8'(16#00#), Status);
      end if;
      if Status then
         Write (This, 16#FF#, UInt8'(16#06#), Status);
      end if;
      if Status then
         Read (This, 16#83#, Tmp, Status);
      end if;
      if Status then
         Write (This, 16#83#, Tmp and not 16#04#, Status);
      end if;
      if Status then
         Write (This, 16#FF#, UInt8'(16#01#), Status);
      end if;
      if Status then
         Write (This, 16#00#, UInt8'(16#01#), Status);
      end if;

      if Status then
         Write (This, 16#FF#, UInt8'(16#00#), Status);
      end if;
      if Status then
         Write (This, 16#80#, UInt8'(16#00#), Status);
      end if;

      return Status;
   end SPAD_Info;

   ---------------------------
   -- Set_Signal_Rate_Limit --
   ---------------------------

   procedure Set_Signal_Rate_Limit
     (This       : VL53L0X_Ranging_Sensor;
      Rate_Limit : Fix_Point_16_16)
   is
      function To_U32 is new Ada.Unchecked_Conversion
        (Fix_Point_16_16, UInt32);
      Reg : UInt16;
      Status : Boolean;
   begin
      --  Encoded as Fixed Point 9.7. Let's translate.
      Reg := UInt16 (Shift_Right (To_U32 (Rate_Limit), 9) and 16#FFFF#);
      Write (This, REG_FINAL_RANGE_CONFIG_MIN_COUNT_RATE_RTN_LIMIT,
             Reg, Status);
   end Set_Signal_Rate_Limit;

   --------------------------------------
   -- Set_Vcsel_Pulse_Period_Pre_Range --
   --------------------------------------

   procedure Set_VCSEL_Pulse_Period_Pre_Range
     (This   : VL53L0X_Ranging_Sensor;
      Period : UInt8;
      Status : out Boolean)
   is
   begin
      Set_VCSel_Pulse_Period (This, Period, Pre_Range, Status);
   end Set_VCSEL_Pulse_Period_Pre_Range;

   ----------------------------------------
   -- Set_Vcsel_Pulse_Period_Final_Range --
   ----------------------------------------

   procedure Set_VCSEL_Pulse_Period_Final_Range
     (This   : VL53L0X_Ranging_Sensor;
      Period : UInt8;
      Status : out Boolean)
   is
   begin
      Set_VCSel_Pulse_Period (This, Period, Final_Range, Status);
   end Set_VCSEL_Pulse_Period_Final_Range;

   ----------------------------
   -- Set_VCSel_Pulse_Period --
   ----------------------------

   procedure Set_VCSel_Pulse_Period
     (This     : VL53L0X_Ranging_Sensor;
      Period   : UInt8;
      Sequence : VL53L0x_Sequence_Step;
      Status   : out Boolean)
   is
      Encoded       : constant UInt8 := Shift_Right (Period, 1) - 1;
      Phase_High    : UInt8;
      Pre_Timeout   : UInt32;
      Final_Timeout : UInt32;
      Msrc_Timeout  : UInt32;
      Timeout_Mclks : UInt32;
      Steps_Enabled : constant VL53L0x_Sequence_Step_Enabled :=
                        Sequence_Step_Enabled (This);
      Budget        : UInt32;
      Sequence_Cfg  : UInt8;

   begin
      --  Save the measurement timing budget
      Budget := Measurement_Timing_Budget (This);

      case Sequence is
         when Pre_Range =>
            Pre_Timeout := Sequence_Step_Timeout (This, Pre_Range);
            Msrc_Timeout := Sequence_Step_Timeout (This, MSRC);

            case Period is
               when 12 =>
                  Phase_High := 16#18#;
               when 14 =>
                  Phase_High := 16#30#;
               when 16 =>
                  Phase_High := 16#40#;
               when 18 =>
                  Phase_High := 16#50#;
               when others =>
                  Status := False;

                  return;
            end case;

            Write (This, REG_PRE_RANGE_CONFIG_VALID_PHASE_HIGH,
                   Phase_High, Status);
            if not Status then
               return;
            end if;

            Write (This, REG_PRE_RANGE_CONFIG_VALID_PHASE_LOW,
                   UInt8'(16#08#), Status);
            if not Status then
               return;
            end if;

            Write (This, REG_PRE_RANGE_CONFIG_VCSEL_PERIOD,
                   Encoded, Status);
            if not Status then
               return;
            end if;

            --  Update the timeouts
            Timeout_Mclks := To_Timeout_Mclks (Pre_Timeout, Period);
            Write (This, REG_PRE_RANGE_CONFIG_TIMEOUT_MACROP_HI,
                   UInt16 (Timeout_Mclks), Status);

            Timeout_Mclks := To_Timeout_Mclks (Msrc_Timeout, Period);
            if Timeout_Mclks > 256 then
               Timeout_Mclks := 255;
            else
               Timeout_Mclks := Timeout_Mclks - 1;
            end if;

            Write (This, REG_MSRC_CONFIG_TIMEOUT_MACROP,
                   UInt8 (Timeout_Mclks), Status);

         when Final_Range =>
            Pre_Timeout := Sequence_Step_Timeout
              (This, Pre_Range, As_Mclks => True);
            Final_Timeout := Sequence_Step_Timeout (This, Final_Range);

            declare
               Phase_High  : UInt8;
               Width       : UInt8;
               Cal_Timeout : UInt8;
               Cal_Lim     : UInt8;
            begin
               case Period is
                  when 8 =>
                     Phase_High  := 16#10#;
                     Width       := 16#02#;
                     Cal_Timeout := 16#0C#;
                     Cal_Lim     := 16#30#;
                  when 10 =>
                     Phase_High  := 16#28#;
                     Width       := 16#03#;
                     Cal_Timeout := 16#09#;
                     Cal_Lim     := 16#20#;
                  when 12 =>
                     Phase_High  := 16#38#;
                     Width       := 16#03#;
                     Cal_Timeout := 16#08#;
                     Cal_Lim     := 16#20#;
                  when 14 =>
                     Phase_High  := 16#48#;
                     Width       := 16#03#;
                     Cal_Timeout := 16#07#;
                     Cal_Lim     := 16#20#;
                  when others =>
                     return;
               end case;

               Write (This, REG_FINAL_RANGE_CONFIG_VALID_PHASE_HIGH,
                      Phase_High, Status);
               if not Status then
                  return;
               end if;

               Write (This, REG_FINAL_RANGE_CONFIG_VALID_PHASE_LOW,
                      UInt8'(16#08#), Status);
               if not Status then
                  return;
               end if;

               Write (This, REG_GLOBAL_CONFIG_VCSEL_WIDTH,
                      Width, Status);
               if not Status then
                  return;
               end if;

               Write (This, REG_ALGO_PHASECAL_CONFIG_TIMEOUT,
                      Cal_Timeout, Status);
               if not Status then
                  return;
               end if;

               Write (This, 16#FF#, UInt8'(16#01#), Status);
               Write (This, REG_ALGO_PHASECAL_LIM,
                      Cal_Lim, Status);
               Write (This, 16#FF#, UInt8'(16#00#), Status);
               if not Status then
                  return;
               end if;
            end;

            --  Apply new VCSEL period
            Write (This, REG_FINAL_RANGE_CONFIG_VCSEL_PERIOD,
                   Encoded, Status);

            --  Update timeouts
            Timeout_Mclks := To_Timeout_Mclks (Final_Timeout, Period);

            if Steps_Enabled (Pre_Range) then
               --  the pre-range timeout must be added in this case
               Timeout_Mclks := Timeout_Mclks + Pre_Timeout;
            end if;

            Write (This, REG_FINAL_RANGE_CONFIG_TIMEOUT_MACROP_HI,
                   Encode_Timeout (Timeout_Mclks), Status);

         when others =>
            Status := False;

            return;
      end case;

      if Status then
         --  Restore the measurement timing budget
         Set_Measurement_Timing_Budget (This, Budget, Status);
      end if;

      --  And finally perform the phase calibration.
      Read (This, REG_SYSTEM_SEQUENCE_CONFIG, Sequence_Cfg, Status);

      if Status then
         Write (This, REG_SYSTEM_SEQUENCE_CONFIG, UInt8'(16#02#), Status);
      end if;
      if Status then
         Perform_Single_Ref_Calibration (This, 16#00#, Status);
      end if;
      if Status then
         Write (This, REG_SYSTEM_SEQUENCE_CONFIG, Sequence_Cfg, Status);
      end if;
   end Set_VCSel_Pulse_Period;

   ------------------------
   -- VCSel_Pulse_Period --
   ------------------------

   function VCSel_Pulse_Period
     (This     : VL53L0X_Ranging_Sensor;
      Sequence : VL53L0x_Sequence_Step) return UInt8
   is
      Ret    : UInt8;
      Status : Boolean;
   begin
      case Sequence is
         when Pre_Range =>
            Read (This, REG_PRE_RANGE_CONFIG_VCSEL_PERIOD,
                  Ret, Status);
         when Final_Range =>
            Read (This, REG_FINAL_RANGE_CONFIG_VCSEL_PERIOD,
                  Ret, Status);
         when others =>
            Status := False;
      end case;

      if Status then
         return Shift_Left (Ret + 1, 1);
      else
         return 0;
      end if;
   end VCSel_Pulse_Period;

end VL53L0X;
