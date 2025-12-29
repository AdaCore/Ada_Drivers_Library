------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2025, AdaCore                             --
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

package body BMP280 is

   function Time_Standby_To_UInt3
     (Time_Standby : Time_Standby_Type) return UInt3;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This  : in out BMP280_Device;
      CS    : Any_GPIO_Point;
      SPI   : Any_SPI_Port;
      Error : out Boolean) is
   begin
      This.Holder :=
        (CS      => CS,
         SPI     => SPI,
         Chip_ID => 0,
         Dig_T1  => 0,
         Dig_T2  => 0,
         Dig_T3  => 0,
         dig_P1  => 0,
         dig_P2  => 0,
         dig_P3  => 0,
         dig_P4  => 0,
         dig_P5  => 0,
         dig_P6  => 0,
         dig_P7  => 0,
         dig_P8  => 0,
         dig_P9  => 0);

      if Get_Chip_ID (This) > 0 then
         Read_Trim_Registers (This);
         Error := False;
      else
         Error := True;
      end if;

   exception
      when others =>
         Error := True;
   end Initialize;

   ------------
   -- CS_Low --
   ------------

   procedure CS_Low (This : BMP280_Device) is
   begin
      if This.Holder.CS /= null then
         This.Holder.CS.Clear;
      end if;
   end CS_Low;

   -------------
   -- CS_High --
   -------------

   procedure CS_High (This : BMP280_Device) is
   begin
      if This.Holder.CS /= null then
         This.Holder.CS.Set;
      end if;
   end CS_High;

   -------------------
   -- Read_Register --
   -------------------

   function Read_Register
     (This     : BMP280_Device;
      Register : Register_Name)
      return UInt8
   is
      Buf    : SPI_Data_8b (1 .. 1);
      Cmd    : Command :=
        (Cmd  => Read,
         Addr => Adresses (Register)) with
          Address => Buf (Buf'First)'Address;
      Status : HAL.SPI.SPI_Status;
   begin
      CS_Low (This);
      This.Holder.SPI.Transmit (Buf, Status);

      if Status = HAL.SPI.Ok then
         This.Holder.SPI.Receive (Buf, Status);
      end if;

      if Status /= HAL.SPI.Ok then
         raise Program_Error;
      end if;

      CS_High (This);
      return Buf (Buf'First);
   end Read_Register;

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register
     (This     : BMP280_Device;
      Register : Register_Name;
      Data     : UInt8)
   is
      Buf   : SPI_Data_8b (1 .. 2);
      Cmd   : Command :=
        (Cmd  => Write,
         Addr => Adresses (Register)) with
          Address => Buf (Buf'First)'Address;
      Local : UInt8 := Data
        with Address => Buf (Buf'First + 1)'Address;

      Status : HAL.SPI.SPI_Status;
   begin
      CS_Low (This);

      This.Holder.SPI.Transmit (Buf, Status);

      CS_High (This);

      if Status /= HAL.SPI.Ok then
         raise Program_Error;
      end if;
   end Write_Register;

   -------------------
   -- Read_Register --
   -------------------

   function Read_Register
     (This : BMP280_Device;
      Msb  : Register_Name;
      Lsb  : Register_Name)
      return UInt16 is
   begin
      return Shift_Left (UInt16 (Read_Register (This, Msb)), 8) +
        UInt16 (Read_Register (This, Lsb));
   end Read_Register;

   -------------------
   -- Read_Register --
   -------------------

   function Read_Register
     (This : BMP280_Device;
      Msb  : Register_Name;
      Lsb  : Register_Name)
      return Interfaces.Integer_16
   is
      Tmp    : UInt16 := Read_Register (This, Msb, Lsb);
      Result : Interfaces.Integer_16 with Import, Address => Tmp'Address;
   begin
      return Result;
   end Read_Register;

   -------------------------
   -- Read_Trim_Registers --
   -------------------------

   procedure Read_Trim_Registers (This : in out BMP280_Device) is
   begin
      This.Holder.Dig_T1 := Integer_32
        (UInt16'(Read_Register (This, dig_T1_msb, dig_T1_lsb)));

      This.Holder.Dig_T2 := Integer_32
        (Interfaces.Integer_16'(Read_Register (This, dig_T2_msb, dig_T2_lsb)));

      This.Holder.Dig_T3 := Integer_32
        (Interfaces.Integer_16'(Read_Register (This, dig_T3_msb, dig_T3_lsb)));

      This.Holder.dig_P1 := Integer_32
        (UInt16'(Read_Register (This, dig_P1_msb, dig_P1_lsb)));

      This.Holder.dig_P2 := Integer_32
        (Interfaces.Integer_16'(Read_Register (This, dig_P2_msb, dig_P2_lsb)));

      This.Holder.dig_P3 := Integer_32
        (Interfaces.Integer_16'(Read_Register (This, dig_P3_msb, dig_P3_lsb)));

      This.Holder.dig_P4 := Integer_32
        (Interfaces.Integer_16'(Read_Register (This, dig_P4_msb, dig_P4_lsb)));

      This.Holder.dig_P5 := Integer_32
        (Interfaces.Integer_16'(Read_Register (This, dig_P5_msb, dig_P5_lsb)));

      This.Holder.dig_P6 := Integer_32
        (Interfaces.Integer_16'(Read_Register (This, dig_P6_msb, dig_P6_lsb)));

      This.Holder.dig_P7 := Integer_32
        (Interfaces.Integer_16'(Read_Register (This, dig_P7_msb, dig_P7_lsb)));

      This.Holder.dig_P8 := Integer_32
        (Interfaces.Integer_16'(Read_Register (This, dig_P8_msb, dig_P8_lsb)));

      This.Holder.dig_P9 := Integer_32
        (Interfaces.Integer_16'(Read_Register (This, dig_P9_msb, dig_P9_lsb)));
   end Read_Trim_Registers;

   -------------------
   -- Read_Register --
   -------------------

   function Read_Register
     (This      : BMP280_Device;
      Precision : Precision_Type;
      Msb       : Register_Name;
      Lsb       : Register_Name;
      Xlsb      : Register_Name)
      return Interfaces.Integer_32
   is
      Xlsb_Data : UInt8 := Read_Register (This, Xlsb);
      Sb_Data   : constant UInt32 := UInt32
        (UInt16'(Read_Register (This => This, Msb => Msb, Lsb => Lsb)));
   begin
      case Precision is
         when Skipped | Ultra_Low_Power =>
            null;

         when Low_Power =>
            Xlsb_Data := Xlsb_Data and 2#1000_0000#;

         when Standard_Resolution =>
            Xlsb_Data := Xlsb_Data and 2#1100_0000#;

         when High_Resolution =>
            Xlsb_Data := Xlsb_Data and 2#1110_0000#;

         when Ultra_High_Resolution =>
            Xlsb_Data := Xlsb_Data and 2#1111_0000#;
      end case;

      return Interfaces.Integer_32
        (Shift_Right (Shift_Left (Sb_Data, 8) + UInt32 (Xlsb_Data), 4));
   end Read_Register;

   -----------------
   -- Get_Chip_ID --
   -----------------

   function Get_Chip_ID (This : BMP280_Device) return Chip_ID is
   begin
      return Chip_ID (Read_Register (This, id));
   end Get_Chip_ID;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : BMP280_Device) is
   begin
      Write_Register
        (This     => This,
         Register => reset,
         Data     => 16#B6#);
   end Reset;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status (This : BMP280_Device) return Status_Type
   is
      Data     : UInt8;
      Register : Status_Type with Import, Address =>  Data'Address;
   begin
      Data := Read_Register (This, status);
      return Register;
   end Get_Status;

   ----------------------
   -- Is_Measuring_Set --
   ----------------------

   function Is_Measuring_Set (Status : Status_Type) return Boolean is
   begin
      return Status.Measuring = 1;
   end Is_Measuring_Set;

   ----------------------
   -- Is_Im_Update_Set --
   ----------------------

   function Is_Im_Update_Set (Status : Status_Type) return Boolean is
   begin
      return Status.Im_Update = 1;
   end Is_Im_Update_Set;

   --------------------
   -- Set_Power_Mode --
   --------------------

   procedure Set_Power_Mode
     (This       : BMP280_Device;
      Power_Mode : Power_Mode_Type)
   is
      Data     : UInt8;
      Register : Ctrl_Meas_Register with Import, Address => Data'Address;
   begin
      Data := Read_Register (This, ctrl_meas);
      Register.mode := Power_Mode;
      Write_Register (This, ctrl_meas, Data);
   end Set_Power_Mode;

   ------------------------
   -- Set_Temp_Precision --
   ------------------------

   procedure Set_Temp_Precision
     (This      : BMP280_Device;
      Precision : Precision_Type)
   is
      Data     : UInt8;
      Register : Ctrl_Meas_Register with Import, Address => Data'Address;
   begin
      Data := Read_Register (This, ctrl_meas);
      Register.osrs_t := Precision;
      Write_Register (This, ctrl_meas, Data);
   end Set_Temp_Precision;

   -------------------------
   -- Set_Press_Precision --
   -------------------------

   procedure Set_Press_Precision
     (This      : BMP280_Device;
      Precision : Precision_Type)
   is
      Data     : UInt8;
      Register : Ctrl_Meas_Register with Import, Address => Data'Address;
   begin
      Data := Read_Register (This, ctrl_meas);
      Register.osrs_p := Precision;
      Write_Register (This, ctrl_meas, Data);
   end Set_Press_Precision;

   ----------------------
   -- Set_Time_Standby --
   ----------------------

   procedure Set_Time_Standby
     (This         : BMP280_Device;
      Time_Standby : Time_Standby_Type)
   is
      Data         : UInt8;
      Register     : Config_Register with Import, Address => Data'Address;
   begin
      Data := Read_Register (This, config);
      Register.t_sb := Time_Standby_To_UInt3 (Time_Standby);
      Write_Register (This, config, Data);
   end Set_Time_Standby;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
     (This     : BMP280_Device;
      Filter   : Filter_Type)
   is
      Data     : UInt8;
      Register : Config_Register with Import, Address => Data'Address;
   begin
      Data := Read_Register (This, config);
      Register.filter := UInt3 (Filter);
      Write_Register (This, config, Data);
   end Set_Filter;

   ----------------
   -- Get_T_Fine --
   ----------------

   function Get_T_Fine
     (This  : BMP280_Device;
      Adc_T : Interfaces.Integer_32)
      return Interfaces.Integer_32
   is
      dig_T1 : constant Integer_32 := This.Holder.Dig_T1;
      dig_T2 : constant Integer_32 := This.Holder.Dig_T2;
      dig_T3 : constant Integer_32 := This.Holder.Dig_T3;

      var1   : Integer_32;
      var2   : Integer_32;
   begin
      var1 := ((Adc_T / 8 - dig_T1 * 2) * dig_T2) / 2048;
      var2 := Adc_T / 16 - dig_T1;
      var2 := (((var2 * var2) / 4096) * dig_T3) / 16384;
      return var1 + var2;
   end Get_T_Fine;

   --------------
   -- Get_Temp --
   --------------

   function Get_Temp (This : BMP280_Device) return Temperature
   is
      Val        : constant UInt8 := Read_Register (This, ctrl_meas);
      Resolution : Ctrl_Meas_Register with Import, Address => Val'Address;
      Adc_T      : Interfaces.Integer_32;
      T          : Interfaces.Integer_32;
   begin
      if Resolution.osrs_t = Skipped then
         return Temperature'First;
      end if;

      Adc_T := Read_Register
        (This      => This,
         Precision => Resolution.osrs_t,
         Msb       => temp_msb,
         Lsb       => temp_lsb,
         Xlsb      => temp_xlsb);
      T := (Get_T_Fine (This, Adc_T) * 5 + 128) / 256;

      return Temperature (Float (T) / 100.0);
   end Get_Temp;

   ---------------
   -- Get_Press --
   ---------------

   function Get_Press (This : BMP280_Device) return Pressure
   is
      Val        : constant UInt8 := Read_Register (This, ctrl_meas);
      Resolution : Ctrl_Meas_Register with Import, Address => Val'Address;

      Adc_T  : Integer_32;
      Adc_P  : Integer_32;
      Var1   : Integer_64;
      Var2   : Integer_64;
      P      : Integer_64;

      dig_P1 : Integer_32 renames This.Holder.dig_P1;
      dig_P2 : Integer_32 renames This.Holder.dig_P2;
      dig_P3 : Integer_32 renames This.Holder.dig_P3;
      dig_P4 : Integer_32 renames This.Holder.dig_P4;
      dig_P5 : Integer_32 renames This.Holder.dig_P5;
      dig_P6 : Integer_32 renames This.Holder.dig_P6;
      dig_P7 : Integer_32 renames This.Holder.dig_P7;
      dig_P8 : Integer_32 renames This.Holder.dig_P8;
      dig_P9 : Integer_32 renames This.Holder.dig_P9;
   begin
      if Resolution.osrs_t = Skipped
        or else Resolution.osrs_p = Skipped
      then
         return Pressure'First;
      end if;

      Adc_T := Read_Register
        (This      => This,
         Precision => Resolution.osrs_t,
         Msb       => temp_msb,
         Lsb       => temp_lsb,
         Xlsb      => temp_xlsb);

      Adc_P := Read_Register
        (This      => This,
         Precision => Resolution.osrs_p,
         Msb       => press_msb,
         Lsb       => press_lsb,
         Xlsb      => press_xlsb);

      Var1 := Integer_64 (Get_T_Fine (This, Adc_T) - 128000);
      Var2 := Var1 * Var1 * Integer_64 (dig_P6);
      Var2 := Var2 + (Var1 * Integer_64 (dig_P5)) * 131072;
      Var2 := Var2 + (Integer_64 (dig_P4) * 34359738368);
      Var1 := ((Var1 * Var1 * Integer_64 (dig_P3)) / 256) +
        ((Var1 * Integer_64 (dig_P2)) * 4096);
      Var1 := ((140737488355328 + Var1) * Integer_64 (dig_P1)) / 8589934592;

      if Var1 = 0 then
         return Pressure'First;
      end if;

      P := 1048576 - Integer_64 (Adc_P);
      P := ((P * 2147483648 - Var2) * 3125) / Var1;
      Var1 := (Integer_64 (dig_P9) * (P / 8192) *  (P / 8192)) / 33554432;
      Var2 := (Integer_64 (dig_P8) * P) / 524288;

      P := (((P + Var1 + Var2) / 256) + (Integer_64 (dig_P7) * 16));

      return Pressure (Float (P) / 25600.0);
   end Get_Press;

   ------------------
   -- Set_SPI_Mode --
   ------------------

   procedure Set_SPI_Mode (This : BMP280_Device; Mode : SPI_Mode)
   is
      Data     : UInt8;
      Register : Config_Register with Import, Address => Data'Address;
   begin
      Data := Read_Register (This, config);
      if Mode = SPI3W then
         Register.spi3w_en := 1;
      else
         Register.spi3w_en := 0;
      end if;
      Write_Register (This, config, Data);
   end Set_SPI_Mode;

   ----------------------
   -- Set_Time_Standby --
   ----------------------

   function Time_Standby_To_UInt3
     (Time_Standby : Time_Standby_Type) return UInt3 is
   begin
      return (if    Time_Standby = 0.5    then 2#000#
              elsif Time_Standby = 62.5   then 2#001#
              elsif Time_Standby = 125.0  then 2#010#
              elsif Time_Standby = 250.0  then 2#011#
              elsif Time_Standby = 500.0  then 2#100#
              elsif Time_Standby = 1000.0 then 2#101#
              elsif Time_Standby = 2000.0 then 2#110#
              else  2#111#);
   end Time_Standby_To_UInt3;

end BMP280;
