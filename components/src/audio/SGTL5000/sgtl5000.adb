------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

package body SGTL5000 is

   ------------
   -- Modify --
   ------------

   procedure Modify (This  : in out SGTL5000_DAC;
                     Reg   : UInt16;
                     Mask  : UInt16;
                     Value : UInt16)
   is
      Tmp : UInt16 := This.I2C_Read (Reg);
   begin

      Tmp := Tmp and (not Mask);
      Tmp := Tmp or Value;

      This.I2C_Write (Reg, Tmp);
   end Modify;

   ---------------
   -- I2C_Write --
   ---------------

   procedure I2C_Write (This  : in out SGTL5000_DAC;
                        Reg   : UInt16;
                        Value : UInt16)
   is
      Status : I2C_Status;

   begin
      This.Port.Master_Transmit
        (Addr          => SGTL5000_QFN20_I2C_Addr,
         Data          => (1 => UInt8 (Shift_Right (Reg, 8) and 16#FF#),
                           2 => UInt8 (Reg and 16#FF#),
                           3 => UInt8 (Shift_Right (Value, 8) and 16#FF#),
                           4 => UInt8 (Value and 16#FF#)),
         Status        => Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
   end I2C_Write;

   --------------
   -- I2C_Read --
   --------------

   function I2C_Read (This : SGTL5000_DAC;
                      Reg  : UInt16)
                      return UInt16
   is
      Status : I2C_Status;
      Data   : I2C_Data (1 .. 2);

   begin
      This.Port.Mem_Read
        (Addr          => SGTL5000_QFN20_I2C_Addr,
         Mem_Addr      => Reg,
         Mem_Addr_Size => Memory_Size_16b,
         Data          => Data,
         Status        => Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
      return Shift_Left (UInt16 (Data (1)), 8) or UInt16 (Data (2));
   end I2C_Read;

   --------------
   -- Valid_Id --
   --------------

   function Valid_Id (This : SGTL5000_DAC) return Boolean is
      Id : constant UInt16 := This.I2C_Read (Chip_ID_Reg);
   begin

      return (Id and 16#FF00#) = SGTL5000_Chip_ID;
   end Valid_Id;

   --------------------
   -- Set_DAC_Volume --
   --------------------

   procedure Set_DAC_Volume (This        : in out SGTL5000_DAC;
                             Left, Right : DAC_Volume)
   is
   begin
      This.I2C_Write (DAC_VOL_REG,
                      UInt16 (Left) or Shift_Left (UInt16 (Right), 8));
   end Set_DAC_Volume;

   --------------------
   -- Set_ADC_Volume --
   --------------------

   procedure Set_ADC_Volume (This        : in out SGTL5000_DAC;
                             Left, Right : ADC_Volume;
                             Minus_6db   : Boolean)
   is
   begin
      This.Modify (ANA_ADC_CTRL_REG,
                   2#0000_0001_1111_1111#,
                   UInt16 (Left) or Shift_Left (UInt16 (Right), 4)
                   or (if Minus_6db then 2#000_0001_0000_0000# else 0));
   end Set_ADC_Volume;

   ---------------------------
   -- Set_Headphones_Volume --
   ---------------------------

   procedure Set_Headphones_Volume (This        : in out SGTL5000_DAC;
                                    Left, Right : HP_Volume)
   is
   begin
      This.Modify (ANA_HP_CTRL_REG,
                   2#0111_1111_0111_1111#,
                   UInt16 (Left) or Shift_Left (UInt16 (Right), 8));
   end Set_Headphones_Volume;

   -------------------------
   -- Set_Line_Out_Volume --
   -------------------------

   procedure Set_Line_Out_Volume (This        : in out SGTL5000_DAC;
                                  Left, Right : Line_Out_Volume)
   is
   begin
      This.Modify (LINE_OUT_VOL_REG,
                   2#0001_1111_0001_1111#,
                   UInt16 (Left) or Shift_Left (UInt16 (Right), 8));
   end Set_Line_Out_Volume;

   ---------------------
   -- Mute_Headphones --
   ---------------------

   procedure Mute_Headphones (This : in out SGTL5000_DAC;
                              Mute : Boolean := True)
   is
   begin
      This.Modify (ANA_CTRL_REG,
                   2#0000_0000_0001_0000#,
                   (if Mute then 2#0000_0000_0001_0000# else 0));
   end Mute_Headphones;

   -------------------
   -- Mute_Line_Out --
   -------------------

   procedure Mute_Line_Out (This : in out SGTL5000_DAC;
                            Mute : Boolean := True)
   is
   begin
      This.Modify (ANA_CTRL_REG,
                   2#0000_0001_0000_0000#,
                   (if Mute then 2#0000_0001_0000_0000# else 0));
   end Mute_Line_Out;

   --------------
   -- Mute_ADC --
   --------------

   procedure Mute_ADC (This : in out SGTL5000_DAC;
                       Mute : Boolean := True)
   is
   begin
      This.Modify (ANA_CTRL_REG,
                   2#0000_0000_0000_0001#,
                   (if Mute then 2#0000_0000_0000_0001# else 0));
   end Mute_ADC;

   --------------
   -- Mute_DAC --
   --------------

   procedure Mute_DAC (This : in out SGTL5000_DAC;
                       Mute : Boolean := True)
   is
   begin
      This.Modify (ADCDAC_CTRL_REG,
                   2#0000_0000_0000_1100#,
                   (if Mute then 2#0000_0000_0000_1100# else 0));
   end Mute_DAC;

   -----------------------
   -- Set_Power_Control --
   -----------------------

   procedure Set_Power_Control (This    : in out SGTL5000_DAC;
                                ADC     : Power_State;
                                DAC     : Power_State;
                                DAP     : Power_State;
                                I2S_Out : Power_State;
                                I2S_In  : Power_State)
   is
      Value : UInt16 := 0;
   begin
      if ADC = On then
         Value := Value or 2#0000_0000_0100_0000#;
      end if;
      if DAC = On then
         Value := Value or 2#0000_0000_0010_0000#;
      end if;
      if DAP = On then
         Value := Value or 2#0000_0000_0001_0000#;
      end if;
      if I2S_Out = On then
         Value := Value or 2#0000_0000_0000_0010#;
      end if;
      if I2S_In = On then
         Value := Value or 2#0000_0000_0000_0001#;
      end if;
      This.Modify (DIG_POWER_REG,
                   2#0000_0000_0111_0011#,
                   Value);
   end Set_Power_Control;

   ---------------------------
   -- Set_Reference_Control --
   ---------------------------

   procedure Set_Reference_Control (This          : in out SGTL5000_DAC;
                                    VAG           : Analog_Ground_Voltage;
                                    Bias          : Current_Bias;
                                    Slow_VAG_Ramp : Boolean)
   is
      Value : UInt16 := 0;
   begin

      Value := Value or Shift_Left (UInt16 (VAG), 4);
      Value := Value or Shift_Left (UInt16 (Bias), 1);
      Value := Value or (if Slow_VAG_Ramp then 1 else 0);

      This.I2C_Write (REF_CTRL_REG, Value);
   end Set_Reference_Control;

   -------------------------
   -- Set_Short_Detectors --
   -------------------------

   procedure Set_Short_Detectors (This      : in out SGTL5000_DAC;
                                  Right_HP  : Short_Detector_Level;
                                  Left_HP   : Short_Detector_Level;
                                  Center_HP : Short_Detector_Level;
                                  Mode_LR   : UInt2;
                                  Mode_CM   : UInt2)
   is
      Value : UInt16 := 0;
   begin
      Value := Value or (Shift_Left (UInt16 (Right_HP), 12));
      Value := Value or (Shift_Left (UInt16 (Left_HP), 8));
      Value := Value or (Shift_Left (UInt16 (Center_HP), 4));
      Value := Value or (Shift_Left (UInt16 (Mode_LR), 2));
      Value := Value or UInt16 (Mode_CM);

      This.I2C_Write (SHORT_CTRL_REG, Value);
   end Set_Short_Detectors;

   -------------------------
   -- Set_Linereg_Control --
   -------------------------

   procedure Set_Linereg_Control (This                     : in out SGTL5000_DAC;
                                  Charge_Pump_Src_Override : Boolean;
                                  Charge_Pump_Src          : Charge_Pump_Source;
                                  Linereg_Out_Voltage      : Linear_Regulator_Out_Voltage)
   is
      Value : UInt16 := 0;
   begin
      if Charge_Pump_Src_Override then
         Value := Value or 2#10_0000#;
      end if;

      if Charge_Pump_Src = VDDIO then
         Value := Value or 2#100_0000#;
      end if;

      Value := Value or UInt16 (Linereg_Out_Voltage);
      This.I2C_Write (LINREG_CTRL_REG, Value);
   end Set_Linereg_Control;

   -------------------------
   -- Set_Lineout_Control --
   -------------------------

   procedure Set_Lineout_Control (This                   : in out SGTL5000_DAC;
                                  Out_Current            : Lineout_Current;
                                  Amp_Analog_GND_Voltage : UInt6)
   is
   begin
      This.I2C_Write (LINE_OUT_CTRL_REG,
                      Shift_Left (UInt16 (Out_Current'Enum_Rep), 8)
                      or
                      UInt16 (Amp_Analog_GND_Voltage));
   end Set_Lineout_Control;

   ----------------------
   -- Set_Analog_Power --
   ----------------------

   procedure Set_Analog_Power (This                      : in out SGTL5000_DAC;
                               DAC_Mono                  : Boolean := False;
                               Linreg_Simple_PowerUp     : Boolean := False;
                               Startup_PowerUp           : Boolean := False;
                               VDDC_Charge_Pump_PowerUp  : Boolean := False;
                               PLL_PowerUp               : Boolean := False;
                               Linereg_D_PowerUp         : Boolean := False;
                               VCOAmp_PowerUp            : Boolean := False;
                               VAG_PowerUp               : Boolean := False;
                               ADC_Mono                  : Boolean := False;
                               Reftop_PowerUp            : Boolean := False;
                               Headphone_PowerUp         : Boolean := False;
                               DAC_PowerUp               : Boolean := False;
                               Capless_Headphone_PowerUp : Boolean := False;
                               ADC_PowerUp               : Boolean := False;
                               Linout_PowerUp            : Boolean := False)
   is
      Value : UInt16 := 0;
   begin
      if Linout_PowerUp then
         Value := Value or 2**0;
      end if;
      if ADC_PowerUp then
         Value := Value or 2**1;
      end if;
      if Capless_Headphone_PowerUp then
         Value := Value or 2**2;
      end if;
      if DAC_PowerUp then
         Value := Value or 2**3;
      end if;
      if Headphone_PowerUp then
         Value := Value or 2**4;
      end if;
      if Reftop_PowerUp then
         Value := Value or 2**5;
      end if;
      if not ADC_Mono then
         Value := Value or 2**6;
      end if;
      if VAG_PowerUp then
         Value := Value or 2**7;
      end if;
      if VCOAmp_PowerUp then
         Value := Value or 2**8;
      end if;
      if Linereg_D_PowerUp then
         Value := Value or 2**9;
      end if;
      if PLL_PowerUp then
         Value := Value or 2**10;
      end if;
      if VDDC_Charge_Pump_PowerUp then
         Value := Value or 2**11;
      end if;
      if Startup_PowerUp then
         Value := Value or 2**12;
      end if;
      if Linreg_Simple_PowerUp then
         Value := Value or 2**13;
      end if;
      if not DAC_Mono then
         Value := Value or 2**14;
      end if;
      This.I2C_Write (ANA_POWER_REG, Value);
   end Set_Analog_Power;

   -----------------------
   -- Set_Clock_Control --
   -----------------------

   procedure Set_Clock_Control (This : in out SGTL5000_DAC;
                                Rate : Rate_Mode;
                                FS   : SYS_FS_Freq;
                                MCLK : MCLK_Mode)
   is
      Value : UInt16 := 0;
   begin
      Value := Value or (case Rate is
                            when SYS_FS         => 2#00_0000#,
                            when Half_SYS_FS    => 2#01_0000#,
                            when Quarter_SYS_FS => 2#10_0000#,
                            when Sixth_SYS_FS   => 2#11_0000#);

      Value := Value or (case FS is
                            when SYS_FS_32kHz => 2#0000#,
                            when SYS_FS_44kHz => 2#0100#,
                            when SYS_FS_48kHz => 2#1000#,
                            when SYS_FS_96kHz => 2#1100#);

      Value := Value or (case MCLK is
                            when MCLK_256FS => 2#00#,
                            when MCLK_384FS => 2#01#,
                            when MCLK_512FS => 2#10#,
                            when Use_PLL    => 2#11#);

      This.I2C_Write (CLK_CTRL_REG, Value);
   end Set_Clock_Control;

   -------------
   -- Set_PLL --
   -------------

   procedure Set_PLL (This            : in out SGTL5000_DAC;
                      PLL_Output_Freq : Natural;
                      MCLK_Freq       : Natural)
   is
      Int_Divisor : constant Natural := PLL_Output_Freq / MCLK_Freq;
      Frac_Divisor : constant Natural :=
        Natural (((Float (PLL_Output_Freq) / Float (MCLK_Freq)) - Float (Int_Divisor)) * 2048.0);
   begin
      This.I2C_Write
        (PLL_CTRL_REG,
         Shift_Left (UInt16 (Int_Divisor), 11) or UInt16 (Frac_Divisor));

      --  Wait for PLL lock
      loop
         declare
            Status : constant UInt16 := This.I2C_Read (ANA_STATUS_REG);
         begin

            --  Check PLL_IS_LOCKED bit
            exit when (Status and 2#0001_0000#) /= 0;
         end;
      end loop;
   end Set_PLL;

   ---------------------
   -- Set_I2S_Control --
   ---------------------

   procedure Set_I2S_Control (This        : in out SGTL5000_DAC;
                              SCLKFREQ    : SCLKFREQ_Mode;
                              Invert_SCLK : Boolean;
                              Master_Mode : Boolean;
                              Data_Len    : Data_Len_Mode;
                              I2S         : I2S_Mode;
                              LR_Align    : Boolean;
                              LR_Polarity : Boolean)
   is
      Value : UInt16 := 0;
   begin
      Value := Value or (case SCLKFREQ is
                            when SCLKFREQ_64FS => 2#0_0000_0000#,
                            when SCLKFREQ_32FS => 2#1_0000_0000#);

      if Master_Mode then
         Value := Value or 2#1000_0000#;
      end if;

      if Invert_SCLK then
         Value := Value or 2#0100_0000#;
      end if;

      Value := Value or (case Data_Len is
                            when Data_32b => 2#00_0000#,
                            when Data_24b => 2#01_0000#,
                            when Data_20b => 2#10_0000#,
                            when Data_16b => 2#11_0000#);

      Value := Value or (case I2S is
                            when I2S_Left_Justified => 2#0000#,
                            when Right_Justified    => 2#0100#,
                            when PCM                => 2#1000#);

      if LR_Align then
         Value := Value or 2#10#;
      end if;

      if LR_Polarity then
         Value := Value or 2#1#;
      end if;

      This.I2C_Write (I2S_CTRL_REG, Value);
   end Set_I2S_Control;

   -----------------------
   -- Select_ADC_Source --
   -----------------------

   procedure Select_ADC_Source (This       : in out SGTL5000_DAC;
                                Source     : ADC_Source;
                                Enable_ZCD : Boolean)
   is
   begin
      This.Modify (ANA_CTRL_REG,
                   2#0000_0000_0000_0100#,
                   (case Source is
                       when Microphone => 2#0000_0000_0000_0000#,
                       when Line_In    => 2#0000_0000_0000_0100#)
                   or (if Enable_ZCD then 2#0000_0000_0000_0010# else 0));
   end Select_ADC_Source;

   -----------------------
   -- Select_DAP_Source --
   -----------------------

   procedure Select_DAP_Source (This   : in out SGTL5000_DAC;
                                Source : DAP_Source)
   is
   begin
      This.Modify (SSS_CTRL_REG,
                   2#0000_0000_1100_0000#,
                   (case Source is
                       when ADC    => 2#0000_0000_0000_0000#,
                       when I2S_In => 2#0000_0000_0100_0000#));
   end Select_DAP_Source;

   ---------------------------
   -- Select_DAP_Mix_Source --
   ---------------------------

   procedure Select_DAP_Mix_Source (This   : in out SGTL5000_DAC;
                                    Source : DAP_Mix_Source)
   is
   begin
      This.Modify (SSS_CTRL_REG,
                   2#0000_0011_0000_0000#,
                   (case Source is
                       when ADC    => 2#0000_0000_0000_0000#,
                       when I2S_In => 2#0000_0001_0000_0000#));
   end Select_DAP_Mix_Source;

   -----------------------
   -- Select_DAC_Source --
   -----------------------

   procedure Select_DAC_Source (This   : in out SGTL5000_DAC;
                                Source : DAC_Source)
   is
   begin
      This.Modify (SSS_CTRL_REG,
                   2#0000_0000_0011_0000#,
                   (case Source is
                       when ADC    => 2#0000_0000_0000_0000#,
                       when I2S_In => 2#0000_0000_0001_0000#,
                       when DAP    => 2#0000_0000_0011_0000#));
   end Select_DAC_Source;

   ----------------------
   -- Select_HP_Source --
   ----------------------

   procedure Select_HP_Source (This       : in out SGTL5000_DAC;
                               Source     : HP_Source;
                               Enable_ZCD : Boolean)
   is
   begin
      This.Modify (ANA_CTRL_REG,
                   2#0000_0000_0100_0000#,
                   (case Source is
                       when DAC     => 2#0000_0000_0000_0000#,
                       when Line_In => 2#0000_0000_0100_0000#)
                   or (if Enable_ZCD then 2#0000_0000_0010_0000# else 0));
   end Select_HP_Source;

   ---------------------------
   -- Select_I2S_Out_Source --
   ---------------------------

   procedure Select_I2S_Out_Source (This   : in out SGTL5000_DAC;
                                    Source : I2S_Out_Source)
   is
   begin
      This.Modify (SSS_CTRL_REG,
                   2#0000_0000_0000_0011#,
                   (case Source is
                       when ADC    => 2#0000_0000_0000_0000#,
                       when I2S_In => 2#0000_0000_0000_0001#,
                       when DAP    => 2#0000_0000_0000_0011#));
   end Select_I2S_Out_Source;
end SGTL5000;
