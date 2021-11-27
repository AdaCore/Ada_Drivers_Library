------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2021, AdaCore                           --
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

package LPS25H is

   type LPS25H_Barometric_Sensor is abstract tagged limited private;

   procedure Initialize (This : in out LPS25H_Barometric_Sensor) is abstract;

   subtype Pressure is Float range 450.0 .. 1100.0;   --  in mBar
   subtype Temperature is Float range -20.0 .. 80.0;  --  in degrees Celsius
   subtype Altitude is Float range -8000.0 .. 8000.0; -- Metres above sea level

   function Is_Initialized (This : LPS25H_Barometric_Sensor) return Boolean;

   procedure Get_Data
     (This   : in out LPS25H_Barometric_Sensor;
      Press  :    out Pressure;
      Temp   :    out Temperature;
      Asl    :    out Altitude;
      Status :    out Boolean) is abstract
   with Pre'Class => Is_Initialized (This);

private

   type LPS25H_Barometric_Sensor is abstract tagged limited record
      Initialized : Boolean := False;
   end record;

   function Is_Initialized (This : LPS25H_Barometric_Sensor) return Boolean
   is (This.Initialized);

   --  These register addresses are copied from the Crazyflie lps25h.h.

   REF_P_XL       : constant := 16#08#;
   REF_P_L        : constant := 16#09#;
   REF_P_H        : constant := 16#0A#;

   WHO_AM_I       : constant := 16#0F#;

   RES_CONF       : constant := 16#10#;

   CTRL_REG1      : constant := 16#20#;
   CTRL_REG2      : constant := 16#21#;
   CTRL_REG3      : constant := 16#22#;
   CTRL_REG4      : constant := 16#23#;
   INTERRUPT_CFG  : constant := 16#24#;
   INT_SOURCE     : constant := 16#25#;
   STATUS_REG     : constant := 16#27#;

   PRESS_OUT_XL   : constant := 16#28#;
   PRESS_OUT_L    : constant := 16#29#;
   PRESS_OUT_H    : constant := 16#2A#;

   TEMP_OUT_L     : constant := 16#2B#;
   TEMP_OUT_H     : constant := 16#2C#;

   FIFO_CTRL      : constant := 16#2E#;
   FIFO_STATUS    : constant := 16#2F#;
   THS_P_L        : constant := 16#30#;
   THS_P_H        : constant := 16#31#;
   RPDS_L         : constant := 16#39#;
   RPDS_H         : constant := 16#3A#;

   --  The expected response to a WHO_AM_I request

   WAI_ID         : constant := 16#BD#;

   type Bit is range 0 .. 1 with Size => 1;

   type Zero_Bit is range 0 .. 0 with Size => 1;
   type Zero_Bits is array (Natural range <>) of Zero_Bit
   with Component_Size => 1;

   --  Pressure and temperature internal average configuration
   type Pressure_Resolution_Configuration is (Avg_8, Avg_32, Avg_128, Avg_512)
   with Size => 2;
   for Pressure_Resolution_Configuration use
     (Avg_8 => 0, Avg_32 => 1, Avg_128 => 2, Avg_512 => 3);
   type Temp_Resolution_Configuration is (Avg_8, Avg_16, Avg_32, Avg_64)
   with Size => 2;
   for Temp_Resolution_Configuration use
     (Avg_8 => 0, Avg_16 => 1, Avg_32 => 2, Avg_64 => 3);
   type Res_Conf_Register is record
      AVGP     : Pressure_Resolution_Configuration := Avg_8;
      AVGT     : Temp_Resolution_Configuration     := Avg_8;
      Reserved : Zero_Bits (4 .. 7)                := (others => 0);
   end record
   with Size => 8;
   for Res_Conf_Register use record
      AVGP     at 0 range 0 .. 1;
      AVGT     at 0 range 2 .. 3;
      Reserved at 0 range 4 .. 7;
   end record;

   --  Control register 1
   type Output_Data_Rate is (One_Shot, Hz_1, Hz_7, Hz_12p5, Hz_25)
   with Size => 3;
   for Output_Data_Rate use
     (One_Shot => 0, Hz_1 => 1, Hz_7 => 2, Hz_12p5 => 3, Hz_25 => 4);
   type Ctrl_Reg1_Register is record
      SIM      : Bit              := 0;
      RESET_AZ : Bit              := 0;
      BDU      : Bit              := 0;
      DIFF_EN  : Bit              := 0;
      ODR      : Output_Data_Rate := One_Shot;
      PD       : Bit              := 0;
   end record
   with Size => 8;
   for Ctrl_Reg1_Register use record
      SIM      at 0 range 0 .. 0;
      RESET_AZ at 0 range 1 .. 1;
      BDU      at 0 range 2 .. 2;
      DIFF_EN  at 0 range 3 .. 3;
      ODR      at 0 range 4 .. 6;
      PD       at 0 range 7 .. 7;
   end record;

   --  Control register 2
   type Ctrl_Reg2_Register is record
      ONE_SHOT      : Bit := 0;
      AUTO_ZERO     : Bit := 0;
      SWRESET       : Bit := 0;
      I2C_ENABLE    : Bit := 0; -- ?
      FIFO_MEAN_DEC : Bit := 0;
      WTM_EN        : Bit := 0;
      FIFO_EN       : Bit := 0;
      BOOT          : Bit := 0;
   end record
   with Size => 8;
   for Ctrl_Reg2_Register use record
      ONE_SHOT      at 0 range 0 .. 0;
      AUTO_ZERO     at 0 range 1 .. 1;
      SWRESET       at 0 range 2 .. 2;
      I2C_ENABLE    at 0 range 3 .. 3;
      FIFO_MEAN_DEC at 0 range 4 .. 4;
      WTM_EN        at 0 range 5 .. 5;
      FIFO_EN       at 0 range 6 .. 6;
      BOOT          at 0 range 7 .. 7;
   end record;

   --  Control register 3
   type Interrupt_Configuration is
     (Data_Signal, Pressure_High, Pressure_Low, Pressure_Low_Or_High)
   with Size => 2;
   for Interrupt_Configuration use
     (Data_Signal => 0, Pressure_High => 1,
      Pressure_Low => 2, Pressure_Low_Or_High => 3);
   type Ctrl_Reg3_Register is record
      INT1     : Interrupt_Configuration := Data_Signal;
      Reserved : Zero_Bits (2 .. 5)      := (others => 0);
      PP_OD    : Bit                     := 0;
      Int_H_L  : Bit                     := 0;
   end record
   with Size => 8;
   for Ctrl_Reg3_Register use record
      INT1     at 0 range 0 .. 1;
      Reserved at 0 range 2 .. 5;
      PP_OD    at 0 range 6 .. 6;
      Int_H_L  at 0 range 7 .. 7;
   end record;

   --  Control register 4
   type Ctrl_Reg4_Register is record
      P1_DRDY    : Bit                := 0;
      P1_OVERRUN : Bit                := 0;
      P1_WTM     : Bit                := 0;
      P1_EMPTY   : Bit                := 0;
      Reserved   : Zero_Bits (4 .. 7) := (others => 0);
   end record
   with Size => 8;
   for Ctrl_Reg4_Register use record
      P1_DRDY    at 0 range 0 .. 0;
      P1_OVERRUN at 0 range 1 .. 1;
      P1_WTM     at 0 range 2 .. 2;
      P1_EMPTY   at 0 range 3 .. 3;
      Reserved   at 0 range 4 .. 7;
   end record;

   --  Bored now, leaving out Interrupt_CFG_Register, Int_Source_Register

   --  Status register
   type Status_Register is record
      T_DA       : Bit                := 0;
      P_DA       : Bit                := 0;
      Reserved_1 : Zero_Bits (2 .. 3) := (others => 0);
      T_OR       : Bit                := 0;
      P_OR       : Bit                := 0;
      Reserved_2 : Zero_Bits (6 .. 7) := (others => 0);
   end record
   with Size => 8;
   for Status_Register use record
      T_DA       at 0 range 0 .. 0;
      P_DA       at 0 range 1 .. 1;
      Reserved_1 at 0 range 2 .. 3;
      T_OR       at 0 range 4 .. 4;
      P_OR       at 0 range 5 .. 5;
      Reserved_2 at 0 range 6 .. 7;
   end record;

   --  FIFO control register
   type FIFO_Mode is (Bypass,
                      FIFO,
                      Stream,
                      Stream_While_Trigger_Then_FIFO,
                      Bypass_While_Trigger_Then_Stream,
                      FIFO_Mean,
                      Bypass_While_Trigger_Then_FIFO) with Size => 3;
   for FIFO_Mode use (Bypass                           => 0,
                      FIFO                             => 1,
                      Stream                           => 2,
                      Stream_While_Trigger_Then_FIFO   => 3,
                      Bypass_While_Trigger_Then_Stream => 4,
                      FIFO_Mean                        => 6,
                      Bypass_While_Trigger_Then_FIFO   => 7);
   type Watermark_Level is (Not_Used,
                            Average_Over_2,
                            Average_Over_4,
                            Average_Over_8,
                            Average_Over_16,
                            Average_Over_32) with Size => 5;
   for Watermark_Level use (Not_Used        => 0,
                            Average_Over_2  => 2#00001#,
                            Average_Over_4  => 2#00011#,
                            Average_Over_8  => 2#00111#,
                            Average_Over_16 => 2#01111#,
                            Average_Over_32 => 2#11111#);
   type FIFO_Ctrl_Register is record
      WTM_POINT : Watermark_Level := Not_Used;
      F_MODE    : FIFO_Mode       := Bypass;
   end record
   with Size => 8;
   for FIFO_Ctrl_Register use record
      WTM_POINT at 0 range 0 .. 4;
      F_MODE    at 0 range 5 .. 7;
   end record;

   --  Left out FIFO Status

end LPS25H;
