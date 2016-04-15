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
--   @file    stm32f429i_discovery_ts.c                                     --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   This file provides a set of functions needed to manage Touch  --
--            screen available with STMPE811 IO Expander device mounted on  --
--            STM32F429I-Discovery Kit.                                     --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with Ada.Real_Time;        use Ada.Real_Time;

with Interfaces.Bit_Types; use Interfaces, Interfaces.Bit_Types;

with STM32.LCD;
with STM32.I2C;            use STM32.I2C;
with STM32.GPIO;           use STM32.GPIO;

with STM32.Device;         use STM32.Device;

with HAL.I2C;

package body Touch_Panel is

   use type HAL.I2C.I2C_Status;

   SCL      : GPIO_Point renames PA8;
   SCL_AF   : constant GPIO_Alternate_Function := GPIO_AF_I2C;

   SDA      : GPIO_Point renames PC9;
   SDA_AF   : constant GPIO_Alternate_Function := GPIO_AF_I2C;

   TP_I2C   : I2C_Port renames I2C_3;

   IOE_ADDR : constant Byte := 16#82#;

   pragma Warnings (Off, "constant * is not referenced");
   --  Control Registers
   IOE_REG_SYS_CTRL1    : constant Byte := 16#03#;
   IOE_REG_SYS_CTRL2    : constant Byte := 16#04#;
   IOE_REG_SPI_CFG      : constant Byte := 16#08#;

   --  Touch Panel Registers
   IOE_REG_TSC_CTRL     : constant Byte := 16#40#;
   IOE_REG_TSC_CFG      : constant Byte := 16#41#;
   IOE_REG_WDM_TR_X     : constant Byte := 16#42#;
   IOE_REG_WDM_TR_Y     : constant Byte := 16#44#;
   IOE_REG_WDM_BL_X     : constant Byte := 16#46#;
   IOE_REG_WDM_BL_Y     : constant Byte := 16#48#;
   IOE_REG_FIFO_TH      : constant Byte := 16#4A#;
   IOE_REG_FIFO_STA     : constant Byte := 16#4B#;
   IOE_REG_FIFO_SIZE    : constant Byte := 16#4C#;
   IOE_REG_TSC_DATA_X   : constant Byte := 16#4D#;
   IOE_REG_TSC_DATA_Y   : constant Byte := 16#4F#;
   IOE_REG_TSC_DATA_Z   : constant Byte := 16#51#;
   IOE_REG_TSC_DATA_XYZ : constant Byte := 16#52#;
   IOE_REG_TSC_FRACT_Z  : constant Byte := 16#56#;
   IOE_REG_TSC_DATA     : constant Byte := 16#57#;
   IOE_REG_TSC_I_DRIVE  : constant Byte := 16#58#;
   IOE_REG_TSC_SHIELD   : constant Byte := 16#59#;

   --  IOE GPIO Registers
   IOE_REG_GPIO_SET_PIN : constant Byte := 16#10#;
   IOE_REG_GPIO_CLR_PIN : constant Byte := 16#11#;
   IOE_REG_GPIO_MP_STA  : constant Byte := 16#12#;
   IOE_REG_GPIO_DIR     : constant Byte := 16#13#;
   IOE_REG_GPIO_ED      : constant Byte := 16#14#;
   IOE_REG_GPIO_RE      : constant Byte := 16#15#;
   IOE_REG_GPIO_FE      : constant Byte := 16#16#;
   IOE_REG_GPIO_AF      : constant Byte := 16#17#;

   --  IOE Functions
   IOE_ADC_FCT          : constant Byte := 16#01#;
   IOE_TSC_FCT          : constant Byte := 16#02#;
   IOE_IO_FCT           : constant Byte := 16#04#;

   --  ADC Registers
   IOE_REG_ADC_INT_EN   : constant Byte := 16#0E#;
   IOE_REG_ADC_INT_STA  : constant Byte := 16#0F#;
   IOE_REG_ADC_CTRL1    : constant Byte := 16#20#;
   IOE_REG_ADC_CTRL2    : constant Byte := 16#21#;
   IOE_REG_ADC_CAPT     : constant Byte := 16#22#;
   IOE_REG_ADC_DATA_CH0 : constant Byte := 16#30#;
   IOE_REG_ADC_DATA_CH1 : constant Byte := 16#32#;
   IOE_REG_ADC_DATA_CH2 : constant Byte := 16#34#;
   IOE_REG_ADC_DATA_CH3 : constant Byte := 16#36#;
   IOE_REG_ADC_DATA_CH4 : constant Byte := 16#38#;
   IOE_REG_ADC_DATA_CH5 : constant Byte := 16#3A#;
   IOE_REG_ADC_DATA_CH6 : constant Byte := 16#3B#;
   IOE_REG_ADC_DATA_CH7 : constant Byte := 16#3C#;

   --  Interrupt Control Registers
   IOE_REG_INT_CTRL     : constant Byte := 16#09#;
   IOE_REG_INT_EN       : constant Byte := 16#0A#;
   IOE_REG_INT_STA      : constant Byte := 16#0B#;
   IOE_REG_GPIO_INT_EN  : constant Byte := 16#0C#;
   IOE_REG_GPIO_INT_STA : constant Byte := 16#0D#;

   --  touch Panel Pins
   TOUCH_YD             : constant Byte := 16#02#;
   TOUCH_XD             : constant Byte := 16#04#;
   TOUCH_YU             : constant Byte := 16#08#;
   TOUCH_XU             : constant Byte := 16#10#;
   TOUCH_IO_ALL         : constant Byte :=
     TOUCH_YD or TOUCH_XD or TOUCH_YU or TOUCH_XU;
   pragma Warnings (On, "constant * is not referenced");

   subtype TSC_Data is HAL.I2C.I2C_Data;

   ---------------
   -- Read_Data --
   ---------------

   function Read_Data (Data_Addr : Byte; Length : Natural) return TSC_Data
   is
      Data   : TSC_Data (1 .. Length);
      Status : HAL.I2C.I2C_Status;

   begin
      TP_I2C.Mem_Read (UInt10 (IOE_ADDR),
                       Short (Data_Addr),
                       HAL.I2C.Memory_Size_8b,
                       Data,
                       Status);

      if Status /= HAL.I2C.Ok then
         raise Program_Error with "Timeout while reading TC data";
      end if;

      return Data;
   end Read_Data;

   -------------------
   -- Read_Register --
   -------------------

   function Read_Register (Reg_Addr : Byte) return Byte
   is
      Data : TSC_Data (1 .. 1);
      Status : HAL.I2C.I2C_Status;

   begin
      TP_I2C.Mem_Read (UInt10 (IOE_ADDR),
                       Short (Reg_Addr),
                       HAL.I2C.Memory_Size_8b,
                       Data,
                       Status);

      if Status /= HAL.I2C.Ok then
         raise Program_Error with "Timeout while reading TC data";
      end if;

      return Data (1);
   end Read_Register;

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register (Reg_Addr : Byte; Data : Byte) is
      Status : HAL.I2C.I2C_Status;

   begin
      TP_I2C.Mem_Write (UInt10 (IOE_ADDR),
                        Short (Reg_Addr),
                        HAL.I2C.Memory_Size_8b,
                        (1 => Data),
                        Status);

      if Status /= HAL.I2C.Ok then
         raise Program_Error with "Timeout while reading TC data";
      end if;
   end Write_Register;

   -------------------
   -- TP_Ctrl_Lines --
   -------------------

   procedure TP_Ctrl_Lines is
      GPIO_Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (GPIO_Points'(SDA, SCL));

      Enable_Clock (TP_I2C);

      Reset (TP_I2C);

      Enable_Clock (TP_I2C);

      SCL.Configure_Alternate_Function (SCL_AF);
      SDA.Configure_Alternate_Function (SDA_AF);

      GPIO_Conf.Speed       := Speed_25MHz;
      GPIO_Conf.Mode        := Mode_AF;
      GPIO_Conf.Output_Type := Open_Drain;
      GPIO_Conf.Resistors   := Floating;
      Configure_IO (GPIO_Points'(SCL, SDA), GPIO_Conf);

      SCL.Lock;
      SDA.Lock;
   end TP_Ctrl_Lines;

   -------------------
   -- TP_I2C_Config --
   -------------------

   procedure TP_I2C_Config is
   begin
      if not TP_I2C.Port_Enabled then
         Reset (TP_I2C);

         TP_I2C.Configure
           ((Mode                     => I2C_Mode,
             Duty_Cycle               => DutyCycle_2,
             Own_Address              => 16#00#,
             Addressing_Mode          => Addressing_Mode_7bit,
             General_Call_Enabled     => False,
             Clock_Stretching_Enabled => True,
             Clock_Speed              => 100_000));
      end if;
   end TP_I2C_Config;

   ---------------
   -- IOE_Reset --
   ---------------

   procedure IOE_Reset is
   begin
      Write_Register (IOE_REG_SYS_CTRL1, 16#02#);

      --  Give some time for the reset
      delay until Clock + Milliseconds (2);

      Write_Register (IOE_REG_SYS_CTRL1, 16#00#);
   end IOE_Reset;

   --------------------------
   -- IOE_Function_Command --
   --------------------------

   procedure IOE_Function_Command (Func : Byte; Enabled : Boolean) is
      Reg : Byte := Read_Register (IOE_REG_SYS_CTRL2);
   begin
      --  CTRL2 functions are disabled when corresponding bit is set

      if Enabled then
         Reg := Reg and (not Func);
      else
         Reg := Reg or Func;
      end if;

      Write_Register (IOE_REG_SYS_CTRL2, Reg);
   end IOE_Function_Command;

   -------------------
   -- IOE_AF_Config --
   -------------------

   procedure IOE_AF_Config (Pin : Byte; Enabled : Boolean) is
      Reg : Byte := Read_Register (IOE_REG_GPIO_AF);
   begin
      if Enabled then
         Reg := Reg or Pin;
      else
         Reg := Reg and (not Pin);
      end if;

      Write_Register (IOE_REG_GPIO_AF, Reg);
   end IOE_AF_Config;

   ----------------
   -- Get_IOE_ID --
   ----------------

   function Get_IOE_ID return Short is
   begin
      return (Short (Read_Register (0)) * (2**8))
        or Short (Read_Register (1));
   end Get_IOE_ID;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if not Initialize then
         Raise Program_Error;
      end if;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   function Initialize return Boolean is
   begin
      TP_Ctrl_Lines;
      TP_I2C_Config;

      delay until Clock + Milliseconds (100);

      if Get_IOE_ID /= 16#0811# then
         return False;
      end if;

      IOE_Reset;

      IOE_Function_Command (IOE_ADC_FCT, true);
      IOE_Function_Command (IOE_TSC_FCT, true);

      Write_Register (IOE_REG_ADC_CTRL1, 16#49#);

      delay until Clock + Milliseconds (2);

      Write_Register (IOE_REG_ADC_CTRL2, 16#01#);

      IOE_AF_Config (TOUCH_IO_ALL, false);

      Write_Register (IOE_REG_TSC_CFG, 16#9A#);

      Write_Register (IOE_REG_FIFO_TH, 16#01#);

      Write_Register (IOE_REG_FIFO_STA, 16#01#);

      Write_Register (IOE_REG_FIFO_TH, 16#00#);

      Write_Register (IOE_REG_TSC_FRACT_Z, 16#00#);

      Write_Register (IOE_REG_TSC_I_DRIVE, 16#01#);

      Write_Register (IOE_REG_TSC_CTRL, 16#01#);

      Write_Register (IOE_REG_INT_STA, 16#FF#);

      return True;
   end Initialize;

   ------------------
   -- Detect_Touch --
   ------------------

   function Detect_Touch return Natural
   is
      Val : constant Byte := Read_Register (IOE_REG_TSC_CTRL) and 16#80#;
   begin
      if Val = 0 then
         Write_Register (IOE_REG_FIFO_STA, 16#01#);
         Write_Register (IOE_REG_FIFO_STA, 16#00#);

         return 0;
      else
         return 1;
      end if;
   end Detect_Touch;

   ---------------
   -- Get_State --
   ---------------

   function Get_State return TP_State
   is
      State     : TP_Touch_State;
      Raw_X     : Word;
      Raw_Y     : Word;
      Raw_Z     : Word;
      X         : Integer;
      Y         : Integer;
   begin

      --  Check Touch detected bit in CTRL register
      if Detect_Touch = 0 then
         return (1 .. 0 => <>);
      end if;

      declare
         Data_X : constant TSC_Data := Read_Data (IOE_REG_TSC_DATA_X, 2);
         Data_Y : constant TSC_Data := Read_Data (IOE_REG_TSC_DATA_Y, 2);
         Data_Z : constant TSC_Data := Read_Data (IOE_REG_TSC_DATA_Z, 1);
         Z_Frac : constant TSC_Data := Read_Data (IOE_REG_TSC_FRACT_Z, 1);

      begin
         Raw_X := 2 ** 12 -
           (Shift_Left (Word (Data_X (1)) and 16#0F#, 8) or Word (Data_X (2)));
         Raw_Y :=
           Shift_Left (Word (Data_Y (1)) and 16#0F#, 8) or Word (Data_Y (2));
         Raw_Z :=
           Shift_Right (Word (Data_Z (1)), Natural (Z_Frac (1) and 2#111#));
      end;

      --  Now we adapt the 12 bit resolution to the LCD width.
      X :=
        Integer
          (Shift_Right
             (Raw_X * (Word (STM32.LCD.LCD_Natural_Width) + 32), 12)) - 16;
      X := Integer'Max (X, 0);
      X := Integer'Min (X, STM32.LCD.LCD_Natural_Width - 1);

      --  Do the same for Y
      Y := Integer (Raw_Y);
      Y := Y - 360;
      Y := Y / 11;
      Y := Integer'Max (Y, 0);
      Y := Integer'Min (Y, STM32.LCD.LCD_Natural_Height - 1);

      --  ??? There seems to be a strange behavior of this touch panel where
      --  sometimes it reports dummy points at X = LCD_Natural_Width.
      --  Let's filter this out
      if X = STM32.LCD.LCD_Natural_Width - 1 then
         return (1 .. 0 => <>);
      end if;

      if not STM32.LCD.SwapXY then
         State.X := X;
         State.Y := Y;
      else
         State.X := STM32.LCD.LCD_Natural_Height - Y - 1;
         State.Y := X;
      end if;

      State.Weight := Integer'Max (Integer (Raw_Z), 8);

      Write_Register (IOE_REG_FIFO_STA, 16#01#);
      Write_Register (IOE_REG_FIFO_STA, 16#00#);

      return (1 => State);
   end Get_State;

end Touch_Panel;
