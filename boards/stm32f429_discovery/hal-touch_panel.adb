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

with STMPE811;             use STMPE811;

package body HAL.Touch_Panel is

   SCL      : GPIO_Point renames PA8;
   SCL_AF   : constant GPIO_Alternate_Function := GPIO_AF_I2C;

   SDA      : GPIO_Point renames PC9;
   SDA_AF   : constant GPIO_Alternate_Function := GPIO_AF_I2C;

   TP_I2C   : I2C_Port_Id renames I2C_3;

   IOE_ADDR : constant Byte := 16#82#;

   pragma Warnings (Off, "constant * is not referenced");
   pragma Warnings (On, "constant * is not referenced");

   subtype TSC_Data is I2C_Data;

   ---------------
   -- Read_Data --
   ---------------

   function Read_Data (Data_Addr : Byte; Length : Natural) return TSC_Data
   is
      Data   : TSC_Data (1 .. Length);
      Status : I2C_Status;

   begin
      Mem_Read (TP_I2C, UInt10 (IOE_ADDR),
                Short (Data_Addr), Memory_Size_8b, Data, Status);

      if Status /= Ok then
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
      Status : I2C_Status;

   begin
      Mem_Read (TP_I2C, UInt10 (IOE_ADDR),
                Short (Reg_Addr), Memory_Size_8b, Data, Status);

      if Status /= Ok then
         raise Program_Error with "Timeout while reading TC data";
      end if;

      return Data (1);
   end Read_Register;

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register (Reg_Addr : Byte; Data : Byte) is
      Status : I2C_Status;

   begin
      Mem_Write (TP_I2C, UInt10 (IOE_ADDR),
                Short (Reg_Addr), Memory_Size_8b, (1 => Data), Status);

      if Status /= Ok then
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

      Configure_Alternate_Function (SCL, SCL_AF);
      Configure_Alternate_Function (SDA, SDA_AF);

      GPIO_Conf.Speed       := Speed_25MHz;
      GPIO_Conf.Mode        := Mode_AF;
      GPIO_Conf.Output_Type := Open_Drain;
      GPIO_Conf.Resistors   := Floating;
      Configure_IO (GPIO_Points'(SCL, SDA), GPIO_Conf);

      Lock (SCL);
      Lock (SDA);
   end TP_Ctrl_Lines;

   -------------------
   -- TP_I2C_Config --
   -------------------

   procedure TP_I2C_Config is
   begin
      if not Port_Enabled (TP_I2C) then
         Reset (TP_I2C);

         Configure
           (TP_I2C,
            (Mode                     => I2C_Mode,
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

end HAL.Touch_Panel;
