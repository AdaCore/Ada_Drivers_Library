------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2016, AdaCore                         --
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

package body STMPE811 is

   pragma Warnings (Off, "constant * is not referenced");
   --  Control Registers
   IOE_REG_SYS_CTRL1    : constant UInt8 := 16#03#;
   IOE_REG_SYS_CTRL2    : constant UInt8 := 16#04#;
   IOE_REG_SPI_CFG      : constant UInt8 := 16#08#;

   --  Touch Panel Registers
   IOE_REG_TSC_CTRL     : constant UInt8 := 16#40#;
   IOE_REG_TSC_CFG      : constant UInt8 := 16#41#;
   IOE_REG_WDM_TR_X     : constant UInt8 := 16#42#;
   IOE_REG_WDM_TR_Y     : constant UInt8 := 16#44#;
   IOE_REG_WDM_BL_X     : constant UInt8 := 16#46#;
   IOE_REG_WDM_BL_Y     : constant UInt8 := 16#48#;
   IOE_REG_FIFO_TH      : constant UInt8 := 16#4A#;
   IOE_REG_FIFO_STA     : constant UInt8 := 16#4B#;
   IOE_REG_FIFO_SIZE    : constant UInt8 := 16#4C#;
   IOE_REG_TSC_DATA_X   : constant UInt8 := 16#4D#;
   IOE_REG_TSC_DATA_Y   : constant UInt8 := 16#4F#;
   IOE_REG_TSC_DATA_Z   : constant UInt8 := 16#51#;
   IOE_REG_TSC_DATA_XYZ : constant UInt8 := 16#52#;
   IOE_REG_TSC_FRACT_Z  : constant UInt8 := 16#56#;
   IOE_REG_TSC_DATA     : constant UInt8 := 16#57#;
   IOE_REG_TSC_I_DRIVE  : constant UInt8 := 16#58#;
   IOE_REG_TSC_SHIELD   : constant UInt8 := 16#59#;

   --  IOE GPIO Registers
   IOE_REG_GPIO_SET_PIN : constant UInt8 := 16#10#;
   IOE_REG_GPIO_CLR_PIN : constant UInt8 := 16#11#;
   IOE_REG_GPIO_MP_STA  : constant UInt8 := 16#12#;
   IOE_REG_GPIO_DIR     : constant UInt8 := 16#13#;
   IOE_REG_GPIO_ED      : constant UInt8 := 16#14#;
   IOE_REG_GPIO_RE      : constant UInt8 := 16#15#;
   IOE_REG_GPIO_FE      : constant UInt8 := 16#16#;
   IOE_REG_GPIO_AF      : constant UInt8 := 16#17#;

   --  IOE Functions
   IOE_ADC_FCT          : constant UInt8 := 16#01#;
   IOE_TSC_FCT          : constant UInt8 := 16#02#;
   IOE_IO_FCT           : constant UInt8 := 16#04#;

   --  ADC Registers
   IOE_REG_ADC_INT_EN   : constant UInt8 := 16#0E#;
   IOE_REG_ADC_INT_STA  : constant UInt8 := 16#0F#;
   IOE_REG_ADC_CTRL1    : constant UInt8 := 16#20#;
   IOE_REG_ADC_CTRL2    : constant UInt8 := 16#21#;
   IOE_REG_ADC_CAPT     : constant UInt8 := 16#22#;
   IOE_REG_ADC_DATA_CH0 : constant UInt8 := 16#30#;
   IOE_REG_ADC_DATA_CH1 : constant UInt8 := 16#32#;
   IOE_REG_ADC_DATA_CH2 : constant UInt8 := 16#34#;
   IOE_REG_ADC_DATA_CH3 : constant UInt8 := 16#36#;
   IOE_REG_ADC_DATA_CH4 : constant UInt8 := 16#38#;
   IOE_REG_ADC_DATA_CH5 : constant UInt8 := 16#3A#;
   IOE_REG_ADC_DATA_CH6 : constant UInt8 := 16#3B#;
   IOE_REG_ADC_DATA_CH7 : constant UInt8 := 16#3C#;

   --  Interrupt Control Registers
   IOE_REG_INT_CTRL     : constant UInt8 := 16#09#;
   IOE_REG_INT_EN       : constant UInt8 := 16#0A#;
   IOE_REG_INT_STA      : constant UInt8 := 16#0B#;
   IOE_REG_GPIO_INT_EN  : constant UInt8 := 16#0C#;
   IOE_REG_GPIO_INT_STA : constant UInt8 := 16#0D#;

   --  touch Panel Pins
   TOUCH_YD             : constant UInt8 := 16#02#;
   TOUCH_XD             : constant UInt8 := 16#04#;
   TOUCH_YU             : constant UInt8 := 16#08#;
   TOUCH_XU             : constant UInt8 := 16#10#;
   TOUCH_IO_ALL         : constant UInt8 :=
     TOUCH_YD or TOUCH_XD or TOUCH_YU or TOUCH_XU;
   pragma Warnings (On, "constant * is not referenced");

   ---------------
   -- Read_Data --
   ---------------

   function Read_Data (This      : in out STMPE811_Device;
                       Data_Addr : UInt8;
                       Length    : Natural) return TSC_Data
   is
      Data   : TSC_Data (1 .. Length);
      Status : I2C_Status;

   begin
      This.Port.Mem_Read (This.I2C_Addr,
                          UInt16 (Data_Addr),
                          Memory_Size_8b, Data,
                          Status);

      if Status /= Ok then
         raise Program_Error with "Timeout while reading TC data";
      end if;

      return Data;
   end Read_Data;

   -------------------
   -- Read_Register --
   -------------------

   function Read_Register (This     : STMPE811_Device;
                           Reg_Addr : UInt8) return UInt8
   is
      Data : TSC_Data (1 .. 1);
      Status : I2C_Status;

   begin
      This.Port.Mem_Read (This.I2C_Addr,
                          UInt16 (Reg_Addr),
                          Memory_Size_8b,
                          Data,
                          Status);

      if Status /= Ok then
         raise Program_Error with "Timeout while reading TC data";
      end if;

      return Data (1);
   end Read_Register;

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register (This     : in out STMPE811_Device;
                             Reg_Addr : UInt8;
                             Data     : UInt8) is
      Status : I2C_Status;

   begin
      This.Port.Mem_Write (This.I2C_Addr,
                           UInt16 (Reg_Addr),
                           Memory_Size_8b,
                           (1 => Data),
                           Status);

      if Status /= Ok then
         raise Program_Error with "Timeout while reading TC data";
      end if;
   end Write_Register;

   ---------------
   -- IOE_Reset --
   ---------------

   procedure IOE_Reset (This : in out STMPE811_Device) is
   begin
      This.Write_Register (IOE_REG_SYS_CTRL1, 16#02#);

      --  Give some time for the reset
      This.Time.Delay_Milliseconds (2);

      This.Write_Register (IOE_REG_SYS_CTRL1, 16#00#);
   end IOE_Reset;

   --------------------------
   -- IOE_Function_Command --
   --------------------------

   procedure IOE_Function_Command (This : in out STMPE811_Device;
                                   Func : UInt8;
                                   Enabled : Boolean)
   is
      Reg : UInt8 := This.Read_Register (IOE_REG_SYS_CTRL2);
   begin
      --  CTRL2 functions are disabled when corresponding bit is set

      if Enabled then
         Reg := Reg and (not Func);
      else
         Reg := Reg or Func;
      end if;

      This.Write_Register (IOE_REG_SYS_CTRL2, Reg);
   end IOE_Function_Command;

   -------------------
   -- IOE_AF_Config --
   -------------------

   procedure IOE_AF_Config (This      : in out STMPE811_Device;
                            Pin       : UInt8;
                            Enabled   : Boolean) is
      Reg : UInt8 := This.Read_Register (IOE_REG_GPIO_AF);
   begin
      if Enabled then
         Reg := Reg or Pin;
      else
         Reg := Reg and (not Pin);
      end if;

      This.Write_Register (IOE_REG_GPIO_AF, Reg);
   end IOE_AF_Config;

   ----------------
   -- Get_IOE_ID --
   ----------------

   function Get_IOE_ID (This : in out STMPE811_Device) return UInt16 is
   begin
      return (UInt16 (This.Read_Register (0)) * (2**8))
        or UInt16 (This.Read_Register (1));
   end Get_IOE_ID;

   ----------------
   -- Initialize --
   ----------------

   function Initialize (This : in out STMPE811_Device) return Boolean
   is
   begin

      This.Time.Delay_Milliseconds (100);

      if This.Get_IOE_ID /= 16#0811# then
         return False;
      end if;

      This.IOE_Reset;

      This.IOE_Function_Command (IOE_ADC_FCT, True);
      This.IOE_Function_Command (IOE_TSC_FCT, True);

      This.Write_Register (IOE_REG_ADC_CTRL1, 16#49#);

      This.Time.Delay_Milliseconds (2);

      This.Write_Register (IOE_REG_ADC_CTRL2, 16#01#);

      This.IOE_AF_Config (TOUCH_IO_ALL, False);

      This.Write_Register (IOE_REG_TSC_CFG, 16#9A#);

      This.Write_Register (IOE_REG_FIFO_TH, 16#01#);

      This.Write_Register (IOE_REG_FIFO_STA, 16#01#);

      This.Write_Register (IOE_REG_FIFO_TH, 16#00#);

      This.Write_Register (IOE_REG_TSC_FRACT_Z, 16#00#);

      This.Write_Register (IOE_REG_TSC_I_DRIVE, 16#01#);

      This.Write_Register (IOE_REG_TSC_CTRL, 16#01#);

      This.Write_Register (IOE_REG_INT_STA, 16#FF#);

      return True;
   end Initialize;

   ----------------
   -- Set_Bounds --
   ----------------

   overriding
   procedure Set_Bounds (This   : in out STMPE811_Device;
                         Width  : Natural;
                         Height : Natural;
                         Swap   : HAL.Touch_Panel.Swap_State)
   is
   begin
      This.LCD_Natural_Width := Width;
      This.LCD_Natural_Height := Height;
      This.Swap := Swap;
   end Set_Bounds;

   -------------------------
   -- Active_Touch_Points --
   -------------------------

   overriding
   function Active_Touch_Points (This : in out STMPE811_Device)
                                 return Touch_Identifier
   is
      Val : constant UInt8 := This.Read_Register (IOE_REG_TSC_CTRL) and 16#80#;
   begin
      if Val = 0 then
         This.Write_Register (IOE_REG_FIFO_STA, 16#01#);
         This.Write_Register (IOE_REG_FIFO_STA, 16#00#);

         return 0;
      else
         return 1;
      end if;
   end Active_Touch_Points;

   ---------------------
   -- Get_Touch_Point --
   ---------------------

   overriding
   function Get_Touch_Point (This     : in out STMPE811_Device;
                             Touch_Id : Touch_Identifier)
                             return TP_Touch_State
   is
      State     : TP_Touch_State;
      Raw_X     : UInt32;
      Raw_Y     : UInt32;
      Raw_Z     : UInt32;
      X         : Integer;
      Y         : Integer;
      Tmp       : Integer;

   begin

      --  Check Touch detected bit in CTRL register
      if Touch_Id /= 1 or else This.Active_Touch_Points = 0 then
         return (0, 0, 0);
      end if;

      declare
         Data_X : constant TSC_Data := This.Read_Data (IOE_REG_TSC_DATA_X, 2);
         Data_Y : constant TSC_Data := This.Read_Data (IOE_REG_TSC_DATA_Y, 2);
         Data_Z : constant TSC_Data := This.Read_Data (IOE_REG_TSC_DATA_Z, 1);
         Z_Frac : constant TSC_Data := This.Read_Data (IOE_REG_TSC_FRACT_Z, 1);

      begin
         Raw_X := 2 ** 12 -
           (Shift_Left (UInt32 (Data_X (1)) and 16#0F#, 8) or UInt32 (Data_X (2)));
         Raw_Y :=
           Shift_Left (UInt32 (Data_Y (1)) and 16#0F#, 8) or UInt32 (Data_Y (2));
         Raw_Z :=
           Shift_Right (UInt32 (Data_Z (1)), Natural (Z_Frac (1) and 2#111#));
      end;

      --  Now we adapt the 12 bit resolution to the LCD width.
      X :=
        Integer
          (Shift_Right (Raw_X * (UInt32 (This.LCD_Natural_Width) + 32), 12));
      X := X - 16;

      X := Integer'Max (X, 0);
      X := Integer'Min (X, This.LCD_Natural_Width - 1);

      --  Do the same for Y
      Y := Integer (Raw_Y);
      Y := Y - 360;
      Y := Y / 11;
      Y := Integer'Max (Y, 0);
      Y := Integer'Min (Y, This.LCD_Natural_Height - 1);

      --  ??? There seems to be a strange behavior of this touch panel where
      --  sometimes it reports dummy points at X = LCD_Natural_Width.
      --  Let's filter this out
      if X = This.LCD_Natural_Width - 1 then
         return (0, 0, 0);
      end if;

      if (This.Swap and Invert_X) /= 0 then
         X := This.LCD_Natural_Width - X - 1;
      end if;
      if (This.Swap and Invert_Y) /= 0 then
         Y := This.LCD_Natural_Height - Y - 1;
      end if;
      if (This.Swap and Swap_XY) /= 0 then
         Tmp := X;
         X := Y;
         Y := Tmp;
      end if;

      State.X := X;
      State.Y := Y;

      State.Weight := Integer'Max (Integer (Raw_Z), 8);

      This.Write_Register (IOE_REG_FIFO_STA, 16#01#);
      This.Write_Register (IOE_REG_FIFO_STA, 16#00#);

      return State;
   end Get_Touch_Point;

   --------------------------
   -- Get_All_Touch_Points --
   --------------------------

   overriding
   function Get_All_Touch_Points
     (This     : in out STMPE811_Device)
      return HAL.Touch_Panel.TP_State
   is
      N_Touch : constant Natural := This.Active_Touch_Points;
      State   : TP_State (1 .. N_Touch);

   begin
      if N_Touch = 0 then
         return (1 .. 0 => <>);
      end if;

      for J in State'Range loop
         State (J) :=  This.Get_Touch_Point (J);
      end loop;

      return State;
   end Get_All_Touch_Points;

end STMPE811;
