------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2016, AdaCore                        --
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
------------------------------------------------------------------------------

--  Based on ft6x06.h from MCD Application Team

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Unchecked_Conversion;

with STM32.Board;   use STM32.Board;
with STM32.Device;  use STM32.Device;
with STM32.I2C;     use STM32.I2C;
with STM32.GPIO;    use STM32.GPIO;
with STM32.LCD;

package body STM32.Touch_Panel is

   --  I2C Slave address of touchscreen FocalTech FT5336
   TP_ADDR  : constant := 16#54#;

   procedure My_Delay (Ms : Integer);
   --  Wait the specified number of milliseconds

   function TP_Read (Reg : Byte; Status : out I2C_Status) return Byte
     with Inline;
   --  Reads a Touch Panel register value

   procedure TP_Write (Reg : Byte; Data :  Byte; Status : out I2C_Status)
     with Inline;
   --  Write a Touch Panel register value

   procedure TP_Init_Pins;
   --  Initializes the Touch Panel GPIO pins

   procedure TP_I2C_Config;
   --  Initializes the I2C bus

   function Check_Id return Boolean;
   --  Check the device Id: returns true on a FT5336 touch panel, False is
   --  none is found.

   procedure TP_Set_Use_Interrupts (Enabled : Boolean);
   --  Whether the touch panel uses interrupts of polling to process touch
   --  information

   function Get_Touch_State
     (Touch_Id : Byte;
      Status   : out I2C_Status) return TP_Touch_State;
   --  Retrieves the position and pressure information of the specified
   --  touch

   pragma Warnings (Off, "* is not referenced");

   ------------------------------------------------------------
   -- Definitions for FT6206 I2C register addresses on 8 bit --
   ------------------------------------------------------------

   --  Current mode register of the FT6206 (R/W)
   FT6206_DEV_MODE_REG                 : constant Byte := 16#00#;

   --  Possible values of FT6206_DEV_MODE_REG
   FT6206_DEV_MODE_WORKING             : constant Byte := 16#00#;
   FT6206_DEV_MODE_FACTORY             : constant Byte := 16#04#;

   FT6206_DEV_MODE_MASK                : constant Byte := 16#07#;
   FT6206_DEV_MODE_SHIFT               : constant Byte := 16#04#;

   --  Gesture ID register
   FT6206_GEST_ID_REG                  : constant Byte := 16#01#;

   --  Possible values of FT6206_GEST_ID_REG
   FT6206_GEST_ID_NO_GESTURE           : constant Byte := 16#00#;
   FT6206_GEST_ID_MOVE_UP              : constant Byte := 16#10#;
   FT6206_GEST_ID_MOVE_RIGHT           : constant Byte := 16#14#;
   FT6206_GEST_ID_MOVE_DOWN            : constant Byte := 16#18#;
   FT6206_GEST_ID_MOVE_LEFT            : constant Byte := 16#1C#;
   FT6206_GEST_ID_ZOOM_IN              : constant Byte := 16#40#;
   FT6206_GEST_ID_ZOOM_OUT             : constant Byte := 16#49#;

   --  Touch Data Status register : gives number of active touch points (0..5)
   FT6206_TD_STAT_REG                  : constant Byte := 16#02#;

   --  Values related to FT6206_TD_STAT_REG
   FT6206_TD_STAT_MASK                 : constant Byte := 16#0F#;
   FT6206_TD_STAT_SHIFT                : constant Byte := 16#00#;

   --  Values Pn_XH and Pn_YH related
   FT6206_TOUCH_EVT_FLAG_PRESS_DOWN    : constant Byte := 16#00#;
   FT6206_TOUCH_EVT_FLAG_LIFT_UP       : constant Byte := 16#01#;
   FT6206_TOUCH_EVT_FLAG_CONTACT       : constant Byte := 16#02#;
   FT6206_TOUCH_EVT_FLAG_NO_EVENT      : constant Byte := 16#03#;

   FT6206_TOUCH_EVT_FLAG_SHIFT         : constant Byte := 16#06#;
   FT6206_TOUCH_EVT_FLAG_MASK          : constant Byte := 2#1100_0000#;

   FT6206_TOUCH_POS_MSB_MASK           : constant Byte := 16#0F#;
   FT6206_TOUCH_POS_MSB_SHIFT          : constant Byte := 16#00#;

   --  Values Pn_XL and Pn_YL related
   FT6206_TOUCH_POS_LSB_MASK           : constant Byte := 16#FF#;
   FT6206_TOUCH_POS_LSB_SHIFT          : constant Byte := 16#00#;

   --  Values Pn_WEIGHT related
   FT6206_TOUCH_WEIGHT_MASK            : constant Byte := 16#FF#;
   FT6206_TOUCH_WEIGHT_SHIFT           : constant Byte := 16#00#;


   --  Values related to FT6206_Pn_MISC_REG
   FT6206_TOUCH_AREA_MASK              : constant Byte := 2#0100_0000#;
   FT6206_TOUCH_AREA_SHIFT             : constant Byte := 16#04#;

   type FT6206_Pressure_Registers is record
      XH_Reg     : Byte;
      XL_Reg     : Byte;
      YH_Reg     : Byte;
      YL_Reg     : Byte;
      --  Touch Pressure register value (R)
      Weight_Reg : Byte;
      --  Touch area register
      Misc_Reg   : Byte;
   end record;

   FT6206_Px_Regs                : constant array (Byte range <>)
                                      of FT6206_Pressure_Registers  :=
                                     (1  => (XH_Reg     => 16#03#,
                                             XL_Reg     => 16#04#,
                                             YH_Reg     => 16#05#,
                                             YL_Reg     => 16#06#,
                                             Weight_Reg => 16#07#,
                                             Misc_Reg   => 16#08#),
                                      2  => (XH_Reg     => 16#09#,
                                             XL_Reg     => 16#0A#,
                                             YH_Reg     => 16#0B#,
                                             YL_Reg     => 16#0C#,
                                             Weight_Reg => 16#0D#,
                                             Misc_Reg   => 16#0E#));

   --  Threshold for touch detection
   FT6206_TH_GROUP_REG                 : constant Byte := 16#80#;

   --  Values FT6206_TH_GROUP_REG : threshold related
   FT6206_THRESHOLD_MASK               : constant Byte := 16#FF#;
   FT6206_THRESHOLD_SHIFT              : constant Byte := 16#00#;

   --  Filter function coefficients
   FT6206_TH_DIFF_REG                  : constant Byte := 16#85#;

   --  Control register
   FT6206_CTRL_REG                     : constant Byte := 16#86#;

   --  Values related to FT6206_CTRL_REG

   --  Will keep the Active mode when there is no touching
   FT6206_CTRL_KEEP_ACTIVE_MODE        : constant Byte := 16#00#;

   --  Switching from Active mode to Monitor mode automatically when there
   --  is no touching
   FT6206_CTRL_KEEP_AUTO_SWITCH_MONITOR_MODE : constant Byte := 16#01#;

   --  The time period of switching from Active mode to Monitor mode when
   --  there is no touching
   FT6206_TIMEENTERMONITOR_REG               : constant Byte := 16#87#;

   --  Report rate in Active mode
   FT6206_PERIODACTIVE_REG             : constant Byte := 16#88#;

   --  Report rate in Monitor mode
   FT6206_PERIODMONITOR_REG            : constant Byte := 16#89#;

   --  The value of the minimum allowed angle while Rotating gesture mode
   FT6206_RADIAN_VALUE_REG             : constant Byte := 16#91#;

   --  Maximum offset while Moving Left and Moving Right gesture
   FT6206_OFFSET_LEFT_RIGHT_REG        : constant Byte := 16#92#;

   --  Maximum offset while Moving Up and Moving Down gesture
   FT6206_OFFSET_UP_DOWN_REG           : constant Byte := 16#93#;

   --  Minimum distance while Moving Left and Moving Right gesture
   FT6206_DISTANCE_LEFT_RIGHT_REG      : constant Byte := 16#94#;

   --  Minimum distance while Moving Up and Moving Down gesture
   FT6206_DISTANCE_UP_DOWN_REG         : constant Byte := 16#95#;

   --  Maximum distance while Zoom In and Zoom Out gesture
   FT6206_DISTANCE_ZOOM_REG            : constant Byte := 16#96#;

   --  High 8-bit of LIB Version info
   FT6206_LIB_VER_H_REG                : constant Byte := 16#A1#;

   --  Low 8-bit of LIB Version info
   FT6206_LIB_VER_L_REG                : constant Byte := 16#A2#;

   --  Chip Selecting
   FT6206_CIPHER_REG                   : constant Byte := 16#A3#;

   --  Interrupt mode register (used when in interrupt mode)
   FT6206_GMODE_REG                    : constant Byte := 16#A4#;

   FT6206_G_MODE_INTERRUPT_MASK        : constant Byte := 16#03#;

   --  Possible values of FT6206_GMODE_REG
   FT6206_G_MODE_INTERRUPT_POLLING     : constant Byte := 16#00#;
   FT6206_G_MODE_INTERRUPT_TRIGGER     : constant Byte := 16#01#;

   --  Current power mode the FT6206 system is in (R)
   FT6206_PWR_MODE_REG                 : constant Byte := 16#A5#;

   --  FT6206 firmware version
   FT6206_FIRMID_REG                   : constant Byte := 16#A6#;

   --  FT6206 Chip identification register
   FT6206_CHIP_ID_REG                  : constant Byte := 16#A8#;

   --   Possible values of FT6206_CHIP_ID_REG
   FT6206_ID_VALUE                     : constant Byte := 16#11#;

   --  Release code version
   FT6206_RELEASE_CODE_ID_REG          : constant Byte := 16#AF#;

   --  Current operating mode the FT6206 system is in (R)
   FT6206_STATE_REG                    : constant Byte := 16#BC#;

   pragma Warnings (On, "* is not referenced");

   --------------
   -- My_Delay --
   --------------

   procedure My_Delay (Ms : Integer) is
      Next_Start : Time := Clock;
      Period    : constant Time_Span := Milliseconds (Ms);
   begin
      Next_Start := Next_Start + Period;
      delay until Next_Start;
   end My_Delay;

   -------------
   -- TS_Read --
   -------------

   function TP_Read (Reg : Byte; Status : out I2C_Status) return Byte
   is
      Ret : I2C_Data (1 .. 1);
   begin
      STM32.I2C.Mem_Read
        (TP_I2C,
         TP_ADDR,
         Short (Reg),
         Memory_Size_8b,
         Ret,
         Status,
         1000);
      return Ret (1);
   end TP_Read;

   -------------
   -- TS_Read --
   -------------

   procedure TP_Write (Reg : Byte; Data :  Byte; Status : out I2C_Status)
   is
   begin
      STM32.I2C.Mem_Write
        (TP_I2C,
         TP_ADDR,
         Short (Reg),
         Memory_Size_8b,
         (1 => Data),
         Status,
         1000);
   end TP_Write;

   ---------------
   -- Init_Pins --
   ---------------

   procedure TP_Init_Pins
   is
      Pins : constant GPIO_Points := TP_Pins;
   begin
      Enable_Clock (Pins);

      Reset (TP_I2C);

      Configure_Alternate_Function (Pins, GPIO_AF_I2C1);
      Configure_IO (Pins,
                    (Speed       => Speed_25MHz,
                     Mode        => Mode_AF,
                     Output_Type => Open_Drain,
                     Resistors   => Floating));
      Lock (Pins);

      Enable_Clock (TP_INT);

      Configure_IO (TP_INT,
                    (Speed       => Speed_50MHz,
                     Mode        => Mode_In,
                     Output_Type => Open_Drain,
                     Resistors   => Pull_Up));
      Lock (TP_INT);
   end TP_Init_Pins;

   -------------------
   -- TP_I2C_Config --
   -------------------

   procedure TP_I2C_Config
   is
      I2C_Conf : I2C_Configuration;
   begin
      --  Wait at least 200ms after power up before accessing the TP registers
      My_Delay (200);

      if not I2C.Port_Enabled (TP_I2C) then
         Reset (TP_I2C);

         I2C_Conf.Own_Address := 16#00#;
         I2C_Conf.Addressing_Mode := Addressing_Mode_7bit;
         I2C_Conf.General_Call_Enabled := False;
         I2C_Conf.Clock_Stretching_Enabled := True;

         I2C_Conf.Clock_Speed := 100_000;

         Configure (TP_I2C, I2C_Conf);
      end if;
   end TP_I2C_Config;

   ---------------------------
   -- TP_Set_Use_Interrupts --
   ---------------------------

   procedure TP_Set_Use_Interrupts (Enabled : Boolean)
   is
      Reg_Value : Byte := 0;
      Status    : I2C_Status with Unreferenced;
   begin
      if Enabled then
         Reg_Value := FT6206_G_MODE_INTERRUPT_TRIGGER;
      else
         Reg_Value := FT6206_G_MODE_INTERRUPT_POLLING;
      end if;

      TP_Write (FT6206_GMODE_REG, Reg_Value, Status);
   end TP_Set_Use_Interrupts;

   -------------
   -- Read_Id --
   -------------

   function Check_Id return Boolean
   is
      Id     : Byte;
      Status : I2C_Status;
   begin
      for J in 1 .. 3 loop
         Id := TP_Read (FT6206_CHIP_ID_REG, Status);

         if Id = FT6206_ID_VALUE then
            return True;
         end if;

         if Status = Err_Error then
            return False;
         end if;
      end loop;

      return False;
   end Check_Id;

   ----------------
   -- Initialize --
   ----------------

   function Initialize return Boolean is
   begin
      TP_Init_Pins;
      TP_I2C_Config;

      TP_Set_Use_Interrupts (False);

      return Check_Id;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Ret : Boolean with Unreferenced;
   begin
      Ret := Initialize;
   end Initialize;

   ------------------
   -- Detect_Touch --
   ------------------

   function Detect_Touch return Natural
   is
     Status   : I2C_Status;
     Nb_Touch : Byte := 0;
   begin
      Nb_Touch := TP_Read (FT6206_TD_STAT_REG, Status);

      if Status /= Ok then
         return 0;
      end if;

      Nb_Touch := Nb_Touch and FT6206_TD_STAT_MASK;

      if Nb_Touch > FT6206_Px_Regs'Last then
         --  Overflow: set to 0
         Nb_Touch := 0;
      end if;

      return Natural (Nb_Touch);
   end Detect_Touch;

   ---------------------
   -- Get_Touch_State --
   ---------------------

   function Get_Touch_State
     (Touch_Id : Byte;
      Status   : out I2C_Status) return TP_Touch_State
   is
      type Short_HL_Type is record
         High, Low : Byte;
      end record with Size => 16;
      for Short_HL_Type use record
         High at 1 range 0 .. 7;
         Low  at 0 range 0 .. 7;
      end record;

      function To_Short is
        new Ada.Unchecked_Conversion (Short_HL_Type, Short);

      RX   : Natural;
      RY   : Natural;
      Rtmp : Natural;
      Ret  : TP_Touch_State;
      Regs : FT6206_Pressure_Registers;
      Tmp  : Short_HL_Type;

   begin
      if Touch_Id > 10 then
         Status := Err_Error;
         return (others => 0);
      end if;

      --  X/Y are swaped from the screen coordinates

      Regs := FT6206_Px_Regs (Touch_Id);

      Tmp.Low := TP_Read (Regs.XL_Reg, Status);

      if Status /= Ok then
         return Ret;
      end if;

      Tmp.High := TP_Read (Regs.XH_Reg, Status) and FT6206_TOUCH_POS_MSB_MASK;

      if Status /= Ok then
         return Ret;
      end if;

      RY := Natural (To_Short (Tmp) - 1);

      Tmp.Low := TP_Read (Regs.YL_Reg, Status);

      if Status /= Ok then
         return Ret;
      end if;

      Tmp.High := TP_Read (Regs.YH_Reg, Status) and FT6206_TOUCH_POS_MSB_MASK;

      if Status /= Ok then
         return Ret;
      end if;

      RX := Natural (To_Short (Tmp) - 1);

      Ret.Weight := Natural (TP_Read (Regs.Weight_Reg, Status));

      if Status /= Ok then
         Ret.Weight := 0;
         return Ret;
      end if;

      if Ret.Weight = 0 then
         Ret.Weight := 50;
      end if;

      if LCD.SwapXY then
         RTmp := RX;
         RX   := RY;
         RY   := RTmp;
         RX   := STM32.LCD.Pixel_Width - RX - 1;
      end if;

      RX := Natural'Max (0, RX);
      RY := Natural'Max (0, RY);
      RX := Natural'Min (LCD.Pixel_Width - 1, RX);
      RY := Natural'Min (LCD.Pixel_Height - 1, RY);

      --  ??? On the STM32F426, Y is returned reverted
      RY := STM32.LCD.LCD_Natural_Height - RY - 1;

      Ret.X := RX;
      Ret.Y := RY;

      return Ret;
   end Get_Touch_State;

   ---------------
   -- Get_State --
   ---------------

   function Get_State return TP_State
   is
      Status  : I2C_Status;
      N_Touch : constant Natural := Detect_Touch;
      State   : TP_State (1 .. N_Touch);

   begin
      if N_Touch = 0 then
         return (1 .. 0 => <>);
      end if;

      for J in State'Range loop
         State (J) := Get_Touch_State (Byte (J), Status);
         if Status /= Ok then
            return (1 .. 0 => <>);
         end if;
      end loop;

      return State;
   end Get_State;

end STM32.Touch_Panel;
