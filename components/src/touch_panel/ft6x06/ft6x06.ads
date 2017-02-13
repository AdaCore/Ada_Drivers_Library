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
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --UInt8
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

--  Driver package for the FT6x06 touch screen panel
--  Based on the ft6x06 driver from MCD Application Team

with HAL;             use HAL;
with HAL.I2C;         use HAL.I2C;
with HAL.Touch_Panel;

package FT6x06 is

   type FT6x06_Device (Port     : not null Any_I2C_Port;
                       I2C_Addr : I2C_Address) is
     limited new HAL.Touch_Panel.Touch_Panel_Device with private;

   function Check_Id (This : in out FT6x06_Device) return Boolean;
   --  Check the device Id: returns true on a FT5336 touch panel, False is
   --  none is found.

   procedure TP_Set_Use_Interrupts (This : in out FT6x06_Device;
                                    Enabled : Boolean);
   --  Whether the touch panel uses interrupts of polling to process touch
   --  information

   overriding
   procedure Set_Bounds (This   : in out FT6x06_Device;
                         Width  : Natural;
                         Height : Natural;
                         Swap   : HAL.Touch_Panel.Swap_State);
   --  Set screen bounds. Touch_State must should stay within screen bounds

   overriding
   function Active_Touch_Points (This : in out FT6x06_Device)
                                 return HAL.Touch_Panel.Touch_Identifier;
   --  Retrieve the number of active touch points

   overriding
   function Get_Touch_Point (This     : in out FT6x06_Device;
                             Touch_Id : HAL.Touch_Panel.Touch_Identifier)
                             return HAL.Touch_Panel.TP_Touch_State;
   --  Retrieves the position and pressure information of the specified
   --  touch

   overriding
   function Get_All_Touch_Points
     (This : in out FT6x06_Device)
      return HAL.Touch_Panel.TP_State;
   --  Retrieves the position and pressure information of every active touch
   --  points
private

   type FT6x06_Device (Port     : not null Any_I2C_Port;
                       I2C_Addr : I2C_Address) is
     limited new HAL.Touch_Panel.Touch_Panel_Device with record
      LCD_Natural_Width  : Natural := 0;
      LCD_Natural_Height : Natural := 0;
      Swap               : HAL.Touch_Panel.Swap_State := 0;
   end record;

   function I2C_Read (This   : in out FT6x06_Device;
                      Reg    : UInt8;
                      Status : out Boolean)
                      return UInt8;

   procedure I2C_Write (This   : in out FT6x06_Device;
                        Reg    : UInt8;
                        Data   : UInt8;
                        Status : out Boolean);
   ------------------------------------------------------------
   -- Definitions for FT6206 I2C register addresses on 8 bit --
   ------------------------------------------------------------

   --  Current mode register of the FT6206 (R/W)
   FT6206_DEV_MODE_REG                 : constant UInt8 := 16#00#;

   --  Possible values of FT6206_DEV_MODE_REG
   FT6206_DEV_MODE_WORKING             : constant UInt8 := 16#00#;
   FT6206_DEV_MODE_FACTORY             : constant UInt8 := 16#04#;

   FT6206_DEV_MODE_MASK                : constant UInt8 := 16#07#;
   FT6206_DEV_MODE_SHIFT               : constant UInt8 := 16#04#;

   --  Gesture ID register
   FT6206_GEST_ID_REG                  : constant UInt8 := 16#01#;

   --  Possible values of FT6206_GEST_ID_REG
   FT6206_GEST_ID_NO_GESTURE           : constant UInt8 := 16#00#;
   FT6206_GEST_ID_MOVE_UP              : constant UInt8 := 16#10#;
   FT6206_GEST_ID_MOVE_RIGHT           : constant UInt8 := 16#14#;
   FT6206_GEST_ID_MOVE_DOWN            : constant UInt8 := 16#18#;
   FT6206_GEST_ID_MOVE_LEFT            : constant UInt8 := 16#1C#;
   FT6206_GEST_ID_ZOOM_IN              : constant UInt8 := 16#40#;
   FT6206_GEST_ID_ZOOM_OUT             : constant UInt8 := 16#49#;

   --  Touch Data Status register : gives number of active touch points (0..5)
   FT6206_TD_STAT_REG                  : constant UInt8 := 16#02#;

   --  Values related to FT6206_TD_STAT_REG
   FT6206_TD_STAT_MASK                 : constant UInt8 := 16#0F#;
   FT6206_TD_STAT_SHIFT                : constant UInt8 := 16#00#;

   --  Values Pn_XH and Pn_YH related
   FT6206_TOUCH_EVT_FLAG_PRESS_DOWN    : constant UInt8 := 16#00#;
   FT6206_TOUCH_EVT_FLAG_LIFT_UP       : constant UInt8 := 16#01#;
   FT6206_TOUCH_EVT_FLAG_CONTACT       : constant UInt8 := 16#02#;
   FT6206_TOUCH_EVT_FLAG_NO_EVENT      : constant UInt8 := 16#03#;

   FT6206_TOUCH_EVT_FLAG_SHIFT         : constant UInt8 := 16#06#;
   FT6206_TOUCH_EVT_FLAG_MASK          : constant UInt8 := 2#1100_0000#;

   FT6206_TOUCH_POS_MSB_MASK           : constant UInt8 := 16#0F#;
   FT6206_TOUCH_POS_MSB_SHIFT          : constant UInt8 := 16#00#;

   --  Values Pn_XL and Pn_YL related
   FT6206_TOUCH_POS_LSB_MASK           : constant UInt8 := 16#FF#;
   FT6206_TOUCH_POS_LSB_SHIFT          : constant UInt8 := 16#00#;

   --  Values Pn_WEIGHT related
   FT6206_TOUCH_WEIGHT_MASK            : constant UInt8 := 16#FF#;
   FT6206_TOUCH_WEIGHT_SHIFT           : constant UInt8 := 16#00#;


   --  Values related to FT6206_Pn_MISC_REG
   FT6206_TOUCH_AREA_MASK              : constant UInt8 := 2#0100_0000#;
   FT6206_TOUCH_AREA_SHIFT             : constant UInt8 := 16#04#;

   type FT6206_Pressure_Registers is record
      XH_Reg     : UInt8;
      XL_Reg     : UInt8;
      YH_Reg     : UInt8;
      YL_Reg     : UInt8;
      --  Touch Pressure register value (R)
      Weight_Reg : UInt8;
      --  Touch area register
      Misc_Reg   : UInt8;
   end record;

   FT6206_Px_Regs                : constant array (Positive range <>)
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
   FT6206_TH_GROUP_REG                 : constant UInt8 := 16#80#;

   --  Values FT6206_TH_GROUP_REG : threshold related
   FT6206_THRESHOLD_MASK               : constant UInt8 := 16#FF#;
   FT6206_THRESHOLD_SHIFT              : constant UInt8 := 16#00#;

   --  Filter function coefficients
   FT6206_TH_DIFF_REG                  : constant UInt8 := 16#85#;

   --  Control register
   FT6206_CTRL_REG                     : constant UInt8 := 16#86#;

   --  Values related to FT6206_CTRL_REG

   --  Will keep the Active mode when there is no touching
   FT6206_CTRL_KEEP_ACTIVE_MODE        : constant UInt8 := 16#00#;

   --  Switching from Active mode to Monitor mode automatically when there
   --  is no touching
   FT6206_CTRL_KEEP_AUTO_SWITCH_MONITOR_MODE : constant UInt8 := 16#01#;

   --  The time period of switching from Active mode to Monitor mode when
   --  there is no touching
   FT6206_TIMEENTERMONITOR_REG               : constant UInt8 := 16#87#;

   --  Report rate in Active mode
   FT6206_PERIODACTIVE_REG             : constant UInt8 := 16#88#;

   --  Report rate in Monitor mode
   FT6206_PERIODMONITOR_REG            : constant UInt8 := 16#89#;

   --  The value of the minimum allowed angle while Rotating gesture mode
   FT6206_RADIAN_VALUE_REG             : constant UInt8 := 16#91#;

   --  Maximum offset while Moving Left and Moving Right gesture
   FT6206_OFFSET_LEFT_RIGHT_REG        : constant UInt8 := 16#92#;

   --  Maximum offset while Moving Up and Moving Down gesture
   FT6206_OFFSET_UP_DOWN_REG           : constant UInt8 := 16#93#;

   --  Minimum distance while Moving Left and Moving Right gesture
   FT6206_DISTANCE_LEFT_RIGHT_REG      : constant UInt8 := 16#94#;

   --  Minimum distance while Moving Up and Moving Down gesture
   FT6206_DISTANCE_UP_DOWN_REG         : constant UInt8 := 16#95#;

   --  Maximum distance while Zoom In and Zoom Out gesture
   FT6206_DISTANCE_ZOOM_REG            : constant UInt8 := 16#96#;

   --  High 8-bit of LIB Version info
   FT6206_LIB_VER_H_REG                : constant UInt8 := 16#A1#;

   --  Low 8-bit of LIB Version info
   FT6206_LIB_VER_L_REG                : constant UInt8 := 16#A2#;

   --  Chip Selecting
   FT6206_CIPHER_REG                   : constant UInt8 := 16#A3#;

   --  Interrupt mode register (used when in interrupt mode)
   FT6206_GMODE_REG                    : constant UInt8 := 16#A4#;

   FT6206_G_MODE_INTERRUPT_MASK        : constant UInt8 := 16#03#;

   --  Possible values of FT6206_GMODE_REG
   FT6206_G_MODE_INTERRUPT_POLLING     : constant UInt8 := 16#00#;
   FT6206_G_MODE_INTERRUPT_TRIGGER     : constant UInt8 := 16#01#;

   --  Current power mode the FT6206 system is in (R)
   FT6206_PWR_MODE_REG                 : constant UInt8 := 16#A5#;

   --  FT6206 firmware version
   FT6206_FIRMID_REG                   : constant UInt8 := 16#A6#;

   --  FT6206 Chip identification register
   FT6206_CHIP_ID_REG                  : constant UInt8 := 16#A8#;

   --   Possible values of FT6206_CHIP_ID_REG
   FT6206_ID_VALUE                     : constant UInt8 := 16#11#;

   --  Release code version
   FT6206_RELEASE_CODE_ID_REG          : constant UInt8 := 16#AF#;

   --  Current operating mode the FT6206 system is in (R)
   FT6206_STATE_REG                    : constant UInt8 := 16#BC#;

end FT6x06;
