------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

with Ada.Unchecked_Conversion;

package body FT5336 is


   pragma Warnings (Off, "* is not referenced");

   --------------------------------------%----------------------
   -- Definitions for FT5336 I2C register addresses on 8 bit --
   ------------------------------------------------------------

   --  Current mode register of the FT5336 (R/W)
   FT5336_DEV_MODE_REG                 : constant UInt8 := 16#00#;

   --  Possible values of FT5336_DEV_MODE_REG
   FT5336_DEV_MODE_WORKING             : constant UInt8 := 16#00#;
   FT5336_DEV_MODE_FACTORY             : constant UInt8 := 16#04#;

   FT5336_DEV_MODE_MASK                : constant UInt8 := 16#07#;
   FT5336_DEV_MODE_SHIFT               : constant UInt8 := 16#04#;

   --  Gesture ID register
   FT5336_GEST_ID_REG                  : constant UInt8 := 16#01#;

   --  Possible values of FT5336_GEST_ID_REG
   FT5336_GEST_ID_NO_GESTURE           : constant UInt8 := 16#00#;
   FT5336_GEST_ID_MOVE_UP              : constant UInt8 := 16#10#;
   FT5336_GEST_ID_MOVE_RIGHT           : constant UInt8 := 16#14#;
   FT5336_GEST_ID_MOVE_DOWN            : constant UInt8 := 16#18#;
   FT5336_GEST_ID_MOVE_LEFT            : constant UInt8 := 16#1C#;
   FT5336_GEST_ID_SINGLE_CLICK         : constant UInt8 := 16#20#;
   FT5336_GEST_ID_DOUBLE_CLICK         : constant UInt8 := 16#22#;
   FT5336_GEST_ID_ROTATE_CLOCKWISE     : constant UInt8 := 16#28#;
   FT5336_GEST_ID_ROTATE_C_CLOCKWISE   : constant UInt8 := 16#29#;
   FT5336_GEST_ID_ZOOM_IN              : constant UInt8 := 16#40#;
   FT5336_GEST_ID_ZOOM_OUT             : constant UInt8 := 16#49#;

   --  Touch Data Status register : gives number of active touch points (0..5)
   FT5336_TD_STAT_REG                  : constant UInt8 := 16#02#;

   --  Values related to FT5336_TD_STAT_REG
   FT5336_TD_STAT_MASK                 : constant UInt8 := 16#0F#;
   FT5336_TD_STAT_SHIFT                : constant UInt8 := 16#00#;

   --  Values Pn_XH and Pn_YH related
   FT5336_TOUCH_EVT_FLAG_PRESS_DOWN    : constant UInt8 := 16#00#;
   FT5336_TOUCH_EVT_FLAG_LIFT_UP       : constant UInt8 := 16#01#;
   FT5336_TOUCH_EVT_FLAG_CONTACT       : constant UInt8 := 16#02#;
   FT5336_TOUCH_EVT_FLAG_NO_EVENT      : constant UInt8 := 16#03#;

   FT5336_TOUCH_EVT_FLAG_SHIFT         : constant UInt8 := 16#06#;
   FT5336_TOUCH_EVT_FLAG_MASK          : constant UInt8 := 2#1100_0000#;

   FT5336_TOUCH_POS_MSB_MASK           : constant UInt8 := 16#0F#;
   FT5336_TOUCH_POS_MSB_SHIFT          : constant UInt8 := 16#00#;

   --  Values Pn_XL and Pn_YL related
   FT5336_TOUCH_POS_LSB_MASK           : constant UInt8 := 16#FF#;
   FT5336_TOUCH_POS_LSB_SHIFT          : constant UInt8 := 16#00#;


   --  Values Pn_WEIGHT related
   FT5336_TOUCH_WEIGHT_MASK            : constant UInt8 := 16#FF#;
   FT5336_TOUCH_WEIGHT_SHIFT           : constant UInt8 := 16#00#;


   --  Values related to FT5336_Pn_MISC_REG
   FT5336_TOUCH_AREA_MASK              : constant UInt8 := 2#0100_0000#;
   FT5336_TOUCH_AREA_SHIFT             : constant UInt8 := 16#04#;

   type FT5336_Pressure_Registers is record
      XH_Reg     : UInt8;
      XL_Reg     : UInt8;
      YH_Reg     : UInt8;
      YL_Reg     : UInt8;
      --  Touch Pressure register value (R)
      Weight_Reg : UInt8;
      --  Touch area register
      Misc_Reg   : UInt8;
   end record;

   FT5336_Px_Regs                : constant array (Positive range <>)
                                      of FT5336_Pressure_Registers  :=
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
                                             Misc_Reg   => 16#0E#),
                                      3  => (XH_Reg     => 16#0F#,
                                             XL_Reg     => 16#10#,
                                             YH_Reg     => 16#11#,
                                             YL_Reg     => 16#12#,
                                             Weight_Reg => 16#13#,
                                             Misc_Reg   => 16#14#),
                                      4  => (XH_Reg     => 16#15#,
                                             XL_Reg     => 16#16#,
                                             YH_Reg     => 16#17#,
                                             YL_Reg     => 16#18#,
                                             Weight_Reg => 16#19#,
                                             Misc_Reg   => 16#1A#),
                                      5  => (XH_Reg     => 16#1B#,
                                             XL_Reg     => 16#1C#,
                                             YH_Reg     => 16#1D#,
                                             YL_Reg     => 16#1E#,
                                             Weight_Reg => 16#1F#,
                                             Misc_Reg   => 16#20#),
                                      6  => (XH_Reg     => 16#21#,
                                             XL_Reg     => 16#22#,
                                             YH_Reg     => 16#23#,
                                             YL_Reg     => 16#24#,
                                             Weight_Reg => 16#25#,
                                             Misc_Reg   => 16#26#),
                                      7  => (XH_Reg     => 16#27#,
                                             XL_Reg     => 16#28#,
                                             YH_Reg     => 16#29#,
                                             YL_Reg     => 16#2A#,
                                             Weight_Reg => 16#2B#,
                                             Misc_Reg   => 16#2C#),
                                      8  => (XH_Reg     => 16#2D#,
                                             XL_Reg     => 16#2E#,
                                             YH_Reg     => 16#2F#,
                                             YL_Reg     => 16#30#,
                                             Weight_Reg => 16#31#,
                                             Misc_Reg   => 16#32#),
                                      9  => (XH_Reg     => 16#33#,
                                             XL_Reg     => 16#34#,
                                             YH_Reg     => 16#35#,
                                             YL_Reg     => 16#36#,
                                             Weight_Reg => 16#37#,
                                             Misc_Reg   => 16#38#),
                                      10 => (XH_Reg     => 16#39#,
                                             XL_Reg     => 16#3A#,
                                             YH_Reg     => 16#3B#,
                                             YL_Reg     => 16#3C#,
                                             Weight_Reg => 16#3D#,
                                             Misc_Reg   => 16#3E#));

   --  Threshold for touch detection
   FT5336_TH_GROUP_REG                 : constant UInt8 := 16#80#;

   --  Values FT5336_TH_GROUP_REG : threshold related
   FT5336_THRESHOLD_MASK               : constant UInt8 := 16#FF#;
   FT5336_THRESHOLD_SHIFT              : constant UInt8 := 16#00#;

   --  Filter function coefficients
   FT5336_TH_DIFF_REG                  : constant UInt8 := 16#85#;

   --  Control register
   FT5336_CTRL_REG                     : constant UInt8 := 16#86#;

   --  Values related to FT5336_CTRL_REG

   --  Will keep the Active mode when there is no touching
   FT5336_CTRL_KEEP_ACTIVE_MODE        : constant UInt8 := 16#00#;

   --  Switching from Active mode to Monitor mode automatically when there
   --  is no touching
   FT5336_CTRL_KEEP_AUTO_SWITCH_MONITOR_MODE : constant UInt8 := 16#01#;

   --  The time period of switching from Active mode to Monitor mode when
   --  there is no touching
   FT5336_TIMEENTERMONITOR_REG               : constant UInt8 := 16#87#;

   --  Report rate in Active mode
   FT5336_PERIODACTIVE_REG             : constant UInt8 := 16#88#;

   --  Report rate in Monitor mode
   FT5336_PERIODMONITOR_REG            : constant UInt8 := 16#89#;

   --  The value of the minimum allowed angle while Rotating gesture mode
   FT5336_RADIAN_VALUE_REG             : constant UInt8 := 16#91#;

   --  Maximum offset while Moving Left and Moving Right gesture
   FT5336_OFFSET_LEFT_RIGHT_REG        : constant UInt8 := 16#92#;

   --  Maximum offset while Moving Up and Moving Down gesture
   FT5336_OFFSET_UP_DOWN_REG           : constant UInt8 := 16#93#;

   --  Minimum distance while Moving Left and Moving Right gesture
   FT5336_DISTANCE_LEFT_RIGHT_REG      : constant UInt8 := 16#94#;

   --  Minimum distance while Moving Up and Moving Down gesture
   FT5336_DISTANCE_UP_DOWN_REG         : constant UInt8 := 16#95#;

   --  Maximum distance while Zoom In and Zoom Out gesture
   FT5336_DISTANCE_ZOOM_REG            : constant UInt8 := 16#96#;

   --  High 8-bit of LIB Version info
   FT5336_LIB_VER_H_REG                : constant UInt8 := 16#A1#;

   --  Low 8-bit of LIB Version info
   FT5336_LIB_VER_L_REG                : constant UInt8 := 16#A2#;

   --  Chip Selecting
   FT5336_CIPHER_REG                   : constant UInt8 := 16#A3#;

   --  Interrupt mode register (used when in interrupt mode)
   FT5336_GMODE_REG                    : constant UInt8 := 16#A4#;

   FT5336_G_MODE_INTERRUPT_MASK        : constant UInt8 := 16#03#;

   --  Possible values of FT5336_GMODE_REG
   FT5336_G_MODE_INTERRUPT_POLLING     : constant UInt8 := 16#00#;
   FT5336_G_MODE_INTERRUPT_TRIGGER     : constant UInt8 := 16#01#;

   --  Current power mode the FT5336 system is in (R)
   FT5336_PWR_MODE_REG                 : constant UInt8 := 16#A5#;

   --  FT5336 firmware version
   FT5336_FIRMID_REG                   : constant UInt8 := 16#A6#;

   --  FT5336 Chip identification register
   FT5336_CHIP_ID_REG                  : constant UInt8 := 16#A8#;

   --   Possible values of FT5336_CHIP_ID_REG
   FT5336_ID_VALUE                     : constant UInt8 := 16#51#;

   --  Release code version
   FT5336_RELEASE_CODE_ID_REG          : constant UInt8 := 16#AF#;

   --  Current operating mode the FT5336 system is in (R)
   FT5336_STATE_REG                    : constant UInt8 := 16#BC#;

   pragma Warnings (On, "* is not referenced");

   --------------
   -- I2C_Read --
   --------------

   function I2C_Read (This   : in out FT5336_Device;
                      Reg    : UInt8;
                      Status : out Boolean)
                      return UInt8
   is
      Ret        : I2C_Data (1 .. 1);
      Tmp_Status : I2C_Status;
   begin
      This.Port.Mem_Read
        (This.I2C_Addr,
         UInt16 (Reg),
         Memory_Size_8b,
         Ret,
         Tmp_Status,
         1000);
      Status := Tmp_Status = Ok;

      return Ret (1);
   end I2C_Read;

   ---------------
   -- I2C_Write --
   ---------------

   procedure I2C_Write (This   : in out FT5336_Device;
                        Reg    : UInt8;
                        Data   : UInt8;
                        Status : out Boolean)
   is
      Tmp_Status : I2C_Status;
   begin
      This.Port.Mem_Write
        (This.I2C_Addr,
         UInt16 (Reg),
         Memory_Size_8b,
         (1 => Data),
         Tmp_Status,
         1000);
      Status := Tmp_Status = Ok;
   end I2C_Write;
   -------------
   -- Read_Id --
   -------------

   function Check_Id (This : in out FT5336_Device) return Boolean
   is
      Id     : UInt8;
      Status : Boolean;

   begin
      for J in 1 .. 3 loop
         Id := This.I2C_Read (FT5336_CHIP_ID_REG, Status);

         if Id = FT5336_ID_VALUE then
            return True;
         end if;

         if not Status then
            return False;
         end if;
      end loop;

      return False;
   end Check_Id;

   ---------------------------
   -- TP_Set_Use_Interrupts --
   ---------------------------

   procedure TP_Set_Use_Interrupts (This    : in out FT5336_Device;
                                    Enabled : Boolean)
   is
      Reg_Value : UInt8 := 0;
      Status    : Boolean with Unreferenced;
   begin
      if Enabled then
         Reg_Value := FT5336_G_MODE_INTERRUPT_TRIGGER;
      else
         Reg_Value := FT5336_G_MODE_INTERRUPT_POLLING;
      end if;

      This.I2C_Write (FT5336_GMODE_REG, Reg_Value, Status);
   end TP_Set_Use_Interrupts;

   ----------------
   -- Set_Bounds --
   ----------------

   overriding
   procedure Set_Bounds (This   : in out FT5336_Device;
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
   function Active_Touch_Points (This : in out FT5336_Device)
                                 return Touch_Identifier
   is
      Status   : Boolean;
      Nb_Touch : UInt8 := 0;
   begin
      Nb_Touch := This.I2C_Read (FT5336_TD_STAT_REG, Status);

      if not Status then
         return 0;
      end if;

      Nb_Touch := Nb_Touch and FT5336_TD_STAT_MASK;

      if Natural (Nb_Touch) > FT5336_Px_Regs'Last then
         --  Overflow: set to 0
         Nb_Touch := 0;
      end if;

      return Natural (Nb_Touch);
   end Active_Touch_Points;

   ---------------------
   -- Get_Touch_Point --
   ---------------------

   overriding
   function Get_Touch_Point (This     : in out FT5336_Device;
                             Touch_Id : Touch_Identifier)
                             return TP_Touch_State
   is
      type UInt16_HL_Type is record
         High, Low : UInt8;
      end record with Size => 16;
      for UInt16_HL_Type use record
         High at 1 range 0 .. 7;
         Low  at 0 range 0 .. 7;
      end record;

      function To_UInt16 is
        new Ada.Unchecked_Conversion (UInt16_HL_Type, UInt16);

      Ret    : TP_Touch_State;
      Regs   : FT5336_Pressure_Registers;
      Tmp    : UInt16_HL_Type;
      Status : Boolean;

   begin
      --  X/Y are swaped from the screen coordinates

      if Touch_Id not in FT5336_Px_Regs'Range
        or else Touch_Id > This.Active_Touch_Points
      then
         return (0, 0, 0);
      end if;

      Regs := FT5336_Px_Regs (Touch_Id);

      Tmp.Low := This.I2C_Read (Regs.XL_Reg, Status);

      if not Status then
         return (0, 0, 0);
      end if;

      Tmp.High := This.I2C_Read (Regs.XH_Reg, Status) and
        FT5336_TOUCH_POS_MSB_MASK;

      if not Status then
         return (0, 0, 0);
      end if;

      Ret.Y := Natural (To_UInt16 (Tmp));

      Tmp.Low := This.I2C_Read (Regs.YL_Reg, Status);

      if not Status then
         return (0, 0, 0);
      end if;

      Tmp.High := This.I2C_Read (Regs.YH_Reg, Status) and
        FT5336_TOUCH_POS_MSB_MASK;

      if not Status then
         return (0, 0, 0);
      end if;

      Ret.X := Natural (To_UInt16 (Tmp));

      Ret.Weight := Natural (This.I2C_Read (Regs.Weight_Reg, Status));

      if not Status then
         Ret.Weight := 0;
      end if;

      Ret.X :=
        Natural'Min (Natural'Max (0, Ret.X), This.LCD_Natural_Width - 1);
      Ret.Y :=
        Natural'Min (Natural'Max (0, Ret.Y), This.LCD_Natural_Height - 1);

      if (This.Swap and Invert_X) /= 0 then
         Ret.X := This.LCD_Natural_Width - Ret.X - 1;
      end if;
      if (This.Swap and Invert_Y) /= 0 then
         Ret.Y := This.LCD_Natural_Height - Ret.Y - 1;
      end if;
      if (This.Swap and Swap_XY) /= 0 then
         declare
            Tmp_X : constant Integer := Ret.X;
         begin
            Ret.X := Ret.Y;
            Ret.Y := Tmp_X;
         end;
      end if;

      return Ret;
   end Get_Touch_Point;

   --------------------------
   -- Get_All_Touch_Points --
   --------------------------

   overriding
   function Get_All_Touch_Points
     (This : in out FT5336_Device) return HAL.Touch_Panel.TP_State
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

end FT5336;
