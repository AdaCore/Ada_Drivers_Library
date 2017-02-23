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

--  Driver package for the FT6x06 touch screen panel
--  Based on the ft6x06 driver from MCD Application Team

with Ada.Unchecked_Conversion;
with HAL.Touch_Panel; use HAL.Touch_Panel;

package body FT6x06 is

   --------------
   -- Check_Id --
   --------------

   function Check_Id (This : in out FT6x06_Device) return Boolean is
      Id     : UInt8;
      Status : Boolean;
   begin
      for J in 1 .. 3 loop
         Id := This.I2C_Read (FT6206_CHIP_ID_REG, Status);

         if Id = FT6206_ID_VALUE then
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

   procedure TP_Set_Use_Interrupts
     (This    : in out FT6x06_Device;
      Enabled : Boolean)
   is
      Reg_Value : UInt8 := 0;
      Status    : Boolean with Unreferenced;
   begin
      if Enabled then
         Reg_Value := FT6206_G_MODE_INTERRUPT_TRIGGER;
      else
         Reg_Value := FT6206_G_MODE_INTERRUPT_POLLING;
      end if;

      This.I2C_Write (FT6206_GMODE_REG, Reg_Value, Status);
   end TP_Set_Use_Interrupts;

   ----------------
   -- Set_Bounds --
   ----------------

   overriding procedure Set_Bounds
     (This   : in out FT6x06_Device;
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
   function Active_Touch_Points (This : in out FT6x06_Device)
                                 return Touch_Identifier
   is
      Status   : Boolean;
      Nb_Touch : UInt8 := 0;
   begin
      Nb_Touch := This.I2C_Read (FT6206_TD_STAT_REG, Status);

      if not Status then
         return 0;
      end if;

      Nb_Touch := Nb_Touch and FT6206_TD_STAT_MASK;

      if Natural (Nb_Touch) > FT6206_Px_Regs'Last
        or else
         Natural (Nb_Touch) not in Touch_Identifier
      then
         --  Overflow: set to 0
         return 0;
      else
         return Touch_Identifier (Nb_Touch);
      end if;
   end Active_Touch_Points;

   ---------------------
   -- Get_Touch_State --
   ---------------------

   overriding
   function Get_Touch_Point (This     : in out FT6x06_Device;
                             Touch_Id : Touch_Identifier)
                             return HAL.Touch_Panel.TP_Touch_State
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

      Ret  : TP_Touch_State;
      Regs : FT6206_Pressure_Registers;
      Tmp  : UInt16_HL_Type;
      Status : Boolean;
   begin
      if Touch_Id not in FT6206_Px_Regs'Range then
         return (0, 0, 0);
      end if;

      if Touch_Id > This.Active_Touch_Points then
         return (0, 0, 0);
      end if;

      --  X/Y are swaped from the screen coordinates

      Regs := FT6206_Px_Regs (Touch_Id);

      Tmp.Low := This.I2C_Read (Regs.XL_Reg, Status);

      if not Status then
         return (0, 0, 0);
      end if;

      Tmp.High := This.I2C_Read (Regs.XH_Reg, Status) and
        FT6206_TOUCH_POS_MSB_MASK;

      if not Status then
         return (0, 0, 0);
      end if;

      Ret.Y := Natural (To_UInt16 (Tmp));

      Tmp.Low := This.I2C_Read (Regs.YL_Reg, Status);

      if not Status then
         return (0, 0, 0);
      end if;

      Tmp.High := This.I2C_Read (Regs.YH_Reg, Status) and
        FT6206_TOUCH_POS_MSB_MASK;

      if not Status then
         return (0, 0, 0);
      end if;

      Ret.X := Natural (To_UInt16 (Tmp));

      Ret.Weight := Natural (This.I2C_Read (Regs.Weight_Reg, Status));

      if not Status then
         Ret.Weight := 0;
      end if;

      if Ret.Weight = 0 then
         Ret.Weight := 50;
      end if;

      Ret.X := Natural'Max (0, Ret.X);
      Ret.Y := Natural'Max (0, Ret.Y);
      Ret.X := Natural'Min (This.LCD_Natural_Width - 1, Ret.X);
      Ret.Y := Natural'Min (This.LCD_Natural_Height - 1, Ret.Y);

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
     (This : in out FT6x06_Device)
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

   --------------
   -- I2C_Read --
   --------------

   function I2C_Read (This   : in out FT6x06_Device;
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

   procedure I2C_Write (This   : in out FT6x06_Device;
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

end FT6x06;
