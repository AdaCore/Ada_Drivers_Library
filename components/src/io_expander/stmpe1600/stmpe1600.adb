------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017-2018, AdaCore                      --
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

with HAL; use HAL;

with Ada.Unchecked_Conversion;

package body STMPE1600 is

   subtype BA_2 is UInt8_Array (1 .. 2);
   function To_Pins is new Ada.Unchecked_Conversion (BA_2, STMPE1600_Pins);
   function From_Pins is new Ada.Unchecked_Conversion (STMPE1600_Pins, BA_2);

   ----------
   -- Read --
   ----------

   procedure Read
     (This   : STMPE1600_Expander;
      Reg    : UInt8;
      Data   : out UInt8_Array;
      Status : out Boolean)
   is
      S : HAL.I2C.I2C_Status;
      use type HAL.I2C.I2C_Status;
   begin
      This.Port.Mem_Read
        (This.Addr,
         UInt16 (Reg),
         HAL.I2C.Memory_Size_8b,
         Data,
         S);

      Status := S = HAL.I2C.Ok;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (This   : STMPE1600_Expander;
      Reg    : UInt8;
      Data   : UInt8_Array;
      Status : out Boolean)
   is
      S : HAL.I2C.I2C_Status;
      use type HAL.I2C.I2C_Status;
   begin
      This.Port.Mem_Write
        (This.Addr,
         UInt16 (Reg),
         HAL.I2C.Memory_Size_8b,
         Data,
         S);

      Status := S = HAL.I2C.Ok;
   end Write;

   --------------
   -- Check_Id --
   --------------

   procedure Check_Id
     (This   : in out STMPE1600_Expander;
      Status : out Boolean)
   is
      Identifier : UInt8_Array (1 .. 2);
   begin
      Read (This, STMPE1600_REG_ChipID, Identifier, Status);

      if Identifier (1) /= 0
        or else Identifier (2) /= 16#16#
      then
         Status := False;
      end if;
   end Check_Id;

   --------------------------
   -- Set_Interrupt_Enable --
   --------------------------

   procedure Set_Interrupt_Enable
     (This     : in out STMPE1600_Expander;
      Enable   : Boolean;
      Polarity : STMPE1600_Pin_Polarity;
      Status   : out Boolean)
   is
      Sys_Ctrl : aliased STMPE1600_SYS_CTRL;
      BA       : aliased UInt8_Array (1 .. 1) with Address => Sys_Ctrl'Address;
   begin
      Read (This, STMPE1600_REG_System_Ctrl, BA, Status);
      if Status then
         Sys_Ctrl.INT_Enable := Enable;
         Sys_Ctrl.INT_Polarity := Polarity;
         Write (This, STMPE1600_REG_System_Ctrl, BA, Status);
      end if;
   end Set_Interrupt_Enable;

   ------------------------
   -- Set_Interrupt_Mask --
   ------------------------

   procedure Set_Interrupt_Mask
     (This : STMPE1600_Expander;
      Mask : STMPE1600_Pins;
      Status : out Boolean)
   is
      BA : aliased UInt8_Array (1 .. 2) with Address => Mask'Address;
   begin
      Write (This, STMPE1600_REG_IEGPIOR_0, BA, Status);
   end Set_Interrupt_Mask;

   --------------------
   -- Interrupt_Mask --
   --------------------

   function Interrupt_Mask
     (This   : STMPE1600_Expander) return STMPE1600_Pins
   is
      BA  : aliased UInt8_Array (1 .. 2);
      Status : Boolean;

   begin
      Read (This, STMPE1600_REG_IEGPIOR_0, BA, Status);
      return To_Pins (BA);
   end Interrupt_Mask;

   ---------------------
   -- Interrupt_State --
   ---------------------

   function Interrupt_State
     (This : STMPE1600_Expander) return STMPE1600_Pins
   is
      BA  : aliased UInt8_Array (1 .. 2);
      Status : Boolean;

   begin
      Read (This, STMPE1600_REG_ISGPIOR_0, BA, Status);
      return To_Pins (BA);
   end Interrupt_State;

   ----------------
   -- Pins_State --
   ----------------

   function Pins_State
     (This : in out STMPE1600_Expander) return STMPE1600_Pins
   is
      BA  : aliased UInt8_Array (1 .. 2);
      Status : Boolean;

   begin
      Read (This, STMPE1600_REG_GPMR_0, BA, Status);
      return To_Pins (BA);
   end Pins_State;

   ---------------
   -- Pin_State --
   ---------------

   function Pin_State
     (This : in out STMPE1600_Expander;
      Pin  : STMPE1600_Pin_Number) return Boolean
   is
      Pins : constant STMPE1600_Pins := Pins_State (This);
   begin
      return Pins (Pin);
   end Pin_State;

   --------------------
   -- Set_Pins_State --
   --------------------

   procedure Set_Pins_State
     (This : in out STMPE1600_Expander;
      Pins : STMPE1600_Pins)
   is
      BA     : constant UInt8_Array (1 .. 2) := From_Pins (Pins);
      Status : Boolean with Unreferenced;

   begin
      Write (This, STMPE1600_REG_GPSR_0, BA, Status);
   end Set_Pins_State;

   -------------------
   -- Set_Pin_State --
   -------------------

   procedure Set_Pin_State
     (This  : in out STMPE1600_Expander;
      Pin   : STMPE1600_Pin_Number;
      State : Boolean)
   is
      Pins : STMPE1600_Pins := Pins_State (This);
   begin
      Pins (Pin) := State;
      Set_Pins_State (This, Pins);
   end Set_Pin_State;

   ------------------------
   -- Set_Pins_Direction --
   ------------------------

   procedure Set_Pins_Direction
     (This      : STMPE1600_Expander;
      Pins      : STMPE1600_Pins_Direction)
   is
      BA     : aliased UInt8_Array (1 .. 2) with Address => Pins'Address;
      Status : Boolean with Unreferenced;
   begin
      Write (This, STMPE1600_REG_GPDR_0, BA, Status);
   end Set_Pins_Direction;

   -----------------------
   -- Set_Pin_Direction --
   -----------------------

   procedure Set_Pin_Direction
     (This      : STMPE1600_Expander;
      Pin       : STMPE1600_Pin_Number;
      Direction : STMPE1600_Pin_Direction)
   is
      Pins      : aliased STMPE1600_Pins;
      BA        : aliased UInt8_Array (1 .. 2) with Address => Pins'Address;
      Status    : Boolean with Unreferenced;
      New_State : constant Boolean := Direction = Output;

   begin
      Read (This, STMPE1600_REG_GPDR_0, BA, Status);
      if Pins (Pin) = New_State then
         --  Nothing to do
         return;
      end if;

      Pins (Pin) := New_State;
      Write (This, STMPE1600_REG_GPDR_0, BA, Status);
   end Set_Pin_Direction;

   -------------------
   -- Pin_Direction --
   -------------------

   function Pin_Direction
     (This : STMPE1600_Expander;
      Pin  : STMPE1600_Pin_Number)
      return STMPE1600_Pin_Direction
   is
      Pins      : aliased STMPE1600_Pins;
      BA        : aliased UInt8_Array (1 .. 2) with Address => Pins'Address;
      Status    : Boolean with Unreferenced;

   begin
      Read (This, STMPE1600_REG_GPDR_0, BA, Status);
      return (if Pins (Pin) then Output else Input);
   end Pin_Direction;

   --------------------------------
   -- Set_Pin_Polarity_Inversion --
   --------------------------------

   procedure Set_Pin_Polarity_Inversion
     (This            : STMPE1600_Expander;
      Pin             : STMPE1600_Pin_Number;
      Inversion_State : Boolean)
   is
      Pins      : aliased STMPE1600_Pins;
      BA        : aliased UInt8_Array (1 .. 2) with Address => Pins'Address;
      Status    : Boolean with Unreferenced;

   begin
      Read (This, STMPE1600_REG_GPPIR_0, BA, Status);
      if Pins (Pin) = Inversion_State then
         --  Nothing to do
         return;
      end if;

      Pins (Pin) := Inversion_State;
      Write (This, STMPE1600_REG_GPPIR_0, BA, Status);
   end Set_Pin_Polarity_Inversion;

   -------------------
   -- As_GPIO_Point --
   -------------------

   function As_GPIO_Point
     (This : in out STMPE1600_Expander;
      Pin  : STMPE1600_Pin_Number) return HAL.GPIO.Any_GPIO_Point
   is
   begin
      This.Points (Pin) := (This'Unrestricted_Access, Pin);
      return This.Points (Pin)'Unchecked_Access;
   end As_GPIO_Point;

   ----------
   -- Mode --
   ----------

   overriding
   function Mode (This : STMPE1600_Pin) return HAL.GPIO.GPIO_Mode is
   begin
      return (case Pin_Direction (This.Port.all, This.Pin) is
                 when Input => HAL.GPIO.Input,
                 when Output => HAL.GPIO.Output);
   end Mode;

   --------------
   -- Set_Mode --
   --------------

   overriding
   procedure Set_Mode (This : in out STMPE1600_Pin;
                       Mode : HAL.GPIO.GPIO_Config_Mode)
   is
   begin
      Set_Pin_Direction (This.Port.all, This.Pin,
                         (case Mode is
                             when HAL.GPIO.Input => Input,
                             when HAL.GPIO.Output => Output));
   end Set_Mode;

   ---------
   -- Set --
   ---------

   overriding
   function Set (This : STMPE1600_Pin) return Boolean
   is
   begin
      return Pin_State (This.Port.all, This.Pin);
   end Set;

   ---------
   -- Set --
   ---------

   overriding
   procedure Set (This : in out STMPE1600_Pin)
   is
   begin
      Set_Pin_State (This.Port.all, This.Pin, True);
   end Set;

   -----------
   -- Clear --
   -----------

   overriding
   procedure Clear (This : in out STMPE1600_Pin)
   is
   begin
      Set_Pin_State (This.Port.all, This.Pin, False);
   end Clear;

   ------------
   -- Toggle --
   ------------

   overriding
   procedure Toggle (This : in out STMPE1600_Pin)
   is
   begin
      if This.Set then
         This.Clear;
      else
         This.Set;
      end if;
   end Toggle;

end STMPE1600;
