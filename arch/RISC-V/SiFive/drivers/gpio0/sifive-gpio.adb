------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

with System.Storage_Elements; use System.Storage_Elements;

with HAL;      use HAL;
with HAL.GPIO; use HAL.GPIO;

package body SiFive.GPIO is

   function Pin_Bit (This : GPIO_Point'Class) return UInt32
     with Inline_Always;

   -------------
   -- Pin_Bit --
   -------------

   function Pin_Bit (This : GPIO_Point'Class) return UInt32
   is (Shift_Left (1, Natural (This.Pin)));

   ------------
   -- Invert --
   ------------

   procedure Invert (This    : in out GPIO_Point;
                     Enabled : Boolean := True)
   is
      Out_Xor : UInt32
        with Volatile,
        Address => To_Address (This.Controller.Base_Address + 16#40#);
   begin
      if Enabled then
         Out_Xor := Out_Xor or This.Pin_Bit;
      else
         Out_Xor := Out_Xor and (not This.Pin_Bit);
      end if;
   end Invert;

   --  Invert the output level

   function Inverted (This : GPIO_Point) return Boolean is
      Out_Xor : UInt32
        with Volatile,
        Address => To_Address (This.Controller.Base_Address + 16#40#);
   begin
      return (Out_Xor and This.Pin_Bit) /= 0;
   end Inverted;

   ----------
   -- Mode --
   ----------

   overriding
   function Mode (This : GPIO_Point) return HAL.GPIO.GPIO_Mode is
      Output_Enable : UInt32
        with Volatile,
        Address => To_Address (This.Controller.Base_Address + 16#08#);
   begin
      if (Output_Enable and This.Pin_Bit) /= 0 then
         return Output;
      else
         return Input;
      end if;
   end Mode;

   --------------
   -- Set_Mode --
   --------------

   overriding
   procedure Set_Mode (This : in out GPIO_Point;
                       Mode : HAL.GPIO.GPIO_Config_Mode)
   is
      Output_Enable : UInt32
        with Volatile,
        Address => To_Address (This.Controller.Base_Address + 16#08#);

      Input_Enable : UInt32
        with Volatile,
        Address => To_Address (This.Controller.Base_Address + 16#04#);
   begin
      --  Input mode is always on to make sure we can read IO state even in
      --  output mode.
      Input_Enable := Input_Enable or This.Pin_Bit;

      if Mode = Output then
         Output_Enable := Output_Enable or This.Pin_Bit;
      else
         Output_Enable := Output_Enable and (not This.Pin_Bit);
      end if;
   end Set_Mode;

   -------------------
   -- Pull_Resistor --
   -------------------

   overriding
   function Pull_Resistor (This : GPIO_Point)
                           return HAL.GPIO.GPIO_Pull_Resistor
   is
      Pullup_Enable : UInt32
        with Volatile,
        Address => To_Address (This.Controller.Base_Address + 16#10#);
   begin
      if (Pullup_Enable and This.Pin_Bit) /= 0 then
         return Pull_Up;
      else
         return Floating;
      end if;
   end Pull_Resistor;

   -----------------------
   -- Set_Pull_Resistor --
   -----------------------

   overriding
   procedure Set_Pull_Resistor (This : in out GPIO_Point;
                                Pull : HAL.GPIO.GPIO_Pull_Resistor)

   is
      Pullup_Enable : UInt32
        with Volatile,
        Address => To_Address (This.Controller.Base_Address + 16#10#);
   begin
      if Pull = Pull_Up then
         Pullup_Enable := Pullup_Enable or This.Pin_Bit;
      else
         Pullup_Enable := Pullup_Enable and (not This.Pin_Bit);
      end if;
   end Set_Pull_Resistor;

   ---------
   -- Set --
   ---------

   overriding
   function Set (This : GPIO_Point) return Boolean is
      Input_Val : UInt32
        with Volatile,
        Address => To_Address (This.Controller.Base_Address + 16#00#);
   begin
      return (Input_Val and This.Pin_Bit) /= 0;
   end Set;

   ---------
   -- Set --
   ---------

   overriding
   procedure Set (This : in out GPIO_Point) is
      Output_Val : UInt32
        with Volatile,
        Address => To_Address (This.Controller.Base_Address + 16#0C#);
   begin
      Output_Val := Output_Val or This.Pin_Bit;
   end Set;

   -----------
   -- Clear --
   -----------

   overriding
   procedure Clear (This : in out GPIO_Point) is
      Output_Val : UInt32
        with Volatile,
        Address => To_Address (This.Controller.Base_Address + 16#0C#);
   begin
      Output_Val := Output_Val and (not This.Pin_Bit);
   end Clear;

   ------------
   -- Toggle --
   ------------

   overriding
   procedure Toggle (This : in out GPIO_Point) is
   begin
      if This.Set then
         This.Clear;
      else
         This.Set;
      end if;
   end Toggle;

end SiFive.GPIO;
