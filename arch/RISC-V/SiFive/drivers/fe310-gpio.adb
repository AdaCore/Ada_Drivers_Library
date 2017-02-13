------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with FE310_SVD.GPIO; use FE310_SVD.GPIO;
with HAL.GPIO;       use HAL.GPIO;

package body FE310.GPIO is

   ---------------------
   -- Set_IO_Function --
   ---------------------

   procedure Set_IO_Function (This : in out GPIO_Point;
                              Func : IO_Function)
   is
   begin
      case Func is
         when Disabled =>
            GPIO0_Periph.IO_FUNC_EN.Arr (This.Pin) := False;
         when IOF0 | IOF1 =>
            GPIO0_Periph.IO_FUNC_SEL.Arr (This.Pin) := Func = IOF1;
            GPIO0_Periph.IO_FUNC_EN.Arr (This.Pin) := True;
      end case;
   end Set_IO_Function;

   ----------
   -- Mode --
   ----------

   overriding
   function Mode (This : GPIO_Point) return HAL.GPIO.GPIO_Mode is
   begin
      if GPIO0_Periph.IO_FUNC_EN.Arr (This.Pin) then
         return Unknown;
      elsif GPIO0_Periph.OUTPUT_EN.Arr (This.Pin) then
         return Output;
      else
         return Input;
      end if;
   end Mode;

   --------------
   -- Set_Mode --
   --------------

   overriding
   function Set_Mode (This : in out GPIO_Point;
                      Mode : HAL.GPIO.GPIO_Config_Mode) return Boolean is
   begin
      --  Input mode is always on to make sure we can read IO state even in
      --  output mode.
      GPIO0_Periph.INPUT_EN.Arr (This.Pin) := True;
      GPIO0_Periph.OUTPUT_EN.Arr (This.Pin) := Mode = Output;
      return True;
   end Set_Mode;

   -------------------
   -- Pull_Resistor --
   -------------------

   overriding
   function Pull_Resistor (This : GPIO_Point)
                           return HAL.GPIO.GPIO_Pull_Resistor is
   begin
      if GPIO0_Periph.PULLUP.Arr (This.Pin) then
         return Pull_Up;
      else
         return Floating;
      end if;
   end Pull_Resistor;

   -----------------------
   -- Set_Pull_Resistor --
   -----------------------

   overriding
   function Set_Pull_Resistor (This : in out GPIO_Point;
                               Pull : HAL.GPIO.GPIO_Pull_Resistor)
                               return Boolean is
   begin
      if Pull = Pull_Down then
         return False;
      else
         GPIO0_Periph.PULLUP.Arr (This.Pin) := Pull = Pull_Up;
         return True;
      end if;
   end Set_Pull_Resistor;

   ---------
   -- Set --
   ---------

   overriding
   function Set (This : GPIO_Point) return Boolean is
   begin
      return GPIO0_Periph.VALUE.Arr (This.Pin);
   end Set;

   ---------
   -- Set --
   ---------

   overriding
   procedure Set (This : in out GPIO_Point) is
   begin
      GPIO0_Periph.PORT.Arr (This.Pin) := True;
   end Set;

   -----------
   -- Clear --
   -----------

   overriding
   procedure Clear (This : in out GPIO_Point) is
   begin
      GPIO0_Periph.PORT.Arr (This.Pin) := False;
   end Clear;

   ------------
   -- Toggle --
   ------------

   overriding
   procedure Toggle (This : in out GPIO_Point) is
   begin
      GPIO0_Periph.PORT.Arr (This.Pin) := not GPIO0_Periph.VALUE.Arr (This.Pin);
   end Toggle;

end FE310.GPIO;
